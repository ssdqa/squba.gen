
#'
#' @importFrom stats loess mad median predict quantile sd
#' @importFrom utils stack
#'
NULL

#' Create a cross-joined master table for variable reference
#'
#' @param cj_tbl a table with the results of a longitudinal analysis
#' @param cj_var_names a vector with the names of variables that should be used as the "anchor"
#'                     of the cross join where all combinations of the variables should be
#'                     present in the final table
#' @param join_type the type of join that should be performed at the end of the function
#'                  left is used for multi-site anomaly (euclidean distance) while full
#'                  is used for single site anomaly (timetk package)
#'
#' @return one data frame with all combinations of the variables from cj_var_names with their
#'         associated facts from the original cj_tbl input
#'
compute_at_cross_join <- function(cj_tbl,
                                  cj_var_names = c('site','concept_id'),
                                  join_type = 'left') {


  cj_tbl <- ungroup(cj_tbl)
  blah <- list()

  date_first <- cj_tbl %>% distinct(time_start) %>% arrange(time_start) %>% first() %>% pull()
  date_last <- cj_tbl %>% distinct(time_start) %>% arrange(time_start) %>% last() %>% pull()
  time_increment_var <- cj_tbl %>% distinct(time_increment) %>% pull()

  all_months <- seq.Date(from=date_first,
                         to=date_last,
                         by=time_increment_var)
  all_months_tbl <- as_tibble(all_months) %>% rename(time_start=value)

  for(i in 1:length(cj_var_names)) {

    cj_var_name_i <- (cj_var_names[[i]])

    cj_tbl_narrowed <- cj_tbl %>% distinct(!! sym(cj_var_name_i))

    blah[[i]] <- cj_tbl_narrowed

  }

  cj_tbl_cjd <- reduce(.x=blah,
                       .f=cross_join)

  cj_tbl_cjd_time <-
    all_months_tbl %>% cross_join(cj_tbl_cjd)

  if(join_type == 'left'){
    cj_tbl_full <-
      cj_tbl_cjd_time %>%
      left_join(cj_tbl) %>%
      mutate(across(where(is.numeric), ~ replace_na(.x,0)))
  }else{
    cj_tbl_full <-
      cj_tbl_cjd_time %>%
      full_join(cj_tbl) %>%
      mutate(across(where(is.numeric), ~ replace_na(.x,0)))
  }


}

#' Compute Distance from Mean & Median
#'
#' This function will, for the values in a given `var_col`, compute the
#' distance between that value and the mean & median values for each group
#' established by the `grp_vars` parameter.
#'
#' @param tbl table with at least the vars specified in `grp_vars` and `var_col`
#' @param grp_vars variables to group by when computing summary statistics
#' @param var_col column to compute summary statistics on
#' @param num_sd (integer) number of standard deviations away from the mean
#'               from which to compute the sd_lower and sd_upper columns
#' @param num_mad (integer) number of median absolute deviations away from the median
#'                from which to compute the mad_lower and mad_upper columns
#' @return a table where, for each group in `grp_vars`, the following statistics are computed
#'         based on the `var_col`: mean, sd, sd_lower, sd_upper, mad, mad_lower, mad_upper,
#'         anomaly_yn (indicator of whether data point is +/- num_sd from mean or data point is > 90th percentile),
#'         abs_diff_mean (absolute value of difference between mean for group and observation),
#'         abs_diff_median (absolute value of difference between median for group and observation),
#'         n_mad (the number of MAD away from the median where the data point falls)
#'
#' @examples
#' # sample input table
#' sample_input <- dplyr::tibble('variable' = c('scd', 'scd', 'scd', 'scd'),
#'                               'site' = c('Site A', 'Site A', 'Site B',
#'                                          'Site B'),
#'                               'count' = c(15, 24, 100, 93))
#'
#' # execute function against sample data
#' compute_dist_mean_median(tbl = sample_input,
#'                          grp_vars = 'variable',
#'                          var_col = 'count',
#'                          num_sd = 1,
#'                          num_mad = 1)
#'
#' @export
#'
compute_dist_mean_median <- function(tbl,
                                     grp_vars,
                                     var_col,
                                     num_sd,
                                     num_mad){

  site_rows <-
    tbl %>% ungroup() %>% select(site) %>% distinct()
  grpd_vars_tbl <- tbl %>% ungroup() %>% select(!!!syms(grp_vars)) %>% distinct()

  tbl_new <-
    cross_join(site_rows,
               grpd_vars_tbl) %>%
    left_join(tbl) %>%
    mutate(across(where(is.numeric), ~replace_na(.x,0)))

  stats <- tbl_new %>%
    group_by(!!!syms(grp_vars))%>%
    summarise(mean=mean(!!!syms(var_col)),
              median=median(!!!syms(var_col)),
              sd=sd(!!!syms(var_col), na.rm=TRUE),
              mad=mad(!!!syms(var_col),center=median),
              `90th_percentile`=quantile(!!!syms(var_col), 0.95)) %>%
    ungroup() %>%
    mutate(sd_lower=mean-num_sd*sd,
           sd_upper=mean+num_sd*sd,
           mad_lower=median-num_mad*mad,
           mad_upper=median+num_mad*mad)

  final <- tbl_new %>%
    inner_join(stats)%>%
    mutate(anomaly_yn=case_when(!!sym(var_col)<sd_lower|!!sym(var_col)>sd_upper|!!sym(var_col)>`90th_percentile`~TRUE,
                                TRUE~FALSE),
           abs_diff_mean=abs(!!sym(var_col)-mean),
           abs_diff_median=abs(!!sym(var_col)-median),
           n_mad=abs_diff_median/mad)

  return(final)
}

#' Compute Euclidean Distance
#'
#' @param ms_tbl output from [compute_dist_mean_median()] where the cross-joined table from
#'               [compute_at_cross_join()] is used as input
#' @param output_var the output variable that should be used to compute the Euclidean distance
#'                   i.e. a count or proportion
#' @param grp_vars vector of grouping variables; the euclidean distance will be computed per group
#'
#' @return one dataframe with all variables from ms_tbl with the addition of columns with a site Loess
#'         value and a site Euclidean distance value
#'
compute_euclidean <- function(ms_tbl,
                              output_var,
                              grp_vars = c('site', 'concept_id')) {

  grp_tbls <- group_split(ms_tbl %>% unite(facet_col, !!!syms(grp_vars), sep = '_', remove = FALSE) %>%
                            group_by(facet_col))

  euclidean_dist <- function(x, y) sqrt(sum((x - y)^2))

  overall <- list()

  for(i in 1:length(grp_tbls)) {

    site_datenumeric <-
      grp_tbls[[i]] %>%
      mutate(date_numeric = as.numeric(time_start),
             output_var = !!sym(output_var))
    site_loess <- loess(output_var ~ date_numeric, data=site_datenumeric)
    site_loess_df <- as_tibble(predict(site_loess)) %>% rename(site_loess=1)
    euclidean_site_loess <- euclidean_dist(predict(site_loess), site_datenumeric$mean_allsiteprop)
    ms_witheuclidean <-
      cbind(site_datenumeric,site_loess_df) %>%
      mutate(dist_eucl_mean=euclidean_site_loess) #%>%
    # mutate(loess_predicted=predict(site_loess))

    overall[[i]] <- ms_witheuclidean

  }

  overall_reduce <- reduce(.x=overall,
                           .f=dplyr::union) %>% as_tibble() %>%
    mutate(dist_eucl_mean=round(dist_eucl_mean,2),
           site_loess=round(site_loess,2)) %>%
    select(-facet_col)

}

#' *_ms_anom_la Euclidean Distance Computation
#'
#' This function will compute the Euclidean Distance for the `var_col` at each
#' site in comparison to the overall, all-site mean.
#'
#' @param fot_input_tbl table output by [compute_fot()] for *_ms_anom_at analyses
#' @param grp_vars the variables that should be preserved in the cross join
#' @param var_col the column with the numerical statistic of interest for the euclidean
#'                distance computation
#'
#' @return data frame with mean and median values for the user provided variable column
#'         and the euclidean distance value from the all site mean
#'
#' @examples
#' # sample multi-site, longitudinal input data (modeled after EVP)
#' sample_ms_la_input <- dplyr::tibble('variable' = c('scd', 'scd', 'scd',
#'                                                    'scd', 'scd', 'scd',
#'                                                    'scd', 'scd', 'scd',
#'                                                    'scd', 'scd', 'scd',
#'                                                    'scd', 'scd'),
#'                              'site' = c('Site A','Site A','Site A',
#'                                         'Site A','Site A','Site A',
#'                                         'Site A','Site B','Site B',
#'                                         'Site B','Site B','Site B',
#'                                         'Site B','Site B'),
#'                              'count' = c(15, 24, 100, 93, 47, 65,
#'                                          33, 92, 153, 122, 5, 99,
#'                                          10, 30),
#'                              'time_start'=c('2018-01-01','2019-01-01',
#'                                    '2020-01-01', '2021-01-01', '2022-01-01',
#'                                    '2023-01-01', '2024-01-01','2018-01-01',
#'                                    '2019-01-01', '2020-01-01', '2021-01-01',
#'                                    '2022-01-01', '2023-01-01', '2024-01-01'),
#'                              'time_increment' = c('year','year','year',
#'                                    'year', 'year','year', 'year','year',
#'                                    'year','year','year','year','year',
#'                                    'year'))
#'
#' # compute euclidean distance for each site & variable combination
#' ms_anom_euclidean(fot_input_tbl = sample_ms_la_input %>%
#'                         dplyr::mutate(time_start = as.Date(time_start)),
#'                   grp_vars = c('site', 'variable'),
#'                   var_col = 'count')
#'
#' @export
#'
ms_anom_euclidean <- function(fot_input_tbl,
                              grp_vars,
                              var_col) {


  ms_at_cj <- compute_at_cross_join(cj_tbl=fot_input_tbl,
                                    cj_var_names = grp_vars)

  allsite_grps <- grp_vars %>% append('time_start')
  allsite_grps <- allsite_grps[! allsite_grps %in% c('site')]

  ms_at_cj_avg <- compute_dist_mean_median(tbl=ms_at_cj,
                                           grp_vars=allsite_grps,
                                           var_col=var_col,
                                           num_sd = 2,
                                           num_mad = 2)  %>%
    rename(mean_allsiteprop=mean)

  euclidiean_tbl <- compute_euclidean(ms_tbl=ms_at_cj_avg,
                                      output_var=var_col,
                                      grp_vars = grp_vars)

  final <-
    euclidiean_tbl %>%
    select(site,time_start, grp_vars, var_col,
           mean_allsiteprop, median, date_numeric,
           site_loess,dist_eucl_mean
    )

  return(final)

}


#' *_ss_anom_la Anomaly Detection
#'
#' For analyses where the `time_period` is smaller than a year, this function
#' will execute [timetk::anomalize()] to identify outliers in the time series
#' using STL regression. For year-level analyses, the same input table will be
#' returned and a different anomaly detection method will be used at the *_output
#' stage
#'
#' @param fot_input_tbl table output by [compute_fot()] for *_ss_anom_at analyses
#' @param grp_vars the variables that should be preserved in the cross join
#' @param var_col the column with the numerical statistic of interest for the euclidean
#'                distance computation
#' @param time_var the column with time information
#'
#' @return one dataframe with all columns from the original input table
#'         plus the columns needed for timetk output generated by the
#'         `anomalize` function
#'
#' @examples
#' # sample single-site, longitudinal input data (modeled after EVP)
#' sample_ss_la_input <- dplyr::tibble('variable' = c('scd', 'scd', 'scd',
#'                                                    'scd', 'scd', 'scd',
#'                                                    'scd', 'scd', 'scd',
#'                                                    'scd', 'scd', 'scd',
#'                                                    'scd', 'scd'),
#'                              'site' = c('Site A','Site A','Site A',
#'                                         'Site A','Site A','Site A',
#'                                         'Site A','Site A','Site A',
#'                                         'Site A','Site A','Site A',
#'                                         'Site A','Site A'),
#'                              'count' = c(15, 24, 100, 93, 47, 65,
#'                                          33, 92, 153, 122, 5, 99,
#'                                          10, 30),
#'                              'time_start'=c('2018-01-01', '2018-02-01',
#'                                 '2018-03-01', '2018-04-01', '2018-05-01',
#'                                 '2018-06-01', '2018-07-01', '2018-08-01',
#'                                 '2018-09-01', '2018-10-01', '2018-11-01',
#'                                 '2018-12-01', '2019-01-01', '2019-02-01'),
#'                              'time_increment' = c('month','month','month',
#'                                    'month', 'month','month', 'month','month',
#'                                    'month','month','month','month','month',
#'                                    'month'))
#' # execute 'anomalization' from timetk package to find anomalies
#' anomalize_ss_anom_la(fot_input_tbl = sample_ss_la_input %>%
#'                          dplyr::mutate(time_start = as.Date(time_start)),
#'                      grp_vars = c('site','variable'),
#'                      time_var = 'time_start',
#'                      var_col = 'count')
#'
#'
#' @export
#'
#' @importFrom timetk anomalize
#'
anomalize_ss_anom_la <- function(fot_input_tbl,
                                 grp_vars,
                                 time_var,
                                 var_col){

  time_inc <- fot_input_tbl %>% ungroup() %>% filter(!is.na(time_increment)) %>%
    distinct(time_increment) %>% pull()

  if(time_inc == 'year'){

    final_tbl <- fot_input_tbl

  }else{

    plt_tbl <- compute_at_cross_join(cj_tbl = fot_input_tbl,
                                     cj_var_names = grp_vars,
                                     join_type = 'full')

    anomalize_tbl <- anomalize(plt_tbl %>% group_by(!!!syms(grp_vars)),
                               .date_var=!!sym(time_var),
                               .value=!!sym(var_col))

    final_tbl <- plt_tbl %>%
      left_join(anomalize_tbl) %>%
      ungroup()

  }

  return(final_tbl)

}

#' *_ms_anom_cs Anomaly Detection Eligibility
#'
#' This function will, for each group in a dataframe, identify groups that are eligible
#' for anomaly detection analysis by examining the values in the `var_col`.
#' The following conditions will disqualify a group from the anomaly detection analysis:
#'  (1) Sample size < 5 in group
#'  (2) Mean < 0.02 or Median < 0.01
#'  (3) Mean value < 0.05 and range <0.01
#'  (4) Coefficient of variance <0.01 and sample size <11
#' If no groups meet this criteria, a warning will display in the console indicating
#' that no groups were eligible.
#'
#' @param df_tbl output from the computation of a particular function for anomaly detection
#' @param grp_vars the columns to group by to compute the summary statistics for
#' @param var_col column to perform summary statistics for, to detect an anomaly
#' @param denom_cols denominator (or other) columns that should be preserved without nulls after
#'                   the cross_join takes place
#'
#' @return the `df_tbl` with the following computed:
#'  `mean_val`, `median_val`, `sd_val`, `mad_val`, `cov_val`, `max_val`,
#'  `min_val`, `range_val`, `total_ct`, `analysis_eligible`
#'  the `analysis_eligible` will indicate whether the group for which the user
#'  wishes to detect an anomaly for is eligible for analysis.
#'
#' @examples
#' # create sample input (modeled after EVP)
#' sample_ms_input <- dplyr::tibble('site' = c('Site A', 'Site A', 'Site A',
#'                                             'Site A', 'Site B', 'Site B',
#'                                             'Site B', 'Site B'),
#'                                  'variable' = c('dx', 'dx', 'drug', 'drug',
#'                                                 'dx', 'dx', 'drug', 'drug'),
#'                                  'count' = c(100, 140, 39, 42, 137, 111,
#'                                              12, 15),
#'                                  'total_var' = c(1000, 1000, 200, 200, 1500,
#'                                                  1500, 100, 100))
#' # execute the full analysis, including compute_dist_anomalies and
#' # detect_outliers
#' anomaly_output1 <- compute_dist_anomalies(df_tbl = sample_ms_input,
#'                                           grp_vars = 'variable',
#'                                           var_col = 'count',
#'                                           denom_cols = 'total_var')
#'
#' anomaly_output2 <- detect_outliers(df_tbl = anomaly_output1,
#'                                    column_analysis = 'count',
#'                                    column_variable = 'variable')
#'
#' @export
#'
compute_dist_anomalies <- function(df_tbl,
                                   grp_vars,
                                   var_col,
                                   denom_cols){

  site_rows <-
    df_tbl %>% ungroup() %>% select(site) %>% distinct()
  grpd_vars_tbl <- df_tbl %>% ungroup() %>% select(!!!syms(grp_vars)) %>% distinct()
  denom_tbl <- df_tbl %>% ungroup() %>% select(site, !!!syms(denom_cols)) %>% distinct()

  tbl_new <-
    cross_join(site_rows,
               grpd_vars_tbl) %>%
    left_join(denom_tbl) %>%
    left_join(df_tbl) %>%
    mutate(across(where(is.numeric), ~replace_na(.x,0)))


  stats <- tbl_new %>%
    group_by(!!!syms(grp_vars))%>%
    summarise(mean_val=mean(!!!syms(var_col)),
              median_val=median(!!!syms(var_col)),
              sd_val=sd(!!!syms(var_col), na.rm=TRUE),
              mad_val=mad(!!!syms(var_col)),
              cov_val=sd(!!!syms(var_col),na.rm=TRUE)/mean(!!!syms(var_col)),
              max_val=max(!!!syms(var_col)),
              min_val=min(!!!syms(var_col)),
              range_val=max_val-min_val,
              total_ct=n()) %>% ungroup() %>%
    ungroup() %>% mutate(analysis_eligible =
                           case_when(mean_val < 0.02 | median_val < 0.01 |
                                       (mean_val < 0.05 & range_val < 0.1) |
                                       (cov_val < 0.1 & total_ct < 11) ~ 'no',
                                     TRUE ~ 'yes'))
  final <- tbl_new %>% left_join(stats,
                                 by=c(grp_vars))

  return(final)


}



#' *_ms_anom_cs Anomaly Detection
#'
#' This function will identify anomalies in a dataframe using the
#' [hotspots::outliers()] function. It assumes:
#' (1) No time component;
#' (2) Table has a column indicating whether a particular group or row is eligible for analysis;
#' (3) column variable exists for which to compute the anomaly
#' These conditions are met by the output of [compute_dist_anomalies()], which is typically
#' the input for this function
#'
#' @param df_tbl tbl for analysis; usually output from [compute_dist_anomalies()]
#' @param tail_input whether to detect anomaly on right, left, or both sides; defaults to `both`
#' @param p_input the threshold for anomaly; defaults to 0.9
#' @param column_analysis a string, which the name of the column for which to compute anomaly detection;
#' @param column_variable a string, which is the name of the variable to compute summary statistics for;
#' @param column_eligible a string, which is the name of the column that indicates eligibility for analysis
#'
#' @examples
#' # create sample input (modeled after EVP)
#' sample_ms_input <- dplyr::tibble('site' = c('Site A', 'Site A', 'Site A',
#'                                             'Site A', 'Site B', 'Site B',
#'                                             'Site B', 'Site B'),
#'                                  'variable' = c('dx', 'dx', 'drug', 'drug',
#'                                                 'dx', 'dx', 'drug', 'drug'),
#'                                  'count' = c(100, 140, 39, 42, 137, 111,
#'                                              12, 15),
#'                                  'total_var' = c(1000, 1000, 200, 200, 1500,
#'                                                  1500, 100, 100))
#' # execute the full analysis, including compute_dist_anomalies and
#' # detect_outliers
#' anomaly_output1 <- compute_dist_anomalies(df_tbl = sample_ms_input,
#'                                           grp_vars = 'variable',
#'                                           var_col = 'count',
#'                                           denom_cols = 'total_var')
#'
#' anomaly_output2 <- detect_outliers(df_tbl = anomaly_output1,
#'                                    column_analysis = 'count',
#'                                    column_variable = 'variable')
#'
#' @export
#'
#'
detect_outliers <- function(df_tbl,
                            tail_input = 'both',
                            p_input = 0.9,
                            column_analysis = 'prop_concept',
                            column_eligible = 'analysis_eligible',
                            column_variable = 'concept_id') {

  final <- list()

  eligible_outliers <-
    df_tbl %>% filter(!! sym(column_eligible) == 'yes')

  if(nrow(eligible_outliers) == 0){

    output_final_all <- df_tbl %>% mutate(anomaly_yn = 'no outlier in group')

    cli::cli_warn('No variables were eligible for anomaly detection analysis')

  }else{

    groups_analysis <- group_split(eligible_outliers %>% unite(facet_col, !!!syms(column_variable), sep = '_', remove = FALSE) %>%
                                     group_by(facet_col))

    for(i in 1:length(groups_analysis)) {

      # filtered <-
      #   eligible_outliers %>% filter(!!! syms(column_variable) == i)

      vector_outliers <-
        groups_analysis[[i]] %>% select(!! sym(column_analysis)) %>% pull()

      outliers_test <-
        hotspots::outliers(x=vector_outliers, p=p_input, tail= tail_input)

      output <- groups_analysis[[i]] %>% mutate(
        lower_tail = outliers_test[[10]],
        upper_tail = outliers_test[[9]]
      ) %>% mutate(anomaly_yn = case_when(!! sym(column_analysis) < lower_tail |
                                            !! sym(column_analysis) > upper_tail ~ 'outlier',
                                          TRUE ~ 'not outlier'))

      final[[i]] <- output


    }

    final

    output_final_anomaly <- purrr::reduce(.x=final,
                                          .f=dplyr::union)

    output_final_all <- df_tbl %>% left_join(output_final_anomaly) %>%
      mutate(anomaly_yn=case_when(
        is.na(anomaly_yn) ~ 'no outlier in group',
        TRUE ~ anomaly_yn
      )) %>% select(-facet_col)
  }

  return(output_final_all)
}


#' Jaccard Index Computation
#'
#' This function will compute the Jaccard Similarity Index for each combination of
#' two variables that occur within a specific patient's record. This function is compatible
#' with both the OMOP and PCORnet CDMs based on the user's selection.
#'
#' @param jaccard_input_tbl tbl that will undergo jaccard index computation;
#'                          the requirement is that it contains at least two columns: `person_id`/`patid` and a variable column
#'                          where each row represents an instance where a specific variable is used for a given patient
#'                          Alternatively, it can be a list of all unique `person_id`/`patid` and variable combinations
#' @param var_col the column within `jaccard_input_table` that contains all the concepts that should be compared to each other
#' @param omop_or_pcornet indication of which CDM should be used - either `omop` or `pcornet`; this will determine
#'                        which person identifier column will be used
#'
#' @return a table with both variables, labeled `concept1` and `concept2`, the co-occurrence (`cocount`), individual
#'         concept counts (`concept1_ct`, `concept2_ct`), total unique patient counts where either code is used (`concept_count_union`),
#'         the `jaccard_index`, as well as proportion of patients where the concept appears (`concept1_prop`, `concept2_prop`)
#'
#' @examples
#' # create sample input with person identifier and variable associations
#' sample_input <- dplyr::tibble('person_id' = c(1,1,2,2,3,4,5,5),
#'                               'variable' = c('dx', 'drug', 'drug', 'dx',
#'                                              'drug', 'dx', 'dx', 'drug'))
#' # compute jaccard index
#' compute_jaccard(jaccard_input_tbl = sample_input,
#'                 var_col = 'variable',
#'                 omop_or_pcornet = 'omop')
#'
#'
#' @export
#'
#' @import tidyr
#'
compute_jaccard <- function(jaccard_input_tbl,
                            var_col,
                            omop_or_pcornet) {

  match_class <- function(x, type = var_class) {class(x) <- type; x}
  if(omop_or_pcornet == 'omop'){person_col <- 'person_id'}else{person_col <- 'patid'}

  persons_concepts <-
    jaccard_input_tbl %>% ungroup %>% #distinct() %>% collect()
    select(!!sym(person_col),
           var_col) %>% distinct() %>% collect()

  var_class <- class(persons_concepts[[var_col]])

  persons_concepts_cts <-
    persons_concepts %>%
    group_by(!!sym(var_col)) %>%
    summarise(var_person_ct=n_distinct(!!sym(person_col)))

  concord <-
    persons_concepts %>% table() %>% crossprod()
  diag(concord) <- -1

  best <- as_tibble(concord, rownames='concept1') %>%
    pivot_longer(!concept1, names_to = 'concept2', values_to='cocount') %>%
    filter(cocount != -1L) %>% mutate(across(.cols = c(concept1, concept2), .fns=match_class)) %>%
    mutate(cocount = as.integer(cocount)) %>%
    left_join(persons_concepts_cts, by = c('concept1'=var_col))%>%
    rename(concept1_ct=var_person_ct)%>%
    left_join(persons_concepts_cts, by = c('concept2'=var_col))%>%
    rename(concept2_ct=var_person_ct) %>%
    mutate(concept_count_union=concept1_ct+concept2_ct-cocount,
           jaccard_index=cocount/concept_count_union) %>%
    mutate(concept1_prop=round(cocount/concept1_ct,2),
           concept2_prop=round(cocount/concept2_ct,2)) %>%
    filter(concept1_ct > 0 & concept2_ct > 0 & cocount > 0) %>%
    filter(concept1 > concept2)

  best

}
