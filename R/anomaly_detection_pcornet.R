
#' Compute Jaccard Index
#'
#' @param jaccard_input_tbl tbl that will undergo jaccard index computation;
#'                          the requirement is that it contains at least two columns: `person_id` and `concept_id`
#'                          where each row represents an instance where a specific `concept_id` is used for a given patient (`person_id`)
#'                          Alternatively, it can be a list of all unique `person_id` and `concept_id` combinations
#' @param var_col the column within `jaccard_input_table` that contains all the concepts that should be compared to each other
#'
#' @return a table with both concepts, labeled `concept1` and `concept2`, the co-occurrence (`cocount`), individual
#'         concept counts (`concept1_ct`, `concept2_ct`), total unique patient counts where either code is used (`concept_count_union`),
#'         the `jaccard_index`, as well as proportion of patients where the concept appears (`concept1_prop`, `concepet2_prop`)
#'
#' @export
#'
#'
compute_jaccard_pcnt <- function(jaccard_input_tbl,
                                 var_col) {

  match_class <- function(x, type = var_class) {class(x) <- type; x}

  persons_concepts <-
    jaccard_input_tbl %>% ungroup %>% #distinct() %>% collect()
    select(patid,
           var_col) %>% distinct() %>% collect()

  var_class <- class(persons_concepts[[var_col]])

  persons_concepts_cts <-
    persons_concepts %>%
    group_by(!!sym(var_col)) %>%
    summarise(var_person_ct=n_distinct(patid))

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
