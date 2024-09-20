## code to prepare `sample_domain_file` dataset goes here

sample_domain_file <- tibble(domain = c('conditions (omop)', 'outpatient visits (omop)', 'procedures (pcornet)'),
                             domain_tbl = c('condition_occurrence', 'visit_occurrence', 'procedures'),
                             source_col = c('condition_source_concept_id', 'visit_source_concept_id', 'raw_px'),
                             concept_col = c('condition_concept_id', 'visit_concept_id', 'px'),
                             date_col = c('condition_start_date', 'visit_start_date', 'px_date'),
                             filter_logic = c(NA, 'visit_concept_id == 9202', NA),
                             vocabulary_col = c(NA, NA, 'px_type'))

usethis::use_data(sample_domain_file, overwrite = TRUE)
