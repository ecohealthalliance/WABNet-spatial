#' Prep data to create a heatmap of CoV prevalence by bat family and quarter of the year
#'
#'
#' @title prep_quarterly_heatmap_data
#' @param CoV_prev dataset containing info on capture date, bat family, and CoV test result
#' @return 
#' @author Cecilia Sanchez

prep_quarterly_heatmap_data <- function(CoV_prev){
  
  # do some initial data cleaning
  cleaned <- CoV_prev %>% 
    janitor::clean_names() %>% 
    mutate(date = lubridate::dmy(date),
           quarter = as.factor(lubridate::quarter(date))) %>% 
    rename(cov_result = co_v_final_results) %>% 
    filter(!is.na(cov_result)) %>% 
    mutate(cov_result = as.factor(cov_result))
  
  # summarize proportion positive for each family and quarter
  quarter_family_summary <- cleaned %>% 
    group_by(family, quarter, cov_result, .drop = F) %>%
    dplyr::summarize(n = n()) %>%
    mutate(prop = n / sum(n)) %>%
    mutate(totN = sum(n)) %>% 
    filter(cov_result == "Positive")
  
  # also calculate a summary only by quarter, lumping all families together
  quarter_summary <- cleaned %>% 
    group_by(quarter, cov_result, .drop = F) %>%
    dplyr::summarize(n = n()) %>%
    mutate(prop = n / sum(n)) %>%
    mutate(totN = sum(n)) %>% 
    filter(cov_result == "Positive") %>% 
    mutate(family = "Overall")
  
  # bind everything together to plot
  heat_data <- bind_rows(quarter_family_summary, quarter_summary) %>%
    mutate_at(vars(family), as.factor) %>%
    mutate(family = fct_relevel(family, "Overall")) %>% 
    drop_na()
  
}
