#tidy up sim data

library(tidyverse)

tidy_simdata <-function(data){
  
  data %>%
    pivot_longer(cols = c('mean', 'var', 'skew', 'kurt'),
                 names_to = 'moment',
                 values_to = 'estimate') %>%
    pivot_longer(cols = contains('ci_high'),
                 names_to = 'ci_high_moment',
                 values_to = 'ci_high') %>%
    mutate(ci_high_moment = str_to_lower(str_extract(ci_high_moment,'[[:alpha:]]*$'))) %>%
    filter(ci_high_moment == moment) %>%
    pivot_longer(cols = contains('ci_low'),
                 names_to = 'ci_low_moment',
                 values_to = 'ci_low') %>%
    mutate(ci_low_moment = str_to_lower(str_extract(ci_low_moment,'[[:alpha:]]*$'))) %>%
    filter(ci_low_moment == moment) %>%
    pivot_longer(cols = contains('true'),
                 names_to = 'true_moment',
                 values_to = 'true_value') %>%
    mutate(true_moment = str_to_lower(str_extract(true_moment,'[[:alpha:]]*$')),
           moment = case_when(moment == 'var' ~ 'variance',
                              moment == 'kurt' ~ 'kurtosis',
                              moment == 'skew' ~ 'skewness',
                              TRUE ~ moment),
           method = case_when(method == 'global cwm' ~ 'Cross-Site CWM',
                              method == 'site-specic CWM' ~ 'Site-Specific CWM',
                              method == 'nonparametric bs' ~ 'Non-Parametric BS',
                              method == 'parametric bs' ~ 'Parametric BS',
                              TRUE ~ method)) %>%
    filter(true_moment == moment) %>%
    select(-c(global, true_moment, ci_low_moment, ci_high_moment)) %>%
    mutate(method = ordered(method,levels = c("Cross-Site CWM",
                                              "Site-Specific CWM",
                                              "Parametric BS",
                                              "Non-Parametric BS")))
}