setwd('/Users/swnam/Google Drive/code/R/OPNS_NestedLogit/code')

#Nested logit coding assignment
source('header.R')

set.seed(0)

#Generate data
list(
  my.buckets = 1:3, 
  my.choices = c('A', 'B', 'C')
) %>% 
  makeData %>% 
  saveRDS('../variables/values.rds')

list(
  values= readRDS(paste0(var_save, 'values.rds')), #val= V_{ik}; lambda= \lambda_k
  n_sim= 1000
) %>% {
    sim_nested(.)
} %>% boot_sample(., n_bstrap= 1000) %>% 
  cov_fcn
sim_oneshot(values= readRDS(paste0(var_save, 'values.rds')), n_sim= 1000) %>% 
  boot_sample(., n_bstrap= 1000) %>%
  cov_fcn

#map_dbl(., get_split_mean)

# Global parameters
df <- readRDS("values.rds")
person_id = 10000
boot_num = 1000

# Main functions
pval_traditional <- trad_app(df, person_id) %>%
  boot_sample(boot_num) %>%
  wald_stat(df)
pval_simultaneous <- trad_app(df, person_id) %>%
  boot_sample(boot_num) %>%
  wald_stat(df)





