library('rsample')

boot_sample <- function(sim, n_bstrap){
  sim %>%
  rsample::bootstraps(n_bstrap) %>%
  .[['splits']] %>% #extract a list of bootstrapped sample
  data.frame %>% {
    tibble(
      sim_id= select(., contains('sim_id')) %>% unlist,
      bucket= select(., contains('bucket')) %>% unlist,
      choice= select(., contains('choice')) %>% unlist,
      boot_id= rep(seq(n_bstrap), each=nrow(sim))
      ) %>% 
      
    group_by(boot_id, bucket, choice) %>% 
    count %>% 
    mutate(choice_prob= n/nrow(sim)) %>%
    select(-n) %>% 
    pivot_wider(names_from= boot_id, values_from= choice_prob) %>% 
    ungroup %>% 
    replace(is.na(.), 0)
  }
}

est_prob <- function(sampled){
  est_probability <- sampled %>%
    group_by(bucket, choice) %>%
    count() %>%
    mutate(prob = n/nrow(sampled)) %>%
    select(bucket, choice, prob)
  
  return(est_probability)
}

theoretical_prob <- function(df){
  prob <- df %>%
    mutate(v = exp(val/lambda)) %>%
    group_by(bucket) %>%
    mutate(inner_sum = sum(v)) %>%
    mutate(interim_prob = v * (inner_sum ^ (lambda - 1))) %>%
    ungroup() %>%
    mutate(final_prob = interim_prob / sum(unique(inner_sum)^unique(lambda))) %>%
    select(bucket, choice, final_prob)
  
  return(prob)
}

cov_fcn <- function(boot_prob){
  boot_prob %>% 
    select(-bucket, -choice) %>% 
    cov %>% 
    .[-nrow(.), -ncol(.)]
}


wald_stat <- function(boot_sample, df){
  vec <- boot_sample %>%
    mutate(est_prob = rowMeans(.[-c(1:2)])) %>%
    left_join(
      theoretical_prob(df), by = c("bucket", "choice")
    ) %>%
    mutate(diff = est_prob - final_prob) %>%
    select(diff) %>%
    .[-nrow(.),]
  
  cov_mat <- cov_fcn(boot_sample)
  
  w <- t(as.matrix(vec)) %*% cov_mat %*% (as.matrix(vec))
  
  pval <- pchisq(w, df = nrow(vec), lower.tail = FALSE)
  
  return(pval)
}

