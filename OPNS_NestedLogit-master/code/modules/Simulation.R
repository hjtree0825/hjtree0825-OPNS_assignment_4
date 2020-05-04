library('fExtremes')

#U_{ik}= V_{ik}+ \epsilon_k + \lambda_k \eta_{ik}
#utility of choosing item i in buket k

#Nested: \epsilon_k (bucket) resolves first then \eta_{ik} (item) resolves
sim_nested <- . %>% { n_sim <- .$n_sim # values(val, lambda), n_sim
  .$values %>% 
  group_by(bucket) %>% 
    summarise(value_bf_eps= unique(lambda)* log(sum(exp(val/lambda)))) %>% #lambda needs to be a scalar; thus unique(lambda)
    merge(tibble(sim_id= seq(n_sim))) %>% #cartesian product
    arrange(bucket) %>%  #to generate rdm numbers by bucket
    mutate(value= value_bf_eps+ rgev(n(), xi= 0, mu= 0, beta=1)) %>% #k = argmax \lambda*LSE + \epsilon_k #\epsilon_k resolves
    group_by(sim_id) %>% 
    dplyr::filter(value== max(value)) %>% #nest chosen
    select(-value_bf_eps, -value) %>% 
    
      left_join(values, by='bucket') %>% 
      arrange(sim_id) %>% 
      mutate(utility= val+ lambda* rgev(n(), xi= 0, mu= 0, beta= 1)) %>% 
      dplyr::filter(utility== max(utility)) %>% 
      select(sim_id, bucket, choice) %>% 
      #mutate(case= 'nested') %>% 
    ungroup
}

sim_oneshot <- function(values, n_sim) {
  values %>%
  select(bucket) %>% 
  unique %>% 
  merge(tibble(sim_id= seq(n_sim))) %>% 
  mutate(eps= rgev(n(), xi= 0, mu= 0, beta=1)) %>% 
  left_join(values, by='bucket') %>% 
  mutate(eta= rgev(n(), xi= 0, mu= 0, beta=1), utility= val +eps +lambda*eta) %>% 
  group_by(sim_id) %>% 
  dplyr::filter(utility== max(utility)) %>% 
  select(sim_id, bucket, choice) %>% 
  ungroup
}


# OLD FUNCTIONS

#############################################################
# Traditional (sequential) approach
#############################################################
# 1st stage: choosing the bucket
trad_app <- function(df, person_id){
  
  #inc_val_df <- sqldf("select distinct(bucket) from df")
  
  # I know we should not have done it this way (hard-coding); there has to be a way to do this without using hard-coding and/or loops
  #inc_val_df$inc_val[1] <- df$lambda[1] * log(sum(exp(df$val[1]/df$lambda[1]) + exp(df$val[4]/df$lambda[1]) + exp(df$val[7]/df$lambda[1])))
  #inc_val_df$inc_val[2] <- df$lambda[2] * log(sum(exp(df$val[2]/df$lambda[2]) + exp(df$val[5]/df$lambda[2]) + exp(df$val[8]/df$lambda[2])))
  #inc_val_df$inc_val[3] <- df$lambda[3] * log(sum(exp(df$val[3]/df$lambda[3]) + exp(df$val[6]/df$lambda[3]) + exp(df$val[9]/df$lambda[3])))
  
  inc_val_df <- df %>%
    group_by(bucket) %>%
    summarise(inc_val = unique(lambda) * log(sum(exp(val/lambda))))
  
  df_combined <- sqldf("select * from df left join inc_val_df on df.bucket = inc_val_df.bucket")
  df_bucket <-
    df_combined %>%
    select(-c(bucket..5)) %>%
    distinct(bucket, inc_val)
  
  bucket_choice <- merge(df_bucket, data.frame(row_id = seq(person_id))) %>%
    group_by(bucket) %>%
    arrange(bucket) %>%
    mutate(inter_util = inc_val + rgev(n(), xi = 0, mu = 0, beta = 1)) %>%
    group_by(row_id) %>%
    dplyr::filter(inter_util == max(inter_util)) %>%
    arrange(row_id)
  
  # 2nd stage: choosing the item, conditional on the selected bucket
  final_choice <- sqldf("select * from bucket_choice left join df on bucket_choice.bucket = df.bucket") %>%
    select(-c(bucket..5)) %>%
    mutate(final_util = val + lambda * rgev(n(), xi = 0, mu = 0, beta = 1)) %>%
    group_by(row_id) %>%
    dplyr::filter(final_util == max(final_util)) %>%
    select(row_id, bucket, choice)
  
  return(final_choice)
}

#############################################################
# Single-shot (simultaneous) approach
#############################################################

single_shot <- function(df, person_id){
  inter_df <- sqldf("select distinct(bucket) from df") %>%
    merge(data.frame(row_id = seq(person_id))) %>%
    mutate(epsilon = rgev(n(), xi = 0, mu = 0, beta = 1))
  
  final_df <- sqldf("select * from inter_df left join df on inter_df.bucket = df.bucket") %>%
    select(-c(bucket..4)) %>%
    mutate(eta = rgev(n(), xi = 0, mu = 0, beta = 1)) %>%
    mutate(final_util = val + epsilon + lambda*eta) %>%
    group_by(row_id) %>%
    dplyr::filter(final_util == max(final_util)) %>%
    select(row_id, bucket, choice)
  
  return(final_df)
}


