makeData <- 
  . %>% {
    inner_join(
      expand.grid(
        bucket = .$my.buckets, 
        choice = .$my.choices
      ) %>%
        mutate(val = rnorm(n())),
      tibble(bucket = .$my.buckets) %>% 
        mutate(lambda = runif(n()))
    )
  }
