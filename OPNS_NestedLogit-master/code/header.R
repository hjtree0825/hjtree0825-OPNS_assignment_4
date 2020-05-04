my.library <- '/Users/swnam/Google Drive/code/R/library'
.libPaths(my.library)

setwd('/Users/swnam/Google Drive/code/R/OPNS_NestedLogit/code')

var_save <- '../variables/'

library('tidyverse')
c('reshape2', 'magrittr', 'fExtremes') %>%
  walk(~library(., character.only=TRUE))

dir('modules') %>% 
  walk(~source(paste('./modules/', ., sep="")))

set.seed(1)