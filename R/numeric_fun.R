##' Description: To produce summary stats for a numeric variable
##'
##' We want to produce sample size, mean, median, and test of differences
##' @name numeric_fun
##' @title Numeric summary
##' @param data data set
##' @param outcome categorical grouping variable
##' @param var name of numerical covariate in the data set
##' @return vector of sample size, mean, median, and p-value for test of differences (nonpar and par)
##' @author C. Rakowski
##' @export
##' 
##' 
library(tidyverse)
numeric_fun <- function(data, outcome, var){
        outcome <- enquo(outcome)       # quote
        var <- enquo(var)               # quote
        data %>% 
                group_by(!!outcome) %>%
                summarize(N= n(), mean=mean(!!var), median=median(!!var))
}

