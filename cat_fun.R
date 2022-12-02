##' Description: To produce summary stats for a categorical variable
##'
##' We want to produce a contingency table and test of differences
##' @name numeric_fun
##' @title Numeric summary
##' @param data data set
##' @param outcome categorical grouping variable
##' @param var name of categorical covariate in the data set
##' @return vector of contingency table, and p-value for test of differences (nonpar and par)
##' @author C. Rakowski
##' @export
##'
##'
library(tidyverse)
cat_fun <- function(data, outcome, var){
        # need {{ }} to use dplyr with input variable names
        # contingency table
        temp <- data %>%
                group_by({{outcome}}, {{var}}) %>%
                tally() %>%
                spread({{outcome}}, n)
        
        # tests of differences
        # chi-square
        chisq <- chisq.test(as.matrix(temp)[,2], as.matrix(temp)[,3])
        para_pval <- chisq$p.value
        # can use $expected to check if we should use fisher's instead
        
        # Fisher's exact test
        fish <- fisher.test(as.matrix(temp)[,2], as.matrix(temp)[,3])
        nonpara_pval <- fish$p.value
        return(c(temp, para_pval, nonpara_pval))
}

