##' Description: To produce summary stats for a categorical variable
##'
##' We want to produce a contingency table and test of differences
##' @name numeric_fun
##' @title Numeric summary
##' @param data data set
##' @param outcome categorical grouping variable, string
##' @param var name of categorical covariate in the data set, string
##' @return vector of contingency table, and p-value for test of differences (nonpar and par)
##' @author C. Rakowski
##' @export
##'
##'
library(hash)
library(tidyverse)
cat_fun <- function(data, outcome, var){
        # contingency table
        temp <- data %>%
                group_by(!!as.symbol(outcome), !!as.symbol(var)) %>%
                tally() %>%
                spread(!!as.symbol(outcome), n)
        # store levels of covariate and counts for contingency table
        temp <- as.matrix(temp)
        n<- length(temp[,1])
        cat_info <- hash(temp[,1], temp[,1])
        for(i in 1:n){
              cat_info[[temp[i,1]]] <- hash(c("out1", "out2"), c(temp[i,2], temp[i, 3]))
        }


        # tests of differences
        temp <- as.matrix(temp)
        # chi-square
        chisq <- chisq.test(temp[,2], temp[,3])
        para_pval <- chisq$p.value
        # can use $expected to check if we should use fisher's instead

        # Fisher's exact test
        fish <- fisher.test(temp[,2], temp[,3])
        nonpara_pval <- fish$p.value
        return(c(cat_info, para_pval, nonpara_pval))
}

