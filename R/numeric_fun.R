##' Description: To produce summary stats for a numeric variable
##'
##' We want to produce sample size, mean, median, and test of differences
##' @name numeric_fun
##' @title Numeric summary
##' @param data data set
##' @param outcome categorical grouping variable, string
##' @param var name of numerical covariate in the data set, string
##' @return vector of sample size, mean, median, and p-value for test of differences (nonpar and par)
##' @author C. Rakowski
##' @export
##'
##'
library(tidyverse)
numeric_fun <- function(data, outcome, var){
        # summary stats
        temp <- data %>%
                group_by(!!as.symbol(outcome)) %>%
                summarize(N=n(),
                          mean=mean(!!as.symbol(var)),
                          median=median(!!as.symbol(var)))

        # tests of differences
        # create formula
        frm <- paste(var,outcome, sep="~")

        # aov
        #perform aov and store p-value
        test<- aov(formula(frm), data)
        sum_test = unlist(summary(test))
        para_pval <- sum_test["Pr(>F)1"]

        # Kruskal-Wallis non-parametric rank-sum test
        test<- kruskal.test(formula(frm), data = data)
        sum_test = unlist(test)
        nonpara_pval <- sum_test["p.value"]
        return(c(temp, para_pval, nonpara_pval))
}

