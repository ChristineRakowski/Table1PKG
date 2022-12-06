##' Summary statistics for a numerical covariate and a two-level outcome
##'
##' @description `numeric_fun()` will produce the sample size, mean, median
##' and a test of differences via parametric p-value (ANOVA) and non-parametric p-value
##' (Kruskal-Wallis test). These summaries can be used in a crude preliminary Table 1 to share
##' with researchers in the beginning stages of analysis.
##' @name numeric_fun
##' @title Numeric summary
##' @param data data set with a two-level outcome and numerical/categorical covariates
##' @param outcome name of categorical grouping variable, must have two levels, string
##' @param var name of numerical covariate in the data set, string
##' @param span boolean, for whether to include StD ; default is FALSE
##' @param digits for rounding on percents, default is 2
##' @return A list of six items.
##' * the `outcome` variable and its levels
##' * `N` a vector of counts of `var`, sorted by levels of `outcome` and then overall
##' * `mean` a vector of means of `var`, sorted by levels of `outcome` and then overall;
##' includes (SD) if `span` = TRUE
##' * `median` a vector of medians of `var`, sorted by levels of `outcome` and then overall;
##' includes Q1 and Q3 if `span` = TRUE
##' * `para_pval` is the parametric p-value for test of differences of `var` using an
##' ANOVA test
##' * `nonpara_pval` is a non-parametric p-value for test of differences of `var` using
##' the Kruskal-Wallis test
##'
##' @author C. Rakowski
##' @import dplyr
##' @import stats
##' @export
##' @examples
##' numeric_fun(data=FEV, outcome="smoke", var="age")


numeric_fun <- function(data, outcome, var, digits=2, span=FALSE) {
        # summary stats
        # by outcome group
        num_info <- data %>%
                group_by(!!as.symbol(outcome)) %>%
                dplyr::summarise(N = n(),
                                mean = mean(!!as.symbol(var)),
                                median = median(!!as.symbol(var)),
                                sd = sd(!!as.symbol(var)),
                                q1 = quantile(!!as.symbol(var), 0.25),
                                q3 = quantile(!!as.symbol(var), 0.75)) %>%
                rowwise() %>%
                mutate(mean = ifelse(span==TRUE,
                                        paste0(round(mean, digits),
                                               " (", round(sd, digits), ")"),
                                        round(mean, digits)),
                        median = ifelse(span==TRUE,
                                        paste0(round(median, digits),
                                               " [", round(q1, digits),
                                               ", ", round(q3, digits), "]"),
                                        round(median, digits))) %>%
                select(-c(sd, q1, q3))

        # overall
        overall <- data %>%
            dplyr::summarise(N = n(),
                             mean = mean(!!as.symbol(var)),
                             median = median(!!as.symbol(var)),
                             sd = sd(!!as.symbol(var)),
                             q1 = quantile(!!as.symbol(var), 0.25),
                             q3 = quantile(!!as.symbol(var), 0.75)) %>%
            mutate(smoke="overall",
                   mean = ifelse(span==TRUE,
                                 paste0(round(mean, digits), " (", round(sd, digits), ")"),
                                 round(mean, digits)),
                   median = ifelse(span==TRUE,
                                   paste0(round(median, digits),
                                          " [", round(q1, digits),
                                          ", ", round(q3, digits), "]"),
                                   round(median, digits))) %>%
            select(smoke, N, mean, median)

        #combine group summary and overall summary into one list
        num_info <- rbind(num_info, overall)

        # tests of differences create formula
        frm <- paste(var, outcome, sep = "~")

        # aov perform aov and store p-value
        test <- aov(formula(frm), data)
        sum_test = unlist(summary(test))
        para_pval <- sum_test["Pr(>F)1"]

        # Kruskal-Wallis non-parametric rank-sum test
        test <- kruskal.test(formula(frm), data = data)
        sum_test = unlist(test)
        nonpara_pval <- as.numeric(sum_test["p.value"])
        return(c(num_info, para_pval, nonpara_pval))
}

