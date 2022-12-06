##' Summary statistics for a categorical covariate and a two-level outcome
##'
##' @description `cat_fun()` will produce the components of contingency table
##' and a test of differences via parametric p-value (Chi-Square) and non-parametric p-value
##' (Fisher's Exact test). These summaries can be used in a crude preliminary Table 1 to share
##' with researchers in the beginning stages of analysis.
##' @name cat_fun
##' @title Categorical summary
##' @param data data set with a two-level outcome and numerical/categorical covariates
##' @param outcome name of categorical grouping variable, must have two levels, string
##' @param var name of categorical covariate in the data set, may have any number of levels, string
##' @param digits for rounding on percents, default is 2
##' @returns A list of three items.
##' * `cat_info` is a hash with keys = levels of `var` and values = another hash
##' with keys = levels of `outcome` and values = counts of `var` by `outcome`.
##' * `para_pval` is the parametric p-value for test of differences of `var` using a
##' Chi-square test
##' * `nonpara_pval` is a non-parametric p-value for test of differences of `var` using
##' Fisher's Exact test
##' @author C. Rakowski
##' @import dplyr
##' @import tidyr
##' @import hash
##' @import stats
##' @export
##' @examples
##' cat_fun(data=FEV, outcome="smoke", var="sex")


cat_fun <- function(data, outcome, var, digits = 2) {
    # 'counts' = contingency table; col2 is for outcome level 1, col3 is for outcome level 2
    # `counts_wpercent` includes percents
    counts_wpercent <- data %>%
        group_by(!!as.symbol(var), !!as.symbol(outcome)) %>%
        mutate(n = n()) %>%
        group_by(!!as.symbol(outcome)) %>%
        distinct(!!as.symbol(outcome), !!as.symbol(var), n) %>%
        mutate(Per = n/sum(n), np = paste0(n, " (", round(Per * 100, 0), " %)")) %>%
        select(-n, -Per) %>%
        spread(!!as.symbol(outcome), np)

    # now do counts for total (optional in table)
    overall <- data %>%
            group_by(!!as.symbol(var)) %>%
            mutate(n = n()) %>%
            distinct(!!as.symbol(var), n) %>%
            ungroup() %>%
            mutate(Per = n/sum(n), Overall = paste0(n, " (", round(Per * 100, 0), " %)")) %>%
            select(-n, -Per)

    #combine the overall and the table by group
    counts_wpercent <- cbind(counts_wpercent, overall[,-1])

    # done a second time with just counts by outcome level for testing 2x2 contingency table
    counts <- data %>%
        group_by(!!as.symbol(outcome), !!as.symbol(var)) %>%
        tally() %>%
        spread(!!as.symbol(outcome), n)
    counts <- as.data.frame(counts)
    counts[, 2] <- as.numeric(counts[, 2])
    counts[, 3] <- as.numeric(counts[, 3])

    # store info for output in later table this includes the levels of covariate and
    # counts/percents for contingency table length of first column for number of levels
    n <- length(counts_wpercent[, 1])
    # store counts/percent for each level of var in a dictionary with key equal to the level of var
    cat_info <- hash(unlist(lst(counts[,1])), unlist(lst(counts[,1])))
    for (i in 1:n) {
        cat_info[unlist(lst(counts[,1]))[i]] <- hash(c("out1", "out2", "overall"),
                        c(counts_wpercent[i, 2], counts_wpercent[i, 3], counts_wpercent[i, 4]))
    }


    # tests of differences chi-square
    chisq <- chisq.test(counts[, 2:3])
    para_pval <- chisq$p.value
    # can use $expected to check if we should use fisher's instead

    # Fisher's exact test
    fish <- fisher.test(counts[, 2:3])
    nonpara_pval <- fish$p.value
    return(c(cat_info, para_pval, nonpara_pval))
}

