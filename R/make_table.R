##' Summary table for multiple covariates and a two-level outcome
##'
##' @description `make_table()` will create a rough summary table that should be readable enough
##' to share as a preliminary step when discussing with researchers.
##' Numerical variables will have mean, median, and counts while categorical variables will
##' have contingency tables. Two columns on the right will show the parametric and non-parametric
##' p-values for a test of differences. Parametric p-values will come from an ANOVA test for
##' numerical variables and a Chi-square test for categorical variables. Non-parametric p-values
##' will be from a Kruskal-Wallis test for numerical variables and Fisher's Exact test for
##' categorical variables.
##' @name make_table
##' @title Make table
##' @param data data set with a two-level outcome and numerical/categorical covariates
##' @param outcome categorical grouping variable, must have two levels, string
##' @param cat_vec vector of names of categorical covariates (strings) in the data set
##' @param num_vec vector of names of numerical covariates (strings) in the data set
##' @param digits number of digits to display, default is 2
##' @return A table.
##' First two columns will have variables names, levels, and names of descriptive statistics.
##' Third and fourth columns will have the summary statistics for levels 1 and 2 of the outcome.
##' Fifth and sixth columns will have parametric and non-parametric p-values from a test of differences.
##' @author C. Rakowski
##' @import dplyr
##' @import knitr
##' @import hash
##' @import kableExtra
##' @export
##' @examples
##' make_table(data=FEV, outcome="smoke", cat_vec="sex", num_vec=c("age", "height", "fev"))


make_table <- function(data, outcome, num_vec, cat_vec, digits =2){
        # dictionary for storing all variable info
        # create keys for each variable
        var_names <- hash(keys=c(num_vec, cat_vec),
                          values=c(num_vec, cat_vec))

        # store numeric info
        for(v in unlist(lst(num_vec))){
                temp <- numeric_fun(data=data, outcome=outcome, var=v)
                var_names[v] <- hash(c("N", "mean", "median", "para_p", "nonpara_p"),
                                           temp[-1])
        }

        #store categorical info
        for(v in unlist(lst(cat_vec))){
                temp <- cat_fun(data=data, outcome=outcome, var=v)
                var_names[v] <- hash(c("cont_table",  "para_p", "nonpara_p"),
                                           temp)
        }


        #make data frame for table
        dat<- data.frame(matrix(c("var", "type",
                                  as.integer(1), as.integer(1),
                                  as.numeric(1.00), as.numeric(1.00)),
                                ncol=6)
                         )
        colnames(dat) <- c("var", "type", "out1", "out2", "para_p", "nonpara_p")
        # store numeric rows
        for(v in unlist(lst(num_vec))){
                v.para_p <- var_names[[v]]$para_p
                v.nonpara_p <- var_names[[v]]$nonpara_p
                #counts
                dat <- rbind(dat, c(v,
                                    "N",
                                    var_names[[v]]$N[1],
                                    var_names[[v]]$N[2],
                                    v.para_p,
                                    v.nonpara_p))
                # means
                # blanks for grouping purposes since collapes_rows didn't work
                dat <- rbind(dat, c(v,
                                    "Mean",
                                    round(var_names[[v]]$mean[1], digits),
                                    round(var_names[[v]]$mean[2], digits),
                                    "",
                                    ""))

                # medians
                # blanks for grouping purposes since collapes_rows didn't work
                dat <- rbind(dat, c(v,
                                    "Median",
                                    round(var_names[[v]]$median[1], digits),
                                    round(var_names[[v]]$median[2], digits),
                                    "",
                                    ""))

        }

        # store categorical rows
        for(v in unlist(lst(cat_vec))){
                i<-1
                for(w in ls(var_names[[v]]$cont_table)){
                        # for loop for w, each level of the cat variable
                        # blanks for grouping purposes since collapes_rows didn't work
                        dat <- rbind(dat,
                                     c(v, w,
                                       var_names[[v]]$cont_table[[w]]$out1,
                                       var_names[[v]]$cont_table[[w]]$out2,
                                       ifelse(i==1,var_names[[v]]$para_p, ""),
                                       ifelse(i==1,var_names[[v]]$nonpara_p, "")
                                       )
                                     )
                        i <- i+1
                        }
        }

        # format table
        # remove placeholder first row from initialization
        dat <- dat[-1,]

        #format p-values
        dat$para_p <- as.numeric(dat$para_p)
        dat$nonpara_p <- as.numeric(dat$nonpara_p)
        dat<- dat %>% mutate(para_p = if_else(para_p < 0.0001,
                        "p <0.0001",
                        if_else(para_p < 0.01,
                                formatC(para_p, digits = 2, format = "e"),
                                formatC(para_p, digits = 2, format = "f", drop0trailing = FALSE)
                        )
                        )
                        ) %>%
                mutate(nonpara_p = if_else(nonpara_p < 0.0001,
                        "p <0.0001",
                        if_else(para_p < 0.01,
                                formatC(nonpara_p, digits = 2, format = "e"),
                                formatC(nonpara_p, digits = 2, format = "f", drop0trailing = FALSE)
                                )
                        )
                       )


        # use knitr for the table
        options(knitr.kable.NA = '')
        k <- kbl(dat[,-1],
                 align = "c",
                 col.names=c("",
                             levels(data[[outcome]])[1],
                             levels(data[[outcome]])[2],
                             "Parametric p-value",
                             "Non-parametric p-value"),
                 row.names = FALSE) %>%
                kableExtra::kable_paper() %>%
                kableExtra::pack_rows(index = table(dat$var)) %>%
                footnote(symbol = c("Parametric p-values for numeric variables are from Chi-squared tests",
                                    "Non-parametric p-values for numeric variables are from Kruskal-Wallis tests",
                                    "Parametric p-values for categorical variables are from ANOVA",
                                    "Non-parametric p-values for categorical variables are from Fisher's Exact tests")
                         )

        return(k)
}

