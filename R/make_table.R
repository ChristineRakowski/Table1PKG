#' Description: To create table
##'
##' We want to run through all covariates given and create table
##' @name make_table
##' @title Make table
##' @param data data set
##' @param outcome categorical grouping variable
##' @param cat_vec vector of categorical covariates in the data set
##' @param num_vec vector of numerical covariates in the data set
##' @return table summary
##' @author C. Rakowski
##' @export
##'
##'
library(hash)
library(kableExtra)
make_table <- function(data, outcome, num_vec, cat_vec){
        # dictionary for storing all variable info
        # create keys for each variable
        var_names <- hash(keys=c(num_vec, cat_vec), 
                          values=c(num_vec, cat_vec)) 
        
        # store numeric info
        for(i in 1:length(num_vec)){
                temp <- numeric_fun(data=data, outcome=outcome, var=num_vec[i])
                var_names[num_vec[i]] <- hash(c("N", "mean", "median", "para_p", "nonpara_p"), 
                                           temp[-1])
        }
        
        #store categorical info
        for(i in 1:length(cat_vec)){
                temp <- cat_fun(data=data, outcome=outcome, var=cat_vec[i])
                var_names[cat_vec[i]] <- hash(c("cont_table",  "para_p", "nonpara_p"), 
                                           temp)
        }
        #return(var_names)
        var_names
        dat<- matrix(ncol=5)
        colnames(dat) <- c("var", "out1", "out2", "para p", "nonpara p")
        for(i in 1:length(num_vec)){
                dat <- rbind(dat, c(num_vec[i],
                            var_names[[num_vec[i]]]$N[1], 
                            var_names[[num_vec[i]]]$N[2],
                            var_names[[num_vec[i]]]$para_p,
                            var_names[[num_vec[i]]]$nonpara_p))
                dat <- rbind(dat, c(num_vec[i],
                            var_names[[num_vec[i]]]$mean[1], 
                            var_names[[num_vec[i]]]$mean[2],
                            var_names[[num_vec[i]]]$para_p,
                            var_names[[num_vec[i]]]$nonpara_p))
                rbind(dat, c(num_vec[i],
                     var_names[[num_vec[i]]]$median[1], 
                     var_names[[num_vec[i]]]$median[2],
                     var_names[[num_vec[i]]]$para_p,
                     var_names[[num_vec[i]]]$nonpara_p))
        }


        for(i in 1:length(cat_vec)){
                for(v in ls(var_names[[cat_vec[i]]]$cont_table)){
                        dat <- rbind(dat, c(cat_vec[i],
                                    var_names[[cat_vec[i]]]$cont_table[[v]]$out1, 
                                    var_names[[cat_vec[i]]]$cont_table[[v]]$out2,
                                    var_names[[cat_vec[i]]]$para_p,
                                    var_names[[cat_vec[i]]]$nonpara_p))
                        }
        }
        # make the table!
        return(dat)
}

