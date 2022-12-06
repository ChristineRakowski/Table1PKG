test_that("test if counts for level 1 of outcome in cat_fun works", {
        expect_equal(cat_fun(data=FEV, outcome="smoke", var="sex")[[1]]$female,
                     hash(keys=c("out1", "out2", "overall"),
                          values=c("279 (47 %)", "39 (60 %)", "318 (49 %)")))
}
)

test_that("test if counts for level 2 of outcome in cat_fun works", {
        expect_equal(cat_fun(data=FEV, outcome="smoke", var="sex")[[1]]$male,
                     hash(keys=c("out1", "out2", "overall"),
                          values=c("310 (53 %)", "26 (40 %)", "336 (51 %)")))
}
)

test_that("test if para_pval of cat_fun works", {
        expect_equal(cat_fun(data=FEV, outcome="smoke", var="sex")[[2]], 0.071403956)
}
)

test_that("test if nonpara_pval of numeric_fun works", {
        expect_equal(cat_fun(data=FEV, outcome="smoke", var="sex")[[3]], 0.066610595)
}
)
