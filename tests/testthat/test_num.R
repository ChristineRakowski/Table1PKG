test_that("test if mean of numeric_fun works", {
        expect_equal(numeric_fun(data=FEV, outcome="smoke", var="age")$mean[1:2], c(9.53, 13.52))
}
)

test_that("test if median of numeric_fun works", {
        expect_equal(numeric_fun(data=FEV, outcome="smoke", var="age")$median[1:2], c(9, 13))
}
)

test_that("test if N of numeric_fun works", {
        expect_equal(numeric_fun(data=FEV, outcome="smoke", var="age")$N[1:2], c(589, 65))
}
)

test_that("test if para_pval of numeric_fun works", {
        expect_equal(numeric_fun(data=FEV, outcome="smoke", var="age")[[5]],4.18878800579493e-27)
}
)

test_that("test if nonpara_pval of numeric_fun works", {
        expect_equal(numeric_fun(data=FEV, outcome="smoke", var="age")[[6]],1.17612842969352e-22)
}
)
