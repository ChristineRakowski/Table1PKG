test_that("test if mean of numeric_fun works", {
        expect_equal(numeric_fun(data=FEV, outcome="smoke", var="age")$mean, c(9.534805, 13.523077))
}
)

test_that("test if median of numeric_fun works", {
        expect_equal(numeric_fun(data=FEV, outcome="smoke", var="age")$median, c(9, 13))
}
)

test_that("test if N of numeric_fun works", {
        expect_equal(numeric_fun(data=FEV, outcome="smoke", var="age")$N, c(589, 65))
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