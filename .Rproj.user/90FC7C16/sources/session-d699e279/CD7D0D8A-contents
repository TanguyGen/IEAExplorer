
for (i in seq_along(var_test)){
  res <- data_test[[i]]
  expect_no_error(ggATAC(results=res,width=20))
}

test_that("there are no errors in ggATAC", {
  expect_no_error(ggATAC(results=res,width=20))
})

test_that("ggATAC makes a plot", {
  expect_true(inherits(ggATAC(results=res,width=20),
                       "ggplot"))
})