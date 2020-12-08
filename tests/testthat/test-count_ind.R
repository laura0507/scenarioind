library(scenarioind)
library(checkmate)

test_that("creates correct dataframe", {
  data("ind_scenarios")
  out <- count_ind(x=ind_scenarios, t="NCP")
  
  expect_true(any(is(out) == "data.frame"))
  expect_equal(ncol(out),2)
  expect_vector(out$NCP)
  expect_integer(x= out$n)
})
