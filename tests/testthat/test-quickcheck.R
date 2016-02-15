context("quickcheck")
library(validations)

test_that("simple success example", {
  add_one <- ensure(
    pre = x %is% numeric,
    post = result %is% numeric,
    function(x) x + 1) 
  quickcheck(add_one)
})

test_that("simple failure example I", {
  add_one <- ensure(
    pre = x %is% numeric,
    post = result %is% character,
    function(x) x + 1) 
  expect_error(quickcheck(add_one), "Quickcheck for add_one failed on item #1")
})

test_that("simple failure example II", {
  add_one <- ensure(
    pre = x %is% character,
    post = result %is% numeric,
    function(x) x + 1) 
  expect_error(quickcheck(add_one), "Quickcheck for add_one failed on item #1")
})
