context("within")

test_that("within", {
  expect_true(1 %within% c(0, 2))
  expect_true(1 %within% c(0, 1.5))
  expect_true(1 %within% c(-10, 2))
  expect_true(1 %within% c(1, 2))
  expect_true(1 %within% c(0, 1))
  expect_true(1 %within% c(1, 1))

  expect_false(1 %within% c(0, 0))
  expect_false(1 %within% c(-1, 0))
  expect_false(1 %within% c(0, 0.9))
})

test_that("quickcheck I", {
  checkr::quickcheck(checkr::ensure(
    pre = list(a %is% numeric, length(a) == 1,
      b %is% numeric, length(b) == 1,
      c %is% numeric, length(c) == 1,
      a >= b, a <= c),
    post = isTRUE(result),
    function (a, b, c) { a %within% c(b, c) }
  ), frame = list(a = sample(seq(1000)), b = sample(seq(1000)), c = sample(seq(1000))))
})

test_that("quickcheck II", {
  checkr::quickcheck(checkr::ensure(
    pre = list(a %is% numeric, length(a) == 1,
      b %is% numeric, length(b) == 1,
      c %is% numeric, length(c) == 1,
      a < b),
    post = !isTRUE(result),
    function (a, b, c) { a %within% c(b, c) }
  ), frame = list(a = sample(seq(1000)), b = sample(seq(1000)), c = sample(seq(1000))))
})

test_that("quickcheck III", {
  checkr::quickcheck(checkr::ensure(
    pre = list(a %is% numeric, length(a) == 1,
      b %is% numeric, length(b) == 1,
      c %is% numeric, length(c) == 1,
      a > c),
    post = !isTRUE(result),
    function (a, b, c) { a %within% c(b, c) }
  ), frame = list(a = sample(seq(1000)), b = sample(seq(1000)), c = sample(seq(1000))))
})
