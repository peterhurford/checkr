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

test_that("it can be vectorized", {
  expect_equal(seq(5) %within% c(1, 3), c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(c(1, 2, 3) %within% c(1, 1), c(TRUE, FALSE, FALSE))
  expect_equal(c(1, 2, 3) %within% c(0, 0), c(FALSE, FALSE, FALSE))
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

test_that("quickcheck vectorized", {
  checkr::quickcheck(checkr::ensure(
    pre = list(length(a) > 1,
      b %is% numeric, length(b) == 1,
      c %is% numeric, length(c) == 1,
      all(a >= b) && all(b <= c)),
    post = all(result),
    function (a, b, c) { a %within% c(b, c) })
  , frame = list(a = replicate(100, sample(seq(100)), simplify = FALSE),
    b = sample(seq(0, 100)), c = sample(seq(80, 1000))))
})
