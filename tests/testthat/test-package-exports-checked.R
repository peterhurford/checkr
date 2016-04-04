context("pacakge_exports_checked")

test_that("it passes when all exported functions are checked", {
  expect_true(package_exports_checked("fakepackages/all_exported_checked"))
})

test_that("it fails when all exported functions are not checked", {
  expect_true(package_exports_checked("fakepackages/not_all_exported_checked"))
})
