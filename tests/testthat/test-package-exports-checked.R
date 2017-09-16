context("package_exports_checked")

test_that("it passes when all exported functions are checked", {
  devtools::install("fakepackages/allexportedchecked")
  expect_true(package_exports_checked("allexportedchecked"))
  expect_true(package_exports_checked("fakepackages/allexportedchecked"))
  remove.packages("allexportedchecked")
  unloadNamespace("notallexportedchecked")
})

test_that("it fails when all exported functions are not checked", {
  devtools::install("fakepackages/notallexportedchecked")
  expect_false(package_exports_checked("notallexportedchecked", stop = FALSE))
  expect_error(package_exports_checked("notallexportedchecked"),
    "not checked by checkr: pending_identity")
  expect_false(package_exports_checked("fakepackages/notallexportedchecked", stop = FALSE))
  remove.packages("notallexportedchecked")
  unloadNamespace("notallexportedchecked")
})

test_that("for example, it doesn't work on checkr", {
  expect_false(package_exports_checked("checkr", stop = FALSE))
  expect_error(package_exports_checked("checkr"), "not checked by checkr")
})
