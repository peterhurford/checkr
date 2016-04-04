context("pacakge_exports_checked")

test_that("it passes when all exported functions are checked", {
  devtools::install("fakepackages/allexportedchecked")
  expect_true(package_exports_checked("allexportedchecked"))
  remove.packages("allexportedchecked")
})

test_that("it fails when all exported functions are not checked", {
  devtools::install("fakepackages/notallexportedchecked")
  expect_false(package_exports_checked("notallexportedchecked", stop = FALSE))
  expect_error(package_exports_checked("notallexportedchecked"),
    "not checked by checkr: pending_identity")
  remove.packages("notallexportedchecked")
})
