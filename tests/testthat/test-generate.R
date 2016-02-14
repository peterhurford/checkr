context("generate")

desired_classes <- c("numeric", "integer", "list", "character", "logical",
  "matrix", "data.frame", "NULL")
testing_frame <- test_objects()

describe("OBJECTS", {
  test_that("OBJECTS has one of every class", {
    found_classes <- unique(unname(unlist(lapply(OBJECTS, list_classes))))
    report <- vapply(desired_classes,
      function(klass) klass %in% found_classes, logical(1))
    error <- paste(paste0(names(which(!report)), collapse = ", "), "not found among OBJECTS")
    expect_true(all(report), info = error)
  })
  test_that("The empties are one of every class, plus NULL-class", {
    found_classes <- list_classes(OBJECTS$empties)
    report <- vapply(desired_classes,
      function(klass) klass %in% found_classes, logical(1))
    error <- paste(paste0(names(which(!report)), collapse = ", "), "not found among empties")
    expect_true(all(report), info = error)
  })
  test_that("The negative integers for OBJECTS work right", {
    expect_true(all(OBJECTS$negative_integers < 0))
    expect_equal("integer", unique(sapply(OBJECTS$negative_integers, class)))
  })
})

describe("test_objects", {
  test_that("the testing_frame has one of every class", {
    found_classes <- unique(unname(unlist(lapply(testing_frame, list_classes))))
    report <- vapply(desired_classes,
      function(klass) klass %in% found_classes, logical(1))
    error <- paste(paste0(names(which(!report)), collapse = ", "),
      "not found among testing_frame")
    expect_true(all(report), info = error)
  })
  test_that("the function is memoised", {
    expect_true(microbenchmark::microbenchmark(test_objects(), times = 1)$time < 1000000)
  })
})
