context("generate")

desired_classes <- c("numeric", "integer", "list", "character", "logical",
  "matrix", "data.frame", "NULL", "table", "factor")
objects <- checkr:::default_objects()
testing_frame <- checkr:::test_objects()

describe("objects", {
  test_that("objects has one of every class", {
    found_classes <- unique(unname(unlist(lapply(objects, list_classes))))
    report <- vapply(desired_classes,
      function(klass) klass %in% found_classes, logical(1))
    error <- paste(paste0(names(which(!report)), collapse = ", "), "not found among objects")
    expect_true(all(report), info = error)
  })
  test_that("The empties are one of every class, plus NULL-class", {
    found_classes <- list_classes(objects$empties)
    report <- vapply(desired_classes,
      function(klass) klass %in% found_classes, logical(1))
    error <- paste(paste0(names(which(!report)), collapse = ", "), "not found among empties")
    expect_true(all(report), info = error)
  })
  test_that("The negative integers for objects work right", {
    expect_true(all(objects$negative_integers < 0))
    expect_equal("integer", unique(sapply(objects$negative_integers, class)))
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
    expect_true(memoise::is.memoised(test_objects))
    expect_true(microbenchmark::microbenchmark(test_objects(), times = 1)$time < 5000000)
  })
  test_that("force_reload_test_objects reloads test objects", {
    expect_false(memoise::is.memoised(force_reload_test_objects))
    testing_frame1 <- test_objects()
    testing_frame2 <- test_objects()
    expect_true(force_reload_test_objects())
    testing_frame3 <- test_objects()
    expect_identical(testing_frame1, testing_frame2)
    expect_false(identical(testing_frame1, testing_frame3))
    expect_false(identical(testing_frame2, testing_frame3))
  })
})
