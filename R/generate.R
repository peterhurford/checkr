LIST_MAX_LENGTH <- 50
LIST_SIZE <- 4

OBJECTS <- list(
  empties           = list(NA, NULL, "", character(0), logical(0), numeric(0), integer(0),
                        data.frame(), list(), matrix(), c()),
  positive_doubles  = c(seq(100, 1), 1000, 100000, 2147483647),
  logicals          = c(TRUE, FALSE),
  characters        = c(letters, LETTERS),
  utf8              = setdiff(lapply(seq(1000L), intToUtf8), c(letters, LETTERS))
)
OBJECTS$positive_integers <- vapply(OBJECTS$positive_doubles, as.integer, integer(1))
OBJECTS$negative_integers <- OBJECTS$positive_integers * -1L
OBJECTS$positive_doubles <- append(OBJECTS$positive_doubles, c(1e18, 1e100))
OBJECTS$negative_doubles <- OBJECTS$positive_doubles * -1

list_classes <- function(object) {
  classes <- unique(vapply(object, class, character(1)))
  if (identical(classes, "list")) {
    unique(sapply(object, function(sl) { sapply(sl, class) }))
  } else { classes }
}

random_objs <- function(objects, amount) {
  lengths <- sample(seq(LIST_MAX_LENGTH), amount)
  lapply(lengths, function(l) {
    random_list <- lapply(seq(l), function(n) sample(objects, 1))
    if (length(list_classes(random_list)) == 1) { unlist(random_list) }
    else { random_list }
  })
}

random_simple_strings <- function(amount, chars = TRUE, utf8 = FALSE) {
  objs <- list()
  if (isTRUE(chars)) { objs <- append(objs, OBJECTS$characters) }
  if (isTRUE(utf8)) { objs <- append(objs, OBJECTS$utf8) }
  lapply(random_objs(objs, amount), function(str) paste0(str, collapse = ""))
}

#' Generates random R objects to be put into functions for testing purposes.
test_objects <- memoise::memoise(function() {
  testing_frame <- list()
  # start with one of each at random
  testing_frame <- append(testing_frame, lapply(OBJECTS, function(type) sample(type, 1)))
  # construct random-length vectors of all types (empties will be a list)
  testing_frame <- append(testing_frame, lapply(OBJECTS, function(type) {
    random_objs(type, LIST_SIZE)
  }))
  # construct random-length vectors of mixed positive and negative doubles; integers
  testing_frame <- append(testing_frame,
    random_objs(c(OBJECTS$positive_doubles, 0, OBJECTS$negative_doubles), LIST_SIZE))
  testing_frame <- append(testing_frame,
    random_objs(c(OBJECTS$positive_integers, 0L, OBJECTS$negative_integers), LIST_SIZE))
  # construct random-length simple strings
  testing_frame <- append(testing_frame,
    random_simple_strings(LIST_SIZE, chars = TRUE, utf8 = FALSE))
  testing_frame <- append(testing_frame,
    random_simple_strings(LIST_SIZE, chars = FALSE, utf8 = TRUE))
  testing_frame <- append(testing_frame,
    random_simple_strings(LIST_SIZE, chars = TRUE, utf8 = TRUE))
  # construct random-length vectors of simple strings
  testing_frame <- append(testing_frame, lapply(seq(LIST_SIZE), function(n) {
    unlist(random_simple_strings(sample(seq(LIST_MAX_LENGTH), 1), chars = TRUE, utf8 = FALSE)) }))
  testing_frame <- append(testing_frame, lapply(seq(LIST_SIZE), function(n) {
    unlist(random_simple_strings(sample(seq(LIST_MAX_LENGTH), 1), chars = FALSE, utf8 = TRUE)) }))
  testing_frame <- append(testing_frame, lapply(seq(LIST_SIZE), function(n) {
    unlist(random_simple_strings(sample(seq(LIST_MAX_LENGTH), 1), chars = TRUE, utf8 = TRUE)) }))
  # copy some of the vectors but make them lists
  testing_frame <- append(testing_frame,
    lapply(Filter(Negate(is.list), testing_frame), as.list))
  # construct random-length lists of mixed doubles and integers
  testing_frame <- append(testing_frame, random_objs(
      c(as.list(OBJECTS$positive_doubles), as.list(OBJECTS$positive_integers)), LIST_SIZE))
  # construct lists that mix empties into all of the above
  testing_frame <- append(testing_frame,
    lapply(Filter(Negate(is.list), testing_frame), function(item) {
      sample(append(item, NA))
    }))
  testing_frame <- append(testing_frame,
    lapply(Filter(is.list, testing_frame), function(item) {
      sample(append(item, sample(OBJECTS$empties, 1)))
    }))
  # make lists that randomly mix all of the above
  testing_frame <- append(testing_frame,
    lapply(seq(LIST_SIZE), function(n) {
      tail(lapply(unname(
        Map(c, sample(testing_frame, 1), sample(testing_frame, 1))
      ), sample), LIST_MAX_LENGTH) }))
  # make lists of lists of the above
  testing_frame <- append(testing_frame,
    lapply(seq(LIST_SIZE), function(n) {
      list(sample(testing_frame, LIST_SIZE))
    }))
  # TODO: more complex objects (matricies, dataframes, custom class objects)
  testing_frame
})

#' Function to force reload the test object cache, if needed.
force_reload_test_objects <- function() {
  memoise::forget(test_objects)
  test_objects()
  TRUE
}
