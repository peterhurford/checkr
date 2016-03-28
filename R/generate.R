# The OBJECTS constant holds all the possible objects to gather into a testing frame.
# It's basically a staging area for our random madness.
default_objects <- memoise::memoise(function() {
  OBJECTS <- list(
    empties           = list(NA, NULL, "", character(0), logical(0), numeric(0), integer(0),
                          data.frame(), list(), matrix(), c(), structure(NA, class = "table"),
                          factor(NA)),
    positive_doubles  = c(seq(100), 1000, 100000, 2147483647),
    logicals          = c(TRUE, FALSE),
    characters        = c(letters, LETTERS),
    utf8              = setdiff(lapply(seq(1000L), intToUtf8), c(letters, LETTERS))
  )
  OBJECTS$positive_integers <- vapply(OBJECTS$positive_doubles, as.integer, integer(1))
  OBJECTS$negative_integers <- OBJECTS$positive_integers * -1L
  OBJECTS$positive_doubles <- append(OBJECTS$positive_doubles, c(1e18, 1e100))
  OBJECTS$negative_doubles <- OBJECTS$positive_doubles * -1
  OBJECTS$matricies <- lapply(seq(12), function(n) random_matrix(OBJECTS))
  dataframes <- installed_dataframes()
  OBJECTS$dataframes <- Filter(is.data.frame, dataframes)
  OBJECTS$factors <- Filter(is.factor, dataframes)
  OBJECTS$table <- Filter(is.table, dataframes)
  OBJECTS
})
#TODO: Maybe someday we can also check functions, environments, and some custom structs.

#' Get all the classes within a list.
#' @param object ANY. The object to check classes for.
list_classes <- function(object) {
  classes <- unique(sapply(object, class))
  if (identical(classes, "list")) {
    unique(sapply(object, function(sl) { sapply(sl, class) }))
  } else { classes }
}

#' Generate a vector or list of random objects from a particular set of possible choices.
#' @param objects list. The list of objects to generate from.
#' @param amount numeric. The amount of objects to generate.
#' @param list_max_length numeric. What is the maximum size of a given vector or list?
random_objs <- function(objects, amount, list_max_length = 50) {
  lengths <- sample(seq(list_max_length), amount, replace = TRUE)
  lapply(lengths, function(l) { sample(objects, l, replace = TRUE) })
}

#' Generate a random simple string (i.e., a length-1 non-empty vector of characters).
#' @param amount numeric. The amount of simple strings to generate.
#' @param chars logical. Whether or not to include characters.
#' @param utf8 logical. Whether or not to include utf8 characters.
random_simple_strings <- function(amount, chars = TRUE, utf8 = FALSE, objects) {
  objs <- list()
  if (isTRUE(chars)) { objs <- append(objs, objects$characters) }
  if (isTRUE(utf8)) { objs <- append(objs, objects$utf8) }
  lapply(random_objs(objs, amount), function(str) paste0(str, collapse = ""))
}

#' Generate a random matrix.
#'
#' A random matrix needs three random things...
#' A random width, a random height, and a random data
#' data should be a random assortment of integers, doubles, logicals, or characters, with
#'   all of them being the same class.
#' Because there are so many possible matricies, it seems easier to generate them on
#' demand rather than preallocate all possible matricies into default_objects().
#' We will then populate some random matricies onto default_objects() for later use.
random_matrix <- function(objects) {
  random_width <- sample(seq(30L), 1)
  random_height <- sample(seq(30L), 1)
  matrix_classes <- c("integer", "double", "logical", "character", "simple_string")
  random_data_class <- sample(matrix_classes, 1)
  sample_data <- function(data) { sample(data, random_width * random_height, replace = TRUE) }
  random_data <- switch(random_data_class,
    integer       = sample_data(c(objects$negative_integers, objects$positive_integers)),
    double        = sample_data(c(objects$negative_doubles, objects$positive_doubles)),
    logical       = sample_data(objects$logicals),
    character     = sample_data(c(objects$characters, objects$utf8)),
    simple_string = sample_data(random_simple_strings(random_width * random_height,
      objects = objects)))
  matrix(random_data, random_width, random_height)
}

#' Get all the user-installed dataframes through data()
installed_dataframes <- function() {
  take_only_part_of_name_before_the_space <- function(name) {
    if (grepl(" ", name, fixed = TRUE)) { strsplit(name, " ")[[1]][[1]] }
    else { name }}
  dataframe_names <- lapply(apply(data()$results, 1, `[[`, "Item"),
    take_only_part_of_name_before_the_space)
  dataframes <- lapply(dataframe_names, function(df) try(get(df), silent = TRUE))
  names(dataframes) <- dataframe_names
  dataframes
}


#' Generates random R objects to be put into functions for testing purposes.
test_objects_ <- function(objects) {
  testing_frame <- list()
  # start with one of each at random
  testing_frame <- append(testing_frame, lapply(objects, function(type) {
    random_obj <- sample(type, 1)
    if (random_obj %is% list) { random_obj[[1]] } else { random_obj }
  }))
  LIST_SIZE <- 4         # How many different lists of the same kind should be made?
  # construct random-length vectors or lists of all types
  #     (depending on class, vectors when possible)
  testing_frame <- append(testing_frame, lapply(objects, function(type) {
    random_objs(type, LIST_SIZE)
  }))
  # construct random-length vectors of mixed positive and negative doubles; integers
  testing_frame <- append(testing_frame,
    random_objs(c(objects$positive_doubles, 0, objects$negative_doubles), LIST_SIZE))
  testing_frame <- append(testing_frame,
    random_objs(c(objects$positive_integers, 0L, objects$negative_integers), LIST_SIZE))
  # construct random-length vectors of single characters
  testing_frame <- append(testing_frame, random_objs(objects$characters, LIST_SIZE))
  testing_frame <- append(testing_frame, random_objs(objects$utf8, LIST_SIZE))
  testing_frame <- append(testing_frame,
    random_objs(c(objects$characters, objects$utf8), LIST_SIZE))
  # construct random-length simple strings
  testing_frame <- append(testing_frame,
    random_simple_strings(LIST_SIZE, chars = TRUE, utf8 = FALSE, objects = objects))
  testing_frame <- append(testing_frame,
    random_simple_strings(LIST_SIZE, chars = FALSE, utf8 = TRUE, objects = objects))
  testing_frame <- append(testing_frame,
    random_simple_strings(LIST_SIZE, chars = TRUE, utf8 = TRUE, objects = objects))
  # construct random-length vectors of simple strings
  LIST_MAX_LENGTH <- 50  # What is the maximum size of a given vector or list?
  testing_frame <- append(testing_frame, lapply(seq(LIST_SIZE), function(n) {
    unlist(random_simple_strings(sample(seq(LIST_MAX_LENGTH), 1),
      chars = TRUE, utf8 = FALSE, objects = objects)) }))
  testing_frame <- append(testing_frame, lapply(seq(LIST_SIZE), function(n) {
    unlist(random_simple_strings(sample(seq(LIST_MAX_LENGTH), 1),
      chars = FALSE, utf8 = TRUE, objects = objects)) }))
  testing_frame <- append(testing_frame, lapply(seq(LIST_SIZE), function(n) {
    unlist(random_simple_strings(sample(seq(LIST_MAX_LENGTH), 1),
      chars = TRUE, utf8 = TRUE, objects = objects)) }))
  # copy some of the vectors but make them lists
  testing_frame <- append(testing_frame,
    lapply(Filter(Negate(is.list), testing_frame), as.list))
  # construct random-length lists of mixed doubles and integers
  testing_frame <- append(testing_frame, random_objs(
      c(as.list(objects$positive_doubles), as.list(objects$positive_integers)), LIST_SIZE))
  # construct lists that mix empties into all of the above
  testing_frame <- append(testing_frame,
    lapply(Filter(Negate(is.empty), Filter(Negate(is.list), testing_frame)), function(item) {
      sample(append(item, NA))
    }))
  testing_frame <- append(testing_frame,
    lapply(Filter(is.list, testing_frame), function(item) {
      sample(append(item, sample(objects$empties, 1)))
    }))
  # make lists that randomly mix all of the above
  testing_frame <- append(testing_frame,
    lapply(seq(LIST_SIZE), function(n) {
      tail(lapply(unname(
        Map(c, sample(testing_frame, 1), sample(testing_frame, 1))
      ), sample), LIST_MAX_LENGTH) }))
  # and we're done!
  testing_frame
}

test_objects <- memoise::memoise(function(objects = default_objects()) {
  testing_frame <- list()
  GENERATIONS <- 3       # How many times should the test generation be repeated?
  for (generation in seq(GENERATIONS)) {
    testing_frame <- append(testing_frame, test_objects_(objects))
  }
  testing_frame
})

#' Function to force reload the test object cache, if needed.
force_reload_test_objects <- function() {
  memoise::forget(test_objects)
  memoise::forget(default_objects)
  test_objects(default_objects())
  TRUE
}
