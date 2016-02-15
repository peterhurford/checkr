LIST_MAX_LENGTH <- 50  # What is the maximum size of a given vector or list?
LIST_SIZE <- 4         # How many different lists of the same kind should be made?
GENERATIONS <- 3       # How many times should the test generation be repeated?

# The OBJECTS constant holds all the possible objects to gather into a testing frame.
# It's basically a staging area for our random madness.
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

#' Get all the classes within a list.
list_classes <- function(object) {
  classes <- unique(sapply(object, class))
  if (identical(classes, "list")) {
    unique(sapply(object, function(sl) { sapply(sl, class) }))
  } else { classes }
}

#' Generate a vector or list of random objects from a particular set of possible choices.
random_objs <- function(objects, amount) {
  lengths <- sample(seq(LIST_MAX_LENGTH), amount, replace = TRUE)
  lapply(lengths, function(l) { sample(objects, l, replace = TRUE) })
}

#' Generate a random simple string (i.e., a length-1 non-empty vector of characters).
random_simple_strings <- function(amount, chars = TRUE, utf8 = FALSE) {
  objs <- list()
  if (isTRUE(chars)) { objs <- append(objs, OBJECTS$characters) }
  if (isTRUE(utf8)) { objs <- append(objs, OBJECTS$utf8) }
  lapply(random_objs(objs, amount), function(str) paste0(str, collapse = ""))
}

#' Generate a random matrix.
#'
#' A random matrix needs three random things...
#' A random width, a random height, and a random data
#' data should be a random assortment of integers, doubles, logicals, or characters, with
#'   all of them being the same class.
#' Because there are so many possible matricies, it seems easier to generate them on
#' demand rather than preallocate all possible matricies into OBJECTS.
#' We will then populate some random matricies onto OBJECTS for later use.
random_matrix <- function() {
  random_width <- sample(seq(30L), 1)
  random_height <- sample(seq(30L), 1)
  matrix_classes <- c("integer", "double", "logical", "character", "simple_string")
  random_data_class <- sample(matrix_classes, 1)
  sample_data <- function(data) { sample(data, random_width * random_height, replace = TRUE) }
  random_data <- switch(random_data_class,
    integer       = sample_data(c(OBJECTS$negative_integers, OBJECTS$positive_integers)),
    double        = sample_data(c(OBJECTS$negative_doubles, OBJECTS$positive_doubles)),
    logical       = sample_data(OBJECTS$logicals),
    character     = sample_data(c(OBJECTS$characters, OBJECTS$utf8)),
    simple_string = sample_data(random_simple_strings(random_width * random_height)))
  matrix(random_data, random_width, random_height)
}
OBJECTS$matricies <- lapply(seq(LIST_SIZE * GENERATIONS), function(n) random_matrix())

#' Get all the user-installed dataframes through data()
installed_dataframes <- function() {
  take_only_part_of_name_before_the_space <- function(name) {
    if (grepl(" ", name, fixed = TRUE)) { strsplit(name, " ")[[1]][[1]] }
    else { name }}
  dataframe_names <- lapply(apply(data()$results, 1, `[[`, "Item"),
    take_only_part_of_name_before_the_space)
  dataframes <- lapply(dataframe_names, get)
  names(dataframes) <- dataframe_names
  dataframes
}
dataframes <- installed_dataframes()
OBJECTS$dataframes <- Filter(is.data.frame, dataframes)
OBJECTS$factors <- Filter(is.factor, dataframes)
OBJECTS$table <- Filter(is.table, dataframes)

#TODO: Maybe someday we can also check functions, environments, and some custom structs.


#' Generates random R objects to be put into functions for testing purposes.
test_objects_ <- function() {
  testing_frame <- list()
  # start with one of each at random
  testing_frame <- append(testing_frame, lapply(OBJECTS, function(type) {
    random_obj <- sample(type, 1)
    if (random_obj %is% list) { random_obj[[1]] } else { random_obj }
  }))
  # construct random-length vectors or lists of all types
  #     (depending on class, vectors when possible)
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
    lapply(Filter(Negate(is.empty), Filter(Negate(is.list), testing_frame)), function(item) {
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
  # and we're done!
  testing_frame
}

test_objects <- memoise::memoise(function() {
  testing_frame <- list()
  for (generation in seq(GENERATIONS)) {
    testing_frame <- append(testing_frame, test_objects_())
  }
  testing_frame
})

#' Function to force reload the test object cache, if needed.
force_reload_test_objects <- function() {
  memoise::forget(test_objects)
  test_objects()
  TRUE
}
