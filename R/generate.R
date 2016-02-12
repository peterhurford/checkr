LIST_MAX_LENGTH <- 100
LIST_SIZE <- 10

OBJECTS <- list(
  empties           = list(NA, NULL, "", character(0), logical(0), numeric(0), integer(0),
                        data.frame(), list(), matrix(), c()),
  positive_doubles = c(seq(100, 0), 1000, 100000, 2147483647),
  logicals          = c(TRUE, FALSE),
  characters        = c(letters, LETTERS)
)
OBJECTS$positive_integers <- vapply(OBJECTS$positive_doubles, as.integer, integer(1))
OBJECTS$negative_integers <- OBJECTS$positive_integers * -1
OBJECTS$positive_doubles <- append(OBJECTS$positive_doubles, c(1e18, 1e100))
OBJECTS$negative_doubles <- OBJECTS$positive_doubles * -1
#TODO: Append symbols to characters

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

random_simple_strings <- function(amount) {
  lapply(random_objs(OBJECTS$characters, amount), function(str) paste0(str, collapse = ""))
}

#' Generates random R objects to be put into functions for testing purposes.
test_objects <- function() {
  testing_frame <- list()
  # start with one of each at random
  testing_frame <- append(testing_frame, lapply(OBJECTS, function(type) sample(type, 1)))
  # construct random-length vectors of all types (empties will be a list)
  testing_frame <- append(testing_frame, lapply(OBJECTS, function(type) {
    random_objs(type, LIST_SIZE)
  }))
  # construct random-length vectors of mixed positive and negative doubles; integers
  testing_frame <- append(testing_frame,
    random_objs(c(OBJECTS$positive_doubles, OBJECTS$negative_doubles), LIST_SIZE))
  testing_frame <- append(testing_frame,
    random_objs(c(OBJECTS$positive_integers, OBJECTS$negative_integers), LIST_SIZE))
  # construct random-length simple strings
  testing_frame <- append(testing_frame, random_simple_strings(LIST_SIZE))
  # construct random-length vectors of simple strings
  testing_frame <- append(testing_frame, lapply(seq(LIST_SIZE),
    function(n) { unlist(random_simple_strings(sample(seq(LIST_MAX_LENGTH), 1))) }))
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
      list(sample(testing_frame, 10))
    }))
  # TODO: more complex objects (matricies, dataframes, custom class objects)
  testing_frame
}
