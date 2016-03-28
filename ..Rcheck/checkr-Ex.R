pkgname <- "checkr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('checkr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("ensure")
### * ensure

flush(stderr()); flush(stdout())

### Name: ensure
### Title: Ensure checks that certain preconditions and postconditions of a
###   function are true.
### Aliases: ensure

### ** Examples

  add <- ensure(pre = list(x %is% numeric, y %is% numeric),
    post = list(result %is% numeric),
    function(x, y) { x + y })



cleanEx()
nameEx("grapes-contains-grapes")
### * grapes-contains-grapes

flush(stderr()); flush(stdout())

### Name: %contains%
### Title: Test if a list contains some elements of the desired class.
### Aliases: %contains% %contains_only%

### ** Examples

  list(1, 2, 3) %contains% numeric
  list(1, 2, "a") %contains% numeric
  list(1, 2, 3) %contains_only% numeric
  list(1, 2, "a") %contains_only% numeric



cleanEx()
nameEx("grapes-does_not_contain-grapes")
### * grapes-does_not_contain-grapes

flush(stderr()); flush(stdout())

### Name: %does_not_contain%
### Title: Test if a list does not contain some elements of the desired
###   class.
### Aliases: %does_not_contain%

### ** Examples

  list(1, 2, 3) %does_not_contain% character



cleanEx()
nameEx("grapes-is-grapes")
### * grapes-is-grapes

flush(stderr()); flush(stdout())

### Name: %is%
### Title: Test for class membership
### Aliases: %is% %isnot%

### ** Examples

  1 %is% numeric
  1.0 %is% double
  1L %is% integer
  iris %is% dataframe
  c("a", "b", "c") %is% vector
  "pizza" %is% simple_string
  list(a = "pizza", b = "pie") %is% c("character", "list")



cleanEx()
nameEx("is.empty")
### * is.empty

flush(stderr()); flush(stdout())

### Name: is.empty
### Title: Tests whether an object is empty.
### Aliases: is.empty

### ** Examples

  is.empty(NULL)
  is.empty(NA)
  is.empty(list(NULL, NA))
  is.empty(list())
  is.empty(c())
  is.empty(data.frame())
  is.empty("")
  is.empty(data.frame())



cleanEx()
nameEx("is.simple_string")
### * is.simple_string

flush(stderr()); flush(stdout())

### Name: is.simple_string
### Title: Tests whether a string is simple.
### Aliases: is.simple_string

### ** Examples

  is.simple_string("pizza")              # true
  is.simple_string(c("pizza", "apple"))  # false
  is.simple_string(iris)                 # false
  is.simple_string(NA)                   # false



cleanEx()
nameEx("print_args")
### * print_args

flush(stderr()); flush(stdout())

### Name: print_args
### Title: Print function arguments
### Aliases: print_args

### ** Examples

l <- list(x = seq(3), y = seq(4))
print_args(l)
[1] "x = 1:3, y = 1:4"



cleanEx()
nameEx("validate")
### * validate

flush(stderr()); flush(stdout())

### Name: validate
### Title: Validate checks that certain facts are true.
### Aliases: validate

### ** Examples

  validate(1 == 1, "a" %is% character, length(c(1, 2, 3)) == 3)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
