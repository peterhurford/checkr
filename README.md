## Validations

R is a dynamically typed language. This is pretty great for writing code quickly, but bad for architecturing large systems.

While [some have tried admirably](https://github.com/zatonovo/lambda.r), it seems like a bad idea to enforce a lot of static checks on R functions.  However, we can still be explicit about our preconditions and postconditions when writing R functions, to adopt a solid functional style.

Things still die on runtime instead of compile-time, which is sad, but functions are more explicit about what they do and less tests need to be written -- better than the status quo!

```R
library(validations)

#' Add two numbers.
#'
#' @param x numeric. The number to add.
#' @param y numeric. The number to add.
add <- ensure(
  pre = list(x %is% numeric, y %is% numeric),
  post = result %is% numeric,  # `result` matches whatever the function returns.
  function(x, y) x + y)
add(1, 2)
# 3
add("a", 2)
# Error on x %is% numeric
add("a", "b")
# Error on x %is% numeric, y %is% numeric.
```

```R
#' Generate a random string.
#'
#' @param length numeric. The length of the random string to generate.
#' @param alphabet character. A list of characters to draw from to create the string.
random_string <- ensure(
  pre = list(length %is% numeric, length(length) == 1, length > 0,
    alphabet %is% list || alphabet %is% vector,
    alphabet %contains_only% simple_string,
    all(sapply(alphabet, nchar) == 1)),
  post = list(result %is% simple_string, nchar(result) == length),
  function(length, alphabet) {
    paste0(sample(alphabet, length, replace = TRUE), collapse = "")
  })
```

#### Installation

This package is not yet available from CRAN.  Instead, it can be installed using [devtools](http
://www.github.com/hadley/devtools):

```R
if (!require("devtools")) { install.packages("devtools") }
devtools::install_github("peterhurford/validations")
```

-

Inspired by [Cobra](http://cobra-language.com/).

Similar to the [ensurer](https://github.com/smbache/ensurer) package (and I think these two packages would work well together), but I didn't remember that package existed until now.
