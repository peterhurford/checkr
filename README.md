## Validations

R is a dynamically typed language. This is pretty great for writing code quickly, but bad for architecturing large systems.

While [some have tried admirably](https://github.com/zatonovo/lambda.r), it seems like a bad idea to enforce a lot of static checks on R functions.  However, we can still be explicit about our preconditions and postconditions when writing R functions, to adopt a solid functional style.

```R
#' Add two numbers.
#'
#' @param x numeric. The number to add.
#' @param y numeric. The number to add.
add <- validations::ensure(pre = list(x %is% numeric, y %is% numeric), function(x, y) x + y)
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
  pre = list(length %is% numeric,
    alphabet %is% list || alphabet %is% vector,
    alphabet %contains_only% simple_string,
    length > 0),
  post = list(result %is% simple_string, nchar(result) == length),
  function(length, alphabet) {
    paste0(sample(alphabet, 10, replace = TRUE), collapse = "")
  })
```

Inspired by [Cobra](http://cobra-language.com/).
