## Quickcheck

**This package is a work in progress and is not ready yet.**

R is a dynamically typed language, which is great for writing code quickly but bad for verifying that your code works as intended.  Instead, we need to write formal test suites to ensure our code works.

Thanks to Hadley's [testthat package](https://github.com/hadley/testthat), writing tests for R code [is pretty easy](http://r-pkgs.had.co.nz/).  But writing tests take a long time and it's easy to forget to write certain tests.  And [while coverage tools in R exist](https://github.com/jimhester/covr), 100% test coverage is still insufficient for verifying that your code works.

Quickcheck, inspired by [the Haskell namesake](https://github.com/nick8325/quickcheck) (and in true Haskell style you can [see the corresponding academic paper](http://www.eecs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf)), aims to automatically verify your code through running hundreds of tests that you don't have to write yourself.

Testing made easy AND quick! ...and more accurate!


## Installation

**This package is a work in progress and is not ready yet.**

This package is not yet available from CRAN.  Instead, it can be installed using [devtools](http://www.github.com/hadley/devtools):

```R
if (!require("devtools")) { install.packages("devtools") }
devtools::install_github("peterhurford/quickcheck")
```


## Using Quickcheck

#### The Random String

Imagine you want to generate a random string of a given length from a given possible alphabet of characters.  Your R function might look like this:

```R
random_string <- function(length, alphabet) {
  paste0(sample(alphabet, 10), collapse = "")
}
```

This is a pretty simple function, but it's possible to make an error even on something this simple -- as you can see, we accidentally hardcoded the length as 10 instead of using the built-in `length` parameter (this isn't contrived -- this is a typo [I have made in real life](https://github.com/peterhurford/validations/commit/585af6de4ee25622dfaa665e83106a2398cc946c)).

We may write some tests using testthat:

```R
test_that("It generates a random string from the given alphabet", {
  random_string <- random_string(10, letters)
  all(strsplit(random_string, "")[[1]] %in% letters)
})
test_that("It generates a random string of the given length", {
  random_string <- random_string(10, letters)
  expect_equal(length(random_string), 10)
})
```

But because we were lazy when writing the tests and had 10 in our mind, all the tests pass and we don't catch our error.

Additionally, we don't look for other errors, such as:

(a) Does it work when the alphabet is only a length 1 list?
(b) Does it work when the alphabet is a string?
(c) Does it work when length is a negative number?
(d) Does it work when length is a list?

For example, if we had written a thorough test for (a), we would have noticed that we're using `sample` with `replace = FALSE`, which means that if the `length` is larger than `length(alphabet)`, the function will crash.  We should use `replace = TRUE` instead!

...So we could add all these tests ourselves and be really thorough, or we could use quickcheck and just automatically test some simple properties:

```R
quickcheck(ensure(
  pre = list(length %is% numeric, length(length) == 1, length > 0,
    alphabet %is% list || alphabet %is% vector,
      alphabet %contains_only% simple_string),
  post = list(nchar(result) == length, length(result) == 1,
    is.character(result), all(strsplit(result, "")[[1]] %in% alphabet)),
  random_string))
```
```
Error: Quickcheck for random_string failed on item #1: length = 53L, alphabet = list("shtafWoWGRWmCSIRquDNxqskiKGyVdHFApld")
```

That will verify that the number of characters of the resulting string is the same as the `length` that you passed into the function, it will verify that the resulting string is not a length > 1 vector, that the resulting string is all characters, and that all the characters in the string are within the given `alphabet`.  This verification will be done with many different values for `length` and `alphabet`.

This easily accomplishes in two lines what normally takes five well thought-out and detailed tests.

**TODO: Write more here, also talk about the validations package.**

#### Reversing and Property-based Testing

Let's say that we want to be confident that the `rev` function in R's base works as intended to reverse a list.  We could create a few test cases ourselves, or we could use quickcheck to specify **properties** that should hold about `rev` are actually true over our randomly generated examples:

First, we know that reversing a length-1 list should be itself.

```R
quickcheck(ensure(pre = list(length(x) == 1, x %is% vector || x %is% list),
  post = identical(result, x), function(x) rev(x)))
```

And when we run the Quickcheck, we get:

```
Quickcheck for function(x) rev(x) passed on 132 random examples!
```

...Here, 576 random possible test objects were created and these objects were filtered down to the 138 ones that met the specified preconditions (input must be of length 1). All of these were then sent to the `rev` function and the result was then checked against the postcondition that `identical(result, x)` to make sure the result is identical to the original `x`.

And we can also test that the reverse of a reverse of a list is that same list:

```R
quickcheck(ensure(pre = list(x %is% vector || x %is% list,
  post = identical(result, x), function(x) rev(rev(x)))))
```
```
Quickcheck for ensure(post = identical(result, x), function(x) rev(rev(x))) passed on 576 random examples!
```


## Why not use Quickcheck by Revolution Analytics?

In June 2015 (8 months before me), Revolution Analytics released [their own version of Quickcheck for R](https://github.com/RevolutionAnalytics/quickcheck) which works [to also automatically verify properties of R functions](https://github.com/RevolutionAnalytics/quickcheck/blob/master/docs/tutorial.md).

**TODO: Explain the improvements I made over Revolution Analytic's Quickcheck.**
