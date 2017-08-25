#### v0.1.5

* Adds `%is_not%` as an alias for `%isnot%`.

#### v0.1.4

* Fixes `is.empty` to work with vectorized input.

#### v0.1.3

* Amends `%within%` to be vectorized.

#### v0.1.2

* Fixes a bug where functions could not be used as formals.

#### v0.1.1

* Adds the ability to manually define a testing frame using an explicit precondition passed to `function_test_objects`, which is now exported. 

## v0.1.0

* Switches to stable versioning.


#### v0.0.4.9008

* Adds `is_empty` as an alias for `is.empty`.

#### v0.0.4.9007

* Allows empty string to be a default argument in a validated function.

#### v0.0.4.9006

* Fixes the bug that prevented using function names in conditions or in arguments.
* Fixes the bug that caused a warning when too many default arguments were assigned.
* Exports `%within%`.

#### v0.0.4.9005

* Adds a matcher for atomic (e.g., `1 %is% atomic`).

#### v0.0.4.9004

* Amends `package_exports_checked` to take a file path as well as a package name.

#### v0.0.4.9003

* Adds `package_exports_checked`, which checks a package for whether all exported functions are checked using checkr (either have zero formals or are wrapped in `ensure` blocks).
* Adds a test for `any` (e.g., `1 %is% any`) that will always be `TRUE`. This is useful to be explicit that your function can take any input.

#### v0.0.4.9002

* Amends `ensure` or `quickcheck` to take `NULL` as a result.

#### v0.0.4.9001


* Fixes errors in testthat from the function name being too long.

## v0.0.4.9000

* Fixes bugs with nesting quickchecks within quickchecks.
* Amends quickcheck to take a custom testing frame.
* Adds `%within%` as a helper to test membership within a numeric boundary.


## v0.0.3.9000

* Amends validated functions to now keep their formals instead of being coerced to splat.
* Adds Travis.
* Fixes to pass R CMD CHECK and lintr. Some functions are no longer validated because this ran into circular issues.

#### v0.0.2.9002

* Redoes the imputation of arguments within the validator.

#### v0.0.2.9001

* Removes calls to the now non-existent `validations` package.

## v0.0.2.9000

* Adds `%does_not_contain%` as a validator.
* Adds NA and NULL as classes that can be tested using `%is%`, `%contains%`, etc.
* Fixes bugs where missing arguments were not allowed in certain places where they should be allowed.
* Fixes to prevent a failed quickcheck from breaking the entire test suite. Quickcheck now returns FALSE when failing instead of an error.
* Amends the testthat integration in quickcheck to be turned off with `testthat = FALSE`
* Fixes a bug where the report of the quickcheck failure would not be correct because the output was too long.
* Fixes a bug where quickcheck breaks if it can't load some built-in dataframes.
* Removes stray references to the old validation package.
* Fixes a merge conflict.


#### v0.0.1.9002

* Fixes how we checked for missing args, fixing bugs with `missing` and `present`.

#### v0.0.1.9001

* Adds `present` to check for the presence of arguments.

## v0.0.1.9000

* Adds quickcheck.


#### v0.0.0.9003

* Amends to create an error when validating a function twice.
* Amends so that printing a validated function will show the preconditions, postconditions, and the original function, rather than the metaprogramming behind the scenes.
* Fixes a bug when calling functions with default arguments.
* Amends to clarify `random_string` preconditions to not allow length >1 `length` arguments or alphabets where each letter has more than one character.

#### v0.0.0.9002

* Adds helpers `preconditions` and `postconditions` for fetching the conditions of validated functions.
* Fixes to handle default arguments and missing arguments in function calls.

#### v0.0.0.9001

* Amends so that the resulting function is now also of class `validated_function`.

## v0.0.0.9000

* Initial package
