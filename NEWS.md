#### v0.1.1

* Add the ability to manually define a testing frame using an explicit precondition passed to `function_test_objects`, which is now exported. 

## v0.1.0

* Switch to stable versioning.


#### v0.0.4.9008

* Add `is_empty` as an alias for `is.empty`.

#### v0.0.4.9007

* Allow empty string to be a default argument in a validated function.

#### v0.0.4.9006

* Fixed the bug that prevented using function names in conditions or in arguments.
* Fixed the bug that caused a warning when too many default arguments were assigned.
* `%within%` is exported.

#### v0.0.4.9005

* Added a matcher for atomic (e.g., `1 %is% atomic`).

#### v0.0.4.9004

* `package_exports_checked` now can take a file path as well as a package name.

#### v0.0.4.9003

* Introduce `package_exports_checked`, which checks a package for whether all exported functions are checked using checkr (either have zero formals or are wrapped in `ensure` blocks).
* Add a test for `any` (e.g., `1 %is% any`) that will always be `TRUE`. This is useful to be explicit that your function can take any input.

#### v0.0.4.9002

* Allow `NULL` to work as a result of the function in `ensure` or `quickcheck`.

#### v0.0.4.9001


* Prevent errors in testthat from the function name being too long.

## v0.0.4.9000

* Resolve bugs with nesting quickchecks within quickchecks.
* Allow a custom testing frame to be passed to quickcheck.
* Add `%within%` as a helper to test membership within a numeric boundary.


## v0.0.3.9000

* Validated functions now keep their formals instead of being coerced to splat.
* Add Travis.
* Pass R CMD CHECK and lintr.
* Some functions are no longer validated because this ran into circular issues.

#### v0.0.2.9002

* Redo the imputation of arguments within the validator.

#### v0.0.2.9001

* Remove calls to the now non-existent `validations` package.

## v0.0.2.9000

* Adds `%does_not_contain%` as a validator.
* Adds NA and NULL as classes that can be tested using `%is%`, `%contains%`, etc.
* Fixed bugs where missing arguments were not allowed in certain places where they should be allowed.
* Prevent a failed quickcheck from breaking the entire test suite. Quickcheck now returns FALSE when failing instead of an error.
* The testthat integration in quickcheck can be turned off with `testthat = FALSE`
* Fixed a bug where the report of the quickcheck failure would not be correct because the output was too long.
* Fixed a bug where quickcheck breaks if it can't load some built-in dataframes.
* Removed stray references to the old validation package.
* Fixed a merge conflict.


#### v0.0.1.9002

* Fixed how we checked for missing args, fixing bugs with `missing` and `present`.

#### v0.0.1.9001

* Added `present` to check for the presence of arguments.

## v0.0.1.9000

* Added quickcheck.


#### v0.0.0.9003

* Validating a function twice creates an error.
* Printing a validated function will show the preconditions, postconditions, and the original function, rather than the metaprogramming behind the scenes.
* Fixed a bug when calling functions with default arguments.
* Clarified `random_string` preconditions to not allow length >1 `length` arguments or alphabets where each letter has more than one character.

#### v0.0.0.9002

* Adds helpers `preconditions` and `postconditions` for fetching the conditions of validated functions.
* Handles default arguments and missing arguments in function calls.

#### v0.0.0.9001

* The resulting function is now also of class `validated_function`.

## v0.0.0.9000

* Initial package
