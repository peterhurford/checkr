#### v0.0.2.9000

* Adds `%does_not_contain%` as a validator.
* Prevent a failed quickcheck from breaking the entire test suite. Quickcheck now returns FALSE when failing instead of an error.
* The testthat integration in quickcheck can be turned off with `testthat = FALSE`
* Fixed a bug where quickcheck breaks if it can't load some built-in dataframes.
* Removed stray references to the old validation package.
* Fixed a merge conflict.

#### v0.0.1.9002

* Fixed how we checked for missing args, fixing bugs with `missing` and `present`.

#### v0.0.1.9001

* Added `present` to check for the presence of arguments.

#### v0.0.1.9000

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

#### v0.0.0.9000

* Initial package
