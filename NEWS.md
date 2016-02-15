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
