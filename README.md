
## `marshal` Prototype to preserve reference identities

This repository implements a prototype for a `marshal()` standard that
enables preservation of reference identities.

The main functions for end end-user are still `marshal()` and
`unmarshal()`.

Package developers that implement custom marshaling methods for their
classes need to implement:

1.  `marshal_id(x)`: Needs to return either `NULL` (is also the default
    implementation) or a `character(1)` giving an unique identifier.
    This is a generic that needs to be implemented for objects with
    reference semantics. During a single call to `marshal()`, objects
    that have the same `marshal_id()` will be identical after
    `unmarshal()`.
2.  `marshal_internal(x, dict)`: This needs to return the marshaled
    object. In case the object `x` contains in turn other marshalable
    objects, these need to be marshaled using
    `marshal_internal_wrapper()` and **NOT** `marshal_internal()`. The
    function `marshal_internal_wrapper()` is a slight wrapper around
    `marshal_internal()` and also takes care of managing the `dict`,
    i.e. marshaling of objects that were already marshaled is skipped.
    The `dict` argument can be ignored unless the object `x` contains
    other marshalable objects, in which case the `dict` must be passed
    further down to `marshal_internal_wrapper()`.
3.  `unmarshal_internal(x, dict_in, dict_out)`: Like `marshal_internal`
    but for unmarshaling. In case the marshaled object contains other
    marshaled objects, they must be unmarshaled using
    `unmarshal_internal_wrapper`, passing `dict_in` and `dict_out`
    further down. The `dict_in` is the dictionary of already unmarshaled
    objects, and `dict_out` is the dictionary of objects that are
    currently being unmarshaled.

## Example

``` r
library(marshal2)
library(testthat)
e = custom_env(1)
cont = container(e, e)
contm = marshal(cont)
contr = unmarshal(contm)

expect_true(identical(contr[[1]], contr[[2]]))
expect_equal(cont, contr)
```
