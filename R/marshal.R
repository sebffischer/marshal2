#' @export
marshal_internal_wrapper = function(x, dict) {
  id = marshal_id(x)
  if (is.null(id)) {
    return(marshal_internal(x, dict = dict))
  }

  if (!exists(id, envir = dict, inherits = FALSE)) {
    dict[[id]] = marshal_internal(x, dict = dict)
  }
  structure(list(id = id), class = "marshaled_ref")
}

#' @export
marshal_id = function(x) {
  UseMethod("marshal_id")
}

#' @export
marshal_id.default = function(x) {
  NULL
}

#' @export
marshal_id.environment = function(x) {
  data.table::address(x)
}

#' @export
marshal = function(x) {
  dict = new.env()
  xm = marshal_internal_wrapper(x, dict)
  structure(list(marshaled = xm, dict = dict), class = "marshaled_with_dict")
}


# marshal_internal and unmarshal_internal must always call marshal_internal_wrapper and unmarshal_internal_wrapper
# when they redirect marshalling to sub-objects.
# Then, they must also pass the dict, dict_in and dict_out to these functions
marshal_internal = function(x, dict) {
  UseMethod("marshal_internal")
}

