#' @export
unmarshal_internal_wrapper = function(x, dict_in, dict_out) {
  if (inherits(x, "marshaled_ref")) {
    if (!exists(x$id, envir = dict_out, inherits = FALSE)) {
      dict_out[[x$id]] = unmarshal_internal(dict_in[[x$id]], dict_in, dict_out)
    }
    return(dict_out[[x$id]])
  }
  unmarshal_internal(x, dict_in, dict_out)
}

#' @export
unmarshal_internal.marshaled_ref = function(x, dict_in, dict_out) {
  if (!exists(x$id, envir = dict_out, inherits = FALSE)) {
    dict_out[[x$id]] = unmarshal_internal_wrapper(dict_in[[x$id]], dict_in, dict_out)
  }
  dict_out[[x$id]]
}


#' @export
unmarshal_internal = function(x, dict_in, dict_out) {
  UseMethod("unmarshal_internal")
}


#' @export
unmarshal_internal.custom_env_marshaled = function(x, dict_in, dict_out) {
  custom_env(x$marshaled)
}

#' @export
unmarshal = function(x) {
  stopifnot(inherits(x, "marshaled_with_dict"))
  dict_out = new.env()

  out = unmarshal_internal_wrapper(x$marshaled, dict_in = x$dict, dict_out = dict_out)
  return(out)
}

#' @export
unmarshal_internal.container_marshaled = function(x, dict_in, dict_out) {
  do.call(container, args = lapply(x$marshaled, unmarshal_internal_wrapper, dict_in = dict_in, dict_out = dict_out))
}
