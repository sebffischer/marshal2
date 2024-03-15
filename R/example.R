#' @export
custom_env = function(data)  {
  e = new.env()
  e$data = data
  e$other_fn = function() {
    print("do some stuff")
  }
  class(e) = c("custom_env", "environment")
  return(e)
}

#' @export
container = function(...) {
  structure(list(...), class = "container")
}

#' @export
marshal_internal.container = function(x, dict) {
  x_marshaled = lapply(x, function(obj) {
    marshal_internal_wrapper(obj, dict)
  })
  names(x_marshaled) = names(x)
  x = structure(list(marshaled = x_marshaled), class = c("container_marshaled", "marshaled"))
  return(x)
}


#' @export
unmarshal_internal.container_marshaled = function(x, dict_in, dict_out) {
  do.call(container, args = lapply(x$marshaled, unmarshal_internal_wrapper, dict_in = dict_in, dict_out = dict_out))
}


#' @export
unmarshal_internal.custom_env_marshaled = function(x, dict_in, dict_out) {
  custom_env(x$marshaled)
}


#' @export
marshal_internal.custom_env = function(x, dict) {
  structure(list(marshaled = x$data), class = c("custom_env_marshaled", "marshaled"))
}
