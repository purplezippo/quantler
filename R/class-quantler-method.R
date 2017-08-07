#' @include class-def.R
NULL



#' Is an object a quantler object?
#'
#' Checks for quantler objects.
#'
#' @param object An object.
#'
#' @return A logical value. TRUE when the input is a quantler object and FALSE
#'   otherwise.
#' @describeIn quantler if an object is a quantler object
#' @export
is.quantler <- function(object){
  inherits(object, 'quantler')
}


#' Conversion functions to coerce data objects of arbitrary classes to class
#' quantler and back, without losing any attributes of the original format.
setGeneric('as.quantler', function(object){
  standardGeneric('as.quantler')
})

setMethod('as.quantler', signature(object = 'data.frame'), function(object){

})

setMethod('as.quantler', signature(object = 'matrix'), function(object){

})






