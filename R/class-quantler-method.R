#' @include class-def.R
NULL

#' Conversion functions to coerce data objects of arbitrary classes to class
#' quantler and back, without losing any attributes of the original format.
setGeneric('as.quantler', function(object){
  standardGeneric('as.quantler')
})

setMethod('as.quantler', signature(object = 'data.frame'), function(object){

})

setMethod('as.quantler', signature(object = 'matrix'), function(object){

})






