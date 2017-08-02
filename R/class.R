#' An S4 class to represent a trade data set producted by quant model
#'
#' @slot trade.info A data.frame
quantler <- setClass('quantler', slots = list(trade.info = 'data.frame'))


#' @describeIn quantler judgement if an object is quantler
is.quantler <- function(object){}
setMethod('is.quantler', 'quantler',
          function(object){
            inherits(object, "quantler")
          }
          )
