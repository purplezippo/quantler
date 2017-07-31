setClass('quantler', slots = c(trade.info = 'data.frame'))

is.quantler <- function(object){
  inherits(object, "quantler")
}
setMethod('is.quantler', 'quantler',
          function(object){
            inherits(object, "quantler")
          }
          )
