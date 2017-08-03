#' An S4 class to represent a trade data set producted by quant model
#'
#' @slot trade.info A data.frame
quantler <- setClass('quantler', slots = list(trade.info = 'data.frame'))


#' Is an object a quantler Object?
#'
#' Checks for quantler objects.
#'
#' @param object An object.
#'
#' @return A logical value. TRUE when the input is a quantler object and FALSE
#'   otherwise.
is.quantler <- function(object){
  inherits(object, 'quantler')
}

