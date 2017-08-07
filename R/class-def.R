#' An S4 class for quant strategy judgements.
#'
#' @slot trade.info A data.frame
#' @slot data.level A character value of one of c('1day', '60mins', '1min')
#' @slot bt.features A list contains severial features about quant strategy
#'   judegments produced by the back-test process.
#' @export
quantler <- setClass('quantler',
                     slots = list(trade.info  = 'data.frame',
                                  data.level  = 'character',
                                  bt.features = 'list'
                                  ),
                     contains = 'list')


#' Is an object a quantler object?
#'
#' Checks for quantler objects.
#'
#' @param object An object.
#'
#' @return A logical value. TRUE when the input is a quantler object and FALSE
#'   otherwise.
#' @export
is.quantler <- function(object){
  # inherits(object, 'quantler')
}
