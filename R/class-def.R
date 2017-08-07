#' An S4 class to represent a trade data set producted by quant model
#'
#' @slot trade.info A data.frame
quantler <- setClass('quantler',
                     slots = list(trade.info  = 'data.frame',
                                  data.level  = 'character',
                                  bt.features = 'list'
                                  ),
                     contains = 'list')

