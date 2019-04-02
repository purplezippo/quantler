#' Make judgements of an input quantler obj.
#'
#' @param trade.info A data frame contains the signal of trade information
#'   and stock data.
#' @param cash Trade money at start.
#' @param is.pic Plot results or not.
#' @param is.pic.whole Should the pic output be in one piece together or in a
#'   list which contains all segments of different pics, only valied when is.pic
#'   is True.
#' @param plotlevel Values in one of "1min", "60mins", "1day", only valied when
#'   is.pic is True.
#' @param is.tax  Boolean values of True or False, add trading commission or
#'   not.
#' @param tax.rate Tax rate per trade, only valide when set is.tax to True.
#' @param is.simple Boolean values of True or False, which means if the
#'   judgement should be done in a simple way to run faster, only valied when
#'   is.pic is True.
#' @return A list contains the information of trading results.
#' @details Trade.info must be a data frame in the form of:
#'   data.frame(
#'    wind_code(character),
#'    stock.name(character),
#'    stock.price(numeric),
#'    stock.time(POSIXCT("yyyy-mm-dd hh:mm")),
#'    stock.date(as.Date("yyyy-mm-dd")),
#'    strategy.b(integer in c(-1, 0, 1), 0:"hold", 1:"buy", -1:"sell"),
#'    strategy.s(c(0,0,1,-1,0,0,....), 0:"hold", 1:"buy", -1:"sell")
#'    )
#'
#' @examples
#' data(stradeinfo_bp)
#' bt <- backtest(stradeinfo_bp, cash = 100000)
#' plot(bt$pic)
#' @export
backtest <- function(trade.info, cash = 0, is.tax = T, tax.rate = 0.003) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~##----------------------------------------------------------------####
  # 1.check and clean ####
  # (1) parms check ####
  if (!is.data.frame(trade.info)) {
    stop("trade.info must be a data frame")
  } else if (!all(c("wind_code", "stock.name", "stock.price", "stock.time", "stock.date") %in% names(trade.info))) {
    stop("components in trade.info are not correct")
  }
  if (class(trade.info$stock.date) != "Date") {
    tryCatch({
      trade.info$stock.date <- as.Date(trade.info$stock.date)
    }, error = function(e) {
      "时间格式错误"
    })
  }
  if (class(trade.info$stock.time)[1] != "POSIXct") {
    tryCatch({
      trade.info$stock.time <- as.POSIXct(trade.info$stock.time)
    }, error = function(e) {
      "时间格式错误"
    })
  }
  tryCatch({
    time.space <- as.numeric(diff(trade.info$stock.time[1:2]), units = "mins")
  }, error = function(e) {
    "时间格式错误"
  })
  if (!is.logical(is.tax)) stop("is.tax must be a logical variable")
  if (!is.numeric(cash)) {
    stop("cash must be a logical variable")
  } else if (cash < 0) {
    stop("cash must be larger than 0")
  }

  # (2) feature extract ####
  if (time.space < 5) { # 数据级别
    strategy.level <- "1min"
  } else if (time.space > 1380) {
    strategy.level <- "1day"
  } else if (time.space >= 5 && time.space <= 1380) {
    strategy.level <- "60mins"
  } else {
    stop("data level is not standard!")
  }

  # (3) acquire trade signal ####
  if (all(c("strategy.s", "strategy.b") %in% names(trade.info))) {
    strategy.type <- c("buy", "sell")
    strategy.b <- trade.info$strategy.b
    strategy.s <- trade.info$strategy.s
    if (any(abs(strategy.b - strategy.s) > 2)) stop("Operate direction conflits occur in strategys!")
    if (!all(c(-1, 1) %in% unique(strategy.b))) stop("Operate signal miss in strategy.b!")
    if (!all(c(-1, 1) %in% unique(strategy.s))) stop("Operate signal miss in strategy.s!")
  } else if ("strategy.b" %in% names(trade.info)) {
    strategy.type <- c("buy")
    strategy.b <- trade.info$strategy.b
    if (!all(c(-1, 1) %in% unique(strategy.b))) stop("Operate signal miss in strategy.b!")
    strategy.s <- c()
  } else if ("strategy.s" %in% names(trade.info)) {
    strategy.type <- c("sell")
    strategy.s <- trade.info$strategy.s
    if (!all(c(-1, 1) %in% unique(strategy.s))) stop("Operate signal miss in strategy.s!")
    strategy.b <- c()
  } else {
    stop("The strategy info is missing!")
  }


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##---------------------------------------------------------------####
  # 2. compute ####
  # (1)inner functions and global variables ####
  strategy.days <- length(unique(trade.info$stock.date)) # 策略执行日期
  risk.free.rate <- .04 # 无风险利率
  stock.price <- trade.info$stock.price # 股价
  len <- nrow(trade.info) # 记录数
  loc.day <- c(which(diff(trade.info$stock.date) >= 1), len) # 日分割点

  # (2) value calculate ####
  weight.b <- 0 # 多头期初持股
  if ("sell" %in% strategy.type) {
    weight.s <- floor(cash / (stock.price[1]) / 100) * 100 # 空头期初持股
  } else {
    weight.s <- 0
  }
  weight.s.temp <- weight.s # 空头期间持股
  trade.value <- c() # 策略期间总价值
  trade.value.b <- c() # 多头期间价值
  trade.value.s <- c() # 空头期间价值
  cash.temp.b <- cash # 多头期间现金
  cash.temp.s <- 0 # 空头期间现金

  for (i in 1:len) {
    if ("buy" %in% strategy.type) { # 做多
      if (strategy.b[i] > 0) {
        if (i > 1 && strategy.b[i - 1] > 0) {
          message(paste(
            "策略出错", trade.info$stock.time[i], "多头 ", "现金", cash.temp.b,
            "持股", weight.b, "连续全仓买入的信号，只执行第一次操作\n",
            sep = ";"
            ))
          strategy.b[i] <- 0 # 修改错误信号
          trade.value.b[i] <- weight.b * stock.price[i] + cash.temp.b
        } else if (cash.temp.b < stock.price[i] * 100) {
          message(paste("策略出错", trade.info$stock.time[i], "多头 ", "现金", cash.temp.b, "持股", weight.b, "资金不足,无法买入\n", sep = ";"))
          strategy.b[i] <- 0 # 修改错误信号
          trade.value.b[i] <- weight.b * stock.price[i] + cash.temp.b
        } else {
          if (weight.b == 0) { # 未持仓
            weight.b <- floor(cash.temp.b / (100 * stock.price[i])) * 100 # 持有股票
            cash.temp.b <- round(cash.temp.b - stock.price[i] * weight.b, 2) # 持有现金
          } else {
            strategy.b[i] <- 0 # 连续加仓暂时只算第一次
            weight.b0 <- weight.b
            weight.b <- weight.b + floor(cash.temp.b / (100 * stock.price[i])) * 100 # 持仓，且继续加仓
            cash.temp.b <- round(cash.temp.b - stock.price[i] * (weight.b - weight.b0), 2) # 持有现金
          }
          trade.value.b[i] <- weight.b * stock.price[i] + cash.temp.b # 现金和股票总价值
        }
      } else if (strategy.b[i] == 0) {
        trade.value.b[i] <- cash.temp.b + stock.price[i] * weight.b # 现金和股票总价值
      } else {
        if (i > 1 && strategy.b[i - 1] < 0) {
          message(paste(
            "strategy fault", trade.info$stock.time[i], "long position ",
            "cash holding", cash.temp.b, "stock holding", weight.b,
            "continuous sell signal, only the first will be execute\n",
            sep = ";"
          ))
          strategy.b[i] <- 0 # 修改错误信号
          trade.value.b[i] <- trade.value.b[i - 1]
        } else if (i == 1) {
          message(paste(
            "strategy fault", trade.info$stock.time[i], "long position ",
            "cash holding", cash.temp.b, "stock holding", weight.b,
            "sell signal in the first handel, sell execution denied\n",
            sep = ";"
          ))
          strategy.b[i] <- 0 # 修改错误信号
          trade.value.b[i] <- cash.temp.b
        } else if (weight.b <= 0) {
          message(paste(
            "strategy fault", trade.info$stock.time[i], "long position ",
            "cash holding", cash.temp.b, "stock holding", weight.b,
            "not enough stock in hand, sell execution denied\n",
            sep = ";"
          ))
          strategy.b[i] <- 0 # 修改错误信号
          trade.value.b[i] <- cash.temp.b
        } else {
          if (stock.price[i] * weight.b * tax.rate < 5) { # 税费小于5元时按5元算
            trade.value.b[i] <-
              cash.temp.b + stock.price[i] * weight.b - 5 * is.tax
          } else {
            trade.value.b[i] <-
              cash.temp.b + stock.price[i] * weight.b - stock.price[i] * weight.b * tax.rate * is.tax
          }
          cash.temp.b <- round(trade.value.b[i], 2)
          weight.b <- 0
        }
      }
    } else {
      trade.value.b[i] <- 0
    }

    if ("sell" %in% strategy.type) { # 做空
      if (strategy.s[i] < 0) { # 卖出信号
        if (i > 1 && strategy.s[i - 1] < 0) {
          message(paste(
            "strategy fault", trade.info$stock.time[i], "short postion",
            "cash", cash.temp.s, "stock holding", weight.s.temp,
            "continuous sell signal, only the first will be executed\n",
            sep = ";"
          ))
          strategy.s[i] <- 0 # 修改错误信号
          trade.value.s[i] <- trade.value.s[i - 1]
        } else if (weight.s.temp <= 0) {
          message(paste(
            "strategy fault", trade.info$stock.time[i], "short postion",
            "cash", cash.temp.s, "stock holding", weight.s.temp,
            "not enough stocks, sell execution denied\n",
            sep = ";"
          ))
          strategy.s[i] <- 0
          trade.value.s[i] <- trade.value.s[i - 1]
        } else {
          if (stock.price[i] * weight.s.temp * tax.rate < 5) {
            trade.value.s[i] <-
              stock.price[i] * weight.s.temp - 5 * is.tax + cash.temp.s
          } else {
            trade.value.s[i] <-
              stock.price[i] * weight.s.temp - stock.price[i] * weight.s.temp * tax.rate * is.tax + cash.temp.s
          }
          cash.temp.s <- round(trade.value.s[i], 2) # 平仓获取现金
          weight.s.temp <- 0 # 持有股票重设为0
        }
      } else if (strategy.s[i] == 0) {
        trade.value.s[i] <- cash.temp.s + stock.price[i] * weight.s.temp
      } else {# 买入信号
        if (i > 1 && strategy.s[i - 1] > 0) {
          message(paste(
            "strategy fault", trade.info$stock.time[i], "short postion ",
            "cash holding", cash.temp.s, "stock holding", weight.s.temp,
            "continuous buy signal, only the first will be executed\n",
            sep = ";"
          ))
          strategy.s[i] <- 0 # 修改错误信号
          trade.value.s[i] <- weight.s.temp * stock.price[i] + cash.temp.s
        } else if (i == 1) {
          message(paste(
            "strategy fault", trade.info$stock.time[i], "short postion ",
            "cash holdinig", cash.temp.s, "stock holding", weight.s.temp,
            "first trade signal is buy in short position, buy execution denied\n",
            sep = ";"
          ))
          strategy.s[i] <- 0 # 修改错误信号
          trade.value.s[i] <- weight.s.temp * stock.price[i] + cash.temp.s
        } else if (cash.temp.s < stock.price[i] * 100) {
          message(paste(
            "strategy fault", trade.info$stock.time[i], "short postion ",
            "cash holding", cash.temp.s, "stock holding", weight.s.temp,
            "not engouth cash, buy execution denied\n",
            sep = ";"
          ))
          strategy.s[i] <- 0 # 修改错误信号
          trade.value.s[i] <- weight.s.temp * stock.price[i] + cash.temp.s
        } else {
          weight.s.temp <- floor(cash.temp.s / (stock.price[i]) / 100) * 100
          cash.temp.s <- round(cash.temp.s - stock.price[i] * weight.s.temp, 2)
          trade.value.s[i] <- weight.s.temp * stock.price[i] + cash.temp.s
        }
      }
    } else {
      trade.value.s[i] <- 0
      weight.s <- 0
    }
    trade.value[i] <- trade.value.b[i] + trade.value.s[i] - stock.price[i] * weight.s # 更新总价值
  }

  if (!"buy" %in% strategy.type) trade.value <- trade.value + stock.price[1] * weight.s # 单独做空时为方便检验效果，加上初始金额，100股除不尽的部分忽略

  # (3) features ####

  # 3.1 总交易次数，买卖各算一次
  strategy.num <- sum(c(strategy.s > 0, strategy.b > 0))

  # 3.2 基准收益（向量）
  stock.benchmarkwin <- bm_return_v(stock.price, cash)

  # 3.3 基准收益率 （股票+现金当前价值 - 初始价值）/初始价值
  benchmark.return <- return_rate(stock.price)

  # 3.4 策略交易收益率
  strategy.return <- return_rate(trade.value)

  # 3.5 日交易基准收益率 （股票+现金当前价值-初始价值)/初始价值
  benchmark.day.return <- return_rate(stock.price, "day", strategy.days)
  strategy.day.return <- return_rate(trade.value, "day", strategy.days)

  # 3.6 基准年化收益(标量)
  benchmark.annual.return <- return_rate(stock.price, "year", strategy.days)

  # 3.7 策略年化收益(标量)
  strategy.annual.return <- return_rate(trade.value, "year", strategy.days)

  # 3.8 beta  cov(策略每日收益, 基准每日收益)/var(基准每日收益)
  beta <- beta_stock(stock.price[loc.day], cash.begin = cash, trade.value = trade.value)

  # 3.9 alpha
  alpha <- alpha_stock(
    stock.price = stock.price, cash.begin = cash, trade.value = trade.value,
    rate = risk.free.rate, trade.days = strategy.days
  )

  # 3.10 Algorithm Volatility（策略波动率）
  alg.vol <- algo_vol(trade.value[loc.day])

  # 3.11夏普比率
  sharpe.ratio <- sharpe_rate(
    trade.value = trade.value, rate = risk.free.rate, trade.days = strategy.days
    )

  # 3.12信息比率
  # 策略与基准每日收益差值的年化标准差
  info.ratio <- info_rate(
    stock.price = stock.price[loc.day], cash.begin = cash,
    trade.value = trade.value[loc.day],
    trade.days = strategy.days
  )

  # 3.13 基准波动率
  ben.vol <- bench_vol(stock.price[loc.day]) # daily

  # 3.14 最大回撤
  max.dd.temp <- maxdrawdown(trade.value = trade.value)
  max.drawdown <- max.dd.temp$value
  max.drawdown.loc <- max.dd.temp$loc
  max.down.date <- trade.info$stock.date[max.drawdown.loc]

  # 3.15 胜率

  if (length(strategy.type) == 2) {
    win.rate.b <- win_rate(strategy.signal = strategy.b, trade.value = trade.value.b, type = "buy")
    win.rate.s <- win_rate(
      strategy.signal = strategy.s, trade.value = trade.value.s, type = "sell",
      weight.s = weight.s, stockp.begin = stock.price[1]
    )

    win.rate <- (win.rate.b[[1]] * win.rate.b[[2]] + win.rate.s[[1]] * win.rate.s[[2]]) / (win.rate.b[[2]] + win.rate.s[[2]])
  } else if (strategy.type == "buy") {
    win.rate.b <- win_rate(strategy.signal = strategy.b, trade.value = trade.value.b, type = "buy")
    win.rate <- win.rate.b[[1]]
  } else {
    win.rate.s <- win_rate(
      strategy.signal = strategy.s, trade.value = trade.value.s, type = "sell",
      weight.s = weight.s, stockp.begin = stock.price[1]
    )
    win.rate <- win.rate.s[[1]]
  }

  # 3.16 日胜率
  win.day.rate <- sum(diff(trade.value[loc.day]) > diff(stock.price[loc.day]) * (cash / stock.price[1])) / strategy.days

  # 3.17盈亏比
  diff.cash <- diff(trade.value)
  win.lose.rate <- sum(diff.cash[diff.cash > 0]) / abs(sum(diff.cash[diff.cash < 0]))

  # 3.18 正确率 & 3.19平均每笔收益率
  if (length(strategy.type) == 1) {
    if (strategy.type == "buy") {
      # 正确率
      acc.rate.b <- pre_correct(stock.price, strategy.b = strategy.b)[1]
      acc.rate.s <- NA
      # 平均收益率
      mean.win.rate.b <- mean(win.rate.b$pertrade.winrate)
      mean.win.rate.s <- NA
    } else {
      acc.rate.s <- pre_correct(stock.price, strategy.s = strategy.s)[2]
      acc.rate.b <- NA
      # 平均收益率
      mean.win.rate.b <- NA
      mean.win.rate.s <- mean(win.rate.s$pertrade.winrate)
    }
  } else {
    acc.both <- pre_correct(stock.price, strategy.s = strategy.s, strategy.b = strategy.b)
    acc.rate.b <- acc.both[1]
    acc.rate.s <- acc.both[2]
    mean.win.rate.b <- mean(win.rate.b$pertrade.winrate)
    mean.win.rate.s <- mean(win.rate.s$pertrade.winrate)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##---------------------------------------------------------------####
  # 3.output ####
  all.features <-
    list(
      benchmark.return, strategy.return, benchmark.day.return,
      strategy.day.return,
      benchmark.annual.return, strategy.annual.return,
      beta, alpha, alg.vol,
      sharpe.ratio, info.ratio, ben.vol,
      max.drawdown, max.drawdown.loc,
      win.rate, win.day.rate, win.lose.rate,
      trade.value, stock.price, stock.benchmarkwin, strategy.type,
      mean.win.rate.b, mean.win.rate.s, win.rate.b, win.rate.s,
      acc.rate.b, acc.rate.s, tax.rate, cash
    )
  names(all.features) <-
    c(
      "benchmark.return", "strategy.return", "benchmark.day.return",
      "strategy.day.return",
      "benchmark.annual.return", "strategy.annual.return",
      "beta", "alpha", "alg.vol",
      "sharpe.ratio", "info.ratio", "ben.vol",
      "max.drawdown", "max.drawdown.loc",
      "win.rate", "win.day.rate", "win.lose.rate",
      "trade.value", "stock.price", "stock.benchmarkwin", "strategy.type",
      "mean.win.rate.b", "mean.win.rate.s", "win.rate.b", "win.rate.s",
      "acc.rate.b", "acc.rate.s", "tax.rate", "cash"
    )

  temp <- quantler(
    trade.info = trade.info, data.level = strategy.level,
    bt.features = all.features
    )
  return(temp)
  # return(all.features)
}
