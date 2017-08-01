#' Make judgements of an input quantler obj.
#'
#' @param trade.info An quantler object contains the signal of trade information and stock data.
#' @param cash Trade money at start.
#' @param is.pic Plot results or not.
#' @param is.pic.whole Should the pic output be in one piece together or in a list which contains
#'   all segments of different pics, only valied when is.pic is True.
#' @param plotlevel Values in one of "1min", "60mins", "1day", only valied when is.pic is True.
#' @param is.tax  Boolean values of True or False, add trading commission or not.
#' @param tax.rate Tax rate per trade, only valide when set is.tax to True.
#' @param is.simple Boolean values of True or False, which means if the judgement should be done in
#'   a simple way to run faster, only valied when is.pic is True.
#' @return A list contains the information of trading results.
#' @export

backtest <- function(trade.info, cash = 0, is.pic = F, is.pic.whole = T, plotlevel = "1day",
                        is.tax = T, tax.rate = 0.003, is.simple = F){
  # info of pars:
  # trade.info: data.frame(
  #  wind_code = <NA>,
  #  stock.name = <NA>,
  #  stock.price = <NA>,
  #  stock.time = POSIXCT("yyyy-mm-dd hh:mm"),
  #  stock.date = as.Date("yyyy-mm-dd")
  #  strategy.b = c(0,0,1,-1,0,0,....), 0:"hold", 1:"buy", -1:"sell"
  #  strategy.s = c(0,0,1,-1,0,0,....), 0:"hold", 1:"buy", -1:"sell"
  # )
  #~~~~~~~~~~~~~~~~~~~~~~~~~~##----------------------------------------------------------------####
  # (一)规格化 ####
  # (1)输入参数检查
  if(!is.data.frame(trade.info))
  {
    stop("trade.info must be a data frame")
  }else if(!all(c("wind_code","stock.name","stock.price","stock.time","stock.date") %in% names(trade.info))){
    stop("components in trade.info are not correct")
  }
  if(class(trade.info$stock.date) != "Date"){
    tryCatch({trade.info$stock.date <- as.Date(trade.info$stock.date)}, error = function(e){"时间格式错误"})
  }
  if(class(trade.info$stock.time)[1] != "POSIXct"){
    tryCatch({trade.info$stock.time <- as.POSIXct(trade.info$stock.time)}, error = function(e){"时间格式错误"})
  }
  tryCatch({time.space <- as.numeric(diff(trade.info$stock.time[1:2]), units = "mins")}, error = function(e){"时间格式错误"})
  if(!(plotlevel %in% c("1min", "1day"))) stop("the value of plotlevel must be one of :\"1min\", \"1day\"")
  if(!is.logical(is.tax)) stop("is.tax must be a logical variable")
  if(!is.logical(is.pic)) stop("is.pic must be a logical variable")
  if(!is.numeric(cash)) {
    stop("cash must be a logical variable")
  }else if(cash < 0){
    stop("cash must be larger than 0")
  }

  # (2)数据特征检查及提取
  if(time.space < 5){                                                              # 数据级别
    strategy.level <- "1min"
  }else if(time.space > 1380){
    strategy.level <- "1day"
  }else if(time.space >= 5 && time.space <= 1380){
    strategy.level <- "60mins"
  }else{
    stop("data level is not standard!")
  }

  # 获取交易信号
  if(all(c("strategy.s", "strategy.b") %in% names(trade.info))){
    strategy.type <- c("buy","sell")
    strategy.b <- trade.info$strategy.b
    strategy.s <- trade.info$strategy.s
    if(any(abs(strategy.b - strategy.s) > 2)) stop("Operate direction conflits occur in strategys!")
    if(!all(c(-1,1) %in% unique(strategy.b))) stop("Operate signal miss in strategy.b!")
    if(!all(c(-1,1) %in% unique(strategy.s))) stop("Operate signal miss in strategy.s!")
  }else if ("strategy.b" %in% names(trade.info)){
    strategy.type <- c("buy")
    strategy.b <- trade.info$strategy.b
    if(!all(c(-1,1) %in% unique(strategy.b))) stop("Operate signal miss in strategy.b!")
    strategy.s <- c()
  }else if ("strategy.s" %in% names(trade.info)){
    strategy.type <- c("sell")
    strategy.s <- trade.info$strategy.s
    if(!all(c(-1,1) %in% unique(strategy.s))) stop("Operate signal miss in strategy.s!")
    strategy.b <- c()
  }else{
    stop("The strategy info is missing!")
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##---------------------------------------------------------------####
  # (二)计算 ####
  #（1）内部函数&全局变量
  strategy.days <- length(unique(trade.info$stock.date))                          # 策略执行日期
  risk.free.rate <- .04                                                           # 无风险利率
  stock.price <- trade.info$stock.price                                           # 股价
  len <- nrow(trade.info)                                                         # 记录数
  loc.day <- c(which(diff(trade.info$stock.date) >= 1 ),len)                      # 日分割点

  # (2)价值计算
  weight.b <- 0                                                                   # 多头期初持股
  if("sell"%in% strategy.type){
    weight.s <- floor(cash/(stock.price[1])/100)*100                              # 空头期初持股
  }else {
    weight.s <- 0
  }
  weight.s.temp <- weight.s                                                       # 空头期间持股
  total.price <- c()                                                              # 策略期间总价值
  total.price.b <- c()                                                            # 多头期间价值
  total.price.s <- c()                                                            # 空头期间价值
  cash.temp.b <- cash                                                             # 多头期间现金
  cash.temp.s <- 0                                                                # 空头期间现金

  for (i in 1:len){
    # 交易成本：0.003，单边（卖）
    if("buy" %in% strategy.type ){                                                # 做多
      if (strategy.b[i] > 0) {
        if (i > 1 && strategy.b[i-1] > 0) {
          message(paste("策略出错",trade.info$stock.time[i],"多头 ","现金",cash.temp.b,"持股",weight.b,"连续全仓买入的信号，只执行第一次操作\n",sep = ";"))
          strategy.b[i] <- 0                                                      # 修改错误信号
          total.price.b[i] <- weight.b*stock.price[i] + cash.temp.b
        } else if (cash.temp.b < stock.price[i]*100){
          message(paste("策略出错",trade.info$stock.time[i],"多头 ","现金",cash.temp.b,"持股",weight.b,"资金不足,无法买入\n",sep = ";"))
          strategy.b[i] <- 0                                                      # 修改错误信号
          total.price.b[i] <- weight.b*stock.price[i] + cash.temp.b
        }else {
          if(weight.b == 0){                                                      # 未持仓
            weight.b <- floor(cash.temp.b/(100 * stock.price[i]))*100             # 持有股票
            cash.temp.b <- round(cash.temp.b - stock.price[i]*weight.b, 2)          # 持有现金
          }else{
            strategy.b[i] <- 0                                                    # 连续加仓暂时只算第一次
            weight.b0 <- weight.b
            weight.b <- weight.b + floor(cash.temp.b/(100 * stock.price[i]))*100  # 持仓，且继续加仓
            cash.temp.b <- round(cash.temp.b - stock.price[i]*(weight.b-weight.b0), 2)          # 持有现金
          }
          total.price.b[i] <- weight.b*stock.price[i] + cash.temp.b               # 现金和股票总价值
        }
      } else if (strategy.b[i] == 0 ){
        total.price.b[i] <- cash.temp.b + stock.price[i]*weight.b                 # 现金和股票总价值
      } else {
        if (i > 1 && strategy.b[i-1] < 0){
          message(paste("strategy fault",trade.info$stock.time[i],"long position ", "cash holding", cash.temp.b,
                        "stock holding",weight.b,"continuous sell signal, only the first will be execute\n",sep = ";"))
          strategy.b[i] <- 0                                                      # 修改错误信号
          total.price.b[i] <- total.price.b[i-1]
        } else if (i == 1){
          message(paste("strategy fault",trade.info$stock.time[i],"long position ","cash holding",cash.temp.b,
                        "stock holding",weight.b,"sell signal in the first handel, sell execution denied\n",sep = ";"))
          strategy.b[i] <- 0                                                      # 修改错误信号
          total.price.b[i] <- cash.temp.b
        } else if (weight.b <= 0){
          message(paste("strategy fault",trade.info$stock.time[i],"long position ","cash holding",cash.temp.b,
                        "stock holding",weight.b,"not enough stock in hand, sell execution denied\n",sep = ";"))
          strategy.b[i] <- 0                                                      # 修改错误信号
          total.price.b[i] <- cash.temp.b
        } else {
          if(stock.price[i]*weight.b*tax.rate < 5){                               # 税费小于5元时按5元算
            total.price.b[i] <-
              cash.temp.b + stock.price[i]*weight.b - 5*is.tax
          }else{
            total.price.b[i] <-
              cash.temp.b + stock.price[i]*weight.b - stock.price[i]*weight.b*tax.rate*is.tax
          }
          cash.temp.b <- round(total.price.b[i], 2)
          weight.b <- 0
        }
      }
    }else{
      total.price.b[i] <- 0
    }

    if ("sell" %in% strategy.type){                                               # 做空
      if (strategy.s[i] < 0) {                                                    # 卖出信号
        if (i > 1 && strategy.s[i-1] < 0){
          message(paste("strategy fault",trade.info$stock.time[i],"short postion", "cash", cash.temp.s,
                        "stock holding",weight.s.temp,"continuous sell signal, only the first will be executed\n",sep = ";"))
          strategy.s[i] <- 0                                                      # 修改错误信号
          total.price.s[i] <- total.price.s[i-1]
        } else if (weight.s.temp <= 0){
          message(paste("strategy fault",trade.info$stock.time[i],"short postion", "cash", cash.temp.s,
                        "stock holding",weight.s.temp,"not enough stocks, sell execution denied\n",sep = ";"))
          strategy.s[i] <- 0
          total.price.s[i] <- total.price.s[i-1]
        }else {
          if(stock.price[i]*weight.s.temp*tax.rate < 5){
            total.price.s[i] <-
              stock.price[i]*weight.s.temp - 5*is.tax + cash.temp.s
          }else{
            total.price.s[i] <-
              stock.price[i]*weight.s.temp - stock.price[i]*weight.s.temp*tax.rate*is.tax + cash.temp.s
          }
          cash.temp.s <- round(total.price.s[i], 2)                               # 平仓获取现金
          weight.s.temp <- 0                                                      # 持有股票重设为0
        }
      } else if (strategy.s[i] == 0){
        total.price.s[i] <- cash.temp.s + stock.price[i]*weight.s.temp
      } else {                                                                    # 买入信号
        if (i > 1 && strategy.s[i-1] > 0){
          message(paste("strategy fault",trade.info$stock.time[i],"short postion ","cash holding",
                        cash.temp.s,"stock holding",weight.s.temp,"continuous buy signal, only the first will be executed\n",sep = ";"))
          strategy.s[i] <- 0                                                      # 修改错误信号
          total.price.s[i] <- weight.s.temp*stock.price[i] + cash.temp.s
        } else if (i == 1){
          message(paste("strategy fault",trade.info$stock.time[i],"short postion ","cash holdinig",
                        cash.temp.s,"stock holding",weight.s.temp,"first trade signal is buy in short position, buy execution denied\n",sep = ";"))
          strategy.s[i] <- 0                                                      # 修改错误信号
          total.price.s[i] <- weight.s.temp*stock.price[i] + cash.temp.s
        } else if (cash.temp.s < stock.price[i]*100){
          message(paste("strategy fault",trade.info$stock.time[i],"short postion ","cash holding",
                        cash.temp.s,"stock holding",weight.s.temp,"not engouth cash, buy execution denied\n",sep = ";"))
          strategy.s[i] <- 0                                                      # 修改错误信号
          total.price.s[i] <- weight.s.temp*stock.price[i] + cash.temp.s
        }else {
          weight.s.temp <- floor(cash.temp.s/(stock.price[i])/100)*100
          cash.temp.s <- round(cash.temp.s - stock.price[i]*weight.s.temp, 2)
          total.price.s[i] <- weight.s.temp*stock.price[i] + cash.temp.s
        }
      }
    }else{
      total.price.s[i] <- 0
      weight.s <- 0
    }
    total.price[i] <- total.price.b[i] + total.price.s[i] - stock.price[i]*weight.s   # 更新总价值
  }

  if(!"buy" %in% strategy.type) total.price <- total.price + stock.price[1]*weight.s  # 单独做空时为方便检验效果，加上初始金额，100股除不尽的部分忽略

  # (3)评估参数计算

  # 3.1 总交易次数，买卖各算一次
  strategy.num <- sum(c(strategy.s > 0, strategy.b > 0))

  # 3.2 基准收益（向量）
  stock.price.win <- bm_return_v(stock.price, cash)

  # 3.3 基准收益率 （股票+现金当前价值 - 初始价值）/初始价值
  benchmark.return <- return_rate(stock.price)

  # 3.4 策略交易收益率
  strategy.return  <- return_rate(total.price)

  # 3.5 日交易基准收益率 （股票+现金当前价值-初始价值)/初始价值
  benchmark.day.return <- return_rate(stock.price, 'day', strategy.days)
  strategy.day.return  <- return_rate(total.price, 'day', strategy.days)

  # 3.6 基准年化收益(标量)
  benchmark.annual.return <- return_rate(stock.price, 'year', strategy.days)

  # 3.7 策略年化收益(标量)
  strategy.annual.return  <- return_rate(total.price, 'year', strategy.days)

  # 3.8 beta  cov(策略每日收益, 基准每日收益)/var(基准每日收益)
  # beta <- cov(diff(stock.price[loc.day])*(cash/stock.price[1]), diff(total.price[loc.day]))/
  #     var(diff(stock.price[loc.day])*(cash/stock.price[1]))
  beta <- beta_stock(stock.price[loc.day], total.price)

  # 3.9 alpha
  # alpha <- strategy.annual.return - (risk.free.rate + beta*(benchmark.annual.return - risk.free.rate))
  alpha <- alpha_stock(stock.price = stock.price, cash.begin = cash, trade.value = total.price,
                       rate = risk.free.rate, trade.days = strategy.days)

  # 3.10 Algorithm Volatility（策略波动率）
  # alg.vol <- sqrt(250*var(diff(total.price[loc.day])/total.price[loc.day][-strategy.days])) # 用来测量策略的风险性，波动越大代表策略风险越高。
  alg.vol <- algo_vol(total.price[loc.day])

  # 3.11夏普比率
  # sharpe.ratio <- (strategy.annual.return - risk.free.rate)/alg.vol
  sharpe.ratio <- sharpe_rate(trade.value = total.price, rate = risk.free.rate, trade.days = strategy.days)

  # 3.12信息比率
  # 策略与基准每日收益差值的年化标准差
  # strategy.benchmark.annual.sd <- sd(diff(stock.price[loc.day])*(cash/stock.price[1]) - diff(total.price[loc.day]))
  # info.ratio <- (strategy.annual.return - benchmark.annual.return)/strategy.benchmark.annual.sd
  info.ratio <- info_rate(stock.price = stock.price, cash.begin = cash, trade.value = total.price,
                          trade.days = strategy.days)

  # 3.13 基准波动率
  ben.vol <- sqrt(250*var(diff(stock.price[loc.day])/stock.price[loc.day[-length(loc.day)]]))

  # 3.14 最大回撤
  if(length(which(diff(total.price) == 0)) == 0){
    total.price.uniq <- total.price
  }else{
    total.price.uniq <- total.price[-(which(diff(total.price) == 0) + 1)]
  }
  loc.raw <- which(diff(total.price) != 0)

  loc.dw.1 <- which(diff(total.price.uniq) < 0)                                             # 下降序列点坐标，极大值居于左端点, 极小值居于右端点
  loc.dw.1.notcont <- loc.dw.1[c(1, which(diff(loc.dw.1) > 1) + 1)]
  # total.price.uniq[loc.dw.1.notcont]                                                      # 极大值点

  loc.dw.2 <- which(diff(total.price.uniq) > 0)                                             # 上升序列点坐标，极小值居于左端点
  loc.dw.2.notcont <- loc.dw.2[c(1, which(diff(loc.dw.2) > 1) + 1)]
  # total.price.uniq[loc.dw.2.notcont]   # 极小值点

  max.down.loc<-outer(loc.dw.1.notcont, loc.dw.2.notcont, function(x,y){x - y})             # 极大值点坐标小于极小值点的矩阵
  max.down.value<-outer(loc.dw.1.notcont, loc.dw.2.notcont, function(x,y){total.price.uniq[x] - total.price.uniq[y]})  # 对应的股价差

  # 找到满足条件的最大值
  id.xlessy <- which(max.down.loc < 0)
  max.id.loc <- which.max(as.vector(max.down.value)[id.xlessy])
  max.id <- id.xlessy[max.id.loc]
  if((max.id %% length(loc.dw.1.notcont))!=0){
    id.2 <- max.id %/% length(loc.dw.1.notcont) + 1                                         # 列的位置，极小值点的位置
    id.1 <- max.id %% length(loc.dw.1.notcont)                                              # 除以行的余数，极大值点的位置
  }else{
    id.1 <- length(loc.dw.1.notcont)                                                        # 标记能整除，除以行的余数，极大值点的位置
    id.2 <- max.id %/% length(loc.dw.1.notcont)
  }
  max.drawdown <- (total.price.uniq[loc.dw.1.notcont[id.1]] - total.price.uniq[loc.dw.2.notcont[id.2]])/
    total.price.uniq[loc.dw.1.notcont[id.1]]
  max.drawdown.loc <- c(loc.raw[loc.dw.1.notcont[id.1]], loc.raw[loc.dw.2.notcont[id.2]])
  max.down.date <- trade.info$stock.date[max.drawdown.loc]

  # 3.15 胜率
  if(length(strategy.type) == 2){
    trade.loc.b.2 <- which(strategy.b == -1)
    trade.loc.b.1 <- which(strategy.b == 1)

    trade.loc.s.2 <- which(strategy.s == -1)
    trade.loc.s.1 <- which(strategy.s == 1)

    # 多头交易收益率
    if(length(trade.loc.b.2) < length(trade.loc.b.1)) trade.loc.b.1 <- trade.loc.b.1[-length(trade.loc.b.1)]
    pertrade.winrate.b <- (total.price.b[trade.loc.b.2] - total.price.b[trade.loc.b.1])/total.price.b[trade.loc.b.1]
    # 空头交易收益率
    # 由于价值变动只在，本期买入和下期卖出时发生变化，因此依然用买入-卖出计算收益
    # 如果第一个点就做空，那么为了计算收益率就需要在最开始补入对应的买入操作（借股）
    if(trade.loc.s.2[1] == 1){
      total.price.s <- c(weight.s*stock.price[1], total.price.s)
      trade.loc.s.2 <- trade.loc.s.2 + 1
      trade.loc.s.1 <- c(1, trade.loc.s.1 + 1)
      if(length(trade.loc.s.1) > length(trade.loc.s.2)) {trade.loc.s.1 <- trade.loc.s.1[-length(trade.loc.s.1)]}
    } else {
      # 如果最后一个操作为卖空，意味着卖空信号比买入信号多一个，则将第一个策略定义为买入股票（对齐）
      if(length(trade.loc.s.2) > length(trade.loc.s.1)) trade.loc.s.1 <- c(1, trade.loc.s.1)
      # 如果最后一个操作为买入，则将第一个策略定义为买入股票，并剔除最后一个买入信号（对齐）
      if(length(trade.loc.s.2) == length(trade.loc.s.1)) trade.loc.s.1 <- c(1, trade.loc.s.1[-length(trade.loc.s.1)])
    }
    pertrade.winrate.s <- (total.price.s[trade.loc.s.2] - total.price.s[trade.loc.s.1])/total.price.s[trade.loc.s.1]
    win.rate <- (sum(pertrade.winrate.b > 0) + sum( pertrade.winrate.s > 0))/(length(trade.loc.b.2) + length(trade.loc.s.2))
  }else if(strategy.type == 'buy'){
    trade.loc.b.2 <- which(strategy.b == -1)
    trade.loc.b.1 <- which(strategy.b == 1)
    # 多头交易收益率
    if(length(trade.loc.b.2) < length(trade.loc.b.1)) trade.loc.b.1 <- trade.loc.b.1[-length(trade.loc.b.1)]
    pertrade.winrate.b <- (total.price.b[trade.loc.b.2] - total.price.b[trade.loc.b.1])/total.price.b[trade.loc.b.1]
    pertrade.winrate.s <- NA
    win.rate <- sum(pertrade.winrate.b > 0)/length(trade.loc.b.2)
  }else{
    trade.loc.s.2 <- which(strategy.s == 1)
    trade.loc.s.1 <- which(strategy.s == -1)
    # 空头交易收益率
    pertrade.winrate.b <- NA
    if(trade.loc.s.2[1] == 1){
      total.price.s <- c(weight.s*stock.price[1], total.price.s)
      trade.loc.s.2 <- trade.loc.s.2 + 1
      trade.loc.s.1 <- c(1, trade.loc.s.1 + 1)
    } else {
      # 如果最后一个操作为卖空，意味着卖空信号比买入信号多一个，则将第一个策略定义为买入股票（对齐）
      if(length(trade.loc.s.1) > length(trade.loc.s.2)){
        trade.loc.s.2 <- c(1, trade.loc.s.2)
      }else if(length(trade.loc.s.2) == length(trade.loc.s.1)){
      # 如果最后一个操作为买入，则将第一个策略定义为买入股票，并剔除最后一个买入信号（对齐）
        trade.loc.s.2 <- c(1, trade.loc.s.2[-length(trade.loc.s.2)])
      }
    }
    pertrade.winrate.s <- (total.price.s[trade.loc.s.2] - total.price.s[trade.loc.s.1])/total.price.s[trade.loc.s.1]
    win.rate <- sum(pertrade.winrate.s > 0)/length(trade.loc.s.1)
  }

  # 3.16 日胜率
  win.day.rate <- sum(diff(total.price[loc.day]) > diff(stock.price[loc.day])*(cash/stock.price[1]))/strategy.days

  # 3.17盈亏比
  diff.cash <- diff(total.price)
  win.lose.rate <- sum(diff.cash[diff.cash > 0])/abs(sum(diff.cash[diff.cash < 0]))

  # 3.18 正确率 & 3.19平均每笔收益率
  if(length(strategy.type) == 1){
    if(strategy.type == "buy"){
      # 正确率
      acc.rate.b <- pre_correct(stock.price, strategy.b = strategy.b)[1]
      acc.rate.s <- NA
      # 平均收益率
      mean.win.rate.b <- mean(pertrade.winrate.b)
      mean.win.rate.s <- NA
    } else {
      acc.rate.s <- pre_correct(stock.price, strategy.s = strategy.s)[2]
      acc.rate.b <- NA
      # 平均收益率
      mean.win.rate.b <- NA
      mean.win.rate.s <- mean(pertrade.winrate.s)
    }
  }else {
    acc.both <- pre_correct(stock.price, strategy.s = strategy.s, strategy.b = strategy.b)
    acc.rate.b <- acc.both[1]
    acc.rate.s <- acc.both[2]
    mean.win.rate.b <- mean(pertrade.winrate.b)
    mean.win.rate.s <- mean(pertrade.winrate.s)
  }

  # 3.20 输出
  all.features <- list(benchmark.return, strategy.return, benchmark.day.return, benchmark.day.return,
                       strategy.day.return, benchmark.annual.return, strategy.annual.return, beta, alpha,
                       alg.vol, sharpe.ratio,info.ratio, ben.vol, max.drawdown, win.rate, win.day.rate,
                       win.lose.rate, total.price, stock.price, stock.price.win)
  names(all.features) <- c("benchmark.return", "strategy.return", "benchmark.day.return", "benchmark.day.return",
                           "strategy.day.return", "benchmark.annual.return", "strategy.annual.return", "beta", "alpha",
                           "alg.vol", "sharpe.ratio","info.ratio", "ben.vol", "max.drawdown", "win.rate", "win.day.rate",
                           "win.lose.rate", "strategy.income", "stock.price", "stock.benchmarkwin")
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##---------------------------------------------------------------####
  # (三)绘图####
  # （1）加载包
  if (is.pic){
   #（2）绘图数据集
   # 2.1 收益率 VS 基准收益率

   if(plotlevel == "1day"){
     if(length(strategy.type) == 2){
       temp.df1 <- data.frame(trade.info[loc.day, c("stock.price", "stock.date", "strategy.b", "strategy.s")],
                            strategy.income = total.price[loc.day], stock.benchmarkwin = stock.price.win[loc.day])

     }else if(strategy.type == "buy"){
       temp.df1 <- data.frame(trade.info[loc.day, c("stock.price", "stock.date", "strategy.b")],
                              strategy.income = total.price[loc.day], stock.benchmarkwin = stock.price.win[loc.day])
     }else{
       temp.df1 <- data.frame(trade.info[loc.day, c("stock.price", "stock.date", "strategy.s")],
                              strategy.income = total.price[loc.day], stock.benchmarkwin = stock.price.win[loc.day])
     }
   }else if(plotlevel == "1min" & strategy.level == "1min"){
     if(length(strategy.type) == 2){
       temp.df1 <- data.frame(trade.info[, c("stock.price", "stock.date", "stock.time", "strategy.b", "strategy.s")],
                            strategy.income = total.price, stock.benchmarkwin = stock.price.win)
     }else if(strategy.type == "buy"){
       temp.df1 <- data.frame(trade.info[, c("stock.price", "stock.date", "stock.time", "strategy.b")],
                              strategy.income = total.price, stock.benchmarkwin = stock.price.win)
     }else{
       temp.df1 <- data.frame(trade.info[, c("stock.price", "stock.date", "stock.time","strategy.s")],
                              strategy.income = total.price, stock.benchmarkwin = stock.price.win)
     }
     temp.df1$id <- 1:nrow(temp.df1)
     temp.df1.breaks <- round(seq(1,nrow(temp.df1), length.out = 5))
   }else{
     stop("wrong plot level!")
   }

    all.features$strprice.df <- temp.df1

   # 2.2 指标表
   feature.name <- c("Benchmark Return(%)" , "Strategy Return(%)","Benchmark Daily Return(%)",
                     "Strategy Daily Return(%)", "Benchmark Annual Return(%)",
                     "Strategy Annual Return(%)", "Average Buy Return(long)", "Average Sell Return(short)",
                     "Algorithm Volatility(%)", "Benchmark Volatility(%)", "Sharpe Ratio(%)",
                     "Information Ratio(%)", "Max Drawdown(%)", "Beta", "Alpha", "Win Rate(%)",
                     "Daily Win Rate(%)", "Win Loss Ratio(%)", "Correct Rate Long(%)","Correct Rate Short(%)")
   temp.df3 <- data.frame(feature = feature.name,
                        value = round(c(benchmark.return*100, strategy.return*100, benchmark.day.return*100,
                                        strategy.day.return*100,benchmark.annual.return*100,
                                        strategy.annual.return*100, mean.win.rate.b*100, mean.win.rate.s*100,
                                        alg.vol*100, ben.vol*100, sharpe.ratio*100, info.ratio*100,
                                        max.drawdown*100, beta, alpha, win.rate*100,
                                        win.day.rate*100, win.lose.rate*100,acc.rate.b*100,
                                        acc.rate.s*100),2),
                        col = c(1,2,1,2,1,2,1,2,3,3,3,3,3,3,3,3,3,3,3,3),
                        id = length(feature.name):1,
                        stringsAsFactors = F)
   if(!is.simple){                                                                   # 简化显示输出
     # 2.3 交易分布，汇总
     if(length(strategy.type) == 1){
       # 交易分布
       if(strategy.type == "sell"){
         temp.xts4 <- xts(trade.info$strategy.s, trade.info$stock.date)
       }else{
         temp.xts4 <- xts(trade.info$strategy.b, trade.info$stock.date )
       }

       if(strategy.level == "60mins"){
         temp.xts4.monthly <- apply.monthly(temp.xts4, function(x){
           buy <- length(x[x > 0])
           hold <- length(x[x == 0])
           sell <- -length(x[x < 0])
           c(buy, hold, sell)
         })
         temp.df4 <- as.data.frame(temp.xts4.monthly)
         names(temp.df4) <- c("buy", "hold", "sell")
         temp.df4$date <- substr(time(temp.xts4.monthly),1,7)
       }else if(strategy.level == "1min"){
         temp.xts4.daily <- apply.daily(temp.xts4, function(x){
           buy <- length(x[x > 0])
           hold <- length(x[x == 0])
           sell <- -length(x[x < 0])
           c(buy, hold, sell)
         })
         temp.df4 <- as.data.frame(temp.xts4.daily)
         names(temp.df4) <- c("buy", "hold", "sell")
         temp.df4$date <- time(temp.xts4.daily)
       }else if(strategy.level == "1day"){
         temp.xts4.daily <- apply.daily(temp.xts4, function(x){
           buy <- length(x[x > 0])
           hold <- length(x[x == 0])
           sell <- -length(x[x < 0])
           c(buy, hold, sell)
         })
         temp.df4 <- as.data.frame(temp.xts4.daily)
         names(temp.df4) <- c("buy", "hold", "sell")
         temp.df4$date <- time(temp.xts4.daily)
       }

       temp.df4.long <- melt(temp.df4[,c("date","buy", "sell")], id = "date")
       # 交易次数
       temp.df5 <- temp.df4 %>% transform(., sell = -sell)
       temp.df5.long <- melt(temp.df5, id = "date")
       temp.df5.short <- ddply(temp.df5.long, .(variable), function(x){sum(x$value)})
     }else{
       # 交易分布
       temp.xts4 <- xts(trade.info[,c("strategy.b", "strategy.s")], trade.info$stock.date)
       if(strategy.level == "60mins"){
         temp.xts4.monthly <- apply.monthly(temp.xts4, function(x){
           b <- x[,1]
           s <- x[,2]
           buy.b <- length(b[b > 0])
           hold.b <- length(b[b == 0])
           sell.b <- -length(b[b < 0])
           buy.s <- length(s[s > 0])
           hold.s <- length(s[s == 0])
           sell.s <- -length(s[s < 0])
           c(buy.b, hold.b, sell.b, buy.s, hold.s, sell.s)
         })
         temp.df4 <- as.data.frame(temp.xts4.monthly)
         names(temp.df4) <- c("buy.b", "hold.b", "sell.b","buy.s", "hold.s", "sell.s")
         temp.df4$date <- substr(time(temp.xts4.monthly),1,7)
       }else if(strategy.level == "1min"){
         temp.xts4.daily <- apply.daily(temp.xts4, function(x){
           b <- x[,1]
           s <- x[,2]
           buy.b <- length(b[b > 0])
           hold.b <- length(b[b == 0])
           sell.b <- -length(b[b < 0])
           buy.s <- length(s[s > 0])
           hold.s <- length(s[s == 0])
           sell.s <- -length(s[s < 0])
           c(buy.b, hold.b, sell.b, buy.s, hold.s, sell.s)

         })
         temp.df4 <- as.data.frame(temp.xts4.daily)
         names(temp.df4) <-  c("buy.b", "hold.b", "sell.b","buy.s", "hold.s", "sell.s")
         temp.df4$date <- time(temp.xts4.daily)
       }else if(strategy.level == "1day"){
         temp.xts4.daily <- apply.daily(temp.xts4, function(x){
           b <- x[,1]
           s <- x[,2]
           buy.b <- length(b[b > 0])
           hold.b <- length(b[b == 0])
           sell.b <- -length(b[b < 0])
           buy.s <- length(s[s > 0])
           hold.s <- length(s[s == 0])
           sell.s <- -length(s[s < 0])
           c(buy.b, hold.b, sell.b, buy.s, hold.s, sell.s)

         })
         temp.df4 <- as.data.frame(temp.xts4.daily)
         names(temp.df4) <-  c("buy.b", "hold.b", "sell.b","buy.s", "hold.s", "sell.s")
         temp.df4$date <- time(temp.xts4.daily)
       }

       temp.df4.long <- melt(temp.df4[,c("date","buy.b" ,"sell.b","buy.s", "sell.s")], id = "date")
       temp.df4.long$class <- "sell"
       temp.df4.long$class[which(temp.df4.long$variable %in% c("buy.b", "hold.b", "sell.b"))] <- "buy"
       # 交易次数
       temp.df5 <- temp.df4 %>% transform(., sell.b = -sell.b, sell.s = -sell.s)
       temp.df5.long <- melt(temp.df5, id = "date")
       temp.df5.short <- ddply(temp.df5.long, .(variable), function(x){sum(x$value)})
       temp.df5.short$class <- "buy"
       temp.df5.short$class[which(temp.df5.short$variable %in% c("buy.s", "hold.s", "sell.s"))] <- "sell"
       temp.df5.short$state <- c("buy", "hold", "sell","buy", "hold", "sell")
     }

     # 2.4 收益率数据
     if(length(strategy.type) > 1){
       temp.df6.s <- data.frame(total.price.rate = pertrade.winrate.s,             # 每笔交易收益率
                                stock.time = trade.info$stock.time[trade.loc.s.2], # 交易完成时间
                                class = "c.short",rate = tax.rate, stringsAsFactors = F)
       temp.df6.b <- data.frame(total.price.rate = pertrade.winrate.b,
                                stock.time = trade.info$stock.time[trade.loc.b.2],
                                class = 'b.long',rate = tax.rate, stringsAsFactors = F)

       temp.df6.temp <- full_join(temp.df6.s[,c("stock.time","total.price.rate")],temp.df6.b[,c("stock.time","total.price.rate")], by = "stock.time")
       temp.df6.temp$total.price.rate.x[is.na(temp.df6.temp$total.price.rate.x)] <- 0
       temp.df6.temp$total.price.rate.y[is.na(temp.df6.temp$total.price.rate.y)] <- 0
       temp.rate <- temp.df6.temp$total.price.rate.x + temp.df6.temp$total.price.rate.y
       temp.df6.temp$rate <- tax.rate
       temp.df6.temp$rate[((temp.df6.temp$total.price.rate.x != 0) + (temp.df6.temp$total.price.rate.y != 0)) == 2] <- tax.rate*2

       temp.df6.all <- data.frame(stock.time = temp.df6.temp$stock.time,
                                  total.price.rate = temp.rate,
                                  class = "a.comprehensive", rate = temp.df6.temp$rate, stringsAsFactors = F)
       temp.df6 <- bind_rows(temp.df6.all, temp.df6.b, temp.df6.s)
     }else if(strategy.type == "buy"){
       temp.df6 <- data.frame(total.price.rate = pertrade.winrate.b,
                              stock.time = trade.info$stock.time[trade.loc.b.2],
                              class = 'b.buy',rate = tax.rate, stringsAsFactors = F)
     }else{
       temp.df6 <- data.frame(total.price.rate = pertrade.winrate.s,
                              stock.time = trade.info$stock.time[trade.loc.s.2],
                              class = "c.short",rate = tax.rate, stringsAsFactors = F)
     }
     # 收益率统计
     temp.df6.rate <- ddply(temp.df6, .(class), function(x){
       up <- sum(x$total.price.rate > tax.rate)/nrow(x)
       down <- sum(x$total.price.rate < -tax.rate)/nrow(x)
       yup <- max(x$total.price.rate)/2
       ydown <- min(x$total.price.rate)/2
       temp <- data.frame(value = paste(round(c(up,down)*100, 1), "%", sep = ""),
                          y = c(yup, ydown))
     })
     temp.df6.rate$time <- min(temp.df6$stock.time)
   }

   # (3)绘图模块
   # 3.1 绘图主题
   theme1 <- theme(
     legend.position = 'none',
     panel.background = element_rect(fill = "black",colour = "grey40"),
     panel.grid.major = element_line(colour = "grey20", linetype = "dashed"),
     panel.grid.minor = element_line(colour = "grey10", linetype = "dashed"),
     axis.text = element_text(face = "bold", size = 10, colour = "orange"),
     plot.background = element_rect(fill = "black",colour = "black",size = 2),
     plot.title = element_text(face = "bold",size = 14, colour = "gold", hjust = .5)
   )

   theme2 <- theme(
     legend.position = 'none',
     panel.background = element_rect(fill = "black",colour = "grey20"),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     strip.background = element_rect(fill = "darkmagenta",colour = "black"),
     strip.text = element_text(face = "bold",size = 14, colour = "gold"),
     legend.background = element_rect(fill = "black", colour = "black"),
     legend.text = element_text(colour = "gold"),
     axis.text = element_text(face = "bold", size = 12, colour = "orange"),
     plot.background = element_rect(fill = "black",colour = "black",size = 2),
     plot.title = element_text(face = "bold",size = 12, colour = "gold", hjust = .5)
   )

   theme3 <- theme(
     legend.position = 'none',
     panel.background = element_rect(fill = "black",colour = "grey70"),
     panel.grid.major.x = element_line(colour = "grey10", linetype = "dashed"),
     panel.grid.minor.x = element_line(colour = "grey10", linetype = "dashed"),
     axis.line.y = element_line(colour = "black", linetype = "dotted"),
     axis.ticks.y = element_line(colour = "white"),
     panel.grid.major.y = element_line(colour = "grey10", linetype = "dashed"),
     panel.grid.minor.y = element_line(colour = "grey40", linetype = "dashed"),
     axis.text.x = element_text(face = "bold", size = 12, colour = "orange"),
     axis.text.x.top = element_text(face = "bold", size = 12, colour = "red"),
     axis.text.y = element_text(face = "bold", size = 14, colour = "orange"),
     axis.text.y.right = element_text(face = "bold", size = 15, colour = "red"),
     plot.background = element_rect(fill = "black",colour = "black",size = 2),
     plot.title = element_text(face = "bold",size = 12, colour = "gold", hjust = .5)
   )

   # 3.2 绘图
   if (plotlevel == "1day"){                                                                                              # a.基准收益率
     pic1 <- ggplot(temp.df1)+
       geom_hline(yintercept = cash , col = "orange", size = .5)+theme1+
       geom_line(aes(x = stock.date, y = strategy.income), col = "gold")+
       geom_line(aes(x = stock.date, y = stock.benchmarkwin), col = "red")+
       ggtitle("strategy-return(yellow) vs benchmarket-return(red)")
     pic2 <- ggplot(temp.df1)+                                                                                            # b.回撤
       geom_hline(yintercept = cash , col = "orange", size = .5)+theme1+
       geom_line(aes(x = stock.date, y = strategy.income), col = "gold")+
       geom_point(data  = temp.df1[max.drawdown.loc,],aes(x = stock.date, y = strategy.income), col = "lemonchiffon", size = 2)+
       geom_line(data  = temp.df1[max.drawdown.loc,],aes(x = stock.date, y = strategy.income), col = "greenyellow", size = 1)+
       ggtitle(paste("Strategy return : max drawdown: ", round(max.drawdown*100,2),"%",sep = ""))
     pic4 <- ggplot(temp.df1)+                                                                                            # c.超额收益
       geom_hline(yintercept = 0 , col = "orange", size = .5)+
       geom_line(aes(x = stock.date, y = strategy.income - stock.benchmarkwin), col = "magenta")+theme1+
       ggtitle("Excess Return")
   } else if (plotlevel == "1min"){
     pic1 <- ggplot(temp.df1)+
       geom_hline(yintercept = cash , col = "orange", size = .5)+theme1+
       geom_line(aes(x = id, y = strategy.income), col = "gold", alpha = .8)+
       geom_line(aes(x = id, y = stock.benchmarkwin), col = "red", alpha = .8)+
       scale_x_continuous(breaks = temp.df1.breaks, labels = temp.df1$stock.date[temp.df1.breaks])+
       ggtitle("strategy-return(yellow) vs benchmarket-return(red)")
     pic2 <- ggplot(temp.df1)+                                                                                            # b.回撤
       geom_hline(yintercept = cash , col = "orange", size = .5)+theme1+
       geom_line(aes(x = id, y = strategy.income), col = "gold")+
       geom_point(data  = temp.df1[max.drawdown.loc,],aes(x = id, y = strategy.income), col = "lemonchiffon", size = 2)+
       geom_line(data  = temp.df1[max.drawdown.loc,],aes(x = id, y = strategy.income), col = "greenyellow", size = 1)+
       scale_x_continuous(breaks = temp.df1.breaks, labels = temp.df1$stock.date[temp.df1.breaks])+
       ggtitle(paste("Strategy return : max drawdown: ", round(max.drawdown*100,2),"%",sep = ""))
     pic4 <- ggplot(temp.df1)+                                                                                           # c.超额收益
       geom_hline(yintercept = 0 , col = "orange", size = .5)+
       geom_line(aes(x = id, y = strategy.income - stock.benchmarkwin), col = "magenta")+theme1+
       scale_x_continuous(breaks = temp.df1.breaks, labels = temp.df1$stock.date[temp.df1.breaks])+
       ggtitle("Excess Return")
   }

   pic3 <- ggplot(temp.df3)+                                                                                             # d.指数表
     geom_hline(yintercept = 0 , col = "white")+
     geom_vline(xintercept = nrow(temp.df3) - 7.5 , col = "white")+
     geom_vline(xintercept = nrow(temp.df3) - 12.5 , col = "white")+theme3+
     geom_vline(xintercept = nrow(temp.df3) - 14.5 , col = "white")+theme3+
     geom_bar(stat = "identity", aes(x = id, y = value, fill = factor(col)), width = .25)+
     geom_point(aes(x = id, y = value, col = factor(col)), size = 6)+
     scale_fill_manual(values = c("1" = "red", "2" = "gold", "3" = "royalblue3"))+
     scale_x_continuous(position = "bottom", expand = c(0.025, 0),breaks = nrow(temp.df3):1, labels = temp.df3$feature,
                           sec.axis = sec_axis(~ ., name = "", breaks = nrow(temp.df3):1, labels = temp.df3$value))+
     ggtitle(paste("Stock Code:", trade.info$wind_code[1]))+
     coord_flip()
   if(!is.simple){
     if(length(strategy.type) == 1){                                                                                     # e.交易分布
       pic5 <- ggplot(temp.df4.long)+
         geom_bar(aes(x = date, y = value, fill = variable), width = .8,stat = "identity", position = "dodge")+
         geom_hline(yintercept = 0 , col = "orange", size = .5)+theme2+
         scale_fill_manual(values = c("sell" = "royalblue3", "buy" = "red"))
       if(strategy.type == "buy") pic5 <- pic5 + ggtitle("Trade distribution of long position")
       if(strategy.type == "sell") pic5 <- pic5 + ggtitle("Trade distribution of short position")
     }else{
       pic5 <- ggplot(temp.df4.long)+
         facet_wrap(~class, nrow = 2,strip.position = "left")+
         geom_bar(aes(x = date, y = value, fill = variable), width = .8,stat = "identity", position = "dodge")+
         geom_hline(yintercept = 0 , col = "orange", size = .5)+theme2+
         scale_fill_manual(values = c("sell.b" = "royalblue3", "buy.b" = "red","sell.s" = "royalblue1", "buy.s" = "orangered"))
     }

     if(length(strategy.type) == 1){                                                                                     # f.交易次数
       pic6 <- ggplot(temp.df5.short)+
         geom_bar(aes(x = reorder(variable,V1), y = V1, fill = reorder(variable,V1)), stat = "identity", width = .8)+
         geom_text(aes(x = reorder(variable,V1), y = max(V1)/2, label = V1), col = "orange", size = 8)+theme2+
         scale_fill_manual(name = NULL,values = c("buy" = "red","hold" = "mediumblue", "sell" = "royalblue3"))
       if(strategy.type == "buy") pic6 <- pic6 + ggtitle("Trade num of long position")
       if(strategy.type == "sell") pic6 <- pic6 + ggtitle("Trade num of short position")
     }else{
       pic6 <- ggplot(temp.df5.short)+
         facet_wrap(~class, ncol = 1, strip.position = "left")+
         geom_bar(aes(x = reorder(state,V1), y = V1, fill = reorder(state,V1)), stat = "identity", width = .8)+
         geom_text(aes(x = reorder(state,V1), y = max(V1)/2, label = V1), col = "orange", size = 8)+ theme2 +
         scale_fill_manual(name = NULL,values = c("buy" = "red","hold" = "mediumblue", "sell" = "royalblue3"))+
         ggtitle("Trade Number")
     }

     pic7 <- ggplot(temp.df6)+                                                                                          # g.收益率分布
       facet_wrap(~class, strip.position = "left", ncol = 1, scale = "free_y")+
       geom_hline(yintercept = 0 , col = "orange3", size = .5)+
       geom_segment(aes(x = stock.time,xend = stock.time, yend = 0, y = total.price.rate , col = class), alpha = .5)+
       geom_point(aes(x = stock.time, y = total.price.rate , col = class), size = .5)+
       geom_line(aes(x = stock.time, y = rate), col = "orange", linetype = "dashed")+
       geom_line(aes(x = stock.time, y = -rate), col = "orange", linetype = "dashed")+
       geom_text(data = temp.df6.rate, aes(x = time, y = y, label = value), size = 4, col = "orange")+theme2+
       scale_color_manual(values = c("a.comprehensive" = "greenyellow", "b.long" = "orangered", "c.short" = "royalblue1"))
   }

   pic0 <- ggplot()+theme1

   if (is.simple){
     if(is.pic.whole){
       lay <- rbind(c(1,1,2,2,2,2),c(1,1,2,2,2,2), c(1,1,3,3,4,4))
       pic <- arrangeGrob(pic3, pic1, pic4, pic2, ncol = 2,layout_matrix = lay)
     }else{
       pic <- list('pic1' = pic1, 'pic2' = pic2, 'pic3' = pic3, 'pic4' = pic4)
     }
   } else {
     if(is.pic.whole){
       lay <- rbind(c(1,1,2,2,2,2),c(1,1,2,2,2,2),c(1,1,3,3,3,3),c(5,5,5,6,4,4),c(5,5,5,7,7,7))                           # h.组合绘图
       pic <- arrangeGrob(pic3, pic1, pic4, pic2, pic7, pic6, pic5, ncol = 2,layout_matrix = lay)
     }else{
       pic <- list('pic1' = pic1, 'pic2' = pic2, 'pic3' = pic3, 'pic4' = pic4, 'pic5' = pic5, 'pic6' = pic6, 'pic7' = pic7)
     }
   }
   # pic <- grid.arrange(pic3, pic1, pic4, pic2, pic7, pic6, pic5, ncol = 2,layout_matrix = lay)
   # pic <- arrangeGrob(pic3, pic1, pic2, pic4, pic5, pic6, ncol = 2,layout_matrix = lay)
   # print(pic)
  } else {
      pic <- NULL
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##---------------------------------------------------------------####
  #(四)输出####
  all.features$pic <- pic
  return(all.features)
}

