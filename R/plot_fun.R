
#' Plot a quantler object
#'
#' A generic function of class quantler.
#'
#' @param x A quantler object
#' @param plotlevel A character variable must be one of c('1day', '60mins',
#'   '1min')
#' @param segments A logical variable. When segments is TRUE this function will
#'   return a list contians all the sub pictures, and when FALSE plot all the
#'   segments together.
#'
#' @return A picture or a list contains all the segments of the picture.
#' @examples
#'    data(stradeinfo_bp)
#'    test.qtler <- backtest(stradeinfo_bp, cash = 100000, is.tax=T, tax.rate = 0.002)
#'    plot(test.qtler)
#' @export
setMethod(
  "plot",
  signature(x = "quantler", y = "missing"),
  function(x, ..., plotlevel = "1day", is.simple = F, segments = F) {
    if (!is.quantler(x)) stop("x must be a quantler object")
    if (length(plotlevel) == 0 || !plotlevel %in% c("1day", "60mins", "1min")) {
      stop("Not correct form of plot.level")
    }
    if (is.na(is.simple) || !is.logical(is.simple)) stop("Not correct form of is.simple")
    if (is.na(segments) || !is.logical(segments)) stop("Not correct form of segments")

    trade.info <- x@trade.info
    stock.date <- trade.info$stock.date
    len <- nrow(trade.info)
    strategy.type <- x@bt.features$strategy.type
    trade.value <- x@bt.features$trade.value
    stock.benchmarkwin <- x@bt.features$stock.benchmarkwin
    strategy.type <- x@bt.features$strategy.type
    tax.rate <- x@bt.features$tax.rate
    cash <- x@bt.features$cash

    if (plotlevel == "1day") {
      loc.day <- c(which(diff(stock.date) >= 1), len)

      if (length(strategy.type) == 2) {
        temp.df1 <- data.frame(
          trade.info[loc.day, c("stock.price", "stock.date", "strategy.b", "strategy.s")],
          strategy.income = trade.value[loc.day],
          stock.benchmarkwin = stock.benchmarkwin[loc.day]
        )
      } else if (strategy.type == "buy") {
        temp.df1 <- data.frame(
          trade.info[loc.day, c("stock.price", "stock.date", "strategy.b")],
          strategy.income = trade.value[loc.day],
          stock.benchmarkwin = stock.benchmarkwin[loc.day]
        )
      } else {
        temp.df1 <- data.frame(
          trade.info[loc.day, c("stock.price", "stock.date", "strategy.s")],
          strategy.income = trade.value[loc.day],
          stock.benchmarkwin = stock.benchmarkwin[loc.day]
        )
      }
    } else if (plotlevel == "1min" & x@data.level == "1min") {
      if (length(strategy.type) == 2) {
        temp.df1 <- data.frame(
          trade.info[, c("stock.price", "stock.date", "stock.time", "strategy.b", "strategy.s")],
          strategy.income = trade.value,
          stock.benchmarkwin = stock.benchmarkwin
        )
      } else if (strategy.type == "buy") {
        temp.df1 <- data.frame(
          trade.info[, c("stock.price", "stock.date", "stock.time", "strategy.b")],
          strategy.income = trade.value,
          stock.benchmarkwin = stock.benchmarkwin
        )
      } else {
        temp.df1 <- data.frame(
          trade.info[, c("stock.price", "stock.date", "stock.time", "strategy.s")],
          strategy.income = trade.value,
          stock.benchmarkwin = stock.benchmarkwin
        )
      }
      temp.df1$id <- 1:nrow(temp.df1)
      temp.df1.breaks <- round(seq(1, nrow(temp.df1), length.out = 5))
    } else {
      stop("Plot level dose not match the given data!")
    }

    # all.features$strprice.df <- temp.df1

    # 2.2 指标表
    feature.name <- c(
      "Benchmark Return(%)",
      "Strategy Return(%)",
      "Benchmark Daily Return(%)",
      "Strategy Daily Return(%)",
      "Benchmark Annual Return(%)",
      "Strategy Annual Return(%)",
      "Average Buy Return(long)",
      "Average Sell Return(short)",
      "Algorithm Volatility(%)",
      "Benchmark Volatility(%)",
      "Sharpe Ratio(%)",
      "Information Ratio(%)",
      "Max Drawdown(%)",
      "Beta",
      "Alpha",
      "Win Rate(%)",
      "Daily Win Rate(%)",
      "Win Loss Ratio(%)",
      "Correct Rate Long(%)",
      "Correct Rate Short(%)"
    )
    temp.df3 <- data.frame(
      feature = feature.name,
      value = round(c(
        x@bt.features$benchmark.return,
        x@bt.features$strategy.return,
        x@bt.features$benchmark.day.return,
        x@bt.features$strategy.day.return,
        x@bt.features$benchmark.annual.return,
        x@bt.features$strategy.annual.return,
        x@bt.features$mean.win.rate.b,
        x@bt.features$mean.win.rate.s,
        x@bt.features$alg.vol,
        x@bt.features$ben.vol,
        x@bt.features$sharpe.ratio,
        x@bt.features$info.ratio,
        x@bt.features$max.drawdown,
        x@bt.features$beta / 100,
        x@bt.features$alpha / 100,
        x@bt.features$win.rate,
        x@bt.features$win.day.rate,
        x@bt.features$win.lose.rate,
        x@bt.features$acc.rate.b,
        x@bt.features$acc.rate.s
      ) * 100, 2),
      col = c(1, 2, 1, 2, 1, 2, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3),
      id = length(feature.name):1,
      stringsAsFactors = F
    )
    if (!is.simple) { # 简化显示输出
      # 2.3 交易分布，汇总
      if (length(strategy.type) == 1) {
        # 交易分布
        if (strategy.type == "sell") {
          temp.xts4 <- xts(trade.info$strategy.s, stock.date)
        } else {
          temp.xts4 <- xts(trade.info$strategy.b, stock.date)
        }

        if (x@data.level == "60mins") {
          temp.xts4.monthly <- apply.monthly(temp.xts4, function(x) {
            buy <- length(x[x > 0])
            hold <- length(x[x == 0])
            sell <- -length(x[x < 0])
            c(buy, hold, sell)
          })
          temp.df4 <- as.data.frame(temp.xts4.monthly)
          names(temp.df4) <- c("buy", "hold", "sell")
          temp.df4$date <- substr(time(temp.xts4.monthly), 1, 7)
        } else if (x@data.level == "1min") {
          temp.xts4.daily <- apply.daily(temp.xts4, function(x) {
            buy <- length(x[x > 0])
            hold <- length(x[x == 0])
            sell <- -length(x[x < 0])
            c(buy, hold, sell)
          })
          temp.df4 <- as.data.frame(temp.xts4.daily)
          names(temp.df4) <- c("buy", "hold", "sell")
          temp.df4$date <- time(temp.xts4.daily)
        } else if (x@data.level == "1day") {
          temp.xts4.daily <- apply.daily(temp.xts4, function(x) {
            buy <- length(x[x > 0])
            hold <- length(x[x == 0])
            sell <- -length(x[x < 0])
            c(buy, hold, sell)
          })
          temp.df4 <- as.data.frame(temp.xts4.daily)
          names(temp.df4) <- c("buy", "hold", "sell")
          temp.df4$date <- time(temp.xts4.daily)
        }

        temp.df4.long <- reshape2::melt(temp.df4[, c("date", "buy", "sell")], id = "date")
        # 交易次数
        temp.df5 <- transform(temp.df4, sell = -sell)
        temp.df5.long <- reshape2::melt(temp.df5, id = "date")
        temp.df5.short <- plyr::ddply(temp.df5.long, .(variable), function(x) {
          sum(x$value)
        })
      } else {
        # 交易分布
        temp.xts4 <- xts(trade.info[, c("strategy.b", "strategy.s")], stock.date)
        if (x@data.level == "60mins") {
          temp.xts4.monthly <- apply.monthly(temp.xts4, function(x) {
            b <- x[, 1]
            s <- x[, 2]
            buy.b <- length(b[b > 0])
            hold.b <- length(b[b == 0])
            sell.b <- -length(b[b < 0])
            buy.s <- length(s[s > 0])
            hold.s <- length(s[s == 0])
            sell.s <- -length(s[s < 0])
            c(buy.b, hold.b, sell.b, buy.s, hold.s, sell.s)
          })
          temp.df4 <- as.data.frame(temp.xts4.monthly)
          names(temp.df4) <- c("buy.b", "hold.b", "sell.b", "buy.s", "hold.s", "sell.s")
          temp.df4$date <- substr(time(temp.xts4.monthly), 1, 7)
        } else if (x@data.level == "1min") {
          temp.xts4.daily <- apply.daily(temp.xts4, function(x) {
            b <- x[, 1]
            s <- x[, 2]
            buy.b <- length(b[b > 0])
            hold.b <- length(b[b == 0])
            sell.b <- -length(b[b < 0])
            buy.s <- length(s[s > 0])
            hold.s <- length(s[s == 0])
            sell.s <- -length(s[s < 0])
            c(buy.b, hold.b, sell.b, buy.s, hold.s, sell.s)
          })
          temp.df4 <- as.data.frame(temp.xts4.daily)
          names(temp.df4) <- c("buy.b", "hold.b", "sell.b", "buy.s", "hold.s", "sell.s")
          temp.df4$date <- time(temp.xts4.daily)
        } else if (x@data.level == "1day") {
          temp.xts4.daily <- apply.daily(temp.xts4, function(x) {
            b <- x[, 1]
            s <- x[, 2]
            buy.b <- length(b[b > 0])
            hold.b <- length(b[b == 0])
            sell.b <- -length(b[b < 0])
            buy.s <- length(s[s > 0])
            hold.s <- length(s[s == 0])
            sell.s <- -length(s[s < 0])
            c(buy.b, hold.b, sell.b, buy.s, hold.s, sell.s)
          })
          temp.df4 <- as.data.frame(temp.xts4.daily)
          names(temp.df4) <- c("buy.b", "hold.b", "sell.b", "buy.s", "hold.s", "sell.s")
          temp.df4$date <- time(temp.xts4.daily)
        }

        temp.df4.long <- reshape2::melt(temp.df4[, c("date", "buy.b", "sell.b", "buy.s", "sell.s")], id = "date")
        temp.df4.long$class <- "sell"
        temp.df4.long$class[which(temp.df4.long$variable %in% c("buy.b", "hold.b", "sell.b"))] <- "buy"
        # 交易次数
        temp.df5 <- transform(temp.df4, sell.b = -sell.b, sell.s = -sell.s)
        temp.df5.long <- reshape2::melt(temp.df5, id = "date")
        temp.df5.short <- plyr::ddply(temp.df5.long, .(variable), function(x) {
          sum(x$value)
        })
        temp.df5.short$class <- "buy"
        temp.df5.short$class[which(temp.df5.short$variable %in% c("buy.s", "hold.s", "sell.s"))] <- "sell"
        temp.df5.short$state <- c("buy", "hold", "sell", "buy", "hold", "sell")
      }

      # 2.4 收益率数据
      if (length(strategy.type) > 1) {
        temp.df6.s <- data.frame(
          trade.value.rate = x@bt.features$win.rate.s$pertrade.winrate, # 每笔交易收益率
          stock.time = trade.info$stock.time[which(trade.info$strategy.s == -1)], # 交易完成时间
          class = "c.short", rate = tax.rate, stringsAsFactors = F
        )
        temp.df6.b <- data.frame(
          trade.value.rate = x@bt.features$win.rate.b$pertrade.winrate,
          stock.time = trade.info$stock.time[which(trade.info$strategy.b == -1)],
          class = "b.long", rate = tax.rate, stringsAsFactors = F
        )

        temp.df6.temp <- dplyr::full_join(temp.df6.s[, c("stock.time", "trade.value.rate")],
          temp.df6.b[, c("stock.time", "trade.value.rate")],
          by = "stock.time"
        )
        temp.df6.temp$trade.value.rate.x[is.na(temp.df6.temp$trade.value.rate.x)] <- 0
        temp.df6.temp$trade.value.rate.y[is.na(temp.df6.temp$trade.value.rate.y)] <- 0
        temp.rate <- temp.df6.temp$trade.value.rate.x + temp.df6.temp$trade.value.rate.y
        temp.df6.temp$rate <- tax.rate
        temp.df6.temp$rate[((temp.df6.temp$trade.value.rate.x != 0) + (temp.df6.temp$trade.value.rate.y != 0)) == 2] <- tax.rate * 2

        temp.df6.all <- data.frame(
          stock.time = temp.df6.temp$stock.time,
          trade.value.rate = temp.rate,
          class = "a.both",
          rate = temp.df6.temp$rate, stringsAsFactors = F
        )
        temp.df6 <- dplyr::bind_rows(temp.df6.all, temp.df6.b, temp.df6.s)
      } else if (strategy.type == "buy") {
        temp.df6 <- data.frame(
          trade.value.rate = x@bt.features$win.rate.b$pertrade.winrate,
          stock.time = trade.info$stock.time[which(trade.info$strategy.b == -1)],
          class = "b.buy", rate = tax.rate, stringsAsFactors = F
        )
      } else {
        temp.df6 <- data.frame(
          trade.value.rate = x@bt.features$win.rate.s$pertrade.winrate,
          stock.time = trade.info$stock.time[which(trade.info$strategy.s == -1)],
          class = "c.short", rate = tax.rate, stringsAsFactors = F
        )
      }
      # 收益率统计
      temp.df6.rate <- plyr::ddply(temp.df6, .(class), function(x) {
        up <- sum(x$trade.value.rate > tax.rate) / nrow(x)
        down <- sum(x$trade.value.rate < -tax.rate) / nrow(x)
        yup <- max(x$trade.value.rate) / 2
        ydown <- min(x$trade.value.rate) / 2
        temp <- data.frame(
          value = paste(round(c(up, down) * 100, 1), "%", sep = ""),
          y = c(yup, ydown)
        )
      })
      temp.df6.rate$time <- min(temp.df6$stock.time)
    }

    # (3)绘图模块
    # 3.1 绘图主题
    theme1 <- theme(
      legend.position = "none",
      panel.background = element_rect(fill = "black", colour = "grey40"),
      panel.grid.major = element_line(colour = "grey20", linetype = "dashed"),
      panel.grid.minor = element_line(colour = "grey10", linetype = "dashed"),
      axis.text = element_text(face = "bold", size = 10, colour = "orange"),
      plot.background = element_rect(fill = "black", colour = "black", size = 2),
      plot.title = element_text(face = "bold", size = 12, colour = "gold", hjust = .5)
    )

    theme2 <- theme(
      legend.position = "none",
      panel.background = element_rect(fill = "black", colour = "grey20"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "darkmagenta", colour = "black"),
      strip.text = element_text(face = "bold", size = 10, colour = "gold"),
      legend.background = element_rect(fill = "black", colour = "black"),
      legend.text = element_text(colour = "gold"),
      axis.text = element_text(face = "bold", size = 12, colour = "orange"),
      plot.background = element_rect(fill = "black", colour = "black", size = 2),
      plot.title = element_text(face = "bold", size = 12, colour = "gold", hjust = .5)
    )

    theme3 <- theme(
      legend.position = "none",
      panel.background = element_rect(fill = "black", colour = "grey70"),
      panel.grid.major.x = element_line(colour = "grey10", linetype = "dashed"),
      panel.grid.minor.x = element_line(colour = "grey10", linetype = "dashed"),
      axis.line.y = element_line(colour = "black", linetype = "dotted"),
      axis.ticks.y = element_line(colour = "white"),
      panel.grid.major.y = element_line(colour = "grey10", linetype = "dashed"),
      panel.grid.minor.y = element_line(colour = "grey40", linetype = "dashed"),
      axis.text.x = element_text(face = "bold", size = 12, colour = "orange"),
      axis.text.x.top = element_text(face = "bold", size = 12, colour = "red"),
      axis.text.y = element_text(face = "bold", size = 12, colour = "orange"),
      axis.text.y.right = element_text(face = "bold", size = 12, colour = "red"),
      plot.background = element_rect(fill = "black", colour = "black", size = 2),
      plot.title = element_text(face = "bold", size = 12, colour = "gold", hjust = .5)
    )

    # 3.2 绘图
    if (plotlevel == "1day") { # a.基准收益率
      pic1 <- ggplot(temp.df1) +
        geom_hline(yintercept = cash, col = "orange", size = .5) + theme1 +
        geom_line(aes(x = stock.date, y = strategy.income), col = "gold") +
        geom_line(aes(x = stock.date, y = stock.benchmarkwin), col = "red") +
        ggtitle("strategy-return(yellow) vs benchmarket-return(red)")
      pic2 <- ggplot(temp.df1) + # b.回撤
        geom_hline(yintercept = cash, col = "orange", size = .5) + theme1 +
        geom_line(aes(x = stock.date, y = strategy.income), col = "gold") +
        geom_point(
          data = temp.df1[x@bt.features$max.drawdown.loc, ],
          aes(x = stock.date, y = strategy.income), col = "lemonchiffon", size = 2
        ) +
        geom_line(
          data = temp.df1[x@bt.features$max.drawdown.loc, ],
          aes(x = stock.date, y = strategy.income), col = "greenyellow", size = 1
        ) +
        ggtitle(paste(
            "Strategy return : max drawdown: ",
            round(x@bt.features$max.drawdown * 100, 2),
            "%",
            sep = ""
            ))
      pic4 <- ggplot(temp.df1) + # c.超额收益
        geom_hline(yintercept = 0, col = "orange", size = .5) +
        geom_line(aes(x = stock.date, y = strategy.income - stock.benchmarkwin), col = "magenta") +
        theme1 +
        ggtitle("Excess Return")
    } else if (plotlevel == "1min") {
      pic1 <- ggplot(temp.df1) +
        geom_hline(yintercept = cash, col = "orange", size = .5) +
        theme1 +
        geom_line(aes(x = id, y = strategy.income), col = "gold", alpha = .8) +
        geom_line(aes(x = id, y = stock.benchmarkwin), col = "red", alpha = .8) +
        scale_x_continuous(breaks = temp.df1.breaks, labels = temp.df1$stock.date[temp.df1.breaks]) +
        ggtitle("strategy-return(yellow) vs benchmarket-return(red)")
      pic2 <- ggplot(temp.df1) + # b.回撤
        geom_hline(yintercept = cash, col = "orange", size = .5) +
        theme1 +
        geom_line(aes(x = id, y = strategy.income), col = "gold") +
        geom_point(
          data = temp.df1[x@bt.features$max.drawdown.loc, ],
          aes(x = id, y = strategy.income), col = "lemonchiffon", size = 2
        ) +
        geom_line(
          data = temp.df1[x@bt.features$max.drawdown.loc, ],
          aes(x = id, y = strategy.income), col = "greenyellow", size = 1
        ) +
        scale_x_continuous(breaks = temp.df1.breaks, labels = temp.df1$stock.date[temp.df1.breaks]) +
        ggtitle(paste(
          "Strategy return : max drawdown: ",
          round(x@bt.features$max.drawdown * 100, 2), "%",
          sep = ""
          ))
      pic4 <- ggplot(temp.df1) + # c.超额收益
        geom_hline(yintercept = 0, col = "orange", size = .5) +
        geom_line(aes(x = id, y = strategy.income - stock.benchmarkwin), col = "magenta") +
        theme1 +
        scale_x_continuous(breaks = temp.df1.breaks, labels = temp.df1$stock.date[temp.df1.breaks]) +
        ggtitle("Excess Return")
    }

    pic3 <- ggplot(temp.df3) + # d.指数表
      geom_hline(yintercept = 0, col = "white") +
      geom_vline(xintercept = nrow(temp.df3) - 7.5, col = "white") +
      geom_vline(xintercept = nrow(temp.df3) - 12.5, col = "white") +
      theme3 +
      geom_vline(xintercept = nrow(temp.df3) - 14.5, col = "white") +
      theme3 +
      geom_bar(stat = "identity", aes(x = id, y = value, fill = factor(col)), width = .25) +
      geom_point(aes(x = id, y = value, col = factor(col)), size = 3) +
      scale_fill_manual(values = c("1" = "red", "2" = "gold", "3" = "royalblue3")) +
      scale_x_continuous(
        position = "bottom", expand = c(0.025, 0), breaks = nrow(temp.df3):1,
        labels = temp.df3$feature,
        sec.axis = sec_axis(~., name = "", breaks = nrow(temp.df3):1, labels = temp.df3$value)
      ) +
      ggtitle(paste("Stock Code:", trade.info$wind_code[1])) +
      coord_flip()

    if (!is.simple) {
      if (length(strategy.type) == 1) { # e.交易分布
        pic5 <- ggplot(temp.df4.long) +
          geom_bar(aes(x = date, y = value, fill = variable),
            width = .8, stat = "identity",
            position = "dodge"
          ) +
          geom_hline(yintercept = 0, col = "orange", size = .5) +
          theme2 +
          scale_fill_manual(values = c("sell" = "royalblue3", "buy" = "red"))
        if (strategy.type == "buy") pic5 <- pic5 + ggtitle("Trade distribution of long position")
        if (strategy.type == "sell") pic5 <- pic5 + ggtitle("Trade distribution of short position")
      } else {
        pic5 <- ggplot(temp.df4.long) +
          facet_wrap(~class, nrow = 2, strip.position = "left") +
          geom_bar(aes(x = date, y = value, fill = variable),
            width = .8, stat = "identity",
            position = "dodge"
          ) +
          geom_hline(yintercept = 0, col = "orange", size = .5) +
          theme2 +
          scale_fill_manual(
            values = c(
              "sell.b" = "royalblue3", "buy.b" = "red",
              "sell.s" = "royalblue1", "buy.s" = "orangered"
            ))
      }

      if (length(strategy.type) == 1) { # f.交易次数
        pic6 <- ggplot(temp.df5.short) +
          geom_bar(aes(x = reorder(variable, V1), y = V1, fill = reorder(variable, V1)),
            stat = "identity", width = .8
          ) +
          geom_text(aes(x = reorder(variable, V1), y = max(V1) / 2, label = V1),
            col = "orange", size = 8
          ) + theme2 +
          scale_fill_manual(name = NULL, values = c("buy" = "red", "hold" = "mediumblue", "sell" = "royalblue3"))
        if (strategy.type == "buy") pic6 <- pic6 + ggtitle("Trade num of long position")
        if (strategy.type == "sell") pic6 <- pic6 + ggtitle("Trade num of short position")
      } else {
        pic6 <- ggplot(temp.df5.short) +
          facet_wrap(~class, ncol = 1, strip.position = "left") +
          geom_bar(aes(x = reorder(state, V1), y = V1, fill = reorder(state, V1)),
            stat = "identity", width = .8
          ) +
          geom_text(aes(x = reorder(state, V1), y = max(V1) / 2, label = V1),
            col = "orange", size = 5
          ) +
          theme2 +
          scale_fill_manual(
            name = NULL,
            values = c("buy" = "red", "hold" = "mediumblue", "sell" = "royalblue3")
            ) +
          ggtitle("Number of Trades")
      }

      pic7 <- ggplot(temp.df6) + # g.收益率分布
        facet_wrap(~class, strip.position = "left", ncol = 1, scale = "free_y") +
        geom_hline(yintercept = 0, col = "orange3", size = .5) +
        geom_segment(
          aes(x = stock.time, xend = stock.time, yend = 0, y = trade.value.rate, col = class),
          alpha = .5
          ) +
        geom_point(aes(x = stock.time, y = trade.value.rate, col = class), size = .5) +
        geom_line(aes(x = stock.time, y = rate), col = "orange", linetype = "dashed") +
        geom_line(aes(x = stock.time, y = -rate), col = "orange", linetype = "dashed") +
        geom_text(
          data = temp.df6.rate,
          aes(x = time, y = y, label = value), size = 4, col = "orange"
          ) +
        theme2 +
        scale_color_manual(values = c(
          "a.both" = "greenyellow", "b.long" = "orangered", "c.short" = "royalblue1"
          ))
    }

    pic0 <- ggplot() + theme1

    if (is.simple) {
      if (!segments) {
        lay <- rbind(
          c(1, 1, 2, 2, 2, 2),
          c(1, 1, 2, 2, 2, 2),
          c(1, 1, 3, 3, 4, 4)
          )
        gridExtra::grid.arrange(pic3, pic1, pic4, pic2, ncol = 2, layout_matrix = lay)
      } else {
        pic <- list("pic1" = pic1, "pic2" = pic2, "pic3" = pic3, "pic4" = pic4)
        return(pic)
      }
    } else {
      if (!segments) {
        lay <- rbind(
          c(1, 1, 2, 2, 2, 2),
          c(1, 1, 2, 2, 2, 2),
          c(1, 1, 3, 3, 3, 3),
          c(5, 5, 5, 6, 4, 4),
          c(5, 5, 5, 7, 7, 7)
          ) # h.组合绘图
        # pic <- gridExtra::arrangeGrob(pic3, pic1, pic4, pic2, pic7, pic6, pic5, ncol = 2,layout_matrix = lay)
        gridExtra::grid.arrange(
          pic3, pic1, pic4, pic2, pic7, pic6, pic5, ncol = 2,
          layout_matrix = lay
          )
      } else {
        pic <- list(
          "pic1" = pic1, "pic2" = pic2, "pic3" = pic3, "pic4" = pic4,
          "pic5" = pic5, "pic6" = pic6, "pic7" = pic7
          )
        return(pic)
      }
    }
    # pic <- grid.arrange(pic3, pic1, pic4, pic2, pic7, pic6, pic5, ncol = 2,layout_matrix = lay)
    # pic <- arrangeGrob(pic3, pic1, pic2, pic4, pic5, pic6, ncol = 2,layout_matrix = lay)
    # print(pic)
  }
)
