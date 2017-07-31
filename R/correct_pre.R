#' Calculate correct rate of buy and sell signal, based on stock price
#'
#' @param stock.price a one dimension vector of numeric data, which represents a time series of stock price.
#' @param strategy.b the trade signal of buy operation consists of a one dimension vector contians integer
#'     values of -1,0,1, of which '-1' equals to 'sell','0' equals to 'hold' and '1' equals to 'buy'.
#' @param strategy.s the trade signal of sell operation consists of a one dimension vector contians integer
#'     values of -1,0,1, of which '-1' equals to 'sell','0' equals to 'hold' and '1' equals to 'buy'.
#' @return a vector with two values represent

pre_correct <- function(stock.price = NA, strategy.b = NA, strategy.s = NA){

  ## 作差分判断上涨样本点令为1
  dprice <- diff(stock.price)
  dprice[which(dprice > 0)] <- 1
  dprice[which(dprice < 0)] <- -1
  dprice <- c(0, dprice)
  if(length(na.omit(strategy.b)) != 0){
    ## 上涨形态预测的正确率
    q1 <- which(strategy.b==1)
    q2 <- which(strategy.b==-1)
    if(q2[1]<=q1[1]){
      q2 <- q2[-1]
    }
    if(q1[length(q1)]==length(strategy.b)){
      q1 <- q1[-length(q1)]
    }else if(length(q1)!=length(q2)){
      q2 <- c(q2,length(strategy.b))
    }

    dq <- q2-q1    #统计判断连续涨的个数
    k <- c()
    for(i in 1:length(q1)){
      k[i] <- length(which(dprice[(q1[i]+1):(q1[i]+dq[i])]==1))
    }

    l <- c()
    for(i in 1:length(q2)){
      l[i] <- length(which((dprice[(q2[i]+1)]==0)|(dprice[(q2[i]+1)]==-1)))
    }
    pre.correct.b <- (sum(k)+sum(l))/sum(dq+1)    #频率
    pre.correct.b <- round(pre.correct.b, 3)
  }else{
    pre.correct.b <- 0
  }

  ## 下跌形态预测的正确率
  if(length(na.omit(strategy.s)) != 0){
    q3 <- which(strategy.s==-1)
    q4 <- which(strategy.s==1)
    if(q4[1]<=q3[1]){
      q4 <- q4[-1]
    }
    if(q3[length(q3)]==length(strategy.s)){
      q3 <- q3[-length(q3)]
    }else if(length(q3)!=length(q4)){
      q4 <- c(q4,length(strategy.s))
    }
    # 统计判断连续涨的个数
    ddq <- q4-q3
    kk <- c()
    for(i in 1:length(q3)){
      kk[i] <- length(which(dprice[(q3[i]+1):(q3[i]+ddq[i])]==-1))
    }

    ll <- c()
    for(i in 1:length(q4)){
      ll[i] <- length(which((dprice[(q4[i]+1)]==0)|(dprice[(q4[i]+1)]==1)))
    }
    pre.correct.s <- (sum(kk)+sum(ll))/sum(ddq+1)   # 频率
    pre.correct.s <- round(pre.correct.s, 3)
  }else{
    pre.correct.s <- 0
  }
  pre.correct <- c(pre.correct.b, pre.correct.s)
  return(pre.correct)
}
