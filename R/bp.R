#加载包
InsPkg(c('ROCR', 'pROC', 'RSNNS', 'gdata', 'kernlab', 'nnet', 'lubridate'))


#~~~~~~~~~~~~~~~~~~~~~~~~~##-----------------------------------------------------####
# （一）模型&策略部分 ####
# 1 数据处理data_pre_func####
data_pre_func <- function(data){

  data[,5:12] <- apply(data[,5:12],2,as.numeric)
  data$close <- data$close/10000
  data$high <- data$high/10000
  data$open <- data$open/10000
  data$low <- data$low/10000

  tt2 <- ifelse(nchar(data$time)>8,data$time,paste0('0',data$time))
  ee <- as.Date(paste0(substr(data$date,1,4),'-',substr(data$date,5,6),'-',substr(data$date,7,8)))
  data$time <- paste(ee,paste0(substr(tt2,1,2),':',substr(tt2,3,4),':',substr(tt2,5,6)))
  data$date <- as.Date(substr(data$time,1,10))

  if( as.numeric(difftime(data$time[2],data$time[1],units = 'min')) == 1){
    aa <- ddply(data,'date',function(x){
      cc <- nrow(x)
    })
    date_val <- (aa[which(aa$V1>=238),])$date ####select data,选取一天中不小于238个点的日期
    data <- data[which(data$date %in% date_val),]
    data <- data[which(abs(data$close-mean(data$close))<5*sd(data$close)),]
  }
  return(data)
}


# 2.1 定义神经网络数据格式set_zd_func(minute data)####
set_zd_func <- function(cycle,SZDATA){
  set_data2 <- data.frame(onerate=NA,fiverate=NA,tenrate=NA,ma_c5=NA,ma_c10=NA,
                          Rc_1=NA,Rc_5=NA,increase=NA,Rc_10=NA,
                          volum=NA,turnover=NA,ismin=NA,ismax=NA,result=NA,stock=NA,stocktime=NA)

  uu <- unique(SZDATA$date)
  for(k in 1:length(unique(uu))){
    print(k)
    SZDATA_new <- subset(SZDATA,date==uu[k])
    len <- nrow(SZDATA_new)
    set_data <- data.frame(onerate=rep(NA,len),fiverate=rep(NA,len),tenrate=rep(NA,len),ma_c5=rep(NA,len),ma_c10=rep(NA,len),
                           Rc_1=rep(NA,len),Rc_5=rep(NA,len),increase=rep(NA,len),Rc_10=rep(NA,len),
                           volum=rep(NA,len),turnover=rep(NA,len),ismin=rep(NA,len),ismax=rep(NA,len),result=rep(NA,len),stock=rep(NA,len),stocktime=rep(NA,len))
    for(i in 1:(len-cycle)){

      section_data <- SZDATA_new[(i:(i+cycle-1)),]

      if((SZDATA_new$close[i+cycle]-SZDATA_new$close[i+cycle-1])/SZDATA_new$close[i+cycle-1] < (-0.002)){
        set_data$result[i] <- c('0')   ##跌
      }else if((SZDATA_new$close[i+cycle]-SZDATA_new$close[i+cycle-1])/SZDATA_new$close[i+cycle-1] > 0.002){
        set_data$result[i] <- c('1')   ##涨
      }

      set_data$stock[i] <- SZDATA_new$close[i+cycle-1]

      set_data$stocktime[i] <-  SZDATA_new$time[i+cycle-1]
      #####对数据集构造特征向量
      set_data$onerate[i] <- (section_data$close[cycle]-section_data$close[cycle-1])/section_data$close[cycle-1] ##当前点相对于上一个点的振幅
      set_data$fiverate[i] <- mean((c(0,diff(section_data$close))/section_data$close)[(cycle-4):cycle])  ###最后5个点的平均振幅
      set_data$tenrate[i] <- mean((c(0,diff(section_data$close))/section_data$close)[(cycle-9):cycle])  ###最后10个点的平均振幅
      set_data$ma_c5[i] <- mean(section_data$close[(cycle-4):cycle])/mean(section_data$close)###前5个点平均收盘价与前30个点均值比
      set_data$ma_c10[i] <- mean(section_data$close[(cycle-9):cycle])/mean(section_data$close)###前10个点平均收盘价与前30个点均值比
      set_data$Rc_1[i] <- round(Rate(section_data$close,1),4)###收盘价在过去1天里变化率
      set_data$Rc_5[i] <- round(Rate(section_data$close,5),4)
      set_data$Rc_10[i] <- round(Rate(section_data$close,10),4)
      set_data$increase[i] <- sum(diff(section_data$close)>0)/cycle ###涨的天数占比
      set_data$volum[i] <- section_data$volumw[cycle]/mean(section_data$volumw[(cycle-4):cycle]) ###当前的成交量与过去5个点成交量均值占比
      set_data$turnover[i] <- section_data$turover[cycle]/mean(section_data$turover[(cycle-4):cycle]) ###当前的转手率与过去5个点转手率均值占比

      if( min(section_data$close) %in% section_data$close[(cycle-4):cycle] ){
        set_data$ismin[i] <- 1
      }else{
        set_data$ismin[i] <- 0
      }
      if(max(section_data$close) %in% section_data$close[(cycle-4):cycle] ){
        set_data$ismax[i] <- 1
      }else{
        set_data$ismax[i] <- 0
      }
    }
    set_data2 <- rbind(set_data2,set_data)
  }
  set_data2 <- set_data2[-which(is.na(set_data2$onerate)),]
  set_data2$result <- as.factor(set_data2$result)
  return(set_data2)
}


# 2.2 定义神经网络数据格式set_zd_func2(hour/day data)####
set_zd_func2 <- function(cycle,SZDATA){
  len <- nrow(SZDATA)
  ll <- rep(NA,len-cycle)
  set_data <- data.frame(onerate=ll,fiverate=ll,tenrate=ll,ma_c5=ll,ma_c10=ll,
                         Rc_1=ll,Rc_5=ll,increase=ll,Rc_10=ll,
                         volum=ll,turnover=ll,ismin=ll,ismax=ll,result=ll,stock=ll,stocktime=ll)

  for(i in 1:(len-cycle)){

    section_data <- SZDATA[(i:(i+cycle-1)),]

    if((SZDATA$close[i+cycle]-SZDATA$close[i+cycle-1])/SZDATA$close[i+cycle-1] < (-0.002)){
      set_data$result[i] <- c('0')   ##跌
    }else if((SZDATA$close[i+cycle]-SZDATA$close[i+cycle-1])/SZDATA$close[i+cycle-1] > 0.002){
      set_data$result[i] <- c('1')   ##涨
    }

    set_data$stock[i] <- SZDATA$close[i+cycle-1]
    set_data$stocktime[i] <-  SZDATA$time[i+cycle-1]

    #####对数据集构造特征向量
    set_data$onerate[i] <- (section_data$close[cycle]-section_data$close[cycle-1])/section_data$close[cycle-1] ##当前点相对于上一个点的振幅
    set_data$fiverate[i] <- mean((c(0,diff(section_data$close))/section_data$close)[(cycle-4):cycle])  ###最后5个点的平均振幅
    set_data$tenrate[i] <- mean((c(0,diff(section_data$close))/section_data$close)[(cycle-9):cycle])  ###最后10个点的平均振幅
    set_data$ma_c5[i] <- mean(section_data$close[(cycle-4):cycle])/mean(section_data$close)###前5个点平均收盘价与前30个点均值比
    set_data$ma_c10[i] <- mean(section_data$close[(cycle-9):cycle])/mean(section_data$close)###前10个点平均收盘价与前30个点均值比
    set_data$Rc_1[i] <- round(Rate(section_data$close,1),4)###收盘价在过去1天里变化率
    set_data$Rc_5[i] <- round(Rate(section_data$close,5),4)
    set_data$Rc_10[i] <- round(Rate(section_data$close,10),4)
    set_data$increase[i] <- sum(diff(section_data$close)>0)/cycle ###涨的天数占比
    set_data$volum[i] <- section_data$volumw[cycle]/mean(section_data$volumw[(cycle-4):cycle]) ###当前的成交量与过去5个点成交量均值占比
    set_data$turnover[i] <- section_data$turover[cycle]/mean(section_data$turover[(cycle-4):cycle]) ###当前的转手率与过去5个点转手率均值占比

    if( min(section_data$close) %in% section_data$close[(cycle-4):cycle] ){
      set_data$ismin[i] <- 1
    }else{
      set_data$ismin[i] <- 0
    }
    if(max(section_data$close) %in% section_data$close[(cycle-4):cycle] ){
      set_data$ismax[i] <- 1
    }else{
      set_data$ismax[i] <- 0
    }
  }

  set_data$result <- as.factor(set_data$result)
  return(set_data)
}


# 3 定义d个交易日变化率函数Rate####
Rate <- function(data,d){
  nn <- length(data)
  log(data[nn]/data[nn-d])
}

# 4 数据输出(最终策略结果)output_func####
output_func <- function(pre_data,stoploss='限价止损', SZDATA.train){

  pre_data$wind_code <- SZDATA.train$wind_code[1]
  pre_data$stock.name <- SZDATA.train$name[1]
  pre_data$stock.date <- as.Date(substr(pre_data$stock.time,1,10))


  pre_data$trade <- 0
  pre_data$trade <- ifelse(pre_data$pre=='0',-1,1)
  # pre_data[(which(pre_data$pre=='0')),]$trade <- c(-1) ##下一时刻涨，买进策略（strategy为1）；下一时刻跌，卖出策略（strategy为-1）
  # pre_data[(which(pre_data$pre=='1')),]$trade <- 1

  pre_data$strategy <- 0
  if(sum(pre_data$trade==1)>0){
    pre_data$strategy[which(pre_data$trade==1)[1]] <- 1

  }

  for( i in (which(pre_data$trade==1)[1]+1):nrow(pre_data)){
    before <- pre_data$strategy[1:(i-1)]
    before_last <- last(which(before != 0)) #前面所有点最后一个交易信号处
    before_trade <- pre_data$trade[before_last] #前面最后一个交易信号
    stock_before <- pre_data$stock.price[before_last] #前面最后一个交易处的股价
    pre_1 <- pre_data[before_last:i,] #前面一个信号至当前时间点的数据集
    ##指标定义
    #1、开仓后的最高价：
    Khigh <- max(pre_1$stock.price)
    #2、开仓后最低价：
    Klow <- min(pre_1$stock.price)

    if(pre_data$trade[i] == 0){ #若当前策略是持有，则最终strategy也是持有
      pre_data$strategy[i] <- 0
    }else if(pre_data$trade[i] == 1){

      if(stoploss=='限价止损'){
        pre_data$strategy[i] <- ifelse(before_trade ==(-1) & ((pre_data$stock.price[i]-stock_before)/stock_before < (-0.003) | (pre_data$stock.price[i]-stock_before)/stock_before > 0.05),1,0)
      }else if (stoploss=='跟踪止损'){
        pre_data$strategy[i] <- ifelse(before_trade ==(-1) & (stock_before - Klow > 50*0.01) & (stock_before > Klow + 0.3*(pre_1$stock.price[1] - Klow)),1,0)
      }else if(stoploss=='阶梯止损'){
        pre_data$strategy[i] <- ifelse(before_trade ==(-1) & (pre_data$stock.price[i] > stock_before-0.3+round((stock_before-Klow)/10)*5),1,0)
      }

    }else if(pre_data$trade[i] == (-1)){
      if(stoploss=='限价止损'){
        pre_data$strategy[i] <- ifelse(before_trade ==1 & ((pre_data$stock.price[i]-stock_before)/pre_data$stock.price[i]> 0.003 | (pre_data$stock.price[i]-stock_before)/stock_before<(-0.05)),-1,0)
      }else if(stoploss=='跟踪止损'){
        pre_data$strategy[i] <- ifelse(before_trade ==1 & (Khigh - pre_1$stock.price[1] > 50*0.01) &  (pre_data$stock.price[i] < (Khigh - 0.3*(Khigh - pre_1$stock.price[1]))), -1,0)
      }else if(stoploss=='阶梯止损'){
        pre_data$strategy[i] <- ifelse(before_trade ==1 & (pre_data$stock.price[i]< (stock_before-0.3+round((Khigh-stock_before)/10)*5)),-1,0)
      }
    }
  }

  if(sum(pre_data$strategy==1)==1){
    pre_data$strategy[nrow(pre_data)] <- -1
  }

  pre_data$strategy.b <- pre_data$strategy
  pre_data$strategy.s <- 0
  if(sum(pre_data$strategy == -1)>1){
    pre_data$strategy.s[(which(pre_data$strategy == -1)[1]):nrow(pre_data)] <- pre_data$strategy[(which(pre_data$strategy == -1)[1]):nrow(pre_data)]

  }
  return(pre_data)
}


# 5 完整实现过程函数wholeprocess_func####
wholeprocess_func <- function(SZDATA.train,SZDATA.test,cycle=30,stoploss, is.train = F, hidsize = 13, learnrate = .1, maxiter = 500){
  # 训练nnet
  if( as.numeric(difftime(SZDATA.train$time[2],SZDATA.train$time[1],units = 'min'))==1){
    set_train <- set_zd_func(cycle,SZDATA.train)    #训练集，神经网路数据准备
    set_test <- set_zd_func(cycle,SZDATA.test)    #测试集，数据准备
  }else{
    set_train <- set_zd_func2(cycle,SZDATA.train)
    set_test <- set_zd_func2(cycle,SZDATA.test)
  }
  set_train_scale <- cbind(scale(set_train[,1:11]),set_train[,12:16]) ##数据标准化
  set_train_scale <- set_train_scale[-which(is.na(set_train_scale$result)),]
  set_test_scale <- cbind(scale(set_test[,1:11]),set_test[,12:16])
  # size:隐层节点；rang:学习率; decay: ; maxit: 最大迭代次数；
  set_nnet <- nnet(result ~ ., data = set_train_scale[,1:14], size = hidsize, rang = learnrate,decay = 5, maxit = maxiter)  ##建模

  predict_data <- data.frame(pre=predict(set_nnet, set_test_scale[,1:13],type='class'),
                             result=set_test_scale$result,stock.price=set_test_scale$stock,stock.time=set_test_scale$stocktime)  ##预测涨跌，并增加字段：股价和时间
  if(is.train) return(list('model' = set_nnet, 'data' = predict_data))
  # 交易信号
  predict_data <- output_func(predict_data,stoploss,SZDATA.train)    # 在涨跌基础上，增加止损策略，形成最终相关操作数据集

  return(predict_data)
}

# 6 模型评估函数 ####
MixMat <- function(tvalue, pval, beta = .5){
  pval <- as.numeric(as.character(pval))[!is.na(tvalue)]

  tvalue <- as.numeric(as.character(tvalue))[!is.na(tvalue)]

  classes <- unique(tvalue)

  # 预测为第一类
  pre.loc1 <- which(pval == classes[1])
  pre.1.tval.1 <- sum(tvalue[pre.loc1] == classes[1])
  pre.1.tval.2 <- length(pre.loc1) - pre.1.tval.1

  # 预测为第二类
  pre.loc2 <- which(pval == classes[2])
  pre.2.tval.2 <- sum(tvalue[pre.loc2] == classes[2])
  pre.2.tval.1 <- length(pre.loc2) - pre.2.tval.2

  themat <- data.frame(c(pre.1.tval.1,pre.2.tval.1),c(pre.1.tval.2, pre.2.tval.2))
  names(themat) <- paste("real.", as.character(classes), sep = "")
  row.names(themat) <- paste("pre.", as.character(classes), sep = "")

  pre.total <- sum(tvalue == pval)/length(tvalue)
  pre <- themat[1,1]/sum(themat[1,])
  rec <- themat[1,1]/(themat[1,1] + themat[2,1])
  accuracy.F <- ((beta^2+1)*pre*rec)/(beta^2*(pre+rec))
  acc <- round(c("pre.total"=pre.total,"pre" = pre,"rec" = rec,"F" = accuracy.F), 3)

  return(list('mixmat' = themat, 'acc' = acc))
}

# 测试代码

#~~~~~~~~~~~~~~~~~~~~~~~~~##-----------------------------------------------------####
# （二）server数据交互 ####

#~~~~~~~~~~~~~~~~~~~~~~~~~##-----------------------------------------------------####
# 模型测试 ####
SZDATA <- read.csv('F:data/1day/sh/600000.csv', header = T, stringsAsFactors = F,skip = 1)
SZDATA <- sh600000
#
#
clean_data <- data_pre_func(SZDATA)  ##清洗数据
SZDATA.train <- subset(clean_data,date > '2012-01-04' & date <'2016-01-01')  #划分训练集
SZDATA.test <- subset(clean_data,date > '2016-01-04' & date < '2017-03-01')   #划分测试集
#
stradeinfo.bp <- wholeprocess_func(SZDATA.train,SZDATA.test,cycle=30,stoploss = '限价止损', is.train = F) #建模并建立买卖策略
# stradeinfo.bp$data
# head(stradeinfo.bp$data)
# pre result stock.price          stock.time
# 1   0      1     15.6321 2016-03-17 15:15:00
# 2   0      1     15.7558 2016-03-18 15:15:00
# 3   0      1     15.9501 2016-03-21 15:15:00
# 4   0   <NA>     16.0296 2016-03-22 15:15:00
# 5   0      0     16.0119 2016-03-23 15:15:00
# 6   1   <NA>     15.6498 2016-03-24 15:15:00

# testroc <- na.omit(stradeinfo.bp$data)
# testroc <- transform(testroc,pre = as.numeric(as.character(pre)), result = as.numeric(as.character(result)))
# theroc <- roc(result~pre, testroc, percent = T, plot = T, ci = T)
# plot(theroc)

# MixMat(testroc$result, testroc$pre)


#devtools::source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
# plot(stradeinfo.bp$model)


# if(all(c("strategy.s", "strategy.b") %in%  names(stradeinfo.bp))){
#   if(all(stradeinfo.bp$strategy.s == 0)){
#     stradeinfo.bp <- stradeinfo.bp[,-which(names(stradeinfo.bp) == "strategy.s")]
#   }
#   if(all(stradeinfo.bp$strategy.b == 0)){
#     stradeinfo.bp <- stradeinfo.bp[,-which(names(stradeinfo.bp) == "strategy.b")]
#   }
# }
temp <- BackTestGen(stradeinfo.bp,cash=100000, is.pic = T, is.pic.whole = T,is.tax=T, tax.rate = 0.002)
# plot(temp$pic$pic3)
# temp.backtest <- BackTestGen(stradeinfo.bp,cash=100000, is.pic = T, is.tax=T, is.simple = T,tax.rate = 0.002)
# save(stradeinfo_bp, file = 'data/stradeinfo_bp.RData')


