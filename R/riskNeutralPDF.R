riskNeutralPDF <- function(st, strsigma, isDebug="false", maturity="30") {
  sigmad <- eval(D( parse(text=strsigma), "x"), list(x=st))
  sigmadd <- eval(D( D(parse(text=strsigma), "x"), "x"), list(x=st))
  fsigma <- function(t)eval( parse(text=strsigma), list(x=t))
  sigma <- fsigma(st)

  r <- 0.004
  T <- as.numeric(maturity) / 365
  #print(paste("T=", T))
  g <- 0 + r*T

  d1 <- (g + (T*sigma^2) / 2) / (sigma * sqrt(T))
  d2 <- (g - (T*sigma^2) / 2) / (sigma * sqrt(T))

  d1d <- (-1*sigmad * g) / ((sigma^2) * sqrt(T)) + (sigmad * sqrt(T)/2) - (1 / (st * sigma * sqrt(T)))
  d2d <- (-1*sigmad * g) / ((sigma^2) * sqrt(T)) - (sigmad * sqrt(T)/2) - (1 / (st * sigma * sqrt(T)))
  
  d1dd <- (2*sigmad + st * sigma) / ((sigma^2) * st * sqrt(T)) - g * (sigmadd * sigma - 2*sigmad^2) / ((sigma^3) * sqrt(T)) + (sigmadd * sqrt(T) / 2)
  d2dd <- (2*sigmad + st * sigma) / ((sigma^2) * st * sqrt(T)) - g * (sigmadd * sigma - 2*sigmad^2) / ((sigma^3) * sqrt(T)) - (sigmadd * sqrt(T) / 2)


  #first <- st * exp(-1 * d1^2 /2 ) * (-1 * d1 * d1d^2 + d1dd) / sqrt(2*pi)
  #second <- exp(-1*r*T) * exp(-1*d2^2/2) * (2*d2d - st * d2 * d2d^2 + st * d2dd) / sqrt(2*pi)
  
  first <- exp(r*T) * st * exp(-1 * (d1^2) /2 ) * (-1 * d1 * d1d^2 + d1dd) / sqrt(2*pi)
  second <- exp(-1 * (d2^2)/2) * (2*d2d - st * d2 * d2d^2 + st * d2dd) / sqrt(2*pi)
  
  cdd <- first - second 
  result <- cdd
  if(result < 0){
    #result <- 0 * result
  }
  
  #breeden <- cdd * exp(r*T)
  #result <- breeden

  if(isDebug != "false"){

  print(paste("sigma=", sigma))
  print(paste("sigmad=", sigmad))
  print(paste("sigmadd=", sigmadd))
  print(paste("d1=", d1))
  print(paste("d2=", d2))
  print(paste("d1d=", d1d))
  print(paste("d2d=", d2d))
  print(paste("d1dd=", d1dd))
  print(paste("d2dd=", d2dd))
  print(paste("first=", first))
  print(paste("second=", second))
  
  }




  return (result)
}
calcSigmak <- function(x, maturity) {
  func <- function(t)eval( parse(text=sigmak(x, maturity)), list(x=t))
  result <- func(x)
  return(result)
}
constSigmak <- function(k){
  str <- "x * 0 + 0.3"
  return (str)
}


sigmak <- function(k, maturity="30") {
  data <- read.csv(paste("./spline/spline_dataM", maturity, "_0214.csv", sep=""))
  range <- as.vector(as.matrix(data[1]))
  datay <- as.vector(as.matrix(data[2]))
  dataq <- as.vector(as.matrix(data[3]))
  datar <- as.vector(as.matrix(data[4]))
  datas <- as.vector(as.matrix(data[5]))
  length <- length(range) 

  if(k < range[1]){
      k <- range[1]
      str <- paste(datay[1], "+", dataq[1], "*", "(x -", k, ") +", datar[1], "*", "(x -", k, ")^2 +", datas[1], "*", "(x -", k, ")^3")
      return(str)
  }
  
  if(range[length] <= k){
      #print(range[length])
      #print(k)
      k <- range[length]
      str <- paste(datay[length-1], "+", dataq[length-1], "*", "(x -", k, ") +", datar[length-1], "*", "(x -", k, ")^2 +", datas[length-1], "*", "(x -", k, ")^3")
      return(str)
  }
  for(i in 1:length-1){
    if( (range[i] <= k ) && (k < range[i+1]) ){
      k <- range[i]
      str <- paste(datay[i], "+", dataq[i], "*", "(x -", k, ") +", datar[i], "*", "(x -", k, ")^2 +", datas[i], "*", "(x -", k, ")^3")
      return(str)
      #result <- function(x) datay[i] + dataq[i] * (x - k) + datar[i] * (x - k)^2 + datas[i] * (x - k)^2
      #print(result(20000))
      #return ( body(datay[i] + dataq[i] * (x - k) + datar[i] * (x - k)^2 + datas[i] * (x - k)^2 ) )
    }
  }
}



calcRiskNeutral <- function(x, isDebug="false", maturity="30")riskNeutralPDF(x, strsigma=sigmak(x, maturity), isDebug, maturity)

makeRiskVec <- function(startnum, endnum, maturity="30") {
  y <- numeric(endnum)
  last <- endnum - startnum
  for(i in 1:last){
    y[startnum+i] <- calcRiskNeutral(startnum + i, "false", maturity)
  }
  len <- length(y)
  append(y, numeric(20000-len))
  return(y)
  #plot(x, y, xlim=c(startnum, endnum))

  #print(result)
  #write(result, file="neutralResult.txt")
}


self_integrate <- function(vec, endnum) {
  startnum <- 6500
  last <- endnum - startnum
  result <- 0
  for(i in 1:last){
    result <- result + vec[startnum+i]
  }
  result <- result
  return(result)
}

kernelDensity <- function(x, isFunc="false") {
  data <- read.csv("nikkei225_from2003to2010.csv")
  data <- as.matrix(data)
  data <- as.vector(data)
  #result <- approxfun(density(data, bw="nrd"))
  result <- approxfun(density(data))
  if(isFunc){
    return( result )
  }else{
    return( result(x) )
  }
}

makeRiskVecAuto <- function(maturity="30") {
  vec <- makeRiskVec(6500, 18300, maturity)
  #vec <- makeRiskVec(6500, 16380)
  return(vec)
}
calc_qd <- function(x){
  #check <- try(class(global.qd[x]), silent=TRUE)
  check <- global.qd[x]
  if(is.na(check)){
    print(paste("calc_qd", x))
    global.qd[x] <<- kernelDensity(x)
    q <- integrate(kernelDensity(1, "true"), 6500, x, subdivisions=10000)
    global.q[x] <<- as.numeric(q[1])
  }
}

calcRiskAversion <- function(x, func, isDebug="false", maturity="30") {
  startnum <- 6500
  nouse <- calc_qd(x)

  qd <- global.qd[x]
  q <- global.q[x]
  #qd <- kernelDensity(x)
  #q <- integrate(kernelDensity(1, "true"), startnum, x, subdivisions=10000)
  #q <- as.numeric(q[1])
  pd <- calcRiskNeutral(x, isDebug, maturity)
  #print(pd)
  #func <- approxfun(vec)
  #print(paste("x=", x))
  try_error <- try( p <- integrate(func, startnum, x, subdivisions=2000), silent=TRUE)
  if(class(try_error)=="try-error"){
    print(try_error)
    print(paste("maturity=", maturity, " x=", x))
    return(0)
  }
  #print(paste("p=", p))
  p <- as.numeric(p[1])
  #p <- self_integrate(vec, x)
  if(isDebug != "false"){
    print(paste("qd=", qd))
    print(paste("q=", q))
    print(paste("pd=", pd))
    print(paste("p=", p))
  }
  qd_q <- qd / q
  pd_p <- pd / p
  result <- qd_q - pd_p

  #relative
  result <- x * result
  if(result > 1000 || result < -1000){
    result <- 0
  }
  return(result)
}

calcRiskTable <- function(maturity="30"){
  #check <- try(class(global.vec), silent=TRUE)
  #if(class(check) == "try-error"){
    #print("makeGlobalVec")
    #global.vec <<- makeRiskVecAuto(maturity)
    #global.func <<- approxfun(global.vec)
  #}
  vec <- makeRiskVecAuto(maturity)
  func <- approxfun(vec)
  #print(vec)
  #func <- global.func
  #data <- read.csv("nikkei225_return_from2003to2010.csv")
  data <- read.csv("nikkei_round_timeorder.csv")
  data <- as.matrix(data)
  data <- as.vector(data)
  len <- length(data)
  result <- numeric(len)
  for(i in 1:len){
    result[i] <- calcRiskAversion(data[i], func, "false", maturity)
  }
  return(result)
}

calcRiskTableAll <- function(isLast="false"){
  result <- list(1:1)
  if(isLast =="false"){
    for(i in 1:6){
      result[[i]] <- calcRiskTable(i*10)
      print(paste("result", i))
    }
  }else if(isLast=="true"){
    for(i in 7:12){
      result[[i]] <- calcRiskTable(i*10)
      print(paste("result", i))
    }
  }else if(isLast=="all"){
    for(i in 1:11){
      result[[i]] <- calcRiskTable(i*10)
      print(paste("result", i))
    }
  }
  return(result)
}


checkSummary <- function(vec){
  result <- list(1:1)
  result[[1]] <- calcSummary(vec[44:145])
  result[[2]] <- calcSummary(vec[146:206])
  result[[3]] <- calcSummary(vec[207:267])
  result[[4]] <- calcSummary(vec[268:330])
  result[[5]] <- calcSummary(vec[331:391])
  result[[6]] <- calcSummary(vec[392:451])
  result[[7]] <- calcSummary(vec[452:490])
  result[[8]] <- calcSummary(vec[491:555])
  result[[9]] <- calcSummary(vec[556:636])
  result[[10]] <- calcSummary(vec[637:697])
  result[[11]] <- calcSummary(vec[698:759])
  result[[12]] <- calcSummary(vec[760:822])
  result[[13]] <- calcSummary(vec[823:884])
  result[[14]] <- calcSummary(vec[885:943])
  result[[15]] <- calcSummary(vec[944:1005])
  result[[16]] <- calcSummary(vec[1006:1067])
  result[[17]] <- calcSummary(vec[1068:1129])
  result[[18]] <- calcSummary(vec[1130:1188])
  result[[19]] <- calcSummary(vec[1189:1250])
  result[[20]] <- calcSummary(vec[1251:1313])
  result[[21]] <- calcSummary(vec[1314:1374])
  result[[22]] <- calcSummary(vec[1375:1433])
  result[[23]] <- calcSummary(vec[1434:1494])
  result[[24]] <- calcSummary(vec[1495:1556])
  result[[25]] <- calcSummary(vec[1557:1617])
  result[[26]] <- calcSummary(vec[1618:1677])
  result[[27]] <- calcSummary(vec[1678:1738])
  result[[28]] <- calcSummary(vec[1739:1801])
  return(result)
}

calcSummary <- function(vec){
  tmp <- c(0)
  tmp[1] <- mean(vec)
  tmp[2] <- var(vec)
  tmp[3] <- sd(vec)
  return(tmp)
}

calcSummaryByMaturity <- function(){
  result <- list(1:1)
  result[[1]] <- calcSummary(outlier2_result[[1]])
  result[[2]] <- calcSummary(outlier2_result[[2]])
  result[[3]] <- calcSummary(outlier2_result[[3]])
  result[[4]] <- calcSummary(outlier2_result[[4]])
  result[[5]] <- calcSummary(outlier2_result[[5]])
  result[[6]] <- calcSummary(outlier2_result[[6]])
  result[[7]] <- calcSummary(outlier2_result[[7]])
  result[[8]] <- calcSummary(outlier2_result[[8]])
  result[[9]] <- calcSummary(outlier2_result[[9]])
  result[[10]] <- calcSummary(outlier2_result[[10]])
  result[[11]] <- calcSummary(outlier2_result[[11]])
  return(result)
}


plot_1 <- function(isUnemploy="false"){
  plot(sum_vec, type="l", col="deepskyblue", main="Stock Price And Risk Aversion", xlab="Time", ylab="Relative Risk Aversion")
  par(new=T)
  plot(underlyingAsset, type="l", col="deeppink", axes=F, ann=F)
  axis(4)
  if(isUnemploy != "false"){
    par(new=T)
    plot(unemployment, type="l", col="darkslategray", axes=F, ann=F)
  }
}
plot_2 <- function(){
    par(mar=c(5,5,5,7))
    plot(sum_vec, type="l", col="gray69", main="Risk Aversion And SD And Historical Volatility", xlab="Time", ylab="Relative Risk Aversion")
    par(new=T)
    plot(HV, type="o", col="deepskyblue", axes=F, ann=F, lty=3, pch=16)
    axis(4)
    par(new=T)
    plot(sd, type="o", col="deeppink", axes=F, ann=F, lty=5, pch=15)
    axis(2, pos=2100)
}

plot_3 <- function(){
    plot(sd, type="l", col="deeppink", main="SD(RiskAversion) And CI And Unemployment", xlab="Time", ylab="SD")
    par(new=T)
    plot(ci, type="o", col="deepskyblue", axes=F, ann=F,lty=3,  pch=17)
    axis(4)
    par(new=T)
    plot(unemployment, type="o", col="darkslategray", axes=F, ann=F, lty=5, pch=15)
    axis(2, pos=2250)
}

plot_4 <- function(){
  par(oma = c(2, 2, 2, 2))
  par(mfrow=c(2, 2))
  plot(outlier2_result[[1]], type="l", main="Relative Risk Aversion(T=10)", xlab="Time", ylab="Relative Risk Aversion")
  plot(outlier2_result[[2]], type="l", main="Relative Risk Aversion(T=20)", xlab="Time", ylab="Relative Risk Aversion")
  plot(outlier2_result[[3]], type="l", main="Relative Risk Aversion(T=30)", xlab="Time", ylab="Relative Risk Aversion")
  plot(outlier2_result[[4]], type="l", main="Relative Risk Aversion(T=40)", xlab="Time", ylab="Relative Risk Aversion")
}

plot_5 <- function(){
  par(oma = c(2, 2, 2, 2))
  par(mfrow=c(2, 2))
  plot(outlier2_result[[5]], type="l", main="Relative Risk Aversion(T=50)", xlab="Time", ylab="Relative Risk Aversion")
  plot(outlier2_result[[6]], type="l", main="Relative Risk Aversion(T=60)", xlab="Time", ylab="Relative Risk Aversion")
  plot(outlier2_result[[7]], type="l", main="Relative Risk Aversion(T=70)", xlab="Time", ylab="Relative Risk Aversion")
  plot(outlier2_result[[8]], type="l", main="Relative Risk Aversion(T=80)", xlab="Time", ylab="Relative Risk Aversion")
}
plot_6 <- function(){
  par(oma = c(2, 2, 2, 2))
  par(mfrow=c(2, 2))
  plot(outlier2_result[[9]], type="l", main="Relative Risk Aversion(T=90)", xlab="Time", ylab="Relative Risk Aversion")
  plot(outlier2_result[[10]], type="l", main="Relative Risk Aversion(T=100)", xlab="Time", ylab="Relative Risk Aversion")
  plot(outlier2_result[[11]], type="l", main="Relative Risk Aversion(T=110)", xlab="Time", ylab="Relative Risk Aversion")
}
