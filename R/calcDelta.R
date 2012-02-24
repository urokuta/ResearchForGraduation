calcDelta <- function() {
  data <- read.csv("for_delta.csv")
  K <- as.vector(as.matrix(data[1]))
  Price <- as.vector(as.matrix(data[2]))
  T <- as.vector(as.matrix(data[3]))
  S <- as.vector(as.matrix(data[4]))
  IV <- as.vector(as.matrix(data[5]))
  length <- length(K)
  delta <- c(0)
  r <- 0.004
  for(i in 1:length){
    d1 <- ( log(S[i]/K[i]) + (r + (IV[i]* IV[i] / 2))* T[i]) / (IV[i] * sqrt(T[i]))
    #gaussF <- function(x)exp(-1 * x*x / 2) / sqrt(2*pi)
    #delta[i] <- integrate(gaussF, -Inf, d1)
    tmp <- integrate(dnorm, -Inf, d1)
    delta[i] <- as.numeric(tmp[1])
  }
  write.csv(delta, file="delta.csv")
  #return(delta)
}



riskNeutralPDF <- function(st, strsigma, isDebug="false") {
  sigmad <- eval(D( parse(text=strsigma), "x"), list(x=st))
  sigmadd <- eval(D( D(parse(text=strsigma), "x"), "x"), list(x=st))
  fsigma <- function(t)eval( parse(text=strsigma), list(x=t))
  sigma <- fsigma(st)

  r <- 0.004
  T <- 30
  g <- 0 + r*T

  d1 <- (g + (T*sigma^2) / 2) / (sigma * sqrt(T))
  d2 <- (g - (T*sigma^2) / 2) / (sigma * sqrt(T))

  d1d <- (-1*sigmad * g) / (sigma^2 * sqrt(T)) + (sigmad * sqrt(T)/2) - (1 / (st * sigma * sqrt(T)))
  d2d <- (-1*sigmad * g) / (sigma^2 * sqrt(T)) - (sigmad * sqrt(T)/2) - (1 / (st * sigma * sqrt(T)))
  
  d1dd <- (2*sigmad + st * sigma) / (sigma^2 * st * sqrt(T)) - g * (sigmadd * sigma - 2*sigmad^2) / (sigma^3 * sqrt(T)) + (sigmadd * sqrt(T) / 2)
  d2dd <- (2*sigmad + st * sigma) / (sigma^2 * st * sqrt(T)) - g * (sigmadd * sigma - 2*sigmad^2) / (sigma^3 * sqrt(T)) - (sigmadd * sqrt(T) / 2)


  #first <- st * exp(-1 * d1^2 /2 ) * (-1 * d1 * d1d^2 + d1dd) / sqrt(2*pi)
  #second <- exp(-1*r*T) * exp(-1*d2^2/2) * (2*d2d - st * d2 * d2d^2 + st * d2dd) / sqrt(2*pi)
  
  first <- exp(r*T) * st * exp(-1 * d1^2 /2 ) * (-1 * d1 * d1d^2 + d1dd) / sqrt(2*pi)
  second <- exp(-1 * d2^2/2) * (2*d2d - st * d2 * d2d^2 + st * d2dd) / sqrt(2*pi)
  
  cdd <- first - second 
  result <- cdd
  #if(result < 0){
  #  result <- -1 * result
  #}
  
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
calcSigmak <- function(x) {
  func <- function(t)eval( parse(text=sigmak(x)), list(x=t))
  result <- func(x)
  return(result)
}
constSigmak <- function(k){
  str <- "x * 0 + 0.3"
  return (str)
}


sigmak <- function(k) {
  data <- read.csv("sigmak_table.csv")
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



calcRiskNeutral <- function(x, isDebug="false")riskNeutralPDF(x, strsigma=sigmak(x), isDebug)

makeRiskVec <- function(startnum, endnum) {
  y <- c(0)
  x <- c(0)
  last <- endnum - startnum
  for(i in 1:last){
    y[i] <- calcRiskNeutral(startnum + i)
    x[i] <- startnum + i
  }
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
    result <- result + vec[i]
  }
  return(result)
}

kernelDensity <- function(x, isFunc="false") {
  data <- read.csv("nikkei225_from2003to2010.csv")
  data <- as.matrix(data)
  data <- as.vector(data)
  result <- approxfun(density(data))
  if(isFunc){
    return( result )
  }else{
    return( result(x) )
  }
}

makeRiskVecAuto <- function() {
  vec <- makeRiskVec(6500, 18300)
  #vec <- makeRiskVec(6500, 16380)
  return(vec)
}

calcRiskAversion <- function(x, vec, isDebug="false") {
  startnum <- 6500
  qd <- kernelDensity(x)
  q <- integrate(kernelDensity(1, "true"), startnum, x)
  q <- as.numeric(q[1])
  pd <- calcRiskNeutral(x)
  p <- self_integrate(vec, x)

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
  return(result)
}

calcRiskTable <- function(){
  vec <- makeRiskVecAuto()
  #data <- read.csv("nikkei225_from2003to2010_round.csv")
  data <- read.csv("nikkei_round_timeorder.csv")
  data <- as.matrix(data)
  data <- as.vector(data)
  len <- length(data)
  result <- c(0)
  for(i in 1:len){
    result[i] <- calcRiskAversion(data[i], vec)
  }
  return(result)
}
