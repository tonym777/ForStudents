# demostrate cor, cov functions within R
myCorCov <- function() {
  df <- read.table("ab.csv", header=TRUE, sep=",")
  a <- df[2] 
  b <- df[3]
  r <- cor(a, b, use="complete.obs", method="pearson") 
  v <- cov(a, b, use="complete.obs")
  return (c(r,v))
}

####
####  BS option model
####
####
myImpVol <- function(S, K, T, r, q, sigma){
  #S: spot price
  #K: strike price
  #T: time to maturity
  #r: interest rate
  #sigma: volatility of underlying asset
  #q: continuous dividend rate
  
  d1 <- (log(S / K) + (r + 0.5 * sigma ** 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  vega <- 1 / sqrt(2 * pi) * S * exp(-q * T) * exp(-(d1 ** 2) * 0.5) * sqrt(T)
  return (c(d1, d2, vega))
}

callOption <- function(S, K, T, r, q, sigma)  {
  list(d1,d2,vega) <- myImpVol(S, K, T, r, q, sigma)
  c <- pnorm(d1) * S - pnorm(d2) * K * exp(-r * T)
  return(c)
}

putOption <- function(S, K, T, r, q, sigma)  {
  list(d1,d2,vega) <- myImpVol(S, K, T, r, q, sigma)
  p <- pnorm(-d2) * K * exp(-r * T) - pnorm(-d1) * S
  return(p)
}