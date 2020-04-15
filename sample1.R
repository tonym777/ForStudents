# demostrate cor, cov, implied vol function within R

df <- read.table("ab.csv", header=TRUE, sep=",")
a <- df[2]
b <- df[3]
r <- cor(a, b, use="complete.obs", method="pearson") 
v <- cov(a, b, use="complete.obs")


myImpVol <- function(S, K, T, r, q, sigma){
  #S: spot price
  #K: strike price
  #T: time to maturity
  #r: interest rate
  #sigma: volatility of underlying asset
  #q: continuous dividend rate
  
  d1 <- (log(S / K) + (r + 0.5 * sigma ** 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(t)
  vega <- 1 / sqrt(2 * pi) * S * exp(-q * T) * exp(-(d1 ** 2) * 0.5) * sqrt(T)
  
  result <- list(d1, d2, vega)
  return(result)
}

callOption <- function(S, K, T, r, q, sigma)  {
  result <- myImpVol(S, K, T, r, q, sigma)
  c <- pnorm(result$d1) * S - pnorm(result$d2) * K * exp(-r * t)
  return(c)
}

putOption <- function(S, K, T, r, q, sigma)  {
  result <- myImpVol(S, K, T, r, q, sigma)
  p <- pnorm(-result$d2) * K * exp(-r * t) - pnorm(-result$d1) * S
  return(p)
}