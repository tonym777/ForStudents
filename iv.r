
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
  r <- myImpVol(S, K, T, r, q, sigma)
  price <- pnorm(r[1]) * S - pnorm(r[2]) * K * exp(-r * T)
  return(c(price,r[3]))
}

putOption <- function(S, K, T, r, q, sigma)  {
  r <- myImpVol(S, K, T, r, q, sigma)
  price <- pnorm(-r[2]) * K * exp(-r * T) - pnorm(-r[1]) * S
  return(c(price,r[3]))
}

calcIV <- function(mkt_px, call_or_put, S, K, T, r, q)  {

     loops <- 10
     precision <- 0.001 		
     sigma <- 0.5
     loop <- 1

     while (loop < loops) {		
     		if (call_or_put == 'c')  {
			r <- callOption(S, K, T, r, q, sigma)
                  print(r)
            }
            else
                  r <- putOption(S, K, T, r, q, sigma)

            diff = r[1] - mkt_px
            if (abs(diff) < precision)
			break

		sigma = sigma + diff/r[2]
            loop = loop + 1
     }

     return(sigma)	 
}

implied_vol = calcIV(10.0, 'c', 386.98, 386, 30/365, 0.0001, 0.0)
print(implied_vol)
