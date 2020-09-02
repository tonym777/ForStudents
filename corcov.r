# demostrate cor, cov functions within R
myCorCov <- function(a, b) {
  r <- cor(a, b, use="complete.obs", method="pearson") 
  v <- cov(a, b, use="complete.obs")
  return (c(r,v))
}



df <- read.table("ab.csv", header=TRUE, sep=",")
a <- df[2] 
b <- df[3]
r <- myCorCov(a, b)
print(r)

