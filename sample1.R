# demostrate cor and cov function within R

df <- read.table("ab.csv", header=TRUE, sep=",")
a <- df[2]
b <- df[3]


cor(a, b, use="complete.obs", method="pearson") 
cov(a, b, use="complete.obs")