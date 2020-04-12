#DCC: Dynamic Conditional Correlation
#PCA: Principle Component Analysis

library(readr)
df <- read_csv("hw8-data.csv")

df['log_return'] = log(1+df['RET']) 
df$CCR=(1+df$RET/365)^365-1

data = data.frame(
  date = df[1:1006,2:2],
  SPY = subset(df, TICKER == "SPY")[,5:5],
  EEM = subset(df, TICKER == "EEM")[,5:5],
  HYG = subset(df, TICKER == "HYG")[,5:5],
  FXI = subset(df, TICKER == "FXI")[,5:5],
  USO = subset(df, TICKER == "USO")[,5:5],
  GLD = subset(df, TICKER == "GLD")[,5:5]
)
colnames(data)=c("date","SPY","EEM","HYG","FXI","USO","GLD")

#1 average continuously compounded returns
ret_SPY = exp(mean(log(1+data$SPY)))-1
ret_EEM = exp(mean(log(1+data$EEM)))-1
ret_HYG = exp(mean(log(1+data$HYG)))-1
ret_FXI = exp(mean(log(1+data$FXI)))-1
ret_USO = exp(mean(log(1+data$USO)))-1
ret_GLD = exp(mean(log(1+data$GLD)))-1

#2 Estimate a DCC model
#(a) estimate GARCH
#(b) DCC estimate
library(rmgarch)
##specify a GARCH(1,1) model assuming the mean returns are zero
uspec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE), distribution.model = "norm")
## Combine the univariate specifications of the 6 GARCH models 
marginspec <- multispec(replicate(6, uspec))
## Create DCC(1,1) specification
mspec <- dccspec(marginspec, dccOrder = c(1,1), model = "DCC", distribution = "mvnorm")
## Fit the DCC(1,1) model (returns are in a matrix returns[]) 
mod <- dccfit(mspec,data[,2:7])

#3 principle component
#(a)
data = data[, 2:7]
y = data.matrix(data)

covariance <- matrix(0, ncol=6, nrow = 6)
for(i in 1:1006){
  r = matrix(y[i,])
  covariance = covariance + r %*% t(r)
}
covariance = covariance /1006
decomp = eigen(covariance)

prcomp(data)  
  
#(c) fraction of the total variance is explained by the first principal component
f = 5.287360e-04/(5.287360e-04 +2.247324e-04+ 6.095459e-05+ 2.756229e-05+ 1.156595e-05+ 4.425693e-06)

#(d) Use the first three principal components to estimate the covariance matrix of the returns on the six ETFs.
v1=matrix(decomp$vectors[,1])
v2=matrix(decomp$vectors[,2])
v3=matrix(decomp$vectors[,3])
v4=matrix(decomp$vectors[,4])
v5=matrix(decomp$vectors[,5])
v6=matrix(decomp$vectors[,6])

p1 = (v1 *decomp$values[1]) %*% t(v1)
p2 = (v2 *decomp$values[2]) %*% t(v2)
p3 = (v3 *decomp$values[3]) %*% t(v3)
p4 = (v4 *decomp$values[4]) %*% t(v4)
p5 = (v5 *decomp$values[5]) %*% t(v5)
p6 = (v6 *decomp$values[6]) %*% t(v6)

cov_by_3 = p1+p2+p3
cov_by_6 = p1+p2+p3+p4+p5+p6
