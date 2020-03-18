df<-read_csv("~/Downloads/2020 Spring/Financial Risk Management/homework/F567C.s2020.HW5.data.csv")
Return = data.frame(date=df[2:nrow(df),1],
                    logreturn=log(df[2:nrow(df), 5]/df[1:(nrow(df)-1), 5] ))
ret = Return$logreturn
sample_std = sqrt(sum(ret^2)/length(ret))
sigma1_hat = sqrt(sum(ret[1:10]^2)/10)

####################################################################################################################
#question part 1
####################################################################################################################
initial_guess_parameters = c(0.1,0.8,0.01,0.01)
fr1 <- function(x) {  
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = x[4]^2 
  #(negative) Log Likelihood 
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001||  x[4]<0.001)
  {
    NeglogLH = 9999
  } else 
  {
    for (i in 1:(length(ret)-1) )
    {
      sigmasqhat[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
    }
    f <- (1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  return(NeglogLH)
}
parameters = optim(initial_guess_parameters, fr1)
parameters

sigmasqhat = rep(0,length(ret))
sigmasqhat[1] = parameters$par[4]^2 
for (i in 1:(length(ret)+1) )
{
  sigmasqhat[i+1] = (1-parameters$par[1]-parameters$par[2])*parameters$par[3]^2+parameters$par[1]*ret[i]^2+parameters$par[2]*sigmasqhat[i]
}
sigmasqhat[length(ret)]
sigmasqhat[length(ret)+1]
sqrt(sigmasqhat[length(ret)+1])

estimate_sigma_sq_sgm = rep(0,21)
estimate_sigma_sq_sgm[1]= (1-parameters$par[1]-parameters$par[2])*parameters$par[3]^2 +parameters$par[1]*ret[length(ret)]^2 + parameters$par[2]*sigmasqhat[length(ret)]
for(i in 2:21){
  estimate_sigma_sq_sgm[i+1] =parameters$par[3]^2 +(parameters$par[1]+parameters$par[2])^(i-1) * (estimate_sigma_sq_sgm[1]-parameters$par[3]^2)
}

variance=sum(estimate_sigma_sq_sgm)
variance
volatility = sqrt(variance*(252/21))
volatility

####################################################################################################################
#question p2
####################################################################################################################
initial_guess_parameters2 = c(0.1,0.8,0.01)
fr2 <- function(x) {  
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = x[3]^2 
  #(negative) Log Likelihood 
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001)
  {
    NeglogLH = 9999
  } else 
  {
    for (i in 1:(length(ret)-1) )
    {
      sigmasqhat[i+1] = (1-x[1]-x[2])*sample_std^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
    }
    f <- (1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  return(NeglogLH)
}
parameters = optim(initial_guess_parameters2, fr2)
parameters
sigmasqhat = rep(0,length(ret))
sigmasqhat[1] = parameters$par[3]^2 
for (i in 1:(length(ret)+1) )
{
  sigmasqhat[i+1] = (1-parameters$par[1]-parameters$par[2])*sample_std^2+parameters$par[1]*ret[i]^2+parameters$par[2]*sigmasqhat[i]
}
sigmasqhat[length(ret)]
sigmasqhat[length(ret)+1]
sqrt(sigmasqhat[length(ret)+1])


estimate_sigma_sq_sgm = rep(0,21)
estimate_sigma_sq_sgm[1]= (1-parameters$par[1]-parameters$par[2])*sample_std^2 +parameters$par[1]*ret[length(ret)]^2 + parameters$par[2]*sigmasqhat[length(ret)]
for(i in 2:21){
  estimate_sigma_sq_sgm[i+1] =sample_std^2 +(parameters$par[1]+parameters$par[2])^(i-1) * (estimate_sigma_sq_sgm[1]-sample_std^2)
}

variance=sum(estimate_sigma_sq_sgm)
variance
volatility = sqrt(variance*(252/21))
volatility

####################################################################################################################
#question1 p3
####################################################################################################################
sigma1_hat = sqrt(sum(ret[1:10]^2)/10)
initial_guess_parameters2 = c(0.1,0.8,0.01)
fr2 <- function(x) {  
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = sigma1_hat^2 
  #(negative) Log Likelihood 
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001)
  {
    NeglogLH = 9999
  } else 
  {
    for (i in 1:(length(ret)-1) )
    {
      sigmasqhat[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
    }
    f <- (1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  return(NeglogLH)
}
parameters = optim(initial_guess_parameters2, fr2)
parameters
sigmasqhat = rep(0,length(ret))
sigmasqhat[1] = sigma1_hat^2 
for (i in 1:(length(ret)+1) )
{
  sigmasqhat[i+1] = (1-parameters$par[1]-parameters$par[2])*parameters$par[3]^2+parameters$par[1]*ret[i]^2+parameters$par[2]*sigmasqhat[i]
}
sigmasqhat[length(ret)]
sigmasqhat[length(ret)+1]
sqrt(sigmasqhat[length(ret)+1])

estimate_sigma_sq_sgm = rep(0,21)
estimate_sigma_sq_sgm[1]= (1-parameters$par[1]-parameters$par[2])*parameters$par[3]^2 + parameters$par[1]*ret[length(ret)]^2 + parameters$par[2]*sigmasqhat[length(ret)]
for(i in 2:21){
  estimate_sigma_sq_sgm[i+1] =parameters$par[3]^2 +(parameters$par[1]+parameters$par[2])^(i-1) * (estimate_sigma_sq_sgm[1]-parameters$par[3]^2)
}

variance=sum(estimate_sigma_sq_sgm)
variance
volatility = sqrt(variance*(252/21))
volatility

####################################################################################################################
#question p4
####################################################################################################################
sample_std = sqrt(sum(ret^2)/length(ret))
sigma1_hat = sqrt(sum(ret[1:10]^2)/10)
initial_guess_parameters2 = c(0.1,0.8)
fr2 <- function(x) {  
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = sigma1_hat^2 
  #(negative) Log Likelihood 
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 )
  {
    NeglogLH = 9999
  } else 
  {
    for (i in 1:(length(ret)-1) )
    {
      sigmasqhat[i+1] = (1-x[1]-x[2])*sample_std^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
    }
    f <- (1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  return(NeglogLH)
}
parameters = optim(initial_guess_parameters2, fr2)
parameters
sigmasqhat = rep(0,length(ret))
sigmasqhat[1] = sigma1_hat^2 
for (i in 1:(length(ret)+1) )
{
  sigmasqhat[i+1] = (1-parameters$par[1]-parameters$par[2])*sample_std^2+parameters$par[1]*ret[i]^2+parameters$par[2]*sigmasqhat[i]
}
sigmasqhat[length(ret)]
sigmasqhat[length(ret)+1]
sqrt(sigmasqhat[length(ret)+1])


estimate_sigma_sq_sgm = rep(0,21)
estimate_sigma_sq_sgm[1]= (1-parameters$par[1]-parameters$par[2])*sample_std^2 +parameters$par[1]*ret[length(ret)]^2 + parameters$par[2]*sigmasqhat[length(ret)]
for(i in 2:21){
  estimate_sigma_sq_sgm[i+1] =sample_std^2 +(parameters$par[1]+parameters$par[2])^(i-1) * (estimate_sigma_sq_sgm[1]-sample_std^2)
}

variance=sum(estimate_sigma_sq_sgm)
variance
volatility = sqrt(variance*(252/21))
volatility

####################################################################################################################
#question 3 
####################################################################################################################
library(tseries)
parameters_garch = garch(ts(ret), order=c(1,1))
summary(parameters_garch)
omiga=coef(parameters_garch)[1]
alpha=coef(parameters_garch)[2]
beta=coef(parameters_garch)[3]
sigma =sqrt(omiga/(1-alpha-beta)) 

library(fGarch)
parameters_garch_fit = garchFit(formula = ~garch(1,1), data = ts(ret), include.mean = FALSE)
summary(parameters_garch_fit)
omiga_fit=coef(parameters_garch_fit)[1]
alpha_fit=coef(parameters_garch_fit)[2]
beta_fit=coef(parameters_garch_fit)[3]
sigma_fit =sqrt(omiga_fit/(1-alpha_fit-beta_fit)) 

####################################################################################################################
#question 4
####################################################################################################################
initial_guess_parameters = c(0.1,0.8,0.01,0.01,0)
fr1 <- function(x) {  
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = x[4]^2 
  #(negative) Log Likelihood 
  if (x[1]*(1+x[5]^2)+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001||  x[4]<0.001)
  {
    NeglogLH = 9999
  } else 
  {
    for (i in 1:(length(ret)-1) )
    {
      sigmasqhat[i+1] = (1-x[1]*(1+x[5]^2)-x[2])*x[3]^2 + x[1]*(ret[i]-x[5]*sqrt(sigmasqhat[i]))^2 +x[2]*sigmasqhat[i]
    }
    f <- (1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  return(NeglogLH)
}
parameters = optim(initial_guess_parameters, fr1)
parameters

