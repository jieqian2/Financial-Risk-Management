df<-read_csv("~/Downloads/2020 Spring/Financial Risk Management/homework/F567C.s2020.HW5.data.csv")
return = data.frame(date=df[2:nrow(df),1],
                    logreturn=log(df[2:nrow(df), 5]/df[1:(nrow(df)-1), 5] ))
loss = -return$logreturn

mean_excess_loss = vector(mode = 'numeric', length = 21)

count = 1#to save eu

for (u in seq(0.010, 0.05, by = 0.002)){
  N = 0
  tmp = 0
  for (i in 1:length(loss)){
    if (loss[i]>u){
      tmp = tmp + loss[i]- u
      N = N+1
    }
  }
  mean_excess_loss[count] = tmp / N
  count = count+1
}

u = seq(0.010, 0.05, by = 0.002)
plot(u, mean_excess_loss, xlab = "u", ylab = "mean excess loss")




shreshold = 0.022
count1 = 0

for (i in 1:length(loss)){
  if (loss[i] >= shreshold){
    count1 = count1+1
  }
}
cat("The number of losses ≥ 0.022: ", count1)


library('stats4')
library('bbmle')
loss_temp <- loss[loss >= 0.022]
Log_Likelihood <- function(ksi,beta){
  L <- 1/beta*(1+ksi*(loss_temp-0.022)/beta)^(-1/ksi-1)
  -sum(log(L))
}
summary(mle2(Log_Likelihood, start = list(ksi = 0.1, beta = 0.1)))



mel = vector(mode ="numeric")
count = 1

for (i in 1:length(loss)){
  if(loss[i] >= 0.022 && loss[i] <= 0.1){
    mel[count] = loss[i]
    count = count + 1
  }
}
beta = 0.00759947 
ksi = 0.32970407
curve( (1+ksi*(x-0.022)/beta)^(-1/ksi),0.022,0.1,main= "Conditional density function",xlab="Losses", ylab="Conditional density function")


gpd_pdf<-function(x){
  beta = 0.00759947 
  ksi = 0.32970407
  (1+ksi*(x-0.022)/beta)^(-1/ksi)*length(loss_temp)/length(loss)
}

(prob1 <- gpd_pdf(0.022))
(prob2 <- gpd_pdf(0.05))
(prob3 <- gpd_pdf(0.10))

plot(gpd_pdf,xlim = c(0.022, 0.1), type="l",ylab="P(X>=x)", main= "Probabilities of various losses")


