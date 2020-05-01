
#1b
correlation_matrix = matrix(0.3, 100, 100)
diag(correlation_matrix) = 1
u = rep(0, 100)
default= c()
loss = c()
d2 = qnorm(0.01)
library(MASS)

for(i in 1:10000){
  z = mvrnorm(100, u, correlation_matrix)
  loss_number=0
  loss_accum=0
  for(j in 1:100){
    if(z[j]<d2){
      loss_number = loss_number+1
      loss_accum = loss_accum+ 10000000 * rbeta(1,2,2)
    }
  }
  default = c(default,loss_number )
  loss = c(loss, loss_accum)
}
hist(loss, breaks = seq(0,200000000,10000000),main="distribution of losses")

#1c
correlation_matrix = matrix(0.4, 100, 100)

diag(correlation_matrix) = 1
u = rep(0, 100)
default= c()
loss = c()
d2 = qnorm(0.01)
library(MASS)

for(i in 1:10000){
  z = mvrnorm(100, u, correlation_matrix)
  loss_number=0
  loss_accum=0
  for(j in 1:100){
    if(z[j]<d2){
      loss_number = loss_number+1
      loss_accum = loss_accum+ 10000000 * rbeta(1,2,2)
    }
  }
  default = c(default,loss_number )
  loss = c(loss, loss_accum)
}
hist(loss, breaks = seq(0,200000000,10000000),main="distribution of losses")


#2c
#2d
m = 2.17009085
rho = 0.3
m1 = 2.1272584
dt = 0.0025
time = seq(0,1,1/400)
n = length(time)
a = 0.05
set.seed(2)
default_ratec = c()
default_rated = c()
for(k in 1:10) {
  default_c = 0
  default_d = 0
  for(i in 1:10000){
    rv = rnorm(n-1)
    rvcorr = rho*rv+sqrt(1-rho^2)*rnorm(n-1)
    
    ma =m
    mbc = m1
    flag_a=0
    flag_b=0
    for(j1 in 2:n){
      ma = ma + sqrt(dt)*rv[j1-1]
      mbc = mbc+a*dt+sqrt(dt)*rvcorr[j1-1]
      if(ma<0 ){
        flag_a=1
      }
      if(mbc<0){
        flag_b=1
      }
      if((flag_a==1) & (flag_b==1)){
        default_c = default_c+1
        break
      }
    }
    
    ma = m
    mbd = m
    flag_a=0
    flag_b=0
    for(j2 in 2:n){
      ma = ma + sqrt(dt)*rv[j2-1]
      mbd =mbd + sqrt(dt)*rvcorr[j2-1]
      if(ma<0 ){
        flag_a=1
      }
      if(mbd<0){
        flag_b=1
      }
      if((flag_a==1) & (flag_b==1)){
        default_d = default_d+1
        break
      }
    } 
    
    
  }
  default_ratec = c(default_ratec, default_c/10000)
  default_rated = c(default_rated, default_d/10000)
}
avg_default_c = mean(default_ratec)
avg_default_c
avg_default_d= mean(default_rated)
avg_default_d
diff = abs(avg_default_c-avg_default_d)
diff

diff*10000


#3
rho = 0.3
dt = 1/50
time = seq(0,1,dt)
n = length(time)

total_losses = 0

for(i in 1:1000){
  rv = rnorm(n)
  cor_mat = matrix(rnorm(n*100),n,100)
  z = rho^(1/2)*rv+(1-rho)^(1/2)*cor_mat
  
  distance_to_default = matrix(0,n,100)
  distance_to_default[1,] = rep(2.17,100)
  default_time = matrix(0,n-1,100)
  default = matrix(0,n-1,100)
  loss = matrix(0,n-1,100)

  for(j in 1:(n-1)){
    distance_to_default[j+1,] = distance_to_default[j,]+sqrt(dt)*z[j+1,]
    
    for(k in 1:100){
      if((distance_to_default[j+1,k]<=0)&(sum(default_time[1:j-1,k]) == 0)){
        default_time[j,k]=time[j+1]/2
        default[j,k]=1
        loss[j,k]=(1-rbeta(1,2,2))*10
      }else{
        default_time[j,k]=0
      }
    }
  }
  total_losses[i] = sum(loss)
}
loss_rate = total_losses/(10*100)
hist(loss_rate)
hist(total_losses)









