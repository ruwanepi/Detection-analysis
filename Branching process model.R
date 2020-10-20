#######################################################################
## This code describes a branching process model for cholera outbreaks
## during an early growth phase
## Submission: BMC Medicine <title: Early detection of cholera epidemics 
## to support control in fragile and conflict-affected states: estimated 
## delays and potential epidemic sizes>
## Author: Ruwan Ratnayake, October 2020
## Code adapted from Althaus et al 2015 
## <doi:10.1016/S1473-3099(15)70135-0>
#######################################################################

####################
# 0 - Load libraries
####################
library(fitdistrplus)
library(viridis)

################
# 1 - Parameters  
################
SI <- 5 # Kahn (2019): the serial interval for cholera was assumed to follow
SIrate <- 0.1 # a gamma distribution (rate = 0.1, shape = 0.5)
SIshape <- 0.5 # with a median serial interval of 5 days

R <- 2.5 # Effective reproduction number in early phase (From: Azman, 2015)
k <- 4.5 # Dispersion parameter k is set to 4.5 
        # (Moore 2014; used range from 0.0039 to 22028)
cap_cases <- 1000


################
# 2 - Approach
################

set.seed(645)
runs <- 10000
seed <- 3
#Branching process model using seed cases. Repeat
#for 10 and 20 seed cases


#############################
# 3 - Branching process model
#############################

## 1. Simulate outbreak trajectory for 5-day delay
plot(NA,xlim=c(0,60),ylim=c(0,100),xlab="Delay since primary case",
     ylab="Number of cases",frame=FALSE)
cols <- sample(viridis(runs))

total_cases <- integer(runs)

for(i in 1:runs) {
  cases <- seed
  t <- rep(0,seed)
  times <- t
  while(cases > 0) {
    secondary <- rnbinom(cases,size=k,mu=R)
    t.new <- numeric()
    for(j in 1:length(secondary)) {
      t.new <- c(t.new,t[j] + rgamma(secondary[j],shape=SIshape[1],
                                     scale=(1/SIrate)))
    }
    cases <- length(t.new) & cases<cap_cases
    t <- t.new
    times <- c(times[times<=5],t.new)
  }
  lines(sort(times),1:length(times),col=cols[i],lwd=1)
  points(max(times),length(times),col=cols[i],pch=16)
  total_cases[i] <-length(times)
}

print(total_cases.median <- median(total_cases))
print(total_cases.sd <- sd(total_cases))
print(total_cases.range <- range(total_cases))

#Transform into log scale
lsm=log(total_cases.median)-(1/2)*log((total_cases.sd/total_cases.median)^2+1)
lssd=sqrt(log((total_cases.sd/total_cases.median)^2+1))
plnorm(20, lsm, lssd)


## 2. Simulate outbreak trajectory for 7-day delay
plot(NA,xlim=c(0,60),ylim=c(0,100),xlab="Delay since primary case",
     ylab="Number of cases",frame=FALSE)
cols <- sample(viridis(runs))

total_cases_7 <- integer(runs)

for(i in 1:runs) {
  cases <- seed
  t <- rep(0,seed)
  times <- t
  while(cases > 0) {
    secondary <- rnbinom(cases,size=k,mu=R)
    t.new <- numeric()
    for(j in 1:length(secondary)) {
      t.new <- c(t.new,t[j] + rgamma(secondary[j],shape=SIshape[1],
                                     scale=(1/SIrate)))
    }
    cases <- length(t.new) & cases<cap_cases
    t <- t.new
    times <- c(times[times<=7],t.new)
  }
  lines(sort(times),1:length(times),col=cols[i],lwd=1)
  points(max(times),length(times),col=cols[i],pch=16)
  total_cases_7[i] <-length(times)
}

print(total_cases_7.median <- median(total_cases_7))
print(total_cases_7.sd <- sd(total_cases_7))
print(total_cases_7.range <- range(total_cases_7))

#Transform into log scale
lsm_7=log(total_cases_7.median)-(1/2)*log((total_cases_7.sd/total_cases_7.median)^2+1)
lssd_7=sqrt(log((total_cases_7.sd/total_cases_7.median)^2+1))
plnorm(20, lsm_7, lssd_7)
 

## 3. Simulate outbreak trajectory for 10-day delay
plot(NA,xlim=c(0,60),ylim=c(0,100),xlab="Delay since primary case",
     ylab="Number of cases",frame=FALSE)
cols <- sample(viridis(runs))

total_cases_10 <- integer(runs)

for(i in 1:runs) {
  cases <- seed
  t <- rep(0,seed)
  times <- t
  while(cases > 0) {
    secondary <- rnbinom(cases,size=k,mu=R)
    t.new <- numeric()
    for(j in 1:length(secondary)) {
      t.new <- c(t.new,t[j] + rgamma(secondary[j],shape=SIshape[1],
                                     scale=(1/SIrate)))
    }
    cases <- length(t.new) & cases<cap_cases
    t <- t.new
    times <- c(times[times<=10],t.new)
  }
  lines(sort(times),1:length(times),col=cols[i],lwd=1)
  points(max(times),length(times),col=cols[i],pch=16)
  total_cases_10[i] <-length(times)
}

print(total_cases_10.median <- median(total_cases_10))
print(total_cases_10.sd <- sd(total_cases_10))
print(total_cases_10.range <- range(total_cases_10))

#Transform into log scale
lsm_10=log(total_cases_10.median)-(1/2)*log((total_cases_10.sd/total_cases_10.median)^2+1)
lssd_10=sqrt(log((total_cases_10.sd/total_cases_10.median)^2+1))
plnorm(20, lsm_10, lssd_10)
 

## 4. Simulate outbreak trajectory for 11-day delay
plot(NA,xlim=c(0,60),ylim=c(0,100),xlab="Delay since primary case",
     ylab="Number of cases",frame=FALSE)
cols <- sample(viridis(runs))

total_cases_11 <- integer(runs)

for(i in 1:runs) {
  cases <- seed
  t <- rep(0,seed)
  times <- t
  while(cases > 0) {
    secondary <- rnbinom(cases,size=k,mu=R)
    t.new <- numeric()
    for(j in 1:length(secondary)) {
      t.new <- c(t.new,t[j] + rgamma(secondary[j],shape=SIshape[1],
                                     scale=(1/SIrate)))
    }
    cases <- length(t.new) & cases<cap_cases
    t <- t.new
    times <- c(times[times<=11],t.new)
  }
  lines(sort(times),1:length(times),col=cols[i],lwd=1)
  points(max(times),length(times),col=cols[i],pch=16)
  total_cases_11[i] <-length(times)
}

print(total_cases_11.median <- median(total_cases_11))
print(total_cases_11.sd <- sd(total_cases_11))
print(total_cases_11.range <- range(total_cases_11))

#Transform into log scale
lsm_11=log(total_cases_11.median)-(1/2)*log((total_cases_11.sd/total_cases_11.median)^2+1)
lssd_11=sqrt(log((total_cases_11.sd/total_cases_11.median)^2+1))
plnorm(20, lsm_11, lssd_11)


## 5. Simulate outbreak trajectory for 14-day delay
plot(NA,xlim=c(0,60),ylim=c(0,100),xlab="Delay since primary case",
     ylab="Number of cases",frame=FALSE)
cols <- sample(viridis(runs))

total_cases_14 <- integer(runs)

for(i in 1:runs) {
  cases <- seed
  t <- rep(0,seed)
  times <- t
  while(cases > 0) {
    secondary <- rnbinom(cases,size=k,mu=R)
    t.new <- numeric()
    for(j in 1:length(secondary)) {
      t.new <- c(t.new,t[j] + rgamma(secondary[j],shape=SIshape[1],
                                     scale=(1/SIrate)))
    }
    cases <- length(t.new) & cases<cap_cases
    t <- t.new
    times <- c(times[times<=14],t.new)
  }
  lines(sort(times),1:length(times),col=cols[i],lwd=1)
  points(max(times),length(times),col=cols[i],pch=16)
  total_cases_14[i] <-length(times)
}

print(total_cases_14.median <- median(total_cases_14))
print(total_cases_14.sd <- sd(total_cases_14))
print(total_cases_14.range <- range(total_cases_14))

#Transform into log scale
lsm_14=log(total_cases_14.median)-(1/2)*log((total_cases_14.sd/total_cases_14.median)^2+1)
lssd_14=sqrt(log((total_cases_14.sd/total_cases_14.median)^2+1))
plnorm(20, lsm_14, lssd_14)
 

## 6. Simulate outbreak trajectory for 21-day delay
plot(NA,xlim=c(0,60),ylim=c(0,100),xlab="Delay since primary case",
     ylab="Number of cases",frame=FALSE)
cols <- sample(viridis(runs))

total_cases_21 <- integer(runs)

for(i in 1:runs) {
  cases <- seed
  t <- rep(0,seed)
  times <- t
  while(cases > 0) {
    secondary <- rnbinom(cases,size=k,mu=R)
    t.new <- numeric()
    for(j in 1:length(secondary)) {
      t.new <- c(t.new,t[j] + rgamma(secondary[j],shape=SIshape[1],
                                     scale=(1/SIrate)))
    }
    cases <- length(t.new) & cases<cap_cases
    t <- t.new
    times <- c(times[times<=21],t.new)
  }
  lines(sort(times),1:length(times),col=cols[i],lwd=1)
  points(max(times),length(times),col=cols[i],pch=16)
  total_cases_21[i] <-length(times)
}

print(total_cases_21.median <- median(total_cases_21))
print(total_cases_21.sd <- sd(total_cases_21))
print(total_cases_21.range <- range(total_cases_21))

#Transform into log scale
lsm_21=log(total_cases_21.median)-(1/2)*log((total_cases_21.sd/total_cases_21.median)^2+1)
lssd_21=sqrt(log((total_cases_21.sd/total_cases_21.median)^2+1))
plnorm(20, lsm_21, lssd_21)
 

## 7. Simulate outbreak trajectory for 30-day delay
plot(NA,xlim=c(0,60),ylim=c(0,100),xlab="Delay since primary case",
     ylab="Number of cases",frame=FALSE)
cols <- sample(viridis(runs))

total_cases_30 <- integer(runs)

for(i in 1:runs) {
  cases <- seed
  t <- rep(0,seed)
  times <- t
  while(cases > 0) {
    secondary <- rnbinom(cases,size=k,mu=R)
    t.new <- numeric()
    for(j in 1:length(secondary)) {
      t.new <- c(t.new,t[j] + rgamma(secondary[j],shape=SIshape[1],
                                     scale=(1/SIrate)))
    }
    cases <- length(t.new) & cases<cap_cases
    t <- t.new
    times <- c(times[times<=30],t.new)
  }
  lines(sort(times),1:length(times),col=cols[i],lwd=1)
  points(max(times),length(times),col=cols[i],pch=16)
  total_cases_30[i] <-length(times)
}

print(total_cases_30.median <- median(total_cases_30))
print(total_cases_30.sd <- sd(total_cases_30))
print(total_cases_30.range <- range(total_cases_30))

#Transform into log scale
lsm_30=log(total_cases_30.median)-(1/2)*log((total_cases_30.sd/total_cases_30.median)^2+1)
lssd_30=sqrt(log((total_cases_30.sd/total_cases_30.median)^2+1))
plnorm(20, lsm_30, lssd_30)