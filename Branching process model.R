# This is code to replicate the branching model from my August 2020
# submission to BMC Medicine entitled Early detection of cholera 
# epidemics to support control in fragile and conflict-affected states: 
# estimated delays and potential impacts.
# Code developed by Ruwan Ratnayake, 
#based on Althaus et al 2015 (doi.org/10.1016/S1473-3099(15)70135-0)
# This code describes a stochastic branching process model for cholera
# during an early growth phase

library(fitdistrplus)
library(viridis)
library(ggplot2)

SI <- 5 # Kahn (2019): the serial interval for cholera was assumed to follow
SIrate <- 0.1 # a gamma distribution (rate = 0.1, shape = 0.5)
SIshape <- 0.5 # with a median serial interval of 5 days

R <- 2.5 # Effective reproduction number in early phase (From: Azman, 2015)
k <- 4.5 # Dispersion parameter k is set to 4.5 to effectively be negligible
        # as we don't expect super-spreading events
        # (Moore 2014; used range from 0.0039 to 22028)
cap_cases <- 1000

#Branching process model using single infected index case. Repeat
#for 3 seed cases
set.seed(645)
runs <- 10000
seed <- 1

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
#Graph
sample.range <-1:28
cases.dist <- plnorm(sample.range, lsm, lssd)
cases.df <- data.frame("Number_cases" = sample.range, "Proportion" = cases.dist)
ggplot(cases.df, aes(x = Number_cases, y = Proportion)) + geom_point()


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
#Graph
sample.range_7 <-1:53
cases_7.dist <- plnorm(sample.range_7, lsm_7, lssd_7)
cases_7.df <- data.frame("Number_cases_7" = sample.range_7, "Proportion_7" = cases_7.dist)
A = cases.df
B = cases_7.df
p = ggplot() +
  geom_point(data=A, aes(x = Number_cases, y = Proportion), color="blue") +
  geom_point(data=B, aes(x = Number_cases_7, y = Proportion_7), color="red") +
  xlab('Cases') +
  ylab('Probability')
print(p)
  
  
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
#Graph
sample.range_10 <-1:66
cases_10.dist <- plnorm(sample.range_10, lsm_10, lssd_10)
cases_10.df <- data.frame("Number_cases_10" = sample.range_10, "Proportion_10" = cases_10.dist)
A = cases.df
B = cases_7.df
C = cases_10.df

p = ggplot() +
  geom_point(data=A, aes(x = Number_cases, y = Proportion), color="blue") +
  geom_point(data=B, aes(x = Number_cases_7, y = Proportion_7), color="red") +
  geom_point(data=C, aes(x = Number_cases_10, y = Proportion_10), color="green") +
  xlab('Cases') +
  ylab('Probability')
print(p)

## 3.5 Simulate outbreak trajectory for 11-day delay
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


## 4. Simulate outbreak trajectory for 14-day delay
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
#Graph
sample.range_14 <-1:69
cases_14.dist <- plnorm(sample.range_14, lsm_14, lssd_14)
cases_14.df <- data.frame("Number_cases_14" = sample.range_14, "Proportion_14" = cases_14.dist)
A = cases.df
B = cases_7.df
C = cases_10.df
D = cases_14.df

p = ggplot() +
  geom_point(data=A, aes(x = Number_cases, y = Proportion), color="blue") +
  geom_point(data=B, aes(x = Number_cases_7, y = Proportion_7), color="red") +
  geom_point(data=C, aes(x = Number_cases_10, y = Proportion_10), color="green") +
  geom_point(data=D, aes(x = Number_cases_14, y = Proportion_14), color="orange") +
  xlab('Cases') +
  ylab('Probability')
print(p)


## 5. Simulate outbreak trajectory for 21-day delay
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
#Graph
sample.range_21 <-1:95
cases_21.dist <- plnorm(sample.range_21, lsm_21, lssd_21)
cases_21.df <- data.frame("Number_cases_21" = sample.range_21, "Proportion_21" = cases_21.dist)
A = cases.df
B = cases_7.df
C = cases_10.df
D = cases_14.df
E = cases_21.df

p = ggplot() +
  geom_point(data=A, aes(x = Number_cases, y = Proportion), color="blue") +
  geom_point(data=B, aes(x = Number_cases_7, y = Proportion_7), color="red") +
  geom_point(data=C, aes(x = Number_cases_10, y = Proportion_10), color="green") +
  geom_point(data=D, aes(x = Number_cases_14, y = Proportion_14), color="orange") +
  geom_point(data=E, aes(x = Number_cases_21, y = Proportion_21), color="yellow") +
  xlab('Cases') +
  ylab('Probability')
print(p)


## 6. Simulate outbreak trajectory for 30-day delay
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
#Graph
sample.range_30 <-1:139
cases_30.dist <- plnorm(sample.range_30, lsm_30, lssd_30)
cases_30.df <- data.frame("Number_cases_30" = sample.range_30, "Proportion_30" = cases_30.dist)
A = cases.df
B = cases_7.df
C = cases_10.df
D = cases_14.df
E = cases_21.df
G = cases_30.df

p = ggplot() +
  geom_point(data=A, aes(x = Number_cases,    y = Proportion,    group = 1, colour = '5-day') , size=0.5) +
  geom_point(data=C, aes(x = Number_cases_10, y = Proportion_10, group = 2, colour = '10-day'), size=0.5) +
  geom_point(data=D, aes(x = Number_cases_14, y = Proportion_14, group = 3, colour = '14-day'), size=0.5) +
  geom_point(data=G, aes(x = Number_cases_30, y = Proportion_30, group = 5, colour = '30-day'), size=0.5) +
  labs(x= "Simulated epidemic size",
       y= "Cumulative probability")+
  scale_color_manual(values=c("orange2", "orangered1", "orangered2",
                              "orangered3", "orange"))+
  scale_x_continuous(breaks=seq(0, 150, 10))+ 
  theme(legend.title=element_blank(),
  text = element_text(size=7))

p + theme(legend.position = "bottom")