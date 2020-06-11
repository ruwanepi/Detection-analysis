## Ruwan Ratnayake, started November 27, 2019
## This code describes a stochastic branching process model for cholera within a ring and outside the ring. 
## It is cut-off at a 60 day period.

library(fitdistrplus)
library(viridis)

## Define parameters

# Serial interval parameters are from Kahn et al (2019): the serial interval for cholera was 
# assumed to follow a gamma distribution (rate = 0.1, shape = 0.5) with a median of five days 
# (for both slow and fast transmission).
SI <- 5 
SIrate <- 0.1
SIshape <- 0.5

# Effective reproduction numbers are set to a range from sub-endemic (0.5), endemic (1), 
# epidemic-low (2), epidemic-high (3), epidemic-very high (15) (From: Azman, 2015, Checchi,2007)
# The dispersion parameter k is set to 10 to effectively be negligible. We don't have reliable
# information on this parameter. Moore (2014) calculated it as a range from 0.0039 to 22028. 
ReCamp <-10
ReVH <-3  
ReH <-2
ReEnd <- 1 
ReSub <- 0.5  
k <- 10  

# Meta-analyzed estimates of intervention reduction of effective reproduction number
AE <- 0.66 #ACP, Reveiz 2011 (meta-analysis)
VE <- 0.69 #OCV, Bi 2017 (meta-analysis)
PE <- 0.42 #POUWT and safe storage, Fewtrell 2005, Roberts 2001 (meta-analysis)
CE <- 0.11 #CWT,Fewtrell 2005 (meta-analysis)
Coverage <- 0.8 #High population coverage
ReWASH <- (1-(Coverage*PE))*ReH #reduction to Rr with POUWT 
ReWASHC <- (1-(Coverage*CE))*ReH #reduction to Rr with local water treatment 
ReACP <- (1-(Coverage*AE))*ReH #reduction to Rr with ACP
ReVE <- (1-(Coverage*VE))*ReH #reduction to Rr with OCV 

# Add stops for caseloads and length of outbreaks (so branching does not go forever)
cap_cases = 100 # Limit number of cases in simulation
cap_max_days <- 30 # Limit number of days of the outbreak to the first month

##Simulating an outbreak. Using the reproductive numbers and the serial interval distribution, simulate stochastic 
##trajectories of cholera outbreaks starting from a single infected index case.

# Set seed for random number generator
set.seed(645)
# Number of simulation runs
runs <- 100
# Number of initial cases
seed <- 1

## 1. Simulate outbreak trajectories inside the ring with an epidemic-level reproduction number (ReCamp=10)

# Initialize plot
plot(NA,xlim=c(0,60),ylim=c(0,100),xlab="Time (days)",
     ylab="Number of cases (Re=10)",frame=FALSE)
cols <- sample(viridis(runs))

# create a vector where you store number of cases in each outbreak
total_cases <- integer(runs)

# The for loop sets up the stochastic model to repeat 100 runs using a seed of one case
# and capturing the length of each outbreak [t] in a vector [times]  
for(i in 1:runs) {  
  cases <- seed  
  t <- rep(0,seed)  
  times <- t  
  # The while loop says that when the caseload >1 (which it will be as the seed=1), a set of 
  # secondary cases is generated using the NegBinom distribution [secondary] for a set serial interval. 
  # To ensure that branching does not go on forever for the outbreaks that do not go extinct,
  # we set a limit to the number of cases in the simulation to 4,500.
  while(cases > 0) {  
    secondary <- rnbinom(cases,size=k,mu=ReCamp) 
    #secondary <- rnbinom(cases,size=k,mu=ReEnd) & secondary<cap_cases [which suggests that
    #we cap secondary generation of cases at cap_cases, but that wouldn't make sense]
    t.new <- numeric()  
    for(j in 1:length(secondary)) {
      t.new <- c(t.new,t[j] + rgamma(secondary[j],shape=SIshape[1],
                                     scale=(1/SIrate))) 
    }
    
    cases <- length(t.new) & cases<cap_cases
    t <- t.new  
    times <- c(times,t.new)  
    
  }
  lines(sort(times),1:length(times),col=cols[i],lwd=1)
  points(max(times),length(times),col=cols[i],pch=16)
  
  total_cases[i] <-length(times) 
}

(tab <- table(total_cases))
barplot(tab, main="Counts")

total_cases.mean <- mean(total_cases)
total_cases.sd <- sd(total_cases)
total_cases.mean 
total_cases.sd 

plnorm(1, total_cases.mean,total_cases.sd,lower.tail=FALSE)
plnorm(20, total_cases.mean,total_cases.sd,lower.tail=FALSE)
plnorm(50, total_cases.mean,total_cases.sd,lower.tail=FALSE)