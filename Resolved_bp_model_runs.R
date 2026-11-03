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
library(tidyverse)

################
# 1 - Parameters  
################
SI <- 5 # Kahn (2019): the serial interval for cholera was assumed to follow
SIrate <- 0.1 # a gamma distribution (rate = 0.1, shape = 0.5)
SIshape <- 0.5 # with a median serial interval of 5 days

R <- 2.5 # Effective reproduction number in early phase (From: Azman, 2015)
## If use 1.5 or 2.0, (in same Azman paper and Camacho paper), you would be 
## simulating populations that are vaccinated or unvaccinated, i.e.
## R_0_unvaccinated = 2.2-2.4 and R_0_vaccinated = 1.5-1.9 in South Sudan

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
dev.off()
plot(NA,xlim=c(0,60), ylim=c(0,1000),
     xlab="Delay since primary case",
     ylab="Number of cases",frame=FALSE)
cols <- sample(viridis(runs))

(total_cases <- as.data.frame(cbind(1:runs, 0)))

for(i in 1:runs) {
  ## Start with initial seed cases
  cases <- seed
  ## Store infection times of current active cases. 
  ## The seed cases occur at time 0.
  t <- rep(0,seed)
  ## Keep a record of all infection times observed so far.
  times <- t
  ## Simulate transmission generation by generation;
  ## Continue until there are no infectious individuals
  while(cases > 0) {
    ## For each current case, draw how many new infections they 
    ## generate.
    (secondary <- rnbinom(cases,size=k,mu=R))
    ## Tally number of secondary cases
    (sum(secondary))
    ## Create an empty vector that will hold infection times of 
    ## all newly infected individuals.
    t.new <- numeric()
    ## Loop through each secondary case (j)
    for(j in seq_along(secondary)) {
      ## For each case (j), draw a serial interval to produce a set
      ## of infection times for each secondary case
      t.new <- c(t.new,t[j] + rgamma(secondary[j],shape=SIshape[1],
                                     scale=(1/SIrate)))
    }
    (t.new)
    
    ## Tally the number of secondary cases, by summing the list of
    ## secondary cases produced by each seed case. However, only 
    ## count them if they are < 1000 cases (cap_cases). 
    ## Otherwise cases go to zero and end that simulation and proceed
    ## to the next simulation.
    
    #(cases <- ifelse(length(times) + sum(secondary) <= cap_cases,
    #                sum(secondary), 0))
    
    new_cases <- sum(secondary)
    
    if(length(times) + new_cases >= cap_cases) {
      break}
    
    # We're calling t.new 't' now
    t <- t.new
    
    ## Append infection times of the 3 seeds (all times = 0) to
    ## infection times of secondary cases (all times > 0)
    #times <- c(times, t.new)
    (times <- c(times, t.new))
    
    cases <- new_cases
    
  }
  # Restrict infection times to within the delay period of 5 days
  (times <- times[times<=5])
  # Plot graph based on restricted infection times
  lines(sort(times), 1:length(times), col=cols[i], lwd=1)
  points(max(times), length(times), col=cols[i], pch=16)
  # Tally total number of cases in this outbreak
  #total_cases[i] <- length(times)
  (total_cases[i,2] <- length(times))
}

print(total_cases.median <- median(total_cases[,2]))
print(total_cases.sd <- sd(total_cases[,2]))
print(total_cases.range <- range(total_cases[,2]))
#Number of outbreaks <20 cases based on total_cases table
total_cases %>% group_by(V2 < 20) %>% 
  summarize(n = n()) %>% mutate(freq = n / sum(n)*100) 
# identify which runs violate the cap (if any) to make sure code is working
max(total_cases[,2])
which(total_cases[,2] > 1000)


#Transform into log scale
lsm=log(total_cases.median)-(1/2)*log((total_cases.sd/total_cases.median)^2+1)
lssd=sqrt(log((total_cases.sd/total_cases.median)^2+1))
plnorm(20, lsm, lssd)
1-plnorm(20, lsm, lssd)



