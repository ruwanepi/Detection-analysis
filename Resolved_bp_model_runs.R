###########################################################################
## This code describes a branching process model for cholera outbreaks
## during an early growth phase
## Submission: BMC Medicine <title: Early detection of cholera epidemics 
## to support control in fragile and conflict-affected states: estimated 
## delays and potential epidemic sizes>
## Author: Ruwan Ratnayake, October 2020
## Code adapted from Althaus et al 2015 <doi:10.1016/S1473-3099(15)70135-0>
## **Note that this code was corrected due to a coding error in Nov 2026
###########################################################################

####################
# 0 - Load libraries
####################
library(fitdistrplus)
library(viridis)

################
# 1 - Parameters  
################
SI <- 5 # Kahn (2019): the median serial interval for cholera was assumed to follow
SIrate <- 0.1 # a gamma distribution (rate = 0.1, shape = 0.5)
SIshape <- 0.5 # with a median serial interval of 5 days

R <- 2.5 # Effective reproduction number in early phase (From: Azman, 2015)
## If use 1.5 or 2.0, (in same Azman paper and Camacho paper), you would be 
## simulating populations that could be considered vaccinated or unvaccinated, i.e.
## R_E_unvaccinated = 2.2-2.4 and R_E_vaccinated = 1.5-1.9 in South Sudan

k <- 4.5 # Dispersion parameter k is set to 4.5 
# (Moore 2014; used range from 0.0039 to 22028; alternately, use k = 1.0 and 1.5)
cap_cases <- 1000


################
# 2 - Approach
################

set.seed(645)
runs <- 10000
seed <- 3 # Adjust the number of seed cases (3, 10, 20)
d <- 5 # Adjust the delay under investigation (5, 7, 10, 11, 14, 21, 30)

#############################
# 3 - Branching process model
#############################

## 1. Simulate outbreak trajectory for 5-day delay
dev.off()
plot(NA,xlim=c(0,60), ylim=c(0,1000),
     xlab="Delay since primary case",
     ylab="Number of cases",frame=FALSE)
cols <- sample(viridis(runs))

total_cases <- data.frame(run = 1:runs, cases = 0)

#system.time({
for(i in 1:runs) {
  cases <- seed
  t <- rep(0,seed)
  times <- t
  while(cases > 0) {
    (secondary <- rnbinom(cases,size=k,mu=R))
    t.new <- numeric()
    for(j in 1:length(secondary)) {
      t.new <- c(t.new,t[j] + rgamma(secondary[j],shape=SIshape, rate=SIrate))
    }
    # This line corrects the coding error which erroneously reset the number 
    ## of secondary cases.
    if(length(times) + length(t.new) >= cap_cases) {
      break}
    cases <- length(t.new)
    t <- t.new
    times <- c(times, t.new)
  }
  times <- times[times<=d]
  lines(sort(times), 1:length(times), col=cols[i], lwd=1)
  points(max(times), length(times), col=cols[i], pch=16)
  (total_cases[i,2] <- length(times))
}
#})

# calculate median, SD, range of simulations
print(total_cases.median <- median(total_cases$cases))
print(total_cases.sd <- sd(total_cases$cases))
print(total_cases.range <- range(total_cases$cases))
# calculate the number of outbreaks <20 cases
total_cases %>% group_by(cases < 20) %>% summarize(n = n()) %>% mutate(freq = n / sum(n)*100) 
# identify whether any runs violate the cap to make sure capping code is working
max(total_cases$cases)
which(total_cases$cases > 1000)