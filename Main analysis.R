#######################################################################
## This code replicate the analyses and figures for the following
## Submission: BMC Medicine <title: Early detection of cholera epidemics 
## to support control in fragile and conflict-affected states: estimated 
## delays and potential epidemic sizes>
## Author: Ruwan Ratnayake, October 2020
#######################################################################


####################
# 0 - Load libraries
####################

library(dplyr) 
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lmtest)
library(e1071)
library(cowplot)
library(scales)


####################
# 1 - Load dataset
####################

#User please load Cholera_outbreaks_2008_2019.xlsx 


#####################################
# TABLE 2: Descriptive statistics
#####################################

# Counts and frequencies of descriptive variables
print(Cholera_outbreaks_2008_2019 %>%
        count(Country) %>%
        mutate(freq=n/sum(n)), n=40)
print(Cholera_outbreaks_2008_2019 %>%
        count(Region) %>%
        mutate(freq=n/sum(n)))
print(Cholera_outbreaks_2008_2019 %>%
        count(Context) %>%
        mutate(freq=n/sum(n)))
print(Cholera_outbreaks_2008_2019 %>%
        count(Context2) %>%
        mutate(freq=n/sum(n)))
print(Cholera_outbreaks_2008_2019 %>%
        count(System) %>%
        mutate(freq=n/sum(n)))
print(Cholera_outbreaks_2008_2019 %>%
        count(Detection_mode) %>%
        mutate(freq=n/sum(n)))
print(Cholera_outbreaks_2008_2019 %>%
        count(Year) %>%
        mutate(freq=n/sum(n)))

summary(Cholera_outbreaks_2008_2019$Date_onset)
#51 missing date of onset


########################################
# TABLE 3: Median delays and 95% CIs
########################################

#Delay from DOS to DOP
summary(Cholera_outbreaks_2008_2019$Delay_symp_pres, na.rm=TRUE)
#Delay from DOS to DOD
summary(Cholera_outbreaks_2008_2019$Delay_symp_det, na.rm=TRUE)
#Delay from DOP to DOD
summary(Cholera_outbreaks_2008_2019$Delay_pres_det, na.rm=TRUE)
#Delay from DOS to DOI
summary(Cholera_outbreaks_2008_2019$Delay_symp_inv, na.rm=TRUE)
#Delay from DOP to DOI
summary(Cholera_outbreaks_2008_2019$Delay_pres_inv, na.rm=TRUE)
#Delay from DOS to DOR
summary(Cholera_outbreaks_2008_2019$Delay_symp_resp, na.rm=TRUE)
#Delay from DOP to DOR
summary(Cholera_outbreaks_2008_2019$Delay_pres_resp, na.rm=TRUE)
#Delay from DOS to DOC
summary(Cholera_outbreaks_2008_2019$Delay_symp_conf, na.rm=TRUE)

print(Cholera_outbreaks_2008_2019 %>%
        count(Delay_symp_resp) %>%
        mutate(freq=n/sum(n)), n=76)

################################################################################
## Annex 3: Histograms of delays from symptom onset to (A) case presentation
## (B) outbreak detection, (C) investigation, (D) response, and (E) confirmation
################################################################################

#Histograms for Annex
p1 <-ggplot(Cholera_outbreaks_2008_2019, aes(x=Delay_symp_pres)) +
  geom_histogram(binwidth=1, color="blue", fill="grey") +
  ylim (0, 10)+
  xlim (0, 100)+
  labs(y="Count", 
       x="Delay to presentation (days)")+
  theme(text=element_text(size=9))

p2 <-ggplot(Cholera_outbreaks_2008_2019, aes(x=Delay_symp_det)) +
  geom_histogram(binwidth=1, color="blue", fill="grey") +
  ylim (0, 10)+
  xlim (0, 100)+
  labs(y="Count", 
       x="Delay to detection (days)")+
  theme(text=element_text(size=9))

p3 <-ggplot(Cholera_outbreaks_2008_2019, aes(x=Delay_symp_inv)) +
  geom_histogram(binwidth=1, color="blue", fill="grey") +
  ylim (0, 10)+
  xlim (0, 100)+
  labs(y="Count", 
       x="Delay to investigation (days)")+
  theme(text=element_text(size=9))

p4 <-ggplot(Cholera_outbreaks_2008_2019, aes(x=Delay_symp_resp)) +
  geom_histogram(binwidth=1, color="blue", fill="grey") +
  ylim (0, 10)+
  xlim (0, 100)+
  labs(y="Count", 
       x="Delay to response (days)")+
  theme(text=element_text(size=9))

p5 <-ggplot(Cholera_outbreaks_2008_2019, aes(x=Delay_symp_conf)) +
  geom_histogram(binwidth=1, color="blue", fill="grey") +
  ylim (0, 10)+
  xlim (0, 100)+
  labs(y="Count", 
       x="Delay to confirmation (days)")+
  theme(text=element_text(size=9))

plot_grid(p1, p2, NULL, p3, p4, p5, labels = c('A', 'B', ' ', 'C', 'D', 'E'), label_size = 12, label_colour = "blue")


################################################################
## FIGURE 1: Graph of median delays and intervals by country
################################################################

Outbreaks <- Cholera_outbreaks_2008_2019 %>%
  select(Outbreak, Year, Region, Delay_symp_pres, Delay_symp_det, 
         Delay_symp_inv, Delay_symp_resp, Delay_symp_conf)

ggplot(Outbreaks %>% filter(!is.na(Delay_symp_resp)), 
       aes(x = reorder(Outbreak, 1-Delay_symp_resp), y = Delay_symp_resp)) +  
  geom_point(aes(y = Delay_symp_det), stat = "identity", 
             color = "dodgerblue3", size = 2.5, shape=21) +      
  geom_point(aes(y = Delay_symp_resp), stat = "identity", 
             color = "black", size = 1.5) +
  geom_point(aes(y = Delay_symp_conf), stat = "identity", 
             color = "darkred", size = 3.5, shape=21) +
  geom_segment(aes(y = 0, ##detection/alert delay
                   x = Outbreak,
                   yend = Delay_symp_det,
                   xend = Outbreak),
               alpha = 0.5, color = "black") +  
  geom_segment(aes(y = 0,  ##response delay
                   x = Outbreak,
                   yend = Delay_symp_resp,
                   xend = Outbreak),
               alpha = 0.5, color = "black") +  
  scale_y_continuous(
    breaks = c(0, 7, 14, 21, 28, 35, 42, 49, 56, 63, 70, 
               77, 84, 91, 98),
    label =  c("0", "1", "2", "3", "4", "5", "6", "7", "8", 
               "9", "10", "11", "12", "13", "14" ),
    name = "Delay to detection, response, and confirmation (weeks)",
    sec.axis = sec_axis(
      trans=~.*1,
      name = "Delay to detection, response, and confirmation (weeks)",
      breaks = c(0, 7, 14, 21, 28, 35, 42, 49, 56, 63, 70, 
                 77, 84, 91, 98),
      label =  c("0", "1", "2", "3", "4", "5", "6", "7", "8", 
                 "9", "10", "11", "12", "13", "14" ))) +
  labs(x=" ") +
  theme(
    axis.title.y = element_text(color="black", size=10),
    text = element_text(size=12), 
    panel.background = element_rect(fill = "gray95"),
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_blank()) + 
  coord_flip()


################################################################
## Factors associated with delays (multivariate analysis)
################################################################

#Transform variables into binary predictors
Cholera_outbreaks_2008_2019$Alert_bin_F <- factor(Cholera_outbreaks_2008_2019$Alert_bin)
Cholera_outbreaks_2008_2019$Context_bin_F <- factor(Cholera_outbreaks_2008_2019$Context)
Cholera_outbreaks_2008_2019$Context2_bin_F <- factor(Cholera_outbreaks_2008_2019$Context2)
Cholera_outbreaks_2008_2019$Region_bin_F <- factor(Cholera_outbreaks_2008_2019$Region)

#Model 0 for forward and backward selection 
#step(lm((log1p(Delay_symp_resp)) ~ Year + Region_bin_F + Context_bin_F + Context2_bin_F, direction="backward", data = Cholera_outbreaks_2008_2019))
#step(lm((log1p(Delay_symp_resp)) ~ Year + Region_bin_F + Context_bin_F + Context2_bin_F, direction="forward", data = Cholera_outbreaks_2008_2019))
step(lm((log1p(Delay_symp_resp)) ~ Year + Region_bin_F + Context_bin_F + Context2_bin_F, direction="both", data = Cholera_outbreaks_2008_2019))
##AIC showed year alone as having the lowest AIC score

#univariate tests
summary(lm_log.model.region <- lm(log1p(Delay_symp_resp) ~ Region_bin_F, data = Cholera_outbreaks_2008_2019))
summary(lm_log.model.context <- lm(log1p(Delay_symp_resp) ~ Context_bin_F, data = Cholera_outbreaks_2008_2019))
summary(lm_log.model.context2 <- lm(log1p(Delay_symp_resp) ~ Context2_bin_F, data = Cholera_outbreaks_2008_2019))
summary(lm_log.model.alert <- lm(log1p(Delay_symp_resp) ~ Alert_bin_F, data = Cholera_outbreaks_2008_2019))

#Model 1: Year-only
summary(model1 <- lm(Delay_symp_resp ~ Year, data = Cholera_outbreaks_2008_2019))
confint(model1)
#Test if log transformation is a better fit
summary(Cholera_outbreaks_2008_2019$Delay_symp_resp, na.rm=TRUE)
ggplot(data = Cholera_outbreaks_2008_2019, aes(Delay_symp_resp)) + geom_histogram()
skewness(Cholera_outbreaks_2008_2019$Delay_symp_resp, na.rm=TRUE)
ggplot(data = Cholera_outbreaks_2008_2019, aes(Year)) + geom_histogram()
hist(log(Cholera_outbreaks_2008_2019$Delay_symp_resp))

#Build a log model, taking into account responses of zero (use log1p)
76-(sum(is.na(Cholera_outbreaks_2008_2019$Delay_symp_resp)))
summary(lm_log.model <- lm(log1p(Delay_symp_resp) ~ Year, data = Cholera_outbreaks_2008_2019))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_log.model, las = 1)
#Exponentiate the coefficients (as unit change and % change)
(1-(exp(coef(lm_log.model)["Year"])))*100
confint(lm_log.model)
(1-(exp(-0.1010914)))*100
(1-(exp(-0.005190563)))*100

(exp(coef(lm_log.model)["Year"])-1)*100
(exp(-0.1010914)-1)*100
(exp(-0.005190563)-1)*100

##For every one-unit increase in year, delay to response 
##decreases by 5.18% (95% CI 0.5-9.6). 


###################################################################################
## FIGURE 2: Scatterplots of delays from symptom onset to (A) case presentation
## (B) outbreak detection, (C) investigation, (D) response, and (E) confirmation
###################################################################################

#Fit regression line and a non-parametric smoother (Loess curve)
Cholera_outbreaks_2008_2019$Date_onset_eff <- as.Date(Cholera_outbreaks_2008_2019$Date_onset_eff)
r <- ggplot(Cholera_outbreaks_2008_2019, aes(x = Date_onset_eff, y = Delay_symp_resp)) +
  theme(legend.position = "bottom") +
  guides(fill="tomato3") +
  labs(y="Delay to response (days)", 
       x="Year of outbreak onset",
       axis.text=element_text(size=1)) +
  ylim (-15, 90) +
  xlim (2007, 2020) +
  scale_x_date(labels = date_format("%Y"), date_breaks = "2 years") 
D <- r + geom_count(color="tomato3", show.legend=F, alpha=1/8, size=4) +
  geom_smooth(span=0.5, SE=T, 
              data=Cholera_outbreaks_2008_2019,
              mapping = aes(x=Date_onset_eff, y=Delay_symp_resp))
  
p <- ggplot(Cholera_outbreaks_2008_2019, aes(x = Date_onset_eff, y = Delay_symp_pres,
                                       lty = 'Loess regression and 95% CI')) +
  theme(legend.position = "bottom") +
  guides(fill="tomato3") +
  labs(y="Delay to presentation (days)", 
       axis.text=element_text(size=1)) +
  theme(axis.title.x=element_blank())+
  ylim (0, 50) +
  xlim (2007, 2020) +
  scale_x_date(labels = date_format("%Y"), date_breaks = "2 years") 
A <- p + geom_count(color="tomato3", show.legend=F, alpha=1/8, size=4) +
  geom_smooth(span=0.5, SE=T, 
              data=Cholera_outbreaks_2008_2019,
              mapping = aes(x=Date_onset_eff, y=Delay_symp_pres))

d <- ggplot(Cholera_outbreaks_2008_2019, aes(x = Date_onset_eff, y = Delay_symp_det,
                                       lty = 'Loess regression and 95% CI')) +
  theme(legend.position = "bottom") +
  guides(fill="tomato3") +
  labs(y="Delay to detection (days)", 
       axis.text=element_text(size=1)) +
  theme(axis.title.x=element_blank())+
  ylim (0, 50) +
  xlim (2007, 2020) +
  scale_x_date(labels = date_format("%Y"), date_breaks = "2 years") 
B <- d + geom_count(color="tomato3", show.legend=F, alpha=1/8, size=4) +
  geom_smooth(span=0.5, SE=T, 
              data=Cholera_outbreaks_2008_2019,
              mapping = aes(x=Date_onset_eff, y=Delay_symp_det))

i <- ggplot(Cholera_outbreaks_2008_2019, aes(x = Date_onset_eff, y = Delay_symp_inv)) +
  theme(legend.position = "bottom") +
  guides(fill="tomato3") +
  labs(y="Delay to investigation (days)", 
       x="Year of outbreak onset",
       axis.text=element_text(size=1)) +
  ylim (-15, 90) +
  xlim (2007, 2020) +
  scale_x_date(labels = date_format("%Y"), date_breaks = "2 years") 
C <- i + geom_count(color="tomato3", show.legend=F, alpha=1/8, size=4) +
  geom_smooth(span=0.5, SE=T, 
              data=Cholera_outbreaks_2008_2019,
              mapping = aes(x=Date_onset_eff, y=Delay_symp_inv))

c <- ggplot(Cholera_outbreaks_2008_2019, aes(x = Date_onset_eff, y = Delay_symp_conf)) +
  theme(legend.position = "bottom") +
  guides(fill="tomato3") +
  labs(y="Delay to confimation (days)", 
       x="Year of outbreak onset",
       axis.text=element_text(size=1)) +
  ylim (-15, 90) +
  xlim (2007, 2020) +
  scale_x_date(labels = date_format("%Y"), date_breaks = "2 years") 
E <- c + geom_count(color="tomato3", show.legend=F, alpha=1/8, size=4) +
  geom_smooth(span=0.5, SE=T, 
              data=Cholera_outbreaks_2008_2019,
              mapping = aes(x=Date_onset_eff, y=Delay_symp_conf))

plot_grid(A + theme(legend.position="top"), B + theme(legend.position="top"), 
          NULL , C + theme(legend.position="none"), D + theme(legend.position="none"),
          E + theme(legend.position="none"), 
          labels = c('A', 'B', ' ', 'C', 'D', 'E'), 
          label_size = 12, label_colour = "blue", align = "vh")


###################################################
## FIGURE 3: Boxplot for alert vs data analysis
###################################################

clean<- na.omit(subset(Cholera_outbreaks_2008_2019, select = c(Alert, Delay_symp_resp)))
g <- ggplot(clean, aes(Alert, Delay_symp_resp))
g + geom_boxplot(width=0.5/length(unique(Cholera_outbreaks_2008_2019$Alert)), 
                 fill="cornflowerblue", alpha=0.2)+
  theme(
    axis.line.y  = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size=30),
    panel.background = element_rect(fill = "transparent"),
    panel.border = element_rect(colour = "black", fill=NA, size=.1),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank()) +
  ylim (0, 40) +
  ylab("Delay to response (days)") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .3,
               shape=11,
               alpha=0.6,
               colour="grey")


########################################################
## ANNEX 4: Change in time to event for other delays
########################################################

#Check skewness of response variables
skewness(Cholera_outbreaks_2008_2019$Delay_symp_pres, na.rm=TRUE)
hist(log(Cholera_outbreaks_2008_2019$Delay_symp_pres))
skewness(Cholera_outbreaks_2008_2019$Delay_symp_det, na.rm=TRUE)
hist(log(Cholera_outbreaks_2008_2019$Delay_symp_det))
skewness(Cholera_outbreaks_2008_2019$Delay_symp_inv, na.rm=TRUE)
hist(log(Cholera_outbreaks_2008_2019$Delay_symp_inv))
skewness(Cholera_outbreaks_2008_2019$Delay_symp_conf, na.rm=TRUE)
hist(log(Cholera_outbreaks_2008_2019$Delay_symp_conf))

#Model summaries

#DOS to DOP
76-(sum(is.na(Cholera_outbreaks_2008_2019$Delay_symp_pres)))
summary(lm_log.model_pres <- lm(log1p(Delay_symp_pres) ~ Year, data = Cholera_outbreaks_2008_2019))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_log.model_pres, las = 1)
(1-(exp(coef(lm_log.model_pres)["Year"])))*100
confint(lm_log.model_pres)
(1-exp(-0.05551829))*100
(1-exp(0.01779795))*100

(exp(coef(lm_log.model_pres)["Year"])-1)*100
(exp(-0.05551829)-1)*100
(exp(0.01779795)-1)*100

#DOS to DOD
76-(sum(is.na(Cholera_outbreaks_2008_2019$Delay_symp_det)))
summary(lm_log.model_det <- lm(log1p(Delay_symp_det) ~ Year, data = Cholera_outbreaks_2008_2019))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_log.model_det, las = 1)
(1-(exp(coef(lm_log.model_det)["Year"])))*100
confint(lm_log.model_det)
(1-exp(-0.08846064))*100
(1-exp(-0.006914679))*100

(exp(coef(lm_log.model_det)["Year"])-1)*100
(exp(-0.08846064)-1)*100
(exp(-0.006914679)-1)*100

#DOS to DOI
76-(sum(is.na(Cholera_outbreaks_2008_2019$Delay_symp_inv)))
summary(lm_log.model_inv <- lm(log1p(Delay_symp_inv) ~ Year, data = Cholera_outbreaks_2008_2019))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_log.model_inv, las = 1)
(1-(exp(coef(lm_log.model_inv)["Year"])))*100
confint(lm_log.model_inv)
(1-exp(-0.1729821))*100
(1-exp(-0.02498092))*100

(exp(coef(lm_log.model_inv)["Year"])-1)*100
(exp(-0.1729821)-1)*100
(exp(-0.02498092)-1)*100

#DOS to DOC
76-(sum(is.na(Cholera_outbreaks_2008_2019$Delay_symp_conf)))
summary(lm_log.model_conf <- lm(log1p(Delay_symp_conf) ~ Year, data = Cholera_outbreaks_2008_2019))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_log.model_conf, las = 1)
(1-(exp(coef(lm_log.model_conf)["Year"])))*100
confint(lm_log.model_conf)
(1-exp(-0.1755834))*100
(1-exp(-0.0256475))*100

(exp(coef(lm_log.model_conf)["Year"])-1)*100
(exp(-0.1755834)-1)*100
(exp(-0.0256475)-1)*100



#ANNEX 4: Graphs

#Delay from onset of symptoms to presentation 
g <- ggplot(Cholera_outbreaks_2008_2019, aes(Cholera_outbreaks_2008_2019$Date_onset_eff, Cholera_outbreaks_2008_2019$Delay_symp_pres))
g + geom_count(col="tomato3", show.legend=F, alpha=1/8, size=6) +
  labs(y="Delay (onset to presentation)", 
       x="Start date of outbreak",
       axis.text=element_text(size=0.5)) +
  geom_smooth(span=0.5, SE=FALSE,
              data=Cholera_outbreaks_2008_2019,
              mapping = aes(x=Date_onset_eff, y=Delay_symp_pres))

#Delay from onset of symptoms to detection 
g <- ggplot(Cholera_outbreaks_2008_2019, aes(Cholera_outbreaks_2008_2019$Date_onset_eff, Cholera_outbreaks_2008_2019$Delay_symp_det))
g + geom_count(col="tomato3", show.legend=F, alpha=1/8, size=6) +
  labs(y="Delay (onset to detection)", 
       x="Start date of outbreak",
       axis.text=element_text(size=0.5)) +
  geom_smooth(span=0.5, SE=FALSE,
              data=Cholera_outbreaks_2008_2019,
              mapping = aes(x=Date_onset_eff, y=Delay_symp_det))

#Delay from onset of symptoms to investigation 
g <- ggplot(Cholera_outbreaks_2008_2019, aes(Cholera_outbreaks_2008_2019$Date_onset_eff, Cholera_outbreaks_2008_2019$Delay_symp_inv))
g + geom_count(col="tomato3", show.legend=F, alpha=1/8, size=6) +
  labs(y="Delay (onset to investigation)", 
       x="Start date of outbreak",
       axis.text=element_text(size=0.5)) +
  geom_smooth(span=0.5, SE=FALSE,
              data=Cholera_outbreaks_2008_2019,
              mapping = aes(x=Date_onset_eff, y=Delay_symp_inv))

#Delay from onset of symptoms to response 
g <- ggplot(Cholera_outbreaks_2008_2019, aes(Cholera_outbreaks_2008_2019$Date_onset_eff, Cholera_outbreaks_2008_2019$Delay_symp_resp))
g + geom_count(col="tomato3", show.legend=F, alpha=1/8, size=6) +
  labs(y="Delay (onset to response)", 
       x="Start date of outbreak",
       axis.text=element_text(size=0.5)) +
  geom_smooth(span=0.5, SE=FALSE,
              data=Cholera_outbreaks_2008_2019,
              mapping = aes(x=Date_onset_eff, y=Delay_symp_resp))

#Delay from onset of symptoms to confirmation 
g <- ggplot(Cholera_outbreaks_2008_2019, aes(Cholera_outbreaks_2008_2019$Date_onset_eff, Cholera_outbreaks_2008_2019$Delay_symp_conf))
g + geom_count(col="tomato3", show.legend=F, alpha=1/8, size=6) +
  labs(y="Delay (onset to confirmation)", 
       x="Start date of outbreak",
       axis.text=element_text(size=0.5)) +
  geom_smooth(span=0.5, SE=FALSE,
            data=Cholera_outbreaks_2008_2019,
            mapping = aes(x=Date_onset_eff, y=Delay_symp_conf))

#Model 2: Alert-only
#Number of observations with alert information
76-(sum(is.na(Cholera_outbreaks_2008_2019$Alert)))
summary(model2 <- lm(log1p(Delay_symp_resp) ~ Alert_bin_F, data = Cholera_outbreaks_2008_2019))
confint(model2)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(model2, las = 1)
(1-(exp(coef(model2))))*100
confint(model2)
(1-(exp(-0.9413772)))*100
(1-(exp(-0.05841244)))*100

(exp(coef(model2))-1)*100
(exp(-0.9413772)-1)*100
(exp(-0.05841244)-1)*100

##For every one-unit increase in alert-status, delay to response 
##decreases by 39.3% (95% CI 5.7-61.0).


