# This is code to replicate the analyses and figures from my July 2020
# submission to BMC Medicine entitled Early detection of cholera 
# epidemics to support control in fragile and conflict-affected states: 
# estimated delays and potential impacts.
# Code developed by Ruwan Ratnayake.

library(dplyr) 
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(viridis)
library(lmtest)
library(broom)
library(e1071)
library(cowplot)

#read input
#input_file <- "Timeliness_2020_07_02" 

# 1. TABLE 2: Descriptive statistics
# Counts and frequencies of descriptive variables
print(Timeliness_2020_07_02 %>%
        count(Country) %>%
        mutate(freq=n/sum(n)), n=40)
print(Timeliness_2020_07_02 %>%
        count(Region) %>%
        mutate(freq=n/sum(n)))
print(Timeliness_2020_07_02 %>%
        count(Context) %>%
        mutate(freq=n/sum(n)))
print(Timeliness_2020_07_02 %>%
        count(Context2) %>%
        mutate(freq=n/sum(n)))
print(Timeliness_2020_07_02 %>%
        count(System) %>%
        mutate(freq=n/sum(n)))
print(Timeliness_2020_07_02 %>%
        count(Detection_mode) %>%
        mutate(freq=n/sum(n)))
#print(Timeliness_2020_07_02 %>%
#        count(Year) %>%
#        mutate(freq=n/sum(n)))

# 2. TABLE 3: Median delays and 95% CIs
#Delay from DOS to DOP
summary(Timeliness_2020_07_02$Delay_symp_pres, na.rm=TRUE)
#Delay from DOS to DOD
summary(Timeliness_2020_07_02$Delay_symp_det, na.rm=TRUE)
#Delay from DOP to DOD
summary(Timeliness_2020_07_02$Delay_pres_det, na.rm=TRUE)
#Delay from DOS to DOI
summary(Timeliness_2020_07_02$Delay_symp_inv, na.rm=TRUE)
#Delay from DOP to DOI
summary(Timeliness_2020_07_02$Delay_pres_inv, na.rm=TRUE)
#Delay from DOS to DOR
summary(Timeliness_2020_07_02$Delay_symp_resp, na.rm=TRUE)
#Delay from DOP to DOR
summary(Timeliness_2020_07_02$Delay_pres_resp, na.rm=TRUE)
#Delay from DOS to DOC
summary(Timeliness_2020_07_02$Delay_symp_conf, na.rm=TRUE)

#Histograms for Annex
p1 <-ggplot(Timeliness_2020_07_02, aes(x=Delay_symp_pres)) +
  geom_histogram(binwidth=1, color="blue", fill="grey") +
  ylim (0, 10)+
  xlim (0, 100)+
  labs(y="Count", 
       x="Delay to presentation (days)")+
  theme(text=element_text(size=9))

p2 <-ggplot(Timeliness_2020_07_02, aes(x=Delay_symp_det)) +
  geom_histogram(binwidth=1, color="blue", fill="grey") +
  ylim (0, 10)+
  xlim (0, 100)+
  labs(y="Count", 
       x="Delay to detection (days)")+
  theme(text=element_text(size=9))

p3 <-ggplot(Timeliness_2020_07_02, aes(x=Delay_symp_inv)) +
  geom_histogram(binwidth=1, color="blue", fill="grey") +
  ylim (0, 10)+
  xlim (0, 100)+
  labs(y="Count", 
       x="Delay to investigation (days)")+
  theme(text=element_text(size=9))

p4 <-ggplot(Timeliness_2020_07_02, aes(x=Delay_symp_resp)) +
  geom_histogram(binwidth=1, color="blue", fill="grey") +
  ylim (0, 10)+
  xlim (0, 100)+
  labs(y="Count", 
       x="Delay to response (days)")+
  theme(text=element_text(size=9))

p5 <-ggplot(Timeliness_2020_07_02, aes(x=Delay_symp_conf)) +
  geom_histogram(binwidth=1, color="blue", fill="grey") +
  ylim (0, 10)+
  xlim (0, 100)+
  labs(y="Count", 
       x="Delay to confirmation (days)")+
  theme(text=element_text(size=9))

plot_grid(p1, p2, NULL, p3, p4, p5, labels = c('A', 'B', ' ', 'C', 'D', 'E'), label_size = 12, label_colour = "blue")


#3. Branching model (Table 3)
#See Branching model estimation.R


#4. FIGURE 1: Graph of median delays and intervals by country
## DOS-->DOP-->DOI-->DOR-->DOC
cc6 <- Timeliness_2020_07_02 %>%
  select(Outbreak, Year, Delay_symp_pres, Delay_symp_det, Delay_symp_inv, Delay_symp_resp, Delay_symp_conf) %>%
  group_by(Outbreak) %>%
  summarise(Delay_symp_pres_grouped = mean(Delay_symp_pres),
            Delay_symp_det_grouped = mean(Delay_symp_det),
            Delay_symp_inv_grouped = mean(Delay_symp_inv),
            Delay_symp_resp_grouped = mean(Delay_symp_resp),
            Delay_symp_conf_grouped = mean(Delay_symp_conf))

ggplot(cc6, aes(x = reorder(Outbreak, 1-Delay_symp_resp_grouped), y = Delay_symp_resp_grouped, label = Delay_symp_resp_grouped)) +  
  geom_point(aes(y = Delay_symp_conf_grouped), stat = "identity", color = "darkorange2", size = 1) +
  geom_point(aes(y = Delay_symp_inv_grouped), stat = "identity", color = "tan2", size = 2.5, shape = 21) +
  geom_point(aes(y = Delay_symp_det_grouped), stat = "identity", color = "dodgerblue3", size = 2.5, shape = 21) +      
  geom_point(aes(y = Delay_symp_pres_grouped), stat = "identity", color = "firebrick", size = 1) +
  geom_point(stat = "identity", color = "black", size = 1) +
  geom_segment(aes(y = 0, ##investigation delay
                   x = Outbreak,
                   yend = Delay_symp_inv_grouped,
                   xend = Outbreak),
               alpha = 0.5, color = "tan2")+ 
  geom_segment(aes(y = 0, ##detection/alert delay
                   x = Outbreak,
                   yend = Delay_symp_det_grouped,
                   xend = Outbreak),
               alpha = 0.5, color = "dodgerblue3") + #this function creates the lines leading to points, black
  geom_segment(aes(y = 0, ##case presentation delay
                   x = Outbreak,
                   yend = Delay_symp_pres_grouped,
                   xend = Outbreak),
               alpha = 0.5, color = "firebrick") + #this function creates the lines leading to points, black
  geom_segment(aes(y = 0,  ##response delay
                   x = Outbreak,
                   yend = Delay_symp_resp_grouped,
                   xend = Outbreak),
               alpha = 0.5, color = "grey") + #this function creates the lines leading to points, black
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text = element_text(size=7), 
        panel.background = element_rect(fill = 'white'))+
  labs(x = " ",
       size=3,
       axis.title.y=element_blank()) +
  coord_flip()


#5. Factors associated with delays (multivariate analysis)

#Transform variables into binary predictors
Timeliness_2020_07_02$Alert_bin_F <- factor(Timeliness_2020_07_02$Alert_bin)
Timeliness_2020_07_02$Context_bin_F <- factor(Timeliness_2020_07_02$Context)
Timeliness_2020_07_02$Context2_bin_F <- factor(Timeliness_2020_07_02$Context2)
Timeliness_2020_07_02$Region_bin_F <- factor(Timeliness_2020_07_02$Region)

#Model 0 for forward and backward selection 
#step(lm((log1p(Delay_symp_resp)) ~ Year + Region_bin_F + Context_bin_F + Context2_bin_F, direction="backward", data = Timeliness_2020_07_02))
#step(lm((log1p(Delay_symp_resp)) ~ Year + Region_bin_F + Context_bin_F + Context2_bin_F, direction="forward", data = Timeliness_2020_07_02))
step(lm((log1p(Delay_symp_resp)) ~ Year + Region_bin_F + Context_bin_F + Context2_bin_F, direction="both", data = Timeliness_2020_07_02))
##AIC showed year alone as having the lowest AIC score

#univariate tests
summary(lm_log.model.region <- lm(log1p(Delay_symp_resp) ~ Region_bin_F, data = Timeliness_2020_07_02))
summary(lm_log.model.context <- lm(log1p(Delay_symp_resp) ~ Context_bin_F, data = Timeliness_2020_07_02))
summary(lm_log.model.context2 <- lm(log1p(Delay_symp_resp) ~ Context2_bin_F, data = Timeliness_2020_07_02))
summary(lm_log.model.alert <- lm(log1p(Delay_symp_resp) ~ Alert_bin_F, data = Timeliness_2020_07_02))

#Model 1: Year-only
summary(model1 <- lm(Delay_symp_resp ~ Year, data = Timeliness_2020_07_02))
confint(model1)
#Test if log transformation is a better fit
summary(Timeliness_2020_07_02$Delay_symp_resp, na.rm=TRUE)
ggplot(data = Timeliness_2020_07_02, aes(Delay_symp_resp)) + geom_histogram()
skewness(Timeliness_2020_07_02$Delay_symp_resp, na.rm=TRUE)
ggplot(data = Timeliness_2020_07_02, aes(Year)) + geom_histogram()
##Right-skewed (mean (15.56)>median (10))
##Skewness of response variable is 2.78 (highly-skewed)
hist(log(Timeliness_2020_07_02$Delay_symp_resp))
##Shows a more symmetrical distribution

#Build a log model, taking into account responses of zero (use log1p)
summary(lm_log.model <- lm(log1p(Delay_symp_resp) ~ Year, data = Timeliness_2020_07_02))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_log.model, las = 1)
#Exponentiate the coefficients (as unit change and % change)
(1-(exp(coef(lm_log.model)["Year"])))*100
confint(lm_log.model)
(1-(exp(-0.102142)))*100
(1-(exp(-0.00642831)))*100

##For every one-unit increase in year, delay to response 
##decreases by 5.3% (95% CI 0.6-9.7). 
##This equates to 58.3% over 11 years (2008-2019)

##6. FIGURE 2: Scatterplot of DOS to DOR over time 
#Fit regression line and a non-parametric smoother (Loess curve)
g <- ggplot(Timeliness_2020_07_02, aes(Timeliness_2020_07_02$Date_onset_eff, Timeliness_2020_07_02$Delay_symp_resp))
g + geom_count(col="tomato3", show.legend=F, alpha=1/8, size=6) +
  labs(y="Delay (onset to response)", 
       x="Start date of outbreak",
       axis.text=element_text(size=0.5)) +
  #geom_smooth(method="lm", SE=FALSE, colour="black", formula=y~x,
  # data=Timeliness_2020_07_02,
  # mapping = aes(x=Date_onset_eff, y=Delay_symp_resp))+
  geom_smooth(span=0.5, SE=FALSE,
              data=Timeliness_2020_07_02,
              mapping = aes(x=Date_onset_eff, y=Delay_symp_resp))


#7. ANNEX 4: Change in time to event for other delays
#Check skewness of response variables
skewness(Timeliness_2020_07_02$Delay_symp_pres, na.rm=TRUE)
##Skewness of response variable is 2.62 (highly-skewed)
hist(log(Timeliness_2020_07_02$Delay_symp_pres))
skewness(Timeliness_2020_07_02$Delay_symp_det, na.rm=TRUE)
##Skewness of response variable is 2.35 (highly-skewed)
hist(log(Timeliness_2020_07_02$Delay_symp_det))
skewness(Timeliness_2020_07_02$Delay_symp_inv, na.rm=TRUE)
##Skewness of response variable is 2.69 (highly-skewed)
hist(log(Timeliness_2020_07_02$Delay_symp_inv))
skewness(Timeliness_2020_07_02$Delay_symp_conf, na.rm=TRUE)
##Skewness of response variable is 2.4 (highly-skewed)
hist(log(Timeliness_2020_07_02$Delay_symp_conf))

#Model summaries

#DOS to DOP
summary(lm_log.model_pres <- lm(log1p(Delay_symp_pres) ~ Year, data = Timeliness_2020_07_02))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_log.model_pres, las = 1)
(1-(exp(coef(lm_log.model_pres)["Year"])))*100
confint(lm_log.model_pres)
(1-exp(-0.0536842))*100
(1-exp(0.01792588))*100

#DOS to DOD
summary(lm_log.model_det <- lm(log1p(Delay_symp_det) ~ Year, data = Timeliness_2020_07_02))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_log.model_det, las = 1)
(1-(exp(coef(lm_log.model_det)["Year"])))*100
confint(lm_log.model_det)
(1-exp(-0.08701538))*100
(1-exp(-0.007503247))*100

#DOS to DOI
summary(lm_log.model_inv <- lm(log1p(Delay_symp_inv) ~ Year, data = Timeliness_2020_07_02))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_log.model_inv, las = 1)
(1-(exp(coef(lm_log.model_inv)["Year"])))*100
confint(lm_log.model_inv)
(1-exp(-0.1732231))*100
(1-exp(-0.03040217))*100

#DOS to DOC
summary(lm_log.model_conf <- lm(log1p(Delay_symp_conf) ~ Year, data = Timeliness_2020_07_02))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_log.model_conf, las = 1)
(1-(exp(coef(lm_log.model_conf)["Year"])))*100
confint(lm_log.model_conf)
(1-exp(-0.1755834))*100
(1-exp(-0.0256475))*100

#ANNEX 4: Graphs

#Delay from onset of symptoms to presentation 
g <- ggplot(Timeliness_2020_07_02, aes(Timeliness_2020_07_02$Date_onset_eff, Timeliness_2020_07_02$Delay_symp_pres))
g + geom_count(col="tomato3", show.legend=F, alpha=1/8, size=6) +
  labs(y="Delay (onset to presentation)", 
       x="Start date of outbreak",
       axis.text=element_text(size=0.5)) +
  geom_smooth(span=0.5, SE=FALSE,
              data=Timeliness_2020_07_02,
              mapping = aes(x=Date_onset_eff, y=Delay_symp_pres))

#Delay from onset of symptoms to detection 
g <- ggplot(Timeliness_2020_07_02, aes(Timeliness_2020_07_02$Date_onset_eff, Timeliness_2020_07_02$Delay_symp_det))
g + geom_count(col="tomato3", show.legend=F, alpha=1/8, size=6) +
  labs(y="Delay (onset to detection)", 
       x="Start date of outbreak",
       axis.text=element_text(size=0.5)) +
  geom_smooth(span=0.5, SE=FALSE,
              data=Timeliness_2020_07_02,
              mapping = aes(x=Date_onset_eff, y=Delay_symp_det))

#Delay from onset of symptoms to investigation 
g <- ggplot(Timeliness_2020_07_02, aes(Timeliness_2020_07_02$Date_onset_eff, Timeliness_2020_07_02$Delay_symp_inv))
g + geom_count(col="tomato3", show.legend=F, alpha=1/8, size=6) +
  labs(y="Delay (onset to investigation)", 
       x="Start date of outbreak",
       axis.text=element_text(size=0.5)) +
  geom_smooth(span=0.5, SE=FALSE,
              data=Timeliness_2020_07_02,
              mapping = aes(x=Date_onset_eff, y=Delay_symp_inv))

#Delay from onset of symptoms to response 
g <- ggplot(Timeliness_2020_07_02, aes(Timeliness_2020_07_02$Date_onset_eff, Timeliness_2020_07_02$Delay_symp_resp))
g + geom_count(col="tomato3", show.legend=F, alpha=1/8, size=6) +
  labs(y="Delay (onset to response)", 
       x="Start date of outbreak",
       axis.text=element_text(size=0.5)) +
  geom_smooth(span=0.5, SE=FALSE,
              data=Timeliness_2020_07_02,
              mapping = aes(x=Date_onset_eff, y=Delay_symp_resp))

#Delay from onset of symptoms to confirmation 
g <- ggplot(Timeliness_2020_07_02, aes(Timeliness_2020_07_02$Date_onset_eff, Timeliness_2020_07_02$Delay_symp_conf))
g + geom_count(col="tomato3", show.legend=F, alpha=1/8, size=6) +
  labs(y="Delay (onset to confirmation)", 
       x="Start date of outbreak",
       axis.text=element_text(size=0.5)) +
  geom_smooth(span=0.5, SE=FALSE,
            data=Timeliness_2020_07_02,
            mapping = aes(x=Date_onset_eff, y=Delay_symp_conf))



#8. Model 2: Alert-only
summary(model2 <- lm(log1p(Delay_symp_resp) ~ Alert_bin_F, data = Timeliness_2020_07_02))
confint(model2)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(model2, las = 1)
(1-(exp(-0.5148)))*100
confint(model2)
(1-(exp(-0.9551606)))*100
(1-(exp(-0.0744075)))*100
##For every one-unit increase in alert-status, delay to response 
##decreases by 40.2% (95% CI 7.2-61.5). 


#9. FIGURE 3: Boxplot for alert
clean<- na.omit(subset(Timeliness_2020_07_02, select = c(Alert, Delay_symp_resp)))
g <- ggplot(clean, aes(Alert, Delay_symp_resp))
g + geom_boxplot()+
  ylim (0, 50) +
  ylab("Delay to response")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="blue")+
  theme(axis.title.x = element_blank(),
        panel.background = element_rect(fill = 'grey90'))