##Load packages
library(dplyr) 
library(tidyverse)
library(boot)
library(rcompanion)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(viridis)

## 1. Descriptive statistics
# Counts and frequencies of descriptive variables

print(Timeliness_2020_05_12 %>%
              count(Country) %>%
              mutate(freq=n/sum(n)), n=40)
print(Timeliness_2020_05_12 %>%
        count(Year) %>%
        mutate(freq=n/sum(n)))
print(Timeliness_2020_05_12 %>%
        count(New) %>%
        mutate(freq=n/sum(n)))
print(Timeliness_2020_05_12 %>%
        count(Region) %>%
        mutate(freq=n/sum(n)))
print(Timeliness_2020_05_12 %>%
        count(Context) %>%
        mutate(freq=n/sum(n)))
print(Timeliness_2020_05_12 %>%
        count(Detection_mode) %>%
        mutate(freq=n/sum(n)))
print(Timeliness_2020_05_12 %>%
        count(System) %>%
        mutate(freq=n/sum(n)))
print(Timeliness_2020_05_12 %>%
        count(Source) %>%
        mutate(freq=n/sum(n)))
print(Timeliness_2020_05_12 %>%
        count(Signal) %>%
        mutate(freq=n/sum(n)))


## 2. Calculation of median delays and summary statistics
# Delay to health seeking behaviour
(median_symp_pres = median(Timeliness_2020_05_12$Delay_symp_pres, na.rm=TRUE))
# Delay to response (from DOS and DOP) 
(median_symp_resp = median(Timeliness_2020_05_12$Delay_symp_resp, na.rm=TRUE))
(median_pres_resp = median(Timeliness_2020_05_12$Delay_pres_resp, na.rm=TRUE))
# Delay to investigation (from DOS and DOP) 
(median_symp_inv = median(Timeliness_2020_05_12$Delay_symp_inv, na.rm=TRUE))
(median_pres_inv = median(Timeliness_2020_05_12$Delay_pres_inv, na.rm=TRUE))
# Delay to laboratory confirmation (from DOS only) 
(median_symp_conf = median(Timeliness_2020_05_12$Delay_symp_conf, na.rm=TRUE))
#Calculate median delays, mean, IQR, NAs
summary(Timeliness_2020_05_12$Delay_symp_pres, na.rm=TRUE)
summary(Timeliness_2020_05_12$Delay_symp_resp, na.rm=TRUE)
summary(Timeliness_2020_05_12$Delay_pres_resp, na.rm=TRUE)
summary(Timeliness_2020_05_12$Delay_symp_inv, na.rm=TRUE)
summary(Timeliness_2020_05_12$Delay_pres_inv, na.rm=TRUE)
summary(Timeliness_2020_05_12$Delay_symp_conf, na.rm=TRUE)

## 4. Calculate bootstraped 95% confidence intervals
# DOS-DOP
Boot_symp_pres = boot(Timeliness_2020_05_12$Delay_symp_pres,
                      function(x,i) median(x[i], na.rm=TRUE),
                      R=10000)
boot.ci(Boot_symp_pres,
        conf = 0.95,
        type = c("norm", "basic" ,"perc", "bca")
)

# DOS-RESP
Boot_symp_resp = boot(Timeliness_2020_05_12$Delay_symp_resp,
                      function(x,i) median(x[i], na.rm=TRUE),
                      R=10000)
boot.ci(Boot_symp_resp,
        conf = 0.95,
        type = c("norm", "basic" ,"perc", "bca")
)

# DOP-RESP
Boot_pres_resp = boot(Timeliness_2020_05_12$Delay_pres_resp,
                      function(x,i) median(x[i], na.rm=TRUE),
                      R=10000)
boot.ci(Boot_pres_resp,
        conf = 0.95,
        type = c("norm", "basic" ,"perc", "bca")
)

# DOS-INV
Boot_symp_inv = boot(Timeliness_2020_05_12$Delay_symp_inv,
                     function(x,i) median(x[i], na.rm=TRUE),
                     R=10000)
boot.ci(Boot_symp_inv,
        conf = 0.95,
        type = c("norm", "basic" ,"perc", "bca")
)

# PRES-INV
Boot_pres_inv = boot(Timeliness_2020_05_12$Delay_pres_inv,
                     function(x,i) median(x[i], na.rm=TRUE),
                     R=10000)
boot.ci(Boot_pres_inv,
        conf = 0.95,
        type = c("norm", "basic" ,"perc", "bca")
)

# SYMP-CONF
Boot_symp_conf = boot(Timeliness_2020_05_12$Delay_symp_conf,
                      function(x,i) median(x[i], na.rm=TRUE),
                      R=10000)
boot.ci(Boot_symp_conf,
        conf = 0.95,
        type = c("norm", "basic" ,"perc", "bca")
)

## 5. Graph (lollipop chart) of median delays and intervals by country for
## DOS-->DOP-->DOI-->DOR-->DOC

cc6 <- Timeliness_2020_05_12 %>%
        select(Country, Year, Delay_symp_pres, Delay_symp_inv, Delay_symp_resp, Delay_symp_conf) %>%
        group_by(Country) %>%
        summarise(Delay_symp_pres_grouped = mean(Delay_symp_pres),
                  Delay_symp_inv_grouped = mean(Delay_symp_inv),
                  Delay_symp_resp_grouped = mean(Delay_symp_resp),
                  Delay_symp_conf_grouped = mean(Delay_symp_conf))

ggplot(cc6, aes(x = reorder(Country, Delay_symp_resp_grouped), y = Delay_symp_resp_grouped, label = Delay_symp_resp_grouped)) +  
        geom_point(aes(y = Delay_symp_conf_grouped), stat = "identity", color = "darkgoldenrod", size = 2) +
        geom_point(aes(y = Delay_symp_inv_grouped), stat = "identity", color = "darkgreen", size = 2) +
        geom_point(aes(y = Delay_symp_pres_grouped), stat = "identity", color = "firebrick", size = 2) +
        geom_point(stat = "identity", fill = "black", size = 2) +
        geom_segment(aes(y = 0,  ##response delay
                         x = Country,
                         yend = Delay_symp_resp_grouped,
                         xend = Country))+ #this function creates the lines leading to points, black
        geom_segment(aes(y = 0, ##investigation delay
                         x = Country,
                         yend = Delay_symp_inv_grouped,
                         xend = Country),
                     alpha = 0.5, color = "darkgreen")+ 
        geom_segment(aes(y = 0, ##case presentation delay
                         x = Country,
                         yend = Delay_symp_pres_grouped,
                         xend = Country),
                     alpha = 0.5, color = "firebrick") + #this function creates the lines leading to points, black
        theme(text = element_text(size=12), panel.background = element_rect(fill = 'grey'))+
        labs(y = "Delay from symptom onset to case presentation (red), investigation (green), response (black), and confirmation (yellow)", 
             x = " ",
             size=5)+
        coord_flip()


## 6. Graph [boxplot with dots] of delays by alert status; by context 

##Boxplot for alert
g <- ggplot(Timeliness_2020_05_12, aes(Alert, Delay_symp_resp))
g + geom_boxplot()+
        ylim (0, 60) +
        scale_fill_viridis(discrete = TRUE, alpha=0.6) +
        geom_dotplot(binaxis='y', 
                     stackdir='center', 
                     dotsize = .5, 
                     fill="red") +
        theme_ipsum() +
        theme(
                legend.position="none",
                plot.title = element_text(size=11)
        ) +
        theme(axis.text=element_text(size=14),
              axis.title.x=element_blank(),
              axis.title=element_text(size=14,face="bold"),
              panel.background = element_rect(fill = 'grey')) + 
        labs(y="Delay (symptom onset to response)")  

##Boxplot for context
c <- ggplot(Timeliness_2020_05_12, aes(Context, Delay_symp_resp))
c + geom_boxplot()+
        ylim (0, 60) +
        scale_fill_viridis(discrete = TRUE, alpha=0.6) +
        geom_dotplot(binaxis='y', 
                     stackdir='center', 
                     dotsize = .5, 
                     fill="red") +
        theme_ipsum() +
        theme(
                legend.position="none",
                plot.title = element_text(size=11)
        ) +
        theme(axis.text=element_text(size=14),
              axis.title.x=element_blank(),
              axis.title=element_text(size=14,face="bold"),
              panel.background = element_rect(fill = 'grey')) + 
        labs(y="Delay (symptom onset to response)")  


## 7. Modelling with alert and context as predictors

##Linear regression to examine predictors of delays 
## First predictor: alert or non-alert
## Showed alert status is a predictor vs data analysis
model <- lm(Delay_symp_resp ~ Alert_bin, data = Timeliness_2020_05_12, na.rm=TRUE)
model
summary(model)
confint(model)
ggplot(Timeliness_2020_05_12, aes(Alert_bin, Delay_symp_resp)) +
        geom_point() +
        stat_smooth(method = lm)

## Second predictor: context (urban, rural, camp)
## Did not show any significant differences between contexts
Timeliness_2020_05_12$Context_bin_F <- factor(Timeliness_2020_05_12$Context_bin)
summary(Timeliness_2020_05_12$Context_bin_F)
model2 <- lm(Delay_symp_resp ~ Context_bin_F, data = Timeliness_2020_05_12)
model2
summary(model2)
confint(model2)
ggplot(Timeliness_2020_05_12, aes(Context_bin, Delay_symp_resp)) +
        geom_point() +
        stat_smooth(method = lm)

## Fourth predictor: regions
model4 <- lm(Delay_symp_resp ~ Region, data = Timeliness_2020_05_12)
model4
summary(model4)
confint(model4)
ggplot(Timeliness_2020_05_12, aes(Region, Delay_symp_resp)) +
        geom_point() +
        stat_smooth(method = lm)

## Fifth predictor: signal
model4 <- lm(Delay_symp_resp ~ Signal, data = Timeliness_2020_05_12)
model4
summary(model4)
confint(model4)
ggplot(Timeliness_2020_05_12, aes(Signal, Delay_symp_resp)) +
        geom_point() +
        stat_smooth(method = lm)

##Multivariate model
model4 <- lm(Delay_symp_resp ~ Alert_bin + Year, data = Timeliness_2020_05_12)
model4
summary(model4)
confint(model4)


## 8. Linear regression of year on total delay (DOS-->DOR)

model3 <- lm(Delay_symp_resp ~ Year, data = Timeliness_2020_05_12)
model3
summary(model3)
confint(model3)
#Jittered scatterplot of date of onset of symptoms ("outbreak start date") vs delay from infection to response 
g <- ggplot(Timeliness_2020_05_12, aes(Timeliness_2020_05_12$Date_onset_eff, Timeliness_2020_05_12$Delay_symp_resp))
g + geom_count(col="tomato3", show.legend=F, alpha=1/8) +
        labs(y="Delay from symptom onset to response", 
             x="Outbreak start date (symptom onset of primary case)") +
        geom_smooth(method="lm", SE=FALSE, colour="black", formula=y~x,
                    data=Timeliness_2020_05_12,
                    mapping = aes(x=Date_onset_eff, y=Delay_symp_resp))










####    EXTRA CODE    ####
####    EXTRA CODE    ####
####    EXTRA CODE    ####
####    EXTRA CODE    ####
####    EXTRA CODE    ####

#Graph of Delays from infection to response, by country (ordered bar chart)
# Prepare data: group delay_inf_resp by country
delay_SR <- aggregate(Timeliness_2020_05_12$Delay_symp_resp, by=list(Timeliness_2020_05_12$Country), FUN=mean) #aggregate
colnames(delay_SR) <- c("country", "delay") #change column names
delay_SR <- delay_SR[order(delay_SR$delay), ] #sort by delay
delay_SR$country <-factor(delay_SR$country, levels = delay_SR$country) #retain order in plot
head(delay_SR, 10)

delay_SP <- aggregate(Timeliness_2020_05_12$Delay_symp_pres, by=list(Timeliness_2020_05_12$Country), FUN=mean) #aggregate
colnames(delay_SP) <- c("country", "delay") #change column names
delay_SP <- delay_SP[order(delay_SP$delay), ] #sort by delay
delay_SP$country <-factor(delay_SP$country, levels = delay_SP$country) #retain order in plot
head(delay_SP, 10)

# Draw plot
theme_set(theme_bw())
ggplot(delay_SR, aes(x=country, y=delay)) +
        ylim (0, 40) +
        geom_bar(stat="identity", width=.5, fill="tomato3") + 
        labs(subtitle="Time between estimated onset of symptoms and response",
             y="Delay from symptom onset to response") + 
        theme(axis.text.x = element_text(angle=65, vjust=0.6))

##Lollipop charts
ggplot(delay_SR, aes(x=country, y=delay)) +
        geom_point(size=1.5) + 
        geom_segment(aes(x=country, 
                         xend=country, 
                         y=0, 
                         yend=delay)) + 
        theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(delay_SP, aes(x=country, y=delay)) +
        geom_point(size=1.5) + 
        geom_segment(aes(x=country, 
                         xend=country, 
                         y=0, 
                         yend=delay)) + 
        theme(axis.text.x = element_text(angle=65, vjust=0.6))
        
###Symptom to investigation version        
delay_SI <- aggregate(Timeliness_2020_05_12$Delay_symp_inv, by=list(Timeliness_2020_05_12$Country), FUN=mean) #aggregate
colnames(delay_SI) <- c("country", "delay") #change column names
delay_SI <- delay_SI[order(delay_SI$delay), ] #sort by delay
delay_SI$country <-factor(delay_SI$country, levels = delay_SI$country) #retain order in plot
head(delay_SI, 100)

# Draw plot
theme_set(theme_bw())
ggplot(delay_SI, aes(x=country, y=delay)) +
        ylim (0, 40) +
        geom_bar(stat="identity", width=.5, fill="tomato3") + 
        labs(subtitle="Time between estimated onset of symptoms and response",
             y="Delay from symptom onset to response") + 
        theme(axis.text.x = element_text(angle=65, vjust=0.6))

##Lollipop chart
ggplot(delay_SI, aes(x=country, y=delay)) +
        geom_point(size=1.5) + 
        geom_segment(aes(x=country, 
                         xend=country, 
                         y=0, 
                         yend=delay)) + 
        theme(axis.text.x = element_text(angle=65, vjust=0.6))






