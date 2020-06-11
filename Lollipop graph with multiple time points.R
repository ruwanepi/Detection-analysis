library(tidyverse)
library(boot)
library(rcompanion)
library(ggthemes)


cc <- Timeliness_2020_05_12
theme_set(theme_gray(base_size = 18))

cc2 <- cc %>%
  select(Country, Year, Delay_symp_pres, Delay_symp_inv, Delay_symp_resp, Delay_symp_conf) %>%
  group_by(Country) %>%
  summarise(Delay_symp_pres_grouped = mean(Delay_symp_pres))

cc3 <- cc %>%
  select(Country, Year, Delay_symp_pres, Delay_symp_inv, Delay_symp_resp, Delay_symp_conf) %>%
  group_by(Country) %>%
  summarise(Delay_symp_inv_grouped = mean(Delay_symp_inv))

cc4 <- cc %>%
  select(Country, Year, Delay_symp_pres, Delay_symp_inv, Delay_symp_resp, Delay_symp_conf) %>%
  group_by(Country) %>%
  summarise(Delay_symp_resp_grouped = mean(Delay_symp_resp))

cc5 <- cc %>%
  select(Country, Year, Delay_symp_pres, Delay_symp_inv, Delay_symp_resp, Delay_symp_conf) %>%
  group_by(Country) %>%
  summarise(Delay_symp_conf_grouped = mean(Delay_symp_conf))

#plotting lollipop graphs individually for delay from symptom onset to various points
ggplot(cc5, aes(x = reorder(Country, Delay_symp_conf_grouped), y = Delay_symp_conf_grouped)) +  
  geom_point(stat = "identity", fill = "black", size = 1.5) +
  geom_segment(aes(y = 0,
                   x = Country,
                   yend = Delay_symp_conf_grouped,
                   xend = Country),
               color = "black") + #this function creates the lines leading to points
  theme(text = element_text(size=12)) +
  labs(y = "Delay from symptom onset to response", x = " ")+
  coord_flip()


ggplot(cc4, aes(x = reorder(Country, Delay_symp_resp_grouped), y = Delay_symp_resp_grouped)) +  
  geom_point(stat = "identity", fill = "black", size = 1.5) +
  geom_segment(aes(y = 0,
                   x = Country,
                   yend = Delay_symp_resp_grouped,
                   xend = Country),
               color = "black") + #this function creates the lines leading to points
  theme(text = element_text(size=12)) +
  labs(y = "Delay from symptom onset to response", x = " ")+
        coord_flip()

ggplot(cc3, aes(x = reorder(Country, Delay_symp_inv_grouped), y = Delay_symp_inv_grouped)) +  
  geom_point(stat = "identity", fill = "black", size = 1.5) +
  geom_segment(aes(y = 0,
                   x = Country,
                   yend = Delay_symp_inv_grouped,
                   xend = Country),
               color = "black") + #this function creates the lines leading to points
  theme(text = element_text(size=12)) +
  labs(y = "Delay from symptom onset to investigation", x = " ")+
  coord_flip()

ggplot(cc2, aes(x = reorder(Country, Delay_symp_pres_grouped), y = Delay_symp_pres_grouped)) +  
  geom_point(stat = "identity", fill = "black", size = 1.5) +
  geom_segment(aes(y = 0,
                   x = Country,
                   yend = Delay_symp_pres_grouped,
                   xend = Country),
               color = "black") + #this function creates the lines leading to points
  theme(text = element_text(size=12)) +
  labs(y = "Delay from symptom onset to presentation", x = " ")+
  coord_flip()

#Comparison of all delays, by country
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




