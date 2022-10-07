# installing all packages

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("dplyr")
install.packages("magrittr")
install.packages("reshape2")
install.packages("ggpubr")
install.packages("wesanderson")

# loading all packages

library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(magrittr)
library(reshape2)
library(ggpubr)
library(wesanderson)

#loading the data frame

fx_data <- read_csv("~/Dropbox/My Mac (Fozils-Air.hitronhub.home)/Desktop/Forex Data Case Study/Forex Project 1/GBP_USD Historical Data.csv")

View(fx_data)

#adding a pip count

pip_col <- mutate(fx_data, Pip_Count = (High - Low) * 10000)

#new date format 

new_data <- pip_col %>%
  mutate(Date = lubridate::mdy(Date))

#converting dates to text

text_date <- new_data %>%
  mutate(Date = format(Date, "%b-%y"))

#Change column name

colnames(text_date)[colnames(text_date) == 'Pip_Count'] <- 'Pip_Count'

#data frames for each year

year_2019 <- text_date %>% 
  slice(14:25)

year_2018 <- text_date %>% 
  slice(26:37)

year_2017 <- text_date %>% 
  slice(38:49)

year_2016 <- text_date %>% 
  slice(50:61)

year_2015 <- text_date %>% 
  slice(62:73)

year_2014 <- text_date %>% 
  slice(74:85)

year_2013 <- text_date %>% 
  slice(86:97)

year_2012 <- text_date %>% 
  slice(98:109) 

year_2011 <- text_date %>% 
  slice(110:120)


#Creating Data Visualizations for each year in a Pie Chart

#Grouped Max and Min to Date 2011

max_2011 <- year_2011$Date[which.max(year_2011$Pip_Count)]
min_2011 <- year_2011$Date[which.min(year_2011$Pip_Count)]

#Year 2011

year_2011_chart <- ggplot(year_2011, aes(x = Date, y = Pip_Count, fill = Date)) +
  geom_col() +
  geom_text(aes(label = floor(Pip_Count)), vjust = 1.5, colour = "black", size = 3) +
  scale_fill_brewer(palette="Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 62, vjust = 0.5, size = 8, face = 'bold')) +
  theme(axis.text.y = element_text(size = 8, face = "bold")) +
  theme(plot.title = element_text(size = 11,color="black",face="bold")) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 10, face ='bold')) +
  theme(axis.title.x = element_text(size = 10, face ='bold')) +
  labs(title = 'Pip Count Per Month in 2011') +
  labs(subtitle = paste('Max:', max_2011, '\n', 'Min:', min_2011)) +
  theme(legend.position="none")

#Grouped Max and Min to Date 2012

max_2012 <- year_2012$Date[which.max(year_2012$Pip_Count)]
min_2012 <- year_2012$Date[which.min(year_2012$Pip_Count)]

#Year 2012

year_2012_chart <- ggplot(year_2012, aes(x = Date, y = Pip_Count, fill = Date)) +
  geom_col() +
  geom_text(aes(label = floor(Pip_Count)), vjust = 1.5, colour = "black", size = 3) +
  scale_fill_brewer(palette="Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 62, vjust = 0.5, size = 8, face = 'bold')) +
  theme(axis.text.y = element_text(size = 8, face = "bold")) +
  theme(plot.title = element_text(size = 11,color="black",face="bold")) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 10, face ='bold')) +
  theme(axis.title.x = element_text(size = 10, face ='bold')) +
  labs(title = 'Pip Count Per Month in 2012') +
  labs(subtitle = paste('Max:', max_2012, '\n', 'Min:', min_2012)) +
  theme(legend.position="none")

#Grouped Max and Min to Date 2013

max_2013 <- year_2013$Date[which.max(year_2013$Pip_Count)]
min_2013 <- year_2013$Date[which.min(year_2013$Pip_Count)]

#Year 2013

year_2013_chart <- ggplot(year_2013, aes(x = Date, y = Pip_Count, fill = Date)) +
  geom_col() +
  geom_text(aes(label = floor(Pip_Count)), vjust = 1.5, colour = "black", size = 3) +
  scale_fill_brewer(palette="Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 62, vjust = 0.5, size = 8, face = 'bold')) +
  theme(axis.text.y = element_text(size = 8, face = "bold")) +
  theme(plot.title = element_text(size = 11,color="black",face="bold")) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 10, face ='bold')) +
  theme(axis.title.x = element_text(size = 10, face ='bold')) +
  labs(title = 'Pip Count Per Month in 2013') +
  labs(subtitle = paste('Max:', max_2013, '\n', 'Min:', min_2013)) +
  theme(legend.position="none")

#Grouped Max and Min to Date 2014

max_2014 <- year_2014$Date[which.max(year_2014$Pip_Count)]
min_2014 <- year_2014$Date[which.min(year_2014$Pip_Count)]

#Year 2014

year_2014_chart <- ggplot(year_2014, aes(x = Date, y = Pip_Count, fill = Date)) +
  geom_col() +
  geom_text(aes(label = floor(Pip_Count)), vjust = 1.5, colour = "black", size = 3) +
  scale_fill_brewer(palette="Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 62, vjust = 0.5, size = 8, face = 'bold')) +
  theme(axis.text.y = element_text(size = 8, face = "bold")) +
  theme(plot.title = element_text(size = 11,color="black",face="bold")) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 10, face ='bold')) +
  theme(axis.title.x = element_text(size = 10, face ='bold')) +
  labs(title = 'Pip Count Per Month in 2014') +
  labs(subtitle = paste('Max:', max_2014, '\n', 'Min:', min_2014)) +
  theme(legend.position="none")

#Grouped Max and Min to Date 2015

max_2015 <- year_2015$Date[which.max(year_2015$Pip_Count)]
min_2015 <- year_2015$Date[which.min(year_2015$Pip_Count)]

#Year 2015

year_2015_chart <- ggplot(year_2015, aes(x = Date, y = Pip_Count, fill = Date)) +
  geom_col() +
  geom_text(aes(label = floor(Pip_Count)), vjust = 1.5, colour = "black", size = 3) +
  scale_fill_brewer(palette="Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 62, vjust = 0.5, size = 8, face = 'bold')) +
  theme(axis.text.y = element_text(size = 8, face = "bold")) +
  theme(plot.title = element_text(size = 11,color="black",face="bold")) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 10, face ='bold')) +
  theme(axis.title.x = element_text(size = 10, face ='bold')) +
  labs(title = 'Pip Count Per Month in 2015') +
  labs(subtitle = paste('Max:', max_2015, '\n', 'Min:', min_2015)) +
  theme(legend.position="none")

#Grouped Max and Min to Date 2016

max_2016 <- year_2016$Date[which.max(year_2016$Pip_Count)]
min_2016 <- year_2016$Date[which.min(year_2016$Pip_Count)]

#Year 2016

year_2016_chart <- ggplot(year_2016, aes(x = Date, y = Pip_Count, fill = Date)) +
  geom_col() +
  geom_text(aes(label = floor(Pip_Count)), vjust = 1.5, colour = "black", size = 3) +
  scale_fill_brewer(palette="Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 62, vjust = 0.5, size = 8, face = 'bold')) +
  theme(axis.text.y = element_text(size = 8, face = "bold")) +
  theme(plot.title = element_text(size = 11,color="black",face="bold")) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 10, face ='bold')) +
  theme(axis.title.x = element_text(size = 10, face ='bold')) +
  labs(title = 'Pip Count Per Month in 2016') +
  labs(subtitle = paste('Max:', max_2016, '\n', 'Min:', min_2016)) +
  theme(legend.position="none")

#Grouped Max and Min to Date 2017

max_2017 <- year_2017$Date[which.max(year_2017$Pip_Count)]
min_2017 <- year_2017$Date[which.min(year_2017$Pip_Count)]

#Year 2017

year_2017_chart <- ggplot(year_2017, aes(x = Date, y = Pip_Count, fill = Date)) +
  geom_col() +
  geom_text(aes(label = floor(Pip_Count)), vjust = 1.5, colour = "black", size = 3) +
  scale_fill_brewer(palette="Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 62, vjust = 0.5, size = 8, face = 'bold')) +
  theme(axis.text.y = element_text(size = 8, face = "bold")) +
  theme(plot.title = element_text(size = 11,color="black",face="bold")) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 10, face ='bold')) +
  theme(axis.title.x = element_text(size = 10, face ='bold')) +
  labs(title = 'Pip Count Per Month in 2017') +
  labs(subtitle = paste('Max:', max_2017, '\n', 'Min:', min_2017)) +
  theme(legend.position="none")

#Grouped Max and Min to Date 2018

max_2018<- year_2018$Date[which.max(year_2018$Pip_Count)]
min_2018 <- year_2018$Date[which.min(year_2018$Pip_Count)]

#Year 2018

year_2018_chart <- ggplot(year_2018, aes(x = Date, y = Pip_Count, fill = Date)) +
  geom_col() +
  geom_text(aes(label = floor(Pip_Count)), vjust = 1.5, colour = "black", size = 3) +
  scale_fill_brewer(palette="Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 62, vjust = 0.5, size = 8, face = 'bold')) +
  theme(axis.text.y = element_text(size = 8, face = "bold")) +
  theme(plot.title = element_text(size = 11,color="black",face="bold")) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 10, face ='bold')) +
  theme(axis.title.x = element_text(size = 10, face ='bold')) +
  labs(title = 'Pip Count Per Month in 2018') +
  labs(subtitle = paste('Max:', max_2018, '\n', 'Min:', min_2018)) +
  theme(legend.position="none")

#Grouped Max and Min to Date 2019

max_2019 <- year_2019$Date[which.max(year_2019$Pip_Count)]
min_2019 <- year_2019$Date[which.min(year_2019$Pip_Count)]

#Year 2019

year_2019_chart <- ggplot(year_2019, aes(x = Date, y = Pip_Count, fill = Date)) +
  geom_col() +
  geom_text(aes(label = floor(Pip_Count)), vjust = 1.5, colour = "black", size = 3) +
  scale_fill_brewer(palette="Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 62, vjust = 0.5, size = 8, face = 'bold')) +
  theme(axis.text.y = element_text(size = 8, face = "bold")) +
  theme(plot.title = element_text(size = 11,color="black",face="bold")) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 10, face ='bold')) +
  theme(axis.title.x = element_text(size = 10, face ='bold')) +
  labs(title = 'Pip Count Per Month in 2019') +
  labs(subtitle = paste('Max:', max_2019, '\n', 'Min:', min_2019)) +
  theme(legend.position="none")

#Combining all nine columns together

ggarrange(year_2011_chart, year_2012_chart, year_2013_chart,
          year_2014_chart, year_2015_chart, year_2016_chart,
          year_2017_chart, year_2018_chart, year_2019_chart,
          ncol= 3, nrow = 3) 

#Group each month together 

combined_data <- new_data %>% 
  group_by(month(Date)) %>% 
  summarize(sum_all_pips = sum(Pip_Count))

#Change column name

colnames(combined_data)[colnames(combined_data) == 'month(Date)'] <- 'Date'

#Convert Date to text

test_g <- month.name[combined_data$Date]

#Adding column

combined_data <- mutate(combined_data, Months = test_g )

#Max and Min for for data from 2011-2019

max_all_time <- combined_data$Date[which.max(combined_data$sum_all_pips)]
min_all_time <- combined_data$Date[which.min(combined_data$sum_all_pips)]

#Convert Max and Min to text Date

max_all_time_text <- month.name[max_all_time]
min_all_time_text <- month.name[min_all_time]

#Creating Data Visualizations for data from 2011-2019

combined_data_chart <- ggplot(combined_data, aes(x = Months, y = sum_all_pips, fill = Months)) +
  geom_col() +
  geom_text(aes(label = floor(sum_all_pips)), vjust = 1.5, colour = "black") +
  scale_fill_brewer(palette="Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 62, vjust = 0.5)) +
  theme(plot.title= element_text(size=12,color="black",face="bold")) +
  labs(title = 'Pip Count per month from 2011-2019') +
  labs(subtitle = paste('Max:', max_all_time_text,'\n','Min:', min_all_time_text)) +
  theme(legend.position="none")

ggarrange(combined_data_chart)
  


getwd()
  
  
