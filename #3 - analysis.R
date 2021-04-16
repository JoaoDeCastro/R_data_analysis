#installing packages and libraries
install.packages("gganimate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggthemes")
install.packages("hrbrthemes")
install.packages("tidyverse")
install.packages("gifski")
install.packages("lubridate")
install.packages("scales")
library(ggplot2)
library(dplyr)
library(ggthemes)
library(hrbrthemes)
library(tidyverse)
library(lubridate)
library(gganimate)
library(gifski)
require(ggplot2)
require(scales) 




###########################################################################


# data updated 04/07
# theme used for lines theme_ft_rc()
# theme used for bar theme_stata()





# This is the code I used to analyze covid 19 data
# I spent a good amount of time learning how to clean the data
# how to make new subset, how to choose different variables, how to plot

# how to use ggplot2 and how to make it work

##########################################################################
##########################################################################








# reading dataset
setwd("C:/Users/joaoh/OneDrive/Desktop/WCSU 2021 SPRING/MAT - 222/Research project")
world_data = read.csv("owid-covid-data.csv")


# ------------------ first graph --------------------- #
# from first case to a month later

usaFirstCase = world_data %>% filter(location == "United States" & date >= "2020-01-22" & date <= "2020-02-22")
View(usaFirstCase)

#converting to date
data_new <- usaFirstCase$date                                   
data_new <- as.Date(usaFirstCase$date)  

#graph bar in this case

graph1 = usaFirstCase %>%
  ggplot(aes(x = data_new, y = total_cases, color = location)) +
  geom_bar(stat="identity", width=.5, fill="blue") + 
  scale_x_date(date_breaks = "3 day", date_labels = "%d %b") +
  labs(
    title = "Total cases in the USA",
    subtitle = "30 days after first case",
    x = "Date", y = "Number of total cases",
    caption="source: ourworldindata.org",
    color = "Country"
        )+
 
  
  theme_stata()+
  theme(plot.title  = element_text(hjust = 0.5, size = 25, face = "bold"))+
  theme(plot.subtitle  = element_text(hjust = 0.5, size = 15))+
  theme(axis.title.x = element_text(hjust = 0.5, size = 12, vjust = -2, face = "italic"))+
  theme(axis.title.y = element_text(hjust = 0.5, size = 12, vjust = 5, face = "italic"))

graph1

#_____________________________________________#


# ------------------ second graph --------------------- #
# from first case to a month later - brazil

brazilFirstCase = world_data %>% filter(location == "Brazil" & date >= "2020-02-26" & date <= "2020-03-26")
View(brazilFirstCase)

#converting to date
data_new <- brazilFirstCase$date                                   
data_new <- as.Date(brazilFirstCase$date)  

#graph bar in this case

graph2 = brazilFirstCase %>%
  ggplot(aes(x = data_new, y = total_cases, color = location)) +
  geom_bar(stat="identity", width=.5, fill="blue") + 
  scale_x_date(date_breaks = "3 day", date_labels = "%d %b") +
  labs(
    title = "Total cases in Brazil",
    subtitle = "30 days after first case",
    x = "Date", y = "Number of total cases",
    caption="source: ourworldindata.org",
    color = "Country"
  )+
  
  theme_stata()+
  theme(plot.title  = element_text(hjust = 0.5, size = 25, face = "bold"))+
  theme(plot.subtitle  = element_text(hjust = 0.5, size = 15))+
  theme(axis.title.x = element_text(hjust = 0.5, size = 12, vjust = -2, face = "italic"))+
  theme(axis.title.y = element_text(hjust = 0.5, size = 12, vjust = 5, face = "italic"))

graph2

#_____________________________________________#


# ------------------ third graph --------------------- #
# same period of brazil

usaSecondMonth = world_data %>% filter(location == "United States" & date >= "2020-02-26" & date <= "2020-03-26")


#converting to date
data_new <- usaSecondMonth$date                                   
data_new <- as.Date(usaSecondMonth$date)  

#graph bar in this case

graph3 = usaSecondMonth %>%
  ggplot(aes(x = data_new, y = total_cases, color = location)) +
  geom_bar(stat="identity", width=.5, fill="blue") + 
  scale_x_date(date_breaks = "3 day", date_labels = "%d %b") +
  labs(
    title = "Total cases in the USA",
    subtitle = "After 60 days",
    x = "Date", y = "Number of total cases",
    caption="source: ourworldindata.org",
    color = "Country"
  )+
  
  theme_stata()+
  theme(plot.title  = element_text(hjust = 0.5, size = 25, face = "bold"))+
  theme(plot.subtitle  = element_text(hjust = 0.5, size = 15))+
  theme(axis.title.x = element_text(hjust = 0.5, size = 12, vjust = -2, face = "italic"))+
  theme(axis.title.y = element_text(hjust = 0.5, size = 12, vjust = 5, face = "italic"))

graph3

#_____________________________________________#




# ------------------  graph 4 --------------------- #
# first death brazil and USA after 60 days


###### filter data 
brazilUsaFirstDeath = world_data %>% filter(location == "Brazil" | location == "United States")
brazil_usa_firstDeath = brazilUsaFirstDeath %>% filter(date >= "2020-03-01" & date <= "2020-05-01")





View(brazil_usa_firstDeath)

#converting to date
data_new <- brazil_usa_firstDeath$date                                   
data_new <- as.Date(brazil_usa_firstDeath$date)  



#graph line

graph4 = brazil_usa_firstDeath %>%
  ggplot(aes(x = data_new, y = new_deaths, color = location)) +
  geom_point(shape=21, color="white", fill="#69b3a2", size=2) +
  geom_line() +
  scale_x_date(date_breaks = "7 days", date_labels = "%d %b") +
  labs(
    x = "Date", y = "Number of new deaths",
    title = "First death in Brazil and in the USA",
    subtitle = "60 days timeline",
    caption="source: ourworldindata.org",
    color = "Country"
  )      +
  
  theme_ft_rc()+
  theme(plot.title  = element_text(hjust = 0.5, size = 25, face = "bold"))+
  theme(plot.subtitle  = element_text(hjust = 0.5, size = 15))+
  theme(axis.title.x = element_text(hjust = 0.5, size = 12, vjust = -5, face = "italic"))+
  theme(axis.title.y = element_text(hjust = 0.5, size = 12, vjust = 5, face = "italic"))


graph4

#_____________________________________________#


# ------------------  graph 5 --------------------- #
# after lockdown


###### filter data 
brazilUsaFirstDeath = world_data %>% filter(location == "Brazil" | location == "United States")
brazil_usa_lockdown = brazilUsaFirstDeath %>% filter(date >= "2020-03-19" & date <= "2020-06-19")





#converting to date
data_new <- brazil_usa_lockdown$date                                   
data_new <- as.Date(brazil_usa_lockdown$date)  



#graph line

graph5 = brazil_usa_lockdown %>%
  ggplot(aes(x = data_new, y = new_deaths, color = location)) +
  #geom_point(shape=26, color="white", fill="#69b3a2", size=2) +
  geom_line(size=1.2, alpha=2) +
  scale_x_date(date_breaks = "12 days", date_labels = "%d %b") +
  #labs
  labs(
    x = "3 Months Timeline", y = "Number of deaths",
    title = "Number of Total Deaths After Lockdown",
    subtitle = "USA and Brazil timeline ",
    caption="source: ourworldindata.org",
    color = "Country"
  )      +
  
  #theme
  
  theme_ft_rc()+
  
  #editing titles
  theme(plot.title  = element_text(hjust = 0.5, size = 25, face = "bold"))+
  theme(plot.subtitle  = element_text(hjust = 0.5, size = 15))+
  theme(axis.title.x = element_text(hjust = 0.5, size = 12, vjust = -5, face = "italic"))+
  theme(axis.title.y = element_text(hjust = 0.5, size = 12, vjust = 5, face = "italic"))



graph5

#_____________________________________________#





#_____________________________________________#


# ------------------  graph 6 --------------------- #
# after lockdown


###### filter data 
brazilUsa = world_data %>% filter(location == "Brazil" | location == "United States")
brazil_usa_lockdown = brazilUsa %>% filter(date >= "2020-03-19" & date <= "2020-06-25")





#converting to date
data_new <- brazil_usa_lockdown$date                                   
data_new <- as.Date(brazil_usa_lockdown$date)  



#graph line

graph6 = brazil_usa_lockdown %>%
  ggplot(aes(x = data_new, y = new_cases, color = location)) +
  #geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  geom_line(size=1.2, alpha=2) +
  scale_x_date(date_breaks = "12 days", date_labels = "%d %b") +
  #labs
  labs(
    x = "3 Months Timeline", y = "Number of new cases daily",
    title = "Number of Total New Cases Daily",
    subtitle = "USA and Brazil new cases after lockdown",
    caption="source: ourworldindata.org",
    color = "Country"
  )      +
  
  #theme
  
  theme_ft_rc()+
  
  #editing titles
  theme(plot.title  = element_text(hjust = 0.5, size = 25, face = "bold"))+
  theme(plot.subtitle  = element_text(hjust = 0.5, size = 15))+
  theme(axis.title.x = element_text(hjust = 0.5, size = 12, vjust = -5, face = "italic"))+
  theme(axis.title.y = element_text(hjust = 0.5, size = 12, vjust = 5, face = "italic"))



graph6

#_________________________________________#



# ------------------  graph 7 --------------------- #
# after holiday


###### filter data 
brazilUsa = world_data %>% filter(location == "Brazil" | location == "United States" | location == "United Kingdom")
brazil_usa_holiday = brazilUsa %>% filter(date >= "2020-11-10" & date <= "2021-03-10")





#converting to date
data_new <- brazil_usa_holiday$date                                   
data_new <- as.Date(brazil_usa_holiday$date)  

View(brazil_usa_holiday)

#graph line
graph7 = brazil_usa_holiday %>%
  mutate(isUsa = (location == "United States"))%>%
  ggplot(aes(x = data_new, y = new_cases_per_million, color = location)) +
  #make USA solid and rest dashed
  geom_line(aes(linetype = isUsa), size=1.5, alpha=3) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  labs(
    title = "Daily new confirmed cases per million",
    subtitle = "Holiday analisys",
    x = "Date", y = "Number of daily new cases per million",
    caption="source: ourworldindata.org",
    color = "Country"
  )+
  
  scale_linetype_manual(values = c("dashed", "solid"), guide = "none")+
  scale_y_continuous(labels = comma)+
  
  theme_ft_rc()+
  theme(plot.title  = element_text(hjust = 0.5, size = 25, face = "bold"))+
  theme(plot.subtitle  = element_text(hjust = 0.5, size = 15))+
  theme(axis.title.x = element_text(hjust = 0.5, size = 12, vjust = -5, face = "italic"))+
  theme(axis.title.y = element_text(hjust = 0.5, size = 12, vjust = 5, face = "italic"))


graph7

#_________________________________________#





# ------------------ graph 8 --------------------- #
# after holiday

brazilAfterHoliday = world_data %>% filter(location == "Brazil" | location == "United States")
brazil_usa_holiday = brazilAfterHoliday %>% filter(date >= "2020-11-10" & date <= "2021-03-25")



#converting to date
data_new <- brazil_usa_holiday$date                                   
data_new <- as.Date(brazil_usa_holiday$date)  

#graph bar in this case

graph8 = brazil_usa_holiday %>%
  ggplot(aes(x = data_new, y = new_deaths, color = location)) +
  #(shape=21, color="black", fill="#69b3a2", size=5) +
  geom_line(size=1.5, alpha=2) +
  scale_x_date(date_breaks = "10 days", date_labels = "%d %b") +
  #labs
  labs(
    x = "Date", y = "Number of new deaths daily",
    title = "Number of Total New Deaths Daily",
    subtitle = "Brazil deaths after holiday",
    caption="source: ourworldindata.org",
    color = "Country"
  )      +
  
  #theme
  
  theme_ft_rc()+
  
  #editing titles
  theme(plot.title  = element_text(hjust = 0.5, size = 25, face = "bold"))+
  theme(plot.subtitle  = element_text(hjust = 0.5, size = 15))+
  theme(axis.title.x = element_text(hjust = 0.5, size = 12, vjust = -5, face = "italic"))+
  theme(axis.title.y = element_text(hjust = 0.5, size = 12, vjust = 5, face = "italic"))

graph8



#_________________________________________#



# ------------------ graph 9 --------------------- #
# vaccination

vaccination = world_data %>% filter(location == "Brazil" | location == "United States"| location == "Mexico"| location == "Israel"| location == "Chile")
vaccination_contries = vaccination %>% filter(date >= "2021-01-10" & date <= "2021-04-05")



#converting to date
data_new <- vaccination_contries$date                                   
data_new <- as.Date(vaccination_contries$date)  

#graph bar in this case

graph9 = vaccination_contries %>%
  ggplot(aes(x = data_new, y = total_vaccinations_per_hundred, color = location)) +
  #(shape=21, color="black", fill="#69b3a2", size=5) +
  geom_line(size=1.5, alpha=2) +
  scale_x_date(date_breaks = "10 days", date_labels = "%d %b") +
  #labs
  labs(
    x = "Date", y = "Total number of Vaccinations per hundred",
    title = "Total Number of Vaccinations",
    subtitle = "Comparing vaccination per hundred",
    caption="source: ourworldindata.org",
    color = "Country"
  )      +
  
  #theme
  
  theme_ft_rc()+
  
  #editing titles
  theme(plot.title  = element_text(hjust = 0.5, size = 25, face = "bold"))+
  theme(plot.subtitle  = element_text(hjust = 0.5, size = 15))+
  theme(axis.title.x = element_text(hjust = 0.5, size = 12, vjust = -5, face = "italic"))+
  theme(axis.title.y = element_text(hjust = 0.5, size = 12, vjust = 5, face = "italic"))

graph9



#_________________________________________#















################ DATA CLEANING FOR USE #######################

#brazil and usa 2021
#separate subset

brazil = world_data %>% filter(location == "Brazil" & date >= "2021-01-01")
usa = world_data %>% filter(location == "United States" & date >= "2021-01-01")



# brazil full data for 2021
brazil_usa.subset = world_data %>%
    filter(location %in% c("United States", "Brazil"))

#filter for 2021
brazil_usa = brazil_usa.subset %>% 
  filter(date >= "2021-01-01")
View(brazil_usa)




             
#convert to date datatype

data_new <- brazil_usa$date                                   
data_new <- as.Date(brazil_usa$date)  




############################# END DATA CLEANING ####################


################ TESTING GRAPHS #################

#first graph comparing total deaths in brazil and usa in 2021

graph1 = brazil_usa %>%
ggplot(aes(x = data_new, y = new_deaths, color = location)) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  geom_line() +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  labs(
    x = "Date", y = "New deaths",
    title = "New confirmed COVID-19 deaths in Brazil & United States"
  )      +
  theme_ipsum()
graph1

############## BAR #################

#graph
graph1 = usaSecondMonth %>%
  ggplot(aes(x = data_new, y = total_cases, color = location)) +
  #geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  #geom_line() +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  labs(
    x = "Date", y = "Number of total cases",
    title = "Total cases in the USA from day 1 to day 30"
  )      +
  theme_ipsum()
graph1


#############################


#brazil subset
brazil.subset <- brazil %>% select(location, new_cases, date, new_deaths)


# converting to date object
data_new <- brazil$date                                   
data_new <- as.Date(brazil$date)  
typeof(data_new)
class(data_new)


brazil.subset %>%
  ggplot(aes(x = data_new, y = new_deaths, group = 1))+
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  geom_line( color="red")+
  labs(title = "Number of confirmed daily deaths in Brazil",
       x = "Date",
       y = "Number of Deaths")+
  theme_ipsum()
#  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b")

