corona_data = read.csv("C:/Users/joaoh/Downloads/owid-covid-data.csv", header = TRUE, sep=",")



##################### understanding dataset ######################

#get information from first 6 elements
head(corona_data)
#get all names in dataset
names(corona_data)

str(corona_data)
summary(corona_data)


#cleaning data to show only Brazil data
brazil = corona_data[ which(corona_data$location == "Brazil" ),]

#showing only 2021 new cases

corona_data.brazil = corona_data[which(corona_data$location == "Brazil" & corona_data$date >= "2021-01-01"),]
head(corona_data.brazil)
View(corona_data.brazil)
plot(corona_data.brazil$new_cases, type = 'o')




#create another data frame

brazil_data <- data.frame(corona_data[9710,6])
View(brazil_data)

###################### filtering data to get brazil ###############

#filtering data
#install.packages("dplyr")
library(dplyr)


#using select function to use only wanted variables

new.subset <- corona_data %>% select(location, new_cases, date, new_deaths)
View(new.subset)

# getting only brazil data

brazil.subset <- corona_data.brazil %>% select(location, new_cases, date, new_deaths)
View(brazil.subset)

#plot brazil new dataset
plot(brazil.subset$new_cases, xlab = "Index", ylab = "New Cases", type = 'l')


install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

hist(brazil.subset$new_cases)
