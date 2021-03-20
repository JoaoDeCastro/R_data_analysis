corona_data = read.csv("C:/Users/joaoh/Downloads/owid-covid-data.csv", header = TRUE, sep=",")

#get information from first 6 elements
head(corona_data)
#get all names in dataset
names(corona_data)

str(corona_data)
summary(corona_data)
