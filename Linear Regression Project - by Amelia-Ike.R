#Read in bikeshare.csv file and set it to a dataframe called bike.
bike <- read.csv("bikeshare.csv")
setwd("C:/Users/Amelia/Documents/R/R  CLASS/First Class/Assignment/LINEAR REGRESSION")
getwd()
#Check the head of df
head(bike)
#Can you figure out what is the target we are trying to predict? 
#Check the Kaggle Link above if you are confused on this.
#The total count of bikes rented during each hour covered by 
#the test set, using only information available prior to the rental period.

#Create a scatter plot of count vs temp. Set a good alpha value.
library(ggplot2)
library(ggthemes)
pl <- ggplot(data=bike, aes(x=temp, y=count))
pl+ geom_point(aes(color=temp), alpha=0.2)

#Plot count versus datetime as a scatterplot with a color gradient based 
#on temperature. You'll need to convert the datetime column into POSIXct 
#before plotting.
bike$datetime <- as.POSIXct(paste(bike$datetime))
head(df)

pl <- ggplot(data=bike, aes(x=datetime, y=count))
pl+geom_point(aes(color=temp), alpha=0.2)+ scale_colour_gradient(high='orange', low='skyblue')

#What is the correlation between temp and count?
cor.data <- cor(bike[c('temp','count')])
cor.data

#Let's explore the season data. Create a boxplot, with the y axis indicating 
#count and the x axis begin a box for each season.

pl <- ggplot(data=bike, aes(x=factor(season), y=count))
pl+geom_boxplot(aes(color=factor(season))) 

#Create an "hour" column that takes the hour from the datetime column. 
#You'll probably need to apply some function to the entire datetime 
#column and reassign it.

bike["hour"] <- format(df$datetime,  "%H")
View(bike)

#Now create a scatterplot of count versus hour, with color scale 
#based on temp. Only use bike data where workingday==1.
bike_workingday <- subset(bike, bike$workingday==1)
bike_workingday
pl <- ggplot(data=bike_workingday, aes(x=hour, y=count))
pl+geom_point(aes(color=temp), position = position_jitter(w=1, h=0))+scale_color_gradientn(colors=c('blue','skyblue','green','yellow', 'orange','red'))

#Now create the same plot for non working days:
bike_nonworkingday <- subset(bike, bike$workingday==0)
bike_nonworkingday

pl <- ggplot(data=bike_nonworkingday, aes(x=hour, y=count))
pl+geom_point(aes(color=temp), position = position_jitter(w=1, h=0))+scale_color_gradientn(colors=c('blue','skyblue','green','yellow', 'orange','red'))

#Use lm() to build a model that predicts count based solely on the temp feature, name it temp.model
temp.model = lm(count~temp, data=bike)

#Review results with summary
summary(temp.model)

#How many bike rentals would we predict if the temperature was 25 degrees Celsius? 
#Calculate this two ways:

#Using the values we just got above
#formula = intercept + (temp * 25)
(6.0462) + (9.1705 * 25)

#Using the predict() function
temp.test <- data.frame(temp=25)
predict(temp.model,temp.test)

#Use sapply() and as.numeric to change the hour 
#column to a column of numeric values.
library(dplyr)
bike$hour <- sapply(bike$hour, as.numeric)
head(bike, 3)

#Finally build a model that attempts to predict count 
#based off of the following features.

count.model <- lm(count ~ . - casual - registered - datetime - atemp, data = bike)
summary(count.model)
