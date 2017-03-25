# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

#Load the dataset from the specified location into R using a variable
opl <- read.csv(file="database.csv", stringsAsFactors=TRUE)
#understanding variables in the dataset
names(opl)
#understanding the summary of all variables in the dataset
summary(opl)

len <- length(opl$Report.Number)

#finding missing values. no missing values for important variables which are used to analyse accident report. However accident city had one missing value
sapply(opl, function(x) sum(is.na(x)))

#preliminary understanding of data
dim(opl)
head(opl)
tail(opl)


#Evaluating the range of dates when accident occured
min_acc <- min(opl$Accident.Year)
max_acc <- max(opl$Accident.Year)
min_acc
max_acc
#The report shows data from 2010 to 2017

#hist shown an increase in accidents from 2010-2015 after which there is a small decrease form 2015-2016.
hist(opl$Accident.Year)
#write.csv(my_solution, file = "my_solution.csv",row.names=FALSE)

#summary shows 7 different causes out of which material/weld/equip failure is the highest
summary(opl$Cause.Category)

#Below plot shows the accidents happening every year based on categories. the accident due to material/weld/equip failure has been happening for four consecutive years.
#other causes have been happening only in one particular year.accidents due to corrossion is happening for 3 years but less in 2012 and after that no corrosion accidents.
#Hence weather plays an important role in knowing the causes based on any of the categories which would occur and predicting accidents.
library(ggplot2)
qplot(opl$Accident.Year,data=opl,facets=Cause.Category ~.)

#Are there any missing values?
is.na(opl)
#shows that there some missing values for few varaibles.

# list rows of data that have missing values 
opl[!complete.cases(opl),]

#Creates a new dataset removing missing values.
opl<- na.omit(opl)

#Representing the number of accidents through pie-chart
mytable <- table(opl$Accident.Year)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, 
    main="Pie Chart of Accidents in a year")
#shows that no.of accidents occur less than 2017

factor(opl$Pipeline.Location)
#Calculating the proportion of onshore and offshore
prop.pipelinelocation <- sum(opl$Pipeline.Location == "OFFSHORE") /
  length(opl$Pipeline.Location)
prop.pipelinelocation

prop.pipelinelocation <- sum(opl$Pipeline.Location == "ONSHORE") /
  length(opl$Pipeline.Location)
prop.pipelinelocation


#pie chart showing the accidents number based on pipeline type
barplot(table(opl$Pipeline.Type),
        ylim=c(0,25),
        main = "Bar Graph of accidents number based on pipeline type",
        col = "lightblue")
box(which = "plot",
    lty = "solid",
    col="black")
#shows that pipelinetype transition area has led to less no of accidents.

#barplot for variable Pipeline.Location
barplot(table(opl$Pipeline.Location),
        ylim=c(0,25),
        main = "Bar Graph of onshore and off-shore",
        col = "lightblue")
box(which = "plot",
    lty = "solid",
    col="black")
#shows that accidents didnt occur on offshore


factor(opl$Liquid.Type)
#pie chart showing the accidents number based on Liquid.Type
mytable <- table(opl$Liquid.Type)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, 
    main="Pie Chart of Accidents in a year based on Liquid type")

#to know the mean accident latitude and longitude
boxplot(Accident.Latitude ~ Pipeline.Location, data=opl)
boxplot(Accident.Longitude ~ Pipeline.Location, data=opl)

#Normalised Accident.state variable bar plot
ggplot() +
  geom_bar(data = opl,
           aes(x = factor(opl$Accident.State),
               fill = factor(opl$Pipeline.Type)),
           position = "fill") +
  scale_x_discrete("Accident.State") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="opl")) +
  scale_fill_manual(values=c("blue", "yellow"))

#Pie chart to show which category cause occurence is more
pie(table(opl$Cause.Category))

#density plot to show the net barrel loss
plot(density(opl$Net.Loss..Barrels.))

plot(jitter(opl$Unintentional.Release..Barrels.), jitter(opl$Intentional.Release..Barrels.))

#BINNING
oplcl <- opl[,42:43]
kmeans.result <- kmeans(oplcl, centers=2)
# cluster centers
kmeans.result$centers
# cluster IDs
kmeans.result$cluster
# calculate distances between objects and cluster centers
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((oplcl - centers)^2))
# pick top 5 largest distances
outliers <- order(distances, decreasing=T)[1:5]
# who are outliers
print(outliers)
print(oplcl[outliers,])


#barplot for Variable liquid type



#barplot for variable liquid explosion
barplot(table(opl$Liquid.Explosion),
        ylim=c(0,5),
        main = "Bar Graph of accidents due to liquid explosion",
        col = "red")

barplot(table(opl$Pipeline.Shutdown),
        ylim=c(0,5),
        main = "Bar Graph of accidents due to liquid explosion",
        col = "blue")

barplot(table(opl$Liquid.Ignition),
        ylim=c(0,5),
        main = "Bar Graph of accidents due to liquid explosion",
        col = "black")

