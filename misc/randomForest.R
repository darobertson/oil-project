#read data
library(randomForest)
library(dplyr)
data <- read.csv("~/Desktop/Oil Project/database.csv")

#finding only 5 complete records
str(data)
completecases<-data[complete.cases(data),]
str(completecases)

#converts into dataframe that won't print out entire thing
data<-tbl_df(data)

#format datetime variables. Look at date later
#data$Accident.Month=as.datetime(format(data$Accident.Date.Time,format="%m/%d/%Y"), "%m")
#data$Accident.Month

#select important columns
#removes report number and supplemental number
data<-select(data,-Report.Number,-Supplemental.Number,-Operator.Name,
             -Pipeline.Facility.Name,-Accident.City, -Accident.County,
            -Liquid.Name)
#remove datetime variables. need to add back later TODO
data<-select(data,-Accident.Date.Time,-Shutdown.Date.Time,-Restart.Date.Time)
data
str(data)
#Now we have 2795 observations and 38 variables

#impute missing values with rfImpute, imputes by proximity
data.imputed <- rfImpute( All.Costs~. ,data)

#train the rf model with imputed data
train.rf <- randomForest(All.Costs ~., data = data.imputed, importance = TRUE, ntree = 500)

#produces variable importance plot
importance(train.rf, type=1)
varImpPlot(train.rf)