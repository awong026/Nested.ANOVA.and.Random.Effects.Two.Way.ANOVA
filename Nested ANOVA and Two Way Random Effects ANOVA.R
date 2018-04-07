#Homework:(Understand nesting and completely crossed)-Andrew Wong


#1. Find data that is nested and perform an ANOVA - discuss yoru results


#we have five different machines making the same part and each machine has two operators, 
#one for the day shift and one for the night shift. We take five samples from each machine for each operator to obtain the following data:

library(readxl)
Nested_Operator_Data_Set <- read_excel("Nested Operator Data Set.xlsx")
View(Nested_Operator_Data_Set)
data <- Nested_Operator_Data_Set
attach(data)
head(data)

#Need to check if Machine is a factor
is.factor(data$Machine)

#Need to change this column to be a factor
data$Machine <- as.factor(data$Machine)

#Need to check if Machine is a factor now
is.factor(data$Machine)

#EDA
#Check means
tapply(data$Part_Rate, data$Shift, mean) #Very small diff

#Check variances
tapply(data$Part_Rate, data$Shift, var) #Very small diff


#Check balanced design
tapply(data$Part_Rate, data$Shift, length) #We have 25 rats between each Operator's Shift

#We can check for normality after modeling since the number of observation per cell is smal

data.shift.aov <- aov(Part_Rate ~ Shift + Error(Machine), data = data) #For nested models. That's why Machine is in Error() function
summary(data.shift.aov) ##p-value is .493 for Shift, so don't reject H0 that Shift's Part_Rate are equal.

#Now we need to check the model
par(mfrow =c(2,2))
plot(lm(data.shift.aov)) #Looks like no pattern in res vs fitted, and qqnorm for residuals plot looks pretty good except maybe some outliers at the ends. 
par(mfrow = c(1,1))

#Shift doesn't change..................Part Rate



#2. Find data that is random effects completely crossed and perform the appropriate analysis. 










