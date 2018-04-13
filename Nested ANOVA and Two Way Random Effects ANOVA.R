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

#Conclusion:Shift (Day or Night) doesn't have a significant impact on the Part Rate.



#2. Find data that is random effects completely crossed and perform the appropriate analysis. 
library(carData)
data<-ToothGrowth
#The response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs. 
#Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods,
#(orange juice or ascorbic acid (a form of vitamin C and coded as VC).
#Response is len and the two random effects factors are supp and dose. 
#supp is a random effect because there are more than 2 ways to inject a suppliment. These are 2 ways out of a population of different ways to inject a treatment
#dose is a random effect because there are in  infinite amount of different doses the experimenters could have chosen. They chose 3 levels out of an infinite amount of possible ones.

attach(data)
summary(data) #supp has 2 levels with 30 observations in each
#Need to make dose a factor since it has been coded as a numeric variable
data$dose<-as.factor(data$dose)
summary(data) #Now we see that dose has 3 levels with 20 observations in each

#Check if the data is completely crossed
xtabs(~ supp + dose, ToothGrowth)
#But here we have completely crossed and is balanced at n = 10.


#EDA

#mean
tapply(data$len, data$supp, mean) #Kind of close, but need to check variance to see if 4 is a larger difference for means
tapply(data$len, data$dose, mean) #Some number look far from each other, but need to check variance to see if this is true


#variance
tapply(data$len, data$supp, var) #variances are not close to one another, but at least one is not 10 times larger than the other so, still okay
tapply(data$len, data$dose, var) #variances are not close to one another, but at least one is not 10 times larger than the other so, still okay

#boxplot
ggplot(data, aes(supp, len, color = dose)) + geom_boxplot() #Confirms that there is probably a difference, since some interquartile regions don't intersect



model <- aov(len~supp * dose, data = data) #Complete model with interactions
anova(model) #Wanted to look at ANOVA table so did anova(model)
#Both the main effects and their interaction effect is signifcant
#This means I will look at the study according to interaction effect now.




##Model Checking
par(mfrow = c(2,2))
plot(model) #Residuals look like no pattern in resdiuals vs fitted and looks pretty normal. 
#Need to really look out of oultiers though. Looks like 3 observations might be outliers (Leverage Points). Theses could really "mess" with the analysis


#Interaction Effect
par(mfrow= c(1,1))
interaction.plot(supp, dose, len)

#Conclusion:Toothgrowth length is higher in OJ suppiment treatment for dose .5 and 1 than in these 2 doses with VC suppliment treatment type. 
#For dose 2, OJ suppliment actually has slightly less toothgrowth length than VC. 