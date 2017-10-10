###########INITIALIZE DATA################
myLGdata<-read.csv("Churn.csv",header=TRUE)
names(myLGdata)
summary(myLGdata)
#total no of rows
noofrows <- nrow(myLGdata)
noofrows
#no of churns from dataset
 noofchurn <- subset(myLGdata,Churn ==1)
############B#############################
 set.seed(12345)
 trainindex <- sample(noofrows, 0.6*noofrows, replace=FALSE)
 training <- myLGdata[trainindex,]
 training
 
 validation <- myLGdata[-trainindex,]
 validation
 install.packages("RColorBrewer")
 library(RColorBrewer)
 library(ggplot2)
 
 qplot(RoamMins,Churn,main = "Roaming Vs Churn", xlab= "Roaming", ylab="Churn",data = training, colour = factor(ContractRenewal))
  
 
 #?plot
 ##Other methods to partition the data
 #train <- sample(2,row,replace = TRUE, prob = c(0.6,0.4))
 #trainingindex <- mydata[train ==1,]
 #validaindex <- mydata[train ==2,]
 #############################C#########################################
 myLogicR<-glm(Churn ~ AccountWeeks + DataUsage + CustServCalls + DayMins + DayCalls + MonthlyCharge + OverageFee + RoamMins,data=training, family=binomial)  
 
 summary(myLogicR)  
 
 val<-predict(myLogicR,validation,type="response")  
 val 

install.packages("SDMTools") 
library(SDMTools) 
matrix = confusion.matrix(validation$Churn,val,threshold=0.5)     
matrix

AccuMeasures = accuracy(validation$Churn,val,threshold=0.5)
AccuMeasures
# Extracting specific values from accuracy table
AccuMeasures$sensitivity
AccuMeasures$specificity

###########################D################################################

myLogicRd<-glm(Churn ~ AccountWeeks + ContractRenewal + DataPlan + DataUsage + CustServCalls + DayMins + DayCalls + MonthlyCharge + OverageFee + RoamMins,data=training, family=binomial)  

summary(myLogicRd)

coef(myLogicRd)

vald<-predict(myLogicRd,validation,type="response")  
vald


library(SDMTools) 
matrixd = confusion.matrix(validation$Churn,vald,threshold=0.5)     
matrixd

AccuMeasures = accuracy(validation$Churn,vald,threshold=0.5)
AccuMeasures
# Extracting specific values from accuracy table
AccuMeasures$sensitivity
AccuMeasures$specificity

################BACKWARD REGRESSION ###########################

mybackward<- step(glm(Churn ~ AccountWeeks + ContractRenewal + DataPlan + DataUsage + CustServCalls + DayMins + DayCalls + MonthlyCharge + OverageFee + RoamMins,data=training, family=binomial), direction = "backward")

valback<-predict(mybackward,validation,type="response")  
valback



library(SDMTools) 
matrixback = confusion.matrix(validation$Churn,valback,threshold=0.5)     
matrixback

AccuMeasures = accuracy(validation$Churn,valback,threshold=0.5)
AccuMeasures
# Extracting specific values from accuracy table
AccuMeasures$sensitivity
AccuMeasures$specificity

##############################ROC MODEL C#######################################

install.packages("ROCR")
library(ROCR)
mydf <-cbind(validation,val)
logit_scores <- prediction(predictions=mydf$val, labels=mydf$Churn)
logit_perf <- performance(logit_scores,"tpr","fpr")

plot(logit_perf,
     main="ROC Curves",
     xlab="xlabty: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="navyblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")

#AREA UNDER CURVE MODEL C#########

logit_auc <- performance(logit_scores, "auc")
as.numeric(logit_auc@y.values)  ##AUC Value model c

# Getting Lift Charts in R
# For getting Lift Chart in R, use measure="lift", x.measure="rpp" in the performance function.
# Get data for ROC curve and create lift chart values
logit_lift <- performance(logit_scores, measure="lift", x.measure="rpp")
plot(logit_lift,
     main="Lift Chart",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")

##############################ROC MODEL D #########################

library(ROCR)
mydf_d <-cbind(validation,vald)
logit_scores_d <- prediction(predictions=mydf$vald, labels=mydf$Churn)
logit_perf_d <- performance(logit_scores_d,"tpr","fpr")

plot(logit_perf_d,
     main="ROC Curves",
     xlab="xlabty: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="navyblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")

#AREA UNDER CURVE MODEL D#########

logit_auc_d <- performance(logit_scores_d, "auc")
as.numeric(logit_auc_d@y.values)  ##AUC Value model d

# Getting Lift Charts in R
# For getting Lift Chart in R, use measure="lift", x.measure="rpp" in the performance function.
# Get data for ROC curve and create lift chart values
logit_lift_d <- performance(logit_scores_d, measure="lift", x.measure="rpp")
plot(logit_lift_d,
     main="Lift Chart",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")


#################MODEL E ###################################

library(ROCR)
mydf_e <- cbind(validation,valback)
logit_scores_e <- prediction(predictions = mydf_e$valback,labels=mydf$Churn)
logit_perf_e <- performance(logit_scores_e,"tpr","fpr")

plot(logit_perf_e, main = "ROC Curve for Model E",  xlab="xlabty: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="navyblue",  lwd = 3)

abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="red")


logit_auc_e <- performance(logit_scores_e, "auc")
as.numeric(logit_auc_e@y.values)  ##AUC Value model e

logit_lift_e <- performance(logit_scores_e, measure="lift", x.measure="rpp")
plot(logit_lift_e,
     main="Lift Chart",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")

#########################CUTOFF/ACCURACY######################

# creating a range of values to test for accuracy
thresh=seq(0.1,1,by=0.1)

# Initializing a 1*20 matrix of zeros to save values of accuracy
acc = matrix(0,1,10)

# computing accuracy for different threshold values from 0 to 1 step by 0.1
for (i in 1:10){
  matrix = confusion.matrix(validation$Churn,valback,threshold=thresh[i])
  acc[i]=(matrix[1,1]+matrix[2,2])/nrow(validation)
}
# print and plot the accuracy vs cutoff threshold values
print(c(accuracy= acc, cutoff = thresh))
plot(thresh,acc,type="l",xlab="Threshold",ylab="Accuracy", main="Validation Accuracy for Different Threshold Values")

abline(a = 0,b=0, v= 0.5, col = "DarkRed")

