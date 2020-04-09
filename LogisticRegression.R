
library(readxl)
Data <- read_excel("D:/1.Sofia University Bechelor/Second Course/Second Term/2. Statistics and Empirical Methods/Binary-Logistic-Regression/Data.xlsx")



Data[!complete.cases(Data),]

#install.packages("ggplot2")

summary(Data)

Data = Data[,-1]
Data = Data[,-2]



#par(mfrow=c(1,1))
#plot(Data$CreditScore)
#plot(Data$Age)
#plot(Data$EstimatedSalary)
#plot(Data$NumOfProducts)
#plot(Data$Age, xlab = "Number of observations", ylab = "Age",main = "Age of each client",col = " red")
#plot(Data$CreditScore, xlab = "Number of observations", ylab = "Credit Score",main = "Credit score per account",col = " Blue")

par(mfrow=c(1,1))
hist(Data$CreditScore, xlab="Credit score", ylab="Number of obeservations",main = "Credit Scores",col="red")
hist(Data$Age, xlab="Age", ylab="Number of obeservations",main = "Age of each customer",col="blue")
hist(Data$Tenure, xlab="Tenure", ylab="Number of obeservations",main = "Tenure of clients",col="pink")
hist(Data$Balance, xlab="Balance", ylab="Number of obeservations",main = "Balance of clients",col="yellow")

par(mfrow=c(1,5))
hist(Data$NumOfProducts, xlab="Number of products", ylab="Number of obeservations",main = "Number of products of customers",col="red")
hist(Data$HasCrCard, xlab="Has card", ylab="Number of obeservations",main = "Customers have cards",col="green")
hist(Data$IsActiveMember, xlab="Is Active", ylab="Number of obeservations",main = "Status of clients",col="blue")
hist(Data$EstimatedSalary, xlab="Estimated salary", ylab="Number of obeservations",main = "Estimated salary",col="pink")
hist(Data$Exited, xlab="Exited", ylab="Number of obeservations",main = "Exited customers",col="orange")


Data$Gender<-Data$Gender(c("Male","Female"),100,replace= TRUE)
barplot(table(Data$Gender), col="blue")

#categorical Data encoding
Data$Gender = factor(Data$Gender,
                        levels = c('Male', 'Female'),
                        labels = c(1, 2))
Data$Gender=as.numeric(Data$Gender)


#install.packages("Hmisc")
library(Hmisc)
library(corrplot)
Data_corr = Data[,-1]

rcorr(as.matrix(Data_corr[,-10]),type="spearman")

install.packages("corrplot")
library(corrplot)
sapply(Data_corr, is.factor)
M <- cor(Data_corr[sapply(Data, function(x) !is.factor(x))])
corrplot(M, method="number")


install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(Data$Exited, SplitRatio = 0.8)
training_set = subset(Data[,-1], split == TRUE)
test_set = subset(Data[,-1], split == FALSE)

summary(training_set)
training_set[,-10] = scale(training_set[,-10])
test_set[,-10] = scale(test_set[,-10])


regressor = glm(formula = Exited ~.,family = binomial, data=training_set)
summary(regressor)



prop_pred = predict(regressor,type = 'response',newdata = test_set[,-10])

y_pred = ifelse(prop_pred > 0.5,1,0) 

cm = table(test_set$Exited,y_pred)
#plot a table
regressor2 = glm(formula = Exited ~ CreditScore  + Gender + Age + Balance  + IsActiveMember,
                 family="binomial", data=training_set)
summary(regressor2)


prop_pred = predict(regressor2,type = 'response',newdata = test_set[,-10])

y_pred = ifelse(prop_pred > 0.5,1,0) 

cm = table(test_set$Exited,y_pred)



regressor3 = glm(formula = Exited ~ CreditScore  + Gender + Age + Balance  + IsActiveMember,
                 family="binomial", data=training_set)
summary(regressor3)


prop_pred <- predict(regressor3,type = 'response',newdata = test_set[,-10])

y_pred = ifelse(prop_pred > 0.5,1,0) 

cm = table(test_set$Exited,y_pred)

pR2(regressor)
pR2(regressor2)
pR2(regressor3)



test_set$prob=y_pred
install.packages("pROC")
library(ROCR)
g <- roc(Exited ~ prob, data = test_set)
plot(g) 
y_pred=as.factor(y_pred)
class(y_pred)
test_set$prob_ppred=prob_pred
prf=prediction(y_pred,test_set$Exited)

