
library(readxl)
Data <- read_excel("D:/1.Sofia University Bechelor/Second Course/Second Term/2. Statistics and Empirical Methods/Binary-Logistic-Regression/Data.xlsx")
View(Data)


Data[!complete.cases(Data),]

install.packages("ggplot2")

summary(Data)

Data = Data[,-1]
View(Data)
Data = Data[,-2]
View(Data)

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

Data$Geography<-Data$Geography(c("France","Spain","Germany"),100,replace= TRUE)
barplot(table(Data$Geography), col="red")

Data$Gender<-Data$Gender(c("Male","Female"),100,replace= TRUE)
barplot(table(Data$Gender), col="blue")

#categorical Data encoding
Data$Geography = factor(Data$Geography,
                       levels = c('France', 'Spain', 'Germany'),
                       labels = c(1, 2, 3))



#categorical Data encoding
Data$Gender = factor(Data$Gender,
                        levels = c('Male', 'Female'),
                        labels = c(1, 2))
