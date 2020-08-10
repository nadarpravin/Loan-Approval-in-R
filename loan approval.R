set.seed(99)

library(readr)
library(modeest)
library(moments)
library(Amelia)
library(car)
library(caTools)
library(ROCR)
library(gplots)


test <- read_csv("Credit_Risk_Test_data.csv")
View(test)

train <- read_csv("Credit_Risk_Train_data.csv")
View(train)

validate <- read_csv("Credit_Risk_Validate_data.csv")
View(validate)

dim(test)
dim(train)
dim(validate)

summary(test)
summary(train)
summary(validate)

colnames(test)
colnames(train)
colnames(validate)


missmap(test,col = c("red","yellow"))
missmap(train,col = c("blue","green"))
missmap(validate,col = c("white","black"))


colSums(is.na(test))
colSums(is.na(train))
colSums(is.na(validate))

sort(colSums(is.na(test)))
class(test$Credit_History)
median(test$Credit_History,na_rm=TRUE)
mfv(test$Credit_History)

#imputation for test

test$Credit_History[which(is.na(test$Credit_History))]<-mfv(test$Credit_History,na_rm = TRUE)
test$Self_Employed[which(is.na(test$Self_Employed))]<-mfv(test$Self_Employed,na_rm=TRUE)
test$Gender[which(is.na(test$Gender))]<-mfv(test$Gender,na_rm=TRUE)
test$Dependents[which(is.na(test$Dependents))]<-mfv(test$Dependents,na_rm=TRUE)
test$Loan_Amount_Term[which(is.na(test$Loan_Amount_Term))]<-median(test$Loan_Amount_Term,na.rm=TRUE)
test$LoanAmount[which(is.na(test$LoanAmount))]<-median(test$LoanAmount,na.rm = TRUE)
sum(is.na(test$LoanAmount))
sort(colSums(is.na(test)))


#imputation for train

sort(colSums(is.na(train)))
train$Credit_History[which(is.na(train$Credit_History))]<-mfv(train$Credit_History,na_rm = TRUE)
train$LoanAmount[which(is.na(train$LoanAmount))]<-median(train$LoanAmount,na.rm = TRUE)
train$Self_Employed[which(is.na(train$Self_Employed))]<-"No"

train$Dependents[which(is.na(train$Dependents))]<-mfv(train$Dependents,na_rm=TRUE)
train$Loan_Amount_Term[which(is.na(train$Loan_Amount_Term))]<-median(train$Loan_Amount_Term,na.rm=TRUE)
train$Gender[which(is.na(train$Gender))]<-mfv(train$Gender,na_rm=TRUE)
train$Married[which(is.na(train$Married))]<-mfv(train$Married,na_rm=TRUE)
median(train$Loan_Amount_Term,na.rm=TRUE)
sum(is.na(train))


#imputation for validate

sort(colSums(is.na(validate)))
validate$Credit_History[which(is.na(validate$Credit_History))]<-mfv(validate$Credit_History,na_rm = TRUE)
validate$Self_Employed[which(is.na(validate$Self_Employed))]<-"No"
mfv(validate$Self_Employed)
validate$Gender[which(is.na(validate$Gender))]<-mfv(validate$Gender,na_rm=TRUE)
validate$Dependents[which(is.na(validate$Dependents))]<-mfv(validate$Dependents,na_rm=TRUE)
validate$Loan_Amount_Term[which(is.na(validate$Loan_Amount_Term))]<-360
validate$LoanAmount[which(is.na(validate$LoanAmount))]<-125
median(validate$LoanAmount,na.rm = TRUE)
median(validate$Loan_Amount_Term,na.rm = TRUE)

#missingness
missmap(test,col = c("red","yellow"))
missmap(train,col = c("blue","green"))
missmap(validate,col = c("white","black"))

colSums(is.na(test))
colSums(is.na(train))
colSums(is.na(validate))


colnames(test)
colnames(train)
colnames(validate)
##visualization

#gender
barplot(table(train$Gender),col=c("pink","blue"))
mfv(train$Gender)
table(train$Gender)

#marriage status
barplot(table(train$Married),col=c("pink","blue"))
table(train$Married)

#dependents
barplot(table(train$Dependents),col=c("pink","blue","red","yellow"))
table(train$Dependents)

#education
barplot(table(train$Education),col=c("pink","blue"))
table(train$Education)

#selfemployed
barplot(table(train$Self_Employed),col=c("pink","blue"))
table(train$Self_Employed)

#loanamount
hist(train$LoanAmount,col = "green",xlim = c(0,600)) #skewness present
max(train$LoanAmount)
min(train$LoanAmount)

#applicant imcome
hist(train$ApplicantIncome,col = "blue",xlim = c(0,30000))
skewness(train$ApplicantIncome)

#loanterm
hist(train$Loan_Amount_Term,col = "orange",xlim = c(0,1000))
skewness(train$Loan_Amount_Term)

#coapplicant
which.max(train$CoapplicantIncome)
which.min(train$CoapplicantIncome)
hist(train$CoapplicantIncome,col = "pink",xlim = c(0,20000))

#cred history
barplot(table(train$Credit_History),col = c("yellow","red"))

#relationship
plot(table(train$Loan_Status,train$Gender),col=c("black","white"))


plot(table(train$Loan_Status,train$Married),col=c("yellow","red"))

plot(table(train$Dependents,train$Loan_Status),col=c("yellow","red"))

plot(table(train$Education,train$Loan_Status),col=c("yellow","red"))

plot(table(train$Self_Employed,train$Loan_Status),col=c("yellow","red"))

plot(table(train$Loan_Amount_Term,train$Loan_Status),col=c("yellow","red"))

table(train$Loan_Amount_Term)
range(train$Loan_Amount_Term)
median(train$Loan_Amount_Term)
fivenum(train$Loan_Amount_Term)

boxplot(train$CoapplicantIncome~train$Loan_Status,col=c("yellow","red"),ylim=c(0,8000))
boxplot(train$ApplicantIncome~train$Loan_Status,col=c("yellow","red"),ylim=c(0,20000))
boxplot(train$LoanAmount~train$Loan_Status,col=c("yellow","red"),ylim=c(0,400))

plot(table(as.factor(train$LoanAmount),train$Loan_Status),col=c("yellow","red"))
plot(table(as.factor(train$Credit_History),train$Loan_Status),col=c("yellow","red"))


#changing dataset to numeric for glm
train$Gender<-as.numeric(ifelse(train$Gender=="Male",1,0))
train$Married<-as.numeric(ifelse(train$Married=="Yes",1,0))
train$Self_Employed<-as.numeric(ifelse(train$Self_Employed=="Yes",1,0))

train$Education<-as.numeric(ifelse(train$Education=="Yes",1,0))
train$Loan_Status<-as.numeric(ifelse(train$Loan_Status=="Y",1,0))
train$Dependents<-as.numeric(ifelse(train$Dependents=="3+",3,train$Dependents))
train$Property_Area_Urban<-as.numeric(ifelse(train$Property_Area=="Urban",1,0))
train$Property_Area_Rural<-as.numeric(ifelse(train$Property_Area=="Rural",1,0))


train$Loan_ID<-NULL
train$Property_Area<-NULL

#changing dataset for test

test$Gender<-as.numeric(ifelse(test$Gender=="Male",1,0))
test$Married<-as.numeric(ifelse(test$Married=="Yes",1,0))
test$Self_Employed<-as.numeric(ifelse(test$Self_Employed=="Yes",1,0))
test$Education<-as.numeric(ifelse(test$Education=="Yes",1,0))
test$Dependents<-as.numeric(ifelse(test$Dependents=="3+",3,test$Dependents))
test$Property_Area_Urban<-as.numeric(ifelse(test$Property_Area=="Urban",1,0))
test$Property_Area_Rural<-as.numeric(ifelse(test$Property_Area=="Rural",1,0))

test$Loan_ID<-NULL
test$Property_Area<-NULL


#conversion for validate data set
validate$Gender<-as.numeric(ifelse(validate$Gender=="Male",1,0))
validate$Married<-as.numeric(ifelse(validate$Married=="Yes",1,0))
validate$Self_Employed<-as.numeric(ifelse(validate$Self_Employed=="Yes",1,0))
validate$Education<-as.numeric(ifelse(validate$Education=="Yes",1,0))
validate$outcome<-as.numeric(ifelse(validate$outcome=="Y",1,0))
validate$Dependents<-as.numeric(ifelse(validate$Dependents=="3+",3,validate$Dependents))
validate$Property_Area_Urban<-as.numeric(ifelse(validate$Property_Area=="Urban",1,0))
validate$Property_Area_Rural<-as.numeric(ifelse(validate$Property_Area=="Rural",1,0))


validate$Loan_ID<-NULL
validate$Property_Area<-NULL


#creating model
mod_1<-glm(Loan_Status~.,data = train,family = "binomial")

summary(mod_1)

predictvalid = predict(mod_1,newdata= validate,type="response") #z value should be high,p value to be low
length(predictvalid)
validate$outcome[1:10]
predictvalid[1:10]
table(validate$outcome,predictvalid>0.5) #threshold value 0.5 ,50% above #20 errors
accuracy<-(58+289)/(58+19+1+289)
print(accuracy)

table(validate$outcome,predictvalid>0.75)
accuracy1<-(70+209)/(70+7+81+209)
print(accuracy1)


table(validate$outcome,predictvalid>0.65)
accuracy2<-(60+279)/(60+17+11+279)
print(accuracy2)  #it seems accuracy 1 was better

#but in real life scenario we dont do that manually

predicttest<-predict(mod_1,newdata = test,type = "response")

predicttest
validate$outcome
table(validate$outcome)
sum(is.na(validate$outcome))
sum(is.na(predicttest))
median(predicttest,na.rm = TRUE)


#rocpred
rocpred<-prediction(predicttest,validate$outcome)
rocpred

rocrpref<-performance(rocpred,"tpr","fpr")
plot(rocrpref)
plot(rocrpref,print.cutoffs.at=seq(0.1,1),colorize=TRUE)

#to calculate area under curve
as.numeric(performance(rocpred,"auc")@y.values)

#for adding aditional column in test dataset
test$approval<-ifelse(predicttest>0.5,"Yes","No")
table(test$approval)
