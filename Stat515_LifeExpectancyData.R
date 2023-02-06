library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(leaps)
library(car)
library(Metrics)
library(reshape2)
library(ggpubr)
library(moments)
data <- read.csv("C:\\Users\\SAURABH SINHA\\OneDrive\\Desktop\\GMU\\Life Expectancy Data.csv")
head(data)
str(data)
dim(data)
summary(data)
sum(is.na(data))
table(is.na(data))
names(data)
names(data)[names(data) == "Income.composition.of.resources"] <- "Income.Composition"
names(data)[names(data) == "percentage.expenditure"] <- "PExpenditure"


#returns number of rows after removing missing values
?na.omit
dim(na.omit(data))
wo_na<-na.omit(data)

#To calculate missing rows 
miss_rows<- dim(data)[1]-dim(na.omit(data))[1]
miss_rows

#The percent of rows missing
nam<-c('Missing','Not Missing')
values<- c(miss_rows,dim(na.omit(data))[1])
tot_rows<- tibble(nam,values)
percent_miss<- miss_rows*100/dim(data)[1]
percent_miss
tot_rows

ggplot(tot_rows,aes(x='',y=values,fill=nam))+
  geom_bar(position = 'stack',stat = 'identity')+
  ggtitle("Missing Vs Non Missing Values")+
  xlab('')+ylab('Count')+
  labs(fill = 'Type')
  #geom_text(aes(label=values),vjust=1.5)

#Missing values are about 40% which cannot be ignored, hence we will impute data with values
#To Impute values, we will have to impute after checking the boxplot.
#The variables with high number of outliers will be imputed with median.
#The variables with low number of outliers will be imputed with mean.
par(mfrow=c(2,7))
names(data)
boxplot(data$Life.expectancy,
        ylab = "Life Expectancy")
boxplot(data$Adult.Mortality,
        ylab = "Adult Mortality")
#boxplot(data$infant.deaths,
#       ylab = "Infant deaths")
boxplot(data$Alcohol,
        ylab = "Alcohol")
#boxplot(data$percentage.expenditure,
 #       ylab = "percentage.expenditure")
boxplot(data$Hepatitis.B,
        ylab = "Hepatitis.B")
#boxplot(data$Measles,
 #       ylab = "Measles")
boxplot(data$BMI ,
        ylab = "BMI ")
boxplot(data$Polio,
        ylab = "Polio")
boxplot(data$Total.expenditure,
        ylab = "Total.expenditure")
boxplot(data$Diphtheria,
        ylab = "Diphtheria")
#boxplot(data$HIV.AIDS,
#        ylab = "HIV.AIDS")
boxplot(data$GDP,
        ylab = "GDP")
boxplot(data$Population,
        ylab = "Population")
boxplot(data$thinness..1.19.years,
        ylab = "thinness..1.19.years")
boxplot(data$thinness.5.9.years,
        ylab = "thinness.5.9.years")
boxplot(data$Income.Composition,
        ylab = "Income Composition")
boxplot(data$Schooling,
        ylab = "Schooling")
#We Can see that Adult Mortality,Life expectancy, Hepatitis B, Polio, Total expenditure,Diphtheria,GDP,
#Thinnes,Population, Schooling all i have extreme outliers present. So we should calculate the median
Life_exp_med<- median(data$Life.expectancy,na.rm = TRUE)
Adult_mor_med<- median(data$Adult.Mortality,na.rm = TRUE)
#Alcohol_med<- median(data$Alcohol,na.rm = TRUE)
Hep_med<- median(data$Hepatitis.B,na.rm = TRUE)
#BMI_med<- median(data$BMI,na.rm = TRUE)
Polio_med<- median(data$Polio,na.rm = TRUE)
Tot_exp_med<- median(data$Total.expenditure,na.rm = TRUE)
Diph_med<- median(data$Diphtheria,na.rm = TRUE)
GDP_med<- median(data$GDP,na.rm = TRUE)
thin1_med<- median(data$thinness..1.19.years,na.rm = TRUE)
thin5_med<- median(data$thinness.5.9.years,na.rm = TRUE)
sch_med<- median(data$Schooling,na.rm = TRUE)
Pop_med<- median(data$Population,na.rm = TRUE)

#Lets calculate the mean for the rest of the variables.
Alcohol_mn<- mean(data$Alcohol, na.rm = TRUE)
BMI_mn<- mean(data$BMI,na.rm = TRUE)
Inc_mn<- mean(data$Income.Composition,na.rm = TRUE)
data$`Income.Composition`

# We must now impute values into the variables
#Imputing Medians
data$Life.expectancy[is.na(data$Life.expectancy)]<-Life_exp_med
data$Adult.Mortality[is.na(data$Adult.Mortality)]<-Adult_mor_med
data$Hepatitis.B[is.na(data$Hepatitis.B)]<-Hep_med
data$Polio[is.na(data$Polio)]<-Polio_med
data$Total.expenditure[is.na(data$Total.expenditure)]<-Tot_exp_med
data$Diphtheria[is.na(data$Diphtheria)]<-Diph_med
data$GDP[is.na(data$GDP)]<-GDP_med
data$thinness..1.19.years[is.na(data$thinness..1.19.years)]<-thin1_med
data$thinness.5.9.years[is.na(data$thinness.5.9.years)]<-thin5_med
data$Schooling[is.na(data$Schooling)]<-sch_med
data$Population[is.na(data$Population)]<-Pop_med

#Imputing Means
data$Alcohol[is.na(data$Alcohol)]<-Alcohol_mn
data$BMI[is.na(data$BMI)]<-BMI_mn
data$Income.Composition[is.na(data$Income.Composition)]<-Inc_mn

# We can see that there are no missing values now.
summary(data)

#Factorizing Status Variable
data$Status<- as.factor(data$Status)

#copy of data
cop_data<-data

#Lets See the Boxplot of the Variable Status
ggplot(data,aes(x=Status,y =Life.expectancy, fill=Status))+
  geom_boxplot()+
  ggtitle("Box Plot for Status")

#Developed Countries have a higher life expectancy mean.
#Hence is a good predictor

#Now We need to check the correlation between the reponse and predictors. This is done to eliminate
#variables which do not add to the model but only increase complexity. Also, this can help to 
#avoid multi-collinearity.

?subset
corr_matrix <- round(cor(subset(data, select =-c(Status,Country))), 3)
ggcorrplot(corr_matrix,lab = TRUE,lab_size = 3,type='lower')+
  ggtitle('Correlation matrix')

#We can see that a few variables have high correlation between them, this can affect the model.
#Under.5.deaths & infant.deaths,GDP & Percent.expenditure, thinnes1.5 & thinnes5.9 have high correlation
#amongst each other.
#Also, there is a variable with correlation as 1
#Lets also check the vifs
par(mfrow=c(1,1))
fake_lm<- lm(Life.expectancy~.-Country,data=data)
summary(fake_lm)
vif(fake_lm)
vif_vals<- data.frame(vif(fake_lm))
head(vif_vals)
names(vif_vals)[names(vif_vals) == "vif.fake_lm."] <- "vifs"
head(vif_vals)
#dev.new(width=5, height=4, unit="in")
#vifplot
ggplot(vif_vals, aes(y=vifs, x=row.names(vif_vals))) + 
  geom_bar(aes(fill=vifs>5),stat="identity")+
  scale_y_continuous(trans = "sqrt",  breaks = c(5, 10, 50, 100))+
  geom_hline(yintercept = 5, colour = "red") + 
  ggtitle("VIF per feature") +
  xlab("Featurs") + ylab("VIF") +
  theme(axis.text.x=element_text(angle=20, hjust=1))
  #theme(text = element_text(size = 18))
  #scale_fill_brewer(palette="Dark2")

#We shall remove 1 variable within each pair of highly correlated features and check VIFs again.
data2 <- subset(data,select= -c(infant.deaths,thinness..1.19.years,GDP))
names(data2)
#Correlation Matrix and plot
corr_matrix2 <- round(cor(subset(data2, select =-c(Status,Country))), 3)
ggcorrplot(corr_matrix2,lab = TRUE,lab_size = 3,type='lower')+
  ggtitle('Correlation matrix 2')

#VIFs plot 2
fake_lm2<- lm(Life.expectancy~.-Country,data=data2)
summary(fake_lm2)
vif(fake_lm2)
vif_values<- data.frame(vif(fake_lm2))
head(vif_values)
names(vif_values)[names(vif_values) == "vif.fake_lm2."] <- "VIFS"
head(vif_values)
#VIFS plot 2
ggplot(vif_values, aes(y=VIFS, x=row.names(vif_values))) + 
  geom_bar(aes(fill=VIFS),stat="identity")+
  scale_y_continuous(trans = "sqrt",  breaks = c(5, 10, 50, 100))+
  geom_hline(yintercept = 5, colour = "red") + 
  ggtitle("VIF per feature") +
  xlab("Features") + ylab("VIF") +
  theme(axis.text.x=element_text(angle=20, hjust=1))

#Now all the VIFS have been checked and are under control.
#We shall now select the best features using subset selection
#3 methods
# Best-fit subset selection
# forward stepwise
# backward stepwise
dim(data2)
#Best- fit Subset selection method
regfit.best<- regsubsets(Life.expectancy~.-Country,data =data2,nvmax = 16)
regbest_sum<- summary(regfit.best)
par(mfrow=c(2,2))
regbest_sum
#plotting RSS according to bestfit
which.min(regbest_sum$rss)
#gives output as 16
plot(regbest_sum$rss,xlab ='Number of Variables', ylab = 'RSS', type ="l")
points(16, regbest_sum$rss[16], col ='red',pch=20,cex=2)

#plotting Adjusted R^2
which.max(regbest_sum$adjr2)
#gives output 16
plot(regbest_sum$adjr2,xlab="Number of Variables",ylab="Adjusted Rsq",type="l")
points(16,regbest_sum$adjr2[16], col="red",cex=2,pch=20)

#plotting Mallows CP
which.min(regbest_sum$cp)
#Guves output 15
plot(regbest_sum$cp,xlab="Number of Variables",ylab="Mallows CP",type="l")
points(15,regbest_sum$cp[15], col="red",cex=2,pch=20)

#plotting BIC
which.min(regbest_sum$bic)
#gives output 12
plot(regbest_sum$bic,xlab="Number of Variables",ylab="BIC",type="l")
points(12,regbest_sum$bic[12], col="red",cex=2,pch=20)

#All metrics suggested either 15 , 16 or 12 as the number of variables to be selected using best subset.

bestf_coefs<-coef(regfit.best,12)

#Lets try forward stepwise
regfit.fwd<- regsubsets(Life.expectancy~.-Country,data2,method='forward',nvmax= 16)
fwd_sum<-summary(regfit.fwd)
par(mfrow = c(1,1))
which.min(fwd_sum$rss)
#lets check BIC of variables suggested by Forward
which.min(fwd_sum$bic)
#Gives 12 as output
?plot
plot(fwd_sum$bic,ylab = 'BIC', xlab = "No of Variables",type="l" ,main = "Forward Subset Selection")
points(12,fwd_sum$bic[12],col = 'red', cex = 2, pch =20)
#Forward BIC suggests 12 variables
fwd_coefs <- coef(regfit.fwd,12)
fwd_coefs

#Backward stepwise
regfit.bwd <- regsubsets(Life.expectancy~.-Country,data2,nvmax=16,method="backward")
bwd.sum <- summary(regfit.bwd)
which.min(bwd.sum$bic)
#gives output 12
plot(bwd.sum$bic,xlab="Number of Variables",ylab="BIC",type='l',main = "Backward Subset Selection")
points(12,bwd.sum$bic[12],col="red",cex=2,pch=20)
#the 12 variables 
bwd_coefs<- coef(regfit.bwd,12)
bwd_coefs

#The 3 subset selection methods have lowest BIC and it suggests 12 variables which are Status,
#Adult_mortality,% expenditure, Hepatitis B, Measles, BMi, Under5.deaths,Polio,Diphtheria,HIV
#Income,Schooling
names(data2)
#We will now save these in features in another dataframe and run our linear regression model on it
data3<- subset(data2, select =- c(Country,Alcohol,Total.expenditure,Population,thinness.5.9.years,Year))

#Lets run our linear model on the newly created dataset suggested by the subset selection methods.
set.seed(11)

#We shall first divide our dataset into trainiing and testing.
sample<- sample(c(TRUE,FALSE),nrow(data3),replace = TRUE, prob=c(0.70,0.30))
train<- data3[sample,]
x.test<-data3[!sample,]
y.test<-data3[!sample,]$Life.expectancy

lm_model<-lm(Life.expectancy~.,data=train)
summary(lm_model)

#We can observe that not all predictors are significant
#lets check its rmse,adjR2 and diagnosti plots.

pred<- predict(lm_model,newdata = x.test)
mean((pred-y.test)^2)
rmse(pred,y.test)
#RMSE comes out to be 4.2
summary(lm_model)$adj.r.squared
#Adjusted R2 comes to be 0.81

par(mfrow=c(2,2))
plot(lm_model)

#The plots show linearity .

# Lasso Regression---------------------------------------------------------------------

# Lasso regression does not use the formula interface so need to create
# a y vector and an x matrix
data4<- subset(data3,select=-c(Status))
y=data4$Life.expectancy
x=model.matrix(Life.expectancy~., data4)[,-1] # removes first column of 1’s

set.seed(400000)
train2=sample(1:nrow(x), size=nrow(x)/2)

# Using training set to perform lasso regression for 100 lambda values

library(glmnet)

grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x[train2,],y[train2],alpha=1,lambda=grid)

# Cross validation of the training set determines the lambda minimizing MSE.
# Also, determines lambda for the 1-standard-error rule.

set.seed(400000)
cv.out=cv.glmnet(x[train2,],y[train2],alpha=1, nfold=10)
plot(cv.out)

(bestlam = cv.out$lambda.min)
(bestlam.1se=cv.out$lambda.1se)

# Plot coefficient paths
par(mfrow=c(1,1))
plot(cv.out$glmnet.fit, xvar="lambda", label=TRUE)

# For optimal values of lambda
abline(v=log(c(bestlam, bestlam.1se)), lty=2)

# Determine test-set MSE for lambda minimizing cross-validated training-set MSE
test=(-train2)
y.test=y[test]

lasso.pred.min=predict(lasso.mod,s=bestlam, newx=x[test,])
(MSE.lasso.min =mean((lasso.pred.min - y.test)^2))
sqrt(MSE.lasso.min)

# Determine test-set MSE for lambda from 1-standard-error rule
lasso.pred.1se=predict(lasso.mod, s=bestlam.1se, newx=x[test,])
MSE.lasso.1se = mean((lasso.pred.1se-y.test)^2)

# Finally, using the full data set to perform lasso regression and
# display non-zero coefficients for best lambda determined from
# cross validation of the training set


out=glmnet(x ,y, alpha=1, lambda=grid)
lasso.coef.min=predict(out,type="coefficients",s=bestlam)[1:12,]
round(lasso.coef.min[lasso.coef.min!=0],2)

(nvars <- length(lasso.coef.min[lasso.coef.min!=0])-1)

# Also, display non-zero coefficients for lambda from 1se rule
lasso.coef.1se=predict(out,type="coefficients",s=bestlam.1se)[1:12,]
round(lasso.coef.1se[lasso.coef.1se!=0],2)
(nvars <- length(lasso.coef.1se[lasso.coef.1se!=0])-1)

