#installed.packages()
#install.packages("caTools")
#install.packages("ROCR")
#install.packages("mice") 
#install.packages("Hmisc")

telco=read.csv("Telco-Customer-Churn.csv")

summary(telco)
library(ggplot2)
qplot(telco$Churn, color=redblue(1), data=telco)
table(telco$Churn)
5174/(5174+1869) #73.5% - Baseline Accuracy

# Handling the NA in 'TotalCharges' variable
library(mice)
md.pattern(telco)

telco$InternetServiceFactors=as.numeric(telco$InternetService)
telco$StreamingTVFactors=as.numeric(telco$StreamingTV)
telco$StreamingMoviesFactors=as.numeric(telco$StreamingMovies)
telco$DeviceProtectionFactors=as.numeric(telco$DeviceProtection)
telco$OnlineSecurityFactors=as.numeric(telco$SOnlineSecurity)

cor(telco[c("StreamingTVFactors","StreamingMoviesFactors")],use="complete.obs")
cor(telco[c("InternetServiceFactors","StreamingTVFactors")],use="complete.obs")
cor(telco[c("InternetServiceFactors","StreamingMoviesFactors")],use="complete.obs")
cor(telco[c("InternetServiceFactors","DeviceProtectionFactors")],use="complete.obs")
# correlation - 0.459904


with(telco, table(InternetService,StreamingTV))
with(telco, table(InternetService,StreamingMovies))
with(telco, table(InternetService,DeviceProtection))
with(telco, table(InternetService,OnlineSecurity))

with(telco, table(StreamingTV,StreamingMovies))
with(telco, table(DeviceProtection,OnlineSecurity))

alias(glm(Churn ~ . -customerID, data = telco, family=binomial))



set.seed(144)
vars.for.imputation = c('MonthlyCharges','TotalCharges')
vars.for.imputation
imputed = complete(mice(telco[c("MonthlyCharges","TotalCharges")]))
telco[vars.for.imputation] = imputed


library(caTools)
set.seed(99)
split=sample.split(telco$Churn, SplitRatio=0.7)
split
telcoTrain=subset(telco,split==TRUE)
telcoTest=subset(telco,split==FALSE)
nrow(telcoTrain)
nrow(telcoTest)




# Model with all variables
churnLog=glm(Churn ~ . -customerID, data=telcoTrain, family=binomial)
summary(churnLog)

# Stepwise Selection
library(MASS)
step=stepAIC(churnLog, direction="both")


# Stepwise selected variables
churnLog1=glm(Churn ~ SeniorCitizen + tenure + MultipleLines + InternetService + 
                OnlineSecurity + DeviceProtection + TechSupport + StreamingTV + 
                StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
                MonthlyCharges + TotalCharges, data=telcoTrain, family=binomial)

churnLog1=glm(Churn ~ SeniorCitizen + tenure + MultipleLines + InternetService + 
                Contract + PaperlessBilling + PaymentMethod + 
                MonthlyCharges + TotalCharges, data=telcoTrain, family=binomial)

#Dhaval
churnLog1=glm(Churn ~ SeniorCitizen + tenure + MultipleLines + InternetService + 
                Contract + PaperlessBilling + PaymentMethod + 
                MonthlyCharges + TotalCharges, data=telcoTrain, family=binomial)

summary(churnLog1)

# Anova comparison
anova(churnLog, churnLog1)

# Training set prediction
predictTrain=predict(churnLog1,type="response",)
summary(predictTrain1)

length(predictTrain1)
nrow(telcoTrain)
head(predictTrain1)

tapply(predictTrain1, telcoTrain$Churn, mean)
table(telcoTrain$Churn, predictTrain1>0.5)
table(telcoTrain$Churn, predictTrain1>0.7)
table(telcoTrain$Churn, predictTrain1>0.3)

library(ROCR)
ROCRpred=prediction(predictTrain1, telcoTrain$Churn)
ROCRperf=performance(ROCRpred,"tpr","fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2))
auc.perf=performance(ROCRpred,measure="auc")
auc.perf@y.values
#0.8414948

# Test set prediction
predictTest1 = predict(churnLog1, newdata=telcoTest, type="response")
table(telcoTest$Churn, predictTest1 >= 0.3)

ROCRpred=prediction(predictTest1, telcoTest$Churn)
ROCRperf=performance(ROCRpred,"tpr","fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1),text.adj=c(0.2))
auc.perf=performance(ROCRpred,measure="auc")
auc.perf@y.values
#0.8610929


# distribution of the prediction score grouped by known outcome
ggplot(telcoTrain, aes( predictTrain1, color = as.factor(Churn) ) ) + 
  geom_density( size = 1 ) +
  ggtitle( "Training Set's Predicted Score" ) + 
  scale_color_economist( name = "data", labels = c( "negative", "positive" ) ) + 
  theme_economist()


# visualize .6 cutoff (lowest point of the previous plot)
cm_info <- ConfusionMatrixInfo( data = telcoTest, predict = "prediction", 
                                actual = "left", cutoff = .6 )
ggthemr("flat")
cm_info$plot
