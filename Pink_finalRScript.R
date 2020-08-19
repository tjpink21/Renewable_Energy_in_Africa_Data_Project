library(ggplot2)
library(caret)
library(randomForest)
library(rJava)
library(bartMachine)
library(glmnet)
library(lme4)
library(tree)
library(bootstrap)
library(rpart)
library(gbm)
library(ipred)
library(car)
library(tstools)
set.seed(1111)
library(corrplot)
ARE_data<-readxl::read_excel("~/Desktop/Data Mining/Pink_RData_FinalData.xlsx")
#Dota has been preprocessed and clened
ARE_data<-ARE_data[,-c(1,2)]
ARE_data$solar_gen<-as.numeric(ARE_data$solar_gen)
ARE_data$Pol_Stab<-as.numeric(ARE_data$Pol_Stab)
ARE_data$GDP_anper<-as.numeric(ARE_data$GDP_anper)
ARE_data$Response<-ARE_data$Response/ARE_data$Total_Pop

preproc2 <- preProcess(ARE_data[,c(4,5,6,7)], method=c("range"),rangeBounds = c(0, 10))
norm <- predict(preproc2, ARE_data[,c(4,5,6,7)])
ARE_data$Total_Pop<-norm$Total_Pop
ARE_data$`Urban pop`<-norm$`Urban pop`
ARE_data$Response<-norm$Response
ARE_data$solar_gen<-norm$solar_gen
i=1
while (i<=length(ARE_data$solar_gen)){
  if(ARE_data$Year[i]<2000){
    ARE_data<-ARE_data[-i,]
  }
  else{
    i<-i+1
  }
}
SA<-NA
for (i in 1:length(ARE_data$Country)){
  if(ARE_data$Country[i]=="SOUTH AFRICA"){
    SA<- rbind(SA, ARE_data[i,])
  }
}
SA<-SA[-c(1),]
Gabon<-NA
for (i in 1:length(ARE_data$Country)){
  if(ARE_data$Country[i]=="GABON"){
    Gabon<- rbind(Gabon, ARE_data[i,])
  }
}
Gabon<-Gabon[-c(1),]
Mor<-NA
for (i in 1:length(ARE_data$Country)){
  if(ARE_data$Country[i]=="MOROCCO"){
    Mor<- rbind(Mor, ARE_data[i,])
  }
}
Mor<-Mor[-c(1),]
Ghana<-NA
for (i in 1:length(ARE_data$Country)){
  if(ARE_data$Country[i]=="GHANA"){
    Ghana<- rbind(Ghana, ARE_data[i,])
  }
}
Ghana<-Ghana[-c(1),]
Zam<-NA
for (i in 1:length(ARE_data$Country)){
  if(ARE_data$Country[i]=="ZAMBIA"){
    Zam<- rbind(Zam, ARE_data[i,])
  }
}
Zam<-Zam[-c(1),]
Togo<-NA
for (i in 1:length(ARE_data$Country)){
  if(ARE_data$Country[i]=="TOGO"){
    Togo<- rbind(Togo, ARE_data[i,])
  }
}
Togo<-Togo[-c(1),]
Egypt<-NA
for (i in 1:length(ARE_data$Country)){
  if(ARE_data$Country[i]=="EGYPT"){
    Egypt<- rbind(Egypt, ARE_data[i,])
  }
}
Egypt<-Egypt[-c(1),]
#sa_dat$Response<-as.character(sa_dat$Response)
ggplot() +
  geom_line(data = SA, aes(x = Year, y = Response, color = "South Africa",group=1), size = 1)+
  geom_line(data = Gabon, aes(x = Year, y = Response, color = "Gabon",group=1), size = 1)+
  geom_line(data = Mor, aes(x = Year, y = Response, color = "Morocco",group=1), size = 1)+
  geom_line(data = Ghana, aes(x = Year, y = Response, color = "Ghana",group=1), size = 1)+
  geom_line(data = Zam, aes(x = Year, y = Response, color = "Zambia",group=1), size = 1)+
  geom_line(data = Egypt, aes(x = Year, y = Response, color = "Egypt",group=1), size = 1)+
  geom_line(data = Togo, aes(x = Year, y = Response, color = "Togo",group=1), size = 1)+
  xlab("Year") +
  ylab("Sucessful Renewable Energy Investment (USD per citizen)")+
  ggtitle("Top Countries for Past Investments")

ggplot() +
  geom_line(data = SA, aes(x = Year, y = GDP_anper, color = "South Africa",group=1), size = 1)+
  geom_line(data = Gabon, aes(x = Year, y = GDP_anper, color = "Gabon",group=1), size = 1)+
  geom_line(data = Mor, aes(x = Year, y = GDP_anper, color = "Morocco",group=1), size = 1)+
  geom_line(data = Ghana, aes(x = Year, y = GDP_anper, color = "Ghana",group=1), size = 1)+
  geom_line(data = Zam, aes(x = Year, y = GDP_anper, color = "Zambia",group=1), size = 1)+
  geom_line(data = Egypt, aes(x = Year, y = GDP_anper, color = "Egypt",group=1), size = 1)+
  geom_line(data = Togo, aes(x = Year, y = GDP_anper, color = "Togo",group=1), size = 1)+
  xlab("Year") +
  ylab("Yearly change in GDP (%)")+
  ggtitle("Top Countries for Past Investments GDP")

ggplot() +
  geom_line(data = SA, aes(x = Year, y = Pol_Stab, color = "South Africa",group=1), size = 1)+
  geom_line(data = Gabon, aes(x = Year, y = Pol_Stab, color = "Gabon",group=1), size = 1)+
  geom_line(data = Mor, aes(x = Year, y = Pol_Stab, color = "Morocco",group=1), size = 1)+
  geom_line(data = Ghana, aes(x = Year, y = Pol_Stab, color = "Ghana",group=1), size = 1)+
  geom_line(data = Zam, aes(x = Year, y = Pol_Stab, color = "Zambia",group=1), size = 1)+
  geom_line(data = Egypt, aes(x = Year, y = Pol_Stab, color = "Egypt",group=1), size = 1)+
  geom_line(data = Togo, aes(x = Year, y = Pol_Stab, color = "Togo",group=1), size = 1)+
  xlab("Year") +
  ylab("Political Stability Rating")+
  ggtitle("Top Countries for Past Investments Political Stability")
ggplot() +
  geom_line(data = SA, aes(x = Year, y = urban_ratio, color = "South Africa",group=1), size = 1)+
  geom_line(data = Gabon, aes(x = Year, y = urban_ratio, color = "Gabon",group=1), size = 1)+
  geom_line(data = Mor, aes(x = Year, y = urban_ratio, color = "Morocco",group=1), size = 1)+
  geom_line(data = Ghana, aes(x = Year, y = urban_ratio, color = "Ghana",group=1), size = 1)+
  geom_line(data = Zam, aes(x = Year, y = urban_ratio, color = "Zambia",group=1), size = 1)+
  geom_line(data = Egypt, aes(x = Year, y = urban_ratio, color = "Egypt",group=1), size = 1)+
  geom_line(data = Togo, aes(x = Year, y = urban_ratio, color = "Togo",group=1), size = 1)+
  xlab("Year") +
  ylab("Urban Population")+
  ggtitle("Top Countries for Past Investments Urban Populaiton")

ARE_data<-as.data.frame(ARE_data)
train_samp = sample(1:nrow(ARE_data),0.80*nrow(ARE_data))
test_samp = -train_samp
test = ARE_data[test_samp,]
train = ARE_data[train_samp,]

summary(norm)
a<-cor(ARE_data[c("Response","GDP_anper","Total_Pop","urban_ratio","Pol_Stab","solar_gen")])
corrplot(a,method= "circle")

lm<-lm(Response~GDP_anper+Total_Pop+urban_ratio+Pol_Stab+solar_gen, data=train)
summary(lm)
y_hat.lm <- predict(lm, newdata = train)
resc <- train$Response-y_hat.lm
lm_RMSE_train <- RMSE(train$Response,y_hat.lm)
y_hat.lm <- predict(lm, newdata = test)
resc <- test$Response-y_hat.lm
lm_RMSE_test <- RMSE(test$Response,y_hat.lm)
varImp(lm)
qqPlot(resc,main = "LM residuals")

#RandomForest:

y_true<-test$Response
rf<- randomForest(Response~GDP_anper+Total_Pop+urban_ratio+Pol_Stab+solar_gen, data=train, n.tree=4000)
rf
y_hat.rf <- predict(rf, newdata = train)
rf_rmse_tr <- RMSE(train$Response,y_hat.rf)
y_hat.rf <- predict(rf, newdata = test)
y_hat.rf_c <- as.numeric(y_hat.rf)
#rf_c_error <- sum(abs(y_true-y_hat.rf_c))/length(y_hat.rf_c)
rf_rmse <- RMSE(y_true,y_hat.rf_c)
res.rf<- y_true-y_hat.rf
qqPlot(res.rf,main = "RF residuals")
importance(rf)
varImpPlot(rf)
partialPlot(x=rf,pred.data=ARE_data,x.var=Response)

boost <- gbm(Response~GDP_anper+Total_Pop+urban_ratio+Pol_Stab+solar_gen, data=train, n.tree=1000, shrinkage = .1, interaction.depth = 3)
boost
#y_hat.bo <- predict(boost, newdata = nys_covid, n.trees = 1000)
#boost_rmse <- RMSE(y_hat.bo,y_true)
#res.boo <- y_true-y_hat.bo
#qqPlot(res.boo,main = "Boosting residuals")
y_true<-test$Response
bagg <- bagging(Response~GDP_anper+Total_Pop+urban_ratio+Pol_Stab+solar_gen, data=train, n.tree=3000, nbagg=500, coob= TRUE, control= rpart.control(minsplit = 2,cp=0))
bagg
y_hat.bag <- predict(bagg, newdata = train, n.trees = 3000)
bag_rmse_tr <- RMSE(y_hat.bag,train$Response)
y_hat.bag <- predict(bagg, newdata = test, n.trees = 3000)
bag_rmse_te <- RMSE(y_hat.bag,y_true)
res.bag <- y_true-y_hat.bag
qqPlot(res.bag,main = "Bagging residuals")
varImp(bagg)

#BART:
test<-data.frame(test)
options(java.parameters = "-Xmx5g")
set_bart_machine_num_cores(4)
x <- subset(train, select = -c(Response))
y <- train$Response
x<-data.frame(x)
bart <- bartMachine(x,y,serialize=TRUE,seed=1111)
bart # RMSE = 5.47
investigate_var_importance(bart_machine=bart)
check_bart_error_assumptions(bart, hetero_plot = "yhats")
Xtest <- subset(test, select = -c(Response))
ytest <- test$Response
Xtest<-data.frame(x)
bart_pred_te<-bart_predict_for_test_data(bart, Xtest, ytest, prob_rule_class = NULL)
#bart_rmse_tr<- RMSE(bart_pred,y_true)
Xtrain <- subset(train, select = -c(Response))
ytrain <- train$Response
Xtrain<-data.frame(x)
bart_pred_tr<-bart_predict_for_test_data(bart, Xtrain, ytrain, prob_rule_class = NULL)
res.bart<-ytrain-bart_pred_te$y_hat
qqPlot(res.bart,main = "BART residuals")
#bart_rmse_te<- RMSE(bart_pred,test$Response)

save(lm,rf,bagg,bart, file = "Pink_RData_Models.RData")
