###########################################################################################################
## This code will work on the  insurance  company  benchmark  data  set and
## compute the OLS estimates and compare them with those obtained from a few variable-selection algorithms.
## Name: Akhil Koppera
###########################################################################################################

rm(list = ls())

setwd("F:/Statistical Data Mining")


#install.packages("leaps")
library(leaps)

train_data <-read.table('ticdata2000.txt')
test_data <-read.table('ticeval2000.txt')
y_test<-read.table('tictgts2000.txt')

#View(train_data)
#View(test_data)

#train_data1 = sample(1:nrow(train_data), nrow(train_data)*0.80)
#test_data1 = -train_data1


#new_train_data = train_data[train_data1, ]     # New Train Data

#new_test_data = train_data[test_data1, ]       # New Test Data

?lm
?predict
lm_model<- lm(V86~., data =train_data)
lm_pred <- predict(lm_model,test_data)
lm_pred<-ifelse(lm_pred > 0.5,1,0)
which(lm_pred!=y_test)
lm_error<-mean((as.matrix(y_test) - lm_pred)^2)
lm_error
lm_reponse<-which(lm_pred==1)
#Forward and backward selection

?regsubsets

forward_selection<- regsubsets(V86~., data=train_data, method = "forward",nvmax = 85)
for_sum <- summary(forward_selection)

par(mfrow = c(2,2))
x11()
plot(for_sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l",main="forward_selection")


i<-which(for_sum$cp== min(for_sum$cp))

fwd_coef = coef(forward_selection, id = i)
fwd_temp_train <- train_data[names(fwd_coef)[2:(i+1)]]
fwd_train_lm <- lm(train_data$V86~., data = fwd_temp_train)
fwd_test_pred <- round(predict(fwd_train_lm, newdata=test_data),0)
fwd_test_errors = mean((as.matrix(y_test) - fwd_test_pred)^2)

fwd_test_errors

backward_selection<- regsubsets(V86~., data=train_data, method = "backward",nvmax = 85)
back_sum <- summary(backward_selection)

par(mfrow = c(2,2))
x11()
plot(back_sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l",main="backward_selection")


i<-which(back_sum$cp== min(back_sum$cp))

back_coef = coef(backward_selection, id = i)
back_temp_train <- train_data[names(back_coef)[2:(i+1)]]
back_train_lm <- lm(train_data$V86~., data = back_temp_train)
back_test_pred <- round(predict(back_train_lm, newdata=test_data),0)
back_test_errors = mean((as.matrix(y_test) - back_test_pred)^2)

back_test_errors

##########################################################
######## Ridge and lasso models##########################
#########################################################

set.seed(555)
X<- as.matrix(train_data[,-86])
Y <- train_data$V86

x_test<-as.matrix(test_data)



###################################
# Model Selection 
###################################

ridge_cv.out <- cv.glmnet(X,Y, alpha = 0)
plot(ridge_cv.out)
summary(ridge_cv.out)
names(ridge_cv.out)
plot(ridge_cv.out, main="Ridge")
ridge_bestlam <- ridge_cv.out$lambda.min
ridge_bestlam

ridge.mod<-glmnet(X,Y,alpha=0,lambda=ridge_bestlam)

ridge.pred<-round(predict(ridge.mod,s=ridge_bestlam ,newx=x_test),0)

#ridge_test_error<-classError(ridge.pred,test_data$Apps)$errorRate

#ridge_test_error


#ridge.pred <- predict(ridge.mod, s= bestlam, type = "coefficients")
#ridge.pred2 <- predict(ridge.mod, s = bestlam, newx = X[-train,], type = "response")

y_hat <- ridge.pred

ridge_test_error <- mean((y_hat - as.matrix(y_test))^2)  #test_error
ridge_test_error
ridge_response<-which(ridge.pred==1)

######################################
#  The LASSO
######################################

lasso_cv.out <- cv.glmnet(X,Y, alpha = 1)

summary(lasso_cv.out)

plot(lasso_cv.out,main="Lasso")

names(lasso_cv.out)
lasso_bestlam <- lasso_cv.out$lambda.min
lasso_bestlam

lasso.mod<-glmnet(X,Y,alpha=1,lambda=lasso_bestlam)

lasso.pred<-round(predict(lasso.mod,s=lasso_bestlam ,newx=x_test),0)

lasso_test_error <- mean((lasso.pred - as.matrix(y_test))^2)  #test_error
lasso_test_error
response<-which(lasso.pred==1)
actual_response<- which(y_test==1)

                  