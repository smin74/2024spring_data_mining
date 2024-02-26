library(tidyverse)
library(ggplot2)
library(modelr)
library(rsample)
library(mosaic)
library(foreach)
library(caret)
library(dplyr)
library(gamlr)

###################### Q1
data(SaratogaHouses)

# 1. linear model

## get rid of insignificant variables : fireplaces, fuel === (lm_a)
## add variables : (lm_a) + landvalue*newconstruction === (lm_b) including 2 and interact !!
## add interaction term : lm_b + lotSize*landValue(lm_c),lotSize*livingArea(lm_d),landValue*livingArea(lm_e)

finding_best_lm = do(100)*{
  saratoga_split = initial_split(SaratogaHouses, prop=0.8)
  saratoga_train = training(saratoga_split)
  saratoga_test = testing(saratoga_split)
  
  lm2 = lm(price ~ . - pctCollege - sewer - waterfront - landValue - newConstruction, data=saratoga_train)
  rmse2 = rmse(lm2, saratoga_test)
  
  lm_a = lm(price ~ . - pctCollege - sewer - waterfront - landValue - newConstruction - fireplaces - fuel, data = saratoga_train)
  rmse_a = rmse(lm_a, saratoga_test)
  
  lm_b = lm(price ~ . - pctCollege - sewer - waterfront - fireplaces - fuel +(landValue*newConstruction), data = saratoga_train)
  rmse_b = rmse(lm_b, saratoga_test)
  
  lm_c = lm(price ~ . - pctCollege - sewer - waterfront - fireplaces - fuel +(landValue*(newConstruction+lotSize)), data = saratoga_train)
  rmse_c = rmse(lm_c, saratoga_test)
  
  lm_d = lm(price ~ . - pctCollege - sewer - waterfront - fireplaces - fuel +(landValue*newConstruction) +(lotSize*livingArea), data = saratoga_train)
  rmse_d = rmse(lm_d, saratoga_test)
  
  lm_e = lm(price ~ . - pctCollege - sewer - waterfront - fireplaces - fuel +(landValue*(newConstruction+livingArea)), data = saratoga_train)
  rmse_e = rmse(lm_e, saratoga_test)
  
  
  c(rmse2,rmse_a,rmse_b,rmse_c,rmse_d,rmse_e)
}

a = data.frame(colMeans(finding_best_lm))
rownames(a) <- c("lm2","lm_a","lm_b","lm_c","lm_d","lm_e")
colnames(a) <- "Avg of RMSE"
a

## As a result, "lm_b" is best as linear model.


# 2.KNN model

#(1) find optimal K
knn_saratoga_split = initial_split(SaratogaHouses, prop = 0.8)
knn_saratoga_train = training(knn_saratoga_split)
knn_saratoga_test = testing(knn_saratoga_split)

## construct the training and test set feature matrices
Xtrain = model.matrix(~ . - pctCollege - sewer - waterfront - fireplaces - fuel - 1, data=knn_saratoga_train)
Xtest = model.matrix(~ . - pctCollege - sewer - waterfront - fireplaces - fuel - 1, data=knn_saratoga_test)

ytrain = knn_saratoga_train$price
ytest = knn_saratoga_test$price

## rescale using training set scales
scale_train = apply(Xtrain, 2, sd)
Xtilde_train = scale(Xtrain, scale=scale_train)%>%as.data.frame()
Xtilde_test = scale(Xtest, scale=scale_train)%>%as.data.frame()

## set the grid of k 
k_grid=2:150

## loop to get rmse for each k value
finding_smallest_k = foreach(i = k_grid, .combine='c') %do% {
  knn_k=knnreg(price ~ age, data=Xtilde_train, k=i)
  rmse_k=rmse(knn_k,Xtilde_test)
  }%>% as.data.frame()

# find k value which has the minimum value of rmse
best_k=which(finding_smallest_k == min(finding_smallest_k))

best_k

#(2) compare LM and KNN

compare_two_model= do(50)*{
  compare_two_split = initial_split(SaratogaHouses, prop = 0.8)
  compare_two_train = training(compare_two_split)
  compare_two_test = testing(compare_two_split)
  
  Xtrain=model.matrix(~ . - pctCollege - sewer - waterfront - fireplaces - fuel - 1, data=compare_two_train)
  Xtest=model.matrix(~ . - pctCollege - sewer - waterfront - fireplaces - fuel - 1, data=compare_two_test)
  
  ytrain=compare_two_train$price
  ytest=compare_two_test$price
  
  scale_train = apply(Xtrain, 2, sd)
  Xtilde_train = scale(Xtrain, scale=scale_train)
  Xtilde_test = scale(Xtest, scale=scale_train)
  
  Xtilde_train = Xtilde_train %>% as.data.frame()
  Xtilde_test = Xtilde_test %>% as.data.frame()
  
  knn_compare=knnreg(price ~ ., data=compare_two_train, k=best_k)
  rmse(knn_compare,compare_two_test)
  
  
  lm_compare = lm(price ~ . - pctCollege - sewer - waterfront - fireplaces - fuel +(landValue*newConstruction), data=compare_two_train)
  rmse(lm_compare,compare_two_test)
  
  c(rmse(knn_compare,compare_two_test),rmse(lm_compare,compare_two_test))
  
}

b <- data.frame(colMeans(compare_two_model))
rownames(b) <- c("KNN","Linear model")
colnames(b) <- "Avg of RMSE"
b

###################### Q2

#(1) plot  
pd_history = german_credit%>%
  group_by(history)%>%
  summarise(default_prob = mean(Default))%>%as.data.frame()

ggplot(pd_history)+
  geom_col(aes(x = history, y = default_prob))+
  labs(x = "Credit history", y = "Default probability",
       title = "Default probability by credit history")

#(2) logistic regr : 
# Even though the effect of having "good" history is unable to be distinguished from the intercept, 
# better credit history has higher default probability in the plot and glm both
# It is inappropriate result in the light of predicting PD
# It is not ramdom sample, but collected from defaulted loans and similar loans with them.
# That's why the proprotion of "good" is so low(8.9%), and it causes biased prediction.
# proposal : random sampling
logit_default = glm(Default ~ duration + amount + installment + age + history + purpose + foreign, 
                    data = german_credit, family = 'binomial')
coef(logit_default)
str(german_credit)
table(german_credit$purpose)

###################### Q3 hotel

hotels_dev = hotels_dev%>%
  filter(reserved_room_type != "L") # it is only 2 cases but it causes error when it is not included in training set 

# (1) model building : it sometimes shows error due to room type(L), then keep trying until it works!!
compare_three_model= do(50)*{
  hotels_dev_split = initial_split(hotels_dev, prop = 0.8)
  hotels_dev_training = training(hotels_dev_split)
  hotels_dev_testing = testing(hotels_dev_split)

## baseline1 : market_segment, adults, customer_type, is_repeated_guest ; no predict child !!
base1 = lm(children ~ market_segment + adults + customer_type + is_repeated_guest, data = hotels_dev_training)

phat_test_base1 = predict(base1, hotels_dev_testing)
yhat_test_base1 = ifelse(phat_test_base1>0.5,1,0)
confusion_out_1 = table(child_Y = hotels_dev_testing$children, child_Yhat = yhat_test_base1)
out_occuracy_1 = sum(diag(confusion_out_1))/sum(confusion_out_1)

## baseline2 : - arrival_date 

base2 = lm(children ~ . - arrival_date, data = hotels_dev_training)
phat_test_base2 = predict(base2, hotels_dev_testing)
yhat_test_base2 = ifelse(phat_test_base2>0.5,1,0)
confusion_out_2 = table(child_Y = hotels_dev_testing$children, child_Yhat = yhat_test_base2)
out_occuracy_2 = sum(diag(confusion_out_2))/sum(confusion_out_2)

## best_model = base 2 + interaction
best_model = lm(children ~ . - arrival_date + adults*(assigned_room_type + total_of_special_requests) + booking_changes*meal, data = hotels_dev_training)
phat_test_best = predict(best_model, hotels_dev_testing)
yhat_test_best = ifelse(phat_test_best>0.5,1,0)
confusion_out_best = table(child_Y = hotels_dev_testing$children, child_Yhat = yhat_test_best)
out_occuracy_best = sum(diag(confusion_out_best))/sum(confusion_out_best)

c(out_occuracy_1,out_occuracy_2,out_occuracy_best)}

out_occuracy_avg = data.frame(colMeans(compare_three_model))
rownames(out_occuracy_avg) = c("base1","base2","best")
colnames(out_occuracy_avg) = "out-of-sample occuracy rate"

out_occuracy_avg

# (2) model validation : step 1

## ROC for best model from the previous part

phat_val_best = predict(best_model,hotels_val)

tpr = foreach(i=1:90, .combine='c') %do% {
  yhat_val_best=ifelse(phat_val_best >= (i/100), 1, 0)
  confusion_val_best=table(y=hotels_val$children, yhat=yhat_val_best)
  FN=confusion_val_best[2,1]
  TP=confusion_val_best[2,2]
  TPR=TP/(FN+TP)
  TPR
}


fpr = foreach(i = 1:90, .combine = 'c') %do% {
  yhat_val_best = ifelse(phat_val_best >= (i/100), 1, 0)
  confusion_val_best = table(y=hotels_val$children, yhat=yhat_val_best)
  TN = confusion_val_best[1,1]
  FP = confusion_val_best[1,2]
  FPR = FP/(TN+FP)
}

roc_data = data.frame(tpr,fpr)
ggplot(roc_data)+
  geom_line(aes(x=fpr, y=tpr)) +
  labs(title="ROC curve for best linear model",
       x="FPR",
       y="TPR")

# (3) model validation : step 2

##1. predict (using best_model above)
phat_val_best = predict(best_model,hotels_val)
yhat_val_best = ifelse(phat_val_best >=0.5, 1, 0)

##2. divide 20 folds and sum up predicted probabilites in each fold
hotels_val = cbind(hotels_val,phat_val_best,yhat_val_best)

hotels_val = hotels_val %>%
  mutate(fold=rep(1:20, length.out = nrow(hotels_val)) %>% sample())


compare_children2 = hotels_val%>%
  group_by(fold)%>%
  summarize(total_count = n(),
            exp_children = sum(phat_val_best)%>%round(1),
            actual_children = sum(children == 1))%>%as.data.frame()

compare_children2



###################### Q4 mushroom
mushrooms_1 = mushrooms%>%
  mutate(pois = ifelse(class == "p", yes = 1, no = 0))%>%
  subset(select = -c(class,veil.type)) #all data have same veil.type(p), and it causes error


# (1) Lasso
mrx = model.matrix(pois ~ .-1, data = mushrooms_1)
mry = mushrooms_1$pois

mrlasso = gamlr(mrx, mry, family="binomial")
plot(mrlasso)



