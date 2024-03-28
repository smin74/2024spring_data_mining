library(tidyverse)
library(ggplot2)
library(modelr)
library(mosaic)
library(foreach)
library(dplyr)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(rsample) 
library(randomForest)
library(lubridate)
library(modelr)
library(gbm)
library(pdp)
library(ggmap)

#Q1 

#Q2 Dengue

##(1)CART : I include all variables in the data set.
#--(ref) tree.examples.R
str(dengue)
dengue=na.omit(dengue)
dengue$city = factor(dengue$city)
dengue$season = factor(dengue$season)

dengue_split = initial_split(dengue, prop=0.8)
dengue_train = training(dengue_split)
dengue_test = testing(dengue_split)

tree_cart_big=rpart(total_cases ~ . , data=dengue_train, control=rpart.control(cp=0.0015, minsplit=30)) 
rpart.plot(tree_cart_big, digits=-5, type=4, extra=1)
a= rmse(tree_cart_big, dengue_test)

# this function prunes the tree as smallest tree whose CV error is within 1 std err of the minimum.
prune_1se = function(my_tree) {
  out = as.data.frame(my_tree$cptable)
  thresh = min(out$xerror + out$xstd)
  cp_opt = max(out$CP[out$xerror <= thresh])
  prune(my_tree, cp=cp_opt)
}

# let's prune our tree at the 1se complexity level. 
# It is the application of the 1SE rule to pick a tree that is simple but whose performance is not discernably different from the best performer
tree_cart_prune1se = prune_1se(tree_cart_big)
rpart.plot(tree_cart_prune1se, digits=-5, type=4, extra=1)
b= rmse(tree_cart_prune1se, dengue_test)
# rmse : (tree_cart_big < tree_cart_prune1se) ; tree_cart_big is the best cart model compared to pruned one.

##(2)random forest 
#--(ref) random_forest_example.R
tree_rforests = randomForest(total_cases ~ . , data=dengue_train, important=TRUE)
c= rmse(tree_rforests, dengue_test)
varImpPlot(tree_rforests)
#random forests are effective, fast and require little or no tuning via CV, default settings do well.

##(3)gradient-boosted tree
#--(ref) capmetro/  cv.folds for CV==========i'm not sure
tree_boosted= gbm(total_cases ~ . , data=dengue_train,interaction.depth=4, n.trees=500, shrinkage=.05, cv.folds = 5) 
gbm.perf(tree_boosted) #error curve
d= rmse(tree_boosted,dengue_test)

##(4) compare models ; random forest model is the best(min RMSE)
rmse_compare = c(a,c,d)%>%as.data.frame()
rownames(rmse_compare) <- c("cart_big","random forest","boosted")
colnames(rmse_compare) <- "RMSE"
rmse_compare

##(5) PD plots for random forest model
partialPlot(tree_rforests, dengue_test, 'specific_humidity')
partialPlot(tree_rforests, dengue_test, 'precipitation_amt')
partialPlot(tree_rforests, dengue_test, 'min_air_temp_k')


#Q3 green certification
## adding some variables 
greenbuildings = greenbuildings%>%
  mutate(rev_per_sq = Rent*leasing_rate)%>%
  mutate(greencertif = ifelse(LEED == 1|Energystar==1,yes=1,no=0))

greenbuild_split = initial_split(greenbuildings, prop = 0.8)
greenbuild_train = training(greenbuild_split)
greenbuild_test = testing(greenbuild_split)

## (1) combine LEED and EnergyStar with tree model
greenbuild_cart = rpart(rev_per_sq ~ . - CS_PropertyID - Rent - leasing_rate - LEED - Energystar, data=greenbuild_train, control = rpart.control(cp = 0.002, minsplit = 30))
rpart.plot(greenbuild_cart, digits=-5, type=4, extra=1)

#Q4 California housing
str(CAhousing)

ggmap(map) +
  geom_point(aes(longitude, latitude), data = , color = "red")

map = get_googlemap("waco texas", zoom = 12)
ggmap(map)

