library(tidyverse)
library(rsample)
library(caret)
library(modelr)
library(parallel)
library(foreach)
library(ggplot2) 
library(tidyverse)
library(dplyr)
library(class)
library(RANN)
library(caret)
library(modelr)
library(parallel)
library(foreach)

#Q1
ABIA <- read.csv("~/Documents/data/ABIA.csv")

##top5 airport connected with AUS
to_aus = ABIA%>%
  filter(Dest =="AUS")%>%
  group_by(Origin)%>%
  summarize(n_of_flight=n())%>%
  arrange(desc(n_of_flight))%>%as.data.frame()

names(to_aus)=c("airport","n_of_flight")


from_aus = ABIA%>%
  filter(Origin =="AUS")%>%
  group_by(Dest)%>%
  summarize(n_of_flight=n())%>%
  arrange(desc(n_of_flight))%>%as.data.frame()

names(from_aus)=c("airport","n_of_flight")

top_airport=merge(to_aus,from_aus, by="airport")%>%
  mutate(total_n_of_flight = n_of_flight.x + n_of_flight.y)%>%
  arrange(desc(total_n_of_flight))%>%
  top_n(5)%>%as.data.frame()

ggplot(top_airport) + geom_col(aes(x=airport, y=total_n_of_flight)) +
  labs(x="Airport",
       y="total number of flight",
       title="Top 5 airports for the number of flight in AUS") +
  scale_y_continuous(breaks = seq(5000, 12000, by = 1000))

##cancellation of top 5 airport
cancelled_to_aus = ABIA%>%
  filter(Dest == "AUS")%>%
  filter(Origin == "DAL" | Origin == "DFW" | Origin == "IAH" | Origin == "PHX" | Origin == "DEN")%>%
  group_by(Month,Origin)%>%
  summarize(n_can = sum(Cancelled),
            n_fl = n(),
            r_can = (n_can*100)/n_fl)%>%as.data.frame()


ggplot(cancelled_to_aus, aes(x=Month, y=r_can, shape=Origin, color=Origin))+
         geom_line()+
         labs(x="Month",
              y="cancellation rate(%)",
              title="Montly cancellation rate of top 5 flight to AUS")+
  scale_x_continuous(breaks = seq(1, 12, by = 1))
  


cancelled_from_aus = ABIA%>%
  filter(Origin == "AUS")%>%
  filter(Dest == "DAL" | Dest == "DFW" | Dest == "IAH" | Dest == "PHX" | Dest == "DEN")%>%
  group_by(Month,Dest)%>%
  summarize(n_can = sum(Cancelled),
            n_fl = n(),
            r_can = (n_can*100)/n_fl)%>%as.data.frame()


ggplot(cancelled_from_aus, aes(x=Month, y=r_can, shape=Dest, color=Dest))+
  geom_line()+
  labs(x="Month",
       y="cancellation rate(%)",
       title="Montly cancellation rate of top 5 flight from AUS ")+
  scale_x_continuous(breaks = seq(1, 12, by = 1))



#Q2
olympics_top20 <- read.csv("~/Documents/data/olympics_top20.csv")

##A)
olymp_f_ath=olympics_top20%>%
  filter(sex=="F",sport=="Athletics")%>%
  na.omit(heights)
f_height_0.95 = quantile(olymp_f$height, 0.95)


##B)
olymp_f=olympics_top20%>%
  filter(sex=="F")

sd_f_by_event=olymp_f %>%
  group_by(event)%>%
  summarise(sig_height=sd(height))%>%
  slice(which.max(sig_height))

##c)
swimmer=olympics_top20%>%
  filter(sport=="Swimming")%>%as.data.frame()


swimmer_sum = swimmer %>%
  group_by(sex,year) %>%
  summarise(Meanage=mean(age))

ggplot(swimmer_sum,aes(x=year, y=Meanage,color=sex))+
  geom_line() + geom_point() +
  labs(title="Average age of Male and Female Swimmers",
       x="Year",
       y="Average age")


#Q3
sclass <- read.csv("~/Documents/data/sclass.csv")

## trim 350
sclass_350 = sclass %>%
  filter(trim == 350)

##split the data
sclass_350_split = initial_split(sclass_350, prop=0.8)
sclass_350_train = training(sclass_350_split)
sclass_350_test = testing(sclass_350_split)

##run knn for each k and calculate rmse for test data
rmse_350_test = foreach(i=2:330, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=sclass_350_train, k=i)
  modelr::rmse(knn_model, sclass_350_test)}

##plot
k_grid=2:330
rmse_350_test_df = data.frame(rmse_350_test)


plot_out=ggplot(rmse_350_test_df, aes(x = k_grid, y = rmse_350_test)) +
  geom_line(color="blue") +
  labs(x="K",
       y="RMSE_test",
       title="Out-of-sample RMSE (350)")
plot_out

##find optimal k
k_grid[which.min(rmse_350_test)]


##knn with optimal k 
knn_opt = knnreg(price ~ mileage, data=sclass_350_train, k=k_grid[which.min(rmse_350_test)])
modelr::rmse(knn_opt, sclass_350_test)

#plot the fit
#attach the predictions to the test data frame
sclass_350_test = sclass_350_test %>%
  mutate(price_pred = predict(knn_opt, sclass_350_test))

p_test = ggplot(data = sclass_350_test) + 
  geom_point(mapping = aes(x = mileage, y = price), alpha=0.5) + 
  ylim(500, 120000)
p_test

#now add the predictions
p_test + geom_line(aes(x = mileage, y = price_pred), color='red', size=1)+
  labs(title="Prediction of price (350)")



## trim 65AMG
sclass_65amg = sclass %>%
  filter(trim == "65 AMG")

#split the data
sclass_65amg_split = initial_split(sclass_65amg, prop=0.8)
sclass_65amg_train = training(sclass_65amg_split)
sclass_65amg_test = testing(sclass_65amg_split)

#run knn for each k and calculate rmse for test data
rmse_65amg_test = foreach(i=2:230, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=sclass_65amg_train, k=i)
  modelr::rmse(knn_model, sclass_65amg_test)}

#plot
k_grid=2:230
rmse_65_test_df = data.frame(rmse_65amg_test)


plot_out=ggplot(rmse_65_test_df, aes(x = k_grid, y = rmse_65amg_test)) +
  geom_line(color="blue") +
  labs(x="K",
       y="RMSE_test",
       title="Out-of-sample RMSE (65AMG)")
plot_out

#find optimal k
k_grid[which.min(rmse_65amg_test)]


#knn with optimal k 
knn_opt = knnreg(price ~ mileage, data=sclass_65amg_train, k=k_grid[which.min(rmse_65amg_test)])
modelr::rmse(knn_opt, sclass_65amg_test)

#plot the fit
#attach the predictions to the test data frame
sclass_65amg_test = sclass_65amg_test %>%
  mutate(price_pred = predict(knn_opt, sclass_65amg_test))

p_test = ggplot(data = sclass_65amg_test) + 
  geom_point(mapping = aes(x = mileage, y = price), alpha=0.5) + 
  ylim(500, 200000)
p_test

#now add the predictions
p_test + geom_line(aes(x = mileage, y = price_pred), color='green', size=1)+
  labs(title="Prediction of price (65AMG)")

