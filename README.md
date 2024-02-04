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

# top5 airport connected with AUS
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

