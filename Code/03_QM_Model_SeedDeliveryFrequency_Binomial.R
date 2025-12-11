## Question: How does terrain ruggedness affect the frequency of successful 
## lodgepole pine seed delivery at increasing distances from a live forest edge?
## 
## Goal: Make a binomial model to predict lodgepole pine seed delivery frequency
## up to 100-m from a forest edge based on terrain ruggedness. 

library(tidyverse)

#reading in data frame
seed.dist.nsl<- read.csv("Data/seed.dist.nsl_siteinfo.csv", header=TRUE)

## Making a binomial model using presence/absence of lodgepole pine seeds by 
## distance. TRI is the terrain ruggedness index and dist.m is the distance from
## a forest edge.
delivfreq.binomial<- glm(deliv.freq.pico~dist.m*max.TRI.30, data= seed.dist.nsl, 
                         family = "binomial")

## plotting data just to see
ggplot(data= seed.dist.nsl, aes(x= dist.m,y= deliv.freq.pico, color= max.TRI.30))+
  geom_point()

## generating predictions based on model
dist.m<- rep(seq(0,100, 1),2) # want the model to give me predictions up to 100 
                              # m from forest edge
max.TRI.30 <- c(rep(3, 101), rep(10, 101)) # want predictions for a "low" TRI 
                                           # and a "high" TRI
LogOdds<- predict(delivfreq.binomial, newdata= data.frame(dist.m, max.TRI.30))
expected.delivfreq.pico= exp(LogOdds) / (1+exp(LogOdds)) # convert back into a 
                                                         # proportion

## putting model predictions into a new dataframe
preddata.TRI.binomilafrequeny<- data.frame(dist.m, max.TRI.30, expected.delivfreq.pico)

## plotting model predictions for delivery frequency for high and low TRI
ggplot(preddata.TRI.binomilafrequeny, aes(x= dist.m, y= expected.delivfreq.pico))+
  geom_smooth(aes(color= factor(max.TRI.30)))+
  labs(caption= "binomial model of lodgepole pine delivery (presence/absence)", 
       x = "distance from forest edge (m)", 
       y = "expected seed delivery frequency", 
       color = "ruggedness")+
  theme_classic()
#ggsave("Output/BinomialFrequencyModel.svg")
