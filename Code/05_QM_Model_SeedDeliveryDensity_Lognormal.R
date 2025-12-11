## Question: How does terrain ruggedness affect lodgepole pine seed delivery 
## density at increasing distances from a live forest edge?
## 
## Goal: Make a binomial model to predict lodgepole pine seed delivery frequency
## up to 100-m from a forest edge based on terrain ruggedness. 

library(tidyverse)
library(car)
library(ggfortify)

#reading in data frame
seed.dist.nsl<- read.csv("Data/seed.dist.nsl_siteinfo.csv", header=TRUE)

## Subsetting data to only include traps that successfully collected seeds. The
## 0s that are being excluded here are accounted for in the binomial frequency 
## model
seed.dist.nozeros.pico<- subset(seed.dist.nsl, seeds.m2.pico>0)

## making model
lognorm.seeddensity.pico <- lm(log(seeds.m2.pico) ~ dist.m + max.TRI.30, 
                              data = seed.dist.nozeros.pico)
summary(lognorm.seeddensity.pico)
# ruggedness and distance are significant predictors!

## Checking linear model assumptions
autoplot(lognorm.seeddensity.pico)
# no crazy deviations from normality
# some heteroscedasticity. 
ncvTest(lognorm.seeddensity.pico) 
# not a significant about of heteroscedasticity

## generating model predictions
dist.m<- rep(seq(0,100, 1),2) # want the model to give me predictions up to 100 
                              # m from forest edge
max.TRI.30 <- c(rep(3, 101), rep(10, 101)) # want predictions for a "low" TRI 
                                           # and a "high" TRI
mod.prediction.log <- predict(lognorm.seeddensity.pico, newdata = 
                                data.frame(dist.m, max.TRI.30), type = "response")
mod.prediction <- exp(mod.prediction.log)

## making new data frame for the model predictions
prediction.data <- data.frame(dist.m, max.TRI.30, mod.prediction)

## plotting model predictions for delivery density for high and low TRI
ggplot(prediction.data, aes(x= dist.m, y= mod.prediction))+
  geom_smooth(aes(color= factor(max.TRI.30)))+
  labs(caption= "model for log-transformed lodgepole pine seed delivery", 
       x = "distance from forest edge (m)", 
       y = "expected seed density", 
       color = "Ruggedness")+
  theme_classic()
#ggsave("Output/lognorm.seeddensity.pico.svg")
