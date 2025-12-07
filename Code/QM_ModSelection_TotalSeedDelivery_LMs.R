## Question: Which characteristics of the nearest live forest edge best predict
## total seed delivery into burned areas?

## Goal: compare candidate models using AICc to find the best model for total 
## seed delivery into dispersal transects.

library(tidyverse)
library(AICcmodavg)
library(MASS)

## Loading in data frame
seed.summary.all<- read.csv("Data/seed.summary.all_siteinfo.csv", header=TRUE)

## Note: since three transects did not collect any seeds, I am excluding those
## from the analysis. Only transects that successfully trapped seeds are included.

## checking the distribution of the response variable, seed density: seeds.m2
hist(seed.summary.all[c(7,10:17),4])
# Strongly skewed, so I'll use a poisson distribution. Since poisson assumes 
# whole number response variables, I'll use just the number of seeds (Total.seeds)
# as the response, and then include the area surveyed (n.traps) as a predictor.

## Total seed delivery: Seeds from all conifer species

## Making candidate models for mean basal area, mean height, and cone abundance.
basalarea.delivery <- glm(Total.seeds ~ Mean.stemarea.m2ha + n.traps, 
                          data = seed.summary.all[c(7,10:17),], family = "poisson")
height.delivery <- glm(Total.seeds ~ Mean.Height.m + n.traps, data = seed.summary.all[c(7,10:17),], 
                       family = "poisson")
cones.delivery <- glm(Total.seeds ~ mean.nonser.cones.ha + n.traps, 
                      data = seed.summary.all[c(7,10:17),], family = "poisson")

## putting all candidate models into a list
allsp.delivery <- list(basalarea.delivery, height.delivery, cones.delivery)

## making a vector of model names
mod.names <- c("basal area", "height", "cones")

## creating AICc table
aictab(cand.set = allsp.delivery, modnames= mod.names)
# Height is the best model, but there aren't huge differences in AICc 

## Checking model fit for each model
lapply(allsp.delivery, summary)
# none of the candidate predictors are significant. 


## Total seed delivery predictors: Lodgepole pine seeds only

## checking the distribution of the response variable, lodgepole pine seed 
## density: seeds.m2.pico
hist(seed.summary.all[c(7,10:17),7])
# This is also strongly skewed, so I'll use a Poisson distribution again. 

## Making candidate models for mean basal area, mean height, and cone abundance.
basalarea.delivery.pico <- glm(Total.seeds.pico ~ Mean.stemarea.m2ha.pico + n.traps, 
                               data = seed.summary.all[c(7,10:17),], family = "poisson")
height.delivery.pico <- glm(Total.seeds.pico ~ Mean.Height.m.pico + n.traps, 
                            data = seed.summary.all[c(7,10:17),], family = "poisson")
cones.delivery.pico <- glm(Total.seeds.pico ~ mean.nonser.cones.ha.pico + n.traps, 
                           data = seed.summary.all[c(7,10:17),], family = "poisson")

## putting all candidate models into a list
pico.delivery <- list(basalarea.delivery.pico, height.delivery.pico, cones.delivery.pico)

## making a vector of model names
mod.names.pico <- c("pico basal area", "pico height", "pico cones")

## creating AICc table
aictab(cand.set = pico.delivery, modnames= mod.names.pico)
# All the models are very similar, but basal area is *slightly* better. 

## Checking model fit for each model
lapply(pico.delivery, summary)
# none of the candidate predictors are significant.
