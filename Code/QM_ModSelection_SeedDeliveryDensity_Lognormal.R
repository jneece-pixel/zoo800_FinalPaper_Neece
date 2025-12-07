## Question: Which terrain and/or forest characteristics best explain non-zero
## lodgepole pine seed density at increasing distances from the live forest edge?

## Goal: compare candidate models using AICc to find the best model for seed 
## delivery density

library(tidyverse)
library(AICcmodavg)

## Reading in data frame
seed.dist.nsl<- read.csv("Data/seed.dist.nsl_siteinfo.csv", header=TRUE)

## Subsetting data to only include traps that successfully collected seeds. The
## 0s that are being excluded here are accounted for in the binomial frequency 
## model
seed.dist.nozeros.pico<- subset(seed.dist.nsl, seeds.m2.pico>0)

## Plotting response variable to check the distribution
hist(seed.dist.nozeros.pico$seeds.m2.pico)
# strongly right-skewed, so I'll try a log-transformation
hist(log(seed.dist.nozeros.pico$seeds.m2.pico))

## Making all candidate models

# Ruggedness
TRI.dist.density <- lm(log(seeds.m2.pico)~ dist.m + max.TRI.30, 
                       data = seed.dist.nozeros.pico)
# Mean basal area for mature, live trees in the forest edge
basalarea.dist.density<-lm(log(seeds.m2.pico)~dist.m+Mean.stemarea.m2.pico, 
                         data= seed.dist.nozeros.pico) 
# Mean height of mature, live trees in the forest edge
height.dist.density<-lm(log(seeds.m2.pico)~dist.m+Mean.Height.m.pico, 
                      data= seed.dist.nozeros.pico)
# Mean abundance of non-serotinous (dispersing) cones in forest edge
cones.dist.density<-lm(log(seeds.m2.pico)~dist.m+mean.nonser.cones.ha.pico, 
                     data= seed.dist.nozeros.pico)
# "null" model with only distance from the forest edge
dist.density<-lm(log(seeds.m2.pico)~dist.m, data= seed.dist.nozeros.pico)
# interaction between distance and ruggedness
TRI.dist.density.intrxn<-lm(log(seeds.m2.pico)~dist.m*max.TRI.30, 
                          data= seed.dist.nozeros.pico)
# interaction between distance and basal area
basalarea.dist.density.intrxn<-lm(log(seeds.m2.pico)~dist.m*Mean.stemarea.m2.pico, 
                               data= seed.dist.nozeros.pico)
# interaction between distance and height
height.dist.density.intrxn<-lm(log(seeds.m2.pico)~dist.m*Mean.Height.m.pico, 
                            data= seed.dist.nozeros.pico)
#interaction between distance and cone abundance
cones.dist.density.intrxn<-lm(log(seeds.m2.pico)~dist.m*mean.nonser.cones.ha.pico, 
                           data= seed.dist.nozeros.pico)


## Putting all candidate model into a list
density.distance.mods <- list(TRI.dist.density, basalarea.dist.density, 
                                height.dist.density, cones.dist.density, 
                                TRI.dist.density.intrxn, basalarea.dist.density.intrxn, 
                                height.dist.density.intrxn, cones.dist.density.intrxn, 
                                dist.density)
# Making a vector of all model names
mod.names <- c("TRI",  "basal.area", "height", "cones", "TRI.intrxn", 
                "basal.area.intrxn", "height.intrxn", "cones.intrxn", "dist")

# Making AICc table to compare models. Since only 9 transects collected seeds, 
# I am using the corrected AIC for small sample sizes. 
aictab(cand.set = density.distance.mods, modnames = mod.names)
# model with just the main effects of ruggedness and distance is the best. 
# The model with the interaction betweeen ruggedness and dist is also pretty good.

summary(TRI.dist.density) # all predictors are significant
summary(TRI.dist.density.intrxn) # no significant effect of the interaction term.
