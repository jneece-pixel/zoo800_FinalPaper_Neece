## Question: Which terrain and/or forest characteristics best explain the frequency 
## of successful lodgepole pine seed delivery at increasing distances from the
## live forest edge?

## Goal: compare candidate models using AICc to find the best model for seed 
## delivery frequency

library(tidyverse)
library(AICcmodavg)

## Reading in data frame
seed.dist.nsl<- read.csv("Data/seed.dist.nsl_siteinfo.csv", header=TRUE)

# Making all candidate models. Note: we know that distance from the forest edge
# has a strong influence on seed delivery, so all models contain dist.m as a 
# predictor. 

# Ruggedness
TRI.dist.freq<-glm(deliv.freq.pico~(dist.m)+max.TRI.30, data= seed.dist.nsl, 
                   family = binomial) 
# Positioning relative to live-forest edge
positioning.dist.freq <- glm(deliv.freq.pico ~ dist.m + Positioning,
                             data = seed.dist.nsl, family = "binomial")
# Mean basal area for mature, live trees in the forest edge
basalarea.dist.freq<-glm(deliv.freq.pico~dist.m + Mean.stemarea.m2.pico, 
                         data= seed.dist.nsl, family = binomial) 
# Mean height of mature, live trees in the forest edge
height.dist.freq<-glm(deliv.freq.pico~dist.m + Mean.Height.m.pico, 
                      data= seed.dist.nsl, family = binomial)
# Mean abundance of non-serotinous (dispersing) cones in forest edge
cones.dist.freq<-glm(deliv.freq.pico~dist.m + mean.nonser.cones.ha.pico, 
                     data= seed.dist.nsl, family = binomial)
# "null" model with only distance from the forest edge
dist.freq<-glm(deliv.freq.pico~(dist.m), data= seed.dist.nsl, 
               family = binomial)
# interaction between distance and ruggedness
TRI.dist.freq.intrxn<-glm(deliv.freq.pico~(dist.m)*max.TRI.30, 
                          data= seed.dist.nsl, family = binomial)
# interaction between distance and basal area
basalarea.dist.freqintrxn<-glm(deliv.freq.pico~(dist.m)*Mean.stemarea.m2.pico, 
                               data= seed.dist.nsl, family = binomial)
# interaction between distance and height
height.dist.freqintrxn<-glm(deliv.freq.pico~(dist.m)*Mean.Height.m.pico, 
                            data= seed.dist.nsl, family = binomial)
#interaction between distance and cone abundance
cones.dist.freqintrxn<-glm(deliv.freq.pico~(dist.m)*mean.nonser.cones.ha.pico, 
                           data= seed.dist.nsl, family = binomial)

## Putting all candidate model into a list
frequency.distance.mods <- list(TRI.dist.freq, positioning.dist.freq, basalarea.dist.freq, 
                                height.dist.freq, cones.dist.freq, 
                                TRI.dist.freq.intrxn, basalarea.dist.freqintrxn, 
                                height.dist.freqintrxn, cones.dist.freqintrxn, 
                                dist.freq)
# Making a vector of all model names
mod.names2 <- c("TRI",  "positioning", "basal.area", "height", "cones", "TRI.intrxn", 
                "basal.area.intrxn", "height.intrxn", "cones.intrxn", "dist")
# Making AICc table to compare models. Since only 9 transects collected seeds, 
# I am using the corrected AIC for small sample sizes. 
aictab(cand.set = frequency.distance.mods, modnames = mod.names2)

## model including the interaction between distance and ruggedness is the best!
summary(TRI.dist.freq.intrxn)

