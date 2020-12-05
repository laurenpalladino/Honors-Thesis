library(haven)
library(readr)
library(dplyr)
library(magrittr)
data <- read_dta("Downloads/anes_timeseries_2012_dta/anes_timeseries_2012.dta")

View(data$tipi_extra) #extraversion
View(data$tipi_crit) #agreeable, reversed
View(data$tipi_dep) #conscientious
View(data$tipi_anx) #neuroticism
View(data$tipi_open) #openness
View(data$tipi_resv) #extraversion, reversed
View(data$tipi_warm) #agreeable
View(data$tipi_disorg) #conscientious, reversed
View(data$tipi_calm) #neuroticism, reversed
View(data$tipi_conv) #openness, reversed

#removing weird -6 datapoints
data[!is.na(data$tipi_extra) & data$tipi_extra <=0, ] %<>%
  mutate(tipi_extra = NA)
data[!is.na(data$tipi_crit) & data$tipi_crit <=0, ] %<>%
  mutate(tipi_crit = NA)
data[!is.na(data$tipi_dep) & data$tipi_dep <=0, ] %<>%
  mutate(tipi_dep = NA)
data[!is.na(data$tipi_anx) & data$tipi_anx <=0, ] %<>%
  mutate(tipi_anx = NA)
data[!is.na(data$tipi_open) & data$tipi_open <=0, ] %<>%
  mutate(tipi_open = NA)
data[!is.na(data$tipi_resv) & data$tipi_resv <=0, ] %<>%
  mutate(tipi_resv = NA)
data[!is.na(data$tipi_warm) & data$tipi_warm <=0, ] %<>%
  mutate(tipi_warm = NA)
data[!is.na(data$tipi_disorg) & data$tipi_disorg <=0, ] %<>%
  mutate(tipi_disorg = NA)
data[!is.na(data$tipi_calm) & data$tipi_calm <=0, ] %<>%
  mutate(tipi_calm = NA)
data[!is.na(data$tipi_conv) & data$tipi_conv <=0, ] %<>%
  mutate(tipi_conv = NA)

#reverse coding where needed
data$tipi_crit <- recode(data$tipi_crit, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
data$tipi_resv <- recode(data$tipi_resv, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
data$tipi_disorg <- recode(data$tipi_disorg, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
data$tipi_calm <- recode(data$tipi_calm, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
data$tipi_conv <- recode(data$tipi_conv, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')

#combining to create aggregate measures for each
extraversion = data$tipi_extra + data$tipi_resv
View(extraversion)

agreeable = data$tipi_crit + data$tipi_warm
View(agreeable)

conscientiousness = data$tipi_dep + data$tipi_disorg
View(conscientiousness)

neuroticism = data$tipi_anx + data$tipi_calm
View(neuroticism)

openness = data$tipi_open + data$tipi_conv
View(openness)

#create a 0-1 scale for each
extraversion = (extraversion-1)/13
View(extraversion)

agreeable = (agreeable-1)/13
View(agreeable)

conscientiousness = (conscientiousness-1)/13
View(conscientiousness)

neuroticism = (neuroticism-1)/13
View(neuroticism)

openness = (openness-1)/13
View(openness)

#histograms !!
hist(extraversion)
#extraversion is normally distributed

hist(agreeable)
#agreeableness is normally distributed

hist(conscientiousness)
#conscientiousness is left skewed (more likely to label self as more conscientious)
consc = log(conscientiousness)
hist(consc) #I have now read that log transformations only work on right skewed data... 
pdf("conscientiousness.pdf")
pdf("consc.pdf")

#un-reverse coding to correct skew:
data$tipi_dep<-recode(data$tipi_dep, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
data$tipi_disorg <- recode(data$tipi_disorg, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
conscientiousness = data$tipi_dep + data$tipi_disorg
conscientiousness = (conscientiousness-1)/13
hist(conscientiousness) #now it's right skewed
consc2 = log(conscientiousness)
hist(consc2) # a little better?

hist(neuroticism)
#neuroticism is right skewed (more likely to label self as less neurotic)
neuro = log(neuroticism)
hist(neuro) #this is better
pdf("neuro.pdf")

hist(openness)
#openness is normally distributed
pdf("openness.pdf")
dev.off()