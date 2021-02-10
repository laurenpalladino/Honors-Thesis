library(haven)
library(readr)
library(dplyr)
library(magrittr)
data <- read_dta("Downloads/anes_timeseries_2012_dta/anes_timeseries_2012.dta") 

#building a 7 pt party scale
View(data$pid_self)
data[!is.na(data$pid_self) & data$pid_self <=0, ] %<>%
  mutate(pid_self = NA)

View(data$pid_strong)
data[!is.na(data$pid_strong) & data$pid_strong <=0, ] %<>%
  mutate(pid_strong = NA)

View(data$pid_lean)
data[!is.na(data$pid_lean) & data$pid_lean <=0, ] %<>%
  mutate(pid_lean = NA)

data$pid7 <- NA
data$pid7[data$pid_self==1 & data$pid_strong==1] <- 0 #strong dem
data$pid7[data$pid_self==1 & data$pid_strong==2] <- 1 #not strong dem
data$pid7[data$pid_self==3 & data$pid_lean==3] <- 2 #ind lean dem
data$pid7[data$pid_self==3 & data$pid_lean==3] <- 3 #true ind
data$pid7[data$pid_self==3 & data$pid_lean==1] <- 4 #ind lean rep
data$pid7[data$pid_self==2 & data$pid_strong==2] <- 5 #not strong rep
data$pid7[data$pid_self==2 & data$pid_strong==1] <- 6 #strong rep
View(data$pid7)

#standardizing party id scale
zpid7<- (scale(data$pid7, center = TRUE, scale = TRUE))
hist(zpid7)

#assigning bias scores to media sources (negative=left slant, positive=right slant)
#removing shows that were: 
    #attention checks (eg. NCIS, American Idol), 
    #comedy/satire programs (eg. SNL, Colbert Report),
    #spanish language programs (eg. Al Punto, CNN en espanol)

#watches ABC news nightline
data[!is.na(data$medsrc_tvprog_03) & data$medsrc_tvprog_03 <0, ] %<>%
  mutate(medsrc_tvprog_03 = NA)
data$medsrc_tvprog_03[data$medsrc_tvprog_03>0] <- -5.2

#watches ABC world news tonight
data[!is.na(data$medsrc_tvprog_04) & data$medsrc_tvprog_04 <0, ] %<>%
  mutate(medsrc_tvprog_04 = NA)
data$medsrc_tvprog_04[data$medsrc_tvprog_04>0] <- -5.2

#watches America's Newsroom
data[!is.na(data$medsrc_tvprog_07) & data$medsrc_tvprog_07 <0, ] %<>%
  mutate(medsrc_tvprog_07 = NA)
data$medsrc_tvprog_07[data$medsrc_tvprog_07>0] <- 24.56

#watches Anderson Cooper
data[!is.na(data$medsrc_tvprog_09) & data$medsrc_tvprog_09 <0, ] %<>%
  mutate(medsrc_tvprog_09 = NA)
data$medsrc_tvprog_09[data$medsrc_tvprog_09>0] <- -11.87

#watches CBS evening news
data[!is.na(data$medsrc_tvprog_11) & data$medsrc_tvprog_11 <0, ] %<>%
  mutate(medsrc_tvprog_11 = NA)
data$medsrc_tvprog_11[data$medsrc_tvprog_11>0] <- -3.96

#watches CBS this morning
data[!is.na(data$medsrc_tvprog_12) & data$medsrc_tvprog_12 <0, ] %<>%
  mutate(medsrc_tvprog_12 = NA)
data$medsrc_tvprog_12[data$medsrc_tvprog_12>0] <- -3.96

#watches Chris Matthews show
data[!is.na(data$medsrc_tvprog_13) & data$medsrc_tvprog_13 <0, ] %<>%
  mutate(medsrc_tvprog_13 = NA)
data$medsrc_tvprog_13[data$medsrc_tvprog_13>0] <- -15.02

#watches dateline NBC
data[!is.na(data$medsrc_tvprog_17) & data$medsrc_tvprog_17 <0, ] %<>%
  mutate(medsrc_tvprog_17 = NA)
data$medsrc_tvprog_17[data$medsrc_tvprog_17>0] <- -7.72

#watches face the nation
data[!is.na(data$medsrc_tvprog_20) & data$medsrc_tvprog_20 <0, ] %<>%
  mutate(medsrc_tvprog_20 = NA)
data$medsrc_tvprog_20[data$medsrc_tvprog_20>0] <- -3.96

#watches fox report
data[!is.na(data$medsrc_tvprog_22) & data$medsrc_tvprog_22 <0, ] %<>%
  mutate(medsrc_tvprog_22 = NA)
data$medsrc_tvprog_22[data$medsrc_tvprog_22>0] <- 24.56

#watches frontline
data[!is.na(data$medsrc_tvprog_23) & data$medsrc_tvprog_23 <0, ] %<>%
  mutate(medsrc_tvprog_23 = NA)
data$medsrc_tvprog_23[data$medsrc_tvprog_23>0] <- -3.55

#watches hannity
data[!is.na(data$medsrc_tvprog_25) & data$medsrc_tvprog_25 <0, ] %<>%
  mutate(medsrc_tvprog_25 = NA)
data$medsrc_tvprog_25[data$medsrc_tvprog_25>0] <- 24.56

#watches insider
data[!is.na(data$medsrc_tvprog_27) & data$medsrc_tvprog_27 <0, ] %<>%
  mutate(medsrc_tvprog_27 = NA)
data$medsrc_tvprog_27[data$medsrc_tvprog_27>0] <- -6.00

#watches the late late show with craig ferguson
data[!is.na(data$medsrc_tvprog_30) & data$medsrc_tvprog_30 <0, ] %<>%
  mutate(medsrc_tvprog_30 = NA)
data$medsrc_tvprog_30[data$medsrc_tvprog_30>0] <- -3.96

#watches meet the press
data[!is.na(data$medsrc_tvprog_32) & data$medsrc_tvprog_32 <0, ] %<>%
  mutate(medsrc_tvprog_32 = NA)
data$medsrc_tvprog_32[data$medsrc_tvprog_32>0] <- -7.72

#watches NBC nightly news
data[!is.na(data$medsrc_tvprog_34) & data$medsrc_tvprog_34 <0, ] %<>%
  mutate(medsrc_tvprog_34 = NA)
data$medsrc_tvprog_34[data$medsrc_tvprog_34>0] <- -7.72

#watches the O'Reilly Factor
data[!is.na(data$medsrc_tvprog_36) & data$medsrc_tvprog_36 <0, ] %<>%
  mutate(medsrc_tvprog_36 = NA)
data$medsrc_tvprog_36[data$medsrc_tvprog_36>0] <- 26.25

#watches on the record with Greta Van Susteren
data[!is.na(data$medsrc_tvprog_37) & data$medsrc_tvprog_37 <0, ] %<>%
  mutate(medsrc_tvprog_37 = NA)
data$medsrc_tvprog_37[data$medsrc_tvprog_37>0] <- -15.12

#watches rock center with Brian Williams
data[!is.na(data$medsrc_tvprog_39) & data$medsrc_tvprog_39 <0, ] %<>%
  mutate(medsrc_tvprog_39 = NA)
data$medsrc_tvprog_39[data$medsrc_tvprog_39>0] <- -15.12

#watches special report with Bret Baier
data[!is.na(data$medsrc_tvprog_41) & data$medsrc_tvprog_41 <0, ] %<>%
  mutate(medsrc_tvprog_41 = NA)
data$medsrc_tvprog_41[data$medsrc_tvprog_41>0] <- 24.56

#watches tavis smiley
data[!is.na(data$medsrc_tvprog_42) & data$medsrc_tvprog_42 <0, ] %<>%
  mutate(medsrc_tvprog_42 = NA)
data$medsrc_tvprog_42[data$medsrc_tvprog_42>0] <- 3.55

#watches sunday morning
data[!is.na(data$medsrc_tvprog_43) & data$medsrc_tvprog_43 <0, ] %<>%
  mutate(medsrc_tvprog_43 = NA)
data$medsrc_tvprog_43[data$medsrc_tvprog_43>0] <- -3.96

#watches the view
data[!is.na(data$medsrc_tvprog_44) & data$medsrc_tvprog_44 <0, ] %<>%
  mutate(medsrc_tvprog_44 = NA)
data$medsrc_tvprog_44[data$medsrc_tvprog_44>0] <- -5.20

#watches this week
data[!is.na(data$medsrc_tvprog_45) & data$medsrc_tvprog_45 <0, ] %<>%
  mutate(medsrc_tvprog_45 = NA)
data$medsrc_tvprog_45[data$medsrc_tvprog_45>0] <- -5.20

#watches the today show
data[!is.na(data$medsrc_tvprog_46) & data$medsrc_tvprog_46 <0, ] %<>%
  mutate(medsrc_tvprog_46 = NA)
data$medsrc_tvprog_46[data$medsrc_tvprog_46>0] <- -7.72

#radio sources

#listens to all things considered (NPR)
data[!is.na(data$medsrc_radio_01) & data$medsrc_radio_01 <0, ] %<>%
  mutate(medsrc_radio_01 = NA)
data$medsrc_radio_01[data$medsrc_radio_01>0] <- -14.16

#listens to the ed schultz show
data[!is.na(data$medsrc_radio_03) & data$medsrc_radio_03 <0, ] %<>%
  mutate(medsrc_radio_03 = NA)
data$medsrc_radio_03[data$medsrc_radio_03>0] <- -15.12

#listens to fresh air (NPR)
data[!is.na(data$medsrc_radio_04) & data$medsrc_radio_04 <0, ] %<>%
  mutate(medsrc_radio_04 = NA)
data$medsrc_radio_04[data$medsrc_radio_04>0] <- -14.16

#listens toglenn beck program
data[!is.na(data$medsrc_radio_05) & data$medsrc_radio_05 <0, ] %<>%
  mutate(medsrc_radio_05 = NA)
data$medsrc_radio_05[data$medsrc_radio_05>0] <- 17.05

#listens to the laura ingraham show
data[!is.na(data$medsrc_radio_06) & data$medsrc_radio_06 <0, ] %<>%
  mutate(medsrc_radio_06 = NA)
data$medsrc_radio_06[data$medsrc_radio_06>0] <- 24.56

#listens to the mark levin show
data[!is.na(data$medsrc_radio_07) & data$medsrc_radio_07 <0, ] %<>%
  mutate(medsrc_radio_07 = NA)
data$medsrc_radio_07[data$medsrc_radio_07>0] <- 24.56

#listens to the morning edition (NPR)
data[!is.na(data$medsrc_radio_08) & data$medsrc_radio_08 <0, ] %<>%
  mutate(medsrc_radio_08 = NA)
data$medsrc_radio_08[data$medsrc_radio_08>0] <- -14.16

#listens to the rish limbaugh show
data[!is.na(data$medsrc_radio_11) & data$medsrc_radio_11 <0, ] %<>%
  mutate(medsrc_radio_11 = NA)
data$medsrc_radio_11[data$medsrc_radio_11>0] <- 27.84

#listens to the sean hannity show
data[!is.na(data$medsrc_radio_13) & data$medsrc_radio_13 <0, ] %<>%
  mutate(medsrc_radio_13 = NA)
data$medsrc_radio_13[data$medsrc_radio_13>0] <- 24.56

#listens to talk of the nation (NPR)
data[!is.na(data$medsrc_radio_14) & data$medsrc_radio_14 <0, ] %<>%
  mutate(medsrc_radio_14 = NA)
data$medsrc_radio_14[data$medsrc_radio_14>0] <- -14.16

#creating a bias score
bias = data$medsrc_tvprog_03 + data$medsrc_tvprog_04 + data$medsrc_tvprog_07 +
  data$medsrc_tvprog_09 + data$medsrc_tvprog_11 + data$medsrc_tvprog_12 + 
  data$medsrc_tvprog_13 + data$medsrc_tvprog_17 + data$medsrc_tvprog_20 + 
  data$medsrc_tvprog_22 + data$medsrc_tvprog_23 + data$medsrc_tvprog_25 +
  data$medsrc_tvprog_27 + data$medsrc_tvprog_30 + data$medsrc_tvprog_32 +
  data$medsrc_tvprog_34 + data$medsrc_tvprog_36 + data$medsrc_tvprog_37 +
  data$medsrc_tvprog_39 + data$medsrc_tvprog_41 + data$medsrc_tvprog_42 +
  data$medsrc_tvprog_43 + data$medsrc_tvprog_44 + data$medsrc_tvprog_45 + 
  data$medsrc_tvprog_46 + data$medsrc_radio_01 + data$medsrc_radio_03 +
  data$medsrc_radio_04 + data$medsrc_radio_05 + data$medsrc_radio_06 +
  data$medsrc_radio_07 + data$medsrc_radio_08 + data$medsrc_radio_11 +
  data$medsrc_radio_13 + data$medsrc_radio_14

View(bias) #tricky because a score of 0 means somebody watches no news (maybe should remove?)
avgbias <-bias/35
View(avgbias)
zbias<- (scale(avgbias, center = TRUE, scale = TRUE)) 
rawslant = ztvbias-zpid7 
slant = abs(rawslant) #closer to 0 = no slant, the higher the #, the more slanted media they consume
View(slant)
