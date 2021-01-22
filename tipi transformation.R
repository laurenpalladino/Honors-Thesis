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

#registered party
View(data$prevote_regpty)
#removing any missing/i don't know/NA answers
data[!is.na(data$prevote_regpty) & data$prevote_regpty <=0, ] %<>%
  mutate(prevote_regpty = NA)
#1=dem, 2=rep, 4=ind, 5=other
hist(data$prevote_regpty)

#building a 7 pt party scale
View(data$pid_self)
data[!is.na(data$pid_self) & data$pid_self <=0, ] %<>%
  mutate(pid_self = NA)
hist(data$pid_self)

View(data$pid_strong)
#1=strong, 2=not strong
data[!is.na(data$pid_strong) & data$pid_strong <=0, ] %<>%
  mutate(pid_strong = NA)

View(data$pid_lean)
#1=close to rep, 2=true ind, 3=close to dem
data[!is.na(data$pid_lean) & data$pid_lean <=0, ] %<>%
  mutate(pid_lean = NA)
hist(data$pid_lean)

#tv sources

#watches 20/20
View(data$medsrc_tvprog_01)
#watches 60 minutes
View(data$medsrc_tvprog_02)
#watches ABC news nightline
View(data$medsrc_tvprog_03)
#watches ABC world news tonight
View(data$medsrc_tvprog_04)
#watches America Live
View(data$medsrc_tvprog_05)
#watches America This Morning
View(data$medsrc_tvprog_06)
#watches America's Newsroom
View(data$medsrc_tvprog_07)
#watches American Idol
View(data$medsrc_tvprog_08)
#watches Anderson Cooper
View(data$medsrc_tvprog_09)
#watches the Big Bang Theory
View(data$medsrc_tvprog_10)
#watches CBS evening news
View(data$medsrc_tvprog_11)
#watches CBS this morning
View(data$medsrc_tvprog_12)
#watches Chris Matthews show
View(data$medsrc_tvprog_13)
#watches Colbert Report
View(data$medsrc_tvprog_14)
#watches daily show with Jon Stewart
View(data$medsrc_tvprog_15)
#watches dancing with the stars
View(data$medsrc_tvprog_16)
#watches dateline NBC
View(data$medsrc_tvprog_17)
#watches doctors
View(data$medsrc_tvprog_18)
#watches Ellen DeGeneres show
View(data$medsrc_tvprog_19)
#watches face the nation
View(data$medsrc_tvprog_20)
#watches the five
View(data$medsrc_tvprog_21)
#watches fox report
View(data$medsrc_tvprog_22)
#watches frontline
View(data$medsrc_tvprog_23)
#watches good morning america
View(data$medsrc_tvprog_24)
#watches hannity
View(data$medsrc_tvprog_25)
#watches Huckabee
View(data$medsrc_tvprog_26)
#watches insider
View(data$medsrc_tvprog_27)
#watches jimmy kimmel live
View(data$medsrc_tvprog_28)
#watches key and peele
View(data$medsrc_tvprog_29)
#watches the late late show with craig ferguson
View(data$medsrc_tvprog_30)
#watches the late show with david letterman
View(data$medsrc_tvprog_31)
#watches meet the press
View(data$medsrc_tvprog_32)
#watches the mentalist
View(data$medsrc_tvprog_33)
#watches NBC nightly news
View(data$medsrc_tvprog_34)
#watches NCIS
View(data$medsrc_tvprog_35)
#watches the O'Reilly Factor
View(data$medsrc_tvprog_36)
#watches on the record with Greta Van Susteren
View(data$medsrc_tvprog_37)
#watches person of interest
View(data$medsrc_tvprog_38)
#watches rock center with Brian Williams
View(data$medsrc_tvprog_39)
#watches Saturday Night Live
View(data$medsrc_tvprog_40)
#watches special report with Bret Baier
View(data$medsrc_tvprog_41)
#watches tavis smiley
View(data$medsrc_tvprog_42)
#watches sunday morning
View(data$medsrc_tvprog_43)
#watches the view
View(data$medsrc_tvprog_44)
#watches this week
View(data$medsrc_tvprog_45)
#watches the today show
View(data$medsrc_tvprog_46)
#watches the voice
View(data$medsrc_tvprog_47)
#watches the talk
View(data$medsrc_tvprog_48)
#watches al punto
View(data$medsrc_tvprog_49)
#watches al rojo vivo
View(data$medsrc_tvprog_50)
#watches aqui y ahora
View(data$medsrc_tvprog_51)
#watches cnn en espanol
View(data$medsrc_tvprog_52)
#watches despierta America
View(data$medsrc_tvprog_53)
#watches ElGordo y La Flaca
View(data$medsrc_tvprog_54)
#watches Enfoque
View(data$medsrc_tvprog_55)
#watches hoy
View(data$medsrc_tvprog_56)
#watches lo mejor de caso cerrado
View(data$medsrc_tvprog_57)
#watches noitceiro enrique gratas
View(data$medsrc_tvprog_58)
#watches noticiero telemundo
View(data$medsrc_tvprog_59)
#watches noticiero univision
View(data$medsrc_tvprog_60)
#watches Pa'lante con cristiana
View(data$medsrc_tvprog_61)
#watches pequenos gigantes
View(data$medsrc_tvprog_62)
#watches primer impacto
View(data$medsrc_tvprog_63)
#watches sabado gigante
View(data$medsrc_tvprog_64)

#radio sources

#listens to all things considered (NPR)
View(data$medsrc_radio_01)
#listens to the dave ramsey show
View(data$medsrc_radio_02)
#listens to the ed schultz show
View(data$medsrc_radio_03)
#listens to fresh air (NPR)
View(data$medsrc_radio_04)
#listens toglenn beck program
View(data$medsrc_radio_05)
#listens to the laura ingraham show
View(data$medsrc_radio_06)
#listens to the mark levin show
View(data$medsrc_radio_07)
#listens to the morning edition (NPR)
View(data$medsrc_radio_08)
#listens to the neal boortz show
View(data$medsrc_radio_09)
#listens to the power (joe madison)
View(data$medsrc_radio_10)
#listens to the rish limbaugh show
View(data$medsrc_radio_11)
#listens to the savage nation (michael savage)
View(data$medsrc_radio_12)
#listens to the sean hannity show
View(data$medsrc_radio_13)
#listens to talk of the nation (NPR)
View(data$medsrc_radio_14)
#listens to the thom hartmann program
View(data$medsrc_radio_15)