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
data$pid7[data$pid_self==3 & data$pid_lean==2] <- 3 #true ind
data$pid7[data$pid_self==3 & data$pid_lean==1] <- 4 #ind lean rep
data$pid7[data$pid_self==2 & data$pid_strong==2] <- 5 #not strong rep
data$pid7[data$pid_self==2 & data$pid_strong==1] <- 6 #strong rep
View(data$pid7)
table(data$pid7)

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

View(bias) #tricky because a score of 0 means somebody watches no news
avgbias <-bias/35
View(avgbias)
zbias<- (scale(avgbias, center = TRUE, scale = TRUE)) 
rawslant = zbias-zpid7 
slant = abs(rawslant) #closer to 0 = no slant
View(slant)

#TIPI transformations

tipidata <- subset(data, select=c("tipi_extra", "tipi_crit", 
                                  "tipi_dep", "tipi_anx",
                                  "tipi_open", "tipi_resv", 
                                  "tipi_warm", "tipi_disorg", 
                                  "tipi_calm", "tipi_conv"))

head(tipidata)

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
tipidata$extraversion = data$tipi_extra + data$tipi_resv
tipidata$agreeable = data$tipi_crit + data$tipi_warm
tipidata$conscientiousness = data$tipi_dep + data$tipi_disorg
tipidata$neuroticism = data$tipi_anx + data$tipi_calm
tipidata$openness = data$tipi_open + data$tipi_conv

#create a 0-1 scale for each
extraversion = (tipidata$extraversion-1)/13
agreeable = (tipidata$agreeable-1)/13
conscientiousness = (tipidata$conscientiousness-1)/13
neuroticism = (tipidata$neuroticism-1)/13
openness = (tipidata$openness-1)/13

#un-reverse coding to correct skew:
hist(conscientiousness)
data$tipi_dep<-recode(data$tipi_dep, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
data$tipi_disorg <- recode(data$tipi_disorg, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')
conscientiousness = data$tipi_dep + data$tipi_disorg
conscientiousness = (conscientiousness-1)/13

consc2 = log(conscientiousness)

#neuroticism is right skewed (more likely to label self as less neurotic)
hist(neuroticism)
neuro = log(neuroticism)

#THE FINAL THING

final <- lm(slant~extraversion + agreeable + conscientiousness + neuro + openness)
summary(final)
plot(final) #wow that's messy
abline(lm(slant~extraversion + agreeable + consc2 + neuro + openness))

#figures + visualizations

library(ggpubr)
ggscatter(tipidata, x = "extraversion", y = "slant", 
          add = "reg.line",
          cor.coef = "TRUE", 
          cor.method = "pearson",
          conf.int = "TRUE",
          xlab = "Extraversion",
          ylab = "Media Slant",)

extraplot <- ggplot(tipidata, aes(x= extraversion, y=slant)) + geom_point() + 
  geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95) 

print(extraplot + ggtitle("Extraversion as a Predictor of Media Slant") +
        labs(y="Media Slant", x="Extraversion"))

agreeplot <- ggplot(tipidata, aes(x= agreeable, y=slant)) + geom_point() + 
  geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95)

print(agreeplot + ggtitle("Agreeableness as a Predictor of Media Slant") +
        labs(y="Media Slant", x="Agreeableness"))

conscplot <- ggplot(tipidata, aes(x= consc2, y=slant)) + geom_point() + 
  geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95)

print(conscplot + ggtitle("Conscientiousness as a Predictor of Media Slant") +
       labs(y="Media Slant", x="Conscientiousness"))

neuroplot <- ggplot(tipidata, aes(x= neuroticism, y=slant)) + geom_point() + 
  geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95)

print(neuroplot + ggtitle("Emotional Stability as a Predictor of Media Slant") +
        labs(y="Media Slant", x="Emotional Stability"))

openplot <- ggplot(tipidata, aes(x= openness, y=slant)) + geom_point() + 
  geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95)

print(openplot + ggtitle("Openness as a Predictor of Media Slant") +
        labs(y="Media Slant", x="Openness"))

trait <- c(extraversion, agreeable, consc2, neuroticism, openness)
