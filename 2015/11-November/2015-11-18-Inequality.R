rm(list=ls())

library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(data.table)

#####################################################

data <-  fread('Z:/Data/Ipums01/usa_00040.csv', header = T, sep = ',')

setnames(data, names(data), tolower(names(data)))

names(data)

#####################################################
data$raceF <- mapvalues(data$race, from = 1:9,
                        to=c("White", 
                             "Black",
                             "American Indian",
                             "Chinese",
                             "Japanese",
                             "Other Asian",
                             "Other",
                             "Two Major",
                             "Three Major"))

data$race4F <- mapvalues(data$race, from = 1:9,
                         to=c("White", 
                              "Black",
                              "American Indian",
                              "Asian",
                              "Asian",
                              "Asian",
                              "Other",
                              "Other",
                              "Other"))

data$poverty[data$poverty==0] <- NA

data$elementary <- 0
data$elementary[data$educ>=1 & data$educ<=2] <- 1


data$some_high_school <- 0
data$some_high_school[data$educ>=3 & data$educ<=5] <- 1

data$high_school <- 0
data$high_school[data$educ==6] <- 1

data$some_college <- 0
data$some_college[data$educ>6 & data$educ<10] <- 1

data$college <- 0
data$college[data$educ>=10] <- 1

data$rentRatio <- data$rentgrs/data$ftotinc
data$rentRatio[data$rentRatio==0] <- NA
data$rentRatio[data$ftotinc==0] <- NA


data$educf <- mapvalues(data$educ, from = 0:11,
                        to=c(rep("Elementary Only",3), 
                             rep("Some High School",3),
                             rep("High School",1),
                             rep("Some College",3),
                             rep("4+ Years of College",2)))


data$divorced <- 0
data$divorced[data$marst==6] <- NA
data$divorced[data$marst==3 | data$marst==4] <- 1

data$ownershp[data$ownershp==2] <- 0

data$morgage <- NA
data$morgage[data$ownershpd==12] <- 0
data$morgage[data$ownershpd==13] <- 1

data$poverty100 <- 0
data$poverty100[(data$poverty <= 100)] <- 1 

data$inctot[data$inctot==9999999] <- NA
data$ftotinc[data$ftotinc==9999999] <- NA
data$incwage[data$incwage==999999] <- NA

data$inctot1k  <- round(data$inctot, -3)/1000
data$inctot10K <- round(data$inctot, -4)/1000

data$empNLF[data$empstat>0] <- 0
data$empNLF[data$empstat==1] <- 1

data$emp[data$empstat==2] <- 0
data$emp[data$empstat==1] <- 1

data$age5t <- data$age5 <- round(data$age/5)*5

data$ageDec <- floor(data$age/10)
data$ageDec <- mapvalues(data$ageDec, from = 0:9,
                         to=c("<10", 
                              "10-20",
                              "20-30",
                              "30-40",
                              "40-50",
                              "50-60",
                              "60+",
                              "60+",
                              "60+",
                              "60+"))


data$gender <- mapvalues(data$sex, from = 1:2,
                         to=c('male','female'))

#######################################################
# Drop any data that does not have total family income
data1 <- subset(data, !is.na(ftotinc))

data2 <-
  data1 %>% 
  group_by(age, year, sex) %>% 
  mutate(nage=n()) %>% 
  group_by(age, sex) %>% 
  mutate(minN = min(nage)) %>%
  group_by(age, year, sex) %>% 
  slice(seq_len(minN[1L]))

data1 <- data1 %>% group_by(year) %>%
  mutate(quartile= ntile(ftotinc, 4))

data1 <- data1 %>% group_by(year) %>%
  mutate(octtile= ntile(ftotinc, 8))

data1 <- data1 %>% group_by(year) %>%
  mutate(decile= ntile(ftotinc, 10))

data1$decilef <- mapvalues(data1$decile, from = 1:10,
                           to=c(paste0((0:9)*10, "-",(1:10)*10, "%")))

data1 <- data1 %>% group_by(year) %>%
  mutate(tile100= ntile(ftotinc, 100) ,
         ftotincR=ftotinc/median(ftotinc))

data1$tile100means <- mapvalues(data1$tile100, from=1:100,
                                c(rep('1-25%',25),
                                  rep('26-50%',25), 
                                  rep('51-75%',25), 
                                  rep('76-90%',15),
                                  rep('91-99%',9),
                                  'Top 1%'))


data2 <- data2 %>% group_by(year) %>%
  mutate(quartile= ntile(ftotinc, 4))

data2 <- data2 %>% group_by(year) %>%
  mutate(octtile= ntile(ftotinc, 8))

data2 <- data2 %>% group_by(year) %>%
  mutate(decile= ntile(ftotinc, 10))

data2$decilef <- mapvalues(data2$decile, from = 1:10,
                           to=c(paste0((0:9)*10, "-",(1:10)*10, "%")))

data2 <- data2 %>% group_by(year) %>%
  mutate(tile100= ntile(ftotinc, 100) ,
         ftotincR=ftotinc/median(ftotinc))

data2$tile100means <- mapvalues(data2$tile100, from=1:100,
                                 c(rep('1-25%',25),
                                   rep('26-50%',25), 
                                   rep('51-75%',25), 
                                   rep('76-90%',15),
                                   rep('91-99%',9),
                                   'Top 1%'))

###############################
# Graphs
###############################
# Income Inequality

setwd('C:/Users/fsmar/Dropbox/Econometrics by Simulation/2015-11-November')

png('inequality1.png', width=900, height=600)
# Income by income Bracket
data2 %>%
  subset() %>% 
  ggplot(aes(x=year, y=ftotinc)) + 
  stat_summary(fun.y=mean, aes(color=tile100means, group=tile100means), 
               geom="line", lwd=3) +
  stat_summary(fun.y=mean, aes(color=tile100means, group=tile100means),
               geom="point", lwd=2, color="black") +
  stat_summary(fun.y=median, geom="line", lwd=1, color="red", linetype=2) +
  scale_colour_brewer(palette=9) +
  scale_y_continuous(labels = comma)+
  theme_bw()+ theme_bw(base_size = 18)+
  guides(color=guide_legend(title="Income Bracket"))+
  ggtitle("Family Average Income by Bracket")+
  ylab("Total Family Income") +
  xlab("Year")
dev.off()

png('inequality2.png', width=900, height=600)
# Income Relative to that of Median Income
data2 %>%
  subset() %>% 
  ggplot(aes(x=year, color=factor(tile100means), group=factor(tile100means), y=ftotincR)) + 
  stat_summary(fun.y=mean, geom="line", lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=2, color="black") +
  scale_colour_brewer(palette=1) +
  scale_y_continuous(labels = comma)+
  theme_bw()+ theme_bw(base_size = 18)+
  guides(color=guide_legend(title="Income Bracket"))+
  ggtitle("Ratio of Family Income to that of Median Family Income")+
  ylab("Total Family Income/Median Family Income") +
  xlab("Year") +
  geom_hline(yintercept=1, lwd=1, color="red", linetype=2)
dev.off()


CPI2013 <- mean(data2$cpi99[data2$year==2013])

png('inequality3.png', width=900, height=600)
# Income by income bracket adjusted for inflation
data2 %>%
  subset() %>% 
  ggplot(aes(x=year, y=ftotinc*cpi99/CPI2013)) + 
  stat_summary(fun.y=mean, aes(color=tile100means, group=tile100means), 
               geom="line", lwd=3) +
  stat_summary(fun.y=mean, aes(color=tile100means, group=tile100means),
               geom="point", lwd=2, color="black") +
  stat_summary(fun.y=median, geom="line", lwd=1, color="red", linetype=2) +
  scale_colour_brewer(palette=9) +
  scale_y_continuous(labels = comma)+
  theme_bw()+ theme_bw(base_size = 18)+
  guides(color=guide_legend(title="Income Bracket"))+
  ggtitle("Family Average Income by Bracket With 2013 Dollars")+
  ylab("Total Family Income") +
  xlab("Year")
dev.off()

png('inequality4.png', width=900, height=600)
# Income ratio to that of Median Income bottom 25%
data2 %>% 
  subset(tile100<26) %>% 
  ggplot(aes(x=year, color=factor(ageDec), group=factor(ageDec), y=ftotinc*cpi99/CPI2013)) + 
  stat_summary(fun.y=mean, geom="line", lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=2, color="black") +
  scale_y_continuous(labels = comma)+
  theme_bw()+ theme_bw(base_size = 18)+
  guides(color=guide_legend(title="Age at\nTime of\nSurvey"))+
  ggtitle("Total Family Income for the Lowest 25% (2013 dollars)")+
  ylab("Total Family Income (2013 Dollars)") +
  xlab("Year")+
  scale_colour_brewer(palette=3)
dev.off()

png('inequality5.png', width=900, height=600)
# More likely to be in poverty
data2 %>% 
  subset(tile100<26) %>% 
  ggplot(aes(x=year, color=factor(ageDec), group=factor(ageDec), y=poverty100)) + 
  stat_summary(fun.y=mean, geom="line", lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=2, color="black") +
  scale_colour_brewer(palette=4) +
  scale_y_continuous(labels = comma)+
  theme_bw()+ theme_bw(base_size = 18)+
  guides(color=guide_legend(title="Age at\nTime of\nSurvey"))+
  ggtitle("Likelihood of being in Poverty for the Lowest 25%")+
  ylab("Likelihood of Being in Poverty") +
  xlab("Year")
dev.off()


png('inequality6.png', width=900, height=600)
# Likelihood of employment ages 18-65
data2 %>%
  subset(age>=18 & age<=65) %>% 
  ggplot(aes(x=year, color=factor(tile100means), group=factor(tile100means), y=empNLF)) + 
  stat_summary(fun.y=mean, geom="line", lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=2, color="black") +
  scale_colour_brewer(palette=5) +
  scale_y_continuous(labels = comma)+
  theme_bw()+ theme_bw(base_size = 18)+
  guides(color=guide_legend(title="Income\nBracket"))+
  ggtitle("Likelihood of Employment for those Between Ages 18-65")+
  ylab("Employed") +
  xlab("Year")
dev.off()

png('inequality7.png', width=900, height=600)
# Number of hours worked per week by income bracket
data2 %>%
  subset(uhrswork>0) %>% 
  ggplot(aes(x=year, color=factor(tile100means), group=factor(tile100means), y=uhrswork)) + 
  stat_summary(fun.y=mean, geom="line", lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=2, color="black") +
  scale_colour_brewer(palette=6) +
  scale_y_continuous(labels = comma)+
  theme_bw()+ theme_bw(base_size = 18)+
  guides(color=guide_legend(title="Income\nBracket"))+
  ggtitle("Number of Hours Worked by Income Bracket")+
  ylab("# of Hours Worked") +
  xlab("Year")
dev.off()


png('inequality8.png', width=900, height=600)
# Likelihood of being below the poverty line for full time employees in lower 50%
data2 %>%
  subset(uhrswork>=40 & tile100<25) %>% 
  ggplot(aes(x=year, color=factor(gender), group=factor(gender), y=poverty100)) + 
  stat_summary(fun.y=mean, geom="line", lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=2, color="black") +
  scale_colour_brewer(palette=7) +
  scale_y_continuous(labels = comma)+
  theme_bw()+ theme_bw(base_size = 18)+
  guides(color=guide_legend(title="Gender"))+
  ggtitle("Likelihood of being At Poverty Line For Full Time Workers (40+ hrs) lower 50%")+
  ylab("Risk of Poverty Line") +
  xlab("Year")
dev.off()


# How has the change in income reflected on cost of rent?
png('inequality9.png', width=900, height=600)
data2 %>%
  subset(rentgrs>0 & tile100<25) %>% 
  ggplot(aes(x=year, color=factor(ageDec), group=factor(ageDec), 
             y=rentgrs*cpi99/CPI2013)) + 
  stat_summary(fun.y=mean, geom="line", lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=2, color="black") +
  scale_colour_brewer(palette=8) +
  theme_bw()+ theme_bw(base_size = 18)+
  guides(color=guide_legend(title="Age"))+
  ggtitle("The Cost of Rent (2013 Dollars)")+
  ylab("Rent (2013 Dollars)") +
  xlab("Year")
dev.off()


# Home ownership rates among the bottom 50%
png('inequality10.png', width=900, height=600)
data2 %>%
  subset(tile100<=50) %>% 
  ggplot(aes(x=year, color=factor(decilef), group=factor(decilef), y=ownershp)) + 
  stat_summary(fun.y=mean, geom="line", lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=2, color="black") +
  scale_colour_brewer(palette=2) +
  scale_y_continuous(labels = comma)+
  theme_bw()+ theme_bw(base_size = 18)+
  guides(color=guide_legend(title="Income\nBracket"))+
  ggtitle("Home Ownership Among the Bottom 50%")+
  ylab("Likelihood of Owning You Home") +
  xlab("Year")
dev.off()


# Likelihood of Divorce of Separation
png('inequality11.png', width=900, height=600)
data2 %>%
  subset() %>% 
  ggplot(aes(x=year, color=factor(tile100means), group=factor(tile100means), y=divorced)) + 
  stat_summary(fun.y=mean, geom="line", lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=2, color="black") +
  scale_y_continuous(labels = comma)+
  theme_bw()+ theme_bw(base_size = 18)+
  scale_colour_brewer(palette=3) +
  guides(color=guide_legend(title="Income\nBracket"))+
  ggtitle("Likelihood of being Divorced or Separated at Time of Survey")+
  ylab("Likelihood of being Divorced or Separated") +
  xlab("Year")
dev.off()


#### 
# Educational Attainment
#

library(reshape2)
data2long <- data2 %>% 
  subset(educ>0) %>%
  melt(, id = c('year', 'tile100'), 
       measure = c("elementary", "some_high_school","high_school", "some_college","college"))

data2long <- data2long %>% 
  subset(tile100<26) %>% 
  group_by(year, variable) %>% 
  summarise(value=mean(value))

data2long <- data2long %>% 
  group_by(year) %>%
  mutate(cum=cumsum(value))

data2long$educf <- mapvalues(data2long$variable, 
  from = c("elementary", "some_high_school","high_school", "some_college","college"),
    to = c('Elementay','Some High School','High School','Some College','4yr College+'))

png('inequality12.png', width=900, height=600)
data2long %>%
  ggplot(aes(x=year, fill=factor(variable), group=factor(variable), y=value)) + 
  geom_area(colour="black", size=.3, alpha=.4)+ 
  scale_colour_brewer(palette=1) +
  scale_y_continuous(labels = comma)+
  theme_bw()+ theme_bw(base_size = 18)+
  guides(fill=FALSE)+
  scale_fill_discrete(labels=
    c("Elementary", "Some H-School", "High School","Some College", "4 Yrs College+"))+
  ggtitle("Highest Level of Educational Attainment for the Lowest 25%")+
  ylab("Proportion of Education Completed") +
  xlab("Year")+
  geom_text(data=subset(data2long, year==2013), 
            aes(x=2011, y=cum-.02, label=educf))
dev.off()
