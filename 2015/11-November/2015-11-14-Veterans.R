rm(list=ls())

library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(data.table)

#####################################################

# Data taken from the IPUMS website (usa.ipums.org)
# required that only those aged 18-65 be included in the data
data <-  fread('Z:/Data/Ipums01/usa_00039.csv', header = T, sep = ',')

setnames(data, names(data), tolower(names(data)))

paste(names(data), collapse=",")
#year,ownershp,ownershpd,age,marst,educ,educd,ftotinc,poverty,vetstat,vetstatd

#####################################################

# Drop data what is missing poverty or veteran status information
data <- subset(data, data$poverty!=0 & data$vetstat!=0 & data$vetstatd != 0)

# Recode variables to make them easier to work with
data$poverty[data$poverty==0] <- NA

data$vet[data$vetstat==0] <- NA
data$vet[data$vetstat==1] <- 0
data$vet[data$vetstat==2] <- 1

data$vetd <- data$vetstatd
data$vetd[data$vetstatd==0 | data$vetstatd==99] <- NA

data$vetdf <- mapvalues(data$vetd, 
                        c(11,12,13,20,21,22,23), 
                        c("No military service",
                          "Active duty",
                          "Nat Guard/Reserves\nActive/Training",
                          "Veteran",
                          "Veteran",
                          "Veteran",
                          "Nat Guard/Reserves\nActive/Training"))

data$high_school <- 0
data$high_school[data$educ>5] <- 1

data$college <- 0
data$college[data$educ>10] <- 1

data$divorced <- 0
data$divorced[data$marst==6] <- NA
data$divorced[data$marst==3 | data$marst==4] <- 1

data$ownershp[data$ownershp==2] <- 0

data$poverty50 <- data$poverty100 <- data$poverty200 <- 0

data$poverty50[(data$poverty <= 50)] <- 1 
data$poverty100[(data$poverty <= 100)] <- 1 
data$poverty200[(data$poverty <= 200)] <- 1 

data$inctot[data$inctot==9999999] <- NA
data$ftotinc[data$ftotinc==9999999] <- NA

data$inctot1k  <- round(data$inctot, -3)/1000
data$inctot10K <- round(data$inctot, -4)/1000

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

# Data2 creates a random subsample of data1 such that within each year
# there is the same number of individuals within the same age.
data <-
  data %>% 
  group_by(age, year) %>% 
  mutate(nage=n()) %>% 
  group_by(age) %>% 
  mutate(minN = min(nage)) %>%
  group_by(age, year) %>% 
  slice(seq_len(minN[1L]))

data <- data %>% group_by(year) %>%
  mutate(quartile= ntile(ftotinc, 4))

data <- data %>% group_by(year) %>%
  mutate(decile= ntile(ftotinc, 10))

###############################
# Veterans Analysis

data %>%
  subset(!is.na(quartile)) %>%
  ggplot(aes(x=year, color=factor(quartile), group=factor(quartile), y=vet)) + 
  stat_summary(fun.y=mean, geom="line", lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=2, color="black") +
  scale_colour_brewer(palette=3) +
  guides(color=guide_legend(title="Income Quartiles"))+
  ylab("% Veterans") +
  xlab("Year") +
  ggtitle("Likelihood of Being A Veteran By Survey Year")+
  scale_y_continuous(labels = percent)+
  theme_bw(base_size = 18)

data %>%
  subset(age >18 & age<65 & !is.na(vetd)) %>%
  ggplot(aes(x=year, color=vetdf, group=vetdf, y=poverty100)) + 
  stat_summary(fun.y=mean, geom="line", lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=3, color="black") +
  scale_colour_brewer(palette=2) +
  guides(color=guide_legend(title="Veteran Status"))+
  ylab("Likelihood of being impoverished") +
  xlab("Year") +
  ggtitle("Likelihood of Being Below Poverty Line")+
  scale_y_continuous(labels = percent)+
  theme_bw()+
  theme_bw(base_size = 18)

data %>% 
  subset(age >18 & age<65 & !is.na(vetdf)) %>%
  ggplot(aes(x=year, color=vetdf, group=vetdf, y=ownershp)) + 
  stat_summary(fun.y=mean, geom="line", lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=2, color="black") +
  scale_colour_brewer(palette=1) +
  guides(color=guide_legend(title="Veteran Status"))+
  ylab("Likelihood of owning your own home") +
  xlab("Year") +
  ggtitle("Home Ownership")+
  scale_y_continuous(labels = percent)+
  theme_bw()+
  theme_bw(base_size = 18)

data %>% 
  subset(age >18 & age<65 & !is.na(vetdf)) %>%
  ggplot(aes(x=year, color=vetdf, group=vetdf, y=high_school)) + 
  stat_summary(fun.y=mean, geom="line", lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=2, color="black") +
  scale_colour_brewer(palette=4) +
  guides(color=guide_legend(title="Veteran Status"))+
  ylab("Likelihood of having completed high-school") +
  xlab("Year") +
  ggtitle("High School Completion")+
  scale_y_continuous(labels = percent)+
  theme_bw()+
  theme_bw(base_size = 18)

data %>% 
  subset(age >18 & age<65 & !is.na(vetdf)) %>%
  ggplot(aes(x=year, color=vetdf, group=vetdf, y=college)) + 
  stat_summary(fun.y=mean, geom="line",  lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=2, color="black") +
  scale_colour_brewer(palette=5) +
  guides(color=guide_legend(title="Veteran Status"))+
  ylab("Likelihood of having completed 4 or more years of college") +
  xlab("Year") +
  ggtitle("College Completion (4+ years)")+
  scale_y_continuous(labels = percent)+
  theme_bw()+
  theme_bw(base_size = 18)

data %>% 
  ggplot(aes(x=year, color=vetdf, group=vetdf, y=divorced)) + 
  stat_summary(fun.y=mean, geom="line",  lwd=3) +
  stat_summary(fun.y=mean, geom="point", lwd=2, color="black") +
  scale_colour_brewer(palette=6) +
  guides(color=guide_legend(title="Veteran Status"))+
  ylab("Likelihood of being divorced or separated from spouse") +
  xlab("Year") +
  ggtitle("Divorced or Separated")+
  scale_y_continuous(labels = percent)+
  theme_bw()+
  theme_bw(base_size = 18)
