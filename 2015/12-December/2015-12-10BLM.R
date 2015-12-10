# Fatal Encounters

rm(list=ls())

library(data.table)
library(plyr)
library(stringr)
library(ggplot2)
library(scales)
library(dplyr)
library(tm)
library(foreign)

C <- function(x) x %>% strsplit(split=",|,\n") %>% unlist %>% gsub("^\\s+|\\s+$","",.)

fataldata <- fread('Z:/Data/FatalEncounters/FatalEncounters.csv', header = T, sep = ',')

fataldata$V22 <- NULL

setnames(fataldata, names(fataldata),
         C('SubmitTime,Name,Age,Gender,Race,URL,Date,Address,City,state,
         Zip,County,Agency,Cause,Circumstance,OfficialDisposition,OfficialURL,
         MentalIllness,SubmitUID,Email,DateDesc,Award,UID')
         )

fataldata$age <- fataldata$Age %>% gsub("`|'","", .) %>% as.numeric()

fataldata$age[grep('months',fataldata$Age)] <- 1
fataldata$age[grep('20s',fataldata$Age)] <- 25
fataldata$age[grep('30s',fataldata$Age)] <- 35
fataldata$age[grep('40s|40-50',fataldata$Age)] <- 45
fataldata$age[grep('50s',fataldata$Age)] <- 55
fataldata$age[grep('60s',fataldata$Age)] <- 65

fataldata$age[grep('45 or 49',fataldata$Age)] <- 47
fataldata$age[grep('25-30',fataldata$Age)] <- 27
fataldata$age[grep('24-25',fataldata$Age)] <- 24
fataldata$age[grep("20's-30's",fataldata$Age)] <- 27

fataldata$Age[fataldata$age %>% is.na] # 169 cases of missing or unknown

fataldata$decade <- ceiling((fataldata$age-1)/10)
fataldata$decade[fataldata$decade==0] <- 1

# Define new variable called state for state abbreviations
# Add DC to the List
state.abb2 <- state.abb %>% c('DC') 
state.name2 <- state.name %>% c('District of Columbia')

census$state <- state.abb2[match(census$STATEFIP,state.name2)]

# Create a list of completed states
complete <- C("Alabama,Connecticut,Delaware,District of Columbia,Florida,Louisiana,Maine,
              Massachusetts,Mississippi,Montana,Nevada,New Hampshire,New York,North Carolina,
              North Dakota,Oregon,Rhode Island,South Dakota,Utah,Vermont,Wyoming")

fataldata$complete <- fataldata$state %in% state.abb2[match(complete,state.name2)]

# Date
fataldata$date <- fataldata$Date %>% as.Date("%d-%b-%y")

# Review financial award information
fataldata$award <- fataldata$Naward <- 
  fataldata$Award %>% gsub(",","", .) %>% as.numeric()

fataldata$award[fataldata$Naward<0] <- NA

fataldata$sued <- 1
fataldata$sued[is.na(fataldata$Naward) | fataldata$Naward==-2] <- 0 

fataldata$awarded <- NA
fataldata$awarded[fataldata$Naward==0] <- 0
fataldata$awarded[fataldata$Naward>0 | fataldata$Naward==-1] <- 1

fataldata$race <- "NA"
fataldata$race[grep("(?i)white", fataldata$Race)] <- "white"
fataldata$race[grep("(?i)black", fataldata$Race)] <- "black"
fataldata$race[grep("(?i)latina|latino", fataldata$Race)] <- "latino"
fataldata$race[grep("(?i)native", fataldata$Race)] <- "native-american"
fataldata$race[grep("(?i)asian", fataldata$Race)] <- "asian"
fataldata$race[grep("(?i)unknown", fataldata$Race)] <- "unknown"
fataldata$race[grep("(?i)mixed", fataldata$Race)] <- "mixed"
fataldata$race[grep("(?i)islander", fataldata$Race)] <- "pacific islander"
fataldata$race[grep("(?i)Middle Eastern", fataldata$Race)] <- "middle eastern"

table(fataldata$race)

fataldata$race4 <- fataldata$race
fataldata$race4[
  fataldata$race %in% C('middle eastern,mixed,native-american,pacific islander,asian')] <- 'other'

table(fataldata$race4)

fataldata$white <- 0
fataldata$white[fataldata$race == 'white'] <- 1

fataldata$whiteunknown <- 0
fataldata$whiteunknown[fataldata$race == 'white'|fataldata$race == 'unknown'] <- 1

fataldata$black <- 0
fataldata$black[fataldata$race == 'black'] <- 1

fataldata$DateDesc[grep("(?i)settle|wrongful",fataldata$DateDesc)]

grep("inconsistenc|account",fataldata$DateDesc)

fataldata$nude <- 0
fataldata$nude[grep("nude|Nude|naked|Naked",fataldata$Circumstance)] <- 1

# Number of shots fired
shots <- str_extract(fataldata$DateDesc, "[0-9]{2} shots") 
sum(!is.na(shots))
shots[!is.na(shots)] <- substr(shots[!is.na(shots)],1,2) %>% as.numeric()
shots <- shots %>% as.numeric()

shocks <- str_extract(fataldata$DateDesc, "shocked him [0-9]+|tasered him [0-9]+") 
sum(!is.na(shocks))

shots <- NA
shots[!is.na(shots)] <- substr(shots[!is.na(shots)],1,2) %>% as.numeric()
shots <- shots %>% as.numeric()



mean(shots[fataldata$black==1], na.rm=TRUE)
mean(shots[fataldata$white==1], na.rm=TRUE)

length(grep("shocked him [0-9]+{2}|tasered.+[0-9]+{2}.+seconds",fataldata$DateDesc))
length(grep("shocked him [0-9]+{2}|tasered.+[0-9]+{2}.+seconds",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("shocked him [0-9]+{2}|tasered.+[0-9]+{2}.+seconds",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))


length(grep("[0-9]+{2} shots|[0-9]+{2} rounds|[0-9]+{2} bullets",fataldata$DateDesc))
length(grep("[0-9]+{2} shots|[0-9]+{2} rounds|[0-9]+{2} bullets",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("[0-9]+{2} shots|[0-9]+{2} rounds|[0-9]+{2} bullets",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))

grep("nude|Nude|naked|Naked",fataldata$DateDesc) %>% length
  
length(grep("nude|Nude|naked|Naked",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("nude|Nude|naked|Naked",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))

grep("stop sign|traffic",fataldata$DateDesc) %>% length
length(grep("stop sign|traffic",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("stop sign|traffic",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))

grep("falsifying report|false report|cover up|changed.+story|changed.+testimony",fataldata$DateDesc) %>% length
length(grep("falsifying report|false report|cover up|changed.+story|changed.+testimony",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("falsifying report|false report|cover up|changed.+story|changed.+testimony",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))

grep("chase",fataldata$DateDesc) %>% length
length(grep("chase",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("chase",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))

grep("indict|grand jury",fataldata$DateDesc) %>% length
length(grep("indict|grand jury",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("indict|grand jury",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))


grep("(?i)robb|stolen",fataldata$DateDesc) %>% length
length(grep("(?i)robb|stolen",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("(?i)robb|stolen",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))


grep("hostage",fataldata$DateDesc) %>% length
length(grep("hostage",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("hostage",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))

grep("(?i)refus",fataldata$DateDesc) %>% length
length(grep("(?i)refus",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("(?i)refus",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))

grep("(?i)standoff",fataldata$DateDesc) %>% length
length(grep("(?i)standoff",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("(?i)standoff",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))

grep("(?i)spree",fataldata$DateDesc) %>% length
length(grep("(?i)standoff",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("(?i)standoff",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))


grep("toy",fataldata$DateDesc) %>% length

length(grep("toy",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("toy",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))

grep("unarmed|Unarmed|hands above.+head|hands up",fataldata$DateDesc[!fataldata$vehi]) %>% length
length(grep("unarmed|Unarmed|hands above.+head|hands up",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("unarmed|Unarmed|hands above.+head|hands up",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))

grep(" cooperative",fataldata$DateDesc) %>% length
length(grep(" cooperative",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep(" cooperative",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))

grep("wrongful death|Wrongful death",fataldata$DateDesc) %>% length
length(grep("wrongful death|Wrongful death",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("wrongful death|Wrongful death",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))

grep("(?i)Reach.+Gun",fataldata$DateDesc) %>% length
length(grep("(?i)Reach.+Gun",fataldata$DateDesc[fataldata$black==1]))/sum(fataldata$black==1)/ 
  (length(grep("(?i)Reach.+Gun",fataldata$DateDesc[fataldata$white==1]))/sum(fataldata$white==1))


sum(fataldata$black==1)/sum(fataldata$white==1)

grep("unarmed|Unarmed|hands above.+head",fataldata$DateDesc)
grep("chase|Chase",fataldata$DateDesc)
grep("(?i)off-duty|off duty(?-i)",fataldata$DateDesc) %>% fataldata$DateDesc[.]

for (i in C('Justified|Excusable|Cleared|Acquitted,
            Unknown|?Unreported|Unreporetd,
            investigation|Investigation|Pending|Indicted,
            Criminal,
            Suicide|suicide, 
            Accident|accident,
            bill,
            overdose')) 
    fataldata$dispotion[grep(i, fataldata$OfficialDisposition)] <- tolower(i)

fataldata$OfficialDisposition[is.na(fataldata$dispotion)]

fataldata$Cause <- fataldata$Cause %>% tolower

sort(table(fataldata$OfficialDisposition))

for (i in C('gunshot,taser,vehicle,
            medical|overdose|drug,
            trama|trauma|beaten|bludgeoned|stabbed|knifed|battered|fall|fell|restraint|fire|burns,
            asphyx|breathe|pepper|drown|smoke,
            unknown|investigation|undetermined|undisclosed|unreported|unclear'
)) {
  fataldata[[substr(i,1,4)]] <- FALSE
  fataldata[[substr(i,1,4)]][grep(i,fataldata$Cause)] <- TRUE
}

sum(fataldata$vehi)

with(fataldata, sum(black& vehi )/sum(black) / (sum(white& vehi )/sum(white)))

with(fataldata %>% subset(!is.na(age)), sum(age>9 & age<=15))
with(fataldata %>% subset(!is.na(age)), sum(black & age>9 & age<=15 )/sum(black) / 
       (sum(white & age>9 & age<=15 )/sum(white)))
with(fataldata %>% subset(!is.na(age)), sum(black & age>50)/sum(black) / 
       (sum(white & age>50)/sum(white)))

with(fataldata %>% subset(!is.na(age)), sum(age<=9))
with(fataldata %>% subset(!is.na(age)), sum(black & age<=9 )/sum(black) / (sum(white & age<=9 )/sum(white)))


mr <- function(x) x %>% mean %>% round(2)

table(fataldata$Gender)
table(fataldata$state)

# Census data 
fatal_pop_density <- 
  fataldata %>% 
  group_by(state) %>% 
  dplyr::summarise(kills=n(),
                   white_fatal=mean(white),
                   wfn=sum(white), 
                   wun=mean(whiteunknown),
                   black_fatal=mean(black), 
                   bfn=sum(black),
                   pkilledbw=mean(black)/mean(white),
                   whiteunknown_black_fatal=mean(whiteunknown)/mean(black),
                   count_black=sum(black),
                   guns=sum(guns),
                   tase=sum(tase),
                   vehi=sum(vehi),
                   medi=sum(medi),
                   tram=sum(tram),
                   asph=sum(asph),
                   unkn=sum(unkn),
                   award=(sum(awarded, na.rm = TRUE)*mean(award, na.rm = TRUE))/10^6,
                   complete=mean(complete))


summary(lm(award~race+age+dispotion+vehi+tram+tase, data=fataldata))

fataldata %>% subset(age>15 & age<=30 & Gender=="Male") %>% group_by(race4) %>% 
  dplyr::summarise(n = n()) %>% mutate(freq = n / sum(n))

##############################################################################
#  Bring In State Data

# Read State Data
census <- read.spss('Z:/Data/FatalEncounters/usa_00047.sav') %>% as.data.table

census$hisp <- TRUE
census$hisp[census$HISPAN=="Not Hispanic"] <- FALSE

census$totinc <- census$policeeduc <- NA
census$totinc[census$FTOTINC!=9999999] <- census$FTOTINC[census$FTOTINC!=9999999]

census$white <- census$black <- census$police <- census$highschool <- FALSE

census$highschool[grep("12|college", census$EDUC)] <- TRUE

census$educ <- 0
census$educ[grep("12", census$EDUC)] <- 1
census$educ[grep("college", census$EDUC)] <- 2
census$educ[grep("4 years of college", census$EDUC)] <- 3
census$educ[grep("5\\+ years of college", census$EDUC)] <- 4

census$poor <- census$POVERTY<=100

# Define new variable called abbreviation
census$state <- state.abb2[match(census$STATEFIP,state.name2)]

census$white[census$RACE=="White" & !census$hisp] <- TRUE
census$black[census$RACE=="Black/Negro"] <- TRUE


census$agestring <- census$AGE[1:100] %>% sapply(toString) 
census$age <- census$agestring %>% as.numeric
census$age[census$agestring == "Less than 1 year old"] <- 0

with(census %>%  subset(age >15 & age<=30), weighted.mean(white, PERWT))
with(census %>%  subset(age >15 & age<=30), weighted.mean(black, PERWT))
with(census %>%  subset(age >15 & age<=30), weighted.mean(!black&hisp, PERWT))
with(census %>%  subset(age >15 & age<=30), weighted.mean(!black&!hisp&!white, PERWT))


census$wHS[census$white] <- census$highschool[census$white]
census$bHS[census$black] <- census$highschool[census$black]

census$wpoor[census$white] <- census$poor[census$white]
census$bpoor[census$black] <- census$poor[census$black]


census$police[census$OCC %in% c(3850, 3860)] <- TRUE

# How many police/sherifs are in the US?
sum(census$police)*100 

4*20000/(sum(census$police)*100)

census$bpolice <- census$wpolice <- NA
census$bpolice[census$police] <- census$wpolice[census$police] <- FALSE

census$bpolice[census$police & census$black] <- TRUE
census$wpolice[census$police & census$white] <- TRUE
census$policeeduc[census$police] <- census$educ[census$police]
census$policetotinc[census$police] <- census$totinc[census$police]

# Census data 
census_pop_density <- 
  census %>% 
  group_by(state) %>% 
  dplyr::summarise(
   pw=weighted.mean(white, PERWT), 
   nw=weighted.mean(white, PERWT)*sum(PERWT), 
   pb=weighted.mean(black, PERWT), 
   nb=weighted.mean(black, PERWT)*sum(PERWT), 
   pwb=weighted.mean(white, PERWT)/weighted.mean(black, PERWT),
   people=sum(PERWT),
   wHS=weighted.mean(wHS, PERWT, na.rm=TRUE),
   bHS=weighted.mean(bHS, PERWT, na.rm=TRUE),
   wbHS=weighted.mean(wHS, PERWT, na.rm=TRUE)/
   weighted.mean(bHS, PERWT, na.rm=TRUE),
   wpoor=weighted.mean(wpoor, PERWT, na.rm=TRUE),
   bpoor=weighted.mean(bpoor, PERWT, na.rm=TRUE),
   bwpoor=weighted.mean(bpoor, PERWT, na.rm=TRUE)/
          weighted.mean(wpoor, PERWT, na.rm=TRUE),
   policeeduc=mean(policeeduc, na.rm=TRUE),
   policetotinc=mean(policetotinc, na.rm=TRUE),
   polwb=sum(wpolice, na.rm=TRUE)/sum(bpolice, na.rm=TRUE),
   bpolic=mean(bpolice, na.rm=TRUE),
   wpolic=mean(wpolice, na.rm=TRUE),
   policebwprop=(weighted.mean(bpolice, PERWT, na.rm=TRUE)/weighted.mean(black, PERWT))/
     (weighted.mean(wpolice, PERWT, na.rm=TRUE)/weighted.mean(white, PERWT)),
   npolice=sum(police, na.rm = TRUE)*100,
   polpercap=sum(police, na.rm = TRUE)/length(police)
   )

#####################
# Merge Census and fatalencounters data summaries

merged <- merge(census_pop_density, fatal_pop_density, by='state')

# P(killed|black) = P(killed)P(black|killed)/P(black)
# P(killed|white) = P(killed)P(white|killed)/P(white)
# P(killed|black)/P(killed|white) = P(white)/P(black) * P(black|killed)/P(white|killed)

merged$bwhazard <- merged$pwb * merged$pkilledbw
# merged$policebwprop <- merged$pwb/merged$polwb

merged$hazard <- merged$kills / merged$people * 100000

merged$awpercapita <- merged$award / (merged$people) * 10^6
merged$awperpolice <- merged$award / (merged$npolice) *10^6

# Likelihood of being killed by police per 100k people
for (i in C('guns,tase,vehi,medi,asph,tram,unkn')) 
  merged[[paste0('h',i)]] <- merged[[i]]/ merged$people * 1000

merged[,.(polwb, pwb, policebwprop, bpolic, wpolic, pw, pb)]

merged$bwhazard_unknown <- merged$pwb/merged$whiteunknown_black_fatal

merged[,.(state,count_black,bwhazard,wbHS,bwpoor,policetotinc,policebwprop)] %>% 
  arrange(-bwhazard) %>% head(60)

merged[,.(state, count_black, bwhazard, bwhazard_unknown, wbHS, bwpoor, policetotinc)] %>% 
  arrange(-bwhazard_unknown) %>% head(100)

merged[,.(state, count_black, bwhazard, bwhazard_unknown, wbHS, bwpoor)] %>% 
  arrange(-bwhazard) %>% subset(count_black>10) %>% head(100)
merged[,.(state, count_black, bwhazard, bwhazard_unknown, wbHS, bwpoor)] %>% 
  arrange(-bwhazard_unknown) %>% subset(count_black>10) %>% head(100)

setwd('C:/Users/fsmar/Dropbox/Econometrics by Simulation/2015-12-December')

png('2015-12-03-PovertyViolence.png', width=1000, height=610)
merged %>% 
  subset(count_black>10 & state!="DC") %>% 
  ggplot(aes(x=bwpoor, y=bwhazard, label=state)) + 
  geom_smooth(method='lm',formula=y~x) +
  geom_text(size=10) + 
  theme_bw(base_size = 18) +
  labs(x='P(Poor|Black)/P(Poor|White)',
       y='P(Killed|Black)/P(Killed|White)',
       title="Relationship Between Poverty and Likelihood of Being Killed by Race")
dev.off()

summary(lm(bwhazard~bwpoor+wbHS, data=merged %>% subset(count_black>10 & state!="DC")))

png('2015-12-03-PovertyHS.png', width=1000, height=610)
merged %>% 
  subset(count_black>10) %>% 
  ggplot(aes(x=1/wbHS, y=bwpoor, label=state)) + 
  geom_smooth(method='lm',formula=y~x) +
  geom_text(size=10) + 
  theme_bw(base_size = 18) +
  labs(x='P(HS|Black)/P(HS|White)',
       y='P(Poor|Black)/P(Poor|White)',
       title="Relationship Between Poverty and High School Completion by Race")
dev.off()

  
# Mapping the data

# Load state border maps
states <- map_data("state")

# Assign state abbreviations
states$state <- state.abb2[match(states$region, state.name2 %>% tolower)]

#####################

# Merge state and merged data together
choro <- merge(states, merged, sort = FALSE, by = "state")

choro <- choro[order(choro$order), ]

png('2015-12-03-BLM.png', width=1000, height=610)
choro %>%
  ggplot(aes(x=long, y=lat, fill = bwhazard, group = group)) + 
  geom_polygon(colour="black") +
  geom_polygon(data=choro %>% subset(complete==1), colour="orange", lwd=1) +
  scale_fill_gradient(low = "white", high = "violetred4", 
                      name="P(Killed|Black)/\nP(Killed|White)",
                      limit=c(1,10)) +
  theme_bw(base_size = 18) +
  labs(x='Orange bordered states are those for which information is complete',
       y='',title="Relative Likelihood of Black Person Being Killed by Police to that of a White Person") +
  geom_text(data=state.center %>% 
              as.data.frame %>% 
              cbind(state.abb=state.abb) %>% 
              subset(!(state.abb %in% C('AK,HI'))), aes(x=x,y=y,label=state.abb, fill=NULL, group=NULL))
dev.off()

# Number of people killed by police
choro %>%
  ggplot(aes(x=long, y=lat, fill = kills, group = group)) + 
  geom_polygon(colour=gray(.2)) +
  scale_fill_gradient(high=rgb(.3,0,.3), low="white") +
  scale_colour_gradient(low = gray(.5), high = , guide=FALSE) +
  theme_bw(base_size = 18) +
  labs(x='The minimum since 2000',y='',title="Number of People Killed by Police")

# People Killed by Police (per 100,000)
png('2015-12-03-BHaz.png', width=1000, height=610)
choro %>%
  ggplot(aes(x=long, y=lat, fill = bfn/nb*10^5, group = group)) + 
  geom_polygon(colour=gray(.2)) +
  geom_polygon(data=choro %>% subset(complete==1), colour="orange", lwd=1) +
  scale_fill_gradient(high="darkblue", low="white",
                      name="P(Killed|Black)") +
  scale_colour_gradient(low = gray(.5), high = , guide=FALSE) +
  theme_bw(base_size = 18)+
#  guides(fill=FALSE)+
  labs(x='Orange bordered states are those for which information is complete',
       y='',title="Black People Killed by Police (per 100,000)")+
  geom_text(data=state.center %>% 
            as.data.frame %>% 
            cbind(state.abb=state.abb) %>% 
            subset(!(state.abb %in% C('AK,HI'))), aes(x=x,y=y,label=state.abb, fill=NULL, group=NULL))+
  geom_text(data=state.center %>% 
              as.data.frame %>% 
              cbind(sabb=state.abb) %>% 
              subset(sabb %in% c('NV','VT')), 
            aes(x=x,y=y,label=sabb, fill=NULL, group=NULL), colour='white')

dev.off()

png('2015-12-03-WHaz.png', width=1000, height=610)
choro %>%
  ggplot(aes(x=long, y=lat, fill = wfn/nw*10^5, group = group)) + 
  geom_polygon(colour=gray(.2)) +
  geom_polygon(data=choro %>% subset(complete==1), colour="orange", lwd=1) +
  scale_fill_gradient(high="turquoise4", low="white",
                      name="P(Killed|White)") +
  scale_colour_gradient(low = gray(.5), high = , guide=FALSE) +
  theme_bw(base_size = 18)+
  #  guides(fill=FALSE)+
  labs(x='Orange bordered states are those for which information is complete',
       y='',title="White People Killed by Police (per 100,000)")+
  geom_text(data=state.center %>% 
              as.data.frame %>% 
              cbind(state.abb=state.abb) %>% 
              subset(!(state.abb %in% C('AK,HI'))), aes(x=x,y=y,label=state.abb, fill=NULL, group=NULL))+
  geom_text(data=state.center %>% 
              as.data.frame %>% 
              cbind(sabb=state.abb) %>% 
              subset(sabb %in% c('NV')), 
            aes(x=x,y=y,label=sabb, fill=NULL, group=NULL), colour='white')

dev.off()


########################################
lm()
names(merged)



fataldata %>%
  group_by(race) %>%
  dplyr::summarize(N=length(Cause),
                   gunshot=mr(guns),
                   taser=mr(tase),
                   vehicle=mr(vehi),
                   medical=mr(medi),
                   trama=mr(tram),
                   asphyxiation=mr(asph),
                   unknown=mr(unkn)) %>%
  arrange(-N)


fataldata %>%
  group_by(decade) %>%
  dplyr::summarize(N=length(Cause),
                   gunshot=sum(guns),
                   taser=sum(tase),
                   vehicle=sum(vehi),
                   medical=sum(medi),
                   trama=sum(tram),
                   asphyxiation=sum(asph),
                   unknown=sum(unkn)) %>%
  arrange(decade)



fataldata %>%
  subset(award>0 & Race=="African-American/Black") %>%
  ggplot(aes(x=award)) + geom_histogram() +
  scale_x_continuous(labels = comma)+
  theme_bw()+ theme_bw(base_size = 18)

fataldata %>%
  subset(race %in% C('black,unknown,white,latino')) %>%
  group_by(race) %>%
  dplyr::summarize(meanAward = mean(award, na.rm=TRUE),
                   medAward = median(award, na.rm=TRUE),
                   suetcount = sum(!is.na(award)),
                   psued = mean(sued),
                   winSuet = mean(awarded, na.rm=TRUE))

# For those who recieve an award
fataldata %>%
  subset(award>0) %>%
  group_by(race) %>%
  dplyr::summarize(meanAward = mean(award, rm.na=TRUE),
                   medAward = median(award),
                   count = length(award))

fataldata %>%
  group_by(race) %>%
  dplyr::summarize(meanAward = mean(award, rm.na=TRUE),
                   medAward = median(award),
                   count = length(award),
                   psued = mean(sued))

# Text Mining
library(qdap)
mycorpus <- with(df, as.Corpus(txt, ID))

mydtm <- as.dtm(Filter(as.wfm(mycorpus, 
                              col1 = "docs", col2 = "text", 
                              stopwords = tm::stopwords("english")), 3, 10))

key_merge(matrix2df(mydtm, "ID"), df2, "ID")

