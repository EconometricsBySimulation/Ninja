require(plyr)
require(ggplot2)
require(scales)
require(data.table)
require(dplyr)

# Download data from:
# http://www.ssa.gov/oact/babynames/names.zip
setwd("Z:/Data/SS-names/")
files<-list.files()
files<-files[grepl(".txt",files)]

###### Reading files
namedata <- matrix(0,ncol=4,nrow=0)

for (i in 1:length(files))
  namedata<-rbind(namedata,
    cbind(read.csv(files[i],header=F), substr(files[i],4,7)))

colnames(namedata)<-c("name","gender","count", "year")

namedata <- namedata %>% data.table

namedata$yearnum <- namedata$year[levels(namedata$year)]
dim(namedata)
# 1.8 million rows

Mdata<-namedata[gender=="M",]
Fdata<-namedata[gender=="F",]

Fdata[year>1970, prop := count/max(count), by=name]

Fdata$year <- Fdata$year %>% as.numeric()


Fdata %>% 
  subset(year>1968 & name %in% FirstLady) %>% 
  ggplot(aes(x=year, y=count, group=name, color=name))+
  geom_line(size=1)

Fdata[, prop2 := count]

# Richard Nixon 1969-1974 Thelma
# Gerald Ford 1974-1977  Betty
# Jimmy Carter 1977-1981 Rosalynn
# Ronald Regan 1981-1989 Nancy
# George H Bush 1989-1993 Barbara
# Bill Clinton 1993-2001 Hillary
# George W Bush 2001-2009 Laura
# Barack Obama 2009-2017 Michelle

FirstLady <- c("Thelma","Betty", "Rosalynn", "Nancy","Barbara","Hillary", "Laura", "Michelle")

Fdata$prop2[Fdata$name=="Thelma"] <- Fdata$count[Fdata$name=="Thelma"]/
  max(Fdata$count[Fdata$name=="Thelma" & Fdata$year>1968 & Fdata$year<=1975])

Fdata$prop2[Fdata$name=="Betty"] <- Fdata$count[Fdata$name=="Betty"]/
  max(Fdata$count[Fdata$name=="Betty" & Fdata$year>1973 & Fdata$year<=1978])

Fdata$prop2[Fdata$name=="Rosalynn"] <- Fdata$count[Fdata$name=="Rosalynn"]/
  max(Fdata$count[Fdata$name=="Rosalynn" & Fdata$year>1976 & Fdata$year<=1982])

Fdata$prop2[Fdata$name=="Nancy"] <- Fdata$count[Fdata$name=="Nancy"]/
  max(Fdata$count[Fdata$name=="Nancy"   & Fdata$year>1980 & Fdata$year<=1990])

Fdata$prop2[Fdata$name=="Barbara"] <- Fdata$count[Fdata$name=="Barbara"]/
  max(Fdata$count[Fdata$name=="Barbara" & Fdata$year>1988 & Fdata$year<=1994])

Fdata$prop2[Fdata$name=="Hillary"] <- Fdata$count[Fdata$name=="Hillary"]/
  max(Fdata$count[Fdata$name=="Hillary" & Fdata$year>1991 & Fdata$year<=2002])

Fdata$prop2[Fdata$name=="Laura"] <- Fdata$count[Fdata$name=="Laura"]/
  max(Fdata$count[Fdata$name=="Laura"  & Fdata$year>2000 & Fdata$year<=2010])

Fdata$prop2[Fdata$name=="Michelle"] <- Fdata$count[Fdata$name=="Michelle"]/
  max(Fdata$count[Fdata$name=="Michelle" & Fdata$year>2008 & Fdata$year<=2018])

# Richard Nixon 1969-1974 Thelma
# Gerald Ford 1974-1977  Betty
# Jimmy Carter 1977-1981 Rosalynn
# Ronald Regan 1981-1989 Nancy
# George H Bush 1989-1993 Barbara
# Bill Clinton 1993-2001 Hillary
# George W Bush 2001-2009 Laura
# Barack Obama 2009-2017 Michelle

Fdata$office <- 0
Fdata$office[Fdata$name=="Thelma" & Fdata$year>= 1969 & Fdata$year<=1974] <- 1
Fdata$office[Fdata$name=="Betty"  & Fdata$year>= 1974 & Fdata$year<=1977] <- 1
Fdata$office[Fdata$name=="Rosalynn"  & Fdata$year>= 1977 & Fdata$year<=1981] <- 1
Fdata$office[Fdata$name=="Nancy"  & Fdata$year>= 1981 & Fdata$year<=1989] <- 1
Fdata$office[Fdata$name=="Barbara"  & Fdata$year>= 1989 & Fdata$year<=1993] <- 1
Fdata$office[Fdata$name=="Hillary"  & Fdata$year>= 1993 & Fdata$year<=2001] <- 1
Fdata$office[Fdata$name=="Laura"  & Fdata$year>= 2001 & Fdata$year<=2009] <- 1
Fdata$office[Fdata$name=="Michelle"  & Fdata$year>= 2009 & Fdata$year<=2017] <- 1

Fdata %>% 
  subset(year>1968 & name %in% FirstLady) %>% 
  ggplot(aes(x=year+.5, y=prop, group=name, color=name))+
  geom_rect(aes(xmin = 1969   , xmax = 1974 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1974   , xmax = 1977 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1976.5 , xmax = 1981 , ymin = 0 , ymax = 1 ),fill = "#DBF6FF")+
  geom_rect(aes(xmin = 1980.5 , xmax = 1989 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1988.5 , xmax = 1993 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1992.5 , xmax = 2001 , ymin = 0 , ymax = 1 ),fill = "#DBF6FF")+
  geom_rect(aes(xmin = 2000.5 , xmax = 2009 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 2008.5 , xmax = 2014 , ymin = 0 , ymax = 1 ),fill = "#DBF6FF")+
  geom_line(size=2)  +
  theme_bw(base_size = 22) +
  geom_vline(xintercept = 2001, size=1, alpha=.3,linetype = 2) 
  
png('2016-02-HillaryBaby.png', width=1200, height=1200/gold)

Fdata %>% 
  subset(year>1968 & name %in% FirstLady) %>% 
  ggplot(aes(x=year+.5, y=prop2, group=name, color=name))+
  geom_rect(aes(xmin = 1969   , xmax = 1974 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1974   , xmax = 1977 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1976.5 , xmax = 1981 , ymin = 0 , ymax = 1 ),fill = "#DBF6FF")+
  geom_rect(aes(xmin = 1980.5 , xmax = 1989 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1988.5 , xmax = 1993 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1992.5 , xmax = 2001 , ymin = 0 , ymax = 1 ),fill = "#DBF6FF")+
  geom_rect(aes(xmin = 2000.5 , xmax = 2009 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 2008.5 , xmax = 2014 , ymin = 0 , ymax = 1 ),fill = "#DBF6FF")+
  geom_line(size=2)  +
  theme_bw(base_size = 22) +
  geom_vline(xintercept = 2001, size=1, alpha=.3,linetype = 2) 
dev.off()

Fdata$prop3 <- Fdata$prop2
Fdata$prop3[Fdata$prop2>1] <- Fdata$prop2[Fdata$prop2>1] * .03 + 1


mid.term <- 
  data.table(name=c('Nixon', 'Ford', 'Carter', 'Regan', 
                    'Bush', 'Clinton', 'Bush', 'Obama'),
             first=c('Thelma', 'Betty', 'Rosalynn', 'Nancy', 
                    'Barbara', 'Hillary', 'Laura', 'Michelle'),
             year=c(1971, 1975.2, 1978.5, 1984, 1990.5, 1996, 2004.5, 2011))

# Richard Nixon 1969-1974 Thelma
# Gerald Ford 1974-1977  Betty
# Jimmy Carter 1977-1981 Rosalynn
# Ronald Regan 1981-1989 Nancy
# George H Bush 1989-1993 Barbara
# Bill Clinton 1993-2001 Hillary
# George W Bush 2001-2009 Laura
# Barack Obama 2009-2017 Michelle
setwd('C:/Users/fsmar/Dropbox/Econometrics by Simulation/2016-02-February')

gold <- (1+5^.5)/2

png('2016-02-HillaryBaby.png', width=1200, height=1200/gold)
Fdata %>% 
  subset(year>1967 & name %in% FirstLady) %>% 
  ggplot(aes(x=year+.5, y=prop3, group=name, color=name))+
  geom_rect(aes(xmin = 1969   , xmax = 1977 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1974   , xmax = 1977 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1976.5 , xmax = 1981 , ymin = 0 , ymax = 1 ),fill = "#DBF6FF")+
  geom_rect(aes(xmin = 1980.5 , xmax = 1989 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1988.5 , xmax = 1993 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1992.5 , xmax = 2001 , ymin = 0 , ymax = 1 ),fill = "#DBF6FF")+
  geom_rect(aes(xmin = 2000.5 , xmax = 2009 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 2008.5 , xmax = 2014 , ymin = 0 , ymax = 1 ),fill = "#DBF6FF")+
  geom_line(size=1, linetype=1)  +
#  geom_line(data=Fdata %>% 
#              subset(year>1968 & name %in% FirstLady & office==1),
#            size=2) +
  guides(color = FALSE) +
  xlab('') + ylab('') +
  theme_bw(base_size = 22) + theme(legend.position="bottom") +
  geom_text(data=Fdata %>% subset(year==2014 & name %in% FirstLady), 
            aes(x=year+.5, y=prop3, label=name, hjust = 0)) +
  geom_text(data=Fdata %>% subset(year==1968 & name %in% FirstLady), 
            aes(x=year+.5, y=prop3, label=name, hjust = 1)) +
  coord_cartesian(ylim = c(0, 1.4), xlim = c(1965, 2017)) +
  geom_text(data=mid.term, aes(label=name, x=year, y=.03), color='black') +
  geom_text(data=mid.term, aes(label=first, x=year, y=.97), color='black') +
  ggtitle('Popularity of First Lady Baby Names')
dev.off()

png('2016-02-HillaryBabyUnscaled.png', width=1200, height=1200/gold)
Fdata %>% 
  subset(year>1967 & name %in% FirstLady) %>% 
  ggplot(aes(x=year+.5, y=prop2, group=name, color=name))+
  geom_rect(aes(xmin = 1969   , xmax = 1977 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1974   , xmax = 1977 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1976.5 , xmax = 1981 , ymin = 0 , ymax = 1 ),fill = "#DBF6FF")+
  geom_rect(aes(xmin = 1980.5 , xmax = 1989 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1988.5 , xmax = 1993 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 1992.5 , xmax = 2001 , ymin = 0 , ymax = 1 ),fill = "#DBF6FF")+
  geom_rect(aes(xmin = 2000.5 , xmax = 2009 , ymin = 0 , ymax = 1 ),fill = "#FFDBDB")+
  geom_rect(aes(xmin = 2008.5 , xmax = 2014 , ymin = 0 , ymax = 1 ),fill = "#DBF6FF")+
  geom_line(size=1, linetype=1)  +
  #  geom_line(data=Fdata %>% 
  #              subset(year>1968 & name %in% FirstLady & office==1),
  #            size=2) +
  guides(color = FALSE) +
  xlab('') + ylab('') +
  theme_bw(base_size = 22) + theme(legend.position="bottom") +
  geom_text(data=Fdata %>% subset(year==2014 & name %in% FirstLady), 
            aes(x=year+.5, y=prop2, label=name, hjust = 0)) +
  geom_text(data=Fdata %>% subset(year==1968 & name %in% FirstLady), 
            aes(x=year+.5, y=prop2, label=name, hjust = 1)) +
  coord_cartesian(xlim = c(1965, 2017)) +
  geom_text(data=mid.term, aes(label=name, x=year, y=.03), color='black') +
  geom_text(data=mid.term, aes(label=first, x=year, y=.97), color='black') +
  ggtitle('Popularity of First Lady Baby Names')
dev.off()

# Richard Nixon 1969-1974 Thelma
# Gerald Ford 1974-1977  Betty
# Jimmy Carter 1977-1981 Rosalynn
# Ronald Regan 1981-1989 Nancy
# George H Bush 1989-1993 Barbara
# Bill Clinton 1993-2001 Hillary
# George W Bush 2001-2009 Laura
# Barack Obama 2009-2017 Michelle

c(
# Drop in the popularity of the name Betty
Thelma= 1-Fdata[name=="Thelma" & year==1974,count]/
  Fdata[name=="Thelma" & year==1968,count]
,
# Drop in the popularity of the name Betty
Betty= 1-Fdata[name=="Betty" & year==1977,count]/
  Fdata[name=="Betty" & year==1973,count]
,

# Drop in the popularity of the name Rosalynn
Rosalynn= 1-Fdata[name=="Rosalynn" & year==1981,count]/
  Fdata[name=="Rosalynn" & year==1976,count]
,

# Drop in the popularity of the name Nancy
Nancy= 1-Fdata[name=="Nancy" & year==1989,count]/
  Fdata[name=="Nancy" & year==1980,count]
,
# Drop in the popularity of the name Barbara
Barbara=1-Fdata[name=="Barbara" & year==1993,count]/
  Fdata[name=="Barbara" & year==1988,count]
,
# Drop in the popularity of the name Hillary
Hillary=1-Fdata[name=="Hillary" & year==2001,count]/
  Fdata[name=="Hillary" & year==1992,count]
,
# Drop in the popularity of the name Laura
Laura=1-Fdata[name=="Laura" & year==2009,count]/
  Fdata[name=="Laura" & year==2000,count]
,
# Drop in the popularity of the name Michelle
Michelle=1-Fdata[name=="Michelle" & year==2014,count]/
  Fdata[name=="Michelle" & year==2008,count]
) -> first_ladies

Table1 <- data.frame(name=names(first_ladies ), ratio=first_ladies %>% round(2))
