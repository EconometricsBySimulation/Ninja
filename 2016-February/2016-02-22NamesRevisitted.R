rm(list=ls())

require(plyr)
require(ggplot2)
require(scales)
require(data.table)
require(dplyr)
require(RColorBrewer)

# Download data from:
# http://www.ssa.gov/oact/babynames/names.zip
setwd("Z:/Data/SS-names/")
files<-list.files()
files<-files[grepl(".txt",files)]

###### Reading files
namedata <- data.table()

for (i in 1:length(files))
  namedata<-rbind(namedata,
                  data.table(read.csv(files[i],header=F,stringsAsFactors = FALSE), 
                             byear=substr(files[i],4,7) %>% as.numeric),
                  fill=TRUE)

C <- function(x) x %>% 
  strsplit(',') %>% `[[`(1) %>% gsub('^\\s+|\\s+$','',.)

to.data.table <- function(x) {
  (class(x) <- class(data.table()))
  x
} 

setnames(namedata, names(namedata), C("name,gender,count,byear"))


nrow(namedata)
# 1.8 million rows

# Binary Variable MALE
namedata[, male:=as.numeric(gender=="M")]

# Proportion
namedata[, prop := count/max(count),    by=.(name,gender)]

# Peak Year
namedata[, peak := max(byear*(prop==1)), by=.(name,gender)]

# Simmarize the data by counting all instances across all years
ordered <- 
  namedata %>%
  group_by(gender,name) %>%
  dplyr::summarize(total=sum(count)) %>%
  to.data.table()

# Rank the data by total count
ordered[order(-total), i := seq(.N), by=gender]

# Show the count of the names Hillary and Hilary
ordered[name=='Hillary',]
ordered[name=='Hilary',]

# merge back in count and ordering values
namedata <- merge(namedata, ordered, by=C('gender, name'))

# Create a short data set composed of only Female names and the 1000 most popular
Short <- namedata[i <= 1000 & gender=='F',]

# Construct a proj variable which is year-peak popularity year
Short[, proj := byear - peak]
Short[order(gender, proj, prop), j := seq(.N), by=.(gender,proj)]

# Calculate the mean j rank for the decade following the peak for all names
Short[proj > 0 & proj <= 9, mj := mean(j) %>% round(), by=.(gender,name)]
Short[, mjr := mj %>% dense_rank(), by=gender]

Short[(gender=='F') & name=='Chelsea' & 
        proj > 0 & proj <= 9, ]

(Short20 <- Short[proj == 9, ][order(mj), ][1:20,])
write.csv(Short20, file='Short20.csv')

# Show the name Hillary in short data
Short[name=='Hillary',]

peaking <- Short[(peak %in% (1990:1994)) & 
        proj==9, .(Peak=peak, Name=name, prop)][order(Peak, Name)]
write.csv(peaking, file='peaking.csv')

Short[byear==peak, .(peak, byear)] %>% 
  ggplot(aes(peak)) +
  geom_histogram()

# Only eight names peaked in 1992
Short[peak==1992 & byear==1992, .(Name=name, Babies=count)][order(-Babies)]

# Find the average proportions of ALL women for each projection level
avgALL <- Short %>% group_by(proj) %>% 
  dplyr::summarise(prop = mean(prop)) %>% 
  arrange(proj) %>% cbind(name='ALL')

# Find the average proportions for the top 100 fastest dropping 
top100 <- Short[mjr<=100,] %>% group_by(proj) %>% 
  dplyr::summarise(prop = mean(prop)) %>% 
  arrange(proj) %>% cbind(name='top100')

# Find the average proportions for the top 20 fastest dropping 
top20 <- Short[mjr<=20,] %>% group_by(proj) %>% 
  dplyr::summarise(prop = mean(prop)) %>% 
  arrange(proj) %>% cbind(name='top20')

# Bind all of these together
averages <- rbind(avgALL, top100, top20,
  Short[name %in% C('Hillary,Chelsea'), .(name, prop, proj)])

setwd('C:/Users/fsmar/Dropbox/Econometrics by Simulation/2016-02-February')

gold <- (1+5^.5)/2

png('2016-02-21Peak.png', width=1200, height=1200/gold)
averages[proj>-10 & proj<20] %>% 
  ggplot(aes(x=proj,y=prop,color=name, group=name)) +
  geom_line(size=4, color='black', alpha=.85) +
  geom_line(size=2) +
  theme_bw(base_size = 22) + theme(legend.position="bottom") +
  scale_colour_manual(values=brewer.pal(6,"Accent") %>% rev,
                      guide = guide_legend(title = "Years Since Peak")) +
  xlab('Years Leading to Peak and Following Peak') +
  ylab('% Popularity Relative to Peak')  
dev.off()

# An unused graph with just the top 50 popular peaking names
Short[gender=='F' & i<=50] %>%
  ggplot(aes(x=byear,y=prop, color=name))+
  geom_line() +
  scale_colour_discrete(guide = FALSE) +
  theme_bw()

# An unused graph showing 10 years before and after peaking for top 500 names
Short[gender=='F' & proj>=-10 & proj<=10 & i<500,] %>%
  ggplot(aes(x=byear,y=prop, color=name))+
  geom_line() +
  scale_colour_discrete(guide = FALSE)

# An unused graph showing densities
Short[proj==1] %>% 
  ggplot(aes(x=prop)) +
  geom_histogram(data=(Short[gender=='F' & proj==1]),fill = "red", alpha = 0.2) +
  geom_histogram(data=(Short[gender=='F' & proj==2]),fill = "blue", alpha = 0.2) +
  geom_histogram(data=(Short[gender=='F' & proj==3]),fill = "green", alpha = 0.2)+
  geom_histogram(data=(Short[gender=='F' & proj==4]),fill = "purple", alpha = 0.2)+
  geom_histogram(data=(Short[gender=='F' & proj==4]),fill = "black", alpha = 0.2)+
  theme_bw()


# An unused graph showing density curves and how
# the name Hillary fit on the graph.
Short[gender=='F' & proj>=1 & proj<=5,] %>% 
  ggplot(aes(x=(prop*100), color=proj %>% as.factor)) +
  geom_freqpoly(size=4, alpha=.1) +
  geom_freqpoly(size=3, alpha=.3) +
  geom_freqpoly(size=2, alpha=.8) +
  theme_bw(base_size = 22) + theme(legend.position="bottom") +
  geom_vline(data=Short[proj>0 & proj<=5 & name=="Hillary",],
             aes(xintercept=(prop*100), color=proj %>% as.factor),
             size=2, linetype=2) +
  scale_colour_manual(values=brewer.pal(6,"YlOrRd") %>% rev) +
  scale_x_continuous(limits=c(0,99.99999)) +
  scale_colour_discrete(guide = guide_legend(title = "Years Since Peak")) +
  xlab('% Popularity Relative to Peak') + ylab('Frequency')
