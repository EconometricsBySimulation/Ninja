library(data.table)
library(dplyr)
library(ggplot2)

#Souce: http://fec.gov/disclosurep/PDownload.do
ALL <- read.csv('Z:/Data/FEC/ALL.csv', header = FALSE,
                    stringsAsFactors=FALSE)[,-19]

names(ALL) <- ALL[1,]
ALL %>% head
ALL <- data.table(ALL[-1,])

ALL$candidate <- gsub("^(.*?),.*", "\\1", ALL$cand_nm)
ALL$contb_receipt_amt <- as.numeric(ALL$contb_receipt_amt)

ALL$date <- ALL$contb_receipt_dt %>% as.Date("%d-%b-%y")
ALL$month <- ALL$date %>% format("%m") %>% as.numeric

# Input manually the number of contributions in total reported to the FEC
ALL$ind <- 0
ALL[candidate=='Clinton',ind := 107511798] 
ALL[candidate=='Sanders',ind :=  72796606] 

# Drop all data but Sanders/Clinton
ALL <- ALL[ind>0,]

ALL[, declaredR   := sum(contb_receipt_amt)/ind, by=candidate]

over.time <- ALL %>% 
  group_by(candidate, contbr_st, month) %>% 
  dplyr::summarise(
    declaredR = mean(declaredR) %>% round(3),
    indN      = n(),
    unique    = length(unique(contbr_nm)),
    REindN      = n()/(mean(declaredR)),
    REunique    = length(unique(contbr_nm))/(mean(declaredR))
)

months <- min(over.time$month):max(over.time$month)

setwd('C:/Users/fsmar/Dropbox/Econometrics by Simulation/2016-02-February')

gold <- (1+5^.5)/2

# Figure 1
png('2016-17-NevadaNContributions.png', width=1200, height=1200/gold)
# Contributions by date

  over.time[contbr_st=='NV',] %>% 
    ggplot(aes(x=month, y=indN, color=candidate)) +
    geom_line(size=2) +
    scale_x_continuous(breaks = months,
                       labels = (month.name)[months]) +
    theme_bw(base_size = 22) + 
    labs(x=NULL, y=NULL) +
    guides(color=guide_legend(title="", 
                             direction = "horizontal",keywidth = 4, keyheight = 1,
                             title.position = "top",
                             title.theme = element_text(size=25, angle = 0),
                             title.hjust=.5)) + 
    theme(legend.position="bottom")+
    ggtitle('Nevada: Number of Contributions Over Time')
dev.off()  

# Figure 2
png('2016-17-NevadaNContributors.png', width=1200, height=1200/gold)
  
  over.time[contbr_st=='NV',] %>% 
    ggplot(aes(x=month, y=unique, color=candidate)) +
    geom_line(size=2) +
    scale_x_continuous(breaks = months,
                       labels = (month.name)[months]) +
    theme_bw(base_size = 22) + 
    labs(x=NULL, y=NULL) +
    guides(color=guide_legend(title="", 
                              direction = "horizontal",keywidth = 4, keyheight = 1,
                              title.position = "top",
                              title.theme = element_text(size=25, angle = 0),
                              title.hjust=.5)) + 
    theme(legend.position="bottom")+
    ggtitle('Nevada: Number of Contributors Over Time')
dev.off() 

# Figure 3
png('2016-17-RENevadaNContributions.png', width=1200, height=1200/gold)
# Contributions by date

over.time[contbr_st=='NV',] %>% 
  ggplot(aes(x=month, y=REindN, color=candidate)) +
  geom_line(size=2) +
  scale_x_continuous(breaks = months,
                     labels = (month.name)[months]) +
  theme_bw(base_size = 22) + 
  labs(x=NULL, y=NULL) +
  guides(color=guide_legend(title="", 
                            direction = "horizontal",keywidth = 4, keyheight = 1,
                            title.position = "top",
                            title.theme = element_text(size=25, angle = 0),
                            title.hjust=.5)) + 
  theme(legend.position="bottom")+
  ggtitle('Nevada: Number of Contributions Over Time Adjusted For Unreported')
dev.off()  

# Figure 4
png('2016-17-RENevadaNContributors.png', width=1200, height=1200/gold)

over.time[contbr_st=='NV',] %>% 
  ggplot(aes(x=month, y=REunique, color=candidate)) +
  geom_line(size=2) +
  scale_x_continuous(breaks = months,
                     labels = (month.name)[months]) +
  theme_bw(base_size = 22) + 
  labs(x=NULL, y=NULL) +
  guides(color=guide_legend(title="", 
                            direction = "horizontal",keywidth = 4, keyheight = 1,
                            title.position = "top",
                            title.theme = element_text(size=25, angle = 0),
                            title.hjust=.5)) + 
  theme(legend.position="bottom")+
  ggtitle('Nevada: Number of Contributors Over Time Adjusted For Unreported')
dev.off() 
