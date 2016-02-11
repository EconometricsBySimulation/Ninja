library(data.table)
library(dplyr)
library(ggplot2)
library(scales)

setwd('Z:/Data/FEC')

#Souce: http://fec.gov/disclosurep/PDownload.do
Clinton <- read.csv('Hillary.csv', header = FALSE,
                    stringsAsFactors=FALSE)[,-19]
names(Clinton) <- Clinton[1,]
Clinton %>% head
Clinton <- data.table(Clinton[-1,])

#Souce: http://fec.gov/disclosurep/PDownload.do
Sanders <- read.csv('Sanders.csv', header = FALSE,
                    stringsAsFactors=FALSE)[,-19]
names(Sanders) <- Sanders[1,]
Sanders %>% head
Sanders <- data.table(Sanders[-1,])

# Create Combined
Sanders$contb_receipt_dt %>% as.Date("%d-%b-%y") %>% max
Sanders$contb_receipt_dt %>% as.Date("%d-%b-%y") %>% min

combined <- rbind(cbind(Sanders, candidate='Sanders'),
                  cbind(Clinton, candidate='Clinton'))

combined$date <- combined$contb_receipt_dt %>% as.Date("%d-%b-%y")

combined$amount <- combined$contb_receipt_amt %>% as.numeric()

combined %>% 
  group_by(candidate) %>%
  summarize(total = sum(amount),
            aver  = mean(amount),
            number= length(amount),
            unique= length(unique(contbr_nm))
  )

# https://www.washingtonpost.com/news/post-politics/wp/2016/01/02/bernie-sanders-raises-more-than-33-million-in-latest-fundraising-quarter/
# For the full 2015 calendar year, Clinton reported raising a total 
# of $112 million, while Sanders said he has raised $73 million.


combined %>% 
  group_by(candidate) %>%
  summarize(total = sum(amount),
            aver  = mean(amount),
            number= length(amount),
            unique= length(unique(contbr_nm))
  )

# https://www.washingtonpost.com/news/post-politics/wp/2015/12/16/bernie-sanders-has-now-received-2-million-campaign-contributions-aides-say/
# 2 Million contributions to Sanders minimum 2015
# (2000000-222014)

# FEC lists 72,796,606 in contributions

# FEC lists 72796606-18733744=54062862 unaccounted for
# 54062862/(2000000-222014) = $30

# Create some categories of recipients
# Not used in post.
combined$size <- "refund"
combined$size[combined$amount>0]    <- "< 101"
combined$size[combined$amount>100]  <- "> 100"
combined$size[combined$amount>300]  <- "> 300"
combined$size[combined$amount>500]  <- "> 300"
combined$size[combined$amount>1000] <- "> 1,000"
combined$size[combined$amount>2000] <- "> 2,000"
combined$size[combined$amount>5000] <- "> 5,000"
combined$size[combined$amount>50000] <- "> 50,000"

combined$size2 <- 0
combined$size2[combined$amount>0]    <- 1
combined$size2[combined$amount>100]  <- 2
combined$size2[combined$amount>300]  <- 3
combined$size2[combined$amount>500]  <- 4
combined$size2[combined$amount>1000] <- 5
combined$size2[combined$amount>2000] <- 6
combined$size2[combined$amount>5000] <- 7
combined$size2[combined$amount>50000] <- 8


combined %>% 
  group_by(candidate, size2) %>%
  summarize(total = sum(amount),
            aver  = mean(amount),
            number= length(amount),
            unique= length(unique(contbr_nm))
  ) %>% arrange(candidate, size2)

setwd('C:/Users/fsmar/Dropbox/Econometrics by Simulation/2016-02-February')

gold <- (1+5^.5)/2

png('2016-09-Histogram.png', width=1200, height=1200/gold)
# Contributions by date
combined %>% 
  subset(amount<10000 & amount>1) %>% 
  ggplot(aes(x=amount, fill=candidate)) + 
  theme_bw(base_size = 22) + theme(legend.position="bottom") +
  geom_histogram(binwidth = .16) +
  facet_grid(candidate ~ .) +
  scale_x_log10(labels=paste0("$",c(1,5,10,50,100,500,1000,5000,10000)), 
                breaks=c(1,5,10,50,100,500,1000,5000,10000))+
  geom_vline(xintercept=2700, linetype=2, color=gray(.5)) +
  geom_text(data=data.table(candidate=c('Sanders', 'Clinton')), 
            aes(x=2700, y=40000), label="Maximum  Individual\nContribution")+
  scale_fill_discrete(guide = FALSE) +
  ggtitle('Contributions for Clinton and Sanders') +
  xlab('') + ylab('# of Contributions')+
  coord_cartesian(xlim=c(1,5500))
dev.off()

combined$month <- combined$date %>% format("%m") %>% as.numeric
combined$monthstr <- combined$date %>% format("%m") %>% as.numeric

# Itemized contributions

png('2016-02-BoxUnrestricted.png', width=1200, height=1200/gold)
combined %>% 
  subset(amount>=1) %>%
  ggplot(aes(month %>% as.factor, amount, fill=candidate), colour=c(gray(.8))) + 
    geom_boxplot(outlier.shape = 4)+ 
    scale_y_log10(labels = comma, breaks=10^(0:6)) +
    geom_hline(yintercept=2700, colour="#990000", linetype="dashed") +
    theme_bw(base_size = 22) + theme(legend.position="bottom") +
    ggtitle("Itemized Contribution Size Over Time") +
    xlab("")+ylab("")+
    scale_x_discrete(labels =month.abb[4:12])+
    guides(fill = guide_legend(title = ""))
dev.off()

# Itemized contributions with Top Donors

png('2016-02-BoxRestricted.png', width=1200, height=1200/gold)
combined %>% 
  subset(amount>=1) %>% 
  ggplot(aes(month %>% as.factor, amount, fill=candidate), colour=c(gray(.8))) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_hline(yintercept=2700, colour="#990000", linetype="dashed") +
  theme_bw(base_size = 22) + theme(legend.position="bottom") +
  ggtitle("Itemized Contribution Size Over Time") +
  xlab("Month") + ylab("") +
  coord_cartesian(ylim=c(0,2700))+
  scale_x_discrete(labels =month.abb[4:12])+
  guides(fill = guide_legend(title = ""))
dev.off()

end     <- '2015-12-31' %>% as.Date
october <- '2015-10-01' %>% as.Date
july    <- '2015-07-01' %>% as.Date
startS   <- min(combined[candidate=="Sanders",date])
startC   <- min(combined[candidate=="Clinton",date])

#####################
# Sanders Unitimized
candidate="Sanders"

  # Year-End: 23,421,034 
  C      <- 23421034
  N      <- round(C/30)
  days   <- as.numeric(end-october)
  d.each <- (C/days)
    
  Sanders.end <- 
    data.table(contbr_nm='unknown', amount=rpois(N,30), 
               candidate=candidate, size="< 101", size2=1,
               date = october+0:days)
  
  # October:  20,187,064
  C      <- 20187064
  N      <- round(C/30)
  days   <- as.numeric(october-july)
  d.each <- (C/days)
  
  Sanders.oct <- 
    data.table(contbr_nm='unknown', amount=rpois(N,30), 
               candidate=candidate, size="< 101", size2=1,
               date = july+0:days)
  
  # July:     10,465,912
  C      <- 10465912
  N      <- round(C/30)
  days   <- as.numeric(july-startS)
  d.each <- (C/days)
  
  Sanders.july <- 
    data.table(contbr_nm='unknown', amount=rpois(N,30), 
               candidate=candidate, size="< 101", size2=1,
               date = startS+0:days)

#####################
# Clinton Unitimized
candidate="Clinton"
  # Year-End: 5,707,408
  C      <- 5707408
  N      <- round(C/30)
  days   <- as.numeric(end-october)
  d.each <- (C/days)
  
  Clinton.end <- 
    data.table(contbr_nm='unknown', amount=rpois(N,30), 
               candidate=candidate, size="< 101", size2=1,
               date = october+0:days)
  
  # October:  5,193,811
  C      <- 5193811
  N      <- round(C/30)
  days   <- as.numeric(october-july)
  d.each <- (C/days)
  
  Clinton.oct <- 
    data.table(contbr_nm='unknown', amount=rpois(N,30), 
               candidate=candidate, size="< 101", size2=1,
               date = july+0:days)
  
  # July:     10,465,912
  C      <- 8098571
  N      <- round(C/30)
  days   <- as.numeric(july-startC)
  d.each <- (C/days)
  
  Clinton.july <- 
    data.table(contbr_nm='unknown', amount=rpois(N,30), 
               candidate=candidate, size="< 101", size2=1,
               date = startC+0:days)
  
  
combined2 <- rbind(combined, 
                   Sanders.end,Sanders.oct,Sanders.july,
                   Clinton.end,Clinton.oct,Clinton.july,
                   fill=TRUE)

diff_ <- function(...) {
  if (min(...)==max(...)) return(max(...))
  max(...)-min(...)
}

sumdate <- 
  combined2 %>% 
  group_by(candidate,date) %>%
  summarize(N = n(),
            ave    = mean(amount),
            med    = median(amount),
            Total  = sum(amount)) %>%
  arrange(candidate, date) %>% 
  group_by(candidate) %>% 
  mutate(cumsumN = cumsum(N),
         cumsumTotal= cumsum(Total)) %>% 
  group_by(date) %>%
  mutate(gapN     = diff_(cumsumN),
         gapTotal = diff_(cumsumTotal)) %>% 
  group_by()

sumdate$month    <- sumdate$date %>% format("%m") %>% as.numeric
sumdate$monthstr <- sumdate$date %>% format("%m") %>% as.numeric

# Number of contributors to the Sander's campaign have 
# vastly exceeded those to the Clinton campaign
png('2016-02-cumN.png', width=1200, height=1200/gold)
sumdate %>%
  ggplot(aes(x=date, y=cumsumN/10^6, color=candidate)) + 
  geom_ribbon(aes(x=date,ymax=gapN/10^6, color=NULL),
              ymin=0,fill="purple", alpha=0.25)+
  geom_line(size=2) +
  theme_bw(base_size = 22) + theme(legend.position="bottom") +
  guides(color = guide_legend(title = "")) +
  ylab("Millions of Contributions") + xlab("") +
  ggtitle("Cumulative Number of Contributions and Difference")+
  geom_vline(xintercept="2015-06-05" %>% as.Date %>% as.numeric,
             linetype=2, color=gray(.5), size=1.25) +
  geom_text(data=sumdate[date=='2015-06-05'],
              y=-.035,label="June 5th",
            color='black') +
  geom_text(data=sumdate[date=='2015-06-05'],
            y=2,label="# Sanders contributions equal\n # Clinton contributions",
            color='black') +
  geom_vline(xintercept="2015-09-20" %>% as.Date %>% as.numeric,
             linetype=2, color=gray(.5), size=1.25) +
  geom_text(data=sumdate[date=='2015-09-20'],
            y=-.035,label="Sept 20th",
            color='black') +
  geom_text(data=sumdate[date=='2015-09-20'],
            y=2,label="# Sanders contributions doubles\n# of Clinton contributions",
            color='black')+
  geom_text(data=sumdate[date=='2015-11-15'],
            y=.25,label="Difference",
            color='black', size=10)
dev.off()

# Total contributions to the Clinton campaign have been matched
# by the Sanders campaign since July.

png('2016-02-cumT.png', width=1200, height=1200/gold)
sumdate %>%
  ggplot(aes(x=date, y=cumsumTotal/10^6, color=candidate)) + 
  geom_ribbon(aes(x=date,ymax=gapTotal/10^6, color=NULL),
              ymin=0,fill="purple", alpha=0.25)+
  geom_line(size=2) +
  theme_bw(base_size = 22) + theme(legend.position="bottom") +
  guides(color = guide_legend(title = "")) +
  ylab("Millions of Dollars Contributed") + xlab("") +
  ggtitle("Cumulative Monetary Contributions and Difference") +
  geom_text(data=sumdate[date=='2015-11-15'],
            y=15,label="Difference",
            color='black', size=10)
dev.off()

#################################################################
#################################################################
