library(utils)
library(dplyr)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(caTools)
library(foreign)
library(reshape2)

setwd('Z:/Data/EconometricsBySimulation/Fileformats')

# A function to generate random strings of unique characters
jumbler <- function(
  invector, #The vector of strings to be jumbled
  nsample=2,  #Number of times the vector will be sampled from
  outlength=NULL #The length of the resulting vector
  ) {
  
  # If outlength is missing then the outlength 
  # is the length of the input vector
  if (length(outlength)==0) outlength=length(invector)
  
  # Sample once 
  out <- sample(invector,outlength, replace=TRUE)

  # Sample and add on values for draws greater than 1
  if (nsample>1) for (i in 2:nsample) 
    out <- paste0(out, sample(invector, outlength, replace=TRUE))
  
  out
}

# Specify the number of rows of data to generate
nrows <- 10^5

# Generate the factors
factors04 <- jumbler(letters,4, 100)
factors16 <- jumbler(letters,16, 100)
factors64 <- jumbler(letters,64, 100)

# Generate a large data frame
largedata <- data.frame(
  index=seq(nrows), 
  wholenum=rpois(nrows,100),  
  bignum=2^(runif(nrows)*20),
  precision=rnorm(nrows),
  factor04=sample(factors04, nrows, replace=TRUE) %>% factor,
  factor_string04=sample(factors04, nrows, replace=TRUE),
  factor16=sample(factors16, nrows, replace=TRUE) %>% factor,
  factor_string16=sample(factors16, nrows, replace=TRUE),
  factor64=sample(factors64, nrows, replace=TRUE) %>% factor,
  factor_string64=sample(factors64, nrows, replace=TRUE),
  rand04=jumbler(letters,4, nrows),
  rand16=jumbler(letters,16, nrows),
  rand64=jumbler(letters,64, nrows)
  )


# Let's save files as R rds format
saveRDS(largedata, file="dataframe.rds")
for (i in names(largedata)) saveRDS(largedata[,i], file=paste0(i,'.rds'))

# Now let's save files as csv format
write.csv(largedata, file="dataframe.csv", row.names=FALSE)
for (i in names(largedata)) write.csv(largedata[,i], file=paste0(i,'.csv'), row.names=FALSE)

# Use the foriegn package to write stata dta, sas, and sps files
write.dta(largedata, file="dataframe.dta")
for (i in names(largedata)) write.dta(largedata[,i] %>% data.frame, file=paste0(i,'.dta'))

# Save the files as dput text files
dput(largedata, file="dataframe.txt")
for (i in names(largedata)) dput(largedata[,i], file=paste0(i,'.txt'))

# Let's try zipping the files
for (i in dir(pattern="csv$|rds$|txt$|dta$")) zip(zipfile = paste0(i,'.zip'), files = i)

# For sas and spss files we add both files to the archive
for (i in dir(pattern="sps$|sas$")) zip(zipfile = paste0(i,'.zip'), files = c(i, paste0(i,"s")))

sizes <- dir() %>% sapply(file.info) %>% `[`('size',) %>% unlist %>% 
  data.frame(name=names(.), filesize=.)

sizes$stem <- gsub("\\.(.*)$", "", sizes$name) 

sizes$compression <- "none"
sizes$compression[grepl('zip', sizes$name)]  <- "zip"

sizes$filetype <- sizes$stem %>% unique %>% c("zip", "\\.") %>% paste( collapse='|') %>% 
                    gsub("", sizes$name) %>% gsub("ss", "s", .)

sizesMerge <- sizes %>% group_by(stem, filetype, compression) %>% summarize(filesize=sum(filesize))

sizeRatios <- sizesMerge[sizesMerge$compression!="zip",-3] %>%
  data.frame(sizesMerge[sizesMerge$compression=="zip",][,"filesize"])

names(sizeRatios) <- c('stem', 'filetype', 'filesize', 'compressed')

sizeRatios$ratio <- sizeRatios$compressed / sizeRatios$filesize

sizeRatios$stemrank <- dense_rank(sizeRatios$stem)

cols <- brewer.pal(8,"Dark2")

sizeRatios %>% ggplot(aes(y=ratio, x=stemrank, color=filetype)) +
  geom_point(size=4, alpha=1)  + 
  geom_line(size=3, alpha=.5) +
  theme_bw() + guides(color=FALSE) +
  labs(x = "", y = "Ratio") + 
  coord_cartesian(ylim=c(-.1,1.05), xlim=c(1,max(sizeRatios$stemrank))) +
  scale_x_discrete() +
  geom_text(aes(label=stem), y=0, color="black", angle = 90, hjust=1, size=4) +
  geom_text(aes(label=filetype, x=stemrank+.1), color="black", hjust=0, vjust=.14) +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())

sizesMerge$stemrank <- dense_rank(sizesMerge$stem)

sizeRatios$min = apply(cbind(sizeRatios$filesize, sizeRatios$compressed), 1, min)
sizeRatios$compression = "none"
sizeRatios$compression[sizeRatios$compressed==sizeRatios$min] <- "zip"

# Uncompressed
sizeRatios %>% ggplot(aes(y=filesize/1024, x=stemrank, color=filetype, label=filetype)) + 
  geom_point(size=4, alpha=1)  + 
  geom_line(size=3, alpha=.5) +
  theme_bw() + 
  scale_x_discrete() +
  labs(x = "", y = "KB") +
  guides(color=FALSE) +
  geom_text(aes(label=stem), y=0, color="black", angle = 90, hjust=0, size=4) +
  geom_text(aes(label=filetype, x=stemrank+.1), color="black", hjust=0, vjust=.14) +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  coord_cartesian(ylim=c(100, 10^4), xlim=c(1,max(sizeRatios$stemrank))) 
ggsave("SizeUnzipped.png")

# Log Scale
sizeRatios %>% ggplot(aes(y=filesize/1024, x=stemrank, color=filetype, label=filetype)) + 
  geom_point(size=4, alpha=1)  + 
  geom_line(size=3, alpha=.5) +
  theme_bw() + 
  scale_x_discrete() +
  labs(x = "", y = "KB") +
  scale_y_log10() + guides(color=FALSE) +
  geom_text(aes(label=stem), y=log10(100), color="black", angle = 90, hjust=0, size=5) +
  geom_text(aes(label=filetype, x=stemrank+.1), color="black", hjust=0, vjust=.14) +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  coord_cartesian(ylim=c(100, max(sizeRatios$filesize/1024)), xlim=c(1,max(sizeRatios$stemrank))) 
ggsave("SizeUnzippedlog10.png")


# Uncompressed
sizeRatios %>% ggplot(aes(y=compressed/1024, x=stemrank, color=filetype, label=filetype)) + 
  geom_point(size=4, alpha=1)  + 
  geom_line(size=3, alpha=.5) +
  theme_bw() + 
  scale_x_discrete() +
  labs(x = "", y = "KB") +
  guides(color=FALSE) +
  geom_text(aes(label=stem), y=0, color="black", angle = 90, hjust=0, size=4) +
  geom_text(aes(label=filetype, x=stemrank+.1), color="black", hjust=0, vjust=.14) +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  coord_cartesian(xlim=c(1,max(sizeRatios$stemrank)))  
ggsave("SizeZipped.png")


# Log Scale
sizeRatios %>% ggplot(aes(y=compressed/1024, x=stemrank, color=filetype, label=filetype)) + 
  geom_point(size=4, alpha=1)  + 
  geom_line(size=3, alpha=.5) +
  theme_bw() + 
  scale_x_continuous(breaks= 1:14) +
  labs(x = "", y = "KB") +
  scale_y_log10() + guides(color=FALSE) +
  geom_text(aes(label=stem), y=log10(3000), color="black", angle = 90, hjust=.5, size=4) +
  geom_text(aes(label=filetype, x=stemrank+.1), color="black", hjust=0, vjust=.14) +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank())+
  coord_cartesian(ylim=c(100, max(sizeRatios$compressed/1024)), xlim=c(1,max(sizeRatios$stemrank))) 
ggsave("SizeZippedlog10.png")

filetypes <- unique(sizeRatios$filetype)

sims <- 50
  
times <- matrix(NA, nrow=sims, ncol = length(filetypes)*2)
  colnames(times) = paste0(filetypes, rep(c("",".zip"), each=length(filetypes)))

for (i in 1:sims) {
  print(i)
  times[i,'csv'] <- system.time(read.csv(file="dataframe.csv"))[1]
  times[i,'rds'] <- system.time( readRDS(file="dataframe.rds"))[1]
  times[i,'dta'] <- system.time(read.dta(file="dataframe.dta"))[1]
  times[i,'txt'] <- system.time(    dget(file="dataframe.txt"))[1]
  
  times[i,'csv.zip'] <- system.time({
    unzip('dataframe.csv.zip', exdir='temp')
    read.csv(file="temp/dataframe.csv")
  })[1]
  times[i,'rds.zip'] <- system.time({
    unzip('dataframe.rds.zip', exdir='temp')
    readRDS(file="temp/dataframe.rds")
  })[1]
  times[i,'dta.zip'] <- system.time({
    unzip('dataframe.dta.zip', exdir='temp')
    read.dta(file="temp/dataframe.dta")
  })[1]
  times[i,'txt.zip'] <- system.time({
    unzip('dataframe.txt.zip', exdir='temp')
    dget(file="temp/dataframe.txt")
  })[1]
}                 

# Collapse the times to long format
speed <- times %>% melt
names(speed) <- c('index','file','seconds')

# Identify as either zip or uncompressed files
speed$zip <- "raw"
speed$zip[speed$file %>% grepl("zip",.)] <- "zip"

# Organize file names alphabetically
speed$file2 <- factor(speed$file,
    levels = speed$file %>% unique %>% as.character %>% sort,ordered = TRUE)

# Plot
speed %>% ggplot(aes(x=file2, y=seconds)) + 
  geom_boxplot() +
  labs(x = "", y = "Seconds") +
  theme_bw()

ggsave("SaveSpeed.png")
