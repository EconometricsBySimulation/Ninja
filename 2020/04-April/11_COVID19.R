
library(readxl)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
require(scales)
library(lubridate)
library(ggrepel)

setwd("Z:\\Dropbox\\Econometrics by Simulation\\2020_04_April\\COVID2019")

influenza = read_excel("Influenza.xlsx") %>% filter(Season == "2019-2020")

influenza$date = influenza$`Week Ending Date` %>% ymd

influenzasum = influenza %>% group_by(date) %>% summarize(flucases = sum(Count)) %>% arrange(date) %>% 
  mutate(totalflucases = cumsum(flucases)) %>% print(n=100)

# Spreadsheet US.xlsx was copied daily from tables from https://www.worldometers.info/coronavirus/
# Which was in turn copied from wikipedia 
# https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_the_United_States

sheets = excel_sheets("US.xlsx")
# Copy this sheet from 
# https://www.dropbox.com/s/gmjnoj4zdnq4mfg/US.xlsx?dl=0
# Or google sheet:
# https://docs.google.com/spreadsheets/d/1YwXwCsccrRTujI2sMQ8c2KK1idwNJ2B_uRreGTYxaDA/edit?usp=sharing


covid2019US = data.frame()%>% as_tibble() 

# Read in each tab in the spreadsheet
for (i in 1:length(sheets)){
  print(i)
  
  dayholder <- read_excel("US.xlsx", sheet=sheets[i])
  names(dayholder) = paste(names(dayholder), dayholder[1,]) %>% 
    str_replace_all("(_[1-2]?|\\s+|/)", "_") %>%
    str_replace_all("(_+)", "_") 
  dayholder = dayholder[-1,] 
  
  dayholder$day = sheets[i]
  
  covid2019US = rbind.fill(covid2019US, dayholder)  %>% as_tibble() 
}
  
# Encode Dates
covid2019US$date = mdy(paste(covid2019US$day,"2020"))

# Replace NA with zeros
covid2019US[is.na(covid2019US)] = 0

# Encode Cases and Deaths as numbers
covid2019US$Total_Cases  = as.integer(covid2019US$Total_Cases)
covid2019US$Total_Deaths = as.integer(covid2019US$Total_Deaths)

# Plot the total number of cases in New York
covid2019US %>% filter(USA_State == "New York") %>%
  ggplot(aes(x=date, y = Total_Cases)) + 
  theme_bw() +
  geom_hline(yintercept = 157426, size=.8, linetype="dashed", color=gray(.7)) +
  annotate("text", x = mean(covid2019US$date), y = 157426+3000, label = "2019/2020 - New York Flu cases", vjust=0, hjust=.5) + 
  geom_line(size=1.5) + geom_point(size=3) +
  geom_vline(xintercept = ymd("2020 03 20"), size = 1, color = "darkgreen", linetype = "dashed") +
  annotate("text", x = ymd("2020 03 20") + .3, y = 
           (covid2019US %>% filter(USA_State == "New York"))$Total_Cases %>% mean(na.rm = TRUE), 
          label = "City Lockin Order", vjust=.25, hjust=0) + 
  annotate("text", x = max(covid2019US$date)-.5, y = 
             max(covid2019US %>% filter(USA_State == "New York") %>% 
                   select(Total_Cases)), label = "COVID-19 Reported Cases", vjust=.25, hjust=1) + 
  geom_line(aes(y = Total_Deaths),size=1.5, color="darkred") + 
  annotate("text", x = max(covid2019US$date)-.5, y = 
             max(covid2019US %>% filter(USA_State == "New York") %>% 
                   select(Total_Deaths)) + 5000, label = "COVID-19 Reported Deaths", vjust=0, hjust=1) + 
  geom_point(aes(y = Total_Deaths), size=3, color="darkred") +
  scale_y_continuous(labels = comma) +
  ylab("") + ggtitle("New York Reported Cases and Deaths of COVID 2019")

# Plot the total number of Deaths in New York
covid2019US %>% filter(USA_State == "New York") %>%
  ggplot(aes(x=date, y = Total_Deaths)) +
  geom_vline(xintercept = ymd("2020 03 20"), size = 1, color = "darkgreen", linetype = "dashed") +
  annotate("text", x = ymd("2020 03 20") + .3, y = 
             max(covid2019US %>% filter(USA_State == "New York") %>% 
                   select(Total_Deaths)) %>% `/`(3), label = "State 'Stay Home' Order", vjust=.25, hjust=0) + 
  geom_hline(yintercept = 4517, size=.8, linetype="dashed", color=gray(.7)) +
  annotate("text", x = min(covid2019US$date), y = 4517-60, label = "2016/2017", vjust=1, hjust=0) + 
  geom_hline(yintercept = 4881, size=.8, linetype="dashed", color=gray(.7)) +
  annotate("text", x = min(covid2019US$date), y = 4881+60, label = "2015 Flu Deaths Statewide", vjust=0, hjust=0) + 
  geom_hline(yintercept = 4702, size=.8, linetype="dashed", color=gray(.7)) +
  annotate("text", x = min(covid2019US$date), y = 4702+30, label = "2014", vjust=0.5, hjust=0) + 
  theme_bw() +
  geom_line(size=2, color="darkred") + 
  geom_point(size=4, color="darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("New York COVID2019 Deaths") 

# Create a measure for total number of deaths accross dates
covid2019US = covid2019US %>% group_by(USA_State) %>% mutate(total_cases = max(Total_Cases))

# Not currently used
covid2019US = covid2019US %>% group_by() %>% mutate(rank = dense_rank(total_cases))

# Compare state infection trends
covid2019US %>% filter(total_cases > 5000, USA_State != "Total:", USA_State != "USA Total") %>%
  ggplot(aes(x=date, y = Total_Cases, color=USA_State, label=USA_State)) +
  theme_bw() + 
  geom_line(size=2) + 
  scale_y_log10() +
  geom_text(data = covid2019US %>% 
              filter(total_cases > 5000, USA_State != "Total:", USA_State != "USA Total", 
                     ((as.numeric(date)+rank)/5)==round((as.numeric(date)+rank)/5)  ), vjust=0, color="black") +
  xlab("") + ylab("Number of Cases")  + theme(legend.position = "none")
  
# New Cases/Total cases this number naturally falls over time
covid2019US %>% arrange(USA_State, date) %>% group_by(USA_State) %>% 
  mutate(new_cases = (Total_Cases - lag(Total_Cases, 3))/3, new_cases_change = new_cases/Total_Cases) %>% 
  select(date, new_cases, new_cases_change) %>% filter(!is.na(new_cases)) %>% print(n=1000)

# Find Day 0 cases for new york
NYmin = (covid2019US %>% filter(USA_State == "New York"))$Total_Cases %>% min

# Find Day 0-today cases for all states
covid2019US = covid2019US %>% group_by(USA_State) %>% filter(Total_Cases >= NYmin) %>% 
  mutate(firstdate = min(date), day=date - firstdate, maxday = max(day))

# Day 0 graph
covid2019US %>% filter(total_cases >= 5000, USA_State != "Total:", USA_State != "USA Total") %>%
  ggplot(aes(x=day, y = Total_Cases, color=USA_State, label=USA_State)) +
  theme_bw() + 
  geom_line(size=2) + 
  scale_y_log10() +
  geom_text_repel(data = covid2019US %>% filter(day == maxday, USA_State != "Total:", USA_State != "USA Total"), 
            vjust=0, color="black") +
  xlab("Day") + ylab("Number of Cases")  + 
  theme(legend.position = "none")

# Prepare national data for graphing new cases
total_change = covid2019US %>% filter(USA_State == "Total:") %>% arrange(date) %>% 
  mutate(new_cases = Total_Cases - lag(Total_Cases), 
         new_cases_smooth = (new_cases + lag(new_cases) + lead(new_cases))/3)

# Graph of new cases by day with LM fit line
total_change %>% select(date, new_cases) %>% group_by() %>%
  ggplot(aes(x=date,y=new_cases)) + 
  theme_bw() +
  geom_point(size=2, color="darkblue") +
  geom_smooth(method = "lm") +
  ggtitle("# of New Cases Per Day in USA") +
  ylab("")

ggsave("NumberOFNewCases.png", width = 8, height = 6, dpi = 100)
