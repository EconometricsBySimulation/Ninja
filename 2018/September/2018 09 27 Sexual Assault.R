library(haven)
library(dplyr)
library(expss)
library(ggplot2)
library(tidyr)
library(reshape2)

x <- 
  read_por("Z:/Data/Sexual Assault/ICPSR_03212-V1/ICPSR_03212/DS0001/03212-0001-Data.por") %>% as_factor 

cbind(sapply(x, function(x) x  %>% attr('label'))) %>%
  write.csv("Z:/Data/Sexual Assault/ICPSR_03212-V1/ICPSR_03212/DS0001/03212-0001-Data-Labels.csv")

x <- lapply(x, as.character) %>% data.frame(stringsAsFactors = FALSE) %>% as.tbl

x$ID <- 1:nrow(x)

x %>% write.csv("Z:/Data/Sexual Assault/ICPSR_03212-V1/ICPSR_03212/DS0001/03212-0001-Data.csv")

x$EDUC  %>% cro # YEARS OF EDUCATION 9 10 F2
x$B_YR  %>% cro # B_YR YEAR OF BIRTH
x$RACE  %>% cro # RACE

x$PARTY %>% cro # ATTEND WILD PARTIES

#########################################################

# Hands shake when doing things.
x$SHAKEH   %>% cro # HANDS SHAKE WHEN DOING THINGS
x$SHAKEH2  %>% cro # HANDS SHAKE WHEN DOING THINGS
x$SHAKEH3  %>% cro # HANDS SHAKE WHEN DOING THINGS
x$SHAKEH4  %>% cro # HANDS SHAKE WHEN DOING THINGS
x$SHAKEH5  %>% cro # HANDS SHAKE WHEN DOING THINGS

cor(cbind(x$RAPED, x$SHAKEH), use = "pairwise", method = c("pearson")) # %>% 
 # htmlTable(caption = "Table with summary statistics and significance marks.")

x$RELAX    %>% cro # RELAX WITHOUT DIFFICULTY
x$RELAX2   %>% cro # RELAX WITHOUT DIFFICULTY
x$RELAX3   %>% cro # RELAX WITHOUT DIFFICULTY
x$RELAX4   %>% cro # RELAX WITHOUT DIFFICULTY
x$RELAX5   %>% cro # RELAX WITHOUT DIFFICULTY

(x$RELAX %>% is.na) %>% mean(na.rm = TRUE)

cor(cbind(x$RAPED, x$RELAX), use = "pairwise", method = c("pearson"))

# Have you had a male attempt sexual intercourse (get on top of you, attempt to insert his penis)
# when you didn't want to by threatening or using some degree of force (twisting your arm, holding
# you down, etc.) but intercourse did not occur?
x$ATTEMPT  %>% cro # ATTEMPTED FORCED INTERCOURSE
x$ATTEMPT2 %>% cro
x$ATTEMPT3 %>% cro
x$ATTEMPT4 %>% cro
x$ATTEMPT5 %>% cro

cor(cbind(x$ATTEMPT, x$RELAX), use = "pairwise", method = c("pearson"))

# Has a male ever deliberately given you alcohol or drugs and attempted to engage in sexual
# intercourse (get on top of you, attempt to insert his penis) when you didn't want to but intercourse
# did not occur?
x$DRUGATT %>% cro

# Have you given in to sexual intercourse when you didn't want to because you were overwhelmed
# by a male's continual arguments and pressure?
x$PRESSSI %>% cro

# Have you engaged in sexual intercourse when you didn't want to because a male used his
# position of authority (boss, teacher, camp counselor, supervisor) to make you?
x$AUTHSI %>% cro

x$AGEEXP  %>% cro

x  %>% filter(!is.na(AGEEXP)) %>% 
  ggplot(aes(AGEEXP)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  ggtitle("") + theme_bw() + ylab("") + xlab("Age at Experience")


# Have you engaged in sexual intercourse when you didn't want to because a male threatened or
# used some degree of physical force (twisting your arm, holding you down, etc.) to make you?
x$FORCESI %>% cro

# Have you ever been in a situation where you had sexual acts with a male such as anal or oral
# intercourse when you didn't want to because he used threats or physical force (twisting your arm,
# holding you down, etc.) to make you?
x$SEXACTS %>% cro

x$KOSS91A1 %>% cro # Nature of sexual experience
x$WHEN90A1 %>% cro # Regarding that experience, when did it happen?
x$AGEEXP   %>% cro # Regarding that experience, how old were you?
x$WHO      %>% cro # Regarding that experience, who was the other person involved?

x$STUDENT  %>% cro # Regarding that experience, was the other person involved a student?
x$AGEWHO   %>% cro # Approximately how old was the other person?

x$TOGETHER %>% cro # Regarding that experience, why were you and the other person together?

x$UNWANTSX %>% cro # If this was an unwanted experience, please indicate the 
                   # various ways you communicated your unwillingness:

x$PAID     %>% cro # Regarding that experience, if purchases were made, i.e., tickets, food, drinks, who paid?
x$WHERE    %>% cro # Regarding this experience, where did it occur?
x$DRUGHE   %>% cro # Regarding this experience, was the other person using any drug at the time?

x$PHYSINJ  %>% cro # Regarding this experience, were you physically injured?
x$TELL     %>% cro # Regarding this experience, whom did you tell?

x$CONSEQ   %>% cro # What was the consequence of this experience for your relationship with the other person?

x$RELEXP   %>% cro # Please describe briefly your relationship after this experience. 
                   # (That is, in what way was it better, the same, worse, or how did it end?)

# Many people have sexual experiences as children. The following questions ask about any experiences
# you may have had before you were 14. For the next set of questions, indicate how often you have had
# each of these experiences. Let:
   
x$SHOORGAN %>% cro # Another person showed his/her sex organs to you or asked you to show yours.

x$FONDLED  %>% cro # A person fondled you in a sexual way or touched your sex organs 
                   # or asked you to touch their sex organs.

x$ATTEMPSI %>% cro # A male attempted intercourse with you (but penetration did not occur).

x$SI       %>% cro #  A male had intercourse with you (penetration occurred; ejaculation not necessary).

# Please think back over your answers to the previous set of questions. Which was the most significant
# experience you had? The following three questions, refer only to that experience. If you never had any of
# these experiences, do not complete.

x$WHODIT   %>% cro # Who was the person who did it ?

x$WHOSTART %>% cro # Who initiated or started this activity?

x$REASON   %>% cro # What is the main reason you participated (choose only one)?
x$SXORIENT %>% cro # What is your sexual orientation?

x$RAPED     %>% cro # Have you ever been raped?
x$RAPED2    %>% cro
x$RAPED3    %>% cro
x$RAPED4    %>% cro
x$RAPED5    %>% cro

cor(cbind(x$RAPED, x$RAPED2, x$RAPED3, x$RAPED4, x$RAPED5), 
    use = "pairwise", method = c("pearson"))

# For the next set of questions, answer how often each of the following has occurred, from the time your
# were 14 to the present. Let:
  
x$XCONSENT  %>% cro # Have you ever had sexual intercourse with a male when you both wanted to?
x$XCONSEN2  %>% cro
x$XCONSEN3  %>% cro
x$XCONSEN4  %>% cro
x$XCONSEN5  %>% cro

# Have you given in to sex play (fondling, kissing or petting but not intercourse) when you didn't
# want to because you were overwhelmed by a male's continual arguments and pressure?
x$XPRESSSP  %>% cro  

# Have you engaged in sex play (fondling, kissing or petting but not intercourse) when you didn't
# want to because a male used his position of authority (boss, teacher, camp counselor, supervisor)
# to make you?
x$XAUTHSP  %>% cro 
x$XAUTHSP2  %>% cro 
x$XAUTHSP3  %>% cro 
x$XAUTHSP4  %>% cro 
x$XAUTHSP5  %>% cro 

# Have you engaged in sex play (fondling, kissing or petting but not intercourse) when you didn't
# want to because a male threatened to use some degree of physical force (twisting your arm,
# holding you down, etc.) to make you?
x$XFORCESP  %>% cro 

# Have you had a male attempt sexual intercourse (get on top of you, attempt to insert his penis)
# when you didn't want to by threatening or using some degree of force (twisting your arm, holding
# you down, etc.) but intercourse did not occur?
x$XATTEMPT  %>% cro 
x$XATTEMP2  %>% cro 
x$XATTEMP3  %>% cro 
x$XATTEMP4  %>% cro 
x$XATTEMP5  %>% cro 

# Has a male ever deliberately given you alcohol or drugs and attempted to engage in sexual
# intercourse (get on top of you, attempt to insert his penis) when you didn't want to but intercourse
# did not occur?
x$XDRUGATT  %>% cro 
x$XDRUGAT2  %>% cro 
x$XDRUGAT3  %>% cro 
x$XDRUGAT4  %>% cro 
x$XDRUGAT5  %>% cro 

x$XPRESSSI  %>% cro # Have you given in to sexual intercourse when you didn't want to because you were overwhelmed
# by a male's continual arguments and pressure?
  
x$XAUTHSI  %>% cro # Have you engaged in sexual intercourse when you didn't want to because a male used his
# position of authority (boss, teacher, camp counselor, supervisor) to make you?

x$XDRUGSI  %>% cro # Has a male ever deliberately given you alcohol or drugs and engaged in sexual intercourse when
# you didn't want to?
  
x$XFORCESI  %>% cro # Have you engaged in sexual intercourse when you didn't want to because a male threatened or
# used some degree of physical force (twisting your arm, holding you down, etc.) to make you?

x$XSEXACTS  %>% cro # Have you ever been in a situation where you had sexual acts with a male such as anal or oral
# intercourse when you didn't want to because he used threats or physical force (twisting your arm,
# holding you down, etc.) to make you?

#########################################################


xlong <- x[,c(key='ID',
              paste0('AUTHSI',c("",2:5)), 
              paste0('DRUGATT',c("",2:5)),
              paste0('PRESSSI',c("",2:5)),
              paste0('FORCESI',c("",2:5)),
              paste0('SEXACTS',c("",2:5)))] %>% gather("key", AUTHSI:SEXACTS5,ID) %>% filter(!value=="Never")

xlong %>% head





xlong <- x[,c(key='ID',varlist) ] %>% melt(id.vars="ID") %>% filter(!value=="Never")

xlong$key[xlong$key=="DRUGATT"] <- "Alcohol/Drug Coerce - Sex"
xlong$key[xlong$key=="PRESSSI"] <- "Continous Pressure - Sex"
xlong$key[xlong$key=="AUTHSI"]  <- "Authority - sex"
xlong$key[xlong$key=="FORCESI"] <- "Threat of Force - Sex"
xlong$key[xlong$key=="SEXACTS"] <- "Threat of Force - Sex Acts"

xlong   %>%
  ggplot(aes(key)) + geom_bar() + 
  ggtitle("") + theme_bw() + ylab("") + xlab("") +
  coord_flip()

################################################ 
# Christine Blasey Ford

# A person fondled you in a sexual way or touched your sex organs or 
# asked you to touch their sex organs.

setwd("Z:/Dropbox/Econometrics by Simulation/2018_09_September/")

png(file="28FONDLED.png",width=800,height=400,res=72)
x %>% filter(!is.na(FONDLED)) %>% ggplot(aes(FONDLED)) + geom_bar() + theme_bw() + 
  geom_bar(data=x %>% filter(!is.na(FONDLED) & FONDLED == "One time"), fill="Red") +
  coord_flip()  + theme_set(theme_grey(base_size = 18)) +
  ggtitle("A person fondled you in a sexual way or touched your \nsex organs or asked you to touch their sex organs.") + xlab("") + ylab("")
dev.off()

# A male attempted intercourse with you (but penetration did not occur).
png(file="28ATTEMPT.png",width=800,height=400,res=72)
x %>% filter(!is.na(ATTEMPT)) %>% ggplot(aes(ATTEMPT)) + geom_bar() + theme_bw() + 
  geom_bar(data=x %>% filter(!is.na(ATTEMPT) & ATTEMPT == "At least once"), fill="Red") +
  theme_set(theme_grey(base_size = 18)) +
  coord_flip() + ggtitle("A male attempted intercourse with you through use of force.") + xlab("") + ylab("")
dev.off()

png(file="28AGEEXP.png",width=800,height=400,res=72)
x %>% filter(!is.na(AGEEXP))   %>% ggplot(aes(AGEEXP)) + geom_bar() + theme_bw() + 
  geom_bar(data=x %>% filter(!is.na(AGEEXP) & AGEEXP == "15"), fill="Red") +
  theme_set(theme_grey(base_size = 18)) +
  coord_flip() + ggtitle("Regarding that experience, how old were you?") + xlab("") + ylab("")
dev.off()

png(file="28WHO.png",width=800,height=400,res=72)
x %>% filter(!is.na(WHO))   %>% ggplot(aes(WHO)) + geom_bar() + theme_bw() + 
  geom_bar(data=x %>% filter(!is.na(WHO) & WHO == "Casual acquaintance"), fill="Red") +
  theme_set(theme_grey(base_size = 18)) +
  coord_flip() + ggtitle("Regarding that experience, who was the other person involved?") + xlab("") + ylab("")
dev.off()

png(file="28AGEWHO.png",width=800,height=400,res=72)
x %>% filter(!is.na(AGEWHO))   %>% ggplot(aes(AGEWHO)) + geom_bar() + theme_bw() + 
  geom_bar(data=x %>% filter(!is.na(AGEWHO) & AGEWHO == "Less than 5 years older"), fill="Red") +
  theme_set(theme_grey(base_size = 18)) +
  coord_flip() + ggtitle("Approximately how old was the other person?") + xlab("") + ylab("")
dev.off()

png(file="28UNWANTSX.png",width=800,height=400,res=72)
x %>% filter(!is.na(UNWANTSX))   %>% ggplot(aes(UNWANTSX)) + geom_bar() + theme_bw() + 
  geom_bar(data=x %>% filter(!is.na(UNWANTSX) & UNWANTSX == "Verbally & physically"), fill="Red") +
  theme_set(theme_grey(base_size = 18)) +
  coord_flip() + ggtitle("Please indicate the various ways you \ncommunicated your unwillingness:") + xlab("") + ylab("")
dev.off()


################################################ 
#### Inferring information
x_Infer <- x

vlist <- c('AUTHSI' , 'AUTHSP' , 'DRUGATT', 'DRUGSI', 'PRESSSI', 'PRESSSP',
           'FORCESI', 'FORCESP', 'SEXACTS', 'RAPED' , 'ATTEMPT')

varlist <- expand.grid(vlist, c("",2:5)) %>% apply(1,paste,collapse="") %>% sort

x_Infer[x_Infer %>% is.na] <- ""

x_Infer <- x_Infer[,order(colnames(x_Infer))]

values <-  c("","2","3","4","5")
for (s in vlist) for (i in 1:5) {
  L <- paste0(s,values[i]) 
  x_Infer[x_Infer[,L] == "At least once", L] <- "Yes"
  x_Infer[x_Infer[,L] == "Never", L] <- "No"
}

#### Drop out
xRAPED1 <- x_Infer %>% filter(RAPED == "Yes" & RAPED2 != "") %>% nrow

x_Infer %>% select(RAPED2, AUTHSI2, DRUGATT2, PRESSSI2, FORCESI2, SEXACTS2) %>% print(n=300)

values <-  c("","2","3","4","5")
for (s in vlist) for (i in 1:4) {

  L <- paste0(s,values[i]) 
  R <- paste0(s,values[i+1]) 
  
  xselect <- (sapply(x_Infer[L],substr,1,3) == "Yes") &
    (x_Infer[, R] == "" | x_Infer[, R] == "No")
  
  x_Infer[xselect, R] <- "Yes(I)"
}

x_Infer %>% select(AUTHSI:AUTHSI5)

x_Infer[x_Infer$AUTHSI == "Yes",paste0('AUTHSI',c("",2:5))]
x_Infer[x_Infer$RAPED  == "Yes",paste0('RAPED' ,c("",2:5))] %>% print(n=66)
x_Infer[x_Infer$RAPED2 == "Yes",paste0('RAPED' ,c("",2:5))] %>% print(n=69)
x_Infer[x_Infer$RAPED2 == "Yes(I)",paste0('RAPED' ,c("",2:5))] %>% print(n=69)
x_Infer[x_Infer$RAPED3 == "Yes",paste0('RAPED' ,c("",2:5))] %>% print(n=120)
x_Infer[x_Infer$RAPED4 == "Yes",paste0('RAPED' ,c("",2:5))] %>% print(n=40)
x_Infer[x_Infer$RAPED5 == "Yes",paste0('RAPED' ,c("",2:5))] %>% print(n=40)

xlong <- x_Infer[,c(key='ID',varlist)] %>% melt(id.vars="ID") %>% filter(!(value=="No" | value==""))

xlong$Inferred <- "Reported"
xlong$Inferred[grepl("(I)", xlong$value)] <- "Inferred"

xlong$value <- sapply(xlong$value,substr,1,3)

xlong   %>%
  ggplot(aes(variable, fill = Inferred)) + geom_bar(aes(y = (..count..)/1580)) + 
  ggtitle("") + theme_bw() + ylab("") + xlab("") +
  coord_flip()

# ATTEMPT	I: ATTEMPTED FORCED INTERCOURSE
# I: SEX PLAY BECAUSE OF FORCE
# I: INTERCOURSE BECAUSE OF FORCE
# I: SEXUAL ACTS BECAUSE OF FORCE
# SEXUAL ACTS BECAUSE OF FORCE

vlistF <- expand.grid(c( 'ATTEMPT', 'FORCESP', 'FORCESI', 'SEXACTS') , c("",2:5)) %>% 
  apply(1,paste,collapse="")

force <- xlong %>% filter(variable %in% vlistF) %>% lapply(as.character) %>% data.frame(stringsAsFactors = FALSE) %>% as.tbl

force$variable <- gsub("ATTEMPT","ATTEMPTED FORCED INTERCOURSE",force$variable)
force$variable <- gsub("FORCESP","SEX PLAY BECAUSE OF FORCE",force$variable)
force$variable <- gsub("FORCESI","INTERCOURSE BECAUSE OF FORCE",force$variable)
force$variable <- gsub("SEXACTS","SEXUAL ACTS BECAUSE OF FORCE",force$variable)

png(file="28ForceStudy.png",width=800,height=800,res=72)
force %>%
  ggplot(aes(variable, fill = Inferred)) + geom_bar(aes(y = (..count..)/1580)) + 
  ggtitle("") + theme_bw() + ylab("") + xlab("") +
  coord_flip()
dev.off()

# II: INTERCOURSE BECAUSE OF AUTHORITY
# II: SEX PLAY BECAUSE OF AUTHORITY

vlistA <- expand.grid(c('AUTHSI', 'AUTHSP', 'PRESSSI', 'PRESSSP', 'DRUGSI', 'DRUGATT'), c("",2:5)) %>% 
  apply(1,paste,collapse="")

authority <- xlong %>% filter(variable %in% vlistA) %>% lapply(as.character) %>% data.frame(stringsAsFactors = FALSE) %>% as.tbl

authority$variable <- gsub("AUTHSI","1. INTERCOURSE BECAUSE OF AUTHORITY",authority$variable)
authority$variable <- gsub("AUTHSP","2. SEX PLAY BECAUSE OF AUTHORITY",   authority$variable)
authority$variable <- gsub("PRESSSI","3. INTERCOURSE BECAUSE OF PRESSURE",   authority$variable)
authority$variable <- gsub("PRESSSP","4. SEX PLAY BECAUSE OF PRESSURE",   authority$variable)
authority$variable <- gsub("DRUGSI","5. INTERCOURSE BECAUSE OF DRUGS",authority$variable)
authority$variable <- gsub("DRUGATT","6. ATTEMPTED INTERCOURSE B/C DRUGS",   authority$variable)

png(file="28authority.png",width=800,height=800,res=72)
authority  %>%
  ggplot(aes(variable, fill = Inferred)) + geom_bar(aes(y = (..count..)/1580)) + 
  ggtitle("") + theme_bw() + ylab("") + xlab("") +
  coord_flip()
dev.off()

