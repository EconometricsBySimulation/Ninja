##################################################################
# Funds Data

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
combined <- rbind(cbind(Sanders, candidate='Sanders'),
                  cbind(Clinton, candidate='Clinton'))

combined[,date   := contb_receipt_dt   %>% as.Date("%d-%b-%y")]
combined[,amount := contb_receipt_amt  %>% as.numeric()]

to.data.table <- function(x) {
  (class(x) <- class(data.table()))
  x
} 

#####################################################
# occupational analysis

setnames(combined, 'contbr_occupation', 'occ')

occupations <-
  merge(
  as.data.table(table(combined[candidate=='Clinton', occ])),
  as.data.table(table(combined[candidate=='Sanders', occ])),
  by='V1')

occupations[, Nxy := N.x+N.y]

occupations <- occupations %>% arrange(-Nxy)

occupations[101:200,1:4, with=FALSE]

occupations %>% setnames(., names(.), c('occ', 'Clinton', 'Sanders', 'Total'))

occupations[, unemployed := occ %in% 
              C('NOT EMPLOYED,NONE, 
                RETIRED, RETIRED PROFESSOR, 
                PROFESSOR EMERITUS,
                UNEMPLOYED, RETIRED TEACHER')]

occupations[, missing := occ %in% 
              C('INFORMATION REQUESTED', '')]

occupations[, financial := occ %in% 
              C('FINANCIAL ADVISOR,BANKER, FINANCE, 
                CPA, ACCOUNTANT, FINANCIAL ADVISOR,
                REAL ESTATE BROKER, INVESTOR,
                MARKETING MANAGER,
                ACCOUNT MANAGER, FINANCIAL ANALYST,
                BUSINESS ANALYST')]

occupations[, management := occ %in% 
              C('CEO, PRESIDENT, 
                VICE PRESIDENT, CFO, 
                GENERAL MANAGER,  
                ENTREPRENEUR,  
                PRODUCT MANAGER, 
                PARTNER, EXECUTIVE DIRECTOR,
                ADMINISTRATION,MANAGEMENT CONSULTANT, 
                OPERATIONS MANAGER, COO,
                EXECUTIVE VICE PRESIDENT, CHAIRMAN,
                SUPERVISOR,
                SENIOR VICE PRESIDENT,
                BUSINESS, PRESIDENT & CEO,
                FOUNDER,
                EXECUTIVE ASSISTANT,
                MANAGING DIRECTOR,
                MANAGEMENT,
                MANAGEMENT CONSULTANT,
                DIRECTOR,
                DIRECTOR OF OPERATIONS,
                BUSINESS MANAGER')]

occupations[, law := occ %in% 
              C('ATTORNEY, LAWYER, PARALEGAL, LAW PROFESSOR,
                LEGAL ASSISTANT')]

occupations[, medical := occ %in% 
              C('PHYSICIAN, PHYSICIAN, RN, REGISTERED NURSE,
                DENTIST,NURSE PRACTITIONER, ACUPUNCTURIST,
                PSYCHOLOGIST, PSYCHIATRIST, 
                PHARMACIST, PSYCHOTHERAPIST,
                MEDICAL DOCTOR, PEDIATRICIAN,
                CLINICAL SOCIAL WORKER,
                CHIROPRACTOR,
                CLINICAL PSYCHOLOGIST, 
                PHYSICAL THERAPIST, 
                DOCTOR, HEALTHCARE,MD,
                COUNSELOR')]

occupations[, academics := occ %in% 
              C('PROFESSOR, COLLEGE PROFESSOR, GRADUATE STUDENT,
                RESEARCHER, SCIENTIST, ASSOCIATE PROFESSOR,
                PHYSICIST, CHEMIST,
                RESEARCH SCIENTIST, LECTURER,
                ASSISTANT PROFESSOR, ADJUNCT PROFESSOR,
                FACULTY, ECONOMIST')]

occupations[, engineers := occ %in% 
              C('ENGINEER, SOFTWARE ENGINEER, SOFTWARE DEVELOPER, 
                IT CONSULTANT, TECHNICIAN, IT, COMPUTER PROGRAMMER,
                WEB DEVELOPER, PROGRAMMER, SOFTWARE ARCHITECT,
                MECHANICAL ENGINEER, IT MANAGER, ELECTRICAL ENGINEER,
                DATA ANALYST, SOFTWARE,
                SYSTEMS ENGINEER, INFORMATION TECHNOLOGY,
                CIVIL ENGINEER')]

occupations[, self.employed := occ %in% 
              C('SELF EMPLOYED, SMALL BUSINESS OWNER, CONTRACTOR,
                ELECTRICIAN, SELF, BUSINESS OWNER, OWNER,
                BUILDER, PAINTER, CHEF, CONSTRUCTION,
                CARPENTER, FARMER')]

occupations[, artists := occ %in% 
              C('ARTIST, WRITER, FILMMAKER, PRODUCER,
                GRAPHIC DESIGNER, ACTOR, PHOTOGRAPHER,
                EDITOR, ARCHITECT, MUSICIAN,
                DESIGNER, CREATIVE DIRECTOR, ACTRESS,
                TECHNICAL WRITER, INTERIOR DESIGNER,
                AUTHOR')]

rbind(
  clinton = c(
  occupations %>% 
  summarize(unemployed=sum(unemployed*Clinton),
            financial=sum(financial*Clinton),
            missing=sum(missing*Clinton),
            management=sum(management*Clinton),
            law=sum(law*Clinton),
            medical=sum(medical*Clinton),
            academics=sum(academics*Clinton),
            engineers=sum(engineers*Clinton),
            self.employed=sum(self.employed*Clinton),
            artists=sum(artists*Clinton))),
  sanders=c(
    occupations %>% 
    summarize(unemployed=sum(unemployed*Sanders),
              financial=sum(financial*Sanders),
              missing=sum(missing*Sanders),
              management=sum(management*Sanders),
              law=sum(law*Sanders),
              medical=sum(medical*Sanders),
              academics=sum(academics*Sanders),
              engineers=sum(engineers*Sanders),
              self.employed=sum(self.employed*Sanders),
              artists=sum(artists*Sanders)))
) %>% t


occup <- 
  combined %>% group_by(candidate) %>% 
  mutate(totalFunds=sum(amount)) %>% 
  group_by(candidate, occ) %>%
  dplyr::summarize(
    count=n(),
    total=sum(amount),
    share=sum(amount)/mean(totalFunds)
  ) %>% arrange(candidate, -share) %>% to.data.table

occup[candidate=='Clinton',][1:100,]
occup[candidate=='Sanders',][1:100,]


C <- function(x) x %>% 
  strsplit(',') %>% `[[`(1) %>% gsub('^\\s+|\\s+$','',.)

combined <- combined %>% to.data.table

combined[, category := 'other']

combined[occ %in% C(
'NOT EMPLOYED,
NONE, 
UNEMPLOYED,
HOMEMAKER'), category := 'Unemployed']

combined[occ %in% C(
'RETIRED,
RETIRED PROFESSOR, 
PROFESSOR EMERITUS,
RETIRED TEACHER,
RETIRED ATTORNEY'), category := 'Retired']

combined[occ %in% C(
'TEACHING ASSISTANT,
TEACHER,
EDUCATOR,
SCHOOL PRINCIPAL,
EDUCATION,
PRINCIPAL,
INSTRUCTOR,
STUDENT'), category := 'Teachers/Students']

#combined[occ %in% C(
#'
#WATER AND WASTEWATER TREATMENT OPERATO,
#LIBRARIAN'), category := 'government']

combined[occ %in% C('INFORMATION REQUESTED, INFO REQUESTED,,N/A'),
         category := 'Missing']

combined[occ %in% C(
'VOLUNTEER, 
PHILANTHROPIST, 
COMMUNITY VOLUNTEER,
SOCIAL WORKER'), category := 'Philanthropist']

#combined[occ %in% C(
#'MILITARY FAMILY LIFE COUNSELOR'),category := 'military']

combined[occ %in% 
C('FINANCIAL ADVISOR,
BANKER, 
FINANCE,
BROKER,
CPA, 
INVESTMENT ADVISOR,
ACCOUNTANT, 
FINANCIAL ADVISOR,
REAL ESTATE BROKER, 
INVESTOR,
INVESTMENT BANKER,
MARKETING MANAGER,
ACCOUNT MANAGER,
INVESTMENT BANKING,
FINANCIAL ANALYST,
BUSINESS ANALYST,
VENTURE CAPITALIST,
CONSULTANT,
PRIVATE EQUITY,
AUDIT & REVIEW ANALYST,
INVESTMENT MANAGEMENT,
BUSINESS CONSULTANT,
BANKING,
BUSINESS DEVELOPMENT,
FINANCIAL SERVICES,
PORTFOLIO MANAGER,
ASSET MANAGEMENT,
TRADER,
ANALYST,
INVESTMENT MANAGER,
SENIOR ADVISOR,
INSURANCE,
ADVISOR'), category := 'Finance']

combined[occ %in% 
           C(
'SALES,
MARKETING,
COMMUNICATIONS,
LOBBYIST,
GOVERNMENT RELATIONS,
PUBLIC RELATIONS,
ADVERTISING,
MEDIA CONSULTANT,
COMMUNICATIONS CONSULTANT
'), category := 'Public Relations']

combined[occ %in% C(
'CHIEF OPERATING OFFICER,
CHIEF EXECUTIVE OFFICER,
CFO, 
CHIEF FINANCIAL OFFICER,
CTO,
CEO,
CIO,
CEO & PRESIDENT,
FOUNDER & CEO,
CHIEF MARKETING OFFICER,
CHIEF OF STAFF,
BOARD MEMBER,
SENIOR FELLOW,
SENIOR DIRECTOR,
MARKETING DIRECTOR,
SENIOR MANAGING DIRECTOR,
DIRECTOR OF MARKETING,
DIRECTOR OF DEVELOPMENT,
MONEY MANAGER,
MARKETING EXECUTIVE,
PRESIDENT, 
SHAREHOLDER,
DEVELOPMENT DIRECTOR,
DIRECTOR,
NONPROFIT EXECUTIVE,
GENERAL MANAGER,
CO-FOUNDER,
BUSINESSWOMAN,
HEALTHCARE EXECUTIVE,
OFFICE MANAGER,
ENTREPRENEUR,
VICE CHAIRMAN,
PRODUCT MANAGER, 
PARTNER, 
ADMINISTRATION,
MANAGEMENT CONSULTANT, 
OPERATIONS MANAGER,
MANAGING PARTNER,
INVESTMENTS,
CEO & FOUNDER,
FOUNDER AND CHIEF EXECUTIVE OFFICER,
EXECUTIVE COACH,
ADMINISTRATOR,
CHAIRMAN & CEO,
COO,
PRESIDENT AND CEO,
EXECUTIVE DIRECTOR,
VICE PRESIDENT,
EXECUTIVE VICE PRESIDENT, 
CHAIRMAN,
CONTROLLER,
SUPERVISOR,
SENIOR VICE PRESIDENT,
BUSINESS, 
PRESIDENT & CEO,
FINANCIAL ADVISOR,
FOUNDER,
EXECUTIVE ASSISTANT,
MANAGING DIRECTOR,
MANAGEMENT,
MANAGEMENT CONSULTANT,
DIRECTOR,
DIRECTOR OF OPERATIONS,
BUSINESS MANAGER,
BUSINESSMAN,
BUSINESS EXECUTIVE,
PROJECT MANAGER,
GENERAL MANAGER,
VICE PRESIDENT
ENTREPRENEUR,
MANAGER,
EXECUTIVE,
EXECUTIVE CHAIRMAN,
VP'), category := 'Business Executives']

combined[occ %in% C(
'REAL ESTATE DEVELOPER,
REAL ESTATE BROKER,
REAL ESTATE,
REAL ESTATE AGENT,
REAL ESTATE INVESTOR,
REALTOR,
REAL ESTATE MANAGEMENT,
REAL ESTATE SALES,
PROPERTY MANAGEMENT,
DEVELOPER,
REAL ESTATE DEVELOPMENT,
PROPERTY MANAGER
'), category := 'Real Estate']

combined[occ %in% C(
'ATTORNEY, 
LAWYER, 
PARALEGAL, 
LAW PROFESSOR,
LEGAL ASSISTANT,
GENERAL COUNSEL,
LEGAL ADMINISTRATOR/PARALEGAL'), category := 'Lawyers']

combined[occ %in% C(
'PHYSICIAN, 
PHYSICIAN, 
RN, 
DENTIST,
NURSE,
REGISTERED NURSE,
NURSE PRACTITIONER,
NURSE ANESTHETIST
ACUPUNCTURIST,
PSYCHOLOGIST, 
PSYCHIATRIST, 
PHARMACIST, 
PSYCHOTHERAPIST,
MEDICAL DOCTOR, 
CHIROPRACTIC PHYSICIAN,
SURGEON,
PEDIATRICIAN,
PHYSICIAN ASSISTANT,
CLINICAL SOCIAL WORKER,
CHIROPRACTOR,
CLINICAL PSYCHOLOGIST, 
PHYSICAL THERAPIST, 
DOCTOR, 
HEALTHCARE,
MD,
COUNSELOR,
VETERINARIAN,
MASSAGE THERAPIST,
HEALTH CARE,
PERSONAL CARE ATTENDANT,
CERTIFIED NURSE AIDE,
PUBLIC HEALTH NURSE,
ORTHODONTIST,
CNA,
HERBALIST,
GIS SPECIALIST,
FAMILY PHYSICIAN,
DOCTOR OF CHIRPORACTIC,
ANESTHESIOLOGIST'), category := 'Health Care']

combined[occ %in% C(
'PROFESSOR, 
COLLEGE PROFESSOR, 
GRADUATE STUDENT,
RESEARCHER, 
SCIENTIST, 
ASSOCIATE PROFESSOR,
PHYSICIST, 
CHEMIST,
RESEARCH SCIENTIST, 
LECTURER,
ASSISTANT PROFESSOR, 
ADJUNCT PROFESSOR,
FACULTY, 
ECONOMIST,
ANTHROPOLOGIST,
PROFESSOR OF FINANCE,
MATHEMATICIAN,
GEOPHYSICIST,
GRADUATE FELLOW'), category := 'Academics']

combined[occ %in% C(
'ENGINEER, 
SOFTWARE ENGINEER, 
SOFTWARE DEVELOPER, 
IT CONSULTANT, 
TECHNICIAN, 
TELEPHONE TECHNICIAN,
IT, 
COMPUTER PROGRAMMER,
WEB DEVELOPER, 
PROGRAMMER, 
SOFTWARE ARCHITECT,
MECHANICAL ENGINEER, 
IT MANAGER, 
ELECTRICAL ENGINEER,
DATA ANALYST, 
SOFTWARE,
SYSTEMS ENGINEER, 
INFORMATION TECHNOLOGY,
CIVIL ENGINEER,
MARINE ENGINEER,
PROJECT ENGINEER,
PRINCIPAL SOFTWARE ENGINEER,
SOFTWARE TECH,
SITE RELIABILITY ENGINEER'), category := 'Engineers']

combined[occ %in% C(
'
SELF EMOLOYED,
SELF-EMPLOYED,
SELF EMPLOYED,
SMALL BUSINESS OWNER, 
CONTRACTOR,
GENERAL CONTRACTOR,
ELECTRICIAN,
PLUMBER,
SELF, 
BUSINESS OWNER, 
OWNER,
BUILDER, 
PAINTER, 
CHEF, 
CONSTRUCTION,
CARPENTER, 
FARMER,
FARMER//FISHERMAN,
ELECTRICIAN,
PILOT,
TRUCK DRIVER'), category := 'Self Employed']

combined[occ %in% C(
'ARTIST, 
WRITER, 
FILMMAKER, 
PRODUCER,
GRAPHIC DESIGNER, 
ACTOR, 
PHOTOGRAPHER,
EDITOR, 
ARCHITECT, 
MUSICIAN,
DESIGNER, 
CREATIVE DIRECTOR, 
ACTRESS,
TECHNICAL WRITER, 
INTERIOR DESIGNER,
AUTHOR,
PHOTOGRAPHER,
INTERIOR DESIGNER,
EDITOR,
WEB DEVELOPER,
FINE ARTIST,
DRAFTER,
ART CONSULTANT,
TALENT AGENT,
ART HISTORIAN,
VIDEO EDITOR,
SCREENWRITER,
JOURNALIST,
INTERIOR DESIGN,
PUBLISHER'), category:='Artists']

occup <- 
  combined %>% 
  subset(category=='other') %>% 
  group_by(candidate) %>% 
  mutate(totalFunds=sum(amount)) %>% 
  group_by(candidate, occ) %>%
  dplyr::summarize(
    count=n(),
    total=sum(amount),
    share=sum(amount)/mean(totalFunds)
  ) %>% arrange(candidate, -share) %>% to.data.table

occup[candidate=='Clinton',][1:100,]
occup[candidate=='Sanders',][1:100,]

occup[,cumamount := cumsum(total)  , by=candidate]
occup[,cumshare  := cumsum(share)  , by=candidate]
occup[,i         := seq(.N)        , by=candidate]

occup %>% ggplot(aes(i, cumshare, color=candidate)) +
  geom_line(size=2) +theme_bw()


combined %>% 
  subset(category=='other') %>% 
  arrange(-amount)  %>% `[`(1:100,.(category,occ))

catsums <- 
  combined %>% group_by(candidate) %>% 
  mutate(totalFunds=sum(amount)) %>% 
  group_by(candidate,category) %>% 
  dplyr::summarise(perc = round(sum(amount)/mean(totalFunds)*100, 1)) %>% 
  group_by() %>% arrange(category,candidate) %>%
  to.data.table

catsums[1:nrow(catsums),]

combined$category2 <- 
  factor(combined$category, 
     levels = C(
      'Business Executives, Lawyers, Finance, Real Estate, 
      Public Relations, Philanthropist, Retired, 
      Teachers/Students,  Academics,  Self Employed, Artists,
      Health Care,
      Engineers'))


combined %>% subset(!(category %in% C('Unemployed,other,Missing,Retired'))) %>% 
  ggplot(aes(category2, fill=candidate)) + 
  geom_bar(position="dodge") + coord_flip()  + 
  geom_bar(position="dodge", width=.5) + coord_flip()  + 
  theme_bw(base_size = 15) +
  scale_x_discrete(
    labels = scales::comma) + 
  theme(legend.position="bottom") +
  ylab('') + xlab('') 

setwd('C:/Users/fsmar/Dropbox/Econometrics by Simulation/2016-02-February')

gold <- (1+5^.5)/2

png('2016-19-BigBizClinton.png', width=1200, height=1200/gold)
combined %>% subset(!(category %in% C('Unemployed,other,Missing,Retired'))) %>% 
  group_by(candidate) %>% 
  mutate(share=amount/sum(amount)) %>% 
  ggplot(aes(category2, fill=candidate, weight=share)) + 
  geom_bar(position="dodge", width=.85) + coord_flip()  + 
  theme_bw(base_size = 20) +
  scale_x_discrete(
    labels = scales::comma) + 
  theme(legend.position="bottom") +
  ylab('Proportion of Funds Provided to Candidate by Industry') + xlab('') +
  theme(legend.title=element_blank())
dev.off()

subset(combined, !(category %in% C('Unemployed,other,Missing,Retired')))[,occ] %>% unique
# 263

combined$month <- combined$date %>% format("%m") %>% as.numeric

combined %>% subset(!(category %in% C('Unemployed,other,Missing,Retired'))) %>% 
  group_by(candidate, category) %>% 
  dplyr::summarise(count = n(),
                   total = sum(amount)) %>% 
  arrange(candidate, -total) %>%
  to.data.table

combined %>% 
  group_by(candidate) %>% 
  dplyr::summarise(other_miss = mean(category %in% C('other,Missing')),
                   unemployed = mean(category %in% C('Unemployed,Retired')))
                   
combined %>% subset((
  category %in% C('Engineers,Health Care, Artists, Self-Employed, Business Executives'))) %>% 
  group_by(candidate) %>% 
  dplyr::summarise(count = n(),
                   total = sum(amount))
