
library(tidyverse)
library(reshape2)



# ==================

# Analysis script for the longitudinal study of updating of Cochrane reviews, 2003 to 2018
# Data files and data guide available at https://github.com/hildabast/cochrane-updating

# ==================

# Calculate update cohort by year first published

cohort <- read.csv("data/cohort_2018.csv")

cohort %>%
  group_by(first_published)%>%
  summarise(n = n())

# ==================

# Analysis for table 1, CRGs and reviews 1995 to 2002

rise <- read.csv("data/summary_1995_2002.csv")

rise %>%
  mutate(all_reviews = cumsum(reviews))%>%
  mutate(proportion = cohort_published / all_reviews)

# ==================

# Analysis for table 2 and fig 1, comparative proportions of reviews per CRG

cohort <- read.csv("data/cohort_2018.csv")

cohort2 <- cohort %>%
  group_by(crg)%>%
  summarise(n2=n())%>%
  mutate(proportion2 = n2/sum(n2))

head(cohort2)

recent <- read.csv("data/crgs_recent_revs.csv")

recent2 <- recent %>%
  mutate(proportion = reviews / sum(reviews))

head(recent2)

revsXcrg <- left_join(recent2, cohort2, by = "crg")

head(revsXcrg)

revsXcrg2 <- revsXcrg %>%
  mutate(n2 = replace_na(n2, 0))%>%
  mutate(proportion2 = replace_na(proportion2, 0))

head(revsXcrg2)

# Ranges, medians, IQRs

summary(cohort2)

summary(revsXcrg2)

# Graphing Figure 1

df <- select(revsXcrg2, c(crg, proportion, proportion2))

head(df)

df1 <- data.frame(c(df$crg), c(df$proportion), c(df$proportion2))

colnames(df1) <- c("CRG", "All recent", "Cohort")

df1 <- melt(df1, id="CRG")

jpeg('figs/fig1A.jpeg')

p <-ggplot()+
  geom_bar(data = df1, aes(x=CRG, y=value, fill=variable), position = ("dodge"), stat = "identity")+
  scale_fill_manual(values = c("blue", "orange"))+
  coord_flip()+
  scale_x_discrete(limits = rev(levels(df1$CRG)))+
  theme_bw()+annotate("text", label = "bw()",
                      col="black", size=4)+
  ylab("Percent")+
  labs(fill="Reviews")

p + guides(fill = guide_legend(reverse=TRUE))

dev.off()

# ==================

# Restricting analysis to reviews by the original CRGs

originals <- revsXcrg2 %>%
  filter(crg == "Pregnancy and Childbirth" | crg == "Neonatal" | crg == "Cystic Fibrosis and Genetic Disorders" 
         | crg == "Airways" | crg == "Gynaecology and Fertility" | crg == "Vascular" | crg == "Oral Health"
         | crg == "Effective Practice and Organisation of Care" | crg == "Stroke" |  crg == "Infectious Diseases"
         | crg == "Acute Respiratory Infections" | crg == "Dementia and Cognitive Improvement"
         | crg == "Wounds" | crg == "Musculoskeletal" | crg == "Inflammatory Bowel Disease"
         | crg == "Tobacco Addiction" | crg == "Metabolic and Endocrine Disorders")

head(originals)

originals2 <-  select (originals, c(crg, reviews, n2))

head(originals2)

# Total number of recent reviews for the original CRGs

originals2 <-  select (originals, c(crg, reviews, n2))%>%
  summarize(sum(reviews))

head(originals2)

# Total number of cohort updates for original CRGs

originals3 <- select (originals, c(crg, reviews, n2))%>%
  summarize(sum(n2))

head(originals3)

# ==================

# Analyses in table 3

# All reviews at the end of 2002

allrevs <- read.csv("data/reviews_2002.csv")

cohort <- read.csv("data/cohort_2018.csv")

# Subset of the cohort that was published by the end of 2002

cohortsubset <- cohort %>%
  filter(first_published != "2003")

# Combine data from the cohort subset with all reviews

status2011 <- left_join(allrevs, cohortsubset, by = "cd_id")

# Ongoing status: cohort subset

status2011 %>%
  filter(ongoing_2011 == "Y") %>%
  summarise(n=n())%>%
  mutate(proportion = round(100 * n/sum(173), 1))

# Ongoing status: not updated in 2003

status2011 %>%
  filter(status == "A") %>%
  filter(is.na(pmid_year)) %>%
  summarise(n=n()) %>%
  mutate(proportion = round(100 * n/sum(1359), 1))

# Ongoing status: all 2002 reviews

status2011 %>%
  filter(status == "A") %>%
  summarise(n=n())%>%
  mutate(proportion = round(100 * n/sum(1532), 1))

# Stable status: cohort subset

status2011 %>%
  filter(stable == "Y") %>%
  filter(ongoing_2011 != "Y") %>%
  summarise(n=n()) %>%
  mutate(proportion = round(100 * n/sum(173), 1))

# Stable status: not updated in 2003

status2011 %>%
  filter(status == "S") %>%
  filter(is.na(pmid_year)) %>%
  summarise(n=n()) %>%
  mutate(proportion = round(100 * n/sum(1359), 1))

# Stable status: all 2002 reviews

status2011 %>%
  filter(status == "S") %>%
  summarise(n=n())%>%
  mutate(proportion = round(100 * n/sum(1532), 1))

# Retracted status: cohort subset

status2011 %>%
  filter(retracted == "Y") %>%
  filter(ongoing_2011 != "Y") %>%
  summarise(n=n()) %>%
  mutate(proportion = round(100 * n/sum(173), 1))

# Retracted status: not updated in 2003

status2011 %>%
  filter(status == "W"| status == "X") %>%
  filter(is.na(pmid_year)) %>%
  summarise(n=n()) %>%
  mutate(proportion = round(100 * n/sum(1359), 1))

# Retracted status: all 2002 reviews

status2011 %>%
  filter(status == "W"| status == "X") %>%
  summarise(n=n())%>%
  mutate(proportion = round(100 * n/sum(1532), 1))

# Ongoing status, no update since 2002: no 2003 update

status2011 %>%
  filter(updated == "N") %>%
  filter(status == "A") %>%
  filter(is.na(pmid_year)) %>%
  summarise(n=n()) %>%
  mutate(proportion = round(100 * n/sum(1274), 1))

# Ongoing status, no update since 2002: all 2002 reviews

status2011 %>%
  filter(updated == "N") %>%
  filter(status == "A") %>%
  filter(is.na(pmid_year)) %>%
  summarise(n=n())%>%
  mutate(proportion = round(100 * n/sum(1424), 1))

# Ongoing status, updated in 2003 only: cohort subset

status2011 %>%
  filter(update_2004_plus == "N") %>%
  filter(ongoing_2011 == "Y") %>%
  summarise(n=n()) %>%
  mutate(proportion = round(100 * n/sum(150), 1))

# Ongoing status, updated in 2003 only: all 2002 reviews

status2011 %>%
  filter(update_2004_plus == "N") %>%
  filter(ongoing_2011 == "Y") %>%
  summarise(n=n()) %>%
  mutate(proportion = round(100 * n/sum(1424), 1))

# Ongoing status, updated between 2004 and 2011: cohort subset

ongoingyes <- status2011 %>%
  filter(update_2004_plus == "Y") %>%
  filter(ongoing_2011 == "Y") %>%
  summarise(n=n()) %>%
  mutate(proportion = round(100 * n/sum(150), 1))

# Ongoing status, updated between 2004 and 2011: not updated in 2003

ongoingno <- status2011 %>%
  filter(status == "A") %>%
  filter(updated == "Y") %>%
  filter(is.na(pmid_year)) %>%
  summarise(n=n()) %>%
  mutate(proportion = round(100 * n/sum(1274), 1))

# Ongoing status, updated between 2004 and 2011: all 2002 reviews

n <- ongoingno$n + ongoingyes$n

n/1424

# Number of empty reviews in 2011

cohort <- read.csv("data/cohort_2018.csv")

cohort %>%
  filter(included_2011 == "0") %>%
  summarise(n=n()) %>%
  mutate(proportion = round(100 * n/sum(177), 1))

# ==================

# Time since publication and number of updates per review

cohort <- read.csv("data/cohort_2018.csv")

timesince <-cohort %>%
  mutate(time_since = 2018 - first_published)

# Medians and IQRs for time since publication and number of updates

summary(timesince$time_since)

summary(cohort$updates_to_2018)

# Graphing time since publication (Fig 2)

jpeg('figs/fig2.jpeg')

ggplot(timesince, aes(time_since))+
  geom_bar(fill = "#0073C2FF", width = 0.7)+
  xlab("Years since publication")+
  ylab("Number of reviews")+
  xlim(14, 24)+
  ylim(0, 40)+
  theme_bw(base_size = 20)

dev.off()


# Graphing number of updates per review (Fig 3)

jpeg('figs/fig3.jpeg')

ggplot(cohort, aes(updates_to_2018))+
  geom_bar(fill = "#0073C2FF", width = 0.7)+
  xlab("Updates")+
  ylab("Number per review")+
  theme_bw(base_size = 20)

dev.off()

# ==================

# Time since publication and time to update

timesince <-cohort %>%
  mutate(time_since = 2018 - first_published)

ttupdate <- timesince %>%
  filter(ongoing_2018 == "Y") %>%
  mutate(tt1 = update_1 - first_published) %>%
  mutate(tt2 = update_2 - update_1) %>%
  mutate(tt3 = update_3 - update_2) %>%
  mutate(tt4 = update_4 - update_3) %>%
  mutate(tt5 = update_5 - update_4) %>%
  mutate(tt6 = update_6 - update_5) %>%
  mutate(tt7 = update_7 - update_6) %>%
  mutate(tt8 = update_7 - update_6) %>%
  mutate(tt9 = update_8 - update_7) %>%
  mutate(tt10 = update_9 - update_8) %>%
  mutate(tt11 = update_10 - update_9)
 
ttup <- bind_cols(list(ttupdate$cd_id, ttupdate$first_published, ttupdate$time_since,  
                       ttupdate$updates_to_2018, ttupdate$tt1, ttupdate$tt2, 
                       ttupdate$tt3, ttupdate$tt4, ttupdate$tt5, 
                       ttupdate$tt6, ttupdate$tt7, ttupdate$tt8, 
                       ttupdate$tt9, ttupdate$tt10, ttupdate$tt11))
  
colnames(ttup) <- c("cd_id", "first_published", "time_since", "updates_to_2018", "1st", "2nd", "3rd", "4th", 
                    "5th", "6th", "7th", "8th", "9th", "10th", "11th")

# Median and range, time each update (including Table 4)

summary(ttup)

# Number and proportion of ongoing reviews for each update (Table 4)

perupdate <- data.frame(c(150 - sum(is.na(ttup$`1st`)), 150 - sum(is.na(ttup$`2nd`)), 
                          150 - sum(is.na(ttup$`3rd`)), 150 - sum(is.na(ttup$`4th`)), 
                          150 - sum(is.na(ttup$`5th`)), 150 - sum(is.na(ttup$`6th`)), 
                          150 - sum(is.na(ttup$`7th`)), 150 - sum(is.na(ttup$`8th`)), 
                          150 - sum(is.na(ttup$`9th`)), 150 - sum(is.na(ttup$`10th`)),
                          150 - sum(is.na(ttup$`11th`))))

colnames(perupdate) <- c("n")

perupdate %>%
  mutate(proportion = round(100*n/sum(150), 1))

# ==================

# Analyses of review currency (included in Table 5)

cohort <- read.csv("data/cohort_2018.csv")

current <- cohort %>%
  filter(ongoing_2018 == "Y") %>%
  mutate(years_since_update = 2018 - last_update)

yrsSince <- current$years_since_update

# Median, IQR years since last update
              
summary(yrsSince)

dfUTD <- current %>%
  group_by(years_since_update) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 1)) %>%
  mutate(cum.n = cumsum(n)) %>%
  mutate(rel.freq2 = round(100 * cum.n/sum(150), 1))

# Graphing Figure 4

jpeg('figs/fig4.jpeg')

ggplot(current, aes(years_since_update))+
  geom_bar(fill = "#0073C2FF", width = 0.7)+
  xlab("Years since update")+
  ylab("Number of reviews")+
  ylim(0, 20)+
  theme_bw(base_size = 20)

dev.off()


# ==================

# Additional analyses of review currency, Table 5

# Proportion of publication life up-to-date

current1 <- current %>%
  mutate(time_since2 = 2018 - first_published)

current2 <- current1 %>%
  mutate(publife = years_utd_2018/time_since2)

summary(current2$publife)

# Years since date of last search (April 2011)

reporting <- read.csv("data/reporting_quality_2011.csv")

summary(reporting)

combReport <- left_join(reporting, cohort, by = "cd_id")

combReport1 <- combReport %>%
  filter(last_update < 2012) %>%
  mutate(years_since_search_april_2011 + 7)

summary(combReport1)


# ==================

# Major changes in conclusion in 2003 update

reporting <- read.csv("data/reporting_quality_2011.csv")

reporting %>%
  filter(major_2003 == "Y") %>%
  summarise(n = n())

# Growth of included studies

cohort <- read.csv("data/cohort_2018.csv")

included <- bind_cols(list(cohort$cd_id, cohort$included_before_2003_update, 
                           cohort$included_2003_update, cohort$included_2011, 
                           cohort$included_2018))
colnames(included) <- c("ID", "Before 2003", "2003 update", "2011", "2018")

# Median, range, IQR

summary(included)

# Graphing Figure 5

included1 <- melt(included, id="ID")

colnames(included1) <- c("ID", "Time", "Studies")

jpeg('figs/fig5.jpeg')
p <- ggplot(included1, aes(Time, Studies))
p + geom_boxplot(outlier.colour = NA, fill="orange")+ coord_cartesian(ylim = c(0, 70)) +
  theme_bw(base_size = 20)
dev.off()
        
# ==================


# Reporting - Table 6

reporting <- read.csv("data/reporting_quality_2011.csv")

reporting %>%
  filter(searches_reported_2011 == "Y") %>%
  summarise(n = n()) %>%
  mutate(proportion = round(100 * n/sum(177), 1))

reporting %>%
  filter(trial_changes_reported_2011 == "Y") %>%
  summarise(n = n()) %>%
  mutate(proportion = round(100 * n/sum(177), 1))

reporting %>%
  filter(!is.na(years_since_search_april_2011)) %>%
  summarise(n = n()) %>%
  mutate(proportion = round(100 * n/sum(177), 1))

reporting %>%
  filter(!is.na(years_since_search_april_2011)) %>%
  filter(searches_reported_2011 == "Y") %>%
  filter(trial_changes_reported_2011 == "Y") %>%
  summarise(n = n()) %>%
  mutate(proportion = round(100 * n/sum(177), 1))

## END
