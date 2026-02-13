library(ipumsr)
library(dplyr)
library(haven)
library(janitor)
library(magrittr)
library(forcats)
library(DescTools)
library(ggplot2)
library(tidyverse)
library(truncnorm)
library(Rmisc)

setwd("C:\\Users\\nscau\\OneDrive - Providence College\\Year 4\\Semester 2\\Data Science Capstone\\research")

ddi <- read_ipums_ddi("atus_00005.xml")
research <- read_ipums_micro(ddi)

research_filtered <- research %>% 
  mutate(across(everything(), ~ if_else(.x == 99 | .x == 999 | .x == 9999 | .x == 99999 | .x == 99999.99, NA, .x)),
                educ = case_when(EDUC == 998 | EDUC == 999 ~ NA,
                          EDUC > 000 & EDUC < 018 ~ "< hs",
                          EDUC == 020 | EDUC == 021 ~ "hs",
                          EDUC > 029 & EDUC < 033 ~ "sc",
                          EDUC >= 040 ~ "ba+"),
         educ = fct_relevel(educ, levels = c("< hs", "hs", "sc", "ba+")),
         marital = case_when(MARST == 1 | MARST == 2 ~ "Married",
                             MARST == 3 | MARST == 4 | MARST == 5 ~ "Not Married",
                             MARST == 6 ~ "Never Married",
                             MARST == 99 ~ NA),
         empstat = case_when(EMPSTAT == 1 | EMPSTAT == 2 ~ "Employed",
                             EMPSTAT == 3 | EMPSTAT == 4 ~ "Unemployed",
                             EMPSTAT == 5 ~ "Not in Labor Force",
                             EMPSTAT == 99 ~ NA),
         fulltime = if_else(FULLPART == 1, 1, 0, NA),
         male = if_else(SEX == 1, 1, 0, NA),
         region = case_when(REGION == 1 ~ "Northeast",
                            REGION == 2 ~ "Midwest",
                            REGION == 3 ~ "South",
                            REGION == 4 ~ "West"),
         metro = if_else(METRO == 1, 1, 0, NA),
         childcare = BLS_CAREHH_KID,
         education = BLS_EDUC,
         leisure = BLS_LEIS,
         selfcare = BLS_PCARE,
         work = BLS_WORK)


mean(research_filtered$childcare)
mean(research_filtered$education)
mean(research_filtered$leisure)
mean(research_filtered$selfcare)
mean(research_filtered$work)

tabyl(research_filtered, educ)
tabyl(research_filtered, marital)
tabyl(research_filtered, empstat)
tabyl(research_filtered, fulltime)
tabyl(research_filtered, male)
tabyl(research_filtered, region)
tabyl(research, filtered, metro)

ggplot(research_filtered, aes(educ)) +
  geom_bar(color = "#342c5c", fill = "#342c5c")

ggplot(research_filtered, aes(marital)) +
  geom_bar(color = "#342c5c", fill = "#342c5c")

ggplot(research_filtered, aes(empstat)) +
  geom_bar(color = "#342c5c", fill = "#342c5c")

ggplot(research_filtered, aes(fulltime)) +
  geom_bar(color = "#342c5c", fill = "#342c5c")

ggplot(research_filtered, aes(male)) +
  geom_bar(color = "#342c5c", fill = "#342c5c")

ggplot(research_filtered, aes(region)) +
  geom_bar(color = "#342c5c", fill = "#342c5c")

ggplot(research_filtered, aes(metro)) +
  geom_bar(color = "#342c5c", fill = "#342c5c")

ggplot(research_filtered, aes(AGE)) +
  geom_histogram(bins = 10, color = "#342c5c", fill = "#342c5c")


tabyl(research_filtered, educ)
tabyl(research_filtered, marital)
tabyl(research_filtered, male)
tabyl(research_filtered, region)
tabyl(research_filtered, empstat)
tabyl(research_filtered, fulltime)

# when respondent is employed full time
summary <- bind_rows(
  research_filtered %>% 
    filter(fulltime == 1) %>%
    summarise(mean_child = mean(childcare, na.rm = TRUE),
              sd_child   = sd(childcare, na.rm = TRUE),
              mean_leis  = mean(leisure, na.rm = TRUE),
              sd_leis    = sd(leisure, na.rm = TRUE),
              mean_self  = mean(selfcare, na.rm = TRUE),
              sd_self   = sd(selfcare, na.rm = TRUE)
              ) %>% 
    mutate(group = "Full-time"),

  research_filtered %>% 
    filter(fulltime == 1 & educ == "ba+") %>%
    summarise(mean_child = mean(childcare, na.rm = TRUE),
              sd_child   = sd(childcare, na.rm = TRUE),
              mean_leis  = mean(leisure, na.rm = TRUE),
              sd_leis    = sd(leisure, na.rm = TRUE),
              mean_self  = mean(selfcare, na.rm = TRUE),
              sd_self   = sd(selfcare, na.rm = TRUE)) %>% 
    mutate(group = "Full-time BA+"),

  research_filtered %>% 
    filter(fulltime == 0) %>%
    summarise(mean_child = mean(childcare, na.rm = TRUE),
              sd_child   = sd(childcare, na.rm = TRUE),
              mean_leis  = mean(leisure, na.rm = TRUE),
              sd_leis    = sd(leisure, na.rm = TRUE),
              mean_self  = mean(selfcare, na.rm = TRUE),
              sd_self   = sd(selfcare, na.rm = TRUE)) %>% 
    mutate(group = "Part-time"),
  
  research_filtered %>% 
    filter(fulltime == 0 & educ == "hs") %>%
    summarise(mean_child = mean(childcare, na.rm = TRUE),
              sd_child   = sd(childcare, na.rm = TRUE),
              mean_leis  = mean(leisure, na.rm = TRUE),
              sd_leis    = sd(leisure, na.rm = TRUE),
              mean_self  = mean(selfcare, na.rm = TRUE),
              sd_self   = sd(selfcare, na.rm = TRUE)) %>% 
    mutate(group = "Part-time HS"))

ggplot(summary, aes(x = group, y = mean_child)) +
  geom_col(fill= "steelblue") +
  geom_errorbar(
    aes(
      ymin = mean_child - sd_child,
      ymax = mean_child + sd_child
    ),
    width = 0.2
  ) + 
  theme_minimal() +
  labs(
    title = "Average Time Use of Childcare by Employment and Education Affected",
    x = "Employment and Education Status",
    y = "Mean Time"
  )

ggplot(summary, aes(x = group, y = mean_leis)) +
  geom_col(fill= "steelblue") +
  geom_errorbar(
    aes(
      ymin = mean_leis - sd_leis,
      ymax = mean_leis + sd_leis
    ),
    width = 0.2
  ) +
  theme_minimal() +
  labs(
    title = "Average Time Use of Leisure by Employment and Education",
    x = "Employment and Education Status",
    y = "Mean Time"
  )

ggplot(summary, aes(x = group, y = mean_self)) +
  geom_col(fill= "steelblue") +
  geom_errorbar(
    aes(
      ymin = mean_self - sd_self,
      ymax = mean_self + sd_self
    ),
    width = 0.2
  ) +
  theme_minimal() +
  labs(
    title = "Average Time Use of Selfcare by Employment and Education",
    x = "Employment and Education Status",
    y = "Mean Time"
  )


# How do education level and full-time employment relate to time spent on selfcare?
model1 <- lm(selfcare ~ educ + fulltime, data = research_filtered)
summary(model1)
# Data summary
#   BA+ spend 49 minutes fewer on selfcare than less than hs students
#   Some college spend 38 minutes fewer on selfcare than less than hs students
#   High school spend 30 minutes fewer on selfcare than less than hs students
# This shows overall that hs students spent the most amount of time on selfcare compared to ba+
#   Full time spend 34 minutes fewer on selfcare than part time employees

# How does education level and full-time employment related to time spent on sleep?
model2 <- lm(BLS_PCARE_SLEEP ~ educ + fulltime, data = research_filtered)
summary(model2)
# Data summary
#   The results are very similar to the variable selfcare overall. Some of the variables are slightly less than or bigger than. 


# How does having more children affect your sleep?
model3 <- lm(BLS_PCARE_SLEEP ~ HH_NUMKIDS, data = research_filtered %>% filter(HH_NUMKIDS == 0 | HH_NUMKIDS >= 3))
summary(model3)
# Data summary
#   Having more children decreases your sleep by about 2 and a half minutes
