# ================= LOAD LIBRARIES =================
# These packages provide tools for reading IPUMS data,
# data cleaning, manipulation, and visualization.

library(ipumsr)    
library(dplyr)     
library(janitor)    
library(forcats)    
library(ggplot2)    
library(tidyverse)  
# ================= SET WORKING DIRECTORY =================
# This tells R where the dataset files are stored.

setwd("C:\\Users\\nscau\\OneDrive - Providence College\\Year 4\\Semester 2\\Data Science Capstone\\research")

# ================= LOAD DATA =================
# Read the IPUMS data dictionary and microdata

ddi <- read_ipums_ddi("atus_00006.xml")
research <- read_ipums_micro(ddi)

# ================= DATA CLEANING =================
# This section prepares the dataset for analysis by:
# 1. Removing coded missing values
# 2. Creating readable variables
# 3. Recoding variables for easier interpretation

research_filtered <- research %>% 
  mutate(
    
    # Convert IPUMS missing value codes (999, 9999, etc.) into NA
    across(where(is.numeric), ~ na_if(.x, 999) %>% na_if(9999) %>% na_if(99999)),
    
    # Recode education into four meaningful categories
    educ = case_when(
      EDUC == 998 | EDUC == 999 ~ NA,
      EDUC > 0 & EDUC < 18 ~ "< hs",
      EDUC == 20 | EDUC == 21 ~ "hs",
      EDUC > 29 & EDUC < 33 ~ "sc",
      EDUC >= 40 ~ "ba+"
    ),
    
    # Set the order of education categories
    educ = fct_relevel(educ, c("< hs", "hs", "sc", "ba+")),
    
    # Recode marital status into broader groups
    marital = case_when(
      MARST == 1 | MARST == 2 ~ "Married",
      MARST == 3 | MARST == 4 | MARST == 5 ~ "Not Married",
      MARST == 6 ~ "Never Married",
      MARST == 99 ~ NA
    ),
    
    # Employment status categories
    empstat = case_when(
      EMPSTAT == 1 | EMPSTAT == 2 ~ "Employed",
      EMPSTAT == 3 | EMPSTAT == 4 ~ "Unemployed",
      EMPSTAT == 5 ~ "Not in Labor Force",
      EMPSTAT == 99 ~ NA
    ),
    
    # Create binary indicator for full-time work
    fulltime = if_else(FULLPART == 1, 1, 0, NA),
    
    # Gender indicator (1 = male, 0 = female)
    male = if_else(SEX == 1, 1, 0, NA),
    
    # Region of residence
    region = case_when(
      REGION == 1 ~ "Northeast",
      REGION == 2 ~ "Midwest",
      REGION == 3 ~ "South",
      REGION == 4 ~ "West"
    ),
    
    # Metro area indicator
    metro = if_else(METRO == 1, 1, 0, NA),
    
    # Indicator for having children in the household
    parents = if_else(HH_NUMOWNKIDS == 1, 1, 0, NA),
    
    # Indicator for working multiple jobs
    multi_jobs = if_else(MULTJOBS == 1, 1, 0, NA),
    
    # Extract time-use variables from ATUS dataset
    childcare = BLS_CAREHH_KID,
    education = BLS_EDUC,
    leisure = BLS_LEIS,
    selfcare = BLS_PCARE,
    work = BLS_WORK
  )


# ================= Z-SCORE STANDARDIZATION =================
# Standardizing variables allows clustering to treat all variables
# equally by removing differences in scale.

research_filtered <- research_filtered %>%
  mutate(
    z_selfcare  = as.numeric(scale(selfcare)),
    z_work      = as.numeric(scale(work)),
    z_leisure   = as.numeric(scale(leisure)),
    z_childcare = as.numeric(scale(childcare))
  )

# Verify standardization worked (mean ~0, sd ~1)

research_filtered %>%
  summarise(
    mean_selfcare = mean(z_selfcare, na.rm = TRUE),
    sd_selfcare   = sd(z_selfcare, na.rm = TRUE),
    mean_work     = mean(z_work, na.rm = TRUE),
    sd_work       = sd(z_work, na.rm = TRUE),
    mean_leisure  = mean(z_leisure, na.rm = TRUE),
    sd_leisure    = sd(z_leisure, na.rm = TRUE),
    mean_childcare= mean(z_childcare, na.rm = TRUE),
    sd_childcare  = sd(z_childcare, na.rm = TRUE)
  )


# ================= DESCRIPTIVE STATISTICS =================
# Calculate mean time spent in major activity categories

mean(research_filtered$childcare, na.rm = TRUE)
mean(research_filtered$education, na.rm = TRUE)
mean(research_filtered$leisure, na.rm = TRUE)
mean(research_filtered$selfcare, na.rm = TRUE)
mean(research_filtered$work, na.rm = TRUE)

# Frequency tables for demographic variables

tabyl(research_filtered, educ)
tabyl(research_filtered, marital)
tabyl(research_filtered, empstat)
tabyl(research_filtered, fulltime)
tabyl(research_filtered, male)
tabyl(research_filtered, region)
tabyl(research_filtered, metro)
tabyl(research_filtered, parents)
tabyl(research_filtered, multi_jobs)


# ================= BASIC VISUALIZATIONS =================
# These graphs show the distribution of key demographic variables.

ggplot(research_filtered, aes(educ)) + 
  geom_bar(fill="#342c5c") +
  labs(title = "Education Distribution", x = "Education", y = "Count")

ggplot(research_filtered, aes(parents)) + 
  geom_bar(fill="#342c5c") + 
  labs(title = "Parent Status", x = "Parent (0 = No, 1 = Yes)", y = "Count")

ggplot(research_filtered, aes(marital)) +
  geom_bar(fill="#342c5c") +
  labs(title = "Marital Distribution", x = "Marital", y = "Count")

ggplot(research_filtered, aes(empstat)) +
  geom_bar(fill="#342c5c") +
  labs(title = "Employee Status Distribution", x = "Employee Status", y = "Count")

ggplot(research_filtered, aes(fulltime)) +
  geom_bar(fill="#342c5c")+ 
  labs(title = "Full Time Distribution", x = "Full Time (0 = No, 1 = Yes)", y = "Count")

ggplot(research_filtered, aes(male)) +
  geom_bar(fill="#342c5c") +
  labs(title = "Sex Distribution", x = "Sex (0 = Female, 1 = Male)", y = "Count")

ggplot(research_filtered, aes(AGE)) +
  geom_histogram(bins = 10, fill="#342c5c") +
  labs(title = "Age Distribution", x = "Age", y = "Count")


# ================= MULTIPLE LINEAR REGRESSION =================
# Model estimating factors associated with time spent in self-care

model_final <- lm(selfcare ~ fulltime + parents + educ + male + marital, data = research_filtered)
summary(model_final)

# Model estimating predictors of sleep time

model_sleep_final <- lm(BLS_PCARE_SLEEP ~ fulltime + parents + educ + male + marital, data = research_filtered)
summary(model_sleep_final)


# ================= GROUP COMPARISON SUMMARY TABLE =================
# Compare activity patterns across employment and education groups

summary <- bind_rows(
  
  # Full-time workers
  research_filtered %>% 
    filter(fulltime == 1) %>%
    summarise(
      mean_child = mean(childcare, na.rm = TRUE),
      sd_child   = sd(childcare, na.rm = TRUE),
      mean_leis  = mean(leisure, na.rm = TRUE),
      sd_leis    = sd(leisure, na.rm = TRUE),
      mean_self  = mean(selfcare, na.rm = TRUE),
      sd_self    = sd(selfcare, na.rm = TRUE)
    ) %>% 
    mutate(group = "Full-time"),
  
  # Full-time workers with bachelor's degree
  research_filtered %>% 
    filter(fulltime == 1 & educ == "ba+") %>%
    summarise(
      mean_child = mean(childcare, na.rm = TRUE),
      sd_child   = sd(childcare, na.rm = TRUE),
      mean_leis  = mean(leisure, na.rm = TRUE),
      sd_leis    = sd(leisure, na.rm = TRUE),
      mean_self  = mean(selfcare, na.rm = TRUE),
      sd_self    = sd(selfcare, na.rm = TRUE)
    ) %>% 
    mutate(group = "Full-time BA+"),
  
  # Part-time workers
  research_filtered %>% 
    filter(fulltime == 0) %>%
    summarise(
      mean_child = mean(childcare, na.rm = TRUE),
      sd_child   = sd(childcare, na.rm = TRUE),
      mean_leis  = mean(leisure, na.rm = TRUE),
      sd_leis    = sd(leisure, na.rm = TRUE),
      mean_self  = mean(selfcare, na.rm = TRUE),
      sd_self    = sd(selfcare, na.rm = TRUE)
    ) %>% 
    mutate(group = "Part-time"),
  
  # Part-time workers with high school education
  research_filtered %>% 
    filter(fulltime == 0 & educ == "hs") %>%
    summarise(
      mean_child = mean(childcare, na.rm = TRUE),
      sd_child   = sd(childcare, na.rm = TRUE),
      mean_leis  = mean(leisure, na.rm = TRUE),
      sd_leis    = sd(leisure, na.rm = TRUE),
      mean_self  = mean(selfcare, na.rm = TRUE),
      sd_self    = sd(selfcare, na.rm = TRUE)
    ) %>% 
    mutate(group = "Part-time HS")
)


# ================= ERROR BAR VISUALIZATIONS =================
# Bar charts with standard deviation error bars

ggplot(summary, aes(group, mean_child)) +
  geom_col(fill="steelblue") +
  geom_errorbar(aes(ymin = mean_child - sd_child, ymax = mean_child + sd_child), width=.2) +
  theme_minimal()

ggplot(summary, aes(group, mean_leis)) +
  geom_col(fill="steelblue") +
  geom_errorbar(aes(ymin = mean_leis - sd_leis, ymax = mean_leis + sd_leis), width=.2) +
  theme_minimal()

ggplot(summary, aes(group, mean_self)) +
  geom_col(fill="steelblue") +
  geom_errorbar(aes(ymin = mean_self - sd_self, ymax = mean_self + sd_self), width=.2) +
  theme_minimal()


# ================= LOGISTIC REGRESSION =================
# Create binary indicator for low self-care

median_selfcare <- median(research_filtered$selfcare, na.rm = TRUE)

research_filtered$deprived <- if_else(
  research_filtered$selfcare < median_selfcare, 1, 0
)

# Predict likelihood of low self-care

model <- glm(deprived ~ educ + fulltime, data = research_filtered, family = binomial)
summary(model)

modelchild <- glm(deprived ~ childcare + fulltime, data = research_filtered, family = binomial)
summary(modelchild)


# ================= CLUSTER ANALYSIS =================
# Identify groups with similar daily time-use patterns

set.seed(1)

analysis_sample <- research_filtered %>%
  select(
    z_selfcare, z_work, z_leisure, z_childcare,
    selfcare, work, leisure, childcare,
    educ, fulltime, parents
  ) %>%
  drop_na(z_selfcare, z_work, z_leisure, z_childcare) %>%
  sample_n(5000)

clusters <- kmeans(
  analysis_sample %>% select(z_selfcare, z_work, z_leisure, z_childcare),
  centers = 4,
  nstart = 10
)

analysis_sample$cluster <- as.factor(clusters$cluster)

# Mean activity levels by cluster

analysis_sample %>%
  group_by(cluster) %>%
  summarise(
    selfcare = mean(selfcare),
    work = mean(work),
    leisure = mean(leisure),
    childcare = mean(childcare)
  )

# Cluster visualization

ggplot(analysis_sample, aes(z_work, z_selfcare, color = cluster)) +
  geom_point(alpha = 0.4) +
  theme_minimal()


# ================= CLUSTER CHARACTERISTICS =================

analysis_sample %>% 
  tabyl(cluster, parents) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting()

analysis_sample %>% 
  tabyl(cluster, fulltime) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting()

analysis_sample %>% 
  tabyl(cluster, educ) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting()


# ================= REGRESSION WITHIN CLUSTERS =================
# Test whether predictors operate differently across lifestyle clusters

model_e1 <- lm(selfcare ~ educ + fulltime, data = analysis_sample %>% filter(cluster == 1))
model_e2 <- lm(selfcare ~ educ + fulltime, data = analysis_sample %>% filter(cluster == 2))
model_e3 <- lm(selfcare ~ educ + fulltime, data = analysis_sample %>% filter(cluster == 3))
model_e4 <- lm(selfcare ~ educ + fulltime, data = analysis_sample %>% filter(cluster == 4))

model_c1 <- lm(selfcare ~ childcare + parents, data = analysis_sample %>% filter(cluster == 1))
model_c2 <- lm(selfcare ~ childcare + parents, data = analysis_sample %>% filter(cluster == 2))
model_c3 <- lm(selfcare ~ childcare + parents, data = analysis_sample %>% filter(cluster == 3))
model_c4 <- lm(selfcare ~ childcare + parents, data = analysis_sample %>% filter(cluster == 4))

summary(model_e1)
summary(model_e2)
summary(model_e3)
summary(model_e4)

summary(model_c1)
summary(model_c2)
summary(model_c3)
summary(model_c4)


# ================= PAIRWISE VISUALIZATION =================
# Explore relationships between variables and self-care within clusters

analysis_long <- analysis_sample %>%
  pivot_longer(
    cols = c(work, leisure, childcare, parents),
    names_to = "variable",
    values_to = "value"
  )

ggplot(analysis_long, aes(x = value, y = selfcare, color = cluster)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Pairwise Relationships with Selfcare by Cluster",
    x = "Variable Value",
    y = "Selfcare (minutes)"
  )


# ================= CLUSTER PROPORTION TABLE =================
# Shows proportion of time allocation across clusters

cluster_proportion <- analysis_long %>%
  group_by(cluster) %>%
  summarise(
    selfcare = mean(selfcare, na.rm = TRUE),
    work = mean(work, na.rm = TRUE),
    leisure = mean(leisure, na.rm = TRUE),
    childcare = mean(childcare, na.rm = TRUE)
  ) %>%
  mutate(
    selfcare = selfcare / sum(selfcare),
    work = work / sum(work),
    leisure = leisure / sum(leisure),
    childcare = childcare / sum(childcare)
  ) %>%
  mutate(across(-cluster, round, 3)) %>% 
  as.data.frame()

cluster_proportion