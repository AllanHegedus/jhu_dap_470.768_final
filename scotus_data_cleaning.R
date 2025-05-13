# The following script is meant to clean the SCOTUS dataset to make it easier to use
# and analyzed. This will mainly involve cleaning our categorical variables of interest.
# In addition, I will perform the analysis here so that I can copy-paste it into 
# the quarto document more easily.

# Author: Allan Hegedus

# libraries
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(knitr)
library(tidytext)

# file

scotus_data <- read.xlsx("SCDB_2024_01_caseCentered_LegalProvision.xlsx", detectDates = T)

# cleaning the categorical variables

# Chief Justice is good as is
scotus_data$chief_justice_clean <- scotus_data$chief

# Issue area - switch from numerics 

# Decision direction
scotus_data <- scotus_clean %>%
  mutate(decision_Direction_clean = case_when(
    decisionDirection == 1 ~ "conservative",
    decisionDirection == 2 ~ "liberal",
    decisionDirection == 3 ~ "unknown",
    TRUE ~ NA_character_
  ))

# Issue area
# labels
issue_area_labels <- c(
  "1" = "Criminal Procedure",
  "2" = "Civil Rights",
  "3" = "First Amendment",
  "4" = "Due Process",
  "5" = "Privacy",
  "6" = "Attorneys",
  "7" = "Unions",
  "8" = "Economic Activity",
  "9" = "Judicial Power",
  "10" = "Federalism",
  "11" = "Interstate Relations",
  "12" = "Federal Taxation",
  "13" = "Miscellaneous",
  "14" = "Private Action"
)

# new variable
scotus_data <- scotus_data %>%
  mutate(issue_area_clean = recode(as.character(issueArea), !!!issue_area_labels))

# Case disposition
case_disposition_labels <- c(
  "1" = "stay, petition, or motion granted",
  "2" = "affirmed (includes modified)",
  "3" = "reversed",
  "4" = "reversed and remanded",
  "5" = "vacated and remanded",
  "6" = "affirmed and reversed (or vacated) in part",
  "7" = "affirmed and reversed (or vacated) in part and remanded",
  "8" = "vacated",
  "9" = "petition denied or appeal dismissed",
  "10" = "certification to or from a lower court",
  "11" = "no disposition"
)

# new variable
scotus_data <- scotus_data %>%
  mutate(case_disposition_clean = recode(as.character(caseDisposition), !!!case_disposition_labels))

# then we'll just confirm the numeric videos and we should be good to go
scotus_data$maj_votes_clean <- as.numeric(scotus_data$majVotes)
scotus_data$min_votes_clean <- as.numeric(scotus_data$minVotes)

scotus_data$remand_clean <- ifelse(grepl("remand", scotus_data$case_disposition_clean, ignore.case = T), 1, 0)

# export
write.xlsx(scotus_data, "scotus_data_clean.xlsx")

# analysis

# get cleaned dataset
scotus_clean <- read.xlsx("scotus_data_clean.xlsx")

# box-and-whisker plot of majority votes by chief justice
# Chief Justice order
scotus_clean <- scotus_clean %>%
  mutate(chief_justice_clean = factor(chief_justice_clean, levels = c(
    "Vinson", "Warren", "Burger", "Rehnquist", "Roberts"
  )))

# Plot
ggplot(scotus_clean, aes(x = chief_justice_clean, y = maj_votes_clean)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Distribution of Majority Votes by Chief Justice",
    x = "Chief Justice",
    y = "Majority Votes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(scotus_clean, aes(x = chief_justice_clean, y = maj_votes_clean)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Distribution of Majority Votes by Chief Justice",
    x = "Chief Justice",
    y = "Majority Votes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# table of missing values - only include variables in the analysis, those with missing vals
missing_summary <- scotus_clean %>%
  select(chief, majVotes, minVotes, issueArea, caseDisposition, decisionDirection) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing Values") %>%
  arrange(desc(`Missing Values`)) %>%
  filter(`Missing Values` > 0)

# Output table in Quarto
kable(missing_summary, caption = "Missing Values in Selected Supreme Court Variables")

# plot of the top 3 issue groups each Chief Justice faced
# Count cases by chief justice and issue area
top_issues <- scotus_data %>%
  count(chief_justice_clean, issue_area_clean, name = "n") %>%
  group_by(chief_justice_clean) %>%
  slice_max(n, n = 3) %>%  
  ungroup()

# Plot
# top 3 issues per chief
top_issues <- scotus_data %>%
  count(chief_justice_clean, issue_area_clean, name = "n") %>%
  group_by(chief_justice_clean) %>%
  slice_max(n, n = 3) %>%
  ungroup()

# refactor
top_issues <- top_issues %>%
  mutate(chief_justice_clean = factor(chief_justice_clean, levels = c(
    "Vinson", "Warren", "Burger", "Rehnquist", "Roberts"
  )))

# plot, ordering bars within each chief
ggplot(top_issues, aes(x = reorder_within(issue_area_clean, -n, chief_justice_clean), y = n, fill = issue_area_clean)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ chief_justice_clean, scales = "free_x") +
  labs(
    title = "Top 3 Issue Areas by Chief Justice",
    x = "Issue Area",
    y = "Number of Cases"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

# Chart of the outcomes of our three interest areas
top_issues_direction <- scotus_clean %>%
  filter(issue_area_clean %in% c("Criminal Procedure", "Economic Activity", "Civil Rights") 
         & chief_justice_clean %in% c("Burger", "Rehnquist", "Roberts")) %>%
  mutate(issue_area_clean = factor(issue_area_clean, levels = c("Criminal Procedure", "Civil Rights", "Economic Activity"))) %>%
  group_by(issue_area_clean, decision_Direction_clean) %>%
  summarize(total_count = n(), .groups = "drop") %>%
  group_by(issue_area_clean) %>%
  mutate(percent = round(100 * total_count / sum(total_count), 1))

# Making the NAs unknown
top_issues_direction$decision_Direction_clean <- ifelse(is.na(top_issues_direction$decision_Direction_clean), "unknown", top_issues_direction$decision_Direction_clean)

# The plot
ggplot(top_issues_direction, aes(x = issue_area_clean, y = percent, fill = decision_Direction_clean)) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = c(
      "liberal" = "blue",
      "unknown" = "yellow",
      "conservative" = "red"
    )
  ) +
  labs(
    title = "Decision Direction by Issue Area",
    x = "Issue Area",
    y = "Percentage of Cases",
    fill = "Decision Direction"
  ) +
  theme_minimal()

# Finally, a plot about remands by case type
remands_by_type <- scotus_clean %>%
  filter(chief_justice_clean == "Roberts") %>%
  group_by(issue_area_clean) %>%
  summarize(
    `Number of Cases` = n(),
    `Percentage of Cases` = mean(remand_clean, na.rm = TRUE) * 100,
    `Standard Deviation` = sd(remand_clean, na.rm = TRUE)
  ) %>%
  arrange(desc(`Percentage of Cases`)) %>%
  rename("Issue Area" = issue_area_clean)

# Table
kable(remands_by_type)
