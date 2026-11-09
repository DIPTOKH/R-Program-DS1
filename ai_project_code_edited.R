
rm(list=ls())

library(rvest)
library(httr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

#scrapping
url <- "https://www.gov.uk/government/publications/ai-skills-for-life-and-work-labour-market-and-skills-projections/ai-skills-for-life-and-work-labour-market-and-skills-projections"

html <- read_html(
  httr::content(
    httr::GET(url),
    "text"
  )
)

# All the tables are present inside Appendix B
# id = "appendix-b-detailed-results"

appendix_b <- html %>%
  html_element(
    xpath = "//*[@id='appendix-b-detailed-results']"
  )


# Extract all tables
appendix_b_tables <- appendix_b %>%
  html_elements(
    xpath = "following::table"
  )


# Should give 6 tables:
# 8.1, 8.2, 8.3, 8.4, 8.5, 8.6

length(appendix_b_tables)

#get table names

table_captions <- sapply(
  appendix_b_tables,
  function(table){
    
    table %>%
      html_elements(
        xpath = "preceding-sibling::p[1]"
      ) %>%
      html_text()
    
  }
)

table_captions

#select tables 8.1-8.4

tbl_8.1_node <- appendix_b_tables[[grep("Table 8\\.1", table_captions)]]
tbl_8.2_node <- appendix_b_tables[[grep("Table 8\\.2", table_captions)]]
tbl_8.3_node <- appendix_b_tables[[grep("Table 8\\.3", table_captions)]]
tbl_8.4_node <- appendix_b_tables[[grep("Table 8\\.4", table_captions)]]


#raw table extraction

# header = FALSE because the tables have multiple header rows

table_8.1_raw <- tbl_8.1_node %>%
  html_table(
    fill = TRUE,
    header = FALSE
  )

table_8.2_raw <- tbl_8.2_node %>%
  html_table(
    fill = TRUE,
    header = FALSE
  )

table_8.3_raw <- tbl_8.3_node %>%
  html_table(
    fill = TRUE,
    header = FALSE
  )

table_8.4_raw <- tbl_8.4_node %>%
  html_table(
    fill = TRUE,
    header = FALSE
  )


# Inspect raw tables

head(table_8.1_raw, 3)
head(table_8.2_raw, 3)
head(table_8.3_raw, 3)
head(table_8.4_raw, 3)


#cleaning table 8.1

# Drop the 2 header rows

table_8.1 <- table_8.1_raw[-c(1,2), ]


# Set column names

names(table_8.1) <- c(
  "occupation",
  
  "BS_2024",
  "BS_2030",
  "BS_2035",
  
  "AS_2024",
  "AS_2030",
  "AS_2035",
  
  "TS_2024",
  "TS_2030",
  "TS_2035",
  
  "HS_2024",
  "HS_2030",
  "HS_2035"
)


# Clean numeric columns

table_8.1[, -1] <- lapply(
  table_8.1[, -1],
  function(x)
    as.numeric(
      gsub("[,*]", "", x)
    )
)


# Check

head(table_8.1)
str(table_8.1)

#clening table 8.2

table_8.2 <- table_8.2_raw[-c(1,2), ]


names(table_8.2) <- c(
  "occupation",
  
  "BS_net_change",
  "BS_replacement_demand",
  
  "AS_net_change",
  "AS_replacement_demand",
  
  "TS_net_change",
  "TS_replacement_demand",
  
  "HS_net_change",
  "HS_replacement_demand"
)


# Clean numeric columns

table_8.2[, -1] <- lapply(
  table_8.2[, -1],
  function(x)
    as.numeric(
      gsub("[,*]", "", x)
    )
)


# Check

head(table_8.2)
str(table_8.2)


#table 8.3

table_8.3 <- table_8.3_raw[-1, ]


if (ncol(table_8.3) == 4) {
  
  names(table_8.3) <- c(
    "occupation",
    "TS_ADJ_2024",
    "TS_ADJ_2030",
    "TS_ADJ_2035"
  )
  
} else {
  
  names(table_8.3) <- c(
    "occupation",
    paste0(
      "col_",
      seq_len(ncol(table_8.3) - 1)
    )
  )
}


# Clean numeric columns

table_8.3[, -1] <- lapply(
  table_8.3[, -1],
  function(x)
    as.numeric(
      gsub("[,*]", "", x)
    )
)


# Check

head(table_8.3)
str(table_8.3)


#table 8.4

table_8.4 <- table_8.4_raw[-1, ]


if (ncol(table_8.4) == 3) {
  
  names(table_8.4) <- c(
    "occupation",
    "net_change",
    "replacement_demand"
  )
  
} else {
  
  names(table_8.4) <- c(
    "occupation",
    paste0(
      "col_",
      seq_len(ncol(table_8.4) - 1)
    )
  )
}


# Clean numeric columns

table_8.4[, -1] <- lapply(
  table_8.4[, -1],
  function(x)
    as.numeric(
      gsub("[,*]", "", x)
    )
)


# Check

head(table_8.4)
str(table_8.4)


#check

sapply(table_8.1, class)
sapply(table_8.2, class)
sapply(table_8.3, class)
sapply(table_8.4, class)


nrow(table_8.1)
nrow(table_8.2)
nrow(table_8.3)
nrow(table_8.4)


# View tables if required

View(table_8.1)
View(table_8.2)
View(table_8.3)
View(table_8.4)



# REMOVE TOTAL ROW FOR OCCUPATION-LEVEL PLOTS

# The "Total" row is much larger than individual occupations
# and therefore makes occupation-level graphs highly skewed.
#
# We keep the original tables unchanged.
# We create separate plotting tables without the Total row.

table_8.1_plot <- table_8.1 %>%
  filter(
    !str_detect(
      occupation,
      regex("^total$", ignore_case = TRUE)
    )
  )


table_8.2_plot <- table_8.2 %>%
  filter(
    !str_detect(
      occupation,
      regex("^total$", ignore_case = TRUE)
    )
  )


# IMPORTANT:
# This graph SHOULD contain total employment.
# Therefore, we use the official total values.

total_data <- data.frame(
  
  Year = c(2024, 2030, 2035),
  
  BS = c(
    8499,
    8988,
    9407
  ),
  
  AS = c(
    8488,
    8881,
    9184
  ),
  
  TS = c(
    8633,
    9223,
    9705
  ),
  
  HS = c(
    8628,
    9213,
    9695
  )
)


total_long <- total_data %>%
  pivot_longer(
    cols = -Year,
    names_to = "Scenario",
    values_to = "Employment"
  )


ggplot(
  total_long,
  aes(
    x = Year,
    y = Employment,
    color = Scenario
  )
) +
  
  geom_line(
    linewidth = 1.2
  ) +
  
  geom_point(
    size = 3
  ) +
  
  labs(
    title = "AI-Related Employment Under Different Scenarios",
    subtitle = "Employment levels, 2024–2035",
    x = "Year",
    y = "Employment (thousands)",
    color = "Scenario"
  ) +
  
  theme_minimal()


# GRAPH 2
# TOP 10 OCCUPATIONS BY 2035 EMPLOYMENT

top10_2035 <- table_8.1_plot %>%
  
  select(
    occupation,
    TS_2035
  ) %>%
  
  arrange(
    desc(TS_2035)
  ) %>%
  
  slice_head(n = 10)


ggplot(
  top10_2035,
  aes(
    x = reorder(
      occupation,
      TS_2035
    ),
    y = TS_2035
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Top 10 AI-Related Occupations by 2035 Employment",
    subtitle = "TS scenario — Total row excluded",
    x = "Occupation",
    y = "Employment in 2035 (thousands)"
  ) +
  
  theme_minimal()


# GRAPH 3
# BIGGEST GROWERS AND DECLINERS

growth_data <- table_8.1_plot %>%
  
  select(
    occupation,
    TS_2024,
    TS_2035
  ) %>%
  
  mutate(
    Change = TS_2035 - TS_2024
  )


# Top 10 increases

top_increases <- growth_data %>%
  arrange(
    desc(Change)
  ) %>%
  slice_head(n = 10)


# Top 10 decreases

top_decreases <- growth_data %>%
  arrange(
    Change
  ) %>%
  slice_head(n = 10)


# Combine

growth_plot_data <- bind_rows(
  top_increases,
  top_decreases
)


ggplot(
  growth_plot_data,
  aes(
    x = reorder(
      occupation,
      Change
    ),
    y = Change
  )
) +
  
  geom_col() +
  
  geom_hline(
    yintercept = 0,
    linewidth = 0.8
  ) +
  
  coord_flip() +
  
  labs(
    title = "Largest Employment Changes by Occupation",
    subtitle = "TS scenario: change from 2024 to 2035 — Total row excluded",
    x = "Occupation",
    y = "Change in employment (thousands)"
  ) +
  
  theme_minimal()


# GRAPH 4
# NET CHANGE VS REPLACEMENT DEMAND

requirement_data <- table_8.2_plot %>%
  
  select(
    occupation,
    TS_net_change,
    TS_replacement_demand
  ) %>%
  
  mutate(
    Total_requirement =
      TS_net_change +
      TS_replacement_demand
  ) %>%
  
  arrange(
    desc(Total_requirement)
  ) %>%
  
  slice_head(n = 10)


# Create occupation order
# This prevents ggplot from giving a strange order

occupation_order <- requirement_data %>%
  arrange(
    Total_requirement
  ) %>%
  pull(occupation)


# Convert to long format

requirement_long <- requirement_data %>%
  
  select(
    occupation,
    TS_net_change,
    TS_replacement_demand
  ) %>%
  
  pivot_longer(
    cols = c(
      TS_net_change,
      TS_replacement_demand
    ),
    names_to = "Demand_Type",
    values_to = "Demand"
  ) %>%
  
  mutate(
    occupation = factor(
      occupation,
      levels = occupation_order
    ),
    
    Demand_Type = recode(
      Demand_Type,
      TS_net_change = "Net Change",
      TS_replacement_demand = "Replacement Demand"
    )
  )


ggplot(
  requirement_long,
  aes(
    x = occupation,
    y = Demand,
    fill = Demand_Type
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Net Change vs Replacement Demand",
    subtitle = "Top 10 occupations under the TS scenario — Total row excluded",
    x = "Occupation",
    y = "Demand (thousands)",
    fill = "Demand Type"
  ) +
  
  theme_minimal()


# GRAPH 5
# SCENARIO COMPARISON — 2035

scenario_data <- table_8.1_plot %>%
  
  select(
    occupation,
    BS_2035,
    AS_2035,
    TS_2035,
    HS_2035
  ) %>%
  
  # Select top occupations using TS scenario
  
  arrange(
    desc(TS_2035)
  ) %>%
  
  slice_head(n = 10)


# Fix occupation ordering

scenario_order <- scenario_data$occupation


scenario_long <- scenario_data %>%
  
  pivot_longer(
    cols = c(
      BS_2035,
      AS_2035,
      TS_2035,
      HS_2035
    ),
    names_to = "Scenario",
    values_to = "Employment"
  ) %>%
  
  mutate(
    
    Scenario = gsub(
      "_2035",
      "",
      Scenario
    ),
    
    occupation = factor(
      occupation,
      levels = scenario_order
    )
  )


ggplot(
  scenario_long,
  aes(
    x = occupation,
    y = Employment,
    fill = Scenario
  )
) +
  
  geom_col(
    position = "dodge"
  ) +
  
  coord_flip() +
  
  labs(
    title = "2035 Employment Across AI Scenarios",
    subtitle = "Top 10 occupations — Total row excluded",
    x = "Occupation",
    y = "Employment (thousands)",
    fill = "Scenario"
  ) +
  
  theme_minimal()

# GRAPH 6
# OCCUPATION GROWTH RATE

growth_rate <- table_8.1_plot %>%
  
  select(
    occupation,
    TS_2024,
    TS_2035
  ) %>%
  
  mutate(
    Growth_Rate =
      (
        (TS_2035 - TS_2024) /
          TS_2024
      ) * 100
  )


# Top 10 growth rates

top_growth_rate <- growth_rate %>%
  
  arrange(
    desc(Growth_Rate)
  ) %>%
  
  slice_head(n = 10)


ggplot(
  top_growth_rate,
  aes(
    x = reorder(
      occupation,
      Growth_Rate
    ),
    y = Growth_Rate
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Occupations with the Highest Projected Growth",
    subtitle = "TS scenario, 2024–2035 — Total row excluded",
    x = "Occupation",
    y = "Growth rate (%)"
  ) +
  
  theme_minimal()

# CLEAN TABLE 8.3

table_8.3_clean <- table_8.3 %>%
  
  # Remove the header-like row
  
  filter(
    occupation != "Levels (000s)"
  ) %>%
  
  # Keep TS adjusted columns
  
  select(
    occupation,
    col_4,
    col_5,
    col_6
  )


names(table_8.3_clean) <- c(
  "occupation",
  "AI_Adj_2024",
  "AI_Adj_2030",
  "AI_Adj_2035"
)


# Remove Total if it exists

table_8.3_clean <- table_8.3_clean %>%
  filter(
    !str_detect(
      occupation,
      regex("^total$", ignore_case = TRUE)
    )
  )

# GRAPH 7
# AI-ADJUSTED EMPLOYMENT IN 2035


top_ai_adjusted <- table_8.3_clean %>%
  
  arrange(
    desc(AI_Adj_2035)
  ) %>%
  
  slice_head(n = 10)


ggplot(
  top_ai_adjusted,
  aes(
    x = reorder(
      occupation,
      AI_Adj_2035
    ),
    y = AI_Adj_2035
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Top Occupations by AI-Adjusted Employment",
    subtitle = "Experimental approach, 2035 — Total row excluded",
    x = "Occupation",
    y = "AI-adjusted employment (thousands)"
  ) +
  
  theme_minimal()

# GRAPH 8
# AI-ADJUSTED EMPLOYMENT CHANGE

ai_change <- table_8.3_clean %>%
  
  mutate(
    Change =
      AI_Adj_2035 -
      AI_Adj_2024
  ) %>%
  
  arrange(
    desc(Change)
  ) %>%
  
  slice_head(n = 10)


ggplot(
  ai_change,
  aes(
    x = reorder(
      occupation,
      Change
    ),
    y = Change
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Largest Increases in AI-Adjusted Employment",
    subtitle = "2024–2035 — Total row excluded",
    x = "Occupation",
    y = "Change in AI-adjusted employment (thousands)"
  ) +
  
  theme_minimal()

# GRAPH 10
# CURRENT EMPLOYMENT VS FUTURE GROWTH

scatter_data <- table_8.1_plot %>%
  
  select(
    occupation,
    TS_2024,
    TS_2035
  ) %>%
  
  mutate(
    Growth =
      TS_2035 -
      TS_2024
  )


ggplot(
  scatter_data,
  aes(
    x = TS_2024,
    y = Growth
  )
) +
  
  geom_point(
    size = 3
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  
  labs(
    title = "Current Employment vs Future Employment Growth",
    subtitle = "TS scenario — Total row excluded",
    x = "Employment in 2024 (thousands)",
    y = "Employment growth 2024–2035 (thousands)"
  ) +
  
  theme_minimal()