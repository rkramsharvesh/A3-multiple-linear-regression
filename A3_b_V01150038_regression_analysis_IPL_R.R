# ----------------------------------------------------
# 1. Load Libraries
# ----------------------------------------------------
library(tidyverse)
library(readxl)

# ----------------------------------------------------
# 2. Load and Prepare Data
# ----------------------------------------------------
setwd('D:\\Masters\\VCU\\Classes\\SCMA\\R\\A3')
salary_data <- read_excel("../A2/IPL SALARIES 2024.xlsx")
performance_data <- read_csv("../A2/IPL_ball_by_ball_updated till 2024.csv")

# Filter recent 3 seasons
performance_data$year <- as.numeric(substr(performance_data$Date, 7, 10))
recent_perf <- performance_data %>% filter(year %in% c(2022, 2023, 2024))

# Aggregate player stats
player_stats <- recent_perf %>%
  group_by(Striker) %>%
  summarise(
    Total_Runs = sum(runs_scored, na.rm = TRUE),
    Balls_Faced = n(),
    Outs = sum(!is.na(`Player Out`) & `Player Out` == Striker)
  ) %>%
  mutate(
    Avg = ifelse(Outs == 0, NA, Total_Runs / Outs),
    SR = (Total_Runs / Balls_Faced) * 100
  ) %>%
  filter(Balls_Faced >= 30) %>%
  rename(Player = Striker)

# Clean salary data
salary_data_clean <- salary_data %>%
  mutate(Salary_Cr = as.numeric(Rs) / 100) %>%
  select(Player, Salary_Cr)

# Merge
merged_df <- inner_join(player_stats, salary_data_clean, by = "Player") %>%
  drop_na(Avg, SR, Salary_Cr) %>%
  mutate(log_salary = log(Salary_Cr + 1))

# ----------------------------------------------------
# 3. Final Model: Salary ~ Total Runs
# ----------------------------------------------------
model_final <- lm(log_salary ~ Total_Runs, data = merged_df)
summary(model_final)

# ----------------------------------------------------
# 4. Visualization
# ----------------------------------------------------
ggplot(merged_df, aes(x = Total_Runs, y = log_salary)) +
  geom_point(color = "blue", size = 2.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Relationship Between Total Runs and Log Salary",
    x = "Total Runs (2022–2024)",
    y = "Log Salary (Crores)"
  ) +
  theme_minimal()
