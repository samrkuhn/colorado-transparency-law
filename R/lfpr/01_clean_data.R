library(tidyverse)
library(appcastGGtheme)
library(ggrepel)
library(fredr)
library(gt)

todays_date <- Sys.Date()
todays_date_plot <- format(todays_date, "%b %d %Y")

# variable names
# Average hourly earnings Utah: SMU49000000500000003
# Average hourly Earnings Colorado: SMU08000000500000003
# All Employees Colorado: CONA
# All Employees Utah: UTNA
params <- list(
  series_id = c(
    "SMU49000000500000003", "SMU08000000500000003", "CONA", "UTNA",
    "SMU56000000500000003", "LBSSA49", "LBSSA08"
  ),
  frequency = c("m"),
  units = c("lin")
)
model_data <- pmap_dfr(
  .l = params,
  .f = ~ fredr(series_id = ..1, frequency = ..2, units = ..3)
) %>%
  filter(date >= "2000-01-01") %>%
  pivot_wider(names_from = "series_id", values_from = "value") %>%
  select(-realtime_start, -realtime_end) %>%
  mutate(date = lubridate::ymd(date)) %>%
  rename(
    avgearnings_utah = SMU49000000500000003,
    avgearnings_colorado = SMU08000000500000003,
    employees_colorado = CONA,
    employees_utah = UTNA,
    avgearnings_wyoming = SMU56000000500000003,
    lfpr_colorado = LBSSA08,
    lfpr_utah = LBSSA49
  ) %>%
  mutate(
    smoothed_avg_colorado = zoo::rollapply(employees_colorado, width = 3, align = "left", FUN = mean, na.rm = TRUE, fill = NA),
    ann_pct_change_smoothed_colorado = ((smoothed_avg_colorado / dplyr::lead(smoothed_avg_colorado))^12) - 1
  ) %>%
  mutate(
    smoothed_avg_utah = zoo::rollapply(employees_utah, width = 3, align = "left", FUN = mean, na.rm = TRUE, fill = NA),
    ann_pct_change_smoothed_utah = ((smoothed_avg_utah / dplyr::lead(smoothed_avg_utah))^12) - 1
  ) %>%
  select(
    date, avgearnings_utah, avgearnings_colorado, employees_colorado, employees_utah, avgearnings_wyoming,
    smoothed_avg_utah, smoothed_avg_colorado, ann_pct_change_smoothed_colorado, ann_pct_change_smoothed_utah,
    lfpr_colorado, lfpr_utah
  ) %>%
  janitor::clean_names()
