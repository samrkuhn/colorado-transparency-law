library(tidyverse)
library(appcastGGtheme)
library(ggrepel)
library(fredr)
library(fixest)

todays_date <- Sys.Date()
todays_date_plot <- format(todays_date, "%b %d %Y")
previous_month <- format(lubridate::floor_date(todays_date, "month") - months(1), format = "%B")
previous_year <- format(lubridate::floor_date(todays_date, "month") - months(12))

# Daily change in indeed job postings in Colorado and Utah
params <- list(
  series_id = c("IHLCHGUSUT", "IHLCHGUSCO"),
  frequency = c("d"),
  units = c("lin")
)

job_postings <- pmap_dfr(
  .l = params,
  .f = ~ fredr(series_id = ..1, frequency = ..2, units = ..3)
) %>%
  filter(date >= "2000-01-01") %>%
  pivot_wider(names_from = "series_id", values_from = "value") %>%
  rename(
    utah = IHLCHGUSUT,
    colorado = IHLCHGUSCO
  ) %>%
  mutate(date == lubridate::ymd(date)) %>%
  select(date, utah, colorado) %>%
  pivot_longer(cols = where(is.numeric), names_to = "state")
