library(tidyverse)
library(appcastGGtheme)
library(ggrepel)
todays_date <- Sys.Date()
todays_date_plot <- format(todays_date, "%b %d %Y")

shading <- model_data %>% 
  filter(lubridate::ymd(date) %in% as.Date(c('2020-02-01', '2020-03-01', '2020-04-01'))) %>% 
  mutate(ymax = Inf, ymin = -Inf) %>% 
  select(date, ymax, ymin)

#Remove June observations since data is not on FRED yet
model_data <- model_data %>% 
  filter(date != '2022-06-01')

#average hourly earnings
model_data %>% 
  filter(date >= '2020-01-01' & date <= '2021-12-31') %>% 
  ggplot(aes(date), shading) +
  geom_line(aes(date, avgearnings_colorado, color = 'Average Hourly Earnings: Colorado'), size = 1.25) +
  geom_line(aes(date, avgearnings_utah, color = 'Average Hourly Earnings: Utah'), size = 1.25) +
  scale_y_continuous(labels = scales::label_dollar(), breaks = seq(25, 35, by = 1)) +
  scale_x_date(date_breaks = '2 months', date_labels = '%b %Y') +
  geom_vline(aes(xintercept = as.Date('2021-01-01')), linetype = 8, color = 'red') +
  ggrepel::geom_label_repel(aes(date, avgearnings_colorado, label = scales::dollar(avgearnings_colorado, scale = 1)),
                            color = appcast_mascoma(),
                            nudge_x = .15,
                            nudge_y = -.15,
                            data = model_data %>% 
                              filter(date >= '2020-01-01' & date <= '2021-12-31') %>%
                              filter(date == max(date))) +
  ggrepel::geom_label_repel(aes(date, avgearnings_utah, label = scales::dollar(avgearnings_utah, scale = 1)),
                            color = appcast_blue(),
                            nudge_x = .15,
                            nudge_y = -.15,
                            data = model_data %>% 
                              filter(date >= '2020-01-01' & date <= '2021-12-31') %>%
                              filter(date == max(date))) +
  labs(
    title = 'Average Hourly Earnings, Before and After "Equal Pay for Equal Work Act"'
  ) +
  appcast_ggtheme() +
  appcast_scale_color()
appcast_ggsave(here::here('figures', 'avg_hourly_earnings.jpg'),
               source_line = paste('Source: Bureau of Labor Statistics; created on', todays_date_plot))


#employee levels
model_data %>% 
  filter(date >= '2020-01-01' & date <= '2021-12-31') %>% 
  ggplot(aes(date)) +
  geom_line(aes(date, employees_colorado, color = 'Total nonfarm employees: Colorado'), size = 1.25) +
  geom_line(aes(date, employees_utah, color = 'Total nonfarm employees: Utah'), size = 1.25) +
  scale_x_date(date_breaks = '2 months', date_labels = '%b %Y') +
  scale_y_continuous(limits = c(1000, 3000), labels = scales::label_number(scale = .001, suffix = 'm', accuracy = .01)) +
  geom_vline(aes(xintercept = as.Date('2021-01-01')), linetype = 8, color = 'red') +
  ggrepel::geom_label_repel(aes(date, employees_colorado, label = scales::number(employees_colorado, scale = .001, suffix = 'm', accuracy = .01)),
                            nudge_x = .1,
                            nudge_y = .1,
                            data = model_data %>% 
                              filter(date >= '2020-01-01' & date <= '2021-12-31') %>%
                              filter(date == max(date))) +
  ggrepel::geom_label_repel(aes(date, employees_utah, label = scales::number(employees_utah, scale = .001, suffix = 'm', accuracy = .01)),
                            nudge_x = .1,
                            nudge_y = .1,
                            data = model_data %>% 
                              filter(date >= '2020-01-01' & date <= '2021-12-31') %>%
                              filter(date == max(date))) +
  labs(
    title = 'Total nonfarm payrolls, Before and After "Equal Pay for Equal Work Act"',
    subtitle = 'in millions, seasonally adjusted'
  ) +
  appcast_ggtheme() +
  appcast_scale_color()
appcast_ggsave(here::here('figures', 'employee_levels.jpg'),
               source_line = paste('Source: Bureau of Labor Statistics; created on', todays_date_plot))

#employment growth plot
model_data %>% 
  filter(date >= '2020-01-01' & date <= '2021-12-31') %>% 
  ggplot(aes(date)) +
  geom_line(aes(date, ann_pct_change_smoothed_colorado, color = 'Total nonfarm employees: Colorado'), size = 1.25) +
  geom_line(aes(date, ann_pct_change_smoothed_utah, color = 'Total nonfarm employees: Utah'), size = 1.25) +
  scale_x_date(date_breaks = '2 months', date_labels = '%b %Y') +
  scale_y_continuous(limits = c(-.25, .75), labels = scales::label_percent(scale = 1, accuracy = .01)) +
  geom_vline(aes(xintercept = as.Date('2021-01-01')), linetype = 8, color = 'red') +
  ggrepel::geom_label_repel(aes(date, ann_pct_change_smoothed_colorado, label = scales::percent(ann_pct_change_smoothed_colorado, scale = 1, accuracy = .001)),
                            color = appcast_mascoma(),
                            nudge_x = -.1,
                            nudge_y = .1,
                            data = model_data %>% 
                              filter(date >= '2020-01-01' & date <= '2021-12-31') %>%
                              filter(date == max(date))) +
  ggrepel::geom_label_repel(aes(date, ann_pct_change_smoothed_utah, label = scales::percent(ann_pct_change_smoothed_utah, scale = 1, accuracy = .001)),
                            color = appcast_blue(),
                            nudge_x = .1,
                            nudge_y = -.1,
                            data = model_data %>% 
                              filter(date >= '2020-01-01' & date <= '2021-12-31') %>% 
                              filter(date == max(date))) +
  labs(
    title = 'Total nonfarm payrolls, Before and After "Equal Pay for Equal Work Act"',
    subtitle = 'percent change, seasonally adjusted'
  ) +
  appcast_ggtheme() +
  appcast_scale_color()
appcast_ggsave(here::here('figures', 'employee_growth.jpg'),
               source_line = paste('Source: Bureau of Labor Statistics; created on', todays_date_plot))

did_data %>% 
  filter(date >= '2010-01-01') %>% 
  ggplot(aes(date, lfpr, color = state)) +
  geom_line(size = 1.25) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  scale_y_continuous(limits = c(65, 71), labels = scales::label_percent(scale = 1, accuracy = .1), breaks = seq(65, 71, by = 1)) +
  scale_color_manual(values = c(appcast_blue(), appcast_red()),
                     labels = c("Colorado", "Utah")) +
  labs(
    title = 'Labor Force Participation Rate, Before and After "Equal Pay for Equal Work Act"',
    subtitle = 'rate, seasonally adjusted'
  ) +
  geom_vline(aes(xintercept = as.Date('2021-01-01')), linetype = 8, color = 'red') +
  ggrepel::geom_label_repel(aes(date, lfpr, label = scales::percent(lfpr, scale = 1)),
                            nudge_x = .5,
                            nudge_y = .5,
                            show.legend = FALSE,
                            data = did_data %>% 
                              filter(date == max(date))) +
  appcast_ggtheme()
appcast_ggsave(here::here('figures', 'lfpr.jpg'),
               source_line = paste('Source: Bureau of Labor Statistics; created on', todays_date_plot))


#lfpr rate of change
model_data %>% 
  mutate(smoothed_avg_colorado = zoo::rollapply(lfpr_colorado, width = 3, align = 'left', FUN = mean, na.rm = TRUE, fill = NA),
         ann_pct_change_smoothed_colorado = ((smoothed_avg_colorado/dplyr::lead(smoothed_avg_colorado))^12)-1) %>% 
  mutate(smoothed_avg_utah = zoo::rollapply(lfpr_utah, width = 3, align = 'left', FUN = mean, na.rm = TRUE, fill = NA),
         ann_pct_change_smoothed_utah = ((smoothed_avg_utah/dplyr::lead(smoothed_avg_utah))^12)-1) %>% 
  filter(date >= '2020-01-01') %>% 
  ggplot(aes(date)) +
  geom_line(aes(date, ann_pct_change_smoothed_colorado, color = "Labor Force Participation Rate: Colorado"), size = 1.25) +
  geom_line(aes(date, ann_pct_change_smoothed_utah, color = "Labor Force Participation Rate: Utah"), size = 1.25) +
  scale_x_date(date_breaks = '3 months', date_labels = '%b-%Y') +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = .01)) +
  geom_vline(aes(xintercept = as.Date('2021-01-01')), linetype = 8, color = 'red') +
  ggrepel::geom_label_repel(aes(date, ann_pct_change_smoothed_colorado, label = scales::percent(ann_pct_change_smoothed_colorado, 
                                                                                                scale = 1, accuracy = .01)),
                            color = appcast_blue(),
                            nudge_x = .005,
                            nudge_y = .005,
                            show.legend = FALSE,
                            data = model_data %>% 
                              mutate(smoothed_avg_colorado = zoo::rollapply(lfpr_colorado, width = 3, align = 'left', FUN = mean, na.rm = TRUE, fill = NA),
                                     ann_pct_change_smoothed_colorado = ((smoothed_avg_colorado/dplyr::lead(smoothed_avg_colorado))^12)-1) %>% 
                              mutate(smoothed_avg_utah = zoo::rollapply(lfpr_utah, width = 3, align = 'left', FUN = mean, na.rm = TRUE, fill = NA),
                                     ann_pct_change_smoothed_utah = ((smoothed_avg_utah/dplyr::lead(smoothed_avg_utah))^12)-1) %>% 
                              filter(date >= '2019-01-01') %>% 
                              filter(date == max(date))) +
  ggrepel::geom_label_repel(aes(date, ann_pct_change_smoothed_utah, label = scales::percent(ann_pct_change_smoothed_utah, 
                                                                                                scale = 1, accuracy = .01)),
                            color = appcast_red(),
                            nudge_x = .005,
                            nudge_y = .005,
                            show.legend = FALSE,
                            data = model_data %>% 
                              mutate(smoothed_avg_colorado = zoo::rollapply(lfpr_colorado, width = 3, align = 'left', FUN = mean, na.rm = TRUE, fill = NA),
                                     ann_pct_change_smoothed_colorado = ((smoothed_avg_colorado/dplyr::lead(smoothed_avg_colorado))^12)-1) %>% 
                              mutate(smoothed_avg_utah = zoo::rollapply(lfpr_utah, width = 3, align = 'left', FUN = mean, na.rm = TRUE, fill = NA),
                                     ann_pct_change_smoothed_utah = ((smoothed_avg_utah/dplyr::lead(smoothed_avg_utah))^12)-1) %>% 
                              filter(date >= '2019-01-01') %>% 
                              filter(date == max(date))) +
  scale_color_manual(values = c(appcast_blue(), appcast_red()),
                     labels = c("Colorado", "Utah")) +
  
  labs(
    title = 'Annualized growth rate of the labor force participation rate',
    subtitle = 'Before and after "Equal Pay for Equal Work Act", seasonally adjusted',
    caption = 'Red line represents Colorado requiring compensation on job postings'
  ) +
  appcast_ggtheme()
appcast_ggsave(here::here('figures', 'lfpr_growth_rate.jpg'),
               source_line = paste('Source: Bureau of Labor Statistics; created on', todays_date_plot))
