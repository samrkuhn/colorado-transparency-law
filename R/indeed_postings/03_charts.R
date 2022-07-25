
# Model plots
# Check regression assumptions
# Histogram plot of residuals
par(mfrow = c(1, 1))
plot(indeed_residuals, type = "p")
plot(indeed_residuals, type = "h")

# QQ Plots
# bimodal distribution of residuals
qqnorm(indeed_residuals)

# daily postings time series (parallel trends assumption)
job_postings %>%
  ggplot(aes(date, value, color = state)) +
  geom_line(size = 1.25) +
  geom_vline(xintercept = as.Date("2021-01-01"), color = "red", linetype = "dashed") +
  scale_x_date(
    breaks = scales::breaks_pretty(10),
    labels = scales::label_date("%b-%Y")
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(15),
    labels = scales::label_percent(scale = 1, accuracy = .01)
  ) +
  scale_color_manual(
    values = c(appcast_blue(), appcast_red()),
    labels = c("Colorado", "Utah")
  ) +
  ggrepel::geom_label_repel(
    mapping = aes(date, value, label = scales::percent(value, scale = 1, accuracy = .01)),
    size = 3,
    show.legend = FALSE,
    data = job_postings %>%
      group_by(state) %>%
      filter(date == max(date))
  ) +
  labs(
    title = "Percent change in Indeed daily job postings from Feb. 1st 2020",
    subtitle = "seasonally adjusted",
    caption = 'Red line represents Colorado requiring compensation on job postings'
  ) +
  appcast_ggtheme()
appcast_ggsave(here::here("figures", "indeed_postings.jpg"),
  source_line = paste("Source: Indeed; created on", todays_date_plot)
)

# daily postings time series (since Jan 1st 2021)
job_postings %>%
  filter(date >= "2021-01-01") %>%
  ggplot(aes(date, value, color = state)) +
  geom_line(size = 1.25) +
  scale_x_date(
    breaks = scales::breaks_pretty(12),
    labels = scales::label_date("%b-%Y")
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(15),
    labels = scales::label_percent(scale = 1, accuracy = .01)
  ) +
  scale_color_manual(
    values = c(appcast_blue(), appcast_red()),
    labels = c("Colorado", "Utah")
  ) +
  ggrepel::geom_label_repel(
    mapping = aes(date, value, label = scales::percent(value, scale = 1, accuracy = .01)),
    size = 3,
    show.legend = FALSE,
    data = job_postings %>%
      group_by(state) %>%
      filter(date == max(date))
  ) +
  labs(
    title = "Percent change in Indeed daily job postings from Jan. 1st 2021",
    subtitle = "seasonally adjusted"
  ) +
  appcast_ggtheme()
appcast_ggsave(here::here("figures", "indeed_postings_jan2021.jpg"),
  source_line = paste("Source: Indeed; created on", todays_date_plot)
)

# Create synthetic DiD plot
# The DiD assumption states that the trends of treatment and control group are identical until the treatment takes place.
# Calculate counterfactual outcome
# co_counterfactual <- tibble(
#   observation = c("2020-02-01", "2021-01-01"),
#   state = c("Colorado (Counterfactual)", "Colorado (Counterfactual)"),
#   postings = as.numeric(c(indeed_pretreatment_co, indeed_pretreatment_co - (indeed_post_treatment_utah - indeed_pretreatment_utah)))
# )
#
# # Data points for treatment event
# intervention <- tibble(
#   observation = c("Intervention", "Intervention", "Intervention"),
#   state = c("Colorado", "Utah", "Colorado (Counterfactual)"),
#   postings = c(68, 75.3, 68)
# )
#
# # Combine data
# did_plotdata <- bind_rows(indeed_differences,
#                           co_counterfactual,
#                           intervention)
#
# #DiD plot
# indeed_model_data %>%
#   mutate(label = if_else(date == "2021-01-01", as.character(state), NA_character_)) %>%
#   ggplot(aes(x=date,y=value, group=state)) +
#   geom_line(aes(color=state), size=1.2) +
#   geom_vline(xintercept = "2021-01-01", linetype="dotted",
#              color = "black", size=1.1) +
#   scale_color_brewer(palette = "Accent") +
#   scale_y_continuous(limits = c(17,24)) +
#   ggrepel::geom_label_repel(aes(label = label),
#                             nudge_x = 0.5, nudge_y = -0.5,
#                             na.rm = TRUE) +
#   guides(color=FALSE) +
#   labs(x="", y="FTE Employment (mean)") +
#   annotate(
#     "text",
#     x = "November 1992",
#     y = 19.6,
#     label = "{Difference-in-Differences}",
#     angle = 90,
#     size = 3
#   )
