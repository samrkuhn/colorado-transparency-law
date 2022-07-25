indeed_model_data <- job_postings %>%
  mutate(time_dummy = ifelse(date == "2021-12-31", 1, 0)) %>%
  mutate(treated = ifelse(state == "colorado", 1, 0))

# first difference
indeed_differences <- indeed_model_data %>%
  group_by(year = lubridate::year(date), state) %>%
  filter(year %in% c(2020, 2021)) %>%
  summarise(avg_postings = mean(value))
print(as_tibble(indeed_differences))

# treatment group (Colorado) BEFORE treatment
indeed_pretreatment_co <- indeed_differences[1, 3]

# control group (Utah) BEFORE treatment
indeed_pretreatment_utah <- indeed_differences[2, 3]

# treatment group (Colorado) AFTER treatment
indeed_post_treatment_co <- indeed_differences[3, 3]

# treatment group (Utah) AFTER treatment
indeed_post_treatment_utah <- indeed_differences[4, 3]

# Calculate Average Treatment Effect (ATT
# Calculate difference between the difference in 2020 and 2021 within Colorado and Utah
(indeed_post_treatment_co - indeed_pretreatment_co) - (indeed_post_treatment_utah - indeed_pretreatment_utah)

# This should print the same results as a check
(indeed_post_treatment_co - indeed_post_treatment_utah) - (indeed_pretreatment_co - indeed_pretreatment_utah)

# create dummy variables for treatment and time
# treat = 1 if Colorado else 0
# post = 1 if 2021 else 0
# treatpost = 1 if Colorado AND 2021 (this is the interaction term in the regression)
indeed_model <- indeed_model_data %>%
  # exclude any data from 2022
  # NOTE: This data set is unbalanced since it does not include observations from January 2020
  filter(
    date >= "2020-02-01",
    date <= "2021-12-31"
  ) %>%
  mutate(
    treat = state == "colorado",
    post = (date >= "2021-01-01"),
    treatpost = treat * post
  ) %>%
  arrange(desc(date))

# from linear fixed effects package, use felm function to fit model with multiple group fixed effects
# Results are statistically significant, with a negative treatment effect for the treatpost interaction term,
# matching the results above for the manual calculation of the treatment effect (this is to double check)
# correct standard errors to be clustered on state level, check thread "Robust Coefficients For Differences in Differences" in PR
lfe::felm(value ~ treatpost | post + treat | 0 | state, data = indeed_model) %>%
  summary()
# save results
indeed_model_results <- lfe::felm(value ~ treatpost | post + treat | 0 | state, data = indeed_model)
# save residuals
indeed_residuals <- indeed_model_results$residuals
