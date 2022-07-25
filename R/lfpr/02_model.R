did_model_data <- model_data %>%
  select(date, employees_colorado, employees_utah, avgearnings_utah, avgearnings_colorado, lfpr_colorado, lfpr_utah) %>%
  pivot_longer(cols = -date, names_to = "variable") %>%
  separate(variable, sep = "_", into = c("variable", "state")) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(date >= "2020-01-01" & date <= "2021-12-31") %>%
  mutate(time_dummy = ifelse(date == "2021-12-01", 1, 0)) %>%
  mutate(treated = ifelse(state == "colorado", 1, 0))

# first difference
differences <- did_model_data %>%
  group_by(year = lubridate::year(date), state) %>%
  summarise(lfpr = mean(lfpr))
print(as_tibble(differences))

# treatment group (Colorado) BEFORE treatment
pretreatment_co <- differences[1, 3]

# control group (Utah) BEFORE treatment
pretreatment_utah <- differences[2, 3]

# treatment group (Colorado) AFTER treatment
post_treatment_co <- differences[3, 3]

# treatment group (Utah) AFTER treatment
post_treatment_utah <- differences[4, 3]

# Calculate Average Treatment Effect (ATT
# Calculate difference between the difference in 2020 and 2021 within Colorado and Utah
(post_treatment_co - pretreatment_co) - (post_treatment_utah - pretreatment_utah)

# same result
(post_treatment_co - post_treatment_utah) - (pretreatment_co - pretreatment_utah)

did_data <- model_data %>%
  select(date, employees_colorado, employees_utah, avgearnings_utah, avgearnings_colorado, lfpr_colorado, lfpr_utah) %>%
  pivot_longer(cols = -date, names_to = "variable") %>%
  separate(variable, sep = "_", into = c("variable", "state")) %>%
  pivot_wider(names_from = variable, values_from = value)

# create dummy variables for treatment and time
# treat = 1 if colorardo else 0
# post = 1 if 2021 else 0
# treatpost = 1 if colorado AND 2021
lfpr_model <- did_data %>%
  filter(date >= "2020-01-01") %>%
  mutate(
    treat = state == "colorado",
    post = (date >= "2021-01-01"),
    treatpost = treat * post
  )

# from linear fixed effects package, use felm function to fit model with multiple group fixed effects
lfe::felm(log(lfpr) ~ treatpost | post + treat, data = lfpr_model) %>%
  summary()
