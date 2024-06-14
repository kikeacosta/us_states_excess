rm (list = ls())
source("code/00_setup.R")
source("code/01_excess_age_estimates.R")
library(ggpubr)

# loading data about government political affiliation
polit <- read_csv("data_input/woolf_tabs3.csv",
                  show_col_types = FALSE)
codes <- read_csv("data_input/us_state_codes.csv",
                  show_col_types = FALSE)

pol2 <- 
  polit %>% 
  filter(phase %in% 1:3) %>% 
  group_by(state) %>% 
  summarise(pol = round(mean(polit))) %>%  
  bind_rows(tibble(state = "District of Columbia", pol = 1),
            tibble(state = "Nebraska", pol = 4),
            tibble(state = "US", pol = 3)) %>% 
  mutate(chm = case_when(pol == 1 ~ "Democrat",
                         pol == 4 ~ "Republican",
                         TRUE ~ "Mixed"),
         gov = ifelse(pol <= 2, "dem","rep"))


# phases definitions
# phases for adjusting rates denominators
ph1 <- ymd(c("2020-03-07", "2020-06-13"))
ph2 <- ymd(c("2020-06-20", "2021-03-13"))
ph3 <- ymd(c("2021-03-20", "2022-03-19"))
ph4 <- ymd(c("2022-03-26", "2023-02-04"))
ph5 <- ymd(c("2023-02-11", "2023-07-02"))

# only for phases 1-4, 
# excluding phase 5 (less than 500 excess deaths in the whole US!!)

# fractions for incomplete years 2020 and 2023
fr2020 <- interval(ymd("2020-03-07"), ymd("2020-12-31")) %>% as.numeric('years')
fr2023 <- interval(ymd("2023-01-01"), ymd("2023-02-04")) %>% as.numeric('years')

# US population by age
# will be used as the reference population for standardization 
pop_us <- read_tsv("data_input/pop_us_agex5_2015_2023.txt", 
                   show_col_types = FALSE)

pop_us2 <- 
  pop_us %>% 
  select(year = `Year Code`,
         age = `Age Group Code`,
         pop = `Projected Populations`) %>% 
  mutate(age = parse_number(age),
         age = if_else(age < 15,0,age)) %>% 
  filter(year %in% 2020:2023) %>% 
  # adding the fraction for each year
  mutate(fr = case_when(year == 2020 ~ fr2020,
                        year == 2023 ~ fr2023,
                        TRUE ~ 1)) %>% 
  # calculating exposures for the whole period 2020-03-07 - 2023-07-02
  group_by(age) %>% 
  summarise(exposure = sum(pop * fr),
            .groups = "drop") %>% 
  mutate(cx = exposure/sum(exposure)) %>% 
  select(-exposure)


# Population by state by age
pop_sts <- read_csv("data_inter/pop_state_age_2013_2024.csv", 
                    show_col_types = FALSE)

# calculating population exposures by state in person-years for all phases
pop_sts2 <- 
  pop_sts %>% 
  filter(year %in% 2020:2023) %>% 
  # adding the fraction for each year
  mutate(fr = case_when(year == 2020 ~ fr2020,
                        year == 2023 ~ fr2023,
                        TRUE ~ 1)) %>% 
  # calculating exposures for the whole period 2020-03-07 - 2023-07-02
  group_by(state, age) %>% 
  summarise(exposure = sum(pop * fr),
            .groups = "drop")


pop_sts_tot <- 
  pop_sts2 %>% 
  group_by(state) %>% 
  summarise(exp_tot = sum(exposure),.groups = "drop")


# excess estimates by state and age
exc <- read_csv("data_inter/excess_state_phase_age.csv", 
                show_col_types = FALSE)

# merging excess deaths and exposures (by age)
exc2 <- 
  exc %>% 
  filter(phase %in% 1:4) %>% 
  group_by(state, age) %>% 
  summarise(dx = sum(dx),
            bsn = sum(bsn),
            exc = sum(exc),
            .groups = "drop") %>% 
  left_join(pop_sts2, 
            by = join_by(state, age)) %>% 
  # this is an approximation, taking info from just 1 pop estimate,
  # can be refined
  mutate(# excess rates (exc_r) expressed as per 100k, by age
    exc_r = 1e5 * exc / exposure,
    # baseline rates (bsn_r) expressed as per 100k, by age
    bsn_r = 1e5 * bsn / exposure,
    # death rates (mx) expressed as per 100k, by age
    mx = 1e5 * dx / exposure
  )

write_csv(exc2, "data_inter/excess_rates_by_age_phases_1_4.csv")


# crude excess rates for the whole period 
tot_rates <- 
  exc2 %>% 
  group_by(state) %>% 
  summarise(exc = sum(exc),
            bsn = sum(bsn),
            exposure = sum(exposure)) %>% 
  mutate(exc_r = 1e5*exc/exposure,
         psc = exc/bsn) %>%
  arrange(exc_r)

# average p-score across states 
tot_rates %>% 
    summarise(psc_av = mean(psc))


# age-specific excess deaths according to standard population
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# using the age structure of the US
# and the total population in each state

std <- 
  exc2 %>% 
  select(state, age, exc_r, bsn_r, mx) %>% 
  left_join(pop_us2, 
            by = join_by(age)) %>% 
  left_join(pop_sts_tot, 
            by = join_by(state)) %>% 
  # remember to undo the 100k units of rates
  mutate(
    exp_std = exp_tot * cx,
    exc_std = exc_r * exp_std / 1e5,
    bsn_std = bsn_r * exp_std / 1e5,
    dx_std = mx * exp_std / 1e5,
  )

std2 <- 
  std %>% 
  # calculate age-standardized obs death, baseline, and excess rates
  group_by(state) %>% 
  summarise(exc_std = sum(exc_std),
            bsn_std = sum(bsn_std),
            dx_std = sum(dx_std),
            exposure = sum(exp_std), 
            .groups = "drop") %>% 
  # once again age-standardized excess rate scaled up to pre 100k
  mutate(exc_r_std = 1e5 * exc_std / exposure,
         bsn_r_std = 1e5 * bsn_std / exposure,
         mx_std = 1e5 * dx_std / exposure,
         psc = exc_std / bsn_std) %>% 
  arrange(-exc_r_std) %>% 
  mutate(rnk_exc_std = 1:n()) %>% 
  arrange(-psc) %>% 
  mutate(rnk_psc_std = 1:n()) %>% 
  arrange(rnk_exc_std)

# checks, for quick stats
std2 %>% 
  summarize(psc_av = mean(psc))

std2 %>% 
  summarize(exc = sum(exc_std))

write_csv(std2, "data_inter/age_standardized_excess_rates_states.csv")

# looking at raw ranking
# ~~~~~~~~~~~~~~~~~~~~~~

# adding all phases together for accumulating Wolff's estimates
exc_wlf <- 
  exc %>% 
  left_join(pop_sts2, 
            by = join_by(state, age)) %>% 
  group_by(state) %>% 
  summarise(exc = sum(exc),
            bsn = sum(bsn),
            dts = sum(dx),
            exposure = sum(exposure),
            .groups = "drop") %>% 
  mutate(exc_r = 1e5*exc/exposure,
         psc = exc / bsn) %>% 
  arrange(-exc_r) %>% 
  mutate(rnk_exc_raw = 1:n()) %>% 
  arrange(-psc) %>% 
  mutate(rnk_psc_raw = 1:n()) %>% 
  arrange(rnk_exc_raw)



# comparing standardized and raw ranks
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cmp <- 
  std2 %>% 
  select(state, rnk_exc_std, rnk_psc_std) %>% 
  left_join(exc_wlf %>% 
              select(state, rnk_exc_raw, rnk_psc_raw),
            by = join_by(state))


write_csv(cmp, "data_inter/preliminary_ranks_raw_std_cum.csv")





# best practice mortality
# ~~~~~~~~~~~~~~~~~~~~~~~
# selecting the lowest mortality baseline in each age among all states
bst <- 
  exc2 %>% 
  group_by(age) %>% 
  filter(bsn_r == min(bsn_r)) %>% 
  arrange(age) 


bst2 <- 
  bst %>% 
  select(age, bsn_bst_r = bsn_r)


# pre-pandemic excess
# ~~~~~~~~~~~~~~~~~~~
exc_pre <- 
  std %>% 
  # joining each state with the best baseline practice for each age
  left_join(bst2, by = join_by(age)) %>% 
  mutate(bsn_bst = bsn_bst_r * exp_std / 1e5,
         exc_pre_r = bsn_r - bsn_bst_r,
         exc_pre = exc_pre_r * exp_std / 1e5)

# all ages together
exc_pre2 <- 
  exc_pre %>% 
  group_by(state) %>% 
  summarise(dx_std = sum(dx_std),
            bsn_std = sum(bsn_std),
            exc_std = sum(exc_std),
            bsn_bst = sum(bsn_bst),
            exc_pre = sum(exc_pre),
            exposure = sum(exp_std), 
            .groups = "drop") %>% 
  mutate(exc_r = 1e5*exc_std/exposure,
         exc_pre_r = 1e5*exc_pre/exposure) %>% 
  arrange(exc_r) %>% 
  mutate(rnk = 1:n())

# total age-standardized pandemic and inequality excess
exc_pre2 %>% 
  summarise(exc_std = sum(exc_std),
            exc_pre = sum(exc_pre)) %>% 
  mutate(ratio = exc_pre/exc_std) 


# all US together
exc_pre_us <- 
  exc_pre %>% 
  summarise(dx_std = sum(dx_std),
            bsn_std = sum(bsn_std),
            exc_std = sum(exc_std),
            bsn_bst = sum(bsn_bst),
            exc_pre = sum(exc_pre),
            exposure = sum(exp_std), 
            .groups = "drop") %>% 
  mutate(exc_r = 1e5*exc_std/exposure,
         exc_pre_r = 1e5*exc_pre/exposure) %>% 
  arrange(exc_std) %>% 
  mutate(rnk = 52) %>% 
  mutate(state = "US")

exc_pre3 <- 
  exc_pre2 %>% 
  bind_rows(exc_pre_us) %>% 
  arrange(rnk) %>% 
  select(state, rnk, exc_r, exc_pre_r) %>% 
  pivot_longer(c(exc_r, exc_pre_r), 
               names_to = "exc_typ", 
               values_to = "exc_r")

# ratio of pandemic to preexisting excess rates
exc_ratio <- 
  exc_pre2 %>% 
  bind_rows(exc_pre_us) %>% 
  arrange(rnk) %>% 
  # spread()
  select(state, rnk, exc_r, exc_pre_r) %>% 
  mutate(ratio = exc_pre_r / exc_r)

# average of ratios. Not same as ratio of the averages
exc_ratio %>% 
  filter(state != "US") %>% 
  summarise(ratio = mean(ratio))


pscs <- 
  exc_pre2 %>% 
  mutate(psc_pre = exc_pre / bsn_bst,
         psc_pan = exc_std / bsn_std) %>% 
  arrange(psc_pan) %>% 
  mutate(rnk = 1:n()) %>% 
  bind_rows(
    exc_pre_us %>% 
      mutate(psc_pre = exc_pre / bsn_bst,
             psc_pan = exc_std / bsn_std)
  ) %>% 
  arrange(rnk) %>% 
  select(state, rnk, psc_pan, psc_pre) %>% 
  pivot_longer(c(psc_pan, psc_pre), names_to = "exc_typ", values_to = "psc")


# quick q: for how many states was baseline shortfall same or less than pandemic excess?
exc_pre3 |> 
  pivot_wider(names_from = exc_typ, values_from = exc_r) |> 
  mutate(ratio = exc_pre_r / exc_r) |> 
  arrange(ratio)
# Note Alaska, Arizona, California all contributed at least one age group to
# the best practice standard. These are also the 6th, 8th, and 1st lowest total 
# excess states.

exc_pre3 |> 
  filter(state != "US") |> 
  pivot_wider(names_from = exc_typ, values_from = exc_r) |> 
  mutate(exc_tot_r = exc_pre_r + exc_r) |> 
  summarize(exc_pre_sd = sd(exc_pre_r),
            exc_tot_sd = sd(exc_tot_r),
            exc_pre_cv = sd(exc_pre_r)/mean(exc_pre_r),
            exc_tot_cv = sd(exc_tot_r)/mean(exc_tot_r))


tt <- 
  exc_pre3 %>% 
  spread(exc_typ, exc_r) %>% 
  mutate(ratio = exc_pre_r/exc_r)

tt %>% 
  summarise(r_av = mean(ratio))




# relative difference with the average before and during the pandemic
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
r <- 
  exc_pre2 %>% 
  select(state, dx_std, bsn_std, exc_std, exposure) %>% 
  mutate(mx = 1e5*dx_std/exposure,
         bsn_r = 1e5*bsn_std/exposure,
         exc_r = 1e5*exc_std/exposure,
         mx_av = mean(mx),
         exc_av = mean(exc_r),
         bsn_r_av = mean(bsn_r),
         rr_pre = bsn_r/bsn_r_av,
         rr_pan = mx/mx_av,
         rr_exc = exc_r/exc_av) %>% 
  left_join(pol2, by = join_by(state)) %>% 
  left_join(codes, by = join_by(state))


tt <- 
  r %>% 
  mutate(governor = if_else(pol <= 2, "#3157d4", "#db2a2a"),
         governor = if_else(state == "Nebraska", "#db2a2a", governor),
         governor = if_else(state == "District of Columbia", "#3157d4",governor),
         chamber = case_when(pol == 1 ~ "#3157d450",
                             pol == 2 ~ "#a618c950",
                             pol == 3 ~ "#a618c950",
                             pol == 4 ~ "#db2a2a50",
                             TRUE ~ "#a618c950"),
         gov = ifelse(pol <= 2, "dem","rep"),
         gov = case_when(state == "Nebraska" ~ "rep",
                         state == "District of Columbia" ~ "dem",
                         TRUE ~ gov))




# without age-standardization
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# inequality- and pandemic-relaetd excess during the pandemic
bst_pan <- 
  exc2 %>% 
  group_by(age) %>% 
  filter(bsn_r == min(bsn_r)) %>% 
  arrange(age) %>% 
  ungroup()
