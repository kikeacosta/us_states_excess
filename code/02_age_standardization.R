rm (list = ls())
source("code/00_setup.R")

# phases for adjusting rates denominators
ph1 <- ymd(c("2020-03-07", "2020-06-13"))
ph2 <- ymd(c("2020-06-20", "2021-03-13"))
ph3 <- ymd(c("2021-03-20", "2022-03-19"))
ph4 <- ymd(c("2022-03-26", "2023-02-04"))
ph5 <- ymd(c("2023-02-11", "2023-07-02"))

# fraction of years to estimate person-years
d1 <- interval(ph1[1], ph1[2]) %>% as.numeric('years')
d2 <- interval(ph2[1], ph2[2]) %>% as.numeric('years')
d3 <- interval(ph3[1], ph3[2]) %>% as.numeric('years')
d4 <- interval(ph4[1], ph4[2]) %>% as.numeric('years')
d5 <- interval(ph5[1], ph5[2]) %>% as.numeric('years')

# US population by age
# will be used as the reference population for standardization 
pop_us <- read_tsv("data_input/pop_us_agex5_2015_2023.txt")

pop_us2 <- 
  pop_us %>% 
  select(year = 3,
         age = 5,
         pop = 6) %>% 
  mutate(age = parse_number(age),
         age = if_else(age < 15,0,age)) %>% 
  filter(year == 2020) %>% 
  group_by(age) %>% 
  summarise(pop = sum(pop),.groups = "drop")

# Population by state by age
pop_sts <- read_csv("data_inter/pop_state_age_2013_2024.csv")

# selecting population exposures in person-years for each phase
pop_sts2 <- 
  pop_sts %>% 
  filter(year %in% 2020:2023) %>% 
  rename(year_pop = year)

# calculating population exposures in person-years for all phases
pop_sts2 <- 
  pop_sts %>% 
  filter(year %in% 2020:2023) %>% 
  rename(year_pop = year)





# excess estimates by state and age
exc <- read_csv("data_inter/excess_state_phase_age.csv")

# merging excess deaths and exposures (by age)
exc2 <- 
  exc %>% 
  mutate(year_pop = case_when(phase == 1 ~ 2020,
                              phase == 2 ~ 2020,
                              phase == 3 ~ 2021,
                              phase == 4 ~ 2022,
                              phase == 5 ~ 2023)) %>% 
  left_join(pop_sts2, 
            by = join_by(state, age, year_pop)) %>% 
  # this is an approximation, taking info from just 1 pop estimate,
  # can be refined
  mutate(yr_fr = case_when(phase == 1 ~ d1,
                           phase == 2 ~ d2,
                           phase == 3 ~ d3,
                           phase == 4 ~ d4,
                           phase == 5 ~ d5,
                           TRUE ~ 0),
         exposure = pop * yr_fr,
         # excess rates (exc_r) expressed as per 100k, by age
         exc_r = 1e5 * exc / exposure,
         # baseline rates (bsn_r) expressed as per 100k, by age
         bsn_r = 1e5 * bsn / exposure,
         # death rates (mx) expressed as per 100k, by age
         mx = 1e5 * dx / exposure
  )

# age-specific excess deaths according to standard population
std <- 
  exc2 %>% 
  select(phase, state, age, exc_r, bsn_r, mx) %>% 
  left_join(pop_us2, by = join_by(age)) %>% 
  # remember to undo the 100k units of rates
  mutate(exc_std = exc_r * pop / 1e5,
         bsn_std = bsn_r * pop / 1e5,
         dx_std = mx * pop / 1e5,
  )

std2 <- 
  std %>% 
  # calculate age-standardized obs death, baseline, and excess rates
  group_by(phase, state) %>% 
  summarise(exc_std = sum(exc_std),
            bsn_std = sum(bsn_std),
            dx_std = sum(dx_std),
            pop = sum(pop), 
            .groups = "drop") %>% 
  # once again age-standardized excess rate scaled up to pre 100k
  # TR: why / pop?
  mutate(exc_r_std = 1e5 * exc_std / pop,
         bsn_r_std = 1e5 * bsn_std / pop,
         mx_std = 1e5 * dx_std / pop,
         psc = exc_std / bsn_std) %>% 
  group_by(phase) %>% 
  arrange(-exc_r_std) %>% 
  mutate(rnk_std = 1:n()) %>% 
  arrange(phase, rnk_std)

exc_wlf <- read_csv("data_input/excess_estimates_by_state_and_phase.csv")

exc_wlf2 <- 
  exc_wlf %>% 
  select(state, ph1, ph2, ph3, ph4, ph5) %>% 
  pivot_longer(-state, names_to = "phase", values_to = "cedr") %>% 
  mutate(phase = str_remove(phase, "ph"),
         phase = phase %>% as.double()) %>% 
  group_by(phase) %>% 
  arrange(-cedr) %>% 
  mutate(rnk_raw = 1:n()) %>% 
  arrange(phase, rnk_raw)

# comparing ranks
cmp <- 
  std2 %>% 
  select(phase, state, rnk_std) %>% 
  left_join(exc_wlf2 %>% 
              select(phase, state, rnk_raw),
            join_by(phase, state))

write_csv(cmp, "data_inter/preliminary_ranks_raw_std.csv")
cmp <- read_csv("data_inter/preliminary_ranks_raw_std.csv")

# average rank change when adjusting for age
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tt <- cmp |>
  select(-rnk_raw) %>% 
  pivot_wider(names_from = phase, values_from = rnk_std) %>% 
  select(-`5`) |>
  mutate(ch1 = abs(`1` - `2`),
         ch2 = abs(`2` - `3`),
         ch3 = abs(`3` - `4`))

# TR: seems OK to me
tt |>
  summarise(ch1 = mean(ch1),
            ch2 = mean(ch2),
            ch3 = mean(ch3),
  )

