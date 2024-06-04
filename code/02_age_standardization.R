rm (list = ls())
source("Code/00_setup.R")

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
  mutate(age = case_when(age == 1 ~ "0", 
                         age == "85+" ~ "85", 
                         TRUE ~ age)) %>% 
  separate(age, c("age", "trash"), sep = "-") %>% 
  mutate(age = age %>% as.double(),
         age = case_when(age < 15 ~ 0,
                         age %in% 15:24 ~ 15,
                         age %in% 25:34 ~ 25,
                         age %in% 35:44 ~ 35,
                         age %in% 45:54 ~ 45,
                         age %in% 55:64 ~ 55,
                         age %in% 65:74 ~ 65,
                         age %in% 75:84 ~ 75,
                         age == 85 ~ 85)) %>% 
  filter(year == 2020) %>% 
  group_by(age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

# Population by state by age
pop_sts <- read_csv("data_inter/pop_state_age_2013_2024.csv")

# selecting population exposures in person-years for each phase
pop_sts2 <- 
  pop_sts %>% 
  filter(year %in% 2020:2023) %>% 
  rename(year_pop = year)

# excess estimates by state and age
exc <- read_csv("data_inter/excess_state_phase_age.csv")

# merging excess and exposures
exc2 <- 
  exc %>% 
  mutate(year_pop = case_when(phase == 1 ~ 2020,
                              phase == 2 ~ 2020,
                              phase == 3 ~ 2021,
                              phase == 4 ~ 2022,
                              phase == 5 ~ 2023)) %>% 
  left_join(pop_sts2) %>% 
  mutate(yr_fr = case_when(phase == 1 ~ d1,
                           phase == 2 ~ d2,
                           phase == 3 ~ d3,
                           phase == 4 ~ d4,
                           phase == 5 ~ d5,
                           TRUE ~ 0),
         exposure = pop * yr_fr,
         exc_r = 1e5*exc/exposure)

std <- 
  exc2 %>% 
  select(phase, state, age, exc_r) %>% 
  left_join(pop_us2) %>% 
  mutate(exc_std = exc_r*pop/1e5)

std2 <- 
  std %>% 
  group_by(phase, state) %>% 
  summarise(exc_std = sum(exc_std),
            pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(exc_std_r = 1e5*exc_std/pop) %>% 
  group_by(phase) %>% 
  arrange(-exc_std_r) %>% 
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
              select(phase, state, rnk_raw))


write_csv(cmp, "data_inter/preliminary_ranks_raw_std.csv")
