rm (list = ls())
source("code/00_setup.R")

# adjusting rates denominators for all 5 phases
# it includes a fraction of 2020
ph2020 <- ymd(c("2020-03-07", "2020-12-31"))
# all years 2021-2022
# and a fraction of 2023
ph2023 <- ymd(c("2023-01-01", "2023-07-02"))

# fraction of years to estimate person-years
fr2020 <- interval(ph2020[1], ph2020[2]) %>% as.numeric('years')
fr2023 <- interval(ph2023[1], ph2023[2]) %>% as.numeric('years')

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
  filter(year %in% 2020:2023) %>% 
  # adding the fraction for each year
  mutate(fr = case_when(year == 2020 ~ fr2020,
                        year == 2023 ~ fr2023,
                        TRUE ~ 1)) %>% 
  # calculating exposures for the whole period 2020-03-07 - 2023-07-02
  group_by(age) %>% 
  summarise(exposure = sum(pop * fr),.groups = "drop")


# Population by state by age
pop_sts <- read_csv("data_inter/pop_state_age_2013_2024.csv")

# calculating population exposures by state in person-years for all phases
pop_sts2 <- 
  pop_sts %>% 
  filter(year %in% 2020:2023) %>% 
  mutate(fr = case_when(year == 2020 ~ fr2020,
                        year == 2023 ~ fr2023,
                        TRUE ~ 1)) %>% 
  # calculating exposures for the whole period 2020-03-07 - 2023-07-02
  group_by(state, age) %>% 
  summarise(exposure = sum(pop * fr),.groups = "drop")





# excess estimates by state and age
exc <- read_csv("data_inter/excess_state_phase_age.csv")

# merging excess deaths and exposures (by age)
exc2 <- 
  exc %>% 
  group_by(state, age) %>% 
  summarise(dx = sum(dx),
            bsn = sum(bsn),
            exc = sum(exc)) %>% 
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

write_csv(exc2, "data_inter/excess_rates_by_age_all_period.csv")

# age-specific excess deaths according to standard population
std <- 
  exc2 %>% 
  select(state, age, exc_r, bsn_r, mx) %>% 
  left_join(pop_us2, by = join_by(age)) %>% 
  # remember to undo the 100k units of rates
  mutate(exc_std = exc_r * exposure / 1e5,
         bsn_std = bsn_r * exposure / 1e5,
         dx_std = mx * exposure / 1e5,
  )

std2 <- 
  std %>% 
  # calculate age-standardized obs death, baseline, and excess rates
  group_by(state) %>% 
  summarise(exc_std = sum(exc_std),
            bsn_std = sum(bsn_std),
            dx_std = sum(dx_std),
            exposure = sum(exposure), 
            .groups = "drop") %>% 
  # once again age-standardized excess rate scaled up to pre 100k
  # TR: why / pop?
  # KA: no adjusted for exposure during the whole observation period (~3.3 years)
  mutate(exc_r_std = 1e5 * exc_std / exposure,
         bsn_r_std = 1e5 * bsn_std / exposure,
         mx_std = 1e5 * dx_std / exposure,
         psc = exc_std / bsn_std) %>% 
  arrange(-exc_r_std) %>% 
  mutate(rnk_exc_std = 1:n()) %>% 
  arrange(-psc) %>% 
  mutate(rnk_psc_std = 1:n()) %>% 
  arrange(rnk_exc_std)

std2 |> 
  ggplot(aes(x = psc, y = state)) +
  geom_point() +
  theme_minimal()


# looking at raw ranking
# ~~~~~~~~~~~~~~~~~~~~~~

# adding all phases together for accumulating Wolff's estimates
exc_wlf <- 
  exc %>% 
  left_join(pop_sts2, by = join_by(state, age)) %>% 
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
            join_by(state))


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
  mutate(bsn_bst = bsn_bst_r * exposure / 1e5,
         exc_pre_r = bsn_r - bsn_bst_r,
         exc_pre = exc_pre_r * exposure / 1e5)

# all ages together
exc_pre2 <- 
  exc_pre %>% 
  group_by(state) %>% 
  summarise(dx_std = sum(dx_std),
            bsn_std = sum(bsn_std),
            exc_std = sum(exc_std),
            bsn_bst = sum(bsn_bst),
            exc_pre = sum(exc_pre),
            exposure = sum(exposure), 
            .groups = "drop") %>% 
  mutate(exc_r = 1e5*exc_std/exposure,
         exc_pre_r = 1e5*exc_pre/exposure)


exc_pre3 <- 
  exc_pre2 %>% 
  select(state, exc_r, exc_pre_r) %>% 
  gather(-state, key = exc_typ, value = exc_r)


ggplot(exc_pre3, aes(fill=exc_typ, y=exc_r, x=state)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip()+
  theme_bw()

ggsave("figures/excess_prepandemic_pandemic.png",
       w = 7,
       h = 10)
