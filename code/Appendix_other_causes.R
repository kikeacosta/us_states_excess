rm (list = ls())
source("code/00_setup.R")

exc <- read_csv("data_inter/excess_rates_by_age_phases_1_4.csv")
# exc <- read_csv("data_inter/excess_rates_by_age_all_period.csv")
dat <- read_tsv("data_input/dts_ages_all_by_cause_annual_states_2018_2023.txt",
                show_col_types = FALSE) 
problems(dat)
dt_cs_p1 <- read_tsv("data_input/dts_ages_all_by_cause_annual_states_2018_2023.txt",
                     show_col_types = FALSE) %>% 
  select(state = `Residence State`, 
         year = `Year Code`,
         cause = `MCD - ICD Chapter`,
         dts = Deaths,
         pop = Population ) %>% 
  filter(year %in% 2020:2023)

dt_cs_p2 <- read_tsv("data_input/dts_ages_all_by_cause_annual_states_2015_2019.txt",
                     show_col_types = FALSE) %>% 
  select(state = State   , 
         year = `Year Code`,
         cause = `MCD - ICD Chapter`,
         dts = Deaths,
         pop = Population ) %>% 
  filter(year %in% 2015:2019)

dt_cs <- 
  bind_rows(dt_cs_p1,
            dt_cs_p2)

tt <- 
  dt_cs %>% 
  mutate(dts = dts %>% as.double()) %>% 
  replace_na(list(dts = 0)) %>% 
  group_by(cause) %>% 
  summarise(dts = mean(dts)) %>% 
  mutate(prop = dts/sum(dts)) %>% 
  arrange(prop)

css <- unique(tt$cause)
css[1:10]

dt <- 
  dt_cs %>% 
  filter(!cause %in% css[1:10]) %>% 
  mutate(dts = dts %>% as.double()) %>% 
  mutate(cause = case_when(cause %in% css[14:15] ~ "external",
                           TRUE ~ cause)) %>% 
  group_by(year, state, cause) %>% 
  summarise(dts = sum(dts),
            pop = mean(pop),
            .groups = "drop") %>% 
  mutate(cdr = 1e5*dts/pop)

# average death rates by cause for the period 2017-2019
av_r <- 
  dt %>% 
  filter(year %in% 2017:2019) %>% 
  group_by(state, cause) %>% 
  summarise(cdr = mean(cdr),
            .groups = "drop")
  
# excess death rates for all ages together
exc2 <- 
  exc %>% 
  group_by(state) %>% 
  summarise(dts = sum(dx),
            bsn = sum(bsn),
            exc = sum(exc),
            exposure = sum(exposure)) %>% 
  mutate(exc_r = 1e5 * exc / exposure)

av_r2 <- 
  av_r %>% 
  bind_rows(exc2 %>% 
              mutate(cause = "Excess C19") %>% 
              select(state, cause, cdr = exc_r)) %>% 
  group_by(state) %>% 
  arrange(state, -cdr,
          .groups = "drop") %>% 
  mutate(ord = 1:n())


c19 <- 
  av_r2 %>% 
  filter(cause == "Excess C19")

c19 %>% 
  group_by() %>% 
  summarise(ord_av = mean(ord))



dt_all <- read_tsv("data_input/dts_ages_all_cause_all_annual_states_2018_2023.txt")

dt_all2 <- 
  dt_all %>% 
  select(state = `Residence State`,
         year = `Year Code`,
         dts_all = Deaths, 
         pop = Population) %>% 
  filter(year %in% 2018:2023)



tt <- 
  dt2 %>% 
  mutate(dts = dts %>% as.double()) %>% 
  replace_na(list(dts = 0)) %>% 
  group_by(cause) %>% 
  summarise(dts = mean(dts)) %>% 
  mutate(prop = dts/sum(dts)) %>% 
  arrange(prop)







  
  
  