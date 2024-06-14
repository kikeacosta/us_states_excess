rm (list = ls())
source("code/00_setup.R")

# age structure of deaths in each pandemic phase ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Monthly deaths in 10-year age groups
# dtm_10a <- read_tsv("data_input/dts_agex10_monthly_states_2020_2023.txt")
dtm_10a <- read_tsv("data_input/dts_agex10_35plus_monthly_states_2020_2023.txt",
                    show_col_types = FALSE)
# Monthly deaths in ages 15+
dtm_15p <- read_tsv("data_input/dts_ages_15plus_monthly_states_2020_2023.txt",
                    show_col_types = FALSE)
# Monthly deaths in ages 25+
dtm_25p <- read_tsv("data_input/dts_ages_25plus_monthly_states_2020_2023.txt",
                    show_col_types = FALSE)
# Monthly deaths in ages 35+
dtm_35p <- read_tsv("data_input/dts_ages_35plus_monthly_states_2020_2023.txt",
                    show_col_types = FALSE)
# Monthly deaths in ages 45+
dtm_45p <- read_tsv("data_input/dts_ages_45plus_monthly_states_2020_2023.txt",
                    show_col_types = FALSE)

# Monthly deaths in all ages
dtm_all <- read_tsv("data_input/dts_ages_all_monthly_states_2020_2023.txt",
                    show_col_types = FALSE)

# Monthly deaths in all ages
dtm_knw <- read_tsv("data_input/dts_ages_all_known_monthly_states_2020_2023.txt",show_col_types = FALSE)

# Function to standardize names for these assorted input files
std_this <- 
  function(x){
    x %>% 
      select(state = `Occurrence State`, 
             year = `Year Code`,
             mth = `Month Code`,
             dx = `Deaths`) %>% 
      drop_na(state) %>% 
      mutate(mth = str_sub(mth, 6, 7) %>% as.double(),
             date = make_date(y = year, m = mth, d = 15))
  }

# standardize each of the death input files
dtm_all2 <- std_this(dtm_all) %>% rename(dx_all = dx)
dtm_knw2 <- std_this(dtm_knw) %>% rename(dx_knw = dx)
dtm_15p2 <- std_this(dtm_15p) %>% rename(dx_15p = dx)
dtm_25p2 <- std_this(dtm_25p) %>% rename(dx_25p = dx)
dtm_35p2 <- std_this(dtm_35p) %>% rename(dx_35p = dx)
dtm_45p2 <- std_this(dtm_45p) %>% rename(dx_45p = dx)

# deaths under 15; we need to infer some deaths for specific
# age groups due to potential cell suppression. This is basically
# decumulating cumulative sums.
dtm <- 
  dtm_all2 %>% 
  left_join(dtm_knw2, by = join_by(state, year, mth, date)) %>% 
  left_join(dtm_15p2, by = join_by(state, year, mth, date)) %>% 
  left_join(dtm_25p2, by = join_by(state, year, mth, date)) %>% 
  left_join(dtm_35p2, by = join_by(state, year, mth, date)) %>% 
  left_join(dtm_45p2, by = join_by(state, year, mth, date)) %>% 
  mutate(dx_unk = dx_all - dx_knw,
         # deaths under 15
         dx_15u = dx_knw - dx_15p,
         # deaths 15-24
         dx_1524 = dx_knw - dx_25p - dx_15u,
         # deaths 25-34
         dx_2534 = dx_knw - dx_35p - dx_1524 - dx_15u,
         # deaths 35-44
         dx_3544 = dx_knw - dx_45p - dx_2534 - dx_1524 - dx_15u,
  )

dtm2 <- 
  dtm %>% 
  select(state, date, dx_15u, dx_1524, dx_2534, dx_3544) %>% 
  pivot_longer(c(dx_15u, dx_1524, dx_2534, dx_3544), names_to = "age", values_to = "dx") |> 
  mutate(age = case_when(age == "dx_15u" ~ 0,
                         age == "dx_1524" ~ 15,
                         age == "dx_2534" ~ 25,
                         age == "dx_3544" ~ 35))
  
# deaths in 10-year ages 45+
dtm_10a2 <- 
  dtm_10a %>% 
  select(state = `Occurrence State`, 
         year = `Year Code`,
         mth = `Month Code`,
         age = `Ten-Year Age Groups Code`,
         dx = Deaths) %>% 
  drop_na(state) %>% 
  mutate(mth = str_sub(mth, 6, 7) %>% as.double(),
         date = make_date(y = year, m = mth, d = 15)) %>% 
  separate(age, c("age", "trash"), sep = "-") %>% 
  mutate(age = ifelse(age == "85+", "85", age),
         age = age %>% as.double(),
         dx = dx %>% as.double()) %>% 
  select(-trash, -year, -mth) %>% 
  filter(age >= 45)

unique(dtm2$age)
unique(dtm_10a2$age)


# putting all ages together
dtm3 <- 
  bind_rows(dtm2,
            dtm_10a2) %>% 
  arrange(state, date, age) %>% 
  left_join(dtm_all2, by = join_by(state, date)) %>% 
  group_by(state, date, year, mth) %>% 
  mutate(dx = dx_all/sum(dx)*dx)



# grouping deaths by pandemic phase ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Actual dates in each phase: between weeks ending in: 
ph1 <- ymd(c("2020-03-07", "2020-06-13"))
ph2 <- ymd(c("2020-06-20", "2021-03-13"))
ph3 <- ymd(c("2021-03-20", "2022-03-19"))
ph4 <- ymd(c("2022-03-26", "2023-02-04"))
ph5 <- ymd(c("2023-02-11", "2023-07-02"))

# defining "equivalent" month periods for each phase (no overlapping)
mts_ph1 <- seq(ymd("2020-03-15"), ymd("2020-06-15"), "months")
mts_ph2 <- seq(ymd("2020-07-15"), ymd("2021-03-15"), "months")
mts_ph3 <- seq(ymd("2021-04-15"), ymd("2022-03-15"), "months")
mts_ph4 <- seq(ymd("2022-04-15"), ymd("2023-01-15"), "months")
mts_ph5 <- seq(ymd("2023-02-15"), ymd("2023-06-15"), "months")

dts_ph <- 
  dtm3 %>% 
  mutate(phase = case_when(date %in% mts_ph1 ~ 1,
                           date %in% mts_ph2 ~ 2,
                           date %in% mts_ph3 ~ 3,
                           date %in% mts_ph4 ~ 4,
                           date %in% mts_ph5 ~ 5,
                           TRUE ~ 9)) %>% 
  filter(phase %in% 1:5) %>% 
  group_by(phase, state, age) %>% 
  summarise(dx = sum(dx),
            dx_all = sum(dx_all),
            .groups = "drop") 


# excess mortality ====
# ~~~~~~~~~~~~~~~~~~~~~

# excess crude death rates by state
exc <- read_csv("data_input/excess_estimates_by_state_and_phase.csv",
                show_col_types = FALSE)

exc2 <- 
  exc %>% 
  select(state, ph1, ph2, ph3, ph4, ph5) %>% 
  pivot_longer(-state, names_to = "phase", values_to = "cedr") %>% 
  mutate(phase = str_remove(phase, "ph"),
         year_pop = case_when(phase == 1 ~ 2020,
                              phase == 2 ~ 2020,
                              phase == 3 ~ 2021,
                              phase == 4 ~ 2022,
                              phase == 5 ~ 2023))


# population
pop <- read_tsv("data_input/pop_agex5_states_2013_2024.txt",
                show_col_types = FALSE)

unique(pop$State)

pop_age <- 
  pop %>% 
  select(state = State, 
         year = `Year Code`,
         sex = `Gender`,
         age = `Age Group Code`,
         pop = `Projected Populations`) %>% 
  drop_na(state) %>% 
  separate(age, c("age", "trash"), sep = "-") %>% 
  select(-trash) %>% 
  mutate(age = ifelse(age == "85+", "85", age),
         age = age %>% as.double(),
         age = case_when(age < 15 ~ 0,
                         age %in% 15:24 ~ 15,
                         age %in% 25:34 ~ 25,
                         age %in% 35:44 ~ 35,
                         age %in% 45:54 ~ 45,
                         age %in% 55:64 ~ 55,
                         age %in% 65:74 ~ 65,
                         age %in% 75:84 ~ 75,
                         age == 85 ~ 85)) %>% 
  group_by(state, year, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

write_csv(pop_age, "data_inter/pop_state_age_2013_2024.csv")

# Derive total state pop
# This is needed to back out excess deaths expressed as counts
pop_tot <- 
  pop_age %>% 
  group_by(state, year) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

# USA total population
pop_us <- 
  pop_tot %>% 
  group_by(year) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

# combine the Woolf data (stated as excess / 100k)
# and convert to counts using imported state populations
exc3 <- 
  exc2 %>% 
  left_join(pop_tot %>% rename(year_pop = year)) %>% 
  mutate(exc_tot = cedr*pop/1e5,
         phase = phase %>% as.double())

# Select just needed columns; here we have excess deaths by state and phase
exc4 <- 
  exc3 %>% 
  select(state, phase, exc_tot) %>% 
  arrange(phase, state)
  

# total baseline ====
# ~~~~~~~~~~~~~~~~~~~

# This is observed deaths (CDC wonder) minus the Woolf excess estimate
dts_ph2 <- 
  dts_ph %>% 
  left_join(exc4, by = join_by(phase, state)) %>% 
  mutate(bsn_tot = dx_all - exc_tot)

# 

# Age structure of the baseline ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Annual data 2015-2020
# deaths in 10-year age groups
dt10a <- read_tsv("data_input/dts_agex10_annual_states_2015_2020.txt",
                  show_col_types = FALSE)
# under 15 mortality as one group
dtu15a <- read_tsv("data_input/dts_ageU15_annual_states_2015_2020.txt",
                   show_col_types = FALSE)

# standardize column names (risky to rename based on position,
# but these files are regular)
dt10a2 <- 
  dt10a %>% 
  select(state = State, 
         year = `Year Code`,
         age = `Ten-Year Age Groups Code`,
         dx = Deaths) %>% 
  drop_na(state) 

# tst <- 
#   dt10a2 %>% 
#   filter(dx == "Suppressed")

# all suppressed values in ages under-15, or unknown age
# we do not need to impute unknown ages because we would use the same 
# age structure we observe here

dtu15a2 <- 
  dtu15a %>% 
  select(state = State, 
         year = `Year Code`,
         dx = Deaths) %>% 
  drop_na(state) %>% 
  mutate(age = "0")

# average age structure of annual deaths by state in 2017-2019
# ages 0-14, 15-24, 25-34, ..., 85+
# The result, cx_bsn is the fraction of deaths in each age 

dta <- 
  dt10a2 %>% 
  filter(!age %in% c("1", "1-4", "5-14", "NS")) %>% 
  mutate(dx = dx %>% as.double()) %>% 
  bind_rows(dtu15a2) %>% 
  arrange(state, year, age) %>% 
  filter(year %in% 2017:2019) %>% 
  group_by(state, age) %>% 
  # sum deaths by state and age in 3-year pre-pandemic period
  summarise(dx = sum(dx)) %>% 
  group_by(state) %>% 
  mutate(cx_bsn = dx/sum(dx)) %>% 
  ungroup() %>% 
  select(-dx) %>% 
  separate(age, c("age", NA)) %>% 
  mutate(age = age %>% as.double()) 

# What do these age patterns of deaths look like?
dta |>
  ggplot(aes(x = age, y = cx_bsn, group = state)) +
  geom_line(alpha = .5) +
  theme_minimal()


# Join inferred state total baseline deaths
# with pre-pandemic baseline age patterns, to
# derive expected deaths by age- difference
# from observed deaths (dx) gives implied 
# excess by age
dts_ph3 <- 
  dts_ph2 %>% 
  left_join(dta,
            by = join_by(state, age)) %>% 
  mutate(bsn = bsn_tot * cx_bsn,
         exc = dx - bsn)

# check sums; teeny tiny residuals, a matter of
# machine precision.
dts_ph3 %>% 
  group_by(phase, state) %>% 
  summarise(exc2 = sum(exc), .groups = 'drop') |> 
  left_join(exc4, by = join_by(state,phase)) |> 
  mutate(check = exc2 - exc_tot) |> 
  pull(check) 


write_csv(dts_ph3, "data_inter/excess_state_phase_age.csv")

# end

