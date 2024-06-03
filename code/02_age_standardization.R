rm (list = ls())
source("Code/00_setup.R")

pop_us <- read_tsv("data_input/pop_us_agex5_2015_2023.txt")

pop_sts <- read_rds("data_inter/pop_state_age_2013_2024.rds")

dt <- read_rds("data_inter/excess_state_phase_age.rds")

dt2 <- 
  dt %>% 
  mutate(exc_r = exc)

# phases for adjusting rates denominators
ph1 <- ymd(c("2020-03-07", "2020-06-13"))
ph2 <- ymd(c("2020-06-20", "2021-03-13"))
ph3 <- ymd(c("2021-03-20", "2022-03-19"))
ph4 <- ymd(c("2022-03-26", "2023-02-04"))
ph5 <- ymd(c("2023-02-11", "2023-07-02"))

# fraction of years
d1 <- interval(ph1[1], ph1[2]) %>% as.numeric('years')
d2 <- interval(ph2[1], ph2[2]) %>% as.numeric('years')
d3 <- interval(ph3[1], ph3[2]) %>% as.numeric('years')
d4 <- interval(ph4[1], ph4[2]) %>% as.numeric('years')
d5 <- interval(ph5[1], ph5[2]) %>% as.numeric('years')
