rm (list = ls())
source("code/00_setup.R")

# monthly C19 deaths by state
dt <- read_tsv("data_input/dts_c19_all_ages_monthly_states_2020_2023.txt")

# Population by state by age
pop_sts <- read_csv("data_inter/pop_state_age_2013_2024.csv")

# calculating population exposures by state in person-years for all phases
pop_sts2 <- 
  pop_sts %>% 
  filter(year %in% 2020:2023) %>% 
  # adding the fraction for each year
  group_by(state, year) %>% 
  summarise(exposure = sum(pop)/12,
            .groups = "drop")


dt2 <- 
  dt %>% 
  select(state = 2,
         mth = 5,
         dts = 6) %>% 
  drop_na(state) %>% 
  mutate(dts = ifelse(dts == "Suppressed", "5", dts),
         dts = dts %>% as.double()) %>% 
  separate_wider_delim(mth, names=c("year", "mth"), delim = "/") %>% 
  mutate(date = make_date(y = year, m = mth, d = 15),
         year = year %>% as.double()) %>% 
  left_join(pop_sts, by = join_by(state, year)) %>% 
  mutate(mx = 1e5*dts/exposure)

dt2 %>% 
  ggplot()+
  geom_line(aes(date, dts, col = state))+
  theme_bw()

dt2 %>% 
  ggplot()+
  geom_line(aes(date, mx, col = state))+
  theme_bw()


polit <- read_csv("data_input/woolf_tabs3.csv")
codes <- read_csv("data_input/us_state_codes.csv")

pol2 <- 
  polit %>% 
  filter(phase %in% 1:2) %>% 
  group_by(state) %>% 
  summarise(pol = round(mean(polit)))

dt3 <- 
  dt2 %>% 
  left_join(pol2) %>% 
  mutate(governor = if_else(pol <= 2, "#3157d4", "#db2a2a"),
         governor = if_else(state == "Nebraska", "#db2a2a", governor),
         governor = if_else(state == "District of Columbia", "#3157d4",governor),
         chamber = case_when(pol == 1 ~ "#3157d450",
                             pol == 2 ~ "#a618c950",
                             pol == 3 ~ "#a618c950",
                             pol == 4 ~ "#db2a2a50",
                             TRUE ~ "#a618c950"),
         chm = case_when(pol == 1 ~ "Democrat",
                         pol == 4 ~ "Republican",
                         TRUE ~ "Mixed"),
         gov = ifelse(pol <= 2, "dem","rep"),
         gov = case_when(state == "Nebraska" ~ "rep",
                         state == "District of Columbia" ~ "dem",
                         TRUE ~ gov))

cols <- c("#3157d4", "#db2a2a")


ph1 <- ymd(c("2020-03-07", "2020-06-13"))
ph2 <- ymd(c("2020-06-20", "2021-03-13"))
ph3 <- ymd(c("2021-03-20", "2022-03-19"))
ph4 <- ymd(c("2022-03-26", "2023-02-04"))
ph5 <- ymd(c("2023-02-11", "2023-07-02"))

dt3 %>% 
  ggplot()+
  geom_line(aes(date, mx, group = state, col = gov),
            alpha = 0.5)+
  geom_vline(xintercept = c(ymd("2020-03-07"),
                            ymd("2020-06-20"),
                            ymd("2021-03-20"),
                            ymd("2022-03-26"),
                            ymd("2023-02-11"),
                            ymd("2023-07-02")),
             lty = "dashed")+
  scale_color_manual(values = cols, labels = c("Democrat",
                                               "Republican"))+
  scale_x_date(date_breaks = "3 months", date_labels = "%m/%y",
               limits = c(ymd("2020/01/01"), ymd("2023-07-02")))+
  labs(x = "Date", y = "C19 crude death rates", col = "Governor")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/c19_monthly_state.png",
       w = 8, h = 4)

cols <- c("#3157d450",
          "#a618c950",
          "#db2a2a50")

dt3 %>% 
  ggplot()+
  geom_line(aes(date, mx, group = state, col = chm),
            alpha = 0.5)+
  geom_vline(xintercept = c(ymd("2020-03-07"),
                            ymd("2020-06-20"),
                            ymd("2021-03-20"),
                            ymd("2022-03-26"),
                            ymd("2023-02-11"),
                            ymd("2023-07-02")),
             lty = "dashed")+
  scale_color_manual(values = cols)+
  scale_x_date(date_breaks = "3 months", date_labels = "%m/%y",
               limits = c(ymd("2020/01/01"), ymd("2023-07-02")))+
  labs(x = "Date", y = "C19 crude death rates", col = "Chamber")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/c19_monthly_state_chamber.png",
       w = 8, h = 4)



  




# 
# # standardized excess rates
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~
# std <- read_csv("data_inter/age_standardized_excess_rates_states.csv")
# 
# std %>% 
#   ggplot()+
#   geom_line(aes(date, exc_std, group = state, col = gov),
#             alpha = 0.6)+
#   scale_color_manual(values = cols)+
#   scale_x_date(date_breaks = "3 months", date_labels = "%m/%y",
#                limits = c(ymd("2020/01/01"), ymd("2023/12/31")))+
#   theme_bw()+
#   theme(legend.position = "bottom",
#         axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave("figures/c19_monthly_state.png",
#        w = 8, h = 4)

  