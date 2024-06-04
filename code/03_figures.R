cmp |> 
  pivot_longer(3:4, names_to = "variant", values_to = "rank") |> 
  ggplot(aes(x = variant, y = rank, group = state)) +
  geom_line() +
  facet_wrap(~phase)  + 
  theme_minimal()


polit <- read_csv("data_input/woolf_tabs3.csv")

codes <- read_csv("data_input/us_state_codes.csv")

  cmp |> 
  full_join(codes, by = join_by(state)) |> 
  full_join(polit, by = join_by(state,phase)) |> 
    filter(!is.na(phase)) |> 
  mutate(governor = if_else(polit <= 2, "#3157d4","#db2a2a"),
         governor = if_else(state == "Nebraska", "#db2a2a",governor),
         governor = if_else(state == "District of Columbia", "#3157d4",governor),
         chamber = case_when(polit == 1 ~ "#3157d450",
                             polit == 2 ~ "#a618c950",
                             polit == 3 ~ "#a618c950",
                             polit == 4 ~ "#db2a2a50",
                             TRUE ~ "#a618c950")) |> 
  ggplot(aes(x = phase, y = rnk_std-1, group = state)) +
  geom_line(alpha = .5) +
  theme_minimal() +
    geom_point(color = "white",
                  size = 5) +
  geom_point(aes(color = governor, fill = chamber), 
             size = 6, 
             pch = 21) +
    scale_color_identity() +
    scale_fill_identity()

  
  ggsave("figures/test1.png",
         w = 6, h = 10)
  
  
  
  tt <- 
    cmp |> 
    full_join(codes, by = join_by(state)) |> 
    full_join(polit, by = join_by(state,phase)) |> 
    filter(!is.na(phase)) |> 
    mutate(governor = if_else(polit <= 2, "#3157d4","#db2a2a"),
           governor = if_else(state == "Nebraska", "#db2a2a",governor),
           governor = if_else(state == "District of Columbia", "#3157d4",governor),
           chamber = case_when(polit == 1 ~ "#3157d450",
                               polit == 2 ~ "#a618c950",
                               polit == 3 ~ "#a618c950",
                               polit == 4 ~ "#db2a2a50",
                               TRUE ~ "#a618c950"))
  
  
  
  library(ggbump)
  
  tt |> 
    ggplot(aes(x = phase, y = rnk_std-1, group = state)) +
    geom_bump(alpha = .5) +
    theme_minimal() +
    geom_point(color = "white",
               size = 5) +
    geom_point(aes(color = governor, fill = chamber), 
               size = 6, 
               pch = 21) +
    scale_color_identity() +
    scale_fill_identity()
  
  
  ggsave("figures/test2.png",
         w = 6, h = 10)
  