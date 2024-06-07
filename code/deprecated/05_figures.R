cmp |> 
  pivot_longer(3:4, names_to = "variant", values_to = "rank") |> 
  ggplot(aes(x = variant, y = rank, group = state)) +
  geom_line() +
  facet_wrap(~phase, ncol = 5)  + 
  theme_minimal()

ggsave("figures/rank_change_age_stand.png",
       w = 8, h = 3)



polit <- read_csv("data_input/woolf_tabs3.csv")

codes <- read_csv("data_input/us_state_codes.csv")
p <-
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
                  size = 10) +
  geom_point(aes(color = governor, fill = chamber), 
             size = 10, 
             pch = 21) +
  geom_text(aes(label = state_code)) +
    scale_color_identity() +
    scale_fill_identity()

ggsave("fig1.pdf",plot=p,height=20,width=10)

# a monotone simple version of the same
std2 |> 
  ggplot(aes(x = phase, y = rnk_std, group = state)) +
  geom_line() +
  theme_minimal()

cmp |> 
  pivot_longer(rnk_std:rnk_raw, names_to = "variant", values_to = "rank") |> 
  mutate(variant = if_else(variant == "rnk_raw", "crude","stand.")) |> 
  ggplot(aes(x = variant, y = rank, group = state)) +
  geom_line() +
  facet_grid(cols = vars(phase)) +
  theme_minimal() +
  xlab("") +
  annotate("text",0,5,label="lower excess",angle=-90)



px = cx + r * cos(angle)
py = cy + r * sin(angle)

draw_circle <- function(x,y,r,n=500, fill, color, lwd = 2){
  rads <- ((1:n)/n) * (2 * pi)
  x_out <- cos(rads) * r + x
  y_out <- sin(rads) * r + y
  polygon(x_out,y_out, col = fill, border = NA)
  lines(x_out, y_out, col = color, lwd = 1)
}
# https://stackoverflow.com/questions/32046889/connecting-two-points-with-curved-lines-s-ish-curve-in-r
connect_circles <- function(x1, y1, x2, y2,scale=.2,...){
  curve( plogis( x, scale = scale, loc = (x1 + x2) /2 ) * (y2-y1) + y1, 
         x1, x2, add = TRUE, ...)
}
connect_circles_wrapper <- function(x1, y1, x2, y2,r=.5,scale=.2,...){
  x1 <- x1 + r
  x2 <- x2 - r
  curve( plogis( x, scale = scale, loc = (x1 + x2) /2 ) * (y2-y1) + y1, 
         x1, x2, add = TRUE, ...)
}

connect_circles_segment <- function(x1, y1, x2, y2,r=.5,scale=.2,...){
  x1 <- x1 + r
  x2 <- x2 - r
  segments(x0=x1,y0=y1,x1=x2,y1=y2,...)
}
plot_data <- 
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
  arrange(state, phase)

plot(NULL, xlim = c(0,61),ylim=c(0,53), axes=FALSE,xlab="",ylab="",asp=1,xaxs="i",yaxs="i",mar=c(0,0,0,0))

for (s in unique(plot_data$state_code)){
  state_chunk <- plot_data |> 
    filter(state_code == s)
  
  for (p in 1:5){
    draw_circle(x = p*10, 
                y = state_chunk$rnk_std[p],
                r = .5,
                fill = state_chunk$chamber,
                color = state_chunk$governor,
                lwd=2)
  }
  for (p in 1:4){
    # connect_circles_wrapper(x1 = p*10,
    #                         y1 = state_chunk$rnk_std[p],
    #                         x2 = (p+1)*10,
    #                         y2 = state_chunk$rnk_std[p+1],
    #                         r = .4,
    #                         scale = .7)
    connect_circles_segment(x1 = p*10,
                            y1 = state_chunk$rnk_std[p],
                            x2 = (p+1)*10,
                            y2 = state_chunk$rnk_std[p+1],
                            r = .5,
                            col = state_chunk$governor[p])
  }
}
