library(tidyverse) # general manipulation
library(lubridate) # date manipulation
library(gganimate) # animated charts
library(hrbrthemes)
library(gifski)

sd_df <- readxl::read_excel("data/solicitudes_seguro_desempleo.xlsx") %>% 
  select(date, Total)

# # static data range
# historical_sd <- sd_df %>% 
#   filter(date <= "2020-02-01") %>%
#   rename(date_2 = date)
# 
# pandemic_spike <- sd_df %>%
#   filter(date >= "2020-02-01")

historical_sd <- sd_df 
  
plot <- ggplot(historical_sd, aes(x = date, y = Total)) +
  geom_line(size = 0.5, colour = "#abcdef") +
  geom_path(data = historical_sd, aes(x = date, y = Total, group = 1),  colour = "#abcdef") + # definte extra data set to make it static
  scale_y_continuous(breaks = c(10000, 20000, 40000, 80000),
                     labels = scales::number_format(big.mark = ".")) +
  labs(title = "Récord en solicitudes de seguro de desempleo en Uruguay",
       x = "",
       y = "Solicitudes mensuales",
       caption = "Fuente: elaboración propia en base a datos del BPS.
                  Paula Pereda - @paubgood") +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour = "grey95"),
        axis.title.y = element_text(size = 8, vjust = 3),
        axis.text.y = element_text(size = 8),
        axis.line.x = element_line(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
        plot.caption = element_text(colour = "grey50")) +
  transition_reveal(date) +
  view_follow(fixed_y = F, fixed_x = T) # show all the x-axis and let the y grow

options(gganimate.dev_args = list(height = 4, width = 4*1.777778, units = 'in', type = "cairo", res = 144))

animate(plot = plot, 
        renderer = gifski_renderer("output/prueba.gif"),
        fps = 20, duration = 12, end_pause = 220)

