library(ggdist)
library(ggtext)
library(colorspace)
library(tidyverse)
library(pokemon)
library(magick)
url <- "https://e7.pngegg.com/pngimages/296/68/png-clipart-pokemon-pokemon.png"
img <- image_transparent(img, "white", fuzz = 10)
pic <- grid::rasterGrob(img, interpolate = TRUE)

data(pokemon)

df <- pokemon |>  
  filter(weight >= 1) |>
  mutate(weight_speed = log(weight) / log(speed))

most_common <- df |
  group_by(type_1) |>
  tally() |>
  arrange(desc(n)) |>
  slice(1:10)
most_common

type_order <- df |>  
  group_by(type_1, color_1) |>
  summarize(weight_speed = mean(weight_speed, na.rm=T)) |>
  arrange(weight_speed) |>
  ungroup() |>
  filter(type_1 %in% most_common$type_1)

type_order

df |>  
  filter(type_1 %in% most_common$type_1, 
         !is.na(weight_speed) & weight_speed < 2.5) |>
  mutate(type_1 = factor(type_1, 
                        levels = type_order$type_1)) |>
  ggplot(aes(x=type_1, 
             y=weight_speed, 
             color=type_1, 
             fill=type_1)) + 
  ggdist::stat_halfeye(
    adjust = 0.5,
    width = 0.9, 
    .width = c(.5, .95), 
    alpha=0.5
  ) + 
  ggdist::stat_dots(
    side = "left", 
    justification = 1.05
  ) + 
  coord_flip() +
  theme_classic() +
  labs(x='', 
       y='\nweight vs. speed') +
  scale_y_continuous(#expand=c(0.01, 0), 
                     breaks = c(0:2), 
                     # limits = c(0, 2.5), 
                     labels = c('faster speed', 
                                'speed = weight', 
                                'larger weight')) + 
  theme(legend.position = 'none', 
        plot.background = element_rect('#FCF6F2'), 
        panel.background = element_rect('#FCF6F2'), 
        axis.text = element_text(size=12),
        axis.title = element_text(size=14,
                                  face = 'bold'),
        plot.margin = margin(t = 20,  # Top margin
                             r = 50,  # Right margin
                             b = 40,  # Bottom margin
                             l = 10)) + 
  scale_fill_manual(values = as.character(type_order$color_1)) +
  scale_color_manual(values = as.character(type_order$color_1)) + 
  annotation_custom(pic, ymin = -0.73, ymax = 0.5 , xmin = 9.9, xmax = 10.9) +
  annotation_custom(pic, ymin = 1.5, ymax = 2.5, xmin = .1, xmax = 1) -> p

ggsave('pokemon.png', p, 
       units='cm', 
       width = 15, 
       height = 20)
  
