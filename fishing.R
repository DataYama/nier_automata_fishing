rm(list=ls())
library(tidyverse)
library(tidytext)
library(tidymodels)
library(readxl)
library(ggdark)
library(glue)
library(cowplot)
library(magick)



# import data >>> fish_raw -------------------------------------------------------------

data_files <- list.files("./data/")

fish_raw <- list()

for (file in seq_along(data_files)) {
  
  print(glue("./data/{data_files[file]}"))
  
  fish_raw[[file]] <- read_excel(glue("./data/{data_files[file]}"))
  
}

fish_raw <- bind_rows(fish_raw)

rm(data_files, file)

# fish_raw >>> reshape data >>> fish_reshaped ------------------------------------------------------------


fish_reshaped <- fish_raw %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  pivot_longer(names_to = "species", values_to = "count", cols = c(everything(), -biome, -contributor))



# bootstrap ---------------------------------------------------------------


get_ratio <- function(split){

  analysis(split) %>%
    group_by(species) %>%
    summarise(nb = n()) %>%
    ungroup() %>%
    mutate(ratio = nb/sum(nb))
  
}


fish_estimates <- fish_reshaped %>%
  rowwise() %>%
  slice(rep(1:n(), each = count)) %>%
  ungroup() %>%
  group_by(biome) %>%
  nest() %>%
  mutate(data_boot = map(data, ~bootstraps(.x, times = 1000))) %>%
  unnest(data_boot) %>%
  mutate(results = map(splits, ~get_ratio(.x))) %>%
  unnest(results) %>%
  dplyr::select(biome, id, species, nb, ratio) %>%
  group_by(biome, species) %>%
  summarise(ratio_mean = mean(ratio),
            ratio_upper = quantile(ratio, 0.95),
            ratio_lower = quantile(ratio, 0.05),
            .groups = "drop")



# catches number for figure caption ---------------------------------------

fish_catches <- fish_reshaped %>%
  group_by(biome) %>%
  summarise(catches = sum(count))

# figure ------------------------------------------------------------------

library(showtext)
font_add(family = "ITC Benguiat Book", regular = "./ITC Benguiat Book.ttf")
showtext_auto()

for (loop_biome in unique(fish_estimates$biome)) {
  
  cat(loop_biome, "\n")
  
  fish_plot <- fish_estimates %>%
    filter(biome == loop_biome) %>%
    ggplot(aes(reorder(species, desc(ratio_mean)), # aes(reorder_within(species, desc(ratio_mean)), within = biome
           ratio_mean, ymin = ratio_lower, ymax = ratio_upper)) +
    geom_pointrange(position = position_dodge(width = 0.5), size = 1) +
    # geom_point() +
    # tidytext::scale_x_reordered() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       minor_breaks = c(seq(0,0.1,0.01), seq(0.1, 1, 0.05)),
                       breaks = c(0.01, 0.05, seq(0.1,1,0.1))) +
    # ggsci::scale_color_npg() +
    # facet_wrap(~biome, scales = "free") +
    coord_flip() +
    dark_theme_gray() +
    theme(#axis.text.x = element_text(angle = 45, hjust = 1),
          # panel.grid.minor.y = element_line(color = "gray95"),
          # panel.grid.major.y = element_line(color = "gray80"),
          axis.title = element_blank(),
          plot.background = element_rect(fill = "#1b2838"),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "grey30", size = 0.8),
          panel.grid.minor.x = element_line(color = "grey30", size = 0.1),
          panel.grid.major.y = element_line(color = "grey30", size = 0.1),
          legend.background = element_blank(),
          axis.ticks = element_blank(),
          legend.key = element_blank(),
          title = element_text(family = "ITC Benguiat Book", size = 45),
          axis.text = element_text(size = 28),
          plot.caption = element_text(size = 20, face = "italic")) +
    labs(subtitle = stringr::str_to_title(loop_biome),
         caption = glue("Computed from {fish_catches[(fish_catches$biome == loop_biome),'catches']} catches."))
  
  screen_jpg <- glue("./images/converted/screen_{loop_biome}.jpg")
  map_jpg <- glue("./images/converted/map_{loop_biome}.jpg")
  
  ggdraw(fish_plot) + 
    draw_image(screen_jpg, x = 0.70, y = 0.7, hjust = 0, vjust = 0, width = 0.2, height = 0.2) +
    draw_image(map_jpg, x = 0.82, y = 0.7, hjust = 0, vjust = 0, width = 0.2, height = 0.2)
  
  ggsave(
    filename = glue("{loop_biome}.png"),
    path = "./outputs",
    width = 16, height = 9, units = "cm"
  )
}



rm(loop_biome)




