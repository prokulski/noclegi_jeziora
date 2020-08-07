library(tidyverse)
library(gridExtra)
library(rlang)

noclegi <- readRDS("data/noclegi_sf.rds")




noclegi %>%
  filter(jezioro_id == 7877) %>%
  ggplot() +
  geom_sf(aes(geometry=geometry, size=odleglosc, color=ocena)) +
  geom_sf_text(aes(geometry=geometry, label=nazwa_obiektu), size = 2)





noclegi %>%
  ggplot() +
  geom_point(aes(ocena, cena))


noclegi %>%
  ggplot() +
  geom_histogram(aes(ocena), binwidth = 0.1) +
  scale_x_continuous(breaks = seq(0, 10, 1),
                     minor_breaks = seq(0, 10, 0.2))



noclegi %>%
  ggplot() +
  geom_histogram(aes(cena), binwidth = 5) +
  scale_x_continuous(breaks = seq(0, 500, 50),
                     minor_breaks = seq(0, 500, 10))


noclegi %>%
  ggplot() +
  geom_histogram(aes(odleglosc/1000), binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 50, 5))




noclegi_long_lat <- noclegi %>%
  select(cena, ocena, geometry, odleglosc) %>%
  mutate( lng = st_coordinates(geometry)[, 1],
          lat = st_coordinates(geometry)[, 2]) %>%
  as_tibble() %>%
  select(-geometry) %>%
  mutate(lng = round(lng, 2),
         lat = round(lat, 2)) %>%
  group_by(lng, lat) %>%
  summarise_all(mean) %>%
  ungroup()


noclegi_long_lat %>%
  ggplot() +
  geom_point(aes(lng, lat, color = cena)) +
  scale_color_distiller(palette = "Reds", direction = 1)


noclegi_long_lat %>%
  ggplot() +
  geom_point(aes(lng, lat, color = ocena)) +
  scale_color_distiller(palette = "Reds", direction = 1)



noclegi_long_lat %>%
  ggplot() +
  geom_point(aes(lng, lat, color = odleglosc)) +
  scale_color_distiller(palette = "Reds", direction = 1)



grid_plot <- function(p_up, p_mid, p_right) {
  empty <- ggplot() +
    geom_point(aes(1,1), colour="white")+
    theme(panel.background=element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())

  p_mid <- p_mid + theme(legend.position = "none")

  gridExtra::grid.arrange(p_up + theme(axis.title = element_blank(),
                                       axis.text = element_blank(),
                                       axis.ticks = element_blank(),
                                       text = element_blank()),
                          empty,
                          p_mid + theme(axis.title = element_blank(),
                                        axis.text = element_blank(),
                                        axis.ticks = element_blank(),
                                        text = element_blank()),
                          p_right + theme(axis.title = element_blank(),
                                          axis.text = element_blank(),
                                          axis.ticks = element_blank(),
                                          text = element_blank()),
                          ncol = 2, nrow = 2,
                          widths = c(4, 1), heights = c(1, 4),
                          top = p_mid$labels$title,
                          left = p_mid$labels$y,
                          bottom = p_mid$labels$x)
}


p_up <- noclegi_long_lat %>%
  group_by(lng) %>%
  summarise(odleglosc = mean(odleglosc), .groups='drop') %>%
  ggplot() +
  geom_area(aes(lng, odleglosc)) +
  theme_minimal() +
  labs(title = "title p_up",
       x = "x p_up",
       y = "y p_up")

p_mid <- noclegi_long_lat %>%
  ggplot() +
  geom_point(aes(lng, lat, color = odleglosc)) +
  scale_color_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  labs(title = "title p_mid",
       x = "x p_mid",
       y = "y p_mid")

p_right <- noclegi_long_lat %>%
  group_by(lat) %>%
  summarise(odleglosc = mean(odleglosc), .groups='drop') %>%
  ggplot() +
  geom_area(aes(lat, odleglosc)) +
  coord_flip() +
  theme_minimal() +
  labs(title = "title p_right",
       x = "x p_right",
       y = "y p_right")

grid_plot(p_up, p_mid, p_right)

# all_lakes_mid %>%
#   filter(obiekt == "Hotel Górski PTTK Kalatówki") %>%
#   ggplot() +
#   geom_sf(aes(geometry=geometry, size=odleglosc)) +
#   geom_sf_text(aes(geometry=geometry, label=name))




kwakwa <- function(df, kol) {
  df %>%
    pull( {{ kol }} ) %>%
    mean()
}

kwakwa(iris, Petal.Width)
