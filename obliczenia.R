library(tidyverse)
library(fs)
library(sf)

# ograniczamy zebrane dane tylko do tych które mają wszystkie kolumny,
# cenę bez outlierów i długość geograficzną plasującą je w Polsce (mniej więcej)
noclegi <- readRDS("data/grabed_data.rds") %>%
  drop_na() %>%
  filter(cena <= quantile(cena, 0.99)) %>%
  filter(between(lng, 14.2, 23.9))



#### MAPY JEZIOR ####

# mapy ściągamy z http://download.geofabrik.de/europe/poland.html
# i rozpakowujemy każde województwo do osobnego folderu (tutaj ~/tmp/osm/)

# wyszukujemy wszystkie pliki zawierające interesujące nas informacje o jeziorach
files <- dir_ls("~/tmp/osm", type = "directory") %>% paste0("/gis_osm_water_a_free_1.shp")


# scalamy w jedną wielką mapę wszystkie jeziora
all_lakes <- NULL
for(f in files) {
  print(paste("Mapa jezior: ", f)) # progress bar

  m <- read_sf(f) %>%
    # interesuje nas tylko to co ma nazwę i jest typu "water"
    filter(fclass == "water" & !is.na(name)) %>%
    # dodajemy info o województwie
    mutate(woj=str_match(f, ".*/tmp/osm/(.*)/gis_osm_water_a_free_1.shp")[2])

  if(is.null(all_lakes)) {
    all_lakes <- m
  }

  all_lakes <- bind_rows(all_lakes, m)
}


# all_lakes %>%
#   ggplot() +
#   geom_sf(aes(geometry=geometry, fill=woj), color="gray50")


# wyliczamy środki obiektów wodnych
all_lakes_mid <- all_lakes %>%
  group_by(osm_id, name) %>%
  summarise(geometry = st_union(geometry), .groups="drop") %>%
  ungroup() %>%
  mutate(geometry = st_centroid(geometry)) %>%
  mutate(id = row_number())


# lista noclegów na SF
noclegi_sf <- st_as_sf(noclegi, coords = c("lng", "lat"), crs = 4326)



#### NAJBLIŻSZE JEZIORO W STOSTUNKU DO NOCLEGU ####

# miejse na nowe kolumny
noclegi_sf$jezioro_nazwa <- NA
noclegi_sf$jezioro_id <- NA
noclegi_sf$odleglosc <- NA

# dla kazdego noclegu
for(i in 1:nrow(noclegi_sf)) {
  print(paste("Odleglosc nocleg - jezioro", i, "/", nrow(noclegi_sf)))

  # szukamy odległości do wszystkich jezior
  distance_to_lakes <- st_distance(noclegi_sf[i, ], all_lakes_mid) %>% as.numeric()

  # który element w tablicy odległości jest najmniejszy?
  nearest_lake <- distance_to_lakes %>% which.min()

  # zapisujemy te informacje w tablicy z noclegami
  # nazwa jeziora
  noclegi_sf$jezioro_nazwa[i] <- all_lakes_mid$name[nearest_lake]
  noclegi_sf$jezioro_id[i] <- all_lakes_mid$id[nearest_lake]
  # odległość ośrodku od środka jeziora
  noclegi_sf$odleglosc[i] <- distance_to_lakes[nearest_lake]
}

noclegi_sf <- mutate(noclegi_sf, id = row_number())

# zapisujemy wynik
saveRDS(noclegi_sf, "data/noclegi_sf.rds")



#### NAJBLIŻESZE NOCLEGI DLA JEZIORA ####

all_lakes_mid$obiekt_nazwa <- NA
all_lakes_mid$obiekt_id <- NA
all_lakes_mid$odleglosc <- NA
all_lakes_mid$lokalizacja <- NA

# dla kazdego jeziora
for(i in 1:nrow(all_lakes_mid)) {
  print(paste("Odleglosc jezioro - nocleg", i, "/", nrow(all_lakes_mid)))

  # szukamy odległości do wszystkich jezior
  distance_to_place <- st_distance(all_lakes_mid$geometry[i], noclegi_sf$geometry) %>% as.numeric()

  # który element w tablicy odległości jest najmniejszy?
  nearest_place <- distance_to_place %>% which.min()

  # zapisujemy te informacje w tablicy z noclegami
  all_lakes_mid$obiekt_nazwa[i] <- noclegi_sf$nazwa_obiektu[nearest_place]
  all_lakes_mid$obiekt_id[i] <- noclegi_sf$id[nearest_place]
  all_lakes_mid$lokalizacja[i] <- noclegi_sf$lokalizacja[nearest_place]
  all_lakes_mid$odleglosc[i] <- distance_to_place[nearest_place]
}


saveRDS(all_lakes_mid, "data/all_lakes_mid.rds")

