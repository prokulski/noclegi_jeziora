library(tidyverse)
library(rvest)
library(glue)


base_url = "https://www.nocowanie.pl/noclegi/?pid="

# maksymalna liczba stron indeksu do zeskrapowania
MAX_I = 450


# funkcja wyciągająca dane o ofertach z pojedynczej strony indeksu
grab_page <- function(page) {

  # tutaj zapiszemy sobie zebrane dane z ofert
  temp_df <- tibble()

  # każde ogłoszenie to osobmy element na liście
  nodes <- page %>% html_node("div#search-container") %>% html_nodes("div.sr-box--full")


  # dla każdej oferty
  for(node in nodes) {
    # div z informacjami
    inner_node <-  tryCatch(
      node %>% html_node("div.sr-box__body") %>% html_node("div.sr-box__body-inner"),
      error = function(x) { return(NULL) })


    # jeżeli ten div istnieje
    if(!is.null(inner_node)) {
      # nazwa obiektu
      nazwa_obiektu <- tryCatch(inner_node %>% html_node("h3") %>% html_text() %>% str_squish(), error = function(x) { return(NA) })
      # lokalizacja (miejscowość)
      lokalizacja <- tryCatch(inner_node %>% html_node("div.sr-box__location") %>% html_node("span") %>% html_text() %>% str_squish(), error = function(x) { return(NA) })
      # współrzędne geograficzne
      lng <- tryCatch(inner_node %>% html_node("div.sr-box__location") %>% html_node("a") %>% html_attr("lng") %>% as.numeric(), error = function(x) { return(NA) })
      lat <- tryCatch(inner_node %>% html_node("div.sr-box__location") %>% html_node("a") %>% html_attr("lat") %>% as.numeric(), error = function(x) { return(NA) })
      # URL do strony obiektu
      url_obiektu <- tryCatch(inner_node %>% html_node("div.sr-box__location") %>% html_node("a") %>% html_attr("mapurl"), error = function(x) { return(NA) })
      # oena obiektu
      ocena <- tryCatch(inner_node %>% html_node("div.sr-box__rating") %>% html_node(".sr-box__badge") %>% html_text() %>% as.numeric(), error = function(x) { return(NA) })
      # cena za miejsce
      cena <- tryCatch(node %>% html_node("div.sr-box__info") %>% html_node(".sr-box__price") %>% html_node(".sr-box__current-price") %>% html_nodes(".secondary") %>% html_text() %>% str_remove("zł") %>% as.numeric(), error = function(x) { return(NA) })

      # złożenie w wiersz i dodanie do wszystkich zebranych ze strony ofert
      temp_df <- bind_rows(temp_df,
                           tibble("nazwa_obiektu"=nazwa_obiektu,
                                  "url"=url_obiektu,
                                  "cena"=cena,
                                  "ocena"=ocena,
                                  "lokalizacja"=lokalizacja,
                                  "lat"=lat,
                                  "lng"=lng))
    }
  }

  return(temp_df)
}


# miejsce na zebrane oferty
grabed_data <- data_frame()


for(i in 1:MAX_I) {
  print(i) # taki progress bar dla ubogich

  # wyczytujemy i-tą stronę indeksu ofert
  page <- read_html(glue("{base_url}{i}"))

  # wyciągamy z niej informacje i dodajemy do pełnej bazy
  grabed_data <- bind_rows(grabed_data, grab_page(page))

  # co 10 stronę zapisujemy dane w razie błędów
  if(i %% 10 == 0) saveRDS(grabed_data, glue("data/grabed_data_{i}.rds"))

  # czekamy dla odciążenia serwera
  Sys.sleep(sample(seq(0.25, 1.0, 0.25), 1))
}

# zapisujemy całość danych
saveRDS(grabed_data, glue("data/grabed_data.rds"))

