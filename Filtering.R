library(tidyverse)
library(sf)
library(DT)


Artlist <- data.table::fread("artsliste.csv") %>%
  as.data.frame()

library(readr)

frekvens2 <- read_csv("Novana/alledata-frekvens2.txt") %>%
  janitor::clean_names() %>%
  as_tibble()

frekvens2$species <- str_split(frekvens2$specieslist, ",")

frekvens2 <- frekvens2 %>%
  dplyr::select(-specieslist) %>%
  unnest(species) %>%
  mutate(species = str_trim(species, side = "both"),
         species = str_remove_all(species, "\\}"),
         site = str_remove_all(site, "\\{"),
         species = as.numeric(species)) %>%
  rename(ArtID = species) %>%
  left_join(Artlist) %>%
  mutate(plot = as.character(plot)) %>% dplyr::select(plot, year, LatArt) %>%
  dplyr::filter(year < (lubridate::year(lubridate::ymd(Sys.Date())) + 1)) %>%
  group_by(plot) %>%
  dplyr::filter(LatArt != "") %>%
  ungroup()


Test <- read_rds("AllData4.rds") %>%
  dplyr::filter(Dataset == "Novana") %>%
  separate(col = "ID", into = c("ID", "plot")) %>%
  right_join(frekvens2) %>%
  dplyr::filter(habtype == 2140) %>%
  dplyr::select("plot", "LatArt", "ID", "Dataset","habtype",
                "MajorHab", "year") %>%
  rename(site = ID) %>%
  dplyr::group_split(plot, year)

## make binary variable with 1 and 0



for(i in 1:length(Test)){
  Test[[i]]$Rosa <- NA

  Test[[i]]$Rosa <- ifelse("Rosa rugosa" %in% Test[[i]]$LatArt, 1, 0)

  Test[[i]] <- Test[[i]] %>%
    dplyr::select(-LatArt) %>%
    distinct()

  Test[[i]]
  if((i %% 100) == 0){
    message(paste(i, "of", length(Test), "ready!", Sys.time()))
  }
}

rm(Artlist)
rm(frekvens2)

gc()

Test <- Test %>%
  purrr::reduce(rbind)


Species <- read_csv("Novana/alledata-abiotiske2.csv") %>% dplyr::select("site", "plot", "year", "antalarter", "antalstjernearter", "antaltostjernearter",
  "antalenaarigearter", "meanscore", "andelstjerne", "andeltostjerne") %>%
  dplyr::filter(plot %in% Test$plot) %>%
  mutate(plot = as.character(plot), site = as.character(site))


Final <- Test %>% full_join(Species)

saveRDS(Final, "Final.rds")

sf::write_sf(Final, "Final.shp")
