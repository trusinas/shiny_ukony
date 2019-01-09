# postup:
#  načíst https://rpp-ais.egon.gov.cz/gen/agendy-detail/
#  převést na DF s hodnotami (kód, název, datum, soubor)
#  omezit na aktuální (<= Sys.Date() a max(datum) pro daný kód)
#  importovat aktuální do pracovní složky s údaji:
#   - kód + název + gestor + datum ukončení platnosti agendy
#   - počet úkonů
#  minulé (<= Sys.Date()) + uvedeným datem ukončení platnosti, výsledek smazat

library(tidyverse)
library(rvest)
library(lubridate)
library(readxl)
# library(future) - při map(agendy, ~value(.x)): Error: Evaluation error: error reading from the connection.

source("R/func.R")

html <- "https://rpp-ais.egon.gov.cz/gen/agendy-detail/" %>% 
  read_html()
tab <- html %>%
  html_node("table") %>%
  html_table()
soubory <- html %>%
  html_node("table") %>%
  html_nodes("a") %>%
  html_attr("href") %>% 
  unique()
tab <- cbind(tab, soubory) 
# puv.jmena <- names(tab)
names(tab) <- c("kod", "nazev", "platnost", "soubor")
tab$platnost <- dmy(tab$platnost)
tab <- tab %>%
  filter(platnost <= Sys.Date()) %>%
  group_by(kod) %>%
  summarise(platnost = max(platnost)) %>% 
  left_join(tab, by = c("kod", "platnost"))
neplatne <- read_rds("output/neplatne.rds")
tab <- tab %>% 
  filter(!kod %in% neplatne)
agendy <- map_df(paste0("https://rpp-ais.egon.gov.cz/gen/agendy-detail/", tab$soubor),
                 possibly(get.data, data.frame(V1 = NA, V2 = NA, V3 = NA, V4 = NA, V5 = NA, V6 = NA))) # 856.76 sec elapsed
names(agendy) <- c("kod", "nazev", "platnost.do", "kod.usu", "usu", "udaju") # ukonu
agendy <- left_join(tab, agendy[,-2], by = "kod")
neplatne <- agendy %>% 
  filter(platnost.do < Sys.Date()) %>% 
  .$kod %>% 
  union(neplatne)
write_rds(neplatne, "output/neplatne.rds")
agendy <- agendy %>%
  filter(is.na(platnost.do) | platnost.do > Sys.Date())

stazeno <- html %>% 
  html_node("body b") %>%
  html_text() %>% 
  str_remove(" Rozcestník vygenerovaných agend")

# DODĚLAT -----------------------------------------------------------------
# pokud je na webu chyba (chybí excel), skript se zastaví - viz agendy-detail_chyba.html
# neplatné vytváří z původního seznamu, takže se opět zastaví
# otestovat, jak se načte tabulka, pokud je:
#  a) buňka prázdná
#  b) chybí celý <td>

