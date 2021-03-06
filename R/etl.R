# postup:
#  načíst https://rpp-ais.egon.gov.cz/gen/agendy-detail/
#  převést na DF s hodnotami (kód, název, datum, soubor)
#  omezit na aktuální (<= Sys.Date() a max(datum) pro daný kód)
#  neplatné + bez změny nestahovat
#  importovat nové do pracovní složky s údaji:
#   - kód + název + gestor + datum ukončení platnosti agendy
#   - počet úkonů
#   - pořadí str_remove("A") %>% as.numeric
#  minulé (<= Sys.Date()) + uvedeným datem ukončení platnosti, výsledek smazat

library(tidyverse)
library(rvest)
library(lubridate)
library(readxl)
# library(future) - při map(agendy, ~value(.x)): Error: Evaluation error: error reading from the connection.

source("R/func.R", encoding = 'UTF-8')

prioritni <- c("A3", "A32", "A42", "A46", "A115", "A117", "A118", "A121", 
  "A124", "A385", "A414", "A416", "A419", "A530", "A531", "A565", 
  "A943", "A963", "A967", "A998", "A1023", "A1025", "A1029", "A1046", 
  "A1086", "A1095", "A1148", "A1154", "A1162", "A1185", "A1186", 
  "A1243", "A1261", "A1341", "A1601", "A1804", "A3082", "A3726", 
  "A3787", "A3791") # vypustit A1601 ?
if(file.exists("data/agendy.bez.ukonu.csv")) {
  agendy.bez.ukonu <- read_tsv("data/agendy.bez.ukonu.csv")
} else agendy.bez.ukonu <- data.frame(kod = character(), nazev = character(), duvod = character())
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
if(file.exists("output/neplatne.rds")) {
  neplatne <- read_rds("output/neplatne.rds")
  tab <- tab %>%
    anti_join(neplatne, by = c("kod", "platnost"))
} else neplatne <- data.frame(kod = character(), platnost = as.Date(character()), platnost.do = as.Date(character()))
if(file.exists("output/stazeno.rds")) {
  stazeno <- read_rds("output/stazeno.rds")
  stahnout <- tab %>%
    anti_join(stazeno, by = c("kod", "platnost"))
} else {
  stahnout <- tab
  stazeno <- structure(list(kod = character(), nazev = character(), platnost.do = as.Date(character()),
                           kod.usu = character(), usu = character(), ukonu = integer(), platnost = as.Date(character()),
                           prioritni = logical(), soubor = character()), class = "data.frame")
}

if (nrow(stahnout) > 0) {
  agendy <- map_df(stahnout$soubor, get.data)
  names(agendy) <- c("kod", "nazev", "platnost.do", "kod.usu", "usu", "ukonu")
  agendy <- left_join(stahnout, agendy[,-2], by = "kod")
  agendy <- agendy %>% 
    mutate(prioritni = kod %in% prioritni)
}
if (nrow(stahnout) == 0) {
  agendy <- structure(list(kod = character(), nazev = character(), platnost.do = as.Date(character()),
                          kod.usu = character(), usu = character(), ukonu = integer(), platnost = as.Date(character()),
                      prioritni = logical(), soubor = character()), class = "data.frame")
}

# OTESTOVAT
agendy.tmp <- rbind(agendy, stazeno)
agendy <- agendy.tmp %>% 
  group_by(kod) %>% 
  summarise(platnost = max(platnost)) %>% # může být vyšší než Sys.Date() ?
  left_join(agendy.tmp, by = c("kod", "platnost")) # s tab ?

neplatne <- agendy %>% 
  filter(platnost.do < Sys.Date()) %>% 
  select(kod, platnost, platnost.do) %>% 
  rbind(neplatne)
# dořešit, zda vyhovuje:
#   1) platná agenda zneplatněna - OK (nebyla v neplatných a nyní přibude)
#   2) neplatná agenda zplatněna
#   3) neplatná agenda stále neplatná - OK (není mezi staženými agendami, ale v [neplatne])
# přeuložit [neplatne] s kod, platnost, platnost.do

write_rds(neplatne, "output/neplatne.rds")
agendy <- agendy %>%
  filter(is.na(platnost.do) | platnost.do > Sys.Date())

write_rds(agendy, "output/stazeno.rds")
stazeno.dne <- html %>% 
  html_node("body b") %>%
  html_text() %>% 
  str_trunc(10, "right", ellipsis = "")

agendy.bez.ukonu <- agendy.bez.ukonu %>% 
  left_join(agendy, by = c("kod", "nazev")) %>% 
  select(kod, nazev, duvod, usu, soubor)
# DODĚLAT -----------------------------------------------------------------
# pokud je na webu chyba (chybí excel), skript se zastaví - viz agendy-detail_chyba.html
# neplatné vytváří z původního seznamu, takže se opět zastaví
# otestovat, jak se načte tabulka, pokud je:
#  a) buňka prázdná
#  b) chybí celý <td>

