# testy
# - funkce
# - části ETL
# - všechny render z app.R - manuálně překopírovat
# - process

library(testthat)
library(tidyverse)
# app.R -------------------------------------------------------------------
# žádoucí zobrazit NA či ne ?
# - output$n.agend
# - output$n.ukonu
# - output$hotovo
# - output$bp.a.usu
# - output$table.agendy.ok (ag.seznam.ok)
# - output$table.agendy
# netestovat:
# - output$n.dnu
# - output$table.ukony
# - output$table.agendy.bez.ukonu

# bez NA
agendy <- read_rds("R/testy/testovaci.agendy.rds")
# test_file("R/testy/testy_app.R")
test_file("R/testy/testy_app_s.agendami.bez.ukonu.R")

# s NA - zatím neřešit, dokud nebude jasné, jak s nimi nakládat
agendy[c(1, 3, 17),8] <- NA
test_file("R/testy/testy_app.s.NA.R")
