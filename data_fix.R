# oprava úkonů (vymazání úkonů z XLSX kvůli neschváleným úkonům)

source("R/etl.R")
fix <- "A341" # agenda k opravě
agendy$ukonu[agendy$kod == fix] <- 0
write_rds(agendy, "output/stazeno.rds")
