readxlsx_url <- function(url, ...) {
  df <- httr::GET(url, httr::write_disk(paste0("tmp", ".xlsx"), overwrite = TRUE))
  df <- readxl::read_excel("tmp.xlsx", ...)
}

#  importovat aktuální do pracovní složky s údaji:
#   - kód + název + gestor + datum ukončení platnosti agendy
#   - počet úkonů

get.data <- function(file) {
  url <- paste0("https://rpp-ais.egon.gov.cz/gen/agendy-detail/", file)
  agenda <- tryCatch(readxlsx_url(url) %>%
                       select(4) %>% 
                       transpose() %>% 
                       unlist() %>% 
                       .[c(2:3, 5, 9:10)], error = function(e) c(str_extract(file, "A\\d{3,4}"), rep(NA, 4)))
  ukonu <- tryCatch(read_excel("tmp.xlsx", sheet = 5, .name_repair = "universal") %>%
                      filter(V..Úkony.poskytované.agendou == "Hlavní atributy úkonu") %>%
                      nrow(), error = function(e) NA)
  agenda.df <- as.data.frame(matrix(agenda, ncol = 5, byrow = T), stringsAsFactors = F) %>% 
    mutate(V6 = ukonu,
           V3 = V3 %>% 
             as.numeric() %>% 
             as.Date(., origin = "1899-12-30"))
  return(agenda.df)
}

get.ukony <- function(kod) {
  path <- paste0("https://rpp-ais.egon.gov.cz/gen/agendy-detail/", tab$soubor[tab$kod == kod])
  atributy <- c("Název úkonu:", "Komentář úkonu:", "Úkon lze řešit elektronicky:")
  seznam.ukonu <- readxlsx_url(path, sheet = 5, .name_repair = "universal") %>% 
    filter(V..Úkony.poskytované.agendou %in% atributy) %>% 
    select(pole = V..Úkony.poskytované.agendou, hodnota = ...3)
  if(nrow(seznam.ukonu) > 3) {
    seznam.ukonu <- unstack(seznam.ukonu, hodnota~pole)
    names(seznam.ukonu) <- c("komentar", "nazev", "elektronicky")
  }
  if(nrow(seznam.ukonu) == 3) {
    seznam.ukonu <- seznam.ukonu %>%
      t() %>%
      as.data.frame()
    names(seznam.ukonu) <- c("nazev", "komentar", "elektronicky")
    seznam.ukonu <- seznam.ukonu[-1,]
    rownames(seznam.ukonu) <- NULL
  }
  select(seznam.ukonu, nazev, komentar, elektronicky)
}
