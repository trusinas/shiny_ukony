readxlsx_url <- function(url, ...) {
  df <- httr::GET(url, httr::write_disk(paste0("tmp", ".xlsx"), overwrite = TRUE))
  df <- readxl::read_excel("tmp.xlsx", ...)
}

#  importovat aktuální do pracovní složky s údaji:
#   - kód + název + gestor + datum ukončení platnosti agendy
#   - počet úkonů

get.data <- function(path) {
  agenda <- readxlsx_url(path) %>%
    select(4) %>% 
    transpose() %>% 
    unlist() %>% 
    .[c(2:3, 5, 9:10)]
  ukonu <- read_excel("tmp.xlsx", sheet = 5, .name_repair = "universal") %>%
    filter(V..Úkony.poskytované.agendou == "Hlavní atributy úkonu") %>% # ``
    nrow()
  agenda.df <- as.data.frame(matrix(agenda, ncol = 5, byrow = T), stringsAsFactors = F) %>% 
    mutate(V6 = ukonu,
           V3 = V3 %>% 
             as.numeric() %>% 
             as.Date(., origin = "1899-12-30"))
  return(agenda.df)
}
