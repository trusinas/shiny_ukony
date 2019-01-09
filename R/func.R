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
  udaju <- read_excel("tmp.xlsx", sheet = 6, .name_repair = "universal") %>% 
  # ukonu <- read_excel("tmp.xlsx", sheet = 5) %>% 
    # filter(`V. Úkony poskytované agendou` == "Hlavní atributy úkonu") %>% 
    select(...2) %>% 
    filter(!is.na(...2) | !str_detect("Kód údaje", ...2)) %>% 
    nrow()-1
  agenda.df <- as.data.frame(matrix(agenda, ncol = 5, byrow = T), stringsAsFactors = F) %>% 
    mutate(V6 = udaju, # opravit na ukonu
           V3 = V3 %>% 
             as.numeric() %>% 
             as.Date(., origin = "1899-12-30"))
  return(agenda.df)
}
