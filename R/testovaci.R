path <- "data/A944_26062018.xlsx" # 0 úkonů
path <- "https://rpp-ais.egon.gov.cz/gen/agendy-detail/A28_16102012.xlsx" # 0 úkonů i údajů on-line
path <- "https://rpp-ais.egon.gov.cz/gen/agendy-detail/A2_06012014.xlsx" # neplatná
path <- "https://rpp-ais.egon.gov.cz/gen/agendy-detail/A117_01072018.xlsx" # údaje
path <- "https://rpp-ais.egon.gov.cz/gen/agendy-detail/A3787_09012019.xlsx" # 1 úkon

# úkony -------------------------------------------------------------------
path0 <- "C:/Users/TrusinaS/Downloads/A1021_17042012.xlsx" # 0 ok
path1 <- "C:/Users/TrusinaS/Downloads/Detail_Ohlaseni_O5538_2019_01_29_14_32_25.xlsx" # 1 ok
path6 <- "C:/Users/TrusinaS/Downloads/Detail_Agenda_A117_2019_01_24_10_56_24.xlsx" # 6

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

test <- get.data(path)
# grafy a statistiky ----------------------------------------------

# údaje - rozdělené na prioritní / vše či ostatní ?:
#   počet agend
nrow(agendy) # ok

#   % agend s úkony = hotovo
agendy %>% 
  filter(udaju != 0) %>% 
  nrow()/nrow(agendy)*100

#   počet úkonů
sum(agendy$udaju) # ok

#     seznam zbývajících agend - podklad pro interaktivní procházení
agendy %>% 
  filter(udaju == 0) %>% 
  select(kod, nazev, usu)
# přidat možnost filtrování dle ÚSÚ

#     stav zpracovaní dle ÚSÚ
agendy %>% 
  group_by(usu) %>% 
  summarise(hotovo = mean(udaju > 0))

agendy %>% 
  group_by(usu) %>% 
  summarise(hotovo = mean(udaju > 0)) %>%
  ggplot(aes(fct_reorder(usu, hotovo), hotovo)) +
  geom_col() +
  coord_flip() +
  labs(y = "zpracováno agend", x = NULL) +
  scale_y_continuous(labels = scales::percent)
# místo názvů zkratky ?