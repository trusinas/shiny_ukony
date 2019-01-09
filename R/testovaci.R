path <- "data/A944_26062018.xlsx" # 0 úkonů
path <- "https://rpp-ais.egon.gov.cz/gen/agendy-detail/A28_16102012.xlsx" # 0 úkonů i údajů on-line
path <- "https://rpp-ais.egon.gov.cz/gen/agendy-detail/A2_06012014.xlsx" # neplatná
path <- "https://rpp-ais.egon.gov.cz/gen/agendy-detail/A117_01072018.xlsx" # údaje

agendy <- read_rds("output/agendy.rds")

agenda <- get.data(path)
test <- tab[1:20,]

# grafy a statistiky ----------------------------------------------

# údaje - rozdělené na prioritní / vše či ostatní ?:
#   počet agend
nrow(agendy)

#   % agend s úkony = hotovo
agendy %>% 
  filter(udaju != 0) %>% 
  nrow()/nrow(agendy)*100

#   počet úkonů
sum(agendy$udaju)

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