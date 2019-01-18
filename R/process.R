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