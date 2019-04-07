# hodnoty ----------------------------------------------
agendy <- agendy %>% 
  mutate(poradi = str_remove(kod, "A") %>% as.numeric()) %>% 
  arrange(poradi) %>% 
  select(-poradi)
# údaje - rozdělené na prioritní / vše či ostatní ?:
#   počet agend
# n.agend <- nrow(agendy) # ok
# n.p.agend <- agendy %>% 
#   filter(prioritni == T) %>% 
#   nrow()

agendy.kody <- agendy %>% filter(ukonu > 0) %>% .$kod
agendy.nazvy <- agendy %>% filter(ukonu > 0) %>% .$nazev
agendy.list <- as.list(agendy.kody)
names(agendy.list) <- paste(agendy.kody, agendy.nazvy, sep = " - ")

# grafy a statistiky ----------------------------------------------
