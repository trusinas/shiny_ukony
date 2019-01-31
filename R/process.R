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


# grafy a statistiky ----------------------------------------------
