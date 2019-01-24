# hodnoty ----------------------------------------------

# údaje - rozdělené na prioritní / vše či ostatní ?:
#   počet agend
n.agend <- nrow(agendy) # ok
n.p.agend <- agendy %>% 
  filter(prioritni == T) %>% 
  nrow()


# grafy a statistiky ----------------------------------------------
