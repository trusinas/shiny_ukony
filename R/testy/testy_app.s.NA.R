# output$bp.a.usu
# context("output$bp.a.usu")
#     agendy %>% filter(prioritni == T) %>%
#       group_by(usu) %>% 
#       summarise(hotovo = mean(ukonu > 0)) %>%
#       ggplot(aes(fct_reorder(usu, hotovo), hotovo)) +
#       geom_col() +
#       coord_flip() +
#       labs(y = "zpracováno agend", x = NULL) +
#       scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
#       theme_minimal()
# 
# output$table.agendy.ok
context("output$table.agendy.ok")
ag.seznam.ok <- agendy %>% filter(ukonu > 0) %>% filter(prioritni == T) %>% select(kod, nazev, usu)
test_that('ag.seznam.ok vrátí ? výsledků', {
  expect_equal(nrow(ag.seznam.ok), 6)
})
test_that('kódy 6 hotových agend', {
  expect_equal(ag.seznam.ok$kod, c("A3", "A32", "A115", "A117", "A118", "A3787"))
})

# output$table.agendy
context("output$table.agendy")
ag.seznam <- agendy %>% filter(ukonu == 0) %>% select(kod, nazev, usu)
test_that('ag.seznam vrátí ? výsledků', {
  expect_equal(nrow(ag.seznam), 6)
})
test_that('kódy 9 zbývajících agend', {
  expect_equal(ag.seznam$kod, c("A5", "A6", "A7", "A8", "A11", "A22", "A23", "A24", "A1117"))
})

# output$n.agend
context("output$n.agend")
n.agend <- agendy %>% filter(prioritni == T) %>% nrow()
test_that('n.agend vrátí 6 výsledků', {
  expect_equal(n.agend, 6)
})

# output$n.ukonu
context("output$n.ukonu")
n.ukonu <- agendy %>% 
  filter(prioritni == T) %>% 
  select(ukonu) %>% 
  sum(na.rm = T)
test_that('n.ukonu vrátí hodnotu 39', {
  expect_equal(n.ukonu, 39)
})

# output$hotovo
context("output$hotovo")
p.hotovo <- agendy %>% 
  filter(ukonu != 0) %>% 
  filter(prioritni == T) %>% 
  nrow()/40*100 # nrow()/length(prioritni)*100
test_that('p.hotovo vrátí hodnotu 12.5', {
  expect_equal(p.hotovo, 12.5)
})