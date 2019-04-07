# opravit kvůli agendám bez úkonů:
# - agend hotovo (přidat) = output$hotovo
# - Zpracování dle ohlašovatelů (přidat) = output$bp.a.usu
# - Zbývající agendy (odebrat) = output$table.agendy

# output$bp.a.usu
context("output$bp.a.usu - číselná hodnota splněných agend")

p.p.hotovo <- agendy %>% filter(prioritni == T) %>%
  group_by(usu) %>%
  summarise(hotovo = mean(ukonu > 0 | bez.ukonu == T)) %>% 
  .$hotovo
p.hotovo <- agendy %>% 
  group_by(usu) %>%
  summarise(hotovo = mean(ukonu > 0 | bez.ukonu == T)) %>% 
  .$hotovo

test_that('% hotových agend odpovídá', {
  expect_equal(p.p.hotovo, c(1, 1))
  expect_equal(p.hotovo, c(0, 1, 0, 0, 1, 0, 1, 1))
})

# output$table.agendy.ok
context("output$table.agendy.ok - Agendy s definovanými úkony")
ag.seznam.ok <- agendy %>% filter(ukonu > 0) %>% filter(prioritni == T) %>% select(kod, nazev, usu)
test_that('ag.seznam.ok vrátí 6 výsledků', {
  expect_equal(nrow(ag.seznam.ok), 6)
})
test_that('kódy 6 hotových agend', {
  expect_equal(ag.seznam.ok$kod, c("A3", "A32", "A115", "A117", "A118", "A3787"))
})
# A1117 není uvedeno

# output$table.agendy
context("output$table.agendy - Zbývající agendy")
ag.seznam <- agendy %>% filter(ukonu == 0) %>% filter(bez.ukonu == F) %>% select(kod, nazev, usu)
test_that('ag.seznam vrátí 8 výsledků', {
  expect_equal(nrow(ag.seznam), 8)
})
test_that('kódy 8 zbývajících agend', {
  expect_equal(ag.seznam$kod, c("A5", "A6", "A7", "A8", "A11", "A22", "A23", "A24"))
})
# A1117 není uvedeno

# output$n.agend
context("output$n.agend - počet agend")
n.agend <- agendy %>% filter(prioritni == T) %>% nrow()
test_that('n.agend vrátí 6 výsledků', {
  expect_equal(n.agend, 6)
})

# output$n.ukonu
context("output$n.ukonu - počet úkonů")
n.ukonu <- agendy %>% 
  filter(prioritni == T) %>% 
  select(ukonu) %>% 
  sum(na.rm = T)
test_that('n.ukonu vrátí hodnotu 49', {
  expect_equal(n.ukonu, 49)
})

# output$hotovo
context("output$hotovo - % hotových agend")
p.p.hotovo <- agendy %>% 
  filter(ukonu != 0 | bez.ukonu == T) %>% 
  filter(prioritni == T) %>% 
  nrow()/40*100 # nrow()/length(prioritni)*100
p.hotovo <- agendy %>% 
  filter(ukonu != 0 | bez.ukonu == T) %>% 
  nrow()/nrow(agendy)*100 # nrow()/length(prioritni)*100
test_that('p.hotovo vrátí požadovanou hodnotu', {
  expect_equal(p.hotovo, 10/18*100)
  expect_equal(p.p.hotovo, 15)
})
# A1117 braná jako hotová