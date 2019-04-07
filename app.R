#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# úkony - rozdělené na prioritní / vše:
#   k datu
#   počet agend
#   % agend s úkony = hotovo
#   počet úkonů
#   dnů do 30.6.2019 / 28.2.2019
#   seznam:
#     zbývajících agend
#     hotových agend
#   graf se stavem:
#     dle ÚSÚ
  
library(shiny)
library(shinydashboard)
library(DT)

source("R/etl.R")
source("R/process.R")

# UI ----------------------------------------------------------------------

# Define sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text = "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem(
      text = "Ohlašovatelé",
      tabName = "ovm",
      icon = icon("chart-bar")
    ),
    menuItem(
      text = "Hotové agendy",
      tabName = "seznamagendok",
      icon = icon("th-list")
    ),
    menuItem(
      text = "Zbývající agendy",
      tabName = "seznamagend",
      icon = icon("th-list")
    ),
    menuItem(
      text = "Přehled úkonů",
      tabName = "prehledukonu",
      icon = icon("th-list")
    ),
    menuItem(
      text = "Agendy bez úkonů",
      tabName = "agendybez",
      icon = icon("th-list")
    ),
    menuItem(
      text = "Info",
      tabName = "info",
      icon = icon("info")
    ),
    checkboxInput("checkbox", "Jen prioritní agendy", value = FALSE, width = NULL)
  )
)
# Define body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2(paste("Přehled o zpracování úkonů v agendách k", stazeno.dne)),
            fluidRow(valueBox(uiOutput("n.agend"), "agend", color = "teal", width = 4, icon = icon("business-time")),
                     valueBox(uiOutput("n.ukonu"), "úkonů", color = "maroon", width = 4, icon = icon("cart-plus"))),
            fluidRow(valueBox(uiOutput("n.dnu"), "dnů zbývá", color = "light-blue", width = 4, icon = icon("calendar-alt")),
                     valueBox(uiOutput("hotovo"), "agend hotovo", color = "purple", width = 4, icon = icon("check-circle"))
            )),
    tabItem(tabName = "ovm",
            h2("Zpracování dle ohlašovatelů"),
            fluidRow(
              plotOutput("bp.a.usu")
            )),
    tabItem(tabName = "seznamagend",
            h2("Zbývající agendy"),
            fluidRow(
              box(DT::DTOutput('table.agendy'), width = 10)
            )),
    tabItem(tabName = "seznamagendok",
            h2("Agendy s definovanými úkony"),
            fluidRow(
              box(DT::DTOutput('table.agendy.ok'), width = 10)
            )),
    tabItem(tabName = "prehledukonu",
            h2("Úkony v agendě"),
            fluidRow(box(
              selectInput("vybranaagenda", h3("Výběr agendy"), 
                          choices = agendy.list, selected = NULL), width = 10)),
            fluidRow(
              box(DT::DTOutput('table.ukony'), width = 10)
            )),
    tabItem(tabName = "agendybez",
            h2("Agendy, ve kterých žádné úkony nejsou"),
            fluidRow(
              box(DT::DTOutput('table.agendy.bez.ukonu'), width = 10)
            )),
    tabItem(tabName = "info",
            h2("Informace"),
            fluidRow(
              box(p("Zdrojem pro zpracování jsou vygenerované XLSX soubory s veřejnými údaji k jednotlivým agendám přístupné na adrese", a("https://rpp-ais.egon.gov.cz/gen/agendy-detail/.",
href = "https://rpp-ais.egon.gov.cz/gen/agendy-detail/"), "Proto je zobrazený stav opožděný o jeden den."),
p("Počítán je pouze stav k uvedenému dni (pomíjí budoucí znění)."), width = 10)),
            fluidRow(
              infoBox("Kód na GitHubu", icon = shiny::icon("github"),
                      href = "https://github.com/trusinas/shiny_ukony", width = 5, color = "light-blue")
            ))
  ))

# Define UI
ui <- dashboardPage(dashboardHeader(title = "Zpracování úkonů"), sidebar, body)

# Server ------------------------------------------------------------------


# Define server logic
server <- function(input, output) {
  ukony.seznam <- reactive({
    get.ukony(input$vybranaagenda)
  })
  
  output$bp.a.usu <- renderPlot({
    if(input$checkbox == T) {
      p <- agendy %>% filter(prioritni == T) %>%
        group_by(usu) %>% 
        summarise(hotovo = mean(ukonu > 0 | bez.ukonu == T)) %>%
        ggplot(aes(fct_reorder(usu, hotovo), hotovo)) +
        geom_col() +
        coord_flip() +
        labs(y = "zpracováno agend", x = NULL) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        theme_minimal()
    }
    if(input$checkbox == F) {
      p <- agendy %>% 
        group_by(usu) %>% 
        summarise(hotovo = mean(ukonu > 0 | bez.ukonu == T)) %>%
        ggplot(aes(fct_reorder(usu, hotovo), hotovo)) +
        geom_col() +
        coord_flip() +
        labs(y = "zpracováno agend", x = NULL) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        theme_minimal()
    }
    print(p)
  })
  output$table.agendy.ok <- DT::renderDT({
    if(input$checkbox == T) {
      ag.seznam.ok <- agendy %>% filter(ukonu > 0) %>% filter(prioritni == T) %>% select(kód = kod, název = nazev, ohlašovatel = usu)
    }
    if(input$checkbox == F) {
      ag.seznam.ok <- agendy %>% filter(ukonu > 0) %>% select(kód = kod, název = nazev, ohlašovatel = usu)
    }
    return(ag.seznam.ok)
  })
  output$table.agendy <- DT::renderDT({
    if(input$checkbox == T) {
      ag.seznam <- agendy %>% filter(ukonu == 0) %>% filter(prioritni == T) %>% filter(bez.ukonu == F) %>% select(kód = kod, název = nazev, ohlašovatel = usu)
    }
    if(input$checkbox == F) {
      ag.seznam <- agendy %>% filter(ukonu == 0) %>% filter(bez.ukonu == F) %>% select(kód = kod, název = nazev, ohlašovatel = usu)
    }
    return(ag.seznam)
  })
  output$n.agend <- renderText({
    if(input$checkbox == T) {
      n.agend <- agendy %>% filter(prioritni == T) %>% nrow()
    }
    if(input$checkbox == F) {
      n.agend <- nrow(agendy)
    }
    return(n.agend)
  })
  output$n.dnu <- renderText({
    if(input$checkbox == T) {
      n.dnu <- 0
    }
    if(input$checkbox == F) {
      n.dnu <- difftime(as.Date("2019-06-30"), Sys.Date()-1)
    }
    return(n.dnu)
  })
  output$n.ukonu <- renderText({
    if(input$checkbox == T) {
      n.ukonu <- sum(agendy$ukonu[agendy$prioritni == T])
    }
    if(input$checkbox == F) {
      n.ukonu <- sum(agendy$ukonu)
    }
    return(n.ukonu)
  })
  output$hotovo <- renderText({
    if(input$checkbox == T) {
      p.hotovo <- agendy %>% 
        filter(ukonu != 0 | bez.ukonu == T) %>% 
        filter(prioritni == T) %>% 
        nrow()/length(prioritni)*100
    }
    if(input$checkbox == F) {
      p.hotovo <- agendy %>% 
        filter(ukonu != 0 | bez.ukonu == T) %>% 
        nrow()/nrow(agendy)*100
    }
    paste(round(p.hotovo), "%")
  })
  output$table.ukony <- DT::renderDT({
    ukony.seznam() %>% 
      select(název = nazev, komentář = komentar, 'lze elektronicky' = elektronicky)
  })
  output$table.agendy.bez.ukonu <- DT::renderDT({
    agendy.bez.ukonu %>% 
      select(kód = kod, název = nazev, důvod = duvod)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

