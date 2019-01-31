#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# údaje - rozdělené na prioritní / vše:
#   k datu
#   počet agend
#   % agend s úkony = hotovo
#   počet úkonů + histogram
#   dnů do 30.6.2019 / 28.2.2019
#   seznam:
#     zbývajících agend
#     zbývajících ÚSÚ
  
library(shiny)
library(shinydashboard)
# library(DT)

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
      icon = icon("th")
    ),
    menuItem(
      text = "Seznam agend",
      tabName = "seznamagend",
      icon = icon("columns")
    ),
    checkboxInput("checkbox", "Prioritní agendy", value = FALSE, width = NULL)
  )
)
# Define body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2(paste("Přehled o zpracování údajů v agendách k", stazeno.dne)),
            fluidRow(title = "Základní přehled",
                     box(htmlOutput("n.agend"), width = 4),
                     box(htmlOutput("n.ukonu"), width = 4),
                     box(htmlOutput("n.dnu"), width = 4),
                     box(htmlOutput("hotovo"), width = 4)
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
          ))
  ))

# Define UI
ui <- dashboardPage(dashboardHeader(title = "Zpracování údajů"), sidebar, body)

# Server ------------------------------------------------------------------


# Define server logic
server <- function(input, output) {
  
  output$bp.a.usu <- renderPlot({
    if(input$checkbox == T) {
      p <- agendy %>% filter(prioritni == T) %>%
        group_by(usu) %>% 
        summarise(hotovo = mean(udaju > 0)) %>%
        ggplot(aes(fct_reorder(usu, hotovo), hotovo)) +
        geom_col() +
        coord_flip() +
        labs(y = "zpracováno agend", x = NULL) +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal()
    }
    if(input$checkbox == F) {
      p <- agendy %>% 
        group_by(usu) %>% 
        summarise(hotovo = mean(udaju > 0)) %>%
        ggplot(aes(fct_reorder(usu, hotovo), hotovo)) +
        geom_col() +
        coord_flip() +
        labs(y = "zpracováno agend", x = NULL) +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal()
    }
    print(p)
  })
  output$table.agendy <- DT::renderDT({
    if(input$checkbox == T) {
      ag.seznam <- agendy %>% filter(udaju > 0) %>% filter(prioritni == T) %>% select(kód = kod, název = nazev, ohlašovatel = usu)
    }
    if(input$checkbox == F) {
      ag.seznam <- agendy %>% filter(udaju > 0) %>% select(kód = kod, název = nazev, ohlašovatel = usu)
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
    paste(h2(n.agend), br(), "agend")
  })
  output$n.dnu <- renderText({
    if(input$checkbox == T) {
      dnu <- difftime(as.Date("2019-02-28"), Sys.Date()-1)
    }
    if(input$checkbox == F) {
      dnu <- difftime(as.Date("2019-06-30"), Sys.Date()-1)
    }
    paste(h2(dnu), br(), "dnů zbývá")
  })
  output$n.ukonu <- renderText({
    if(input$checkbox == T) {
      ukonu <- sum(agendy$udaju[agendy$prioritni == T])
    }
    if(input$checkbox == F) {
      ukonu <- sum(agendy$udaju)
    }
    paste(h2(ukonu), br(),"úkonů")
  })
  output$hotovo <- renderText({
    if(input$checkbox == T) {
      p.hotovo <- agendy %>% 
        filter(udaju != 0) %>% 
        filter(prioritni == T) %>% 
        nrow()/nrow(agendy)*100
    }
    if(input$checkbox == F) {
      p.hotovo <- agendy %>% 
        filter(udaju != 0) %>% 
        nrow()/nrow(agendy)*100
    }
    paste(h2(round(p.hotovo), "%"), br(), "agend hotovo")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

