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

source("R/etl.R")
# source("R/process.R")

# Define sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text = "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem(
      text = "OVM",
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
            fluidRow(
              box(title = "Základní přehled",
              paste("počet agend:", nrow(agendy)), br(),
              paste("počet úkonů:", sum(agendy$udaju)), br(),
              paste("zbývá dnů:", difftime(as.Date("2019-06-30"), Sys.Date()-1))
              ))),
    tabItem(tabName = "ovm",
            h2("OVM"),
            fluidRow(
            )),
    tabItem(tabName = "seznamagend",
          h2("Zbývající agendy"),
          fluidRow(
            tableOutput('table.agendy')
          ))
  ))

# Define UI for application that draws a histogram
ui <- dashboardPage(dashboardHeader(title = "Zpracování údajů"), sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$hist1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- agendy$udaju
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  output$bp1 <- renderPlot({
    agendy %>% 
      group_by(usu) %>% 
      summarise(prumer = mean(udaju)) %>% 
      ggplot(aes(fct_reorder(usu, prumer), prumer)) +
      geom_col() +
      coord_flip() +
      labs(y = "průměrný počet údajů", x = NULL)
  })
  output$table.agendy <- renderTable(agendy %>% filter(udaju > 0) %>% select(kód = kod, název = nazev, ohlašovatel = usu))
}

# Run the application 
shinyApp(ui = ui, server = server)

