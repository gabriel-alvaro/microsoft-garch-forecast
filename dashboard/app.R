# Gabriel Alvaro Batista  RA 171822
# Geovani Reolon de Sousa RA 185245
# Pedro Pimentel Cabrini  RA 187169

library(shiny)
library(shinydashboard)
library(dygraphs)
library(quantmod)

# header
header = dashboardHeader(title = "Microsoft Stock Price")

# sidebar
sidebar = dashboardSidebar(
  sidebarMenu(menuItem("Início", tabName = "inicio", icon = icon("house")),
              menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-simple"))))

# body

frow1 = fluidRow(
  box(
    title = "Histórico de Preços",
    status = "primary",
    solidHeader = TRUE,
    dygraphOutput("plot_ajustado", height = "400px")
  ),
  box(
    title = "Retornos Diários",
    status = "primary",
    solidHeader = TRUE,
    dygraphOutput("plot_retornos", height = "400px")
  )
)

ftext1 = fluidPage(
  mainPanel(
    tags$ul(
      tags$li("Gabriel Alvaro Batista"),
      tags$li("Geovani Reolon de Sousa"),
      tags$li("Pedro Pimentel Cabrini")
    )
  )
)

body = dashboardBody(
  tags$style(".centered-row { display: flex; justify-content: center; }"),
  tabItems(
    tabItem("inicio",
            h2("ME607 - Trabalho Final"),
            p("Trabalho desenvolvido por"),
            ftext1),
    tabItem("dashboard",
            fluidRow(
              class = "centered-row",
              box(
                title = "Filtro de Data",
                status = "primary",
                solidHeader = TRUE,
                width = 6,
                dateRangeInput("date_range", "", 
                               start = "2007-01-04", end = Sys.Date()))),
            frow1)
  ))

ui = dashboardPage(title = "ME607",
                   header,
                   sidebar,
                   body)

server = function(input, output){
  
  ## coleta dados
  microsoft_df = reactive({
    data1 = fortify.zoo(quantmod::getSymbols("MSFT", 
                                             src = "yahoo", auto.assign = FALSE, 
                                             from = '2007-01-01', return.class = 'zoo'))
    data1
  })
  
  microsoft = reactive({
    data2 = microsoft_df()[,c(1,7)]
    data2
  })
  
  microsoft_retorno = reactive({
    data3 = microsoft()
    data3[,2] = c(NA, diff(microsoft()[,2]))
    data3
  })
  
  microsoft_filtered = reactive({
    data4 = subset(microsoft(), Index >= input$date_range[1] & Index <= input$date_range[2])
    data4
  })
  
  microsoft_retorno_filtered = reactive({
    data5 = subset(microsoft_retorno(), Index >= input$date_range[1] & Index <= input$date_range[2])
    data5
  })
  
  ## plot precos ajustados
  output$plot_ajustado = renderDygraph({
    dygraph(microsoft_filtered(), ylab = "Preços Ajustados", xlab = "Tempo")
  })
  
  ## plot retornos
  output$plot_retornos = renderDygraph({
    dygraph(microsoft_retorno_filtered(), ylab = "Retornos Diários", xlab = "Tempo")
  })
}

shinyApp(ui = ui,
         server = server)
