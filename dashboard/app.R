# Gabriel Alvaro Batista  RA 171822
# Geovani Reolon de Sousa RA 185245
# Pedro Pimentel Cabrini  RA 187169

library(shiny)
library(shinydashboard)
library(dygraphs)
library(quantmod)
library(DT)

# header
header = dashboardHeader(title = "Microsoft Stock Price",
                         tags$li(class = 'dropdown', tags$a(HTML(paste(textOutput("update"))))))

# sidebar
sidebar = dashboardSidebar(
  sidebarMenu(menuItem("Início", tabName = "inicio", icon = icon("house")),
              menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-simple")),
              menuItem("Previsão", tabName = "previsao", icon = icon("magnifying-glass-chart"))))

# body
date_selector = fluidRow(
  class = "centered-row",
  box(
    title = "Filtro de Data",
    status = "primary",
    solidHeader = TRUE,
    width = 3,
    dateRangeInput("date_range", "", 
                   start = "2007-01-04", end = Sys.Date())))

frow1 = fluidRow(
  class = "centered-row",
  box(width = 4,
      title = "Histórico de Preços",
      status = "primary",
      solidHeader = TRUE,
      dygraphOutput("plot_ajustado", height = "300px")
  ),
  box(width = 4,
      title = "Retornos Diários",
      status = "primary",
      solidHeader = TRUE,
      dygraphOutput("plot_retorno", height = "300px")
  ),
  box(width = 4,
      title = "Correlação",
      status = "primary",
      solidHeader = TRUE,
      plotOutput("plot_acf", height = "300px")
  )
)

frow2 = fluidRow(
  class = "centered-row",
  box(width = 4,
      title = "Log-Retornos Diários",
      status = "primary",
      solidHeader = TRUE,
      dygraphOutput("plot_logretorno", height = "300px")
  ),
  box(width = 4,
      title = "Log-Volume Diário",
      status = "primary",
      solidHeader = TRUE,
      dygraphOutput("plot_logvolume", height = "300px")
  ),
  box(width = 4,
      title = "Correlação Parcial",
      status = "primary",
      solidHeader = TRUE,
      plotOutput("plot_pacf", height = "300px")
  )
)

ftext1 = fluidPage(
  tags$ul(
    tags$li("Gabriel Alvaro Batista"),
    tags$li("Geovani Reolon de Sousa"),
    tags$li("Pedro Pimentel Cabrini")
  )
)

ftext2 = fluidPage(
  h2(strong("Modelo")),
  p("O modelo escolhido foi um", strong("ARMA(2,2)-GARCH(1,1).")),
  box(
    title = "Parâmetros",
    width = 3,
    dataTableOutput("tbl_parametros")
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
            date_selector,
            frow1,
            frow2),
    tabItem("previsao",
            ftext2)
  ))

ui = dashboardPage(title = "ME607",
                   header,
                   sidebar,
                   body)

server = function(input, output){
  
  ## coleta dados
  microsoft_df = reactive({
    data = fortify.zoo(quantmod::getSymbols("MSFT", 
                                            src = "yahoo", auto.assign = FALSE, 
                                            from = '2007-01-01', return.class = 'zoo'))
    data
  })
  
  microsoft = reactive({
    data = microsoft_df()[,c(1,7)]
    data = subset(data, Index >= input$date_range[1] & Index <= input$date_range[2])
    data
  })
  
  microsoft_retorno = reactive({
    data = microsoft()
    data[,2] = c(NA, diff(microsoft()[,2]))
    data = subset(data, Index >= input$date_range[1] & Index <= input$date_range[2])
    data
  })
  
  microsoft_logretorno = reactive({
    data = microsoft()
    data[,2] = c(NA, diff(log(microsoft()[,2])))
    data = subset(data, Index >= input$date_range[1] & Index <= input$date_range[2])
    data
  })
  
  microsoft_logvolume = reactive({
    data = microsoft_df()[,c(1,6)]
    data[,2] = log(data[,2])
    data = subset(data, Index >= input$date_range[1] & Index <= input$date_range[2])
  })
  
  ## plot precos ajustados
  output$plot_ajustado = renderDygraph({
    dygraph(microsoft(), ylab = "Preços Ajustados", xlab = "Tempo")
  })
  
  ## plot retorno
  output$plot_retorno = renderDygraph({
    dygraph(microsoft_retorno(), ylab = "Retornos Diários", xlab = "Tempo")
  })
  
  ## plot log retorno
  output$plot_logretorno = renderDygraph({
    dygraph(microsoft_logretorno(), ylab = "(log) Retornos Diários", xlab = "Tempo")
  })
  
  ## plot log volume
  output$plot_logvolume = renderDygraph({
    dygraph(microsoft_logvolume(), ylab = "(log) Volume Diário", xlab = "Tempo")
  })
  
  ## plot acf
  output$plot_acf = renderPlot({
    acf(microsoft_retorno()[,2], na.action = na.pass, main = "", ylab = "")
  })
  
  ## plot pacf
  output$plot_pacf = renderPlot({
    pacf(microsoft_retorno()[,2], na.action = na.pass, main = "", ylab = "")
  })
  
  ## update
  output$update = renderText({
    Sys.setenv(TZ = "America/Sao_Paulo")
    paste0("Última atualização: \t", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), " (UTC-3)")
  })
  
  ## tabela parametros
  fit = readRDS(url("https://github.com/gabriel-alvaro/microsoft-garch-forecast/raw/main/modelagem/garch_fit.rds"))
  
  params = data.frame(parametro = round(fit@fit$coef, 4))
  
  output$tbl_parametros = renderDataTable({
    datatable(head(params, 9), colnames = c("Parâmetro", "Valor"), 
              options = list(dom = "t", ordering = FALSE)
    )
  })
}

shinyApp(ui = ui,
         server = server)
