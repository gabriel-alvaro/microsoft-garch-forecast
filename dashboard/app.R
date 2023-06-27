# Gabriel Alvaro Batista  RA 171822
# Geovani Reolon de Sousa RA 185245
# Pedro Pimentel Cabrini  RA 187169

library(shiny)
library(shinydashboard)
library(dygraphs)
library(quantmod)
library(DT)
library(readr)
library(tsibble)
library(rugarch)
library(zoo)

# header
header = dashboardHeader(title = "Microsoft Stock Price",
                         tags$li(class = 'dropdown', tags$a(HTML(paste(textOutput("update"))))))

# sidebar
sidebar = dashboardSidebar(width = 150,
                           sidebarMenu(menuItem("Início", tabName = "inicio", icon = icon("house")),
                                       menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-simple"))))

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

ftextInicio = fluidPage(
  h1("Microsoft - Previsão de Ações"),
  h4("Neste projeto, foram coletados e analisados dados da ação MSFT (Microsoft), entre o período de 2007 à 2023,",
     "a fim de realizar previsões dos retornos esperados no dia seguinte."),
  h4("Os arquivos desse projeto, bem como o histórico de previsões, podem ser encontrados no ",
     tags$a(href= "https://github.com/gabriel-alvaro/microsoft-garch-forecast", "GitHub")),
  tags$head(
    tags$style(HTML(
      "#content {
          position: fixed;
          bottom: 0;
          width: 100%;
          text-align: left;
          padding-bottom: 10px;
        }
      "))),
  tags$div(id = "content",
           h4(p("Trabalho desenvolvido por"),
              tags$ul(
                tags$li("Gabriel Alvaro Batista"),
                tags$li("Geovani Reolon de Sousa"),
                tags$li("Pedro Pimentel Cabrini")
              ))),
  box(
    title = "Última Previsão",
    width = 4,
    dataTableOutput("tbl_previsoes")
  )
)

body = dashboardBody(
  tags$style(".centered-row { display: flex; justify-content: center; }"),
  tabItems(
    tabItem("inicio",
            ftextInicio),
    tabItem("dashboard",
            date_selector,
            frow1,
            frow2)
  )
)

ui = dashboardPage(title = "ME607",
                   header,
                   sidebar,
                   body)

server = function(input, output){
  
  ## coleta dados
  microsoft_df = reactive({
    data = fortify.zoo(quantmod::getSymbols("MSFT", 
                                            src = "yahoo", auto.assign = FALSE, 
                                            from = '2007-01-01', to = Sys.Date() + 1, return.class = 'zoo'))
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
  
  ## tabela previsoes
  forecast_data = reactive({
    microsoft_df = getSymbols("MSFT", 
                              src = "yahoo", auto.assign = FALSE, 
                              from = '2007-01-01', to = Sys.Date() + 1, return.class = 'zoo')
    
    log_retorno_dif = diff(log(microsoft_df[,6]))
    df = fortify.zoo(log_retorno_dif)
    df = as_tsibble(df, index = Index)
    
    spec = readRDS(url("https://github.com/gabriel-alvaro/microsoft-garch-forecast/raw/main/modelagem/garch_spec.rds"))
    fit = ugarchfit(spec, df, solver = 'hybrid')
    
    # previsao
    forecast = ugarchforecast(fit, data = df, n.ahead = 1)
    
    # nova previsao
    data = data.frame(data = as.Date(colnames(forecast@forecast$seriesFor)) + 1,
                      previsao_retorno = round(forecast@forecast$seriesFor[1], 4),
                      previsao_sigma = round(forecast@forecast$sigmaFor[1], 4))
    data
  })
  
  output$tbl_previsoes = renderDataTable({
    datatable(forecast_data(),
              colnames = c("Data", "Previsão (Retorno)", "Previsão (Volatilidade)"),
              options = list(dom = "t", ordering = FALSE))
  })
}

shinyApp(ui = ui,
         server = server)
