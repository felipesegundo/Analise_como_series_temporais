# Carregando bibliotecas necess??rias
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(tidyr)
library(forecast)
library(urca)

# Defini????o da UI
ui <- dashboardPage(
  dashboardHeader(title = "Previsao e Analise de Serie Temporal"),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Carregamento do Banco de Dados", tabName = "dados", icon = icon("database")),
      menuItem("Efeito Sazonal", tabName = "sazonal", icon = icon("calendar")),
      menuItem("Teste de Normalidade", tabName = "normalidade", icon = icon("chart-bar")),
      menuItem("Analise da Estacionaridade", tabName = "estacionaridade", icon = icon("signal")),
      menuItem("Grafico de Autocorrelacao", tabName = "autocorrelacao", icon = icon("retweet")),
      menuItem("Modelo ARIMA e Residuos", tabName = "arima", icon = icon("line-chart"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tab de Carregamento do Banco de Dados
      tabItem(tabName = "dados",
              fluidRow(
                box(
                  title = "Tabela de Dados",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,  # Ajuste de largura para ocupar toda a linha
                  tableOutput("tabela_dados")
                )
              )
      ),
      
      # Tab de Efeito Sazonal
      tabItem(tabName = "sazonal",
              fluidRow(
                box(
                  title = "Efeito Sazonal (2010 - 2018)",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,  # Ajuste de largura para ocupar toda a linha
                  plotlyOutput("plot_sazonal", height = 600)
                )
              )
      ),
      
      # Tab de Teste de Normalidade
      tabItem(tabName = "normalidade",
              fluidRow(
                box(
                  title = "Teste de Normalidade da Serie Temporal",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,  # Ajuste de largura para ocupar toda a linha
                  plotOutput("plot_normalidade", height = 600),
                  verbatimTextOutput("teste_shapiro")
                )
              )
      ),
      
      # Tab de Analise da Estacionaridade
      tabItem(tabName = "estacionaridade",
              fluidRow(
                box(
                  title = "Analise da Estacionaridade (Dickey-Fuller)",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,  # Ajuste de largura para ocupar toda a linha
                  verbatimTextOutput("teste_dickeyfuller")
                )
              )
      ),
      
      # Tab de Grafico de Autocorrelacao
      tabItem(tabName = "autocorrelacao",
              fluidRow(
                box(
                  title = "Grafico de Autocorrelacao",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,  # Ajuste de largura para ocupar toda a linha
                  plotOutput("plot_autocorrelacao", height = 600)
                )
              )
      ),
      
      # Tab de Modelo ARIMA e Residuos
      tabItem(tabName = "arima",
              fluidRow(
                box(
                  title = "Modelo ARIMA e Analise dos Residuos",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,  # Ajuste de largura para ocupar toda a linha
                  plotlyOutput("plot_modelo_arima", height = 600)
                )
              )
      )
    )
  )
)

# Defini????o do servidor
server <- function(input, output) {
  # Carregar dados
  dados <- read.csv("chuva_tratado.csv", sep = ';', encoding = 'latin1')
  
  output$tabela_dados <- renderTable({
    dados
  })
  
  # Transformar em s??rie temporal
  chuva_sp2 <- as.vector(t(dados))
  serie <- ts(chuva_sp2, start = c(1985, 1), end = c(2018, 12), frequency = 12)
  
  output$plot_sazonal <- renderPlotly({
    p <- ggseasonplot(window(serie, start = c(2010)), year.labels = TRUE) +
      theme_bw()
    ggplotly(p)
  })
  
  output$plot_normalidade <- renderPlot({
    qqnorm(serie, col = "green", main = "Analise de Normalidade")
    qqline(serie, col = "red")
  })
  
  output$teste_shapiro <- renderText({
    shapiro_test <- shapiro.test(serie)
    paste("Estatistica do teste de Shapiro-Wilk:", shapiro_test$statistic)
    paste("Valor p:", shapiro_test$p.value)
  })
  
  
  output$plot_autocorrelacao <- renderPlot({
    tsdisplay(serie)
  })
  
  output$plot_modelo_arima <- renderPlotly({
    modelo_sarima <- arima(serie, order = c(0, 0, 0), seasonal = c(2, 1, 0))
    serie_df <- data.frame(Ano = time(serie), Valor = serie)
    serie_df$modelo <- fitted(modelo_sarima) + modelo_sarima$resid
    
    gg <- ggplot(serie_df, aes(x = Ano)) +
      geom_line(aes(y = Valor), color = "blue", size = 0.3) +
      geom_line(aes(y = modelo), color = "red", size = 0.3) +
      labs(title = "Ajuste do Modelo SARIMA ?? Serie Temporal",
           x = "Ano", y = "Valor")
    
    ggplotly(gg)
  })
}

# Rodar o aplicativo Shiny
shinyApp(ui = ui, server = server)


