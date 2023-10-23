library(shiny)
library(ggplot2)
library(stringr)
library(foreach)
library(seewave)
source('tasks/task1.R')
source('tasks/task2.R')
# Главный интерфейс
ui <- navbarPage(
  theme = "theme.css",
  "Технология обработки и передачи данных",
  tabPanel("Расчет АЧС АМ-сигнала", task1_ui("task1")),
  tabPanel("Расчет АЧС импульсной последовательности", task2_ui("task2"))
)

# Главный сервер
server <- function(input, output,session){
  task1_server("task1")
  task2_server("task2")
}

# Запуск приложения
shinyApp(ui, server)