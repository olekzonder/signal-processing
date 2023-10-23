task2_ui <- function(id){
  ns = NS(id)
  uiOutput(ns("task2"))
  fluidPage(
    titlePanel("Расчет АЧС АМ-сигнала"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(ns("u0"), "Амплитуда несущих колебаний u0, мВ", min = 1, max = 25, value = 1),
        sliderInput(ns("f0"), "Частота несущих колебаний f0, МГц", min = 100, max = 200, value = 100),
        sliderInput(ns("m1"), "Глубина амплитудной модуляции m1, %", min = 0, max = 100, value = 100),
        sliderInput(ns("fm1"), "Частота колебаний модулирующего сигнала Fm1, КГц", min = 200, max = 2000, value = 200),
        checkboxInput(ns("env"),"Отобразить огибающую"),
        selectInput(ns("dropdown"), "Выберите условие", c("Условие 1.1","Условие 1.2","Условие 1.3")),
        uiOutput(ns("header")),
        uiOutput(ns("formula")),
      ),
      mainPanel(
      uiOutput(ns("task")),
      plotOutput(ns("output_plot1")),
      plotOutput(ns("output_plot2"))
    )
    )
  )
}

task2_server <- function(id){
  moduleServer(
    id,
    function(input,output,session){
      ns <- session$ns
      output$output_plot1 <- renderPlot({
        a = input$a * 10^3
        tu1 = input$tu1 * 10^6
        tp1 = input$tp1 * 10^6
        x = seq(0,3*tu1, length.out = 1000)
        y = abs(a * sin(2*pi*x*tu1)/(pi*x*tp1))
        p <- ggplot(df, aes(x=x)) +
        geom_line(aes(y=y)) +
        labs(x = "t, мкс", y = "Uам(t), мВ") +
        theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm")))) +
        theme(panel.grid.major = element_line(color = "grey",linewidth = 0.5,linetype = 2)) + 
        theme(panel.grid.minor = element_line(color = "grey",linewidth = 0.25,linetype = 2)) +
        ggtitle("Амплитудно-частотный спектр импульсной последоватлеьности")
        p
      }
      )
})
}