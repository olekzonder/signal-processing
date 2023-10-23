  task2_ui <- function(id){
  ns = NS(id)
  uiOutput(ns("task2"))
  fluidPage(
    titlePanel("Амплитудно-частотный спектр импульсной последоватлеьности"),
    titlePanel("Расчет АЧС АМ-сигнала"),
    sidebarLayout(
      sidebarPanel(
                sliderInput(ns("a"), "Амплитуда A, мВ", min = 1, max = 25, value = 1),
        sliderInput(ns("tu1"), "Период импульса tu1, мкс", min = 1, max = 25, value = 1),
        sliderInput(ns("tp1"), "Период последовательности Tп1, мкс", min = 5, max = 350, value = 5),
                uiOutput(ns("formula"))
      )
    ),
    mainPanel(
      plotOutput(ns("output_plot1"))
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