task2_ui <- function(id){
  ns = NS(id)
  uiOutput(ns("task1"))
  fluidPage(
    titlePanel("Расчет АЧС АМ-сигнала"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(ns("a"), "Амплитуда A, мВ", min = 1, max = 25, value = 1),
        sliderInput(ns("tu1"), "Период импульса tu1, мкс", min = 1, max = 25, value = 1),
        sliderInput(ns("tp1"), "Период последовательности Tп1, мкс", min = 5, max = 350, value = 5),
        uiOutput(ns("formula")),
      ),
      mainPanel(
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
        a = input$a
        tu1 = input$tu1
        tp1 = input$tp1
        impulses <- data.frame(
        xmin = c(-tp1-tu1/2,-tu1/2,tp1-tu1/2), # координаты x начала прямоугольников
        xmax = c(-tp1+tu1/2,tu1/2,tp1+tu1/2), # координаты x конца прямоугольников
        ymin = rep(0, 3), # координата y начала прямоугольников
        ymax = rep(a, 3) # координата y конца прямоугольников
        )
        p = ggplot(impulses, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
        geom_rect(color = "blue", fill = "lightblue") + # рисуем прямоугольники
        xlim(-tu1, 3*tp1) + # устанавливаем пределы оси x
        ylim(0, a) + # устанавливаем пределы оси y
        xlab("Время, мс") + # подписываем оси
        ylab("Мощность импульса, мВ") +
        theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm")))) +
        theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm")))) +
        theme(panel.grid.major = element_line(color = "grey",linewidth = 0.5,linetype = 2)) + 
        theme(panel.grid.minor = element_line(color = "grey",linewidth = 0.25,linetype = 2))+
        ggtitle("График импульсов")
        if (tu1%%2){
          p = p + scale_x_continuous(breaks=c(-tp1-tu1/2,-tp1+tu1/2,-tu1/2,tu1/2,tp1-tu1/2,tp1+tu1/2))
        } else{
          p = p + scale_x_continuous(breaks=c(-tp1-tu1/2,-tp1,-tp1+tu1/2,-tu1/2,0,tu1/2,tp1-tu1/2,tp1,tp1+tu1/2)) # добавляем заголовок
        }
        p = p + scale_y_continuous(expand=c(0,0),breaks=c(a))
        p
      })

      output$output_plot2 <- renderPlot({
        a = input$a
        tu1 = input$tu1
        tp1 = input$tp1
        x = seq(0,tp1, length.out = 1000)
        y = abs(a * sin(2*pi*x*tp1)/(pi*x*tp1))
        df <- data.frame(x = x, y = y)
        p <- ggplot(df, aes(x=x)) +
        geom_line(aes(y=y)) +
        labs(x = "t, мкс", y = "Uам(t), мВ") +
        theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm")))) +
        theme(panel.grid.major = element_line(color = "grey",linewidth = 0.5,linetype = 2)) + 
        theme(panel.grid.minor = element_line(color = "grey",linewidth = 0.25,linetype = 2)) +
        ggtitle("Амплитудно-частотный спектр импульсной последовательности")
        p <- p + scale_x_continuous(expand = c(0, 0))
        p
      })

      output$formula <- renderUI({
        a = input$a
        tu1 = input$tu1
        tp1 = input$tp1 
        output <- c(paste('Q = \\frac{\\tau_п}{T_и} =', paste0("\\frac{",tp1,"}{",tu1,"} ="), tp1/tu1))
        foreach(i = output) %do%{
          withMathJax(paste0("$$",i,"$$"))
        }
      })
})
}
