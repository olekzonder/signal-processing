task1_ui <- function(id){
  ns = NS(id)
  uiOutput(ns("task1"))
  fluidPage(
    titlePanel("Расчет АЧС АМ-сигнала"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(ns("u0"), "Амплитуда несущих колебаний u0, мВ", min = 1, max = 25, value = 1),
        sliderInput(ns("f0"), "Частота несущих колебаний f0, МГц", min = 100, max = 200, value = 100),
        sliderInput(ns("m1"), "Глубина амплитудной модуляции m1, %", min = 0, max = 100, value = 100),
        sliderInput(ns("fm1"), "Частота колебаний модулирующего сигнала Fm1, КГц", min = 200, max = 2000, value = 200),
        checkboxInput(ns("env"),"Отобразить огибающую"),
        uiOutput(ns("header")),
        uiOutput(ns("formula"))
      ),
      mainPanel(
      
      plotOutput(ns("output_plot1")),
      plotOutput(ns("output_plot2"))
    )
    )
  )
}

task1_server <- function(id){
  moduleServer(
    id,
    function(input,output,session){
      ns <- session$ns
      output$header <- renderUI({
        h4("Решение задачи:")
      })

      output$formula <- renderUI({
      u0 <- input$u0 * 10^-3
      f0 <- input$f0 * 10^6
      m1 <- input$m1 / 100
      fm1 <- input$fm1 * 10^3
      output <- c(paste("A_0 = U_0 =", input$u0, "\\spaceмВ"),
                paste("A_{н\\spaceбок} = A_{в\\spaceбок} = \\frac{m_1*U_0}{2} = ", m1*u0/2/10^-3, "\\spaceмВ"),
                paste("f_{в\\spaceбок} = f_0 + F_{м1} =",(f0 + fm1)/10^6, "\\spaceМГц"),
                paste("f_{н\\spaceбок} = f_0 - F_{м1} =",(f0 - fm1)/10^6, "\\spaceМГц"),
                paste("\\Delta f_c = 2F_{м1} = ", 2*fm1/10^3, "\\spaceкГц"),
                paste("\\frac{1}{F_м} = ", round((1/fm1)*10^6,digits=4), "\\spaceмкс"),
                paste("\\frac{1}{f_0} = ", round((1/f0)*10^6,digits=4), "\\spaceмкс"))
      foreach(i = output) %do%{
        withMathJax(paste0("$$",i,"$$"))
      }
    })
      output$output_plot1 <- renderPlot({
      u0 <- input$u0 * 10^-3
      f0 <- input$f0 * 10^6
      m1 <- input$m1 / 100
      fm1 <- input$fm1 *10^3
      x <- seq(0, 3/fm1, length.out=round(5/f0*10^11,digits=-3))
      y <- u0*cos(2*pi*f0*x) + (0.5*(m1*u0))*cos(2*pi*(f0+fm1)*x) + (0.5*(m1*u0))*cos(2*pi*(f0-fm1)*x) #u0*sin(2*pi*f0*x)+m1*u0/2*cos(2*pi*(f0+fm1)*x)+m1*u0/2*sin(x*(f0-fm1))
      df <- data.frame(x = x*10^6, y = y*10^3)
      p <- ggplot(df, aes(x=x)) +
        geom_line(aes(y=y)) +
        labs(x = "t, мкс", y = "Uам(t), мВ") +
        theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm")))) +
        theme(panel.grid.major = element_line(color = "grey",linewidth = 0.5,linetype = 2)) + 
        theme(panel.grid.minor = element_line(color = "grey",linewidth = 0.25,linetype = 2)) +
        ggtitle("Временная диаграмма однотонального АМ сигнала")
      if(input$env){
        hilbert <- env(y,fm1)
        p = p +
        geom_line(aes(y=hilbert*10^3),color='red', linetype='dashed', linewidth=1.25) +
        geom_line(aes(y=-hilbert*10^3),color='red',linetype='dashed',linewidth=1.25)
      }
      p <- p + scale_x_continuous(expand = c(0, 0),breaks=c(10^6/fm1,2*10^6/fm1,3*10^6/fm1)) + ylim(1.25*min(df$y),1.25*max(df$y))
      p
    })
      output$output_plot2 <- renderPlot({
        u0 <- input$u0 * 10^-3
        f0 <- input$f0 * 10^6
        m1 <- input$m1 / 100
        fm1 <- input$fm1 * 10^3

        x <- seq(0,f0+fm1)
        y <- seq(0,u0+1)
        df <- data.frame(x=c((f0-fm1)/10^6,f0/10^6,(f0+fm1)/10^6),y=c((u0*m1/2)*10^3,u0*10^3,(u0*m1/2)*10^3),label=c("Aн.бок","A0","Ав.бок"))

        ggplot(df, aes(x=x, y=y))+
        geom_point()+
        geom_segment(aes(x = x, y = 0, xend = x, yend = y), linetype = "dashed")+
        labs(x = "f, МГц", y = "S(f), мВ") +
        geom_text(aes(label = label), nudge_x = 0, nudge_y = (u0*10^3)/12.5)+
        scale_y_continuous(expand=c(0, 0),limit=c(0,(u0*(10^3)+(u0*10^3)/10)))+
        theme(axis.line = element_line(arrow = arrow(angle = 15, length = unit(.15,"inches"),type = "closed"))) + 
        theme(panel.grid.major = element_line(color = "grey",linewidth = 0.5,linetype = 2)) + 
        theme(panel.grid.minor = element_line(color = "grey",linewidth = 0.25,linetype = 2)) + 
        theme(axis.text.y = element_text(hjust = 1)) +
        ggtitle("АЧС радиосигнала с однотональной АМ")
      })
    })}