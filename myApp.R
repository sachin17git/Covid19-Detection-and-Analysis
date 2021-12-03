library(shiny)

ui <- fluidPage(
  titlePanel("First App!"),
    mainPanel(
      plotOutput(outputId = "temp")

    )
  )

server <- function(input, output) {
  output$temp <- renderPlot({
    plot(1:10, 1:10, xlab = "x", ylab = "y")
})
}
shinyApp(ui = ui, server = server)