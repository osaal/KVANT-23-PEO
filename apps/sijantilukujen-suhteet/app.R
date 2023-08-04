#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("ggplot2")

ui <- fluidPage(

    # Application title
    titlePanel("Sijaintilukujen muutokset"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("alpha",
                        "Alfa:",
                        min = 1,
                        max = 5,
                        value = 4.0,
                        step = 0.25),
            sliderInput("beta",
                        "Beta:",
                        min = 1,
                        max = 5,
                        value = 4.0,
                        step = 0.25),
            actionButton("reset",
                         "Palauta"),
            helpText("Liikuttamalla alfa- ja beta-liukureita voit muuttaa",
                     "jakauman muotoa. Näet keskiarvon, mediaanin ja moodin",
                     "kuvan oikeassa yläkulmassa. Painamalla 'Palauta'-nappia",
                     "pääset takaisin normaalijakaumaan.")
        ),

        mainPanel(
           plotOutput("distPlot")
        )
    )
)

server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    mean <- input$alpha / (input$alpha + input$beta)
    median <- (input$alpha - 1/3) / (input$alpha + input$beta - 2/3)
    mode <- (input$alpha - 1) / (input$alpha + input$beta - 2)
    graph <- ggplot(
      data.frame(x = c(0, 1)), aes(x = x)) +
      stat_function(fun = dbeta, args = list(shape1 = input$alpha, shape2 = input$beta)) + 
      geom_segment(
        x = mean,
        y = 0,
        xend = mean,
        yend = 2.5,
        col = "red",
        linetype = "dashed",
        linewidth = 1.5
      ) + 
      geom_segment(
        x = median,
        y = 0,
        xend = median,
        yend = 2.5,
        col = "orange",
        linetype = "dotted",
        linewidth = 1.5
      ) + 
      geom_segment(
        x = mode,
        y = 0,
        xend = mode,
        yend = 2.5,
        col = "purple",
        linewidth = 1.5
      )
    ypos <- ggplot_build(graph)$layout$panel_params[[1]]$y.range[2]
    just <- xpos <- if(input$alpha < 2){ 1 } else { 0 }
    graph +
      annotate(
        "text",
        x = xpos,
        y = ypos,
        hjust = just,
        label = paste0("Keskiarvo: ", round(mean, 2))
      ) +
      annotate(
        "text",
        x = xpos,
        y = ypos - 0.1,
        hjust = just,
        label = paste0("Mediaani: ", round(median, 2))
      ) +
      annotate(
        "text",
        x = xpos,
        y = ypos - 0.2,
        hjust = just,
        label = paste0("Moodi: ", round(mode, 2))
      )
  })
  observeEvent(input$reset, {
    updateSliderInput(session, "alpha", value = 4)
    updateSliderInput(session, "beta", value = 4)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
