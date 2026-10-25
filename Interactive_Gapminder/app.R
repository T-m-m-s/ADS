library(gapminder)
library(dplyr)
library(ggplot2)
library(shiny)

axis_choices <- c("Life expectancy" = "lifeExp", "Population" = "pop", "GDP per capita" = "gdpPercap")

ui <- fluidPage(
  titlePanel("Shiny Gapminder"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year", min = min(gapminder$year), max = max(gapminder$year), step = 5, value = min(gapminder$year), sep = "", animate = TRUE),
      selectInput("continent", "Continent",
                  choices = c("All", levels(gapminder$continent))),
      radioButtons("xaxis", "X", choices = axis_choices, selected = "gdpPercap"),
      radioButtons("yaxis", "Y", choices = axis_choices, selected = "lifeExp")
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    gapminder |>
      filter(year == input$year,input$continent == "All" | continent == input$continent)
  })
  output$scatterPlot <- renderPlot({
    ggplot(dataset(), aes(.data[[input$xaxis]], .data[[input$yaxis]], size = pop, colour = country)) +
      geom_point(alpha = 0.5, show.legend = FALSE) +
      scale_colour_manual(values = country_colors) +
      scale_size(range = c(2, 20)) +
      scale_x_log10() +
      scale_y_log10() +
      labs(x = names(axis_choices)[axis_choices == input$xaxis], y = names(axis_choices)[axis_choices == input$yaxis]) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "grey95", colour = NA))
  })
}

shinyApp(ui, server)