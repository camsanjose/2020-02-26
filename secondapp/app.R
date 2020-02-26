library(shiny)
library(tidyverse)
library(dplyr)
library(magrittr)
library(gapminder)

# Define UI for application that draws a histogram
gapminder %<>% mutate_at(c("year","country"),as.factor)

gapminder_year = unique(gapminder$year)
gapminder_country =unique(gapminder$country) %>% str_sort()

headerRow<- div(
    selectInput("selYear",
                "Select the year",
                multiple=TRUE,
                choices = gapminder_year),
    selectInput("selCountry",
                "Select country",
                multiple=TRUE,
                choices = gapminder_country)
)

dataPanel = tabPanel("Data",
                     tableOutput("dataTable")
)

plotPanel = tabPanel("Plot",
                     plotOutput("plotData"),
                     selectInput("selCountry",
                                 "Select country",
                                 multiple=TRUE,
                                 choices = gapminder_country)
)


ui <-navbarPage("Shiny App",
                dataPanel, plotPanel, header= headerRow
)

server = function(input, output) {
    gapminder_filtered <- reactive(gapminder %>% filter(year %in% input$selYear, country %in% input$selCountry))
    
    output$dataTable = renderTable({
        req(input$selYear)
        gapminder_filtered()
    })
    output$plotData = renderPlot({
        req(input$selCountry)
        req(input$selYear)
        gapminder_filtered() %>%
            ggplot(aes(x=country, y=pop, fill=year)) + 
            geom_bar(stat="identity", position=position_dodge())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)