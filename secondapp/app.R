library(shiny)
library(tidyverse)
library(dplyr)
library(magrittr)
library(gapminder)
library(shinyjs)

esDB<- read_csv("es.csv")
# Define UI for application that draws a histogram
gapminder %<>% mutate_at(c("year","country"),as.factor)

gapminder_year = levels(gapminder$year)
gapminder_country =levels(gapminder$country) %>% str_sort()

headerRow<- div(id="header", useShinyjs(),
    selectInput("selYear",
                "Select the year",
                multiple=TRUE,
                choices = gapminder_year,
                selected=head(gapminder_year,3)),
    selectInput("selCountry",
                "Select country",
                multiple=TRUE,
                choices = gapminder_country, 
                selected=head(gapminder_country,2))
)

dataPanel = tabPanel("Data",
                     tableOutput("dataTable")
)

plotPanel = tabPanel("Plot",
                     fluidRow(
                         column(width=8,
                                plotOutput("plotData", 
                                           hover = hoverOpts(id = "plot_hover", delayType = "throttle"))
                                ),
                         column(width=4,
                                h2("Info"), 
                                div("Country: ",
                                    textOutput("txtCountry", inline=T)),
                                div("Year: ",
                                    textOutput("txtYear", inline=T)),
                                div("Pop: ",
                                    textOutput("txtPop", inline=T))
                                #,verbatimTextOutput("plot_hoverinfo")
                     )
                     ) #closes fluidRow
                    ) #closes tabPanel
plotlyPanel <- tabPanel("Plotly", 
                        plotly::plotlyOutput("plotlyData")
                        )
mapPanel <- tabPanel("Map", 
                        tableOutput("mapTable")
)
ui <-navbarPage("Shiny App",
                dataPanel, plotPanel, plotlyPanel, mapPanel,
                id= "navBar", 
                header= headerRow
)

server = function(input, output) {
    
    observe(if(input$navBar=="Map"){
        cat(file=stderr(), input$navBar, "/n")
        shinyjs::hide("header")
        }else {
        shinyjs::show("header")
        })
    gapminder_filtered <- reactive({
        req(input$selCountry)
        req(input$selYear)
        gapminder %>% filter(year %in% input$selYear, country %in% input$selCountry)
        })
    
    output$dataTable = renderTable({
        gapminder_filtered()
    })
    output$mapTable = renderTable({
        head(esDB)
    })
    output$plotData = renderPlot({
        gapminder_filtered() %>%
            ggplot(aes(x=country, y=pop, fill=year)) + 
            geom_bar(stat="identity", position=position_dodge())
    })
    output$plotlyData = plotly::renderPlotly({
        gapminder_filtered() %>%
            ggplot(aes(x=country, y=pop, fill=year)) + 
            geom_bar(stat="identity", position=position_dodge())
    })
#    output$plot_hoverinfo <- renderPrint({
#        cat("Hover (throttled):\n")
#        str(input$plot_hover)
#    })
    countryIndex<- reactive({
        req(input$plot_hover$x)
        round(input$plot_hover$x)
        })
    countryname<- reactive({
        req(countryIndex() > 0 & countryIndex()<= length(input$selCountry))
        str_sort(input$selCountry)[countryIndex()]
        })
    output$txtCountry<- renderText(countryname())
    yearIndex<- reactive({
        req(input$plot_hover$x)
        ceiling((input$plot_hover$x-round(input$plot_hover$x)+0.5) * length(input$selYear))
    })
    yearname<- reactive({
        req(yearIndex() > 0 & yearIndex()<= length(input$selYear))
        str_sort(input$selYear)[yearIndex()]
    })
    output$txtYear<- renderText(yearname())
    output$txtPop<- renderText(
        gapminder %>% filter(year== yearname(), country== countryname() ) %>% pull(pop))
}

# Run the application 
shinyApp(ui = ui, server = server)