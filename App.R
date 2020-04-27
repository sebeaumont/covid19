library(shiny)
source("jh-cov2-sars-functions.R")

initial <- c("US", "China", "United Kingdom","Germany", "Italy", "France", "Sweden")
base_data <- ensure_data() %>% calculate_population_stats(get_population_table())
countries <- base_data %>% get_countries()

##
## called once per hit hence reactive functions to apply transformations
##

server <- function (input, output, session) {

    growth_stats <- reactive({
        base_data %>% apply_growth_function(safe_ratio)
    })

    filtered_by_caseload <- reactive({
        growth_stats() %>% get_significant_caseload(input$threshold)
    })

    starting_from <- reactive({
        filtered_by_caseload() %>% filter(date > as.Date(input$startDate))
    })

    selected_by_countries <- reactive({
        starting_from() %>% filter(country %in% input$countries)
    })

    ## observe({
    ##     selectable_countries <- starting_from() %>% get_countries()
    ##     updateSelectInput(session, "countries", "Countries",
    ##                       choices = selectable_countries)
    ## })
    
    output$plot <- renderPlot({
        plot_confirmed_cases_growth(selected_by_countries())
    })
}

##
## user controls and layout
##

ui <- fluidPage(

    titlePanel("SARS-CoV2-Pandemic"),

    sidebarLayout(

        sidebarPanel(
            
            helpText("Select (up to 8) countries to compare for confirmed case growth rates."),

            dateInput("startDate", "Starting Date", "2020-02-14"),
            
            sliderInput("threshold", "Starting Cases", 50, 500, 100),
            
            selectInput("countries", "Countries", countries, selected=initial, multiple=TRUE)

        ),

        mainPanel(plotOutput("plot"))
    )
)

##
## start application
##

shinyApp(ui, server)
    
