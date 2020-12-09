library(shiny)
library(shinythemes)
library(here)
library(data.table)
library(forcats)
library(ggplot2)



# set up data ------------------------------------------------------------------

dt <- readRDS(here("data", "data.rds"))
genes <- unique(dt, by = "gene_name")[ ,gene_name]
individuals <- unique(dt, by = "individual")[ ,individual]
populations <- unique(dt, by = "population")[ ,population]
sexes <- unique(dt, by = "sex")[, sex]
metrics <- unique(dt, by = "metric")[, metric]


# set up ui --------------------------------------------------------------------

ui <- fluidPage(
  navbarPage(title = "Coriell GEUVADIS Expression Browser",
             theme = shinytheme("flatly"),
             tabPanel("Browser",
                      pageWithSidebar(
                        headerPanel("Select Data"),
                        sidebarPanel(
                          width = 2,
                          selectizeInput(inputId = "gene",
                                         label = "Enter Gene Symbol",
                                         choices = genes,
                                         selected = "BRCA1",
                                         multiple = FALSE, 
                                         options = list(create = FALSE,
                                                        maxOptions = 50)),
                          selectInput(inputId = "individuals",
                                      label = "Individual(s)",
                                      choices = individuals,
                                      selected = individuals,
                                      size = 10,
                                      selectize = FALSE,
                                      multiple = TRUE),
                          checkboxGroupInput(inputId = "populations",
                                             label = "Populations(s)",
                                             choices = populations,
                                             selected = populations),
                          checkboxGroupInput(inputId = "sexes",
                                      label = "Sex",
                                      choices = sexes,
                                      selected = sexes),
                          selectInput(inputId = "metric",
                                      label = "Count Metric",
                                      choices = metrics,
                                      selected = "CPM",
                                      multiple = FALSE)
                            ),
                        mainPanel = mainPanel(
                          tabsetPanel(
                            id = 'main',
                            tabPanel("Table", DT::dataTableOutput("table"),
                                     downloadButton("downloadData", "Download")),
                            tabPanel("Expression Histogram",
                                     sliderInput("bins", label = "Select number of bins", value = 30, min = 1, max = 462),
                                     selectizeInput("individual",
                                                    "Show expression for:",
                                                    choices = individuals,
                                                    options = list(
                                                      placeholder = "Select an indivual sample",
                                                      onInitialize = I('function() { this.setValue(""); }'))),
                                     plotOutput("hist"),
                                     h3("Distribution Summary"),
                                     verbatimTextOutput("summary")
                                     )
                            )
                          )
                        )
                      ),
             tabPanel("About",
                      h2("Data Collection"),
                      p("Data was downloaded from GEUVADIS..."),
                      hr(),
                      h2("Data Processing"),
                      p("EdgeR was used to..."),
                      hr(),
                      h2("Definitions"),
                      p("Counts Per Million (CPM)...")
                      )
             )
  )


# set up server functions ------------------------------------------------------

server <- function(input, output) {
  
  # set up base reactive table used in most tabsets -----------------
  d <- reactive({
    dt[.(input$gene, input$metric)
       ][individual %in% input$individuals &
         population %in% input$populations &
         sex %in% input$sexes
         ][order(-value)]
  })
  
  # datatable tabset ------------------------------------------------
  output$table <- DT::renderDataTable({ d() })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Coriell-GEUV-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      fwrite(d(), file)
    }
  )
  
  # histogram tabset ------------------------------------------------
  individual_expr <- reactive({ d()[individual == input$individual, value]})
  output$hist <- renderPlot({
    d() %>% 
    ggplot(aes(x = value)) +
      geom_histogram(fill = "steelblue", color = "black", bins = input$bins) +
      geom_vline(xintercept = individual_expr(), linetype = 2, color = "red", size = 3) +
      theme_light() +
      labs(title = paste("Distribution of", input$gene, "Expression"),
           x = input$metric,
           y = "Count")
  })
  
  output$summary <- renderPrint({ summary(d()[, value]) })
}


# run app ----------------------------------------------------------------------

shinyApp(ui = ui, server = server)