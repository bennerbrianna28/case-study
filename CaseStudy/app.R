# libraries
library(vroom)
library(tidyverse)
library(shiny)

# load in the data
if (!exists("injuries")) {
  injuries <- vroom::vroom("injuries.tsv.gz")
  products <- vroom::vroom("products.tsv")
  population <- vroom::vroom("population.tsv")
}


# Define UI 
ui <- fluidPage(
  # give user choice between number of injuries or population standardised rate
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    column(2, selectInput("y", "Y axis", c("rate", "count"))) # choose between rate and count
  ),
  fluidRow(
    column(4, tableOutput("diagnosis")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  # interactively explore narratives
  fluidRow(
    column(2, actionButton("story", "Tell me a story")), # action button
    column(10, textOutput("narrative")) # text output
  ),
  # provide definitions for common abbreviations
  h2(strong("Common abbreviations")),
  tags$p("The stories use a variety of abbreviations to describe the patient, their injury, the diagnosis, and treatment. Several common abbreviations include:"),
  tags$ul(
    tags$li("DX = diagnosis"),
    tags$li("FX = fracture"),
    tags$li("YOM/F = year old male/female"),
    tags$li("C/O = complains of"),
    tags$li("LAC = laceration"),
    tags$li("ABD = abdomen"),
    tags$li("STR/SPR = strain/sprain"),
    tags$li("SP = status post")
  )
)


# find the 5 most common injuries by count
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

# Define server logic required to draw a histogram
server <- function(input, output) {

  selected <- reactive(injuries %>% filter(prod_code %in% input$code))
  
  output$diagnosis <- renderTable(count_top(selected(), diag), width = "100%",
                                  caption = paste("Diagnosis"))
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%",
                                  caption = paste("Body part inured"))
  output$location <- renderTable(count_top(selected(), location), width = "100%",
                                 caption = paste("Where patient was when injury occured"))
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })

  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 5)) +
        coord_cartesian(xlim = c(0, 84)) +
        theme_bw()
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 5)) +
        coord_cartesian(xlim = c(0, 84)) +
        theme_bw()
    }
  }, res = 96)
  
  # create a reactive that only updates when action button is pushed 
  # or underlying data changes
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  output$narrative <- renderText(narrative_sample())
}

# Run the application 
shinyApp(ui = ui, server = server)
