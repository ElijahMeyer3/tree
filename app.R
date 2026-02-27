library(shiny)

ui <- fluidPage(
  
  titlePanel("Tree Girth Sampling"),
  
  h3("Click a tree to generate a new girth observation"),
  
  fluidRow(
    column(4,
           actionButton(
             "oak",
             label = tags$img(src = "oak.png", height = "200px"),
             style = "border:none; background:none;"
           )
    ),
    
    column(4,
           actionButton(
             "pine",
             label = tags$img(src = "pine.png", height = "200px"),
             style = "border:none; background:none;"
           )
    ),
    
    column(4,
           actionButton(
             "maple",
             label = tags$img(src = "maple.png", height = "200px"),
             style = "border:none; background:none;"
           )
    )
  ),
  
  br(),
  h4("All Observed Girths:"),
  tableOutput("girth_table")
)

server <- function(input, output, session) {
  
  # Store all samples in a data frame
  sampled_data <- reactiveVal(
    data.frame(Tree = character(),
               Girth_cm = numeric(),
               stringsAsFactors = FALSE)
  )
  
  # Function to add a new sample
  add_sample <- function(tree_name, mean, sd) {
    new_value <- round(rnorm(1, mean, sd), 1)
    new_row <- data.frame(Tree = tree_name,
                          Girth_cm = new_value)
    sampled_data(rbind(sampled_data(), new_row))
  }
  
  observeEvent(input$oak, { add_sample("Oak", 85, 8) })
  observeEvent(input$pine, { add_sample("Pine", 60, 5) })
  observeEvent(input$maple, { add_sample("Maple", 72, 6) })
  
  output$girth_table <- renderTable({
    sampled_data()
  })
}

shinyApp(ui, server)