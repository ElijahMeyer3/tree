library(shiny)

ui <- fluidPage(
  tabsetPanel(id = "main_tabs",
              
              # --- Tree Girth Tab ---
              tabPanel("Tree Girth Sampling",
                       div(
                         style = "
          background-image: url('forest.png');
          background-size: cover;
          background-position: center;
          min-height: 100vh;
          padding: 20px;
          font-weight: bold;
          font-size: 18px;
          color: black;",
                         
                         # Semi-transparent overlay for text readability
                         div(style = "background-color: rgba(255,255,255,0.7); padding: 20px; border-radius: 10px;",
                             
                             h3("Assignment Instructions:"),
                             p("Your goal is to estimate the population mean girth of ALL trees in the forest. However, there are 1000s of trees in the forest, and you don't have time to sample all of them."),
                             p("You must submit the following as part of your homework-1. This is a hand-written assignment you will turn in on Gradescope. Label each question (forest vs skittles) and question part (1, 2, 3) appropriately on your assignment for full credit. I also want you to upload a screenshot of you collecting data from the app on Gradescope."),
                             p("Note, if you refresh your current page, you will lose your sample!"),
                             p("This homework assignment was prepared by Dr. Meyer."),
                             p("For this assignment, you may estimate the value of σ = 7, but do not assume you know the distribution of your parent distribution X."),
                             tags$ol(
                               tags$li("Discuss how you could take a random sample of trees from the forest."),
                               tags$li("Discuss what a convenience sample would look like, and how that could cause sampling bias in your results."),
                               tags$li("Take your sample! Report your sample size in proper notation."),
                               tags$li("Calculate your sample mean. Report this using proper notation."),
                               tags$li("Are you justified to calculate a confidence interval with your data? Justify your answer."),
                               tags$li("Regardless of your answer to the last question, report a 99% confidence interval."),
                               tags$li("Interpret your confidence interval in the context of the problem.")
                             ),
                             br(),
                             h3("Click a tree to generate a new girth observation"),
                             fluidRow(
                               column(4, actionButton("oak", label = tags$img(src = "oak.png", height = "150px"), style = "border:none; background:none;")),
                               column(4, actionButton("pine", label = tags$img(src = "pine.png", height = "150px"), style = "border:none; background:none;")),
                               column(4, actionButton("maple", label = tags$img(src = "maple.png", height = "150px"), style = "border:none; background:none;"))
                             ),
                             br(),
                             h4("All Observed Girths:"),
                             tableOutput("girth_table")
                         )
                       )
              ),
              
              # --- Purple Skittles Tab ---
              tabPanel("Purple Skittles Sampling",
                       div(
                         style = "
          background-image: url('factory.png');
          background-size: cover;
          background-position: center;
          min-height: 100vh;
          padding: 20px;
          font-weight: bold;
          font-size: 18px;
          color: black;",
                         
                         # Semi-transparent overlay for text readability
                         div(style = "background-color: rgba(255,255,255,0.7); padding: 20px; border-radius: 10px;",
                             
                             h3("Assignment Instructions:"),
                             p("Your goal is to estimate the population proportion of purple skittles in skittles bags. However, you can't realistically calculate this information, so you plan to take a sample!"),
                             p("You must submit the following as part of your homework-1:"),
                             tags$ol(
                               tags$li("Discuss how you could take a random sample of skittles from a skittles factory. Hint: Think about the different designs we have discussed in class."),
                               tags$li("Discuss what a convenience sample would look like, and how that could cause sampling bias in your results."),
                               tags$li("Take your sample! Report your sample size in proper notation."),
                               tags$li("Calculate your sample proportion. Report this using proper notation."),
                               tags$li("Are you justified to calculate a confidence interval with your data? Justify your answer."),
                               tags$li("Regardless of your answer to the last question, report a 90% confidence interval."),
                               tags$li("Interpret your confidence interval in the context of the problem.")
                             ),
                             br(),
                             h3("Click the Skittle to sample if the candy is purple (Yes/No)"),
                             fluidRow(
                               column(12, actionButton("sample_skittle", label = tags$img(src = "skittle.png", height = "150px"), style = "border:none; background:none;"))
                             ),
                             br(),
                             h4("Samples:"),
                             tableOutput("skittle_samples")
                         )
                       )
              )
  )
)

server <- function(input, output, session) {
  
  ## Tree Girth logic
  tree_params <- list(
    Oak = list(mean = 85, sd = 8),
    Pine = list(mean = 60, sd = 5),
    Maple = list(mean = 72, sd = 6)
  )
  
  sampled_data <- reactiveVal(data.frame(Tree = character(), Girth_cm = numeric(), stringsAsFactors = FALSE))
  
  add_sample <- function(tree_name) {
    params <- tree_params[[tree_name]]
    new_value <- round(rnorm(1, mean = params$mean, sd = params$sd), 1)
    new_row <- data.frame(Tree = tree_name, Girth_cm = new_value)
    sampled_data(rbind(sampled_data(), new_row))
  }
  
  observeEvent(input$oak, { add_sample("Oak") })
  observeEvent(input$pine, { add_sample("Pine") })
  observeEvent(input$maple, { add_sample("Maple") })
  
  output$girth_table <- renderTable({
    sampled_data()
  })
  
  ## Purple Skittles logic
  true_proportion <- 0.3
  skittle_samples <- reactiveVal(data.frame(Sample_Number = integer(), Purple = character(), stringsAsFactors = FALSE))
  
  observeEvent(input$sample_skittle, {
    is_purple <- ifelse(runif(1) < true_proportion, "Yes", "No")
    current <- skittle_samples()
    new_sample <- data.frame(Sample_Number = nrow(current) + 1, Purple = is_purple)
    skittle_samples(rbind(current, new_sample))
  })
  
  output$skittle_samples <- renderTable({
    skittle_samples()
  })
}

shinyApp(ui, server)