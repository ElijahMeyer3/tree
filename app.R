library(shiny)

ui <- fluidPage(
  withMathJax(
    div(
      style = "
        background-image: url('forest.png');
        background-size: cover;
        background-position: center;
        min-height: 100vh;
        padding: 20px;
        padding: 20px;
        font-weight: bold;
        font-size: 18px;
        color: black;
      ",
      
      div(
        style = "background-color: rgba(255,255,255,0.75); padding: 20px; border-radius: 10px;",
        
        h2("Tree Girth Sampling — Difference in Means (Oak − Maple)"),
        
        p("To earn full credit on the homework assignment, you must submit a hand-written assignment answering the following questions below AND a screenshot of you interacting with this application in some way."),
        
        p("Goal: Estimate the difference in population mean girth between Oak and Maple trees."),
        
        p("Assumption: You may assume that the girth of each tree type is normally distributed."),
        
        p("Each click generates a random observation. Construct a 90% confidence interval for μ_Oak − μ_Maple."),
        
        tags$ol(
          tags$li("Take separate random samples for Oak and Maple."),
          tags$li("Report your sample means, sample size, and sample standard deviation in proper notation."),
          tags$li("Report your critical value."),
          tags$li("Compute your 90% confidence interval for μ_Oak − μ_Maple."),
          tags$li("Interpret your results in the context of the problem.")
        ),
        
        br(),
        h3("Click trees to generate observations"),
        
        fluidRow(
          column(6,
                 actionButton("oak",
                              label = tags$img(src = "oak.png", height = "150px"),
                              style = "border:none; background:none;")
          ),
          column(6,
                 actionButton("maple",
                              label = tags$img(src = "maple.png", height = "150px"),
                              style = "border:none; background:none;")
          )
        ),
        
        br(),
        
        h4("Oak Sample"),
        tableOutput("oak_table"),
        
        h4("Maple Sample"),
        tableOutput("maple_table"),
        
        br(),
        
        h4("Sample Statistics"),
        verbatimTextOutput("summary_stats"),
        
        br(),
        
        h4("Confidence Interval Formula (90%)"),
        div(
          style = "background-color: white; padding: 10px; border-radius: 8px;",
          "$$
          (\\bar{x}_{oak} - \\bar{x}_{maple})
          \\pm t^* \\sqrt{
            \\frac{s_{oak}^2}{n_{oak}} +
            \\frac{s_{maple}^2}{n_{maple}}
          }
          $$
          "
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  tree_params <- list(
    Oak = list(mean = 85, sd = 8),
    Maple = list(mean = 72, sd = 6)
  )
  
  oak_data <- reactiveVal(
    data.frame(Tree = character(), Girth_cm = numeric())
  )
  
  maple_data <- reactiveVal(
    data.frame(Tree = character(), Girth_cm = numeric())
  )
  
  add_sample <- function(tree_name) {
    params <- tree_params[[tree_name]]
    new_value <- round(rnorm(1, params$mean, params$sd), 1)
    new_row <- data.frame(Tree = tree_name, Girth_cm = new_value)
    
    if (tree_name == "Oak") {
      oak_data(rbind(oak_data(), new_row))
    } else {
      maple_data(rbind(maple_data(), new_row))
    }
  }
  
  observeEvent(input$oak, { add_sample("Oak") })
  observeEvent(input$maple, { add_sample("Maple") })
  
  output$oak_table <- renderTable({
    oak_data()
  })
  
  output$maple_table <- renderTable({
    maple_data()
  })
  
  output$summary_stats <- renderPrint({
    x <- oak_data()$Girth_cm
    y <- maple_data()$Girth_cm
    
    n1 <- length(x)
    n2 <- length(y)
    
    cat("Oak sample size (n₁):", n1, "\n")
    cat("Maple sample size (n₂):", n2, "\n\n")
    
    if (n1 > 0) {
      cat("Oak mean (x̄₁):", round(mean(x), 3), "\n")
    }
    
    if (n2 > 0) {
      cat("Maple mean (x̄₂):", round(mean(y), 3), "\n")
    }
    
    cat("\n")
    
    if (n1 > 1) {
      cat("Oak SD (s₁):", round(sd(x), 3), "\n")
    }
    
    if (n2 > 1) {
      cat("Maple SD (s₂):", round(sd(y), 3), "\n")
    }
    
    if (n1 > 1 & n2 > 1) {
      df <- min(n1, n2) - 1
      t_star <- qt(0.95, df = df)
      
      cat("\nCritical value (t*):", round(t_star, 4), "\n")
    } else {
      cat("\nNeed at least 2 observations per group for t*")
    }
  })
}

shinyApp(ui, server)