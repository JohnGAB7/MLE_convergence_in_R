# Install and load necessary packages
# Packages already installed

library(shiny)
library(ggplot2)

# Function to calculate the MLE for mean (mu) given a dataset
calculate_mle <- function(data) {
    n <- length(data)
    mle <- mean(data)
    return(mle)
}

# Define the UI
ui <- fluidPage(
    titlePanel("MLE Convergence App"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("sample_size", "Choose Sample Size:",
                        min = 10, max = 10000, value = 50, step = 10)
        ),
        mainPanel(
            plotOutput("boxplot")
        )
    )
)

# Define the server
server <- function(input, output) {
    observe({
        n <- input$sample_size
        mle_results <- replicate(500, calculate_mle(rnorm(n, mean = 0, sd = 1)))
        mle_df <- data.frame(SampleSize = rep(n, 500), MLE = mle_results)

        output$boxplot <- renderPlot({
            ggplot(mle_df, aes(x = 1, y = MLE)) +
                geom_boxplot(color = "gray7", fill = "gray") +
                labs(title = "Convergence of MLE for Mean",
                     x = "Sample Size (n)",
                     y = "MLE") +
                theme_minimal()
        })
    })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
