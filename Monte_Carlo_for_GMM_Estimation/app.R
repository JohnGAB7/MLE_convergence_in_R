# Load the shiny library
library(shiny)

# Function to calculate the GMM criterion with unit weights
gmm_criterion <- function(theta, X) {
    n <- length(X)
    # Define the g function for the second estimator
    g <- c(mean(X) - theta[1], var(X) - theta[2])
    # Use unit weighting matrix (identity matrix)
    W <- diag(2)
    # Calculate the GMM criterion
    Q <- t(g) %*% W %*% g
    return(Q)
}

# Define UI
ui <- fluidPage(
    titlePanel("Monte Carlo Simulation - MLE vs GMM"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("R", "Number of Simulations:", min = 1, max = 2000, value = 1000),
            sliderInput("n", "Sample Size:", min = 10, max = 1000, value = 500),
            sliderInput("mu0", "True Mean:", min = 0, max = 10, value = 4),
            sliderInput("sigma0", "True Variance:", min = 0.1, max = 10, value = 2)
        ),
        mainPanel(
            plotOutput("mle_mu_plot"),
            plotOutput("mle_sigma_plot"),
            plotOutput("gmm_mu_plot"),
            plotOutput("gmm_sigma_plot"),
            verbatimTextOutput("mle_mse_output"),
            verbatimTextOutput("gmm_mse_output"),
            verbatimTextOutput("mle_params_output"),
            verbatimTextOutput("gmm_params_output")
        )
    )
)

# Define server logic
server <- function(input, output) {
    observe({
        R <- input$R
        n <- input$n
        mu0 <- input$mu0
        sigma0 <- input$sigma0

        results <- matrix(NA, nrow = R, ncol = 4)

        for (r in 1:R) {
            # Generate a sample of size n from N(mu0, sigma0^2)
            X <- rnorm(n, mean = mu0, sd = sqrt(sigma0))

            # Estimate parameters using MLE
            mle_params <- c(mean(X), var(X))

            # Estimate parameters using the second GMM estimator
            gmm_params <- optim(par = c(mu0, sigma0), fn = gmm_criterion, X = X)$par

            # Store results
            results[r, ] <- c(mle_params, gmm_params)
        }

        # Calculate mean squared errors
        MSE <- apply(results, 2, function(x) mean((x - c(mu0, sigma0))^2))

        # Plot MLE for mu
        output$mle_mu_plot <- renderPlot({
            hist(results[, 1], main = "Histogram of MLE (mu)", col = "lightblue", border = "black", xlab = "MLE Estimate (mu)")
            abline(v = mean(results[, 1]), col = "red", lwd = 2)
            legend("topright", legend = paste("True Mean:", mu0), col = "red", lwd = 2)
        })

        # Plot MLE for sigma
        output$mle_sigma_plot <- renderPlot({
            hist(results[, 2], main = "Histogram of MLE (sigma)", col = "lightblue", border = "black", xlab = "MLE Estimate (sigma)")
            abline(v = mean(results[, 2]), col = "red", lwd = 2)
            legend("topright", legend = paste("True variance:", sigma0), col = "red", lwd = 2)
        })

        # Plot GMM for mu
        output$gmm_mu_plot <- renderPlot({
            hist(results[, 3], main = "Histogram of GMM (mu)", col = "lightgreen", border = "black", xlab = "GMM Estimate (mu)")
            abline(v = mean(results[, 3]), col = "red", lwd = 2)
            legend("topright", legend = paste("True Mean:", mu0), col = "red", lwd = 2)
        })

        # Plot GMM for sigma
        output$gmm_sigma_plot <- renderPlot({
            hist(results[, 4], main = "Histogram of GMM (sigma)", col = "lightgreen", border = "black", xlab = "GMM Estimate (sigma)")
            abline(v = mean(results[, 4]), col = "red", lwd = 2)
            legend("topright", legend = paste("True Variance:", sigma0), col = "red", lwd = 2)
        })

        # Display MLE MSE
        output$mle_mse_output <- renderPrint({
            paste("MSE for MLE (mu):", MSE[1], "MSE for MLE (sigma^2):", MSE[2])
        })

        # Display GMM MSE
        output$gmm_mse_output <- renderPrint({
            paste("MSE for GMM (mu):", MSE[3], "MSE for GMM (sigma^2):", MSE[4])
        })

        # Display estimated parameters for MLE
        output$mle_params_output <- renderPrint({
            paste("MLE Estimate (mu):", mean(results[, 1]), "MLE Estimate (sigma^2):", mean(results[, 2]))
        })

        # Display estimated parameters for GMM
        output$gmm_params_output <- renderPrint({
            paste("GMM Estimate (mu):", mean(results[, 3]), "GMM Estimate (sigma^2):", mean(results[, 4]))
        })
    })
}

# Run the application
shinyApp(ui = ui, server = server)
