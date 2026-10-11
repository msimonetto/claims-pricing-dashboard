# Make prerequisite imports
library(shiny)
library(bslib)
library(car)
library(moments)
library(psych)
library(pROC)
library(readr)

# Main calculation routine located here
source("src/model.R")
source("src/pricing.R")

characteristicsLabels <- list(
    gender = list(
        label = "Gender",
        chartLabel = "Gender",
        choices = c("Male" = "FALSE", "Female" = "TRUE")
    ),
    drivingExperience = list(
        label = "Driving Experience",
        chartLabel = "Experience",
        choices = c("0-9y" = "0-9y", "10-19y" = "10-19y", "20-29y" = "20-29y", "30y+" = "30y+")
    ),
    isMarried = list(
        label = "Marital Status",
        chartLabel = "Marital",
        choices = c("Single" = "FALSE", "Married" = "TRUE")
    ),
    hasChildren = list(
        label = "Has Children or Dependents",
        chartLabel = "Dependents",
        choices = c("No" = "FALSE", "Yes" = "TRUE")
    ),
    isVehicleBefore2015 = list(
        label = "Vehicle Manufacturing Date",
        chartLabel = "Vehicle Era",
        choices = c("After 2015" = "FALSE", "Before/At 2015" = "TRUE")
    ),
    isVehicleOwner = list(
        label = "Vehicle Ownership",
        chartLabel = "Ownership",
        choices = c("Renter (or Other)" = "FALSE", "Owner" = "TRUE")
    )
)

# Define UI components
ui <- page_fluid(
    theme = bs_theme(
    bootswatch = "shiny",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
    ),
    sidebarLayout(
      sidebarPanel(
        fileInput("uploadCsv", "Choose CSV File", accept = ".csv")
      ),
      mainPanel(
        tableOutput("data_table")
      )
    ),
    div(
    class = "container my-4", style = "max-width: 960px",
    div(
        class = "text-center mb-4",
        h3("Single-Claim Insurance Pricing Engine", class = "fw-bold tracking-tight"),
        p("Policy: Single-claim fixed indemnity policy of $50,000", class = "text-muted small")
    ),
    value_box(
        title = "Estimated Annual Premium",
        value = textOutput("finalPremium"),
        showcase = bsicons::bs_icon("shield-check"),
        theme = "primary",
        class = "mb-3 shadow-sm"
    ),
    card(
      class = "shadow-sm",
      card_header(strong("Rating Factors")),
      card_body(
        layout_columns(
          col_widths = 6,
          gap = "1rem",
          !!!lapply(names(characteristicsLabels), function(term) {
            selectInput(term, characteristicsLabels[[term]]$label, choices = characteristicsLabels[[term]]$choices)
          })
        )
      )
    ),
    div(
      class = "mt-3 shadow-sm",
      navset_card_tab(
        title = "Model Statistics",
        nav_panel("Premium Breakdown", tableOutput("premiumTable")),
        nav_panel("Coefficients", tableOutput("coefTable")),
        nav_panel("Performance",
                  layout_columns(
                    col_widths = 4,
                    value_box("Test AUC", textOutput("aucValue")),
                    value_box("Brier Score", textOutput("brierValue")),
                    value_box("Dispersion", textOutput("dispersionValue"))
                  )
        ),
        nav_panel("Diagnostics",
                  tableOutput("diagTable"),
                  plotOutput("cooksPlot", height = "300px")
        ),
        nav_panel("Model Selection", verbatimTextOutput("selectionOut")),
        nav_panel("Variable Checks", verbatimTextOutput("checksOut"))
      )
    )
    )
)

# Implemented server logic
server <- function(input, output, session) {
    processedData <- reactive({
      req(input$uploadCsv)
      processData(input$uploadCsv$datapath)
    })
    
    modelFit <- reactive({
      req(processedData())
      model <- fitFinalModel(processedData())
      writeCoefficients(model)
      model
    })
    
    pricingResult <- reactive({
        req(modelFit())
        factorLevels <- lapply(names(characteristicsLabels), function(term) input[[term]])
        names(factorLevels) <- names(characteristicsLabels)

        do.call(calculatePremium, c(factorLevels))
    })
    
    safely <- function(expr) tryCatch(expr, error = function(e) paste("Error:", conditionMessage(e)))
    part   <- function(x, name) if (is.list(x)) x[[name]] else x
    
    modelStats <- reactive({
      req(modelFit())
      claims <- processedData()
      model  <- modelFit()
      list(
        credit       = safely(testCreditScore(claims)),
        collinearity = safely(testMulticollinearity(claims)),
        selection    = safely(performModelSelection(claims)),
        diagnostics  = safely(getResidualLeverageInfluence(model, claims)),
        performance  = safely(getTestPerformance(claims))
      )
    })

    output$finalPremium <- renderText({
        paste0("$", format(round(pricingResult()$finalPremium, 2), big.mark = ",", nsmall = 2))
    })
}

# Run
shinyApp(ui, server)
