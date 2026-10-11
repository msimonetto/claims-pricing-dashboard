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
    div(
      class = "container mt-4", style = "max-width: 960px",
      div(
        class = "d-flex justify-content-center",
        fileInput("uploadCsv", "Choose CSV File", accept = ".csv", width = "320px")
      ),
      div(class = "table-responsive", tableOutput("data_table"))
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
    
    observeEvent(processedData(), {
      lv <- levels(processedData()$drivingExperience)
      updateSelectInput(session, "drivingExperience", choices = lv, selected = lv[1])
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
    
    output$premiumTable <- renderTable({
      r <- pricingResult()
      data.frame(
        Component = c("Linear predictor", "Claim probability", "Pure premium", "Final premium (incl. expense)"),
        Value = c(sprintf("%.4f", r$predictorSum), sprintf("%.2f%%", 100 * r$probability),
                  sprintf("$%.2f", r$purePremium), sprintf("$%.2f", r$finalPremium))
      )
    })
    
    output$coefTable <- renderTable({
      s <- summary(modelFit())$coefficients
      data.frame(Term = rownames(s), Estimate = s[, 1], `Std. Error` = s[, 2],
                 `p-value` = s[, 4], `Odds Ratio` = exp(s[, 1]), check.names = FALSE)
    }, digits = 4)
    
    output$aucValue <- renderText({
      p <- modelStats()$performance
      validate(need(is.list(p), p))
      as.character(round(as.numeric(p$auc), 3))
    })
    
    output$brierValue <- renderText({
      p <- modelStats()$performance
      validate(need(is.list(p), p))
      as.character(round(p$brier, 3))
    })
    
    output$dispersionValue <- renderText({
      d <- modelStats()$diagnostics
      validate(need(is.list(d), d))
      as.character(round(d$dispersion, 3))
    })
    
    output$diagTable <- renderTable({
      d <- modelStats()$diagnostics
      validate(need(is.list(d), d))
      data.frame(
        Metric = c("Dispersion ratio", "Leverage threshold", "Cook's D threshold",
                   "High-leverage points", "High-influence points"),
        Value = as.character(c(round(d$dispersion, 3), signif(d$hlpThreshold, 3),
                               signif(d$cooksThreshold, 3), length(d$highLeverage),
                               length(d$highInfluence)))
      )
    })
    
    output$cooksPlot <- renderPlot({
      d <- cooks.distance(modelFit())
      plot(d, type = "h", xlab = "Observation", ylab = "Cook's distance")
      abline(h = 4 / length(d), lty = 2, col = "red")
    })
    
    output$selectionOut <- renderPrint({
      s <- modelStats()$selection
      validate(need(is.list(s), s))
      cat("AIC-selected model:\n"); print(s$aicFormula)
      cat("\nBIC-selected model:\n"); print(s$bicFormula)
      cat("\nLRT: AIC model vs full\n"); print(s$lrtAIC)
      cat("\nLRT: BIC model vs full\n"); print(s$lrtBIC)
    })
    
    output$checksOut <- renderPrint({
      s <- modelStats()
      cat("Credit score LRT (rows with a credit score only)\n")
      print(part(s$credit, "lrt"))
      cat("\nVIF (full model)\n")
      print(part(s$collinearity, "vif"))
      cat("\nKMO (speedingViolations, DUIs, pastAccidents)\n")
      print(part(s$collinearity, "kmo"))
    })
}

# Run
shinyApp(ui, server)
