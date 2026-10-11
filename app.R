# Make prerequisite imports
library(shiny)
library(bslib)
library(car)
library(moments)
library(psych)
library(pROC)

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
        choices = unique(c("0-9" = "0-9"))
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
    class = "container my-4", style = "max-width: 720px",
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
    )
    )
)

# Implemented server logic
server <- function(input, output, session) {
    modelFit <- reactive({
        req(input$csv)
        fitModel(processData(input$csv$datapath))
    })
    
    observeEvent(modelFit(), {
        ml <- modelLevels <- modelFit()$xlevels$drivingExperience
        updateSelectInput(session, "drivingExperience", choices = ml)
    })
    
    pricingResult <- reactive({
        factorLevels <- lapply(names(characteristicsLabels), function(term) input[[term]])
        names(factorLevels) <- names(characteristicsLabels)

        do.call(calculatePremium, c(factorLevels))
    })

    output$finalPremium <- renderText({
        paste0("$", format(round(pricingResult()$finalPremium, 2), big.mark = ",", nsmall = 2))
    })
}

# Run
shinyApp(ui, server)
