# Make prerequisite imports
library(shiny)
library(bslib)

# Main calculation routine located here
source("src/pricing.R")

getCSVLevels <- function(term) {
    matchedLevels <- coefficients$factor_level[coefficients$term == term]
    setNames(matchedLevels, matchedLevels)
}

characteristics <- list(
    gender = list(
        label = "Gender",
        chartLabel = "Gender",
        choices = c("Male" = "FALSE", "Female" = "TRUE")
    ),
    drivingExperience = list(
        label = "Driving Experience",
        chartLabel = "Experience",
        choices = unique(c("0-9" = "0-9", getCSVLevels("drivingExperience")))
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
            !!!lapply(names(characteristics), function(term) {
            selectInput(term, characteristics[[term]]$label, choices = characteristics[[term]]$choices)
            })
        )
        )
    )
    )
)

# Implemented server logic
server <- function(input, output, session) {
    pricingResult <- reactive({
        factorLevels <- lapply(names(characteristics), function(term) input[[term]])
        names(factorLevels) <- names(characteristics)

        do.call(calculatePremium, c(factorLevels))
    })

    output$finalPremium <- renderText({
        paste0("$", format(round(pricingResult()$finalPremium, 2), big.mark = ",", nsmall = 2))
    })
}

# Run
shinyApp(ui, server)
