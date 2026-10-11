# Coefficients are stored locally, specific entries are retrieved via function
coefficients <- read.csv("data/coefficients.csv", stringsAsFactors = FALSE)
indemnityAmt <- 50000

getCoefficients <- function(t, level = "") {
    if (nzchar(level)) {
        value <- coefficients$estimate[coefficients$term == t & coefficients$factor_level == level]
    } else {
        value <- coefficients$estimate[coefficients$term == t]
    }

    if (length(value) == 0) {
        0
    } else {
        value
    }
}

processData <- function(path) {
    claims <- read.csv(path)
    claims <- claims[, -1]
    claims$creditScore[claims$creditScore == 0] <- NA
  
    claims$gender      <- ifelse(claims$gender == "female", 1, 0)
    claims$vehicleType <- ifelse(claims$vehicleType == "sports car", 1, 0)
  
    claims$age               <- as.factor(claims$age)
    claims$education         <- as.factor(claims$education)
    claims$drivingExperience <- as.factor(claims$drivingExperience)
    claims
}

fitModel <- function(claims) {
    glm(isClaimRecorded ~ gender + drivingExperience + isMarried + hasChildren +
        isVehicleBefore2015 + isVehicleOwner + pastAccidents,
        family = binomial(link = "logit"),
        data = claims)
}

calculatePremium <- function(model, gender, drivingExperience, isMarried, hasChildren, isVehicleBefore2015,
                              isVehicleOwner, pastAccidents, fixedExpense = 50) {

    # Linear predictor (beta_i * x_i)
    predictorSum <- getCoefficients("(Intercept)") +
        as.logical(gender)              * getCoefficients("gender", "TRUE") +
        as.logical(isMarried)           * getCoefficients("isMarried", "TRUE") +
        as.logical(hasChildren)         * getCoefficients("hasChildren", "TRUE") +
        as.logical(isVehicleBefore2015) * getCoefficients("isVehicleBefore2015", "TRUE") +
        as.logical(isVehicleOwner)      * getCoefficients("isVehicleOwner", "TRUE") +
        getCoefficients("drivingExperience", drivingExperience)

    # Apply inverse logit (link function) transformation
    probability <- 1 / (1 + exp(-predictorSum))

    # Simple premium calculation
    purePremium <- probability * 50000
    finalPremium <- purePremium + fixedExpense

    # Return values that will appear on interface
    list (
        predictorSum = predictorSum,
        probability = probability,
        purePremium = purePremium,
        finalPremium = finalPremium
    )
    
    p <- unname(predict(model, newdata, type = "response"))
    list(claimProbability = p, finalPremium = p * indemnityAmt)
}
