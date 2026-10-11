library(car)
library(psych)
library(pROC)

characteristics <- c("id",
                     "age", "gender", "isMarried", "education", "hasChildren",
                     "drivingExperience", "creditScore", "speedingViolations", "DUIs", "pastAccidents",
                     "vehicleType", "isVehicleBefore2015", "isVehicleOwner",
                     "isClaimRecorded")

processData <- function(path) {

  claims <- read.csv(path)

  # Remove ID
  claims <- claims[, -1]

  # LLM appeared to occasionally assert missing credit score as 0 (none in data are strictly 0), change these to NA
  claims$creditScore[claims$creditScore == 0] <- NA

  # Check NAs
  predictorsNA <- sapply(1:13, function(x) { length(which(is.na(claims[, x]))) })
  names(predictorsNA) <- names(claims)[1:13]

  # Encode two-level categorical variables as binary
  claims$gender <- ifelse(claims$gender == "female", 1, 0)
  claims$vehicleType <- ifelse(claims$vehicleType == "sports car", 1, 0)

  # Encode remaining categorical variables as factors
  claims$age               <- as.factor(claims$age)
  claims$education         <- as.factor(claims$education)
  claims$drivingExperience <- as.factor(claims$drivingExperience)
  
  claims
}

testCreditScore <- function(claims) {
  claims.culled = claims[!is.na(claims$creditScore), ]

  # Fit with creditScore (full model)
  model.full <- glm(isClaimRecorded ~ age + gender + education + drivingExperience + vehicleType + isMarried + hasChildren + isVehicleBefore2015 + isVehicleOwner + creditScore + speedingViolations + DUIs + pastAccidents,
                  family = binomial(link = "logit"),
                  data = claims.culled)

  # Fit without creditScore
  model.noCredit <- glm(isClaimRecorded ~ age + gender + education + drivingExperience + vehicleType + isMarried + hasChildren + isVehicleBefore2015 + isVehicleOwner + speedingViolations + DUIs + pastAccidents,
                      family = binomial(link = "logit"),
                      data = claims.culled)

  # Observe that creditScore is not a strong predictor and the only source of NAs, reasonable to exclude (not associated with other variables where PCA doesn't need to be performed etc)
  list (
    model1 = summary(model.full),
    model2 = summary(model.noCredit),
    lrt = anova(model.noCredit, model.full, test = "Chisq")
  )
}

fitModel <- function(claims) {

  model <- glm(isClaimRecorded ~ age + gender + education + drivingExperience + vehicleType + isMarried + hasChildren + isVehicleBefore2015 + isVehicleOwner + speedingViolations + DUIs + pastAccidents,
             family = binomial(link = "logit"),
             data = claims)
  
  model
}

fitFinalModel <- function(claims) {
  model <- glm(isClaimRecorded ~ gender + drivingExperience + isMarried + hasChildren + isVehicleBefore2015 + isVehicleOwner,
               family = binomial(link = "logit"),
               data = claims)
  
  model
}

writeCoefficients <- function(model, path = "data/coefficients.csv") {
    selected <- coef(model)
    output <- data.frame(term = names(selected), factor_level = "",
                    estimate = unname(selected), stringsAsFactors = FALSE)
  
    for (t in c("gender", "drivingExperience", "isMarried", "hasChildren",
              "isVehicleBefore2015", "isVehicleOwner", "pastAccidents")) {
      idx <- startsWith(output$term, t)
      output$factor_level[idx] <- substring(output$term[idx], nchar(t) + 1)
      output$term[idx] <- t
    }
  
  write.csv(output, path, row.names = FALSE)
}

testMulticollinearity <- function(claims) {
  list (
    vif = vif(fitModel(claims)),
    kmo = KMO(claims[, c("speedingViolations", "DUIs", "pastAccidents")])
  )
}

performModelSelection <- function(claims) {
  set.seed(1)
  stepwiseAIC <- step(model, direction = "backward", k = 2, trace = 0)
  stepwiseBIC <- step(model, direction = "backward", k = log(nrow(claims)), trace = 0)
  summary(stepwiseAIC)
  summary(stepwiseBIC)

  list(
    aicFormula = formula(stepwiseAIC),
    bicFormula = formula(stepwiseBIC),
    lrtAIC = anova(stepwiseAIC, model, test = "Chisq"),
    lrtBIC = anova(stepwiseBIC, model, test = "Chisq"),

    model = glm(isClaimRecorded ~ gender + drivingExperience + isMarried + hasChildren + isVehicleBefore2015 + isVehicleOwner + pastAccidents,
             family = binomial(link = "logit"),
             data = claims)
  )
}

getResidualLeverageInfluence <- function(model, claims) {
  residuals.deviance <- residuals(model, type = "deviance")
  residuals.pearson  <- residuals(model, type = "pearson")

  dispersionRatio    <- summary(model)$deviance / summary(model)$df.residual

  leverageValues     <- hatvalues(model)
  cooksDistance      <- cooks.distance(model)

  hlpThreshold       <- 3 * (length(coef(model)) - 1 + 1) / nrow(claims)
  cooksThreshold     <- 4 / nrow(claims)

  highLeverage       <- which(leverageValues > hlpThreshold)

  highInfluence      <- which(cooksDistance  > cooksThreshold)
  
  list (
    dispersion = dispersionRatio,
    leverage = leverageValues,
    hlpThreshold = hlpThreshold,
    cooksThreshold = cooksThreshold,
    highLeverage = highLeverage,
    highInfluence = highInfluence
  )
}

getTestPerformance <- function(claims) {
  set.seed(1)
  trainSample  <- sample(nrow(claims), nrow(claims) * 0.7)

  claims.train <- claims[ trainSample, ]
  claims.test  <- claims[-trainSample, ]

  model.test <- glm(isClaimRecorded ~ gender + drivingExperience + isMarried + hasChildren + isVehicleBefore2015 + isVehicleOwner + pastAccidents,
                  family = binomial(link = "logit"),
                  data = claims.train)

  predictions <- predict(model, newdata = claims.test, type = "response")

  rocScore <- roc(claims.test$isClaimRecorded, predictions)
  AUCROC <- auc(rocScore)

  brierScore <- mean((claims.test$isClaimRecorded - predictions)^2)
  
  list (
    auc = AUCROC,
    brier = brierScore
  )
}
