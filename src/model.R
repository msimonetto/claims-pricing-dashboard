characteristics <- c("id",
                     "age", "gender", "isMarried", "education", "hasChildren",
                     "drivingExperience", "creditScore", "speedingViolations", "DUIs", "pastAccidents",
                     "vehicleType", "isVehicleBefore2015", "isVehicleOwner",
                     "isClaimRecorded")
processData <- function() {

  claims <- read.csv("~/data/claims.csv")

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
}

testCreditScore <- function() {
  claims.culled = claims[-which(is.na(claims$creditScore)), ]

  # Fit with creditScore (full model)
  model.full <- glm(isClaimRecorded ~ age + gender + education + drivingExperience + vehicleType + isMarried + hasChildren + isVehicleBefore2015 + isVehicleOwner + creditScore + speedingViolations + DUIs + pastAccidents,
                  family = binomial(link = "logit"),
                  data = claims.culled)

  # Fit without creditScore
  model.noCredit <- glm(isClaimRecorded ~ age + gender + education + drivingExperience + vehicleType + isMarried + hasChildren + isVehicleBefore2015 + isVehicleOwner + speedingViolations + DUIs + pastAccidents,
                      family = binomial(link = "logit"),
                      data = claims.culled)

  # Observe that creditScore is not a strong predictor and the only source of NAs, reasonable to exclude (not associated with other variables where PCA doesn't need to be performed etc)
  summary(model.noCredit)

  anova(model.noCredit, model.full, test = "Chisq")
}

model <- glm(isClaimRecorded ~ age + gender + education + drivingExperience + vehicleType + isMarried + hasChildren + isVehicleBefore2015 + isVehicleOwner + speedingViolations + DUIs + pastAccidents,
             family = binomial(link = "logit"),
             data = claims)

testMulticollinearity <- function() {
  collinearity <- vif(model)
  kmoStat <- KMO(claims[, c("speedingViolations", "DUIs", "pastAccidents")])
}

performModelSelection <- function() {
  set.seed(1)
  stepwiseAIC <- step(model, direction = "backward", k = 2, trace = 0)
  stepwiseBIC <- step(model, direction = "backward", k = log(nrow(claims)), trace = 0)
  summary(stepwiseAIC)
  summary(stepwiseBIC)

  anova(stepwiseAIC, model, test = "Chisq")
  anova(stepwiseBIC, model, test = "Chisq")

  model <- glm(isClaimRecorded ~ gender + drivingExperience + isMarried + hasChildren + isVehicleBefore2015 + isVehicleOwner + pastAccidents,
             family = binomial(link = "logit"),
             data = claims)
}

getResidualLeverageInfluence <- function() {
  residuals.deviance <- residuals(model, type = "deviance")
  residuals.pearson  <- residuals(model, type = "pearson")

  dispersionRatio    <- summary(model)$deviance / summary(model)$df.residual

  leverageValues     <- hatvalues(model)
  cooksDistance      <- cooks.distance(model)

  hlpThreshold       <- 3 * (length(coef(model)) - 1 + 1) / nrow(claims)
  cooksThreshold     <- 4 / nrow(claims)

  highLeverage       <- which(leverageValues > hlpThreshold)

  highInfluence      <- which(cooksDistance  > cooksThreshold)
}

getTestPerformance <- function() {
  set.seed(1)
  trainSample  <- sample(nrow(df), nrow(df) * 0.7)

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
    AUCROC,
    brierScore
  )
}
