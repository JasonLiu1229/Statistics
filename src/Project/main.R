# Import data
data <- read.csv("Dataset/Project/eik.csv")

# Question one
hist(data$Volume)

hist(data$Grootte)

# Question two
data$dikke_eikel <- ifelse(data$Volume >= 3, 1, 0)
hist(data$dikke_eikel, breaks = c(-0.5, 0.5, 1.5), main = "Histogram of thick acorns", xlab = "Thickness acorns", ylab = "Frequency")
result <- chisq.test(data$dikke_eikel, data$Regio, p = 0.05, correct = FALSE)
print(result)

thick_acorns <- subset(data, Volume >= 3)
acorn_counts <- table(thick_acorns$Regio)
barplot(acorn_counts,
         main = "Number of Thick Acorns in California and Atlantic",
         xlab = "Region",
         ylab = "Count",
         col = c("skyblue", "lightgreen"),
         legend = c("California", "Atlantic"))
# Question three
data$log_volume <- log(data$Volume)

# Create a scatter plot
plot(data$log_volume, data$Hoogte,
     xlab = "log(Volume)", ylab = "Hoogte",
     main = "Scatter Plot of log(Volume) vs Hoogte")

model <- lm(Hoogte ~ log_volume, data = data)

abline(model, col = "red")

correlation <- cor(data$log_volume, data$Hoogte)
print(correlation)

library(caret)
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$log_volume, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

model <- lm(Hoogte ~ log_volume, data = trainData)

predictions <- predict(model, newdata = testData)
rmse <- sqrt(mean((testData$Hoogte - predictions)^2))
coefficients <- coef(model)
print(coefficients)