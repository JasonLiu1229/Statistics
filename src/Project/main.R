# Import data
data <- read.csv("Dataset/Project/eik.csv")

# Question one
hist(data$Volume)
boxplot(data$Volume)
qqnorm(data$Volume)
qqline(data$Volume)

hist(data$Grootte)
boxplot(data$Grootte)
qqnorm(data$Grootte)
qqline(data$Grootte)
# Question two

# Question three