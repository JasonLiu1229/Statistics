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
data$dikke_eikel <- ifelse(data$Volume >= 3, 1, 0)
mu <- mean(data$dikke_eikel)
t.test(data$dikke_eikel, mu = mu, alternative = "two.sided")
# Question three