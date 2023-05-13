# 1.2
data_1_2 <- read_xls("Dataset/Preparations/datajuni(1).xls", sheet = "Sheet1")

# Boxplot
boxplot(data_1_2$TEMPERATUUR)

# Create a QQ plot of the data
qqnorm(data_1_2$TEMPERATUUR)
qqline(data_1_2$TEMPERATUUR)

# Create a histogram of the data
hist(data_1_2$TEMPERATUUR)

# Determine null hypothesis and alternative hypothesis
# The null hypothesis = that the average temperature is 20 degrees in Leuven 2001.
# The alternative hypothesis = that the average temperature is not 20 degrees in Leaven 2001. Two sided hypothesis

# Which test statistics are we using:
# We use the student distribution

# If we use all three plots we can determine the following:
# Histogram:
# It is kinda bell shaped, so we can assume it's normal
# Boxplot:
# If it's semetric around the median, we can also assume it's normal
# QQ-plot:
# We can assume it's normal because it's almost a straight line

t.test(data_1_2$TEMPERATUUR,mu=20,alternative="two.sided")

# Create a normal probability plot
qqplot <- qqnorm(data_1_2$TEMPERATUUR)

# Define the normal distribution function
normal_distribution <- function(x, mu, sigma) {
  (1 / (sigma * sqrt(2 * pi))) * exp(-(x - mu)^2 / (2 * sigma^2))
}

# Define the null hypothesis mean and standard deviation
mu_0 <- 20
# Extract the slope of the line
sigma <- summary(lm(qqplot$y ~ qqplot$x))$coefficients[2, 1]

# Define the test statistic
x <- 1.5

# Calculate the p-value
p_value <- 0.8996

# Draw the normal distribution curve
curve(dnorm(x, mu = mu_0, sigma = sigma), from = -4, to = 4, xlab = "x", ylab = "Density")

# Shade the area corresponding to the p-value
x_values <- seq(x, 4, length.out = 100)
y_values <- dnorm(x_values, mu = mu_0, sigma = sigma)
polygon(c(x_values, rev(x_values)), c(y_values, rep(0, length(y_values))), col = "blue", border = NA)

# Add a legend
legend("topright", legend = paste0("p-value = ", round(p_value, 3)), fill = "blue")