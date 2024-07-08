hopping_data <- read.csv("C:/Users/ABDUL/Downloads/archive (1)/Shopping_data.csv", header=FALSE)
View(Shopping_data)
summary(Shopping_data)
colnames(Shopping_data)
colSums(Shopping_data)

Shopping_data$V1=as.numeric(Shopping_data$V1)
Shopping_data$V2=as.numeric(Shopping_data$V2)
Shopping_data$V3=as.numeric(Shopping_data$V3)
Shopping_data$V4=as.numeric(Shopping_data$V4)
Shopping_data$V5=as.numeric(Shopping_data$V5)

colSums(Shopping_data)
hist(Shopping_data$V5)
summary(Shopping_data$V5)
kurtosis(Shopping_data$V5)
pie(Shopping_data)
Shopping_data <- read.csv("C:/Users/ABDUL/Downloads/archive (1)/Shopping_data.csv", header=FALSE)
Shopping_data$V1=as.numeric(Shopping_data$V1)
Shopping_data$V2=as.numeric(Shopping_data$V2)
Shopping_data$V3=as.numeric(Shopping_data$V3)
Shopping_data$V4=as.numeric(Shopping_data$V4)
Shopping_data$V5=as.numeric(Shopping_data$V5)
pie(Shopping_data)
data.frame(Shopping_data$V2)
pie(C()Shopping_data)
negatives <- Shopping_data < 0
Shopping_data[negatives] <- 0
pie(Shopping_data)
cor(Shopping_data)
# Load the dataset
data <- data.frame(
  CustomerID = 2:25,
  Genre = c("Male", "Male", "Female", "Female", "Female", "Female", "Female", "Female", "Male", "Female", 
            "Male", "Male", "Female", "Female", "Male", "Male", "Female", "Male", "Male", "Female", 
            "Male", "Male", "Female", "Male"),
  Age = c(19, 21, 20, 23, 31, 22, 35, 23, 64, 30, 67, 35, 58, 24, 37, 22, 35, 20, 52, 35, 35, 25, 46, 31),
  Annual_Income = c(15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 19, 19, 20, 20, 20, 20, 21, 21, 23, 23, 24, 24, 25, 25),
  Spending_Score = c(39, 81, 6, 77, 40, 76, 6, 94, 3, 72, 14, 99, 15, 77, 13, 79, 35, 66, 29, 98, 35, 73, 5, 73)
)

# Summary statistics for numerical variables
summary_data <- summary(data[, c("Age", "Annual_Income", "Spending_Score")])

# Count and distribution for categorical variable
genre_count <- table(data$Genre)

# Output summary statistics
print("Summary Statistics for Numerical Variables:")
print(summary_data)
data=as.numeric(data)

print("Count and Distribution for Categorical Variable (Genre):")
print(genre_count)
negatives <-data < 0

# Remove negative values or set them to zero
data[negatives] <- 0

# Create pie chart
pie(data)
correlation_matrix <- cor(data[, c("Age", "Annual_Income", "Spending_Score")])

# Print correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)
# Load the dataset
data <- data.frame(
  CustomerID = 2:25,
  Genre = c("Male", "Male", "Female", "Female", "Female", "Female", "Female", "Female", "Male", "Female", 
            "Male", "Male", "Female", "Female", "Male", "Male", "Female", "Male", "Male", "Female", 
            "Male", "Male", "Female", "Male"),
  Age = c(19, 21, 20, 23, 31, 22, 35, 23, 64, 30, 67, 35, 58, 24, 37, 22, 35, 20, 52, 35, 35, 25, 46, 31),
  Annual_Income = c(15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 19, 19, 20, 20, 20, 20, 21, 21, 23, 23, 24, 24, 25, 25),
  Spending_Score = c(39, 81, 6, 77, 40, 76, 6, 94, 3, 72, 14, 99, 15, 77, 13, 79, 35, 66, 29, 98, 35, 73, 5, 73)
)

# Compute correlation matrix
correlation_matrix <- cor(data[, c("Age", "Annual_Income", "Spending_Score")])

# Print correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)
# Load the dataset
data <- data.frame(
  CustomerID = 2:25,
  Genre = c("Male", "Male", "Female", "Female", "Female", "Female", "Female", "Female", "Male", "Female", 
            "Male", "Male", "Female", "Female", "Male", "Male", "Female", "Male", "Male", "Female", 
            "Male", "Male", "Female", "Male"),
  Age = c(19, 21, 20, 23, 31, 22, 35, 23, 64, 30, 67, 35, 58, 24, 37, 22, 35, 20, 52, 35, 35, 25, 46, 31),
  Annual_Income = c(15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 19, 19, 20, 20, 20, 20, 21, 21, 23, 23, 24, 24, 25, 25),
  Spending_Score = c(39, 81, 6, 77, 40, 76, 6, 94, 3, 72, 14, 99, 15, 77, 13, 79, 35, 66, 29, 98, 35, 73, 5, 73)
)

# Compute correlation matrix
correlation_matrix <- cor(data[, c("Age", "Annual_Income", "Spending_Score")])

# Set threshold for weak correlation
threshold <- 0.3

# Find weak correlations
weak_correlations <- which(abs(correlation_matrix) < threshold & correlation_matrix != 1, arr.ind = TRUE)

# Print out the weak correlations
print("Weak Correlations:")
if (length(weak_correlations) > 0) {
  for (i in 1:nrow(weak_correlations)) {
    row <- weak_correlations[i, 1]
    col <- weak_correlations[i, 2]
    print(paste("Variable", row, "and Variable", col, ":", correlation_matrix[row, col]))
  }
} else {
  print("No weak correlations found.")
}
install.packages("gplots")
# Load required library
library(gplots)


# Compute correlation matrix
correlation_matrix <- cor(data[, c("Age", "Annual_Income", "Spending_Score")])

# Plot heatmap
heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),  # Color palette
        symm = TRUE,  # Ensure symmetric
        margins = c(10, 10)  # Add margins
)

# Load required library
library(gplots)

# Compute correlation matrix
correlation_matrix <- cor(data[, c("Age", "Annual_Income", "Spending_Score")])

# Set threshold for strong correlation
threshold <- 0.7

# Find indices of strong correlations
strong_correlations <- which(abs(correlation_matrix) >= threshold & correlation_matrix != 1, arr.ind = TRUE)

# Plot heatmap
heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),  # Color palette
        symm = TRUE,  # Ensure symmetric
        margins = c(10, 10),  # Add margins
        Rowv = NA, Colv = NA,  # Do not show dendrograms
        main = "Strong Correlation Heatmap",  # Title
        cex.main = 1.5  # Title size
)

# Add rectangles to highlight strong correlations
for (i in 1:nrow(strong_correlations)) {
  row <- strong_correlations[i, 1]
  col <- strong_correlations[i, 2]
  rect(col - 0.5, row - 0.5, col + 0.5, row + 0.5, border = "black", lwd = 2)
}
# Calculate average annual income for each gender
average_income <- aggregate(Annual_Income ~ Genre, data = data, FUN = mean)

# Print the result
print(average_income)
Shopping_data <- read.csv("C:/Users/ABDUL/Downloads/archive (1)/Shopping_data.csv", header=FALSE)
# Calculate average spending score for each gender
average_spending <- aggregate(Spending_Score ~ Genre, data = data, FUN = mean)

# Print the result
print(average_spending)
# Load required libraries
library(ggplot2)

# Create a data frame for analysis
analysis_data <- data.frame(
  Genre = c("Female", "Male"),
  Annual_Income = c(19167, 20333),
  Spending_Score = c(50.08, 50.33),
  Savings = c(19116.92, 20282.67)
)

library(ggplot2)
# Plot histograms
# Income histogram
income_plot <- ggplot(analysis_data, aes(x = Genre, y = Annual_Income, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Annual Income by Gender",
       x = "Gender",
       y = "Average Annual Income") +
  theme_minimal()

# Spending score histogram
spending_plot <- ggplot(analysis_data, aes(x = Genre, y = Spending_Score, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Spending Score by Gender",
       x = "Gender",
       y = "Average Spending Score") +
  theme_minimal()

# Savings histogram
savings_plot <- ggplot(analysis_data, aes(x = Genre, y = Savings, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Savings by Gender",
       x = "Gender",
       y = "Average Savings") +
  theme_minimal()

# Plot all histograms together
grid.arrange(income_plot, spending_plot, savings_plot, ncol = 3)

# Load required libraries
library(ggplot2)
library(gridExtra)

# Create a data frame for analysis
analysis_data <- data.frame(
  Genre = c("Female", "Male"),
  Annual_Income = c(19167, 20333),
  Spending_Score = c(50.08, 50.33),
  Savings = c(19116.92, 20282.67)
)

# Plot histograms
# Income histogram
income_plot <- ggplot(analysis_data, aes(x = Genre, y = Annual_Income, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Annual Income by Gender",
       x = "Gender",
       y = "Average Annual Income") +
  theme_minimal()

# Spending score histogram
spending_plot <- ggplot(analysis_data, aes(x = Genre, y = Spending_Score, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Spending Score by Gender",
       x = "Gender",
       y = "Average Spending Score") +
  theme_minimal()

# Savings histogram
savings_plot <- ggplot(analysis_data, aes(x = Genre, y = Savings, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Savings by Gender",
       x = "Gender",
       y = "Average Savings") +
  theme_minimal()

# Plot all histograms together
grid.arrange(income_plot, spending_plot, savings_plot, ncol = 3)
# Income histogram
income_plot <- ggplot(analysis_data, aes(x = Genre, y = Annual_Income, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Annual Income by Gender",
       x = "Gender",
       y = "Average Annual Income") +
  theme_minimal()

# Print income histogram
print(income_plot)
# Spending score histogram
spending_plot <- ggplot(analysis_data, aes(x = Genre, y = Spending_Score, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Spending Score by Gender",
       x = "Gender",
       y = "Average Spending Score") +
  theme_minimal()

# Print spending score histogram
print(spending_plot)
# Savings histogram
savings_plot <- ggplot(analysis_data, aes(x = Genre, y = Savings, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Savings by Gender",
       x = "Gender",
       y = "Average Savings") +
  theme_minimal()

# Print savings histogram
print(savings_plot)

