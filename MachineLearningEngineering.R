# Ice Cream Flavor Histogram Visualization in R
# Machine Learning Engineering Example 1

# Load libraries
library(ggplot2)

# Sample dataset: Ice cream flavor popularity scores
icecream_data <- data.frame(
  flavor = c(
    "Vanilla", "Chocolate", "Strawberry", "Mint Chip",
    "Cookies & Cream", "Cookie Dough", "Peanut Butter Cup",
    "Pistachio", "Brownie Batter", "Coffee"
  ),
  popularity = c(95, 88, 72, 65, 90, 84, 60, 40, 55, 50)
)

# Create histogram-style visualization
ggplot(icecream_data, aes(x = popularity)) +
  geom_histogram(
    binwidth = 10,
    fill = "lightpink",
    color = "white",
    alpha = 0.9
  ) +
  labs(
    title = "Histogram of Ice Cream Flavor Popularity",
    subtitle = "Machine Learning Engineering Visualization in R",
    x = "Popularity Score",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14)
  )


# Advanced Ice Cream Histogram in R

# Load libraries
library(ggplot2)

# Dataset
icecream_data <- data.frame(
  flavor = c(
    "Vanilla",
    "Chocolate",
    "Strawberry",
    "Mint Chip",
    "Cookies & Cream",
    "Cookie Dough",
    "Peanut Butter Cup",
    "Pistachio",
    "Brownie Batter",
    "Coffee"
  ),
  popularity = c(95, 88, 72, 65, 90, 84, 60, 40, 55, 50)
)

# Calculate mean popularity
mean_popularity <- mean(icecream_data$popularity)

# Histogram
ggplot(icecream_data, aes(x = popularity)) +
  
  # Histogram bars
  geom_histogram(
    binwidth = 10,
    fill = "lightpink",
    color = "white",
    alpha = 0.85
  ) +
  
  # Mean popularity line
  geom_vline(
    xintercept = mean_popularity,
    color = "black",
    linewidth = 1.5,
    linetype = "dashed"
  ) +
  
  # Labels
  labs(
    title = "Ice Cream Flavor Popularity Distribution",
    subtitle = "Enhanced Machine Learning Engineering Visualization",
    x = "Popularity Score",
    y = "Frequency",
    caption = "Mean popularity shown with dashed blue line"
  ) +
  
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold",
      color = "black"
    ),
    plot.subtitle = element_text(
      size = 13,
      color = "gray30"
    ),
    axis.title = element_text(
      size = 14,
      face = "bold"
    ),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(
      fill = "#FFFFFF",
      color = NA
    )
  )

ggplot(icecream_data, aes(x = popularity)) +
  
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 10,
    fill = "lightpink",
    color = "white",
    alpha = 0.8
  ) +
  
  geom_density(
    color = "deeppink4",
    linewidth = 1.5
  )