# Ice Cream Analytics Flavors + Recommendation Systems in R
# Machine Learning Engineering Visualization Example 2


# Load libraries
library(ggplot2)
library(dplyr)


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
  
  popularity = c(95, 88, 72, 65, 90, 84, 60, 40, 55, 50),
  
  sweetness = c(7, 8, 9, 6, 8, 9, 9, 5, 10, 4),
  
  creaminess = c(9, 8, 7, 6, 9, 8, 8, 7, 9, 5)
)


# Histogram Visualization


mean_popularity <- mean(icecream_data$popularity)

hist_plot <- ggplot(icecream_data, aes(x = popularity)) +
  
  geom_histogram(
    binwidth = 10,
    fill = "pink",
    color = "grey",
    alpha = 0.85
  ) +
  
  geom_vline(
    xintercept = mean_popularity,
    color = "blue",
    linewidth = 1.5,
    linetype = "dashed"
  ) +
  
  labs(
    title = "Ice Cream Flavor Popularity Distribution",
    subtitle = "Machine Learning Engineering Visualization",
    x = "Popularity Score",
    y = "Frequency",
    caption = "Blue dashed line = Mean popularity"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 13,
      color = "gray30"
    ),
    
    axis.title = element_text(
      size = 14,
      face = "bold"
    ),
    
    plot.background = element_rect(
      fill = "#FFFFFF",
      color = NA
    )
  )

# Display histogram
print(hist_plot)


# Simple Recommendation System


# User preference input
preferred_sweetness <- 8
preferred_creaminess <- 8

# Calculate similarity score
icecream_data <- icecream_data %>%
  
  mutate(
    recommendation_score =
      sqrt(
        (sweetness - preferred_sweetness)^2 +
          (creaminess - preferred_creaminess)^2
      )
  ) %>%
  
  arrange(recommendation_score)


# Top Recommendations


cat("\n")
cat("=====================================\n")
cat(" ICE CREAM RECOMMENDATION SYSTEM\n")
cat("=====================================\n\n")

cat("User Preferences:\n")
cat("- Sweetness:", preferred_sweetness, "\n")
cat("- Creaminess:", preferred_creaminess, "\n\n")

cat("Top Recommended Flavors:\n\n")

top_recommendations <- icecream_data %>%
  select(
    flavor,
    popularity,
    sweetness,
    creaminess,
    recommendation_score
  ) %>%
  head(5)

print(top_recommendations)


# Recommendation Visualization

recommendation_plot <- ggplot(
  top_recommendations,
  aes(
    x = reorder(flavor, popularity),
    y = popularity,
    fill = flavor
  )
) +
  
  geom_bar(stat = "identity") +
  
  coord_flip() +
  
  labs(
    title = "Top Ice Cream Recommendations",
    subtitle = "Recommendation System Output",
    x = "Flavor",
    y = "Popularity Score"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    
    plot.title = element_text(
      size = 18,
      face = "bold"
    )
  )

# Display recommendation chart
print(recommendation_plot)


# Machine Learning Summary
cat("\n")
cat("=====================================\n")
cat(" MODEL SUMMARY\n")
cat("=====================================\n\n")

cat("Total flavors analyzed:",
    nrow(icecream_data), "\n")

cat("Average popularity:",
    round(mean(icecream_data$popularity), 2), "\n")

cat("Most popular flavor:",
    icecream_data$flavor[
      which.max(icecream_data$popularity)
    ], "\n")
