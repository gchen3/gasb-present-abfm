# plot_discount_rates.R

# Load necessary libraries
library(haven)
library(ggplot2)
library(dplyr)

# Load the dataset from the Stata file
data <- read_dta("C:/Users/Gang Chen/Dropbox/Gang Chen/GASB project/Data for descriptive figure/ppd_data_descriptive.dta")

# Summarize variables by year
data_summary <- data %>%
  group_by(fy) %>%
  summarise(
    avg_rate1 = mean(rate1, na.rm = TRUE),
    avg_rate2 = mean(rate2, na.rm = TRUE),
    rate1_up_total = sum(rate1_increase, na.rm = TRUE),
    rate1_down_total = sum(rate1_decrease, na.rm = TRUE),
    rate2_up_total = sum(rate2_increase, na.rm = TRUE),
    rate2_down_total = sum(rate2_decrease, na.rm = TRUE),
    total_plan = n()
  )

# Create the graph for change of discount rates
ggplot(data_summary, aes(x = fy)) +
  geom_line(aes(y = avg_rate1, color = "Blended Discount Rate"), linewidth = 1) +
  geom_line(aes(y = avg_rate2, color = "Long-term Expected Rate of Return"), linewidth = 1, linetype = "dashed") +
  
  # Add vertical lines for major GASB events
  geom_vline(xintercept = 2006, linetype = "dashed", color = "gray") +
  geom_text(aes(x = 2006, y = 0.09, label = "GASB project added"), hjust = -0.1, size = 3) +
  
  geom_vline(xintercept = 2009, linetype = "dashed", color = "gray") +
  geom_text(aes(x = 2009, y = 0.09, label = "Public comments"), hjust = -0.1, size = 3) +
  
  geom_vline(xintercept = 2010, linetype = "dashed", color = "gray") +
  geom_text(aes(x = 2010, y = 0.088, label = "Preliminary Views"), hjust = -0.1, size = 3) +
  
  geom_vline(xintercept = 2011, linetype = "dashed", color = "gray") +
  geom_text(aes(x = 2011, y = 0.086, label = "Draft Statement"), hjust = -0.1, size = 3) +
  
  geom_vline(xintercept = 2012, linetype = "dashed", color = "gray") +
  geom_text(aes(x = 2012, y = 0.084, label = "GASB 67 & 68 Release"), hjust = -0.1, size = 3) +
  
  geom_vline(xintercept = 2013, linetype = "dashed", color = "gray") +
  geom_text(aes(x = 2013, y = 0.082, label = "GASB 67 Effective"), hjust = -0.1, size = 3) +
  
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray") +
  geom_text(aes(x = 2014, y = 0.080, label = "GASB 68 Effective"), hjust = -0.1, size = 3) +
  
  # Add titles and labels
  labs(
    title = "Change of Discount Rates in U.S. Public Pension Plans (2001-2022)",
    y = "Average Discount Rate",
    x = "Fiscal Year"
  ) +
  
  # Customize the legend and theme
  scale_color_manual(values = c("Blended Discount Rate" = "grey10", "Long-term Expected Rate of Return" = "grey30")) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save the plot
ggsave("graph_change_discount_rates.png", width = 10, height = 6)
