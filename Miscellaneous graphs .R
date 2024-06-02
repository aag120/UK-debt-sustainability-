

#Breakdown of spending 

# Create a more detailed sample dataset to mimic the provided graph
expenditure_data <- data.frame(
  Year = 1998:2023,
  Social.Protection = c(200, 210, 220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360, 370, 360, 350, 340, 330, 320, 310, 300, 290),
  Health = c(60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 160, 170, 180, 190, 200, 210, 220),
  Education = c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175),
  General.Public.Services = c(40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 88, 90),
  Economic.Affairs = c(30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 80, 100, 150, 130, 120, 110, 100),
  Defence = c(20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45)
)

# Convert data to long format for ggplot
expenditure_long <- expenditure_data %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Expenditure")

# Create the plot
ggplot(expenditure_long, aes(x = Year, y = Expenditure, color = Category, group = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Social.Protection" = "#377eb8", 
    "Health" = "#e41a1c", 
    "Education" = "#ff7f00",
    "General.Public.Services" = "#984ea3", 
    "Economic.Affairs" = "#4daf4a", 
    "Defence" = "#a65628"
  )) +
  labs(
    title = "Public Sector Expenditure",
    x = NULL,
    y = "£ billion",
    color = NULL
  ) +
  scale_x_continuous(breaks = seq(1998, 2023, by = 1), limits = c(1998, 2023), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 400), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

view(Spending.breakdown.transpose.data)

# Convert the Year column to numeric, handling the string format appropriately
Spending.breakdown.transpose.data$Year <- as.numeric(gsub("-.+", "", Spending.breakdown.transpose.data$Year))

# Filter out the years 1998 and 2023
Spending.breakdown.transpose.data <- Spending.breakdown.transpose.data %>% filter(Year > 1998 & Year < 2023)

# Convert data to long format for ggplot
expenditure_long <- Spending.breakdown.transpose.data %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Expenditure")

# Adjust the legend labels
expenditure_long$Category <- recode(expenditure_long$Category,
                                    "Social.protection" = "Social Protection",
                                    "Health" = "Health",
                                    "Education" = "Education",
                                    "General.public.services" = "General Public Services",
                                    "Economic.affairs" = "Economic Affairs",
                                    "Defence" = "Defence"
)

# Create the plot
ggplot(expenditure_long, aes(x = Year, y = Expenditure, color = Category, group = Category)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  scale_color_manual(values = c(
    "Social Protection" = "#377eb8", 
    "Health" = "#e41a1c", 
    "Education" = "#ff7f00",
    "General Public Services" = "#984ea3", 
    "Economic Affairs" = "#4daf4a", 
    "Defence" = "#a65628"
  )) +
  labs(
    title = "Public Sector Expenditure",
    x = NULL,
    y = "£ billion",
    color = NULL
  ) +
  scale_x_continuous(breaks = seq(min(expenditure_long$Year), max(expenditure_long$Year), by = 1), limits = c(min(expenditure_long$Year), max(expenditure_long$Year)), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 360), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
    
  )

#Historic spending

Hist <- Historic.borrowing.and.debt 

view(Hist)


# Convert 'Year' to numeric (taking the first part of the year range for simplicity)
Hist$Year <- as.numeric(substr(Hist$Year, 1, 4))

# Create the ggplot
ggplot(Hist = df, aes(x = Year)) + 
  geom_line(aes(y = Hist$Government.debt, color = "Government debt"), size = 1) + 
  geom_line(aes(y = Hist$Government.borrowing., color = "Government borrowing"), size = 1) + 
  scale_y_continuous(
    name = "Government debt (per cent of GDP, left axis)", 
    sec.axis = sec_axis(~., name = "Government borrowing (per cent of GDP, right axis)")
  ) + 
  scale_color_manual(values = c("Government debt" = "blue", "Government borrowing" = "gold")) +
  labs(x = NULL, color = "Legend") +
  theme_minimal(base_size = 15) +  # Adjust base_size to increase overall font size
  theme(
    axis.title.y.left = element_text(color = "blue", size = 15),
    axis.title.y.right = element_text(color = "gold", size = 15),
    axis.text = element_text(size = 12),  # Adjust axis text size
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.position = "bottom"
  ) +
  # Add historical event annotations
  annotate("rect", xmin = 1701, xmax = 1714, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("text", x = 1708, y = 250, label = "War of the Spanish Succession", angle = 90, hjust = 1, size = 5) +
  annotate("rect", xmin = 1740, xmax = 1748, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("text", x = 1744, y = 250, label = "War of the Austrian Succession", angle = 90, hjust = 1, size = 5) +
  annotate("rect", xmin = 1756, xmax = 1763, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("text", x = 1759, y = 250, label = "Seven Years' War", angle = 90, hjust = 1, size = 5) +
  annotate("rect", xmin = 1775, xmax = 1783, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("text", x = 1779, y = 250, label = "American War of Independence", angle = 90, hjust = 1, size = 5) +
  annotate("rect", xmin = 1793, xmax = 1802, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("text", x = 1797, y = 250, label = "French Revolutionary Wars", angle = 90, hjust = 1, size = 5) +
  annotate("rect", xmin = 1803, xmax = 1815, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("text", x = 1809, y = 250, label = "Napoleonic Wars", angle = 90, hjust = 1, size = 5) +
  annotate("rect", xmin = 1853, xmax = 1856, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("text", x = 1854, y = 250, label = "Crimean War", angle = 90, hjust = 1, size = 5) +
  annotate("rect", xmin = 1914, xmax = 1918, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("text", x = 1916, y = 250, label = "WWI", angle = 90, hjust = 1, size = 5) +
  annotate("rect", xmin = 1939, xmax = 1945, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("text", x = 1942, y = 250, label = "WWII", angle = 90, hjust = 1, size = 5) +
  annotate("rect", xmin = 2007, xmax = 2009, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("text", x = 2008, y = 250, label = "Global Financial Crisis", angle = 90, hjust = 1, size = 5)





#FRS

# Convert 'Year' to numeric if necessary
FRS.data$X <- as.numeric(substr(FRS.data$X, 1, 4))

# Assuming the correct column names are `Year`, `FRS_2023_baseline`, `FRS_2022`, and `FRS_2023_constant_primary_balance`
# You may need to adjust the column names if they differ.

# Create the ggplot
ggplot(data = FRS.data, aes(x = FRS.data$X)) + 
  geom_line(aes(y = FRS.data$FRS.2023.baseline, color = "FRS 2023 baseline"), size = 1.2) + 
  geom_line(aes(y = FRS.data$FRS.2022, color = "FRS 2022"), size = 1.2) + 
  geom_line(aes(y = FRS.data$FRS.2023.constant.primary.balance, color = "FRS 2023 constant primary balance"), size = 1.2) +
  scale_y_continuous(name = "Per cent of GDP", limits = c(0, 350)) + 
  scale_x_continuous(breaks = seq(2022, 2073, by = 5)) +
  scale_color_manual(values = c("FRS 2023 baseline" = "darkgreen", "FRS 2022" = "gold", "FRS 2023 constant primary balance" = "blue")) +
  labs(x = NULL, color = "Legend") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  annotate("rect", xmin = 2022, xmax = 2028, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "cyan") +
  annotate("text", x = 2025, y = 340, label = "EFO forecast", hjust = 0.5, size = 5) +
  annotate("rect", xmin = 2028, xmax = 2073, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "lightblue") +
  annotate("text", x = 2050, y = 340, label = "FRS projection", hjust = 0.5, size = 5)