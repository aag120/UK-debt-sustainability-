# Convert Year to factor
Results$Year <- as.factor(Results$Year)


#BASELINE

# Convert Year to factor
Results$Year <- as.factor(Results$Year)



# Convert Baseline Model and Harsh Immigration to numeric (if they contain percentage signs)
Results$Baseline.Model <- as.numeric(gsub("%", "", Results$Baseline.Model))


# Filter the necessary columns: Year, Baseline Model, and Harsh Immigration
filtered_data <- Results %>%
  select(Year, Baseline.Model) %>%
  gather(key = "Scenario", value = "Percentage", -Year)

# Rename the scenarios for better readability in the legend
filtered_data$Scenario <- recode(filtered_data$Scenario, 
                                 "Baseline.Model" = "Baseline Model")

# Create the plot with ggplot2
Baseline <- ggplot(data = filtered_data, aes(x = Year, y = Percentage, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  xlab("Year") +
  ylab("Total Government Debt to GDP ratio / %") +
  ggtitle("Debt-to-GDP Ratio Over Time", subtitle = "Baseline Model") + 
  scale_color_manual(values = c("Baseline Model" = "red")) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    panel.grid.major = element_line(color = "gray", size = 0.5), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031', '2032')) +
  scale_y_continuous(expand = c(0, 0), limits = c(75, 150), breaks = seq(0, 130, by = 10)) +  # Start y-axis from 0 with larger increments
  labs(color = "Scenario")

# Print the plot
print(Baseline)

#LONG TERM BASELINE:
# Convert Year to factor
LongTerm.Baseline$Year <- as.factor(LongTerm.Baseline$Year)


# Convert Baseline Model and Harsh Immigration to numeric (if they contain percentage signs)
LongTerm.Baseline$Debt.GDP <- as.numeric(gsub("%", "", LongTerm.Baseline$Debt.GDP))


# Filter the necessary columns: Year, Baseline Model, and Harsh Immigration
filtered_data <- Results %>%
  select(Year, LongTerm.Baseline) %>%
  gather(key = "Scenario", value = "Percentage", -Year)

# Rename the scenarios for better readability in the legend
filtered_data$Scenario <- recode(filtered_data$Scenario, 
                                 "Baseline.Model" = "Baseline Model")

# Create the plot with ggplot2
BaselineLong <- ggplot(data = filtered_data, aes(x = Year, y = Percentage, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  xlab("Year") +
  ylab("Total Government Debt to GDP ratio / %") +
  ggtitle("Debt-to-GDP Ratio Over Time", subtitle = "Baseline Model") + 
  scale_color_manual(values = c("Baseline Model" = "red")) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    panel.grid.major = element_line(color = "gray", size = 0.5), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031', '2032')) +
  scale_y_continuous(expand = c(0, 0), limits = c(75, 150), breaks = seq(0, 130, by = 10)) +  # Start y-axis from 0 with larger increments
  labs(color = "Scenario")

# Print the plot
print(BaselineLong)



#HARSH IMMMIGRATION

# Convert Baseline Model and Harsh Immigration to numeric (if they contain percentage signs)
Results$Baseline.Model <- as.numeric(gsub("%", "", Results$Baseline.Model))
Results$Harsh.Immigration <- as.numeric(gsub("%", "", Results$Harsh.Immigration))

# Filter the necessary columns: Year, Baseline Model, and Harsh Immigration
filtered_data <- Results %>%
  select(Year, Baseline.Model, Harsh.Immigration) %>%
  gather(key = "Scenario", value = "Percentage", -Year)

# Rename the scenarios for better readability in the legend
filtered_data$Scenario <- recode(filtered_data$Scenario, 
                                 "Baseline.Model" = "Baseline Model", 
                                 "Harsh.Immigration" = "Harsh Immigration")

# Create the plot with ggplot2
HarshImm <- ggplot(data = filtered_data, aes(x = Year, y = Percentage, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  xlab("Year") +
  ylab("Total Government Debt to GDP ratio / %") +
  ggtitle("Debt-to-GDP Ratio Over Time", subtitle = "Comparison of Baseline Model and Harsh Immigration Scenario") + 
  scale_color_manual(values = c("Baseline Model" = "red", "Harsh Immigration" = "blue")) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    panel.grid.major = element_line(color = "gray", size = 0.5), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031', '2032')) +
  scale_y_continuous(expand = c(0, 0), limits = c(75, 150), breaks = seq(0, 130, by = 25)) +  # Start y-axis from 0 with larger increments
  labs(color = "Scenario")

# Print the plot
print(HarshImm)

#HEALTHCARE 

# Convert relevant columns to numeric (if they contain percentage signs)
Results$Baseline.Model <- as.numeric(gsub("%", "", Results$Baseline.Model))
Results$Public.Health.Crisis.Conservative <- as.numeric(gsub("%", "", Results$Public.Health.Crisis.Conservative))
Results$Public.Health.Crisis.Severe <- as.numeric(gsub("%", "", Results$Public.Health.Crisis.Severe))

# Filter the necessary columns: Year, Baseline Model, Public Health Crisis Conservative, and Public Health Crisis Severe
filtered_data <- Results %>%
  select(Year, Baseline.Model, Public.Health.Crisis.Conservative, Public.Health.Crisis.Severe) %>%
  gather(key = "Scenario", value = "Percentage", -Year)

# Rename the scenarios for better readability in the legend
filtered_data$Scenario <- recode(filtered_data$Scenario, 
                                 "Baseline.Model" = "Baseline Model", 
                                 "Public.Health.Crisis.Conservative" = "Moderate Public Health Crisis", 
                                 "Public.Health.Crisis.Severe" = "Severe Health Public Crisis")

# Create the plot with ggplot2 and ggbreak
Health <- ggplot(data = filtered_data, aes(x = Year, y = Percentage, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  xlab("Year") +
  ylab("Total Government Debt:GDP ratio / %") +
  ggtitle("Debt:GDP Ratio Over Time", subtitle = "Comparison of Baseline Model, Severe Health Public Crisis, and Moderate Public Health Crisis") + 
  scale_color_manual(values = c("Baseline Model" = "red", "Severe Health Public Crisis" = "blue", "Moderate Public Health Crisis" = "purple")) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    panel.grid.major = element_line(color = "gray", size = 0.5), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031', '2032')) +
  scale_y_continuous(expand = c(0, 0), limits = c(75, 150), breaks = seq(0, 130, by = 25)) +  # Start y-axis from 0 with larger increments
  labs(color = "Scenario")
# Print the plot
print(Health)


#WARFARE



# Convert Year to factor
Results$Year <- as.factor(Results$Year)

# Convert relevant columns to numeric (if they contain percentage signs)
Results$Baseline.Model <- as.numeric(gsub("%", "", Results$Baseline.Model))
Results$Warfare.Conservative <- as.numeric(gsub("%", "", Results$Warfare.Conservative))
Results$Warfare.Severe <- as.numeric(gsub("%", "", Results$Warfare.Severe))

# Filter the necessary columns: Year, Baseline Model, Warfare Conservative, and Warfare Severe
filtered_data <- Results %>%
  select(Year, Baseline.Model, Warfare.Conservative, Warfare.Severe) %>%
  gather(key = "Scenario", value = "Percentage", -Year)

# Rename the scenarios for better readability in the legend
filtered_data$Scenario <- recode(filtered_data$Scenario, 
                                 "Baseline.Model" = "Baseline Model", 
                                 "Warfare.Conservative" = "Moderate Warfare", 
                                 "Warfare.Severe" = "Severe Warfare")

# Create the plot with ggplot2
War <- ggplot(data = filtered_data, aes(x = Year, y = Percentage, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  xlab("Year") +
  ylab("Total Government Debt to GDP ratio / %") +
  ggtitle("Debt-to-GDP Ratio Over Time", subtitle = "Comparison of Baseline Model, Conservative Warfare, and Severe Warfare") + 
  scale_color_manual(values = c("Baseline Model" = "red", "Moderate Warfare" = "Purple", "Severe Warfare" = "blue")) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    panel.grid.major = element_line(color = "gray", size = 0.5), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031', '2032')) +
  scale_y_continuous(expand = c(0, 0), limits = c(75, 150), breaks = seq(0, 130, by = 25)) +  # Start y-axis from 0 with larger increments
  labs(color = "Scenario")

# Print the plot
print(War)

#CLIMATE 

# Convert relevant columns to numeric (if they contain percentage signs)
Results$Baseline.Model <- as.numeric(gsub("%", "", Results$Baseline.Model))
Results$Climate.Crisis.Proactive <- as.numeric(gsub("%", "", Results$Climate.Crisis.Proactive))
Results$Climate.Crisis.Tax <- as.numeric(gsub("%", "", Results$Climate.Crisis.Tax))
Results$Climate.Crisis.Shock <- as.numeric(gsub("%", "", Results$Climate.Crisis.Shock))

# Filter the necessary columns: Year, Baseline Model, Climate Crisis Proactive, Climate Crisis Severe, and Climate Crisis Shock
filtered_data <- Results %>%
  select(Year, Baseline.Model, Climate.Crisis.Proactive, Climate.Crisis.Tax, Climate.Crisis.Shock) %>%
  gather(key = "Scenario", value = "Percentage", -Year)

# Rename the scenarios for better readability in the legend
filtered_data$Scenario <- recode(filtered_data$Scenario, 
                                 "Baseline.Model" = "Baseline Model", 
                                 "Climate.Crisis.Proactive" = "Climate Crisis Proactive", 
                                 "Climate.Crisis.Tax" = "Climate Crisis Tax",
                                 "Climate.Crisis.Shock" = "Climate Crisis Shock")

# Create the plot with ggplot2
ClimatePlot <- ggplot(data = filtered_data, aes(x = Year, y = Percentage, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  xlab("Year") +
  ylab("Total Government Debt to GDP ratio / %") +
  ggtitle("Debt-to-GDP Ratio Over Time", subtitle = "Comparison of Baseline Model, Climate Crisis Proactive, Climate Crisis Tax, and Climate Crisis Shock") + 
  scale_color_manual(values = c("Baseline Model" = "red", "Climate Crisis Proactive" = "green", "Climate Crisis Tax" = "blue", "Climate Crisis Shock" = "purple")) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    panel.grid.major = element_line(color = "gray", size = 0.5), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031', '2032')) +
  scale_y_continuous(expand = c(0, 0), limits = c(75, 150), breaks = seq(0, 125, by = 25)) +  # Start y-axis from 0 with larger increments
  labs(color = "Scenario")

# Print the plot
print(ClimatePlot)




#HOUSING CRISIS

# Convert relevant columns to numeric (if they contain percentage signs)
Results$Baseline.Model <- as.numeric(gsub("%", "", Results$Baseline.Model))
Results$Housing.Crisis <- as.numeric(gsub("%", "", Results$Housing.Crisis))

# Filter the necessary columns: Year, Baseline Model, and Housing Crisis
filtered_data <- Results %>%
  select(Year, Baseline.Model, Housing.Crisis) %>%
  gather(key = "Scenario", value = "Percentage", -Year)

# Rename the scenarios for better readability in the legend
filtered_data$Scenario <- recode(filtered_data$Scenario, 
                                 "Baseline.Model" = "Baseline Model", 
                                 "Housing.Crisis" = "Housing Crisis")

# Create the plot with ggplot2
HousingPlot <- ggplot(data = filtered_data, aes(x = Year, y = Percentage, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  xlab("Year") +
  ylab("Total Government Debt to GDP ratio / %") +
  ggtitle("Debt-to-GDP Ratio Over Time", subtitle = "Comparison of Baseline Model and Housing Crisis") + 
  scale_color_manual(values = c("Baseline Model" = "red", "Housing Crisis" = "blue")) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    panel.grid.major = element_line(color = "gray", size = 0.5), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031', '2032')) +
  scale_y_continuous(expand = c(0, 0), limits = c(75, 125), breaks = seq(0, 150, by = 25)) +  # Start y-axis from 0 with larger increments
  labs(color = "Scenario")

# Print the plot
print(HousingPlot)

#LABOUR 

# Convert relevant columns to numeric (if they contain percentage signs)
Results$Baseline.Model <- as.numeric(gsub("%", "", Results$Baseline.Model))
Results$Labour <- as.numeric(gsub("%", "", Results$Labour))

# Filter the necessary columns: Year, Baseline Model, and Labour
filtered_data <- Results %>%
  select(Year, Baseline.Model, Labour) %>%
  gather(key = "Scenario", value = "Percentage", -Year)

# Rename the scenarios for better readability in the legend
filtered_data$Scenario <- recode(filtered_data$Scenario, 
                                 "Baseline.Model" = "Baseline Model", 
                                 "Labour" = "Labour Government")

# Create the plot with ggplot2
LabourPlot <- ggplot(data = filtered_data, aes(x = Year, y = Percentage, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  xlab("Year") +
  ylab("Total Government Debt to GDP ratio / %") +
  ggtitle("Debt-to-GDP Ratio Over Time", subtitle = "Comparison of Baseline Model and Model under a Labour Government") + 
  scale_color_manual(values = c("Baseline Model" = "red", "Labour Government" = "blue")) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    panel.grid.major = element_line(color = "gray", size = 0.5), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031', '2032')) +
  scale_y_continuous(expand = c(0, 0), limits = c(75, 125), breaks = seq(0, 150, by = 15)) +  # Start y-axis from 0 with larger increments
  labs(color = "Scenario")

# Print the plot
print(LabourPlot)

#CONSERVATIVE 

# Convert relevant columns to numeric (if they contain percentage signs)
Results$Baseline.Model <- as.numeric(gsub("%", "", Results$Baseline.Model))
Results$Conservative.Proposed <- as.numeric(gsub("%", "", Results$Conservative.Proposed))
Results$Conservative.Adjusted <- as.numeric(gsub("%", "", Results$Conservative.Adjusted))

# Filter the necessary columns: Year, Baseline Model, Conservative Proposed, and Conservative Adjusted
filtered_data <- Results %>%
  select(Year, Baseline.Model, Conservative.Proposed, Conservative.Adjusted) %>%
  gather(key = "Scenario", value = "Percentage", -Year)

# Rename the scenarios for better readability in the legend
filtered_data$Scenario <- recode(filtered_data$Scenario, 
                                 "Baseline.Model" = "Baseline Model", 
                                 "Conservative.Proposed" = "Conservative Proposed", 
                                 "Conservative.Adjusted" = "Conservative Adjusted")

# Create the plot with ggplot2
ConservativePlot <- ggplot(data = filtered_data, aes(x = Year, y = Percentage, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  xlab("Year") +
  ylab("Total Government Debt to GDP ratio / %") +
  ggtitle("Debt-to-GDP Ratio Over Time", subtitle = "Comparison of Baseline Model, Conservative Proposed, and Conservative Adjusted") + 
  scale_color_manual(values = c("Baseline Model" = "red", "Conservative Proposed" = "green", "Conservative Adjusted" = "blue")) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    panel.grid.major = element_line(color = "gray", size = 0.5), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031', '2032')) +
  scale_y_continuous(expand = c(0, 0), limits = c(75, 120), breaks = seq(0, 150, by = 15)) +  # Start y-axis from 0 with larger increments
  labs(color = "Scenario")

# Print the plot
print(ConservativePlot)

#BASELINE

# Convert relevant columns to numeric (if they contain percentage signs)
Results$Baseline.Model <- as.numeric(gsub("%", "", Results$Baseline.Model))


# Filter the necessary columns: Year, Baseline Model, Conservative Proposed, and Conservative Adjusted
filtered_data <- Results %>%
  select(Year, Baseline.Model) %>%
  gather(key = "Scenario", value = "Percentage", -Year)

# Rename the scenarios for better readability in the legend
filtered_data$Scenario <- recode(filtered_data$Scenario, 
                                 "Baseline.Model" = "Baseline Model", "")

# Create the plot with ggplot2
BaselinePlot <- ggplot(data = filtered_data, aes(x = Year, y = Percentage, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  xlab("Year") +
  ylab("Total Government Debt to GDP ratio / %") +
  ggtitle("Debt-to-GDP Ratio Over Time", subtitle = "Comparison of Baseline Model, Conservative Proposed, and Conservative Adjusted") + 
  scale_color_manual(values = c("Baseline Model" = "red", "High Growth Model" = "blue")) 
theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    panel.grid.major = element_line(color = "gray", size = 0.5), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031', '2032')) +
  scale_y_continuous(expand = c(0, 0), limits = c(75, 120), breaks = seq(0, 150, by = 15)) +  # Start y-axis from 0 with larger increments
  labs(color = "Scenario")

# Print the plot
print(BaselinePlot)

#LONG TERM 


# Convert Year to factor
Long.term.debt.GDP$Year <- as.factor(Long.term.debt.GDP$Year)

# Convert relevant columns to numeric (if they contain percentage signs)
Long.term.debt.GDP$Baseline.Debt.to.GDP <- as.numeric(gsub("%", "", Long.term.debt.GDP$Baseline.Debt.to.GDP))
Long.term.debt.GDP$High.Growth.Debt.to.GDP <- as.numeric(gsub("%", "", Long.term.debt.GDP$High.Growth.Debt.to.GDP))

# Filter the necessary columns: Year, Baseline Model, and High Growth Model
filtered_data <- Long.term.debt.GDP %>%
  select(Year, Baseline.Debt.to.GDP, High.Growth.Debt.to.GDP) %>%
  gather(key = "Scenario", value = "Percentage", -Year)

# Rename the scenarios for better readability in the legend
filtered_data$Scenario <- recode(filtered_data$Scenario, 
                                 "Baseline.Debt.to.GDP" = "Baseline Model",
                                 "High.Growth.Debt.to.GDP" = "High Growth Model")

# Create the plot with ggplot2
LongTermPlot <- ggplot(data = filtered_data, aes(x = Year, y = Percentage, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  xlab("Year") +
  ylab("Total Government Debt to GDP ratio / %") +
  ggtitle("Debt-to-GDP Ratio Over Time", subtitle = "Comparison of Baseline Model and High Growth Model") + 
  scale_color_manual(values = c("Baseline Model" = "red", "High Growth Model" = "blue")) +
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    panel.grid.major = element_line(color = "gray", size = 0.5), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031', '2032', '2033', '2034', '2035', '2036', '2037', '2038', '2039','2040', '2041','2042','2043', '2044', '2045', '2046', '2047', '2048', '2049', '2050')) +
  scale_y_continuous(expand = c(0, 0), limits = c(75, 180), breaks = seq(75, 180, by = 25)) +  # Adjusted y-axis limits and breaks
  labs(color = "Scenario")

# Print the plot
print(LongTermPlot)

#LONG TERM 


# Convert Year to factor
Long.term.debt.GDP$Year <- as.factor(Long.term.debt.GDP$Year)

# Convert relevant columns to numeric (if they contain percentage signs)
Long.term.debt.GDP$Baseline.Debt.to.GDP <- as.numeric(gsub("%", "", Long.term.debt.GDP$Baseline.Debt.to.GDP))

# Filter the necessary columns: Year, Baseline Model, and High Growth Model
filtered_data <- Long.term.debt.GDP %>%
  select(Year, Baseline.Debt.to.GDP) %>%
  gather(key = "Scenario", value = "Percentage", -Year)

# Rename the scenarios for better readability in the legend
filtered_data$Scenario <- recode(filtered_data$Scenario, 
                                 "Baseline.Debt.to.GDP" = "Baseline Model")

# Create the plot with ggplot2
LongTermPlot <- ggplot(data = filtered_data, aes(x = Year, y = Percentage, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  xlab("Year") +
  ylab("Total Government Debt to GDP ratio / %") +
  ggtitle("Debt-to-GDP Ratio Over Time", subtitle = "Comparison of Baseline Model and High Growth Model") + 
  scale_color_manual(values = c("Baseline Model" = "red")) +
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank(),  # Remove legend title
    panel.grid.major = element_line(color = "gray", size = 0.5), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031', '2032', '2033', '2034', '2035', '2036', '2037', '2038', '2039','2040', '2041','2042','2043', '2044', '2045', '2046', '2047', '2048', '2049', '2050')) +
  scale_y_continuous(expand = c(0, 0), limits = c(75, 180), breaks = seq(75, 180, by = 25)) +  # Adjusted y-axis limits and breaks
  labs(color = "Scenario")

# Print the plot
print(LongTermPlot)
