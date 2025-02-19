#------------ descriptive stats table
sumtable(data_cleaned)

#------------------------------balance checks
#tech adoption
library(ggplot2)
library(vtable)
#tech openess
ggplot(data_cleaned, aes(x = Treatment_Randomizer, y = Tech_Adoption_Openess, fill = Treatment_Randomizer)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Tech Adoption Openness by Treatment Group",
       x = "Treatment Group",
       y = "Tech Adoption Openness") +
  scale_fill_manual(values = c("Treatment-Neutral" = "lightyellow", 
                               "Treatment-Negative" = "salmon", 
                               "Treatment-Positive" = "lightgreen")) +
  theme(legend.position = "none")

dev.print(file="techAdoption.png", device=png, width=800)

#ai knowledge
ggplot(data_cleaned, aes(x = Treatment_Randomizer, y = AI_Knowledge_Rating, fill = Treatment_Randomizer)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of AI Knowledge Openness by Treatment Group",
       x = "Treatment Group",
       y = "AI Knowledge") +
  scale_fill_manual(values = c("Treatment-Neutral" = "lightyellow", 
                               "Treatment-Negative" = "salmon", 
                               "Treatment-Positive" = "lightgreen")) +
  theme(legend.position = "none")

dev.print(file="AIknowledge.png", device=png, width=800)

#ai usage
ggplot(data_cleaned, aes(x = Treatment_Randomizer, y = AI_Usage_Frequency, fill = Treatment_Randomizer)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of AI Usage by Treatment Group",
       x = "Treatment Group",
       y = "AI Usage") +
  scale_fill_manual(values = c("Treatment-Neutral" = "lightyellow", 
                               "Treatment-Negative" = "salmon", 
                               "Treatment-Positive" = "lightgreen")) +
  theme(legend.position = "none")
dev.print(file="AIUsage.png", device=png, width=800)

#digital privacy
ggplot(data_cleaned, aes(x = Treatment_Randomizer, y = Digital_Privacy_Concern, fill = Treatment_Randomizer)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Digital Privacy Concern by Treatment Group",
       x = "Treatment Group",
       y = "Digital Privacy Concern") +
  scale_fill_manual(values = c("Treatment-Neutral" = "lightyellow", 
                               "Treatment-Negative" = "salmon", 
                               "Treatment-Positive" = "lightgreen")) +
  theme(legend.position = "none")

dev.print(file="digprivacy.png", device=png, width=800)



#-------------------- change in trust/confidence
ggplot(data_cleaned, aes(x = Treatment_Randomizer, y = Trust_Change, fill = Treatment_Randomizer)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Change in Trust by Treatment Group",
       x = "Treatment Group",
       y = "Change in Trust") +
  scale_fill_manual(values = c("Treatment-Neutral" = "lightyellow", 
                               "Treatment-Negative" = "salmon", 
                               "Treatment-Positive" = "lightgreen")) +
  theme(legend.position = "none")
dev.print(file="changeTrust.png", device=png, width=800)

library(dplyr)

# Summarize change in trust by treatment group
trust_summary <- data_cleaned %>%
  group_by(Treatment_Randomizer) %>%
  summarise(
    Mean_Trust_Change = mean(Trust_Change, na.rm = TRUE),
    Median_Trust_Change = median(Trust_Change, na.rm = TRUE),
    SD_Trust_Change = sd(Trust_Change, na.rm = TRUE),
    Min_Trust_Change = min(Trust_Change, na.rm = TRUE),
    Max_Trust_Change = max(Trust_Change, na.rm = TRUE),
    Count = n()
  )

# Print the summarized table
print(trust_summary)


ggplot(data_cleaned, aes(x = Treatment_Randomizer, y = Confidence_Change, fill = Treatment_Randomizer)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Change in Confidence by Treatment Group",
       x = "Treatment Group",
       y = "Change in Confidence") +
  scale_fill_manual(values = c("Treatment-Neutral" = "lightyellow", 
                               "Treatment-Negative" = "salmon", 
                               "Treatment-Positive" = "lightgreen")) +
  theme(legend.position = "none")
dev.print(file="changeConfidence.png", device=png, width=800)


# Summarize change in confidence by treatment group
confidence_summary <- data_cleaned %>%
  group_by(Treatment_Randomizer) %>%
  summarise(
    Mean_Trust_Change = mean(Confidence_Change, na.rm = TRUE),
    Median_Trust_Change = median(Confidence_Change, na.rm = TRUE),
    SD_Trust_Change = sd(Confidence_Change, na.rm = TRUE),
    Min_Trust_Change = min(Confidence_Change, na.rm = TRUE),
    Max_Trust_Change = max(Confidence_Change, na.rm = TRUE),
    Count = n()
  )

# Print the summarized table
print(confidence_summary)



# Load necessary libraries
library(dplyr)
library(tidyr)
# Select numeric columns only
numeric_data <- select_if(data_cleaned, is.numeric)

# Generate descriptive statistics
descriptive_stats <- numeric_data %>%
  summarise(across(everything(), 
                   list(Mean = ~mean(.),
                        Median = ~median(.),
                        Std = ~sd(.),
                        Min = ~min(.),
                        Max = ~max(.),
                        Range = ~max(.) - min(.)), 
                   .names = "{.col}_{.fn}"))

# Reshape to a more readable format
final_table <- descriptive_stats %>%
  pivot_longer(cols = everything(), 
               names_to = c("Variable", "Statistic"), 
               names_sep = "_") %>%
  pivot_wider(names_from = Statistic, values_from = value)

# Display the final table
print("Descriptive Statistics Table:")
print(final_table)