#DATA CLEANUP

# Load necessary library
library(dplyr)
library(tidyr)


# Read the data
file_path <- '/Users/monikakaczorowska/Desktop/DSBA/semester 1/Business Analytics/project/11.19.data.csv'
data <- read.csv(file_path, stringsAsFactors = FALSE, na.strings=c("","NA"))
data$Q3_1 <- as.numeric(data$Q3_1)
data$Q6_1 <- as.numeric(data$Q6_1)
data$Q7_1 <- as.numeric(data$Q7_1)
data$Q8_1 <- as.numeric(data$Q8_1)
data$Q9_1 <- as.numeric(data$Q9_1)
data$Q15_1 <- as.numeric(data$Q15_1)
data$Q18_1 <- as.numeric(data$Q18_1)
data$Q19_1 <- as.numeric(data$Q19_1)

# Step 1: Remove rows where the "Status" column indicates a preview
data_cleaned <- data %>%
  filter(Status != "Survey Preview")


data_cleaned <- data %>%
  filter(Progress == 100)



# Step 3: Select only question columns and the treatment randomizer column
# Keep only question columns and the randomizer column explicitly
columns_to_keep <- c(question_cols, "FL_7_DO")
data_cleaned <- data_cleaned %>%
  select(all_of(columns_to_keep))
data_cleaned <- data_cleaned %>% select(-Q_DataPolicyViolations)


# Step 2: Drop rows where any column has no data
data_cleaned <- data_cleaned %>% drop_na()


# Step 4: Rename columns to be more intuitive
colnames(data_cleaned) <- c("Age", "Gender", "AI_Usage_Frequency", "AI_Knowledge_Rating", 
                            "Digital_Privacy_Concern", "Tech_Adoption_Openess",
                            "Trust_AI_Privacy_Pre", "Decision_Confidence_Pre", 
                            "Trust_AI_Privacy_Post", "Decision_Confidence_Post", "Treatment_Randomizer")


# Save the cleaned data
write.csv(data_cleaned, "cleaned_data.csv", row.names = FALSE)

data_cleaned$Treatment_Randomizer <- factor(data_cleaned$Treatment_Randomizer, 
                                            levels = c("Treatment-Neutral", "Treatment-Negative", "Treatment-Positive"))


data_cleaned$pre_trust_segment <- cut(data_cleaned$Trust_AI_Privacy_Pre, 
                                      breaks = c(-Inf, 3, 6, Inf), 
                                      labels = c("low", "medium", "high"))


data_cleaned$pre_confidence_segment <- cut(data_cleaned$Decision_Confidence_Pre, 
                                      breaks = c(-Inf, 3, 6, Inf), 
                                      labels = c("low", "medium", "high"))


# Define dependent variables
data_cleaned$Trust_Change <- data_cleaned$Trust_AI_Privacy_Post - data_cleaned$Trust_AI_Privacy_Pre
data_cleaned$Confidence_Change <- data_cleaned$Decision_Confidence_Post - data_cleaned$Decision_Confidence_Pre

write.csv(data_cleaned,"dataCleaned.csv", row.names = FALSE)



