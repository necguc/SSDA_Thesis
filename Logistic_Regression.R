# Install packages
install.packages("readr")
install.packages("dplyr")
install.packages("fastDummies")
install.packages("stargazer")
install.packages("tidyr")
install.packages("RColorBrewer")
install.packages("readxl")



# Load necessary packages
library(readr)
library(dplyr)
library(fastDummies)
library(stargazer)
library(readxl)
library(tidyr)
library(ggplot2)
library(readxl)

# Specify the file path
file_path <- "C:/Users/User/Desktop/Thesis/speeches_with_sentiment_analysis_final2.xlsx"

# Read the CSV file
data <- read_excel(file_path)

# Display the structure of the data (including column names and data types)
str(data)

# Display unique categories in the SpeechType column
unique_speech_types <- unique(data$SpeechType)
print(unique_speech_types)

# Count the number of missing values (NA) in the SpeechType column
num_missing <- sum(is.na(data$SpeechType))
print(num_missing)

# Count total missing values in the entire dataset
total_missing <- sum(is.na(data))
print(total_missing)

# Count missing values per column
missing_per_column <- colSums(is.na(data))
print(missing_per_column)

# View rows with missing values in SpeechType and Category columns
missing_data <- data[is.na(data$SpeechType) | is.na(data$Category), ]
print(missing_data)

# View unique values in the Title column for missing_data
unique_titles_in_missing_data <- unique(missing_data$Title)
print(unique_titles_in_missing_data)

# Fill missing values in SpeechType and Category columns with specific values
# For example, fill missing values with "Unknown"
data$SpeechType[is.na(data$SpeechType)] <- "Ribbon Cutting"
data$Category[is.na(data$Category)] <- "Economic"

# Verify the replacement
missing_data_filled <- data[is.na(data$SpeechType) | is.na(data$Category), ]
print(missing_data_filled)

# Create a new column 'Location' based on 'SpeechType'
data$Location <- ifelse(data$SpeechType == "International Speeches", "International", "National")

# Print column names
print(colnames(data))

# Reorder the columns
required_columns <- c("ID", "Title", "Cleaned_Paragraph", "SpeechType", "Category", "Location", 
                      "Date", "MonthYear", "Year", "SpeechLength", "SentenceCount")

remaining_columns <- setdiff(names(data), required_columns)
ordered_columns <- c(required_columns, remaining_columns)

# Reorder the data frame
data <- data[, ordered_columns]

# Print the first few rows to verify the order
print(head(data))

# Select columns
selected_columns <- c(
  "ID", 
  "Cleaned_Paragraph", 
  "SpeechType",  
  "Year", 
  "highest_frame", 
  "sentiment"
)


# Select the specified columns to create a new data frame
new_data <- data %>% select(all_of(selected_columns))

# Display the first few rows to verify the selection
print(head(new_data))


# Print the first few rows of the new data frame
print(head(new_data))
print(colnames(new_data))



# Convert categorical variables to factors
new_data$Cleaned_Paragraph<- as.factor(new_data$Cleaned_Paragraph)
new_data$SpeechType <- as.factor(new_data$SpeechType)
new_data$highest_frame <- as.factor(new_data$highest_frame)
new_data$sentiment <- as.factor(new_data$sentiment)
new_data$year <- as.factor(new_data$year)
new_data$diagnostic <- as.factor(new_data$diagnostic)
new_data$prognostic <- as.factor(new_data$prognostic)
new_data$adversarial <- as.factor(new_data$adversarial)
new_data$identity <- as.factor(new_data$identity)

# Ensure numeric variables are numeric
new_data$ID <- as.numeric(new_data$ID)

sapply(new_data, class)

# Verify the changes
str(new_data)

print(colnames(new_data))


# Create new binary columns for each frame type
new_data$diagnostic <- ifelse(new_data$highest_frame == "diagnostic", 1, 0)
new_data$prognostic <- ifelse(new_data$highest_frame == "prognostic", 1, 0)
new_data$adversarial <- ifelse(new_data$highest_frame == "adversarial", 1, 0)
new_data$identity <- ifelse(new_data$highest_frame == "identity", 1, 0)


# Original column names
names(new_data)

# Change specific column names to lowercase
names(new_data)[names(new_data) == "ID"] <- "id"
names(new_data)[names(new_data) == "Cleaned_Paragraph"] <- "cleaned_paragraph"
names(new_data)[names(new_data) == "SpeechType"] <- "speech_type"
names(new_data)[names(new_data) == "Year"] <- "year"
# View the updated column names
names(new_data)

# Remove rows where speech_type is "Other"
new_data <- new_data[new_data$speech_type != "Other", ]

# Recalculate and print unique speech_type values to verify
unique_speech_types <- unique(new_data$speech_type)
print("Unique speech_type values after filtering:")
print(unique_speech_types)




############################### Descriptive Analysis ########################################


######### Calculate the number of unique speeches per year############
speech_counts_per_year <- new_data %>%
  group_by(year) %>%
  summarise(Unique_Speeches = n_distinct(id)) %>%
  arrange(year)

# Print the calculated data
print(speech_counts_per_year)

# Plot the change in the number of unique speeches over time using a bar chart
ggplot(speech_counts_per_year, aes(x = factor(year), y = Unique_Speeches)) +
  geom_bar(stat = "identity", fill = "#1f78b4", color = "black") + # Dark blue fill
  labs(
    title = "Change in the Number of Speeches Over Years",
    x = "Year",
    y = "Number of Speeches"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "plain", hjust = 0.5), # Larger size for title
    axis.title.x = element_text(size = 14), # Uniform size for axis titles
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1), # 45-degree angle for x-axis text
    axis.text.y = element_text(size = 12), # Ensure axis text readability
    panel.grid.major = element_line(color = "grey80", size = 0.5), # Subtle grid lines for both axes
    panel.grid.minor = element_blank(), # No minor grids for a cleaner look
    panel.background = element_blank(), # Clean background
    plot.background = element_rect(fill = "white", color = NA) # White plot background
  )


#################### Distribution of Sentiment Across Paragraphs by Year #################


# Calculate the number of paragraphs with positive and negative sentiment per year
paragraph_sentiment_distribution <- new_data %>%
  group_by(Year = year, Sentiment = sentiment) %>%
  summarise(Paragraph_Count = n(), .groups = 'drop') %>%
  mutate(Sentiment = factor(Sentiment, labels = c("Negative", "Positive")))

# View the resulting data frame
print(paragraph_sentiment_distribution)

# Create a complete sequence of years to include all years in the plot
all_years <- seq(min(new_data$year), max(new_data$year), by = 1)

# Ensure that every year has an entry for both sentiments
paragraph_sentiment_distribution <- paragraph_sentiment_distribution %>%
  complete(Year = all_years, Sentiment, fill = list(Paragraph_Count = 0))

# Plotting the stacked bar chart with shades of blue
ggplot(paragraph_sentiment_distribution, aes(x = factor(Year), y = Paragraph_Count, fill = Sentiment)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Distribution of Sentiment Across Paragraphs by Year",
    x = "Year",
    y = "Number of Paragraphs",
    fill = "Sentiment"
  ) +
  scale_fill_manual(values = c("Negative" = "#a6cee3", "Positive" = "#1f78b4")) + # Blue shades
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels for better readability
  )
######################## Distribution of Paragraphs Across Frames by Year ###############

# Prepare the data
frame_speech_type_distribution <- new_data %>%
  pivot_longer(cols = c(adversarial, diagnostic, prognostic, identity),
               names_to = "Frame", values_to = "Presence") %>%
  filter(Presence == 1) %>%
  count(speech_type, Frame)

# Generate the plot
p <- ggplot(frame_speech_type_distribution, aes(x = Frame, y = n, fill = speech_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of Frames by Speech Type",
    x = "Frame",
    y = "Count of Occurrences",
    fill = "Speech Type"
  ) +
  scale_fill_brewer(palette = "Paired") +  # Using 'Paired' palette for better color distinction
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Display the plot
print(p)

# Save the plot
ggsave("frame_speech_type_distribution.png", plot = p, width = 10, height = 6, dpi = 300)

######################### Distribution of Frames by Sentiments ##################


# Check for distinct sentiment values in the dataset
print(unique(new_data$sentiment))

# Generate the plot
p <- ggplot(frame_sentiment_distribution, aes(x = Frame, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of Frames by Sentiment",
    x = "Frame",
    y = "Count of Frame Presence",
    fill = "Sentiment"
  ) +
  scale_fill_manual(values = c("negative" = "#cd6155", "positive" = "#5499c7"), 
                    labels = c("Negative", "Positive")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, hjust = 0.5),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Display the plot
print(p)

# Save the plot
ggsave("frame_sentiment_distribution.png", plot = p, width = 10, height = 6, dpi = 300)


###################### Frames Across Speech Types #####################################


# Prepare the data
frame_speech_type_distribution <- new_data %>%
  pivot_longer(cols = c(adversarial, diagnostic, prognostic, identity),
               names_to = "Frame", values_to = "Presence") %>%
  filter(Presence == 1) %>%
  count(speech_type, Frame)

# Generate the plot
p <- ggplot(frame_speech_type_distribution, aes(x = Frame, y = n, fill = speech_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of Frames by Speech Type",
    x = "Frame",
    y = "Count of Occurrences",
    fill = "Speech Type"
  ) +
  scale_fill_brewer(palette = "Paired") +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Display the plot
print(p)

# Save the plot
ggsave("frame_speech_type_distribution.png", plot = p, width = 10, height = 6, dpi = 300)

############################ LOGISTIC REGRESSION MODELS ###################################



######################### Diagnostic #############################

# Model for Diagnostic - Sentiment only
model_diagnostic_basic <- glm(diagnostic ~ sentiment, 
                              data = new_data, family = binomial)
summary(model_diagnostic_basic)

# Model for Diagnostic - Sentiment and Year
model_diagnostic_year <- glm(diagnostic ~ sentiment + year, 
                             data = new_data, family = binomial)
summary(model_diagnostic_year)

# Full model for Diagnostic (Sentiment, Year, and Speech Type)
model_diagnostic_full <- glm(diagnostic ~ sentiment + year + speech_type, 
                             data = new_data, family = binomial)
summary(model_diagnostic_full)

# Load stargazer package
library(stargazer)

# Save the stargazer output to a text file without the intercept
stargazer(model_diagnostic_basic, model_diagnostic_year, model_diagnostic_full, 
          title = "Logistic Regression Results", 
          align = TRUE, 
          type = "text",  # Ensure type is set to 'text' for text file output
          dep.var.labels = c("Diagnostic"),
          covariate.labels = c("Sentiment Positive", "Year 2015", "Year 2016", 
                               "Year 2017", "Year 2018", "Year 2019", "Year 2020", 
                               "Year 2021", "Year 2022", "Year 2023", "Year 2024", 
                               "Awards and Graduations", "Civil Society Organization", 
                               "Crisis Speeches", "International Speeches", 
                               "National Ceremonies", "Official Speeches", 
                               "Rallies", "Ribbon Cutting", "Social Events"),
          omit.stat = c("LL", "aic"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*p",
          omit = "(Intercept)",  
          out = "logistic_regression_results.html")  







####################### Prognostic ####################################

# Model for Prognostic - Sentiment only
model_prognostic_basic <- glm(prognostic ~ sentiment, 
                              data = new_data, family = binomial)
summary(model_prognostic_basic)

# Model for Prognostic - Sentiment and Year
model_prognostic_year <- glm(prognostic ~ sentiment + year, 
                             data = new_data, family = binomial)
summary(model_prognostic_year)

# Full model for Prognostic (Sentiment, Year, and Speech Type)
model_prognostic_full <- glm(prognostic ~ sentiment + year + speech_type, 
                             data = new_data, family = binomial)
summary(model_prognostic_full)



# Save the stargazer output to a text file without the intercept
stargazer(model_prognostic_basic, model_prognostic_year, model_prognostic_full, 
          title = "Logistic Regression Results", 
          align = TRUE, 
          type = "text",  # Ensure type is set to 'text' for text file output
          dep.var.labels = c("prognostic"),
          covariate.labels = c("Sentiment Positive", "Year 2015", "Year 2016", 
                               "Year 2017", "Year 2018", "Year 2019", "Year 2020", 
                               "Year 2021", "Year 2022", "Year 2023", "Year 2024", 
                               "Awards and Graduations", "Civil Society Organization", 
                               "Crisis Speeches", "International Speeches", 
                               "National Ceremonies", "Official Speeches", 
                               "Rallies", "Ribbon Cutting", "Social Events"),
          omit.stat = c("LL", "aic"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*p",
          omit = "(Intercept)",  
          out = "prognostic_logistic_regression_results.html")  



###################### Identity #######################################

# Model for Identity - Sentiment only
model_identity_basic <- glm(identity ~ sentiment, 
                            data = new_data, family = binomial)
summary(model_identity_basic)

# Model for Identity - Sentiment and Year
model_identity_year <- glm(identity ~ sentiment + year, 
                           data = new_data, family = binomial)
summary(model_identity_year)

# Full model for Identity (Sentiment, Year, and Speech Type)
model_identity_full <- glm(identity ~ sentiment + year + speech_type, 
                           data = new_data, family = binomial)
summary(model_identity_full)

# Save the stargazer output to a text file without the intercept
stargazer(model_identity_basic, model_identity_year, model_identity_full, 
          title = "Logistic Regression Results", 
          align = TRUE, 
          type = "text",  # Ensure type is set to 'text' for text file output
          dep.var.labels = c("identity"),
          covariate.labels = c("Sentiment Positive", "Year 2015", "Year 2016", 
                               "Year 2017", "Year 2018", "Year 2019", "Year 2020", 
                               "Year 2021", "Year 2022", "Year 2023", "Year 2024", 
                               "Awards and Graduations", "Civil Society Organization", 
                               "Crisis Speeches", "International Speeches", 
                               "National Ceremonies", "Official Speeches", 
                               "Rallies", "Ribbon Cutting", "Social Events"),
          omit.stat = c("LL", "aic"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*p",
          omit = "(Intercept)",  
          out = "identity_logistic_regression_results.html")  



######################### Adversarial #############################

# Model for Adversarial - Sentiment only
model_adversarial_basic <- glm(adversarial ~ sentiment, 
                               data = new_data, family = binomial)
summary(model_adversarial_basic)

# Model for Adversarial - Sentiment and Year
model_adversarial_year <- glm(adversarial ~ sentiment + year, 
                              data = new_data, family = binomial)
summary(model_adversarial_year)

# Full model for Adversarial (Sentiment, Year, Speech Type)
model_adversarial_full <- glm(adversarial ~ sentiment + year + speech_type, 
                              data = new_data, family = binomial)
summary(model_adversarial_full)

# Save the stargazer output to a text file without the intercept
stargazer(model_adversarial_basic, model_adversarial_year, model_adversarial_full, 
          title = "Logistic Regression Results", 
          align = TRUE, 
          type = "text",  # Ensure type is set to 'text' for text file output
          dep.var.labels = c("adversarial"),
          covariate.labels = c("Sentiment Positive", "Year 2015", "Year 2016", 
                               "Year 2017", "Year 2018", "Year 2019", "Year 2020", 
                               "Year 2021", "Year 2022", "Year 2023", "Year 2024", 
                               "Awards and Graduations", "Civil Society Organization", 
                               "Crisis Speeches", "International Speeches", 
                               "National Ceremonies", "Official Speeches", 
                               "Rallies", "Ribbon Cutting", "Social Events"),
          omit.stat = c("LL", "aic"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          single.row = TRUE,
          no.space = TRUE,
          report = "vc*p",
          omit = "(Intercept)",  
          out = "adversarial_identity_logistic_regression_results.html") 


###################### Comparison of Full Models for Each Frame ###############

# Define the full model list and model names
full_model_list <- list(model_diagnostic_full, model_prognostic_full, model_identity_full, model_adversarial_full)
full_model_names <- c("Diagnostic Full Model", "Prognostic Full Model", "Identity Full Model", "Adversarial Full Model")

# Generate the stargazer output for Full Models
stargazer(full_model_list, 
          title="Logistic Regression Results for Full Models",
          align=TRUE, type="text", no.space=TRUE, 
          custom.model.names=full_model_names)

# Save the results to a file
stargazer(full_model_list, 
          title="Logistic Regression Results for Full Models",
          align=TRUE, type="text", no.space=TRUE, 
          custom.model.names=full_model_names, 
          out="full_models_results.txt")

