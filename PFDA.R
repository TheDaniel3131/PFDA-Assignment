###############################################################################################################################


## Data Import (Import the dataset & packages to use)
# >>  The process of loading data from external sources or files into a software environment 
# for analysis and manipulation. <<

# Installing Packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
install.packages("lubridate")
install.packages("validate")
install.packages("naniar")
install.packages("arsenal")
install.packages("hrbrthemes")
install.packages("RColorBrewer")
install.packages("gridExtra")
install.packages("ggridges")
install.packages("viridis")
install.packages("tidyr")
install.packages(c("httr", "jsonlite"))
install.packages("mapdata")
install.packages("ggrepel")
install.packages("crayon")
install.packages("na.tools")
install.packages("tidyverse")
install.packages("scales")
install.packages("caret")
# install.packages("randomForest")
install.packages("crayon")
# install.packages("treemapify")
# install.packages("sunburstR")
install.packages("ggplotify")
install.packages("treemap")
install.packages("plotly")
install.packages("waffle")
install.packages("ggmosaic")
install.packages("ggalt")
# install.packages("ggrepel")
install.packages("ggmap")
install.packages("plotrix")
install.packages("directlabels")



# Utilizing Packages
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(validate)
library(naniar)
library(arsenal)
library(hrbrthemes)
library(RColorBrewer)
library(gridExtra)
library(ggridges)
library(stringr)
library(viridis)
library(tidyr)
library(httr)
library(jsonlite)
library(maps)
library(mapdata)
library(ggrepel)
library(crayon)
library(na.tools)
library(tidyverse)
library(scales)
library(caret)
# library(randomForest)
library(crayon)
library(treemapify)
# library(sunburstR)
library(ggplotify)
library(treemap)
library(ggfittext)
library(plotly)
library(waffle)
library(ggmosaic)
library(ggalt)
library(ggrepel)
library(ggmap)
library(plotrix)
library(directlabels)



# Set The Working Directory / Path
setwd("C:\\Users\\danie\\Documents\\Degree Year 2 2023\\Programming on Data Analytics\\Assignment")


# Get The Working Directory / Path
getwd()

# Import Data From Employee Attrition's .csv file
excelfile_path <- "C:\\Users\\danie\\OneDrive - Asia Pacific University\\Assignment\\R\\employee_attrition.csv"
employee_attrition <- read.csv(excelfile_path, header=TRUE)
employee_attrition

# Set How Many Output Lines You Want
options(max.print = 50000)



## Data Exploration (Observe the dataset)
# >> The initial step in data analysis where the dataset is examined to gain insights, understand the structure, 
# identify patterns, and summarize the main characteristics of the data. <<


# !! Some Functions That I Use To Check What The Dataset Is About !!
# Explore the dataset with
View(employee_attrition)
glimpse(employee_attrition)

# Count the number of occurences
count(employee_attrition)
print(count(employee_attrition), n = 50000)


# Display first and last few rows of the dataset.
head(employee_attrition)
tail(employee_attrition)

# Summarize the dataset
str(employee_attrition)
summary(employee_attrition)

# Calculate the dimension of the dataset
dim(employee_attrition)

# Find the names of the dataset
names(employee_attrition)

# Find specific column name of the dataset
# Ex:
table(employee_attrition$EmployeeID)

# Create a pivot table of the dataset to summarize/cross-tabulate
pivot_table <- employee_attrition %>%
  pivot_wider(names_from = department_name, 
              values_from = length_of_service, 
              values_fn = list(length_of_service = mean))

pivot_table # -----------/> [ Extra Feature 1 ]

# Create histogram of the dataset
hist(employee_attrition$age)

# Create plot for the dataset
plot(employee_attrition$length_of_service)

# Count Unique Employee ID from the dataset
employee_attrition %>% summarise(count = n_distinct(EmployeeID))

# Check Unique Dataset
unique(employee_attrition) # -----------/> [ Extra Feature 2 ]

# Check Each & Every Col Unique Values
unique(employee_attrition$EmployeeID)
unique(employee_attrition$recorddate_key)
unique(employee_attrition$birthdate_key)
unique(employee_attrition$orighiredate_key)
unique(employee_attrition$terminationdate_key)
unique(employee_attrition$age)
unique(employee_attrition$length_of_service)
unique(employee_attrition$city_name)
unique(employee_attrition$department_name)
unique(employee_attrition$job_title)
sort(unique(employee_attrition$job_title))
unique(employee_attrition$store_name)
unique(employee_attrition$gender_full)
unique(employee_attrition$termreason_desc)
unique(employee_attrition$termtype_desc)
unique(employee_attrition$STATUS_YEAR)
unique(employee_attrition$STATUS)
unique(employee_attrition$BUSINESS_UNIT)

# ----------------------------------------------------------------------------#
# or use for loop:

for (col in names(employee_attrition)) {
  unique_values <- unique(employee_attrition[[col]])
  cat("Unique values for", col, ":\n")
  print(unique_values)
  cat("\n")
}

# ----------------------------------------------------------------------------#

# Check Classes From The Names of the dataset
# Loop through each column
for (col in names(employee_attrition)) {
  var_class <- class(employee_attrition[[col]])
  cat("Variable", col, "has class:", var_class, "\n")
}


# Check missing values in each column
colSums(is.na(employee_attrition)) # -----------/> [ Extra Feature 3 ]

# Count missing values in each column
sapply(employee_attrition, function(x) sum(is.na(x))) # -----------/> [ Extra Feature 4 ]

# Check if termination dates are after hire dates
employee_attrition[employee_attrition$terminationdate_key < employee_attrition$orighiredate_key, ]

# Check if EmployeeID is unique
length(unique(employee_attrition$EmployeeID)) == nrow(employee_attrition)

# Identify near-zero variance variables
near_zero_vars <- nearZeroVar(employee_attrition) # -----------/> [ Extra Feature 5 ]
near_zero_vars

# Perform recursive feature elimination
# control <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
# rfe_result <- rfe(employee_attrition[, -near_zero_vars], employee_attrition$status, sizes = c(1:10), rfeControl = control)



# # Data pre-processing (Make the dataset better)
# >> Data pre-processing: The preparation stage that involves cleaning, transforming, and organizing the data to
# ensure it is in a suitable format for analysis, addressing missing values, outliers, and inconsistencies. <<

# Define the desired column order
reorder_cols <- c("EmployeeID", "age", "birthdate_key", "gender_short", "gender_full", "city_name", "recorddate_key",
                  "store_name", "BUSINESS_UNIT", "department_name", "job_title", "length_of_service",
                  "orighiredate_key", "terminationdate_key", "termtype_desc", "termreason_desc", "STATUS_YEAR", "STATUS")

# Reorder the columns
employee_attrition <- employee_attrition %>% 
  dplyr::select(reorder_cols) # -----------/> [ Extra Feature 6 ]


# Renaming the names of the dataset 
employee_attrition <- employee_attrition %>%
  rename(
    employee_id = EmployeeID,
    record_date = recorddate_key,
    birth_date = birthdate_key,
    hired_date = orighiredate_key,
    terminated_date = terminationdate_key,
    age = age,
    length_of_service = length_of_service,
    city = city_name,
    department_name = department_name,
    job_title = job_title,
    store_id = store_name,
    gender_abbreviation = gender_short,
    gender = gender_full,
    termination_reason = termreason_desc,
    termination_type = termtype_desc,
    status_year = STATUS_YEAR,
    status = STATUS,
    business_unit = BUSINESS_UNIT
  )
names(employee_attrition)

# ----------------------------------------------------------------------------#
# or can change it like this:
# Rename Store ID to Store Name
employee_attrition <- employee_attrition %>% rename(store_id = store_name)

# Rename Termination Date to preferred version:
employee_attrition <- employee_attrition %>% rename(terminated_date = terminationdate_key)

# ----------------------------------------------------------------------------#


# Check gender_short and gender_full
check_gender <- validator(gender_short == "M" & gender == "Male" |
                           gender_short == "F" & gender == "Female" 
                           )  # -----------/> [ Extra Feature 7 ]
employee_attrition %>%
  confront(check_gender) %>% # -----------/> [ Extra Feature 8 ]
  summary


# ----------------------------------------------------------------------------#

# Rename this resignation word (spelling error)
employee_attrition$termination_reason <- ifelse(employee_attrition$termination_reason == "Resignaton", "Resignation", employee_attrition$termination_reason)

# Rename this job title #1 (spelling error)
employee_attrition$job_title <- ifelse(employee_attrition$job_title=="Account Receiveable","Account Receivable",employee_attrition$job_title)

# Rename this job title #2 (spelling error)
employee_attrition$job_title <- ifelse(employee_attrition$job_title == "CHief Information Officer", "Chief Information Officer", employee_attrition$job_title)

# Rename this city name (spelling error)
# levels(employee_attrition$city)[levels(employee_attrition$city) == "New Westminister"] = "New Westminster"
employee_attrition$city <- ifelse(employee_attrition$city == "New Westminister", "New Westminster", employee_attrition$city)

# Remove gender_short due to repetition with gender_full
employee_attrition <- employee_attrition %>% select(-gender_abbreviation) # -----------/> [ Extra Feature 9 ]

# ----------------------------------------------------------------------------#
# or this also works
employee_attrition$gender_abbreviation = NULL
# ----------------------------------------------------------------------------#



# Correct the word spelling mistake
employee_attrition <- employee_attrition %>% 
  mutate(
    terminated_date = ifelse(terminated_date == "1/1/1900", NA, terminated_date),
    termination_reason = ifelse(termination_reason == "Resignaton", "Resignation", termination_reason)
  ) # -----------/> [ Extra Feature 10 ]

saved_ea <- employee_attrition
saved_ea

# ----------------------------------------------------------------------------#
# and

# Termination Date (1/1/1900) turn to "NA" (Not Applicable)
employee_attrition <- employee_attrition %>%
  replace_with_na(replace = list(terminated_date = ymd('1900-01-01'))) # -----------/> [ Extra Feature 11 ]

# Termination Type from "Not Applicable" turn to "NA" 
employee_attrition <- employee_attrition %>%
  replace_with_na(replace = list(termination_type = 'Not Applicable'))

# Termination Reason from "Not Applicable" turn to "NA" 
employee_attrition <- employee_attrition %>%
  replace_with_na(replace = list(termination_reason = 'Not Applicable'))

# ----------------------------------------------------------------------------#


## Data Cleaning (Mostly Clean the unimportant stuffs)
# >> The process of identifying and correcting or removing errors, 
# inconsistencies, and inaccuracies in the dataset to improve data quality and reliability.<<


new_employee_attrition_data <- employee_attrition_data %>%
  group_by(EmployeeID) %>% # -----------/> [ Extra Feature 12 ]
  slice_max(order_by = age) %>%
  slice_max(order_by = termreason_desc) # -----------/> [ Extra Feature 13 ]

# employee_attrition <- employee_attrition%>%
#   group_by(employee_id)%>%
#   dplyr::arrange(desc(record_date),.by_group = TRUE)%>% # -----------/> [ Extra Feature 14 ]
#   distinct(employee_id,.keep_all = TRUE)

# employee_attrition <- employee_attrition %>%
#   group_by(employee_id) %>%
#   arrange(desc(record_date), .by_group = TRUE) %>% # -----------/> [ Extra Feature 15 ]
#   slice(1) %>%
#   ungroup()

View(employee_attrition)

# Check Missing Value
is.na(employee_attrition) # -----------/> [ Extra Feature 16 ]

# Count Missing Value
colSums(is.na(employee_attrition))

# Recognize Complete Cases From The Data Frame
complete.cases(employee_attrition)

# Remove Any Rows Contain Missing Values from the dataset 
na.omit(employee_attrition) # -----------/> [ Extra Feature 17 ]

nrow(na.omit(employee_attrition))

# Check Duplication of the data set
duplicated(employee_attrition) # -----------/> [ Extra Feature 18 ]

# Count Duplicated Row(s) if there is
employee_attrition[duplicated(employee_attrition),] %>% nrow()

# Count number of duplicated rows on the dataset
sum(duplicated(employee_attrition))

# Remove the names/columns that are not necessary
# employee_attrition <- employee_attrition %>% select(-gender_abbreviation, -terminationdate_key, -termreason_desc)
employee_attrition <- employee_attrition %>% select(-terminationdate_key, -termreason_desc)

View(employee_attrition)

# employee_attrition <- employee_attrition %>% select(everything(), -c(gender_abbreviation))



## Data Transformation (Converting DataType Format)
# >>  The conversion of raw data into a format suitable for analysis or modeling, which may include aggregating, 
# scaling, normalizing, or creating new variables based on the existing data. <<

# Convert the type of store_name from integer to character
employee_attrition$store_id <- as.character(employee_attrition$store_id)

# Convert record date key to class type date
employee_attrition$record_date <- as.Date(employee_attrition$record_date, format = "%m/%d/%Y") # -----------/> [ Extra Feature 19 ]
class(employee_attrition$record_date)

# Convert birth date key to class type date
employee_attrition$birth_date <- as.Date(employee_attrition$birth_date, format = "%m/%d/%Y")

# Convert from char datatype to date datatype (hired date)
employee_attrition$hired_date <- as.Date(employee_attrition$hired_date, format = "%m/%d/%Y")

# Convert from char datatype to date datatype (termination date)
employee_attrition$terminated_date = as.Date(employee_attrition$terminated_date, format == "%m/%d/%Y")

employee_attrition <- employee_attrition %>%
  mutate(
    terminated_date = as.Date(terminated_date, format = "%m/%d/%Y")
  )


# ----------------------------------------------------------------------------#
# or 
# Convert char datatype to date datatype
employee_attrition <- employee_attrition %>%
  mutate(
    birth_date = as.Date(birth_date, format = "%m/%d/%Y"),
    record_date = as.Date(record_date, format = "%m/%d/%Y"),
    hired_date = as.Date(hired_date, format = "%m/%d/%Y"),
    
  )

# Convert char datatype to factor datatype
employee_attrition <- employee_attrition %>%
  mutate(
    city = as.factor(city),
    department_name = as.factor(department_name),
    job_title = as.factor(job_title),
    # store_name = as.factor(store_name),
    # gender_short = as.factor(gender_short),
    gender = as.factor(gender),
    termination_reason = as.factor(termination_reason),
    termination_type = as.factor(termination_type),
    status_year = as.factor(status_year),
    status = as.factor(status),
    business_unit = as.factor(business_unit)
  ) 
  
# ----------------------------------------------------------------------------#


# Check classes to see if it is changed
check_class_ea <- sapply(employee_attrition, class)
check_class_ea

# Remove weird classes that display on the latest dataset
# terminated_date <- as.Date(terminated_date, format = "%m/%d/%Y"
# employee_attrition <- employee_attrition[, -which(names(employee_attrition) == 'terminated_date <- as.Date(terminated_date, format = "%m/%d/%Y"')]



## Saving Clean, Processed Dataset 
# >> Why Use .rds format? because it is for R specifically, basically binary format that it stores.
employee_attrition %>% saveRDS(file = "clearedEmployeeAttrition.rds") # -----------/> [ Extra Feature 20 ]

## Using the clean, processed dataset
new_employee_attrition <- readRDS(file = "clearedEmployeeAttrition.rds") # -----------/> [ Extra Feature 21 ]
new_employee_attrition


# The Finalized, Saved Version
View(new_employee_attrition)


# Check classes to see if it is changed, saved version.
finalized <- sapply(new_employee_attrition, class)
finalized

# Presenter: Daniel Poh Ting Fong (TP056258)

#----------------------------------------------------------------------------------#

### ---------------------- Questions To Analysis -------------------------- ###

## Question 1: What Makes Employees Quit Their Jobs? (Termination)


# Analysis 1.1 - Is there correlation between termination type and length of service?

# Filter the data to include only relevant columns and drop NA values
filtered_data <- new_employee_attrition %>%
  select(termination_type, length_of_service) %>%
  drop_na() # -----------/> [ Extra Feature 21 ]

# Group by termination type and calculate the count of each length of service
termtype_los <- filtered_data %>%
  group_by(termination_type, length_of_service) %>%
  summarize(count = n()) # -----------/> [ Extra Feature 22 ]

# Create a line chart with theme and theme_minimal # -----------/> [ Extra Feature 23 ]
ggplot(termtype_los, aes(x = length_of_service, y = count, color = termination_type)) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 21, fill = "lightskyblue1") +
  labs(x = "Length of Service", y = "Count", color = "Termination Type", 
       title = "Correlation between Termination Type and Length of Service") +
  theme_minimal() 


ggsave(filename = "analysis_1p1.png", plot = last_plot(), dpi = 300) # -----------/> [ Extra Feature 24 ]



# Analysis 1.2 - I want to investigate if voluntary termination is related due to length of service?

# Filter the data to include only voluntary terminations and length of service columns
filtered_data <- new_employee_attrition %>%
  filter(termination_type == "Voluntary") %>%
  select(termination_type, length_of_service) # -----------/> [ Extra Feature 25 ]

# Group by length of service and calculate the count of voluntary terminations
voluntary_length_of_service <- filtered_data %>%
  group_by(length_of_service) %>%
  summarize(count = n())

# Sort the data by length of service
voluntary_length_of_service <- voluntary_length_of_service[order(voluntary_length_of_service$length_of_service), ]

# Create an area chart to visualize the relationship # -----------/> [ Extra Feature 26: All the charts' functions are extra features basically. ]
ggplot(voluntary_length_of_service, aes(x = length_of_service, y = count)) +
  geom_area(fill = "steelblue", alpha = 0.7) +
  labs(x = "Length of Service", y = "Count", 
       title = "Relationship Between Voluntary Termination & Length Of Service") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(0, max(voluntary_length_of_service$length_of_service), by = 5)) +
  scale_y_continuous(limits = c(0, max(voluntary_length_of_service$count) * 1.1)) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.position = "none")


# Analysis 1.3 - I want to investigate if involuntary termination is related due to length of service?

# Filter the data to include only involuntary terminations and length of service columns
filtered_data <- new_employee_attrition %>%
  filter(termination_type == "Involuntary") %>%
  select(termination_type, length_of_service)

# Group by length of service and calculate the count of each termination type
involuntary_termination <- filtered_data %>%
  group_by(length_of_service, termination_type) %>%
  summarize(count = n())

# Create a stacked area chart to visualize the relationship
ggplot(involuntary_termination, aes(x = length_of_service, y = count, fill = termination_type)) +
  geom_area(color = "white") +
  labs(x = "Length of Service", y = "Count", title = "Relationship between Involuntary Termination and Length of Service") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 11, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("darkgreen", "lightgreen")) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.position = "bottom")


# Analysis 1.4 - What is the correlation between voluntary termination (termination type) and age of employee?

# Filter the data to include only voluntary terminations and age columns
filtered_data <- new_employee_attrition %>%
  filter(termination_type == "Voluntary") %>%
  select(termination_type, age)

# Group by termination type and calculate the count of each age
voluntary_age <- filtered_data %>%
  group_by(age) %>%
  summarize(count = n())

# Create a connected scatter plot to visualize the correlation
ggplot(voluntary_age, aes(x = age, y = count)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(x = "Age", y = "Count", title = "Correlation between Voluntary Termination and Age") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_color_gradient(low = "darkblue", high = "lightblue") +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.position = "none")


# Analysis 1.5 - What is the correlation between involuntary termination (termination type) and age of employee?

# Filter the data to include only involuntary terminations and age columns, and group by age
involuntary_age <- new_employee_attrition %>%
  filter(termination_type == "Involuntary") %>%
  group_by(age) %>%
  summarize(count = n())

# Create a scatter plot with enhanced aesthetics
ggplot(involuntary_age, aes(x = age, y = count)) +
  geom_point(color = "#FF6384", size = 4, alpha = 0.8) +
  labs(x = "Age", y = "Count", title = "Correlation between Involuntary Termination and Age") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 11, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.7, "cm")) +
  scale_color_gradient(low = "#FF6384", high = "#FFABAB") +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))


# Analysis 1.6 - Is there correlation between termination reason and age of employee?

# Filter, group, summarize, and arrange the data
termreason_age <- new_employee_attrition %>%
  filter(!is.na(termination_reason)) %>%
  group_by(age, termination_reason) %>%
  summarize(count = n())

# Create a stacked bar chart 
ggplot(termreason_age, aes(x = age, y = count, fill = termination_reason)) +
  geom_bar(stat = "identity") +
  labs(x = "Age", y = "Total Count Of Term. Reasons", fill = "Termination Reason") +
  ggtitle("Correlation Between Termination Reasons & Employee Ages")


# Analysis 1.7 - How does employees' genders relate to their reasons for leaving?

# Filter, group, summarize, and arrange the data
termreason_gender <- employee_attrition %>%
  filter(!is.na(termination_reason) & !is.na(gender)) %>%
  group_by(gender, termination_reason) %>%
  summarize(count = n()) %>%
  arrange(desc(gender))

# Create a visually appealing grouped bar chart
ggplot(termreason_gender, aes(x = gender, y = count, fill = termination_reason)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) +
  labs(x = "Gender", y = "Total Count of Term. Reasons", fill = "Termination Reason") +
  ggtitle("Correlation Between Termination Reasons & Gender") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")


# Analysis 1.8 - What is the relationship between termination types and gender?

# Filter the data to include only relevant columns and non-null values for termination type and gender
filtered_data <- new_employee_attrition %>%
  filter(!is.na(termination_type) & !is.na(gender))

# Group by termination type and gender and calculate the count of each combination
grouped_data <- filtered_data %>%
  group_by(termination_type, gender) %>%
  summarize(count = n())

# Create a stacked bar chart
ggplot(grouped_data, aes(x = termination_type, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(x = "Termination Type", y = "Count", fill = "Gender") +
  ggtitle("Relationship Between Termination Types & Gender") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Analysis 1.9 - What is the relationship between voluntary termination and gender?

# Filter the data to include only relevant columns and non-null values for termination type and gender
filtered_data <- new_employee_attrition %>%
  filter(!is.na(termination_type) & !is.na(gender))

# Filter the data to include only voluntary terminations
voluntary_data <- filtered_data %>%
  filter(termination_type == "Voluntary")

# Group by gender and calculate the count of each gender
grouped_data <- voluntary_data %>%
  group_by(gender) %>%
  summarize(count = n())

grouped_data

# Create a pie chart with distinct colors for each gender category
ggplot(grouped_data, aes(x = "", y = count, fill = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  labs(x = "", y = "", fill = "Gender") +
  ggtitle("Correlation Between Voluntary Termination & Gender") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_manual(values = c("#FF6384", "#36A2EB"))



# Analysis 1.10 - What is the relationship between involuntary termination and gender?

# Filter the data to include only involuntary terminations and gender columns
filtered_data <- new_employee_attrition %>%
  filter(termination_type == "Involuntary" & !is.na(gender))

# Group by gender and calculate the count of involuntary terminations
grouped_data <- filtered_data %>%
  group_by(gender) %>%
  summarize(count = n())

# Create a pie chart with text labels
ggplot(grouped_data, aes(x = "", y = count, fill = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  labs(x = "", y = "", fill = "Gender") +
  ggtitle("Correlation Between Involuntary Termination & Gender") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_manual(values = c("lightsalmon1", "salmon4")) +
  coord_polar("y", start = 0, direction = -1) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 1.11 - Are job titles make employees terminated voluntarily? 
# termination type - voluntary

# Filter the data to include only relevant columns and voluntary terminations
filtered_data <- new_employee_attrition %>%
  filter(termination_type == "Voluntary" & !is.na(job_title))

# Group by job title and calculate the count of voluntary terminations
grouped_data <- filtered_data %>%
  group_by(job_title) %>%
  summarize(count = n())

# Create a horizontal bar chart with a bigger plot title
ggplot(grouped_data, aes(x = count, y = job_title, fill = job_title)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Count", y = "Job Title", fill = "Job Title") +
  ggtitle("Relationship Between Voluntary Termination & Job Title") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16))


# Analysis 1.12 - Are job titles make employees terminated involuntarily? 
# termination type - Involuntary

# Filter the data to include only relevant columns and involuntary terminations
filtered_data <- new_employee_attrition %>%
  filter(termination_type == "Involuntary" & !is.na(job_title))

# Group by job title and calculate the count of involuntary terminations
grouped_data <- filtered_data %>%
  group_by(job_title) %>%
  summarize(count = n())

# Create a vertical bar chart
ggplot(grouped_data, aes(x = job_title, y = count, fill = job_title)) +
  geom_bar(stat = "identity", color = "white") +
  labs(x = "Job Title", y = "Count", fill = "Job Title") +
  ggtitle("Connection Between Involuntary Termination & Job Title") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))


# Analysis 1.13 - Does the department in which employees work have an impact on their likelihood of termination?

# Filter the data to include only relevant columns
filtered_data <- new_employee_attrition %>%
  filter(!is.na(department_name) & !is.na(termination_type))

# Group by department and termination type, and calculate the count of each combination
grouped_data <- filtered_data %>%
  group_by(department_name, termination_type) %>%
  summarize(count = n())

# Create a stacked bar chart
ggplot(grouped_data, aes(x = department_name, y = count, fill = termination_type)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Department", y = "Count", fill = "Termination Type") +
  ggtitle("Connection Between Termination Type & Department") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


# Analysis 1.14 - Does the business unit in which employees work have an influence on the occurrence of terminations?

# Filter the data to include only relevant columns
filtered_data <- new_employee_attrition %>%
  filter(!is.na(business_unit) & !is.na(termination_type))

# Group by business unit and calculate the count of terminations
grouped_data <- filtered_data %>%
  group_by(business_unit) %>%
  summarize(count = n())

# Sort the data by count in descending order
grouped_data <- grouped_data[order(grouped_data$count, decreasing = TRUE), ]

# Create a treemap chart
ggplot(grouped_data, aes(area = count, fill = business_unit, label = business_unit)) +
  geom_treemap() +
  geom_treemap_text(fontface = "bold", color = "white", place = "centre", min.size = 0) +
  labs(fill = "Business Unit") +
  ggtitle("Connection Between Termination Type & Business Unit") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom") +
  scale_fill_viridis_d(option = "D", direction = -1) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_equal() +
  theme(legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12, face = "bold"))


# Analysis 1.15 - Does city have to do with the result of terminations from employees?


# Filter the data to include only relevant columns
filtered_data <- new_employee_attrition %>%
  filter(!is.na(city) & status == "TERMINATED")

# Group by city and calculate the count of terminations
grouped_data <- filtered_data %>%
  group_by(city) %>%
  summarize(count = n())

# Sort the data by count in descending order
grouped_data <- grouped_data[order(grouped_data$count, decreasing = TRUE), ]

# Create a bar chart
ggplot(grouped_data, aes(x = city, y = count, fill = city)) +
  geom_bar(stat = "identity", color = "ivory1") +
  labs(x = "City", y = "Termination Count", fill = "City") +
  ggtitle("Correlation Between Terminations Count & City") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


## Question 2: How is the gender distribution distributed among the employees in the company?


# Analysis 2.1 - What are the total male and female of employees in the company?

# Convert gender to character type
filtered_data$gender <- as.character(filtered_data$gender)

# Group by gender and calculate the count of employees
grouped_data <- filtered_data %>%
  group_by(gender) %>%
  summarize(count = n())

# Create a donut chart
plot_ly(grouped_data, labels = ~gender, values = ~count, type = 'pie',
        text = ~paste(gender, ": ", count), textposition = 'inside',
        hole = 0.6, marker = list(colors = c("pink", "steelblue"))) %>%
  layout(title = list(text = "Gender Proportion in the Company", x = 0.5),
         showlegend = TRUE,
         legend = list(orientation = "h", x = 0.5, y = -0.15))


# Analysis 2.2 - What is the gender ratio between males and females here?

# Filter the data to include only relevant columns
filtered_data <- new_employee_attrition %>%
  filter(!is.na(gender))

# Group by gender and calculate the count of employees
grouped_data <- filtered_data %>%
  group_by(gender) %>%
  summarize(count = n())

# Calculate the total count of employees
total_count <- sum(grouped_data$count)

# Calculate the gender ratio
grouped_data <- grouped_data %>%
  mutate(ratio = count / total_count * 100)

# Create a stacked bar chart
ggplot(grouped_data, aes(x = 1, y = ratio, fill = gender)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "", y = "Ratio (%)", fill = "Gender") +
  ggtitle("Gender Ratio in the Company") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip()


# Analysis 2.3 - How does the gender distribution vary across different departments in the company?

# Filter the data to include only relevant columns
filtered_data <- new_employee_attrition %>%
  filter(!is.na(gender) & !is.na(department_name))

# Group by department and gender, and calculate the count of employees
grouped_data <- filtered_data %>%
  group_by(department_name, gender) %>%
  summarize(count = n())

# Calculate the proportion of each gender within each department
proportion_data <- grouped_data %>%
  group_by(department_name) %>%
  mutate(proportion = count / sum(count))

# Create a stacked bar chart
ggplot(proportion_data, aes(x = department_name, y = proportion, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(x = "Department", y = "Proportion", fill = "Gender") +
  ggtitle("Gender Distribution across Departments") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1))


# Analysis 2.4 - Is there a difference in the gender distribution between different job titles within the company?

# Filter the data to include only relevant columns
filtered_data <- new_employee_attrition %>%
  filter(!is.na(gender) & !is.na(job_title))

# Group by job title and gender, and calculate the count of employees
grouped_data <- filtered_data %>%
  group_by(job_title, gender) %>%
  summarize(count = n())

# Calculate the proportion of each gender within each job title
proportion_data <- grouped_data %>%
  group_by(job_title) %>%
  mutate(proportion = count / sum(count))

# Create a grouped dot plot
ggplot(proportion_data, aes(x = job_title, y = proportion, color = gender)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = "Job Title", y = "Proportion", color = "Gender") +
  ggtitle("Gender Distribution across Job Titles") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())


# Analysis 2.5 - I want to know if there is connection between length of service and gender.


# Filter the data to include only relevant columns
filtered_data <- new_employee_attrition %>%
  filter(!is.na(gender) & !is.na(length_of_service))

# Create a violin plot
ggplot(filtered_data, aes(x = gender, y = length_of_service, fill = gender)) +
  geom_violin(trim = FALSE) +
  labs(x = "Gender", y = "Length of Service", fill = "Gender") +
  ggtitle("Length of Service by Gender") +
  theme_minimal() +
  theme(legend.position = "none")


# Analysis 2.6 - Is there relationship between gender and the age of employees in the company?

# Filter the data to include only relevant columns
filtered_data <- new_employee_attrition %>%
  filter(!is.na(gender) & !is.na(age))

# Create a box plot
ggplot(filtered_data, aes(x = gender, y = age, fill = gender)) +
  geom_boxplot(color = "black", outlier.shape = NA) +
  labs(x = "Gender", y = "Age", fill = "Gender") +
  ggtitle("Relationship between Gender and Age") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "slategrey"),
        axis.text = element_text(size = 12, color = "gray40"),
        axis.title = element_text(size = 14, face = "bold", color = "steelblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(fill = "bisque1"),
        # plot.border = element_rect(color = "steelblue", fill = NA, size = 1),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "gray80", fill = NA),
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = "lightgray", color = NA),
        strip.background = element_rect(fill = "steelblue", color = NA),
        strip.text = element_text(size = 12, face = "bold", color = "white"))


# Analysis 2.7 - Is there a relationship between gender and departments in the company?

# Filter the data to include only relevant columns
filtered_data <- new_employee_attrition %>%
  filter(!is.na(gender) & !is.na(department_name))

# Group by gender and department, and calculate the count of employees
grouped_data <- filtered_data %>%
  group_by(gender, department_name) %>%
  summarize(count = n())

# Create a heatmap chart with red color palette
ggplot(grouped_data, aes(x = department_name, y = gender, fill = count)) +
  geom_tile() +
  labs(x = "Department", y = "Gender", fill = "Count") +
  ggtitle("Correlation between Gender and Department") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_gradient(low = "red4", high = "orange")


# Analysis 2.8 - Investigate the gender distribution of Cashier (Job Title).

# Filter the data to include only employees with the job title "Cashier"
filtered_data <- new_employee_attrition %>%
  filter(job_title == "Cashier")

# Group by gender and calculate the count of employees
grouped_data <- filtered_data %>%
  group_by(gender) %>%
  summarize(count = n())

# Create a pie chart
ggplot(grouped_data, aes(x = "", y = count, fill = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL, fill = "Gender") +
  ggtitle("Gender Distribution of Cashiers") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold")) +
  scale_fill_manual(values = c("steelblue", "pink"))


# Analysis 2.9 - Identify the gender proportion of current active employees.

# Filter the data to include only current active employees
filtered_data <- new_employee_attrition %>%
  filter(status == "ACTIVE")

# Create a boxplot
ggplot(filtered_data, aes(x = gender, y = age, fill = gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Age", fill = "Gender") +
  ggtitle("Gender Distribution of Current Active Employees") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


# Analysis 2.10 - Identify the gender proportion of past employees.

# Filter the data to include only past employees
filtered_data <- new_employee_attrition %>%
  filter(status == "TERMINATED")

# Group by gender and calculate the count of past employees
grouped_data <- filtered_data %>%
  group_by(gender) %>%
  summarize(count = n())

# Calculate the proportion of each gender
grouped_data <- grouped_data %>%
  mutate(proportion = count / sum(count))

# Create a horizontal bar chart
ggplot(grouped_data, aes(x = proportion, y = reorder(gender, proportion), fill = gender)) +
  geom_col() +
  labs(x = "Proportion", y = "Gender", fill = "Gender") +
  ggtitle("Gender Proportion of Past Employees") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(hjust = 0))


# Analysis 2.11 -  Does the gender ratio change over time? (e.g., analyzing gender distribution by year)?

# Filter the data to include only relevant columns
filtered_data <- new_employee_attrition %>%
  filter(!is.na(gender) & !is.na(status_year))

# Group by status_year and gender, and calculate the count of employees
grouped_data <- filtered_data %>%
  group_by(status_year, gender) %>%
  summarize(count = n())

# Calculate the proportion of each gender within each year
proportion_data <- grouped_data %>%
  group_by(status_year) %>%
  mutate(proportion = count / sum(count))

# Create a bar chart with modified colors
ggplot(proportion_data, aes(x = as.factor(status_year), y = proportion, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Proportion", fill = "Gender") +
  ggtitle("Gender Distribution Over Time") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightyellow2", "purple"))



# Analysis 2.12 - Are there any variations in the gender distribution based on the business units in the company?

# Filter the data to include only relevant columns
filtered_data <- new_employee_attrition %>%
  filter(!is.na(gender) & !is.na(business_unit))

# Group by business_unit and gender, and calculate the count of employees
grouped_data <- filtered_data %>%
  group_by(business_unit, gender) %>%
  summarize(count = n())

# Calculate the proportion of each gender within each business unit
proportion_data <- grouped_data %>%
  group_by(business_unit) %>%
  mutate(proportion = count / sum(count))

# Define a custom color palette
colors <- brewer.pal(3, "Set2")

# Create a stacked bar chart with custom colors
ggplot(proportion_data, aes(x = business_unit, y = proportion, fill = gender)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Business Unit", y = "Proportion", fill = "Gender") +
  ggtitle("Gender Distribution by Business Unit") +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))


# Analysis 2.13 - How many female employees are still active in this company from 2010 to 2011?

# Convert status_year to numeric
new_employee_attrition$status_year <- as.numeric(as.character(new_employee_attrition$status_year))

# Filter the data to include only relevant columns and years 2010-2011
filtered_data <- new_employee_attrition %>%
  filter(gender == "Female" & status_year >= 2010 & status_year <= 2011)

# Group by status_year and calculate the count of active females
grouped_data <- filtered_data %>%
  group_by(status_year) %>%
  summarize(count = n())

# Create a scatter plot with encircling
ggplot(grouped_data, aes(x = status_year, y = count)) +
  geom_point() +
  geom_encircle(data = grouped_data[grouped_data$status_year >= 2010 & grouped_data$status_year <= 2011, ], 
                aes(x = status_year, y = count), color = "blue", expand = 0.1) +
  labs(x = "Year", y = "Count", title = "Number of Active Females (2010-2011)") +
  theme_minimal()


# Analysis 2.14 - Which gender dominates from the highest job positions in the company?

# Define the job titles of interest
higher_positions <- c("CEO", "Chief Information Officer", "VP Finances", "VP Human Resources", "VP Stores")

# Filter the data to include only relevant job titles
filtered_data <- new_employee_attrition %>%
  filter(job_title %in% higher_positions)

# Group by gender and job title, and calculate the count of employees
grouped_data <- filtered_data %>%
  group_by(gender, job_title) %>%
  summarize(count = n())

# Order the job titles by count in descending order
grouped_data <- grouped_data %>%
  arrange(job_title, desc(count))

# Create the lollipop chart
ggplot(grouped_data, aes(x = reorder(job_title, count), y = count, fill = gender)) +
  geom_segment(aes(xend = reorder(job_title, count), yend = 0), color = "black") +
  geom_point(size = 3, color = "black", shape = 21) +
  labs(x = "Job Title", y = "Count", fill = "Gender") +
  ggtitle("Gender Distribution in Higher Job Positions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


# Analysis 2.15 - I want to know gender distribution from the stores? 

# Filter the data to include only relevant columns
filtered_data <- new_employee_attrition %>%
  filter(!is.na(store_id) & !is.na(gender))

# Group by store_id and gender, and calculate the count of employees
grouped_data <- filtered_data %>%
  group_by(store_id, gender) %>%
  summarize(count = n())

# Create a bar chart
ggplot(grouped_data, aes(x = store_id, y = count, fill = gender)) +
  geom_bar(stat = "identity", color = "ivory1") +
  labs(x = "Store ID", y = "Count", fill = "Gender") +
  ggtitle("Gender Distribution from Stores") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))



## Question 3: At What age do employees typically retire?

# Analysis 3.1 - What are the retirement ages among the employees?

# Filter, group, summarize, and arrange the data
ret_age <- new_employee_attrition %>%
  filter(termination_reason == "Retirement") %>%
  group_by(age) %>%
  summarize(ret_count = n())

# Calculate density estimate
density_data <- density(ret_age$age)  

# Create a density plot
ggplot(ret_age, aes(x = age)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.7) +
  xlab("Age") +
  ylab("Density") +
  ggtitle("Density Plot of Retirement Age") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 3.2 - What is the average retirement age in the company?

# Filter the data for employees with retirement as the termination reason
retirement_data <- new_employee_attrition %>%
  filter(termination_reason == "Retirement")

# Calculate the average retirement age
average_retirement_age <- mean(retirement_data$age, na.rm = TRUE)

# Print the average retirement age in the company
cat("Average Retirement Age in the Company: ", round(average_retirement_age), " years\n")

# Create a ggplot chart to visualize the average retirement age
ggplot(retirement_data, aes(x = age)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = average_retirement_age), color = "red", linetype = "dashed", size = 1) +
  xlab("Age") +
  ylab("Density") +
  ggtitle("Density Plot of Retirement Age") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 3.3 - How does the average retirement age vary across different departments?

# Filter, group and summarize the data
retirement_age_by_dept <- new_employee_attrition %>%
  filter(termination_reason == "Retirement") %>%
  group_by(department_name) %>%
  summarize(Average_Retirement_Age = round(mean(age), 2))  # Round the average retirement age

# Print the retirement age in each department
cat("Retirement Age by Department:\n")
cat(paste(retirement_age_by_dept$department_name, ": ", retirement_age_by_dept$Average_Retirement_Age, " years\n"))

# Create a bar chart of average retirement age by department
ggplot(data = retirement_age_by_dept, aes(x = department_name, y = Average_Retirement_Age, fill = department_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Retirement Age in Each Department", x = "Department", y = "Average Retirement Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 0.8, vjust = 1.1))


# Analysis 3.4 - What is the average retirement age for employees in different job titles?

# Convert Age column to numeric
new_employee_attrition$age <- as.numeric(as.character(new_employee_attrition$age))

# Filter and summarize data
ret_job_ave <- new_employee_attrition %>%
  filter(termination_reason == "Retirement") %>%
  group_by(job_title) %>%
  summarise(job_mean = mean(age, na.rm = TRUE))

# Box plot with a more colorful palette
ggplot(ret_job_ave, aes(x = job_title, y = job_mean)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(x = "Job Title", y = "Average Retirement Age") +
  ggtitle("Average Retirement Age in Each Job Title") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) +
  scale_fill_manual(values = brewer.pal(12, "Set3"))  # Use the "Set3" palette with 12 colors


# Analysis 3.5 - How does the retirement age differ among different business units?

# Calculate the number of retirements by business unit
busunit_ret <- new_employee_attrition %>%
  filter(termination_reason == "Retirement") %>%
  group_by(business_unit) %>%
  summarise(bunit = n())

# Define colors for the bar chart
bu_color <- c("#0398fe", "#1265f4")

# Bar chart with facetting
ggplot(busunit_ret, aes(x = business_unit, y = bunit, fill = business_unit)) +
  geom_col() +
  labs(x = "Business Unit", y = "Number of Retirements") +
  ggtitle("Retirements by Business Unit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_fill_manual(values = bu_color) +
  facet_wrap(~ business_unit, ncol = 2)


# Analysis 3.6 - How many years have those retired employees worked for the company?

# Calculate the number of employees by retirement age and length of service
ser_ret <- new_employee_attrition %>%
  filter(termination_reason == "Retirement") %>%
  group_by(length_of_service, age) %>%
  summarise(ser_leng_ret = n(), .groups = "drop")

# Create a 3D scatter plot to visualize the relationship between retirement age, length of service, and employee count
plot_ly(ser_ret, x = ~length_of_service, y = ~age, z = ~ser_leng_ret,
        type = "scatter3d", mode = "markers") %>%
  add_markers(marker = list(size = 5, opacity = 0.8, name = "Employee Count")) %>%
  layout(scene = list(xaxis = list(title = "Length of Service"),
                      yaxis = list(title = "Retirement Age"),
                      zaxis = list(title = "Employee Count"),
                      title = list("The Relationship between Retirement Age, Length of Service, and Total Employee"),
                      camera = list(eye = list(x = 1.7, y = 1.7, z = 0.5))),
         title = "The Relationship between Retirement Age, Length of Service, and Total Employee")



# Analysis 3.7 - What is the gender distribution among retired employees?

# Calculate the count of retirements by gender and age
gdr_ret <- new_employee_attrition %>%
  filter(termination_reason == "Retirement") %>%
  group_by(gender, age) %>%
  summarise(gdr_count = n(), .groups = "drop")

# Create a faceted stacked bar plot to visualize the gender distribution among retired employees
ggplot(gdr_ret, aes(x = age, y = gdr_count, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(x = "Retirement Age", y = "Count") +
  ggtitle("Gender Distribution Among Retired Employees") +
  theme_minimal() +
  facet_wrap(~ gender, ncol = 1) +
  scale_fill_manual(values = c("#0398fe", "#e6007e"))



# Analysis 3.8 - How many male employees are nearing retirement age?

# Filter and count male employees close to retirement age
male_ret <- new_employee_attrition %>%
  filter(gender == "Male", age >= 55, age <= 64, status == "ACTIVE") %>%
  group_by(age) %>%
  summarise(male_ret_age = n())

# Create a scatter plot to visualize the number of male employees close to retirement age
ggplot(data = male_ret, aes(x = age, y = male_ret_age)) +
  geom_point(shape = 11, size = 2) +
  labs(x = "Age", y = "Number of Male Employees") +
  ggtitle("Male Employees Close to Retirement Age") +
  scale_x_continuous(breaks = male_ret$age, labels = male_ret$age) +
  theme_minimal()


# Analysis 3.9 - Which department have the highest number of employees approaching retirement age?

# Calculate the data
dept_app_ret <- new_employee_attrition %>%
  filter(age >= 55, age <= 64, status == "ACTIVE") %>%
  group_by(department_name) %>%
  summarise(dept_ret_name = n())

# Get the labels and sizes
labels <- dept_app_ret$department_name
sizes <- dept_app_ret$dept_ret_name

# Generate a vector of colors for each department
colors <- rainbow(length(labels))

# Set the plot parameters for a smaller pie chart
par(mar = c(0, 4, 0, 0))  # Adjust the margin values as needed
radius <- 0.8  # Adjust the radius value for the desired size

# Create the 3D pie chart
pie3D(sizes, main = "The Number of Employees Approaching Retirement in each Department",
      col = colors, explode = 0.1, radius = radius, height = 0.3, theta = 0.9)

# Calculate the legend position coordinates
legend_x <- par("usr")[2] + 0.1
legend_y <- par("usr")[4] - 1.25

# Set the xpd parameter to allow plotting outside the plot region
par(xpd = TRUE)

# Calculate the number of columns for the legend
num_columns <- ceiling(length(labels) / 50)  # Adjust the number 10 based on your preference

# Create the legend on the right side with smaller size and multiple columns
legend(legend_x, legend_y, legend = labels, cex = 0.6, fill = colors, bty = "n", xjust = 0, yjust = 0.5, ncol = num_columns)

# Reset the xpd parameter to its default value
par(xpd = FALSE)

# Reset the plot parameters to their default values
par(mar = c(5, 4, 4, 2) + 0.1)  # Adjust the margin values as needed



# Analysis 3.10 - Which Job Title have the highest number of employees approaching retirement age?

# Filter the data to include only active employees between the ages of 55 and 64
job_app_ret <- new_employee_attrition %>%
  filter(age >= 55, age <= 64, status == "ACTIVE") %>%
  group_by(job_title, termination_reason) %>%
  summarise(job_app_ret_count = n(), .groups = "drop")

# Create a stacked area plot to visualize the number of employees approaching retirement by job title
ggplot(job_app_ret, aes(x = job_title, y = job_app_ret_count, group = termination_reason, fill = termination_reason)) +
  geom_area(position = "stack") +  # Use geom_area to create a stacked area plot
  labs(title = "The Number of Employees Approaching Retirement by Job Title", x = "Job Title", y = "Number of Employees") +
  scale_fill_manual(values = viridis::viridis(n_distinct(job_app_ret$termination_reason))) +  # Use viridis color palette for fill colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12), plot.title = element_text(hjust = 0.5))




## Question 4: Analyzing the Relationship Between Job Titles and Departments.

# Analysis 4.1 - What are the total number of employees in each department to provide insights of the relationship?

# Calculate the total number of employees in each department
department_counts <- new_employee_attrition %>%
  group_by(department_name) %>%
  summarize(total_employees = n())

# Bar chart to visualize the total number of employees by department
ggplot(department_counts, aes(x = department_name, y = total_employees)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Number of Employees by Department",
       x = "Department",
       y = "Total Employees") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Analysis 4.2 - What are the unique job titles in each department? 

# Obtain unique job titles in each department
unique_job_titles <- new_employee_attrition %>%
  group_by(department_name, job_title) %>%
  summarize(count = n()) %>%
  ungroup()

# Filter unique job titles with count <= 10
filtered_job_titles <- unique_job_titles %>%
  filter(count <= 10)

# Count the number of unique job titles per department
job_title_counts <- filtered_job_titles %>%
  group_by(department_name) %>%
  summarize(count = sum(count))

# Create a bar chart to visualize the number of unique job titles in each department
ggplot(job_title_counts, aes(x = department_name, y = count, fill = department_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Most Unique Job Titles in Each Department",
       x = "Department",
       y = "Number of Unique Job Titles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Analysis 4.3 -  What are the most common job titles across all departments?

# Calculate the frequency of each job title across all departments
job_title_freq <- new_employee_attrition %>%
  group_by(job_title) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency))

# Select the common job titles based on frequencies
most_job_titles <- head(job_title_freq, 10)

# Create a bar chart to visualize the top job titles
ggplot(most_job_titles, aes(x = reorder(job_title, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Most Common Job Titles Across All Departments",
       x = "Job Title",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Analysis 4.4 -  What is the employee count for each job title in each department

# Calculate the employee count for each job title in each department
employee_count <- new_employee_attrition %>%
  group_by(department_name, job_title) %>%
  summarize(count = n()) %>%
  ungroup()

# Create a table to display the employee count for each job title in each department
employee_count_table <- employee_count %>%
  pivot_wider(names_from = job_title, values_from = count, values_fill = 0)

# Print the first 10 rows of the table
print(employee_count_table, n = 23)



# Analysis 4,5 - Are there any job titles that are unique to a specific department?

# Calculate the count of unique job titles within each department
unique_job_titles <- new_employee_attrition %>%
  group_by(department_name) %>%
  summarise(unique_job_count = n_distinct(job_title))

# Identify the departments with a count of 1 (indicating unique job titles)
unique_departments <- unique_job_titles %>%
  filter(unique_job_count == 1)

# Filter the data to include only the unique job titles
unique_job_titles_data <- new_employee_attrition %>%
  filter(department_name %in% unique_departments$department_name)

# Create a bar chart to display the unique job titles by department
ggplot(unique_job_titles_data, aes(x = department_name, fill = job_title)) +
  geom_bar() +
  labs(title = "Unique Job Titles by Department",
       x = "Department",
       y = "Count") +
  theme_minimal()
guides(fill = guide_legend(title = "Job Title"))


# Analysis 4.6 -  How does the distribution of job titles vary across departments?

# Calculate the proportion of each job title within each department
job_title_proportions <- new_employee_attrition %>%
  group_by(department_name, job_title) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(department_name) %>%
  mutate(proportion = count / sum(count),
         label = paste0(percent(proportion), " (", count, ")"))  # Add percentage and count labels

# Create a grouped bar chart with smaller legend
ggplot(job_title_proportions, aes(x = department_name, y = proportion, fill = job_title)) +
  geom_bar(stat = "identity", position = "dodge") +
  # geom_text(aes(label = label), position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5, color = "white", fontface = "bold") +
  labs(title = "Distribution of Job Titles Across Departments",
       x = "Department", y = "Proportion",
       fill = "Job Title") +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "Arial"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 8))  # Adjust the size of the legend text



# Analysis 4.7 - Are there any job titles that show significant growth or decline in popularity over time (2010 - 2011)?

# Group data by job title and year, calculate the count of each job title within each year
job_title_counts <- new_employee_attrition %>%
  group_by(job_title, status_year) %>%
  summarise(count = n()) %>%
  ungroup()

# Calculate the percentage change in count for each job title over time
job_title_change <- job_title_counts %>%
  group_by(job_title) %>%
  mutate(percentage_change = (count - lag(count)) / lag(count) * 100) %>%
  ungroup()

# Filter job titles with significant growth or decline (percentage change >= 50% or <= -50%)
significant_change <- job_title_change %>%
  filter(percentage_change >= 50 | percentage_change <= -50)

# Create a line chart to display the percentage change in job title counts over time
ggplot(significant_change, aes(x = status_year, y = percentage_change, color = job_title)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(title = "Job Titles with Significant Growth or Decline in Popularity",
       x = "Year",
       y = "Percentage Change") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_blank())


# Analysis 4.8 - What is the average length of service of employees in each job title?

# Calculate the average length of service for each job title
avg_service_length <- new_employee_attrition %>%
  group_by(job_title) %>%
  summarise(avg_length_of_service = mean(length_of_service)) %>%
  arrange(desc(avg_length_of_service))

# Create a simplified bar chart to compare the average length of service between job titles
ggplot(avg_service_length, aes(x = reorder(job_title, avg_length_of_service), y = avg_length_of_service)) +
  geom_bar(stat = "identity", fill = "#3498DB", width = 0.5) +
  labs(title = "Average Length of Service by Job Title",
       x = "Job Title",
       y = "Average Length of Service") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.y = element_blank(),
        legend.position = "none") +
  coord_cartesian(ylim = c(0, max(avg_service_length$avg_length_of_service) * 1.1)) +
  scale_y_continuous(labels = scales::comma)



## Question 5: Assessing the impacts of layoffs from employees in the company.

# Analysis 5.1 - Which departments have experienced the highest number of layoffs?

# Filter data to include only layoffs
dapt_layoff <- new_employee_attrition %>%
  filter(termination_reason == "Layoff") %>%
  group_by(department_name) %>%
  summarise(dapt_layoff_count = n())

# Create bar plot to visualize the number of layoffs by department
ggplot(dapt_layoff, aes(x = department_name, y = dapt_layoff_count, fill = department_name)) +
  geom_bar(stat = "identity") +
  labs(x = "Department", y = "Number of Employees") +
  ggtitle("Departments with the Highest Number of Layoffs") +
  theme_minimal() +
  coord_flip()


# Analysis 5.2 - What are the top 5 job titles that have been most affected by layoffs?

# Filter data to include only layoffs
job_lay_top <- new_employee_attrition %>%
  filter(termination_reason == "Layoff") %>%
  group_by(job_title) %>%
  summarise(lay_count = n())

# Select the top 5 job titles with the highest number of layoffs
top_dapt <- 6
top_dapt_lay <- job_lay_top %>%
  top_n(top_dapt, lay_count) %>%
  arrange(desc(lay_count))

# Create a 3D pie chart to display the distribution of layoffs across job titles
labels <- top_dapt_lay$job_title
sizes <- top_dapt_lay$lay_count
colors <- rainbow(length(labels))
pie3D(sizes, labels = paste(labels, sizes, sep = ": "), main = "Top Job Titles Most Affected by Layoffs",
      col = colors, explode = 0.1, radius = 1.1, height = 0.2, theta = 0.6,
      labelcex = 0.8, labelcol = "black", labelfont = "Helvetica", labelbgcolor = "lightgray")



# Analysis 5.3 - How has the number of layoffs varied across different departments over time?

# Filter data to include only layoffs
lay_time <- new_employee_attrition %>%
  filter(termination_reason == "Layoff") %>%
  group_by(department_name, status_year) %>%
  summarise(dapt_count = n(), .groups = "drop")

# Create a 3D scatter plot to visualize the relationship between department, layoff year, and number of layoffs
plot_ly(lay_time, x = ~department_name, y = ~status_year, z = ~dapt_count,
        type = "scatter3d", mode = "markers") %>%
  add_markers(marker = list(size = 5, opacity = 0.8)) %>%
  layout(scene = list(xaxis = list(title = "Department"),
                      yaxis = list(title = "Year"),
                      zaxis = list(title = "Number of Layoffs"),
                      title = list("Variation in Layoffs Across Departments and Years"),
                      camera = list(eye = list(x = 1.7, y = 1.7, z = 0.5))),
         title = "The Relationship between Department, Layoff's Year, and Total Layoff Employee")


# Analysis 5.4 - How does the average length of service for employees impacted by layoffs vary across different departments?

# Filter data to include only layoffs
avg_service <- new_employee_attrition %>%
  filter(termination_reason == "Layoff") %>%
  group_by(department_name) %>%
  summarise(avg_length_of_service = mean(length_of_service, na.rm = TRUE), .groups = "drop")

# Create bar plot to visualize the average length of service for employees impacted by layoffs
ggplot(avg_service, aes(x = department_name, y = avg_length_of_service, fill = department_name)) +
  geom_bar(stat = "identity") +
  labs(x = "Department", y = "Average Length of Service", title = "Average Length of Service for Employees Impacted by Layoffs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_hue(l = 25) +
  theme_minimal()


# Analysis 5.5 - Which cities have experienced the highest number of layoffs?

# Filter data to include only layoffs
city_layoff <- new_employee_attrition %>%
  filter(termination_reason == "Layoff") %>%
  group_by(city) %>%
  summarise(city_lay_count = n())

# Create bar plot to visualize the number of layoffs by city
ggplot(city_layoff, aes(x = city, y = city_lay_count, fill = city)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "City", y = "Number of Layoffs", title = "Cities with the Highest Number of Layoffs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# Analysis 5.6 - Which business units have been most impacted by layoffs?

# Filter data to include only layoffs
business_layoff <- new_employee_attrition %>%
  filter(termination_reason == "Layoff" & !is.na(business_unit) & business_unit != "") %>%
  group_by(business_unit) %>%
  summarise(unit_count = n())

# Order the business units by the number of layoffs in descending order
business_layoff <- business_layoff %>%
  arrange(desc(unit_count))

# Create a bubble chart to visualize the distribution of layoffs by business unit
ggplot(business_layoff, aes(x = business_unit, y = unit_count, size = unit_count)) +
  geom_point(color = "blue", alpha = 0.8) +
  labs(x = "Business Unit", y = "Number of Layoffs", title = "Distribution of Layoffs by Business Unit (Bubble Chart)") +
  theme_minimal() +
  scale_size_area(max_size = 10)



## Question 6: Exploring the Relationship Between Cities and Employees.

# Analysis 6.1 - What is the distribution of employees across the top 5 cities based on the top 5 job titles?

# Filter the relevant columns and group by job titles and cities
filtered_data <- new_employee_attrition %>%
  select(job_title, city) %>%
  group_by(job_title, city)

# Summarize the count of job titles in each city
summarized_data <- filtered_data %>%
  summarize(count = n(), .groups = "drop")

# Arrange the data in descending order based on the count of employees
summarized_data <- summarized_data %>%
  arrange(desc(count))

# Select the top 5 cities with the highest employee counts
top_5_cities <- summarized_data %>%
  top_n(5, count)

# Create the histogram chart with facet_wrap using ggplot2
ggplot(top_5_cities, aes(x = job_title, y = count, fill = job_title)) +
  geom_bar(stat = "identity") +
  labs(x = "Job Title", y = "Count", title = "Distribution of Employees Across Top 5 Cities Based on Job Titles") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank()) +
  facet_wrap(~ city, ncol = 1)


# Analysis 6.2 - What is the age distribution (25~45) of employees across the top 4 cities?

# Filter the relevant columns for age and city
age_data <- new_employee_attrition %>%
  select(age, city)

# Select the top 4 cities with the highest employee counts
top_4_cities <- age_data %>%
  group_by(city) %>%
  summarize(count = n()) %>%
  top_n(4, count) %>%
  select(city)

# Filter the data for the top 4 cities and an appropriate age range (e.g., 25 to 45)
filtered_age_data <- age_data %>%
  filter(city %in% top_4_cities$city, age >= 25, age <= 45)

# Set the custom color palette
custom_palette <- c("#E13A3A", "#DF8D22", "#F1C40F", "#27AE60")

# Create a box plot with the custom color palette
ggplot(filtered_age_data, aes(x = city, y = age, fill = city)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = custom_palette) +
  labs(x = "City", y = "Age", 
       title = "Age Distribution of Employees (25-45 years old) Across Top 4 Cities") +
  theme_minimal()


# Analysis 6.3 - How does the gender representation vary across in Vancouver?

# Filter the relevant columns for gender and city
gender_data <- new_employee_attrition %>%
  select(gender, city)

# Filter the data for employees in Vancouver
vancouver_data <- gender_data %>%
  filter(city == "Vancouver")

# Count the number of employees for each gender in Vancouver
gender_counts <- vancouver_data %>%
  group_by(gender) %>%
  summarize(count = n(), .groups = "drop")

# Determine the y-axis limits based on the minimum and maximum count values
y_min <- min(gender_counts$count)
y_max <- max(gender_counts$count)

# Create a point chart to visualize the gender representation in Vancouver
ggplot(gender_counts, aes(x = gender, y = count)) +
  geom_point(size = 3) +
  labs(x = "Gender", y = "Count", title = "Gender Representation in Vancouver") +
  theme_minimal() +
  coord_cartesian(ylim = c(y_min, y_max))



###############################################################################################################################












