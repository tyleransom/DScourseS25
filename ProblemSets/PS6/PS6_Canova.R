install.packages("ggthemes")
install.packages("hexbin")

library(ggplot2)
library(ggthemes)
library(hexbin)
library(tidyverse)
library(modelsummary)
library(stargazer)
library(readxl)
library(magrittr)

#load data from File "NASO Cleanse (signaling).xlsx" using tab M1 (Controls&DV) (3), using the first row as variable names
{
  library(readxl)
  # error, the file path doesn't exist Figure out the correct path
  NASO_df <- read_excel("C:/Users/cano0017/Desktop/OU/Working Papers/2 -Commitment Conundrum (Persistent Hybrid Ent)/Data/Processed Data/NASO Cleanse (signaling).xlsx", sheet = "M1 (Controls&DV) (3)", col_names = TRUE)
  view(NASO_df)
}

#Work around for the error
{
  library(readxl)
  NASO_df <- NASO_Cleanse_signalling_
  View(NASO_df)
}

#Run the OLS Calculation with interactions
{
  #Z scored calculations
  {
    
    #Creating Z-score dataframe
    {
      
      # Identify numeric columns (excluding the response variable 'Future')
      numeric_cols <- setdiff(names(NASO_df)[sapply(NASO_df, is.numeric)], "Future")
      
      # Create a new dataframe with z-scores for numeric predictors
      NASO_df_z <- NASO_df %>%
        mutate(across(all_of(numeric_cols), scale)) %>%
        mutate(across(all_of(numeric_cols), as.vector))  # Convert back to vector from matrix
      
      # Verify the transformation
      summary(NASO_df_z[, numeric_cols])
      
    }
    
    #Model1 Controls z-score
    {
      Mod1z <- lm(Future ~ W_Income + Age + Kids + Educ + Current + Gender, data = NASO_df_z)
      summary(Mod1z)
      modelsummary(Mod1z, stars = T)
      stargazer(Mod1z, type = "text")
    }
    
    #Model2 Controls and IV for H1 test z-score
    {
      Mod2z <- lm(Future ~ Yrs_reffing + W_Income + Age + Kids + Educ + Current + Gender, data = NASO_df_z)
      summary(Mod2z)
      modelsummary(Mod2z, stars = T)
      stargazer(Mod2z, type="text")
    }
    
    #Model3 Controls and IV (Self rank continuous) for H2 z-score
    {
      Mod3z <- lm(Future ~ Self_Rank + W_Income + Age + Kids + Educ + Current + Gender, data = NASO_df_z)
      summary(Mod3z)
      modelsummary(Mod3z, stars = T)
      stargazer(Mod3z, type = "text")
    }
    
    #Model4 Controls and IV (self Confidence continuous) for H2 (Robustness Check) z-score 
    {
      Mod4z <- lm(Future ~ Confidence_reversed + W_Income + Age + Kids + Educ + Current + Gender, data = NASO_df_z)
      summary(Mod4z)
      modelsummary(Mod4z, stars = T)
      stargazer(Mod4z, type = "text")
    }
    
    #Model5 Controls, IV, Mods (Self Rank continuous) z-score
    {
      Mod5z <- lm(Future ~ Self_Rank + Yrs_reffing + W_Income + Age + Kids + Educ + Current + Gender + Yrs_reffing:Self_Rank, data = NASO_df_z)
      modelsummary(Mod5z, stars = T)
      summary(Mod5z)
      stargazer(Mod5z, type = "text")
      #sapply(NASO_df$Self_Rank)
    }
    
    #Model6 Controls, IV, Mods (Self confidence continuous) z-score
    {
      Mod6z <- lm(Future ~ Confidence_reversed + Yrs_reffing + W_Income + Age + Kids + Educ + Current + Gender + Yrs_reffing:Confidence_reversed, data = NASO_df_z)
      modelsummary(Mod6z, stars = T)
      summary(Mod6z)
      stargazer(Mod6z, type = "text")
      #sapply(NASO_df$Self_Rank)
    }
    
    
    #All z models together.
    {
      modelsummary(list("Model 1 z"=Mod1z, "Model 2 z" = Mod2z, "Model 3 z" = Mod3z, "Model 4 z" = Mod4z, "Model 5 z" = Mod5z, "Model 6 z" = Mod6z), stars = T)
    }
    
    
  }
}

#interaction plot
# Create levels for the moderator (Self_Rank) at -1 SD, Mean, and +1 SD
# Since the data is already z-scored, we can use fixed values
NASO_df_z$Self_Rank_Level <- cut(NASO_df_z$Self_Rank, 
                                 breaks = c(-Inf, -0.5, 0.5, Inf),
                                 labels = c("-1 SD", "Mean", "+1 SD"))

# For the x-axis (Yrs_reffing), create a sequence of values for a smooth plot
yrs_seq <- seq(min(NASO_df_z$Yrs_reffing, na.rm = TRUE), 
               max(NASO_df_z$Yrs_reffing, na.rm = TRUE), 
               length.out = 100)

# Fit the full model with all control variables
full_model <- lm(Future ~ Self_Rank + Yrs_reffing + W_Income + Age + Kids + 
                   Educ + Current + Gender + Yrs_reffing:Self_Rank, 
                 data = NASO_df_z)

# Calculate the means of numeric control variables
numeric_controls <- c("W_Income", "Age", "Kids", "Educ", "Current")
control_means <- colMeans(NASO_df_z[, numeric_controls], na.rm = TRUE)

# For the categorical Gender variable, determine the most frequent category
gender_table <- table(NASO_df_z$Gender)
modal_gender <- names(gender_table)[which.max(gender_table)]

# Create a new data frame for predictions with all necessary variables
new_data <- expand.grid(
  Yrs_reffing = yrs_seq,
  Self_Rank_Level = c("-1 SD", "Mean", "+1 SD")
)

# Convert the Self_Rank_Level back to numeric values for prediction
new_data$Self_Rank <- ifelse(new_data$Self_Rank_Level == "-1 SD", -1,
                             ifelse(new_data$Self_Rank_Level == "Mean", 0, 1))

# Add the numeric control variables at their mean values
for (var in numeric_controls) {
  new_data[[var]] <- control_means[var]
}

# Add the modal gender category
new_data$Gender <- modal_gender

# Get predictions from the full model
new_data$predicted <- predict(full_model, newdata = new_data)

# Create the grayscale plot with ggplot2
library(ggplot2)

# Using grayscale with different line types for distinction
ggplot(new_data, aes(x = Yrs_reffing, y = predicted, 
                     linetype = Self_Rank_Level, 
                     group = Self_Rank_Level)) +
  geom_line(size = 1, color = "black") +
  labs(
    title = "Interaction of Years Refereeing and Self Rank on Future",
    subtitle = paste0("Controls at means, Gender = ", modal_gender),
    x = "Years Refereeing (z-score)",
    y = "Future (z-score)",
    linetype = "Self Rank"
  ) +
  theme_minimal() +
  scale_linetype_manual(values = c("dotted", "solid", "dashed")) +
  theme(legend.position = "top")

# For additional clarity, adding different point shapes
ggplot(new_data, aes(x = Yrs_reffing, y = predicted, 
                     linetype = Self_Rank_Level, 
                     shape = Self_Rank_Level,
                     group = Self_Rank_Level)) +
  geom_line(size = 1, color = "black") +
  geom_point(size = 3, color = "black", 
             data = subset(new_data, Yrs_reffing %in% seq(min(yrs_seq), max(yrs_seq), length.out = 5))) +
  labs(
    title = "Interaction of Years Refereeing and Self Rank on Future",
    subtitle = paste0("Controls at means, Gender = ", modal_gender),
    x = "Years Refereeing (z-score)",
    y = "Future (z-score)",
    linetype = "Self Rank",
    shape = "Self Rank"
  ) +
  theme_minimal() +
  scale_linetype_manual(values = c("dotted", "solid", "dashed")) +
  scale_shape_manual(values = c(1, 16, 2)) +
  theme(legend.position = "top")

# If you prefer using base R's interaction.plot:
# Create representative values for the plot
Self_Rank_values <- c(-1, 0, 1)  # -1 SD, Mean, +1 SD
Yrs_reffing_values <- seq(min(NASO_df_z$Yrs_reffing, na.rm = TRUE),
                          max(NASO_df_z$Yrs_reffing, na.rm = TRUE),
                          length.out = 5)

plot_data <- expand.grid(
  Self_Rank = Self_Rank_values,
  Yrs_reffing = Yrs_reffing_values
)

# Add numeric control variables at their means
for (var in numeric_controls) {
  plot_data[[var]] <- control_means[var]
}

# Add the modal gender
plot_data$Gender <- modal_gender

# Get predictions using the full model
plot_data$Future <- predict(full_model, newdata = plot_data)

# Convert Self_Rank to factor with proper labels
plot_data$Self_Rank_f <- factor(plot_data$Self_Rank, 
                                levels = c(-1, 0, 1),
                                labels = c("-1 SD", "Mean", "+1 SD"))

# Create the interaction plot
# Set up grayscale parameters
par(bg = "white", fg = "black")

interaction.plot(
  plot_data$Yrs_reffing,
  plot_data$Self_Rank_f,
  plot_data$Future,
  type = "b",
  col = "black",  # Single color for grayscale
  pch = c(1, 16, 2),  # Different point symbols for each level
  lty = c(2, 1, 3),   # Different line types (dotted, solid, dashed)
  xlab = "Years Refereeing (z-score)",
  ylab = "Future (z-score)",
  trace.label = "Self Rank",
  legend = TRUE,
  main = paste0("Interaction Effect (Controls at means, Gender = ", modal_gender, ")")
)
