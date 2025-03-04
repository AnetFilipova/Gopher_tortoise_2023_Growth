##### Exploring the effect of cold dormancy on growth trajectories in head-started Gopher tortoises #####


## Load necessary libraries ##
library(lme4) ## Will be used for mixed effect models with random and fixed effects 
library(emmeans) ## Will be used for post-hoc comparisons
library(ggplot2) ## Will be used for making graphs/data visualization 
library(dplyr) ## Will be used for specifying logical oprators
library(ggpubr) ## Will be used for combining individual plots into one plot
library(tidyr) ## Will be used for combining all individual plots into one plot

#Clear memory
rm(list=ls(all = TRUE))

#### Data Preparation ####

## Reading the .csv file and exploring the structure of the data ##
datum <- read.csv("Growth_data_Working.csv")
head(datum)
str(datum)

## Filter out individuals where Tortoise_ID is equal to "Not_Viable" as well as specific IDs that need to be removed from the analyses ##
## Removing those individuals usin g the 'subset' function ##
## The logical operator %in% checks if elements of Tortoise_ID are present in the given vector c("Not_Viable", "GT2023_N05.03", "GT2023_N06.01", "GT2023_N05.06", "GT2023_N15.04") ##
## The operator ! returns the values from the column 'Tortoise_ID' that are NOT equal to any of the elements in the specified vector ##
datum <- subset(datum, !(Tortoise_ID %in% c("Not_Viable", "GT2023_N05.03", "GT2023_N06.01", "GT2023_N05.06", "GT2023_N15.04")))

# Confirm the filtered dataset
head(datum)

############### Data Analysis ##################

## Question 1: What is the growth rate of animals before cold dormancy treatment?
## Mixed effect model with treatment as a fixed effect and Nest_ID and Tank as a random effects ##
## If individuals from the same nest are more similar to each other than to individuals from different nests, treating Nest_ID as a random effect accounts for this correlation, avoiding pseudoreplication ##
## Statistical reason: If animals within the same tank grow similarly due to shared conditions, treating Tank as a random effect accounts for environmental clustering, preventing false inflation of significance ##

model_before <- lmer(Growth_rate_Before ~ Treatment +  (1 | Nest_ID ) + (1 | Tank), data = datum)

## Summarize the results
summary(model_before)

## Post-hoc tests
emmeans(model_before, pairwise ~ Treatment)

#### Checking for outliers with z-scores > 3 meaning more than 3 standard deviations away from the mean, which is considered a stronger outlier ####
# Calculate the z-scores for the Growth_rate_Before variable
datum <- datum %>%
  mutate(z_score = (Growth_rate_Before - mean(Growth_rate_Before, na.rm = TRUE)) / sd(Growth_rate_Before, na.rm = TRUE))

# Filter out outliers based on Tortoise_ID
outliers_before_tortoise <- datum %>%
  filter(abs(z_score) > 3) %>%
  select(Tortoise_ID, Growth_rate_Before, z_score)

# Print the outliers corresponding to individual Tortoise_ID
print(outliers_before_tortoise)

#### Plotting the data ####

## Definining an object for a colour-blind friendly pallette ##
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

before <- ggplot(datum, aes(x = Treatment, y = Growth_rate_Before, color = Treatment)) +  # Define aesthetics: x-axis as Treatment, y-axis as Growth_rate_Before, and color by Treatment
  geom_boxplot(position = position_dodge(0.85)) +  # Add boxplots with dodged positions to avoid overlap
  geom_jitter(width = 0.10, alpha = 0.5, size = 2) + # Add jittered points to show individual data points with some transparency defined by alpha = 0.5
  ylab("Growth Rate (g/day)") +  # Label the y-axis
  xlab("Treatment") +  # Label the x-axis
  scale_color_manual(values = c(cbbPalette[[6]], cbbPalette[[7]]), name = "", labels = c("", "")) + # Manually set colors and labels for the Treatment variable
  theme_classic() +  
  theme(strip.background = element_blank(), legend.position = "none", # Removing the default grey facet background as well as the legend by specifying 'none'
        axis.title = element_text(size = 18),  # Increase axis labels size
        axis.text = element_text(size = 14)    # Increase tick labels (treatment labels) size
  )  
before

# Save file as PNG for final figure production
ggsave(before, file="Growth_rate_Before.png", width=9, height=7, dpi=600)

####### Question 2: What is the effect of treatment on growth rate during dormancy? ########
# Mixed effect model with treatment as a fixed effect and Nest_ID as a random effect
# Since tanks may not introduce additional variability in a period of minimal or no growth, we exclude Tank as a random effect

model_during <- lmer(Growth_rate_During ~ Treatment + (1 | Nest_ID) , data = datum)

# Summarize the results
summary(model_during)

# Post-hoc tests
emmeans(model_during, pairwise ~ Treatment)


#### Checking for outliers with z-scores > 3 meaning more than 3 standard deviations away from the mean, which is considered a stronger outlier ####
# Calculate the z-scores for the Growth_rate_During variable
datum <- datum %>%
  mutate(z_score = (Growth_rate_During - mean(Growth_rate_During, na.rm = TRUE)) / sd(Growth_rate_During, na.rm = TRUE))

# Filter out outliers based on Tortoise_ID
outliers_during_tortoise <- datum %>%
  filter(abs(z_score) > 3) %>%
  select(Tortoise_ID, Growth_rate_During, z_score)

# Print the outliers corresponding to individual Tortoise_ID
print(outliers_during_tortoise)


##### Plot the data #####
during <- ggplot(datum, aes(x = Treatment, y = Growth_rate_During, color = Treatment)) +  # Define aesthetics: x-axis as Treatment, y-axis as Growth_rate_During, and color by Treatment
  geom_boxplot(position = position_dodge(0.85)) +  # Add boxplots with dodged positions to avoid overlap
  geom_jitter(width = 0.15, height = 0, alpha = 0.5, size = 2) + # Add jittered points to show individual data points with some transparency defined by alpha = 0.5
  ylab("Growth Rate (g/day)") +  # Label the y-axis
  xlab("Treatment") +  # Label the x-axis
  scale_color_manual(values = c(cbbPalette[[6]], cbbPalette[[7]]), name = "", labels = c("", "")) + # Manually set colors and labels for the Treatment variable
  theme_classic() +  
  theme(strip.background = element_blank(), legend.position = "none", # Removing the default grey facet background as well as the legend by specifying 'none'
        axis.title = element_text(size = 18),  # Increase axis labels size
        axis.text = element_text(size = 14)    # Increase tick labels (treatment labels) size
  )  
during

# Save file as PNG for final figure production
ggsave(during, file="Growth_rate_During.png", width=9, height=7, dpi=600)

####### Question 3: What is the effect of treatment on growth rate 3 weeks post-cold dormancy? #######
model_3Weeks <- lmer(Growth_rate_3_Weeks_Post ~ Treatment + (1 | Nest_ID) + (1 | Tank) , data = datum)

# Summarize the results
summary(model_3Weeks)

# Post-hoc tests
emmeans(model_3Weeks, pairwise ~ Treatment)

#### Checking for outliers with z-scores > 3 meaning more than 3 standard deviations away from the mean, which is considered a stronger outlier ####
# Calculate the z-scores for the Growth_rate_3_Weeks_Post  variable
datum <- datum %>%
  mutate(z_score = (Growth_rate_3_Weeks_Post - mean(Growth_rate_3_Weeks_Post, na.rm = TRUE)) / sd(Growth_rate_3_Weeks_Post, na.rm = TRUE))

# Filter out outliers based on Tortoise_ID
outliers_3_Weeks_tortoise <- datum %>%
  filter(abs(z_score) > 3) %>%
  select(Tortoise_ID, Growth_rate_3_Weeks_Post, z_score)

# Print the outliers corresponding to individual Tortoise_ID
print(outliers_3_Weeks_tortoise) ## Returns GT2023_N05.01 and GT2023_N17.03 as outliers

# Remove the outliers based on Tortoise_ID
datum_clean <- datum %>%
  filter(!Tortoise_ID %in% c("GT2023_N05.01", "GT2023_N17.03"))

# Re-run the mixed model without the outliers
model_3Weeks_clean <- lmer(Growth_rate_3_Weeks_Post ~ Treatment + (1 | Nest_ID) + (1 | Tank), data = datum_clean)

# Summarize the new model
summary(model_3Weeks_clean)

# Post-hoc tests
emmeans(model_3Weeks_clean, pairwise ~ Treatment)


###### Plot the data ######

three_weeks <- ggplot(datum, aes(x = Treatment, y = Growth_rate_3_Weeks_Post, color = Treatment)) +  # Define aesthetics: x-axis as Treatment, y-axis as Growth_rate_3_Weeks_Post, and color by Treatment
  geom_boxplot(position = position_dodge(0.85)) +  # Add boxplots with dodged positions to avoid overlap
  geom_jitter(width = 0.15, height = 0, alpha = 0.5, size = 2) + # Add jittered points to show individual data points with some transparency defined by alpha = 0.5
  ylab("Growth Rate (g/day)") +  # Label the y-axis
  xlab("Treatment") +  # Label the x-axis
  scale_color_manual(values = c(cbbPalette[[6]], cbbPalette[[7]]), name = "", labels = c("", "")) + # Manually set colors and labels for the Treatment variable
  theme_classic() +  
  theme(strip.background = element_blank(), legend.position = "none", # Removing the default grey facet background as well as the legend by specifying 'none'
        axis.title = element_text(size = 18),  # Increase axis labels size
        axis.text = element_text(size = 14)    # Increase tick labels (treatment labels) size
  )  
three_weeks

# Save file as PNG for final figure production
ggsave(three_weeks, file="Growth_rate_3Weeks.png", width=9, height=7, dpi=600)

##### Plot the data WITHOUT the two outliers #######
# Filter out the outliers from the dataset
datum_no_outliers <- datum %>%
  filter(!Tortoise_ID %in% c("GT2023_N05.01", "GT2023_N17.03"))

# Create the plot using the filtered dataset
three_weeks_no_outliers <- ggplot(datum_no_outliers, aes(x = Treatment, y = Growth_rate_3_Weeks_Post, color = Treatment)) +  
  geom_boxplot(position = position_dodge(0.85)) +  
  geom_jitter(width = 0.15, height = 0, alpha = 0.5, size = 2) + 
  ylab("Growth Rate (g/day)") +  
  xlab("Treatment") +  
  scale_color_manual(values = c(cbbPalette[[6]], cbbPalette[[7]]), name = "", labels = c("", "")) + 
  theme_classic() +  
  theme(strip.background = element_blank(), legend.position = "none", 
        axis.title = element_text(size = 16),  
        axis.text = element_text(size = 14)
        
  )
three_weeks_no_outliers


######## Question 4: What is the effect of treatment on growth rate 3 months post-cold dormancy? ########
model_3Months <- lmer(Growth_rate_3_Months_Post ~ Treatment + (1 | Nest_ID) + (1| Tank), data = datum)

## Checking for singularity because it looks like there is not enough variation among tanks ##
VarCorr(model_3Months)

## Returns The following output so removing tank from the model since Tank is not contributing to explaining the variation in growth rate ##
##Groups   Name        Std.Dev.
##Nest_ID  (Intercept) 0.12531 
##Tank     (Intercept) 0.00000 
##Residual             0.28755 

model_3Months <- lmer(Growth_rate_3_Months_Post ~ Treatment + (1 | Nest_ID), data = datum)

# Summarize the results
summary(model_3Months)

# Post-hoc tests
emmeans(model_3Months, pairwise ~ Treatment)

#### Checking for outliers with z-scores > 3 meaning more than 3 standard deviations away from the mean, which is considered a stronger outlier ####
# Calculate the z-scores for the Growth_rate_3_Months_Post  variable
datum <- datum %>%
  mutate(z_score = (Growth_rate_3_Months_Post - mean(Growth_rate_3_Months_Post, na.rm = TRUE)) / sd(Growth_rate_3_Months_Post, na.rm = TRUE))

# Filter out outliers based on Tortoise_ID
outliers_3_Months_tortoise <- datum %>%
  filter(abs(z_score) > 3) %>%
  select(Tortoise_ID, Growth_rate_3_Months_Post, z_score)

# Print the outliers corresponding to individual Tortoise_ID
print(outliers_3_Months_tortoise)

###### Plot the data ########

three_months <- ggplot(datum, aes(x = Treatment, y = Growth_rate_3_Months_Post, color = Treatment)) +  # Define aesthetics: x-axis as Treatment, y-axis as Growth_rate_3_Weeks_Post, and color by Treatment
  geom_boxplot(position = position_dodge(0.85)) +  # Add boxplots with dodged positions to avoid overlap
  geom_jitter(width = 0.15, height = 0, alpha = 0.5, size = 2) + # Add jittered points to show individual data points with some transparency defined by alpha = 0.5
  ylab("Growth Rate (g/day)") +  # Label the y-axis
  xlab("Treatment") +  # Label the x-axis
  scale_color_manual(values = c(cbbPalette[[6]], cbbPalette[[7]]), name = "", labels = c("", "")) + # Manually set colors and labels for the Treatment variable
  theme_classic() +  
  theme(strip.background = element_blank(), legend.position = "none", # Removing the default grey facet background as well as the legend by specifying 'none'
        axis.title = element_text(size = 18),  # Increase axis labels size
        axis.text = element_text(size = 14)    # Increase tick labels (treatment labels) size
  )  
three_months

# Save file as PNG for final figure production
ggsave(three_months, file="Growth_rate_3Months.png", width=9, height=7, dpi=600)

######### Combine the results from all three models into one graph ##########

# Reshape data to long format
datum_long <- datum %>%
  pivot_longer(
    cols = c(Growth_rate_Before, Growth_rate_During, Growth_rate_3_Weeks_Post, Growth_rate_3_Months_Post),
    names_to = "Timepoint",
    values_to = "Growth_rate"
  )

# Adjust the Timepoint column for better labels
datum_long$Timepoint <- factor(
  datum_long$Timepoint,
  levels = c("Growth_rate_Before", "Growth_rate_During", "Growth_rate_3_Weeks_Post", "Growth_rate_3_Months_Post"),
  labels = c("Before", "During", "3 Weeks Post", "3 Months Post")
)


### Plot the combined data ###
combined <- ggplot(datum_long, aes(x = Timepoint, y = Growth_rate, color = Treatment)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, position = position_dodge(0.8), 
               fill = "white", alpha = 0.5, linewidth = 0.5) +  # Boxplot with dodging
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
              size = 2, alpha = 0.4) +  # Jitter points with dodging
  stat_summary(fun = mean, geom = "point", size = 2, aes(group = Treatment), 
               position = position_dodge(width = 0.8)) +  # Mean points, properly dodged
  stat_summary(fun = mean, geom = "line", aes(group = Treatment), linewidth = 0.6, 
               position = position_dodge(width = 0.8)) +  # Mean lines, properly dodged
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "right",
    legend.title.align = 0.5
  ) +
  scale_color_manual(
    values = cbbPalette[c(6, 7)], 
    name = "Treatment", 
    labels = c("Constant heat", "Cold dormancy")
  ) +
  labs(
    x = "Timepoint",  
    y = "Growth rate (g/day)"
  )

# Display the plot
print(combined)

# Save file as PNG for final figure production
ggsave(combined, file = "Combined_Growth_rate.png", width = 12, height = 6, dpi = 600)









## Question 5: How does growth rate change before and after cold dormancy ONLY for the cold dormancy treatment
cold_dormancy_data <- subset(datum, Treatment == "Cold_dormancy")

#Reshape the data into a long format to get a column for Timepoint and a column for Growth_rate

datum_long <- read.csv("Growth_data_Working.csv")
datum_long <- reshape(datum_long, 
                      varying = c("Growth_rate_Before", "Growth_rate_3_Months_Post"), 
                      v.names = "Growth_rate", 
                      timevar = "Timepoint", 
                      times = c("Growth_rate_Before", "Growth_rate_3_Months_Post"), 
                      direction = "long")

# Reorder the 'Timepoint' factor to ensure correct order
datum_long$Timepoint <- factor(datum_long$Timepoint, 
                               levels = c("Growth_rate_Before", "Growth_rate_3_Months_Post"))

# Filter the data for the cold dormancy treatment
cold_dormancy_only <- datum_long[datum_long$Treatment == "Cold_dormancy", ]

model_cold_dormancy <- lmer(Growth_rate ~ Timepoint + (1 | Nest_ID), data = cold_dormancy_only)


# Summarize the model results
summary(model_cold_dormancy)

emmeans(model_cold_dormancy, pairwise ~ Timepoint)

# Create the boxplot for Growth_rate before and after cold dormancy
plot_growth <- ggplot(cold_dormancy_only, aes(x = Timepoint, y = Growth_rate, fill = Timepoint)) + 
  geom_boxplot() + 
  labs(x = "Timepoint", y = "Growth Rate (g/day)") + 
  scale_x_discrete(labels = c("Growth_rate_Before" = "Before Cold Dormancy", 
                              "Growth_rate_3_Months_Post" = "3 Months Post Cold Dormancy")) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),  # Increase size of axis titles
    axis.text = element_text(size = 14)    # Increase size of axis tick labels
  )

# Save the plot
ggsave(plot_growth, file = "Growth_rate_Cold_Dormancy_Boxplot.png", width = 12, height = 6, dpi = 600)


