
# Install packages and load libraries
install.packages("tidyverse")
install.packages("esquisse")
install.packages("cluster")
install.packages("plotly")
install.packages("viridisLite")
install.packages("class")
library(ggplot2)
library(esquisse)
library(tidyverse)
library(dplyr)
library(cluster)
library(plotly)
library(viridis)
library(class)
library(corrplot)

# Load Data into R
data <- read.csv(file.choose())
View(data)
glimpse(data)
summary(data)

# Check for missing values
sum(is.na(data))
mis_data <- colMeans(is.na(data))
mis_data

# Data imputation (only columns with the non-conflicting numeric values were chosen)
data["PM3_FREQ_POLITICS"][data["PM3_FREQ_POLITICS"] == -99] <- 5    
data["PM2_FREQ_COVID"][data["PM2_FREQ_COVID"] == -99] <- 5

data[data == -99] <- 3

# Creating Age, BMI and Income categories
#-------- AGE
age_brackets <- c(1,20,40,60,80,max(data$AGE))
age_labels <- c("Under 20", "21-40", "41-60","61-80", "Above 80")
Age_cat <- cut(data$AGE, breaks = age_brackets, labels = age_labels)

data <- cbind(data, Age_cat)

age_plot <- data %>% 
              group_by(Age_cat) %>% 
              ggplot() + geom_bar(mapping = aes(x = Age_cat, fill = Age_cat)) + 
              labs( title = "Data Distribution for participations based on Age")

# Visualization for Age wise Data Distribution                                    
age_plot         

# ------BMI
bmi_func <- function(bmi){
  if(bmi <= 18.5){return("underweight")}
  else if(bmi > 18.5 & bmi < 24.9){return("Normal Range")}
  else if(bmi >= 25){return("overweight")}
  else{return("obese")}
}

bmi_cat <- sapply(data$BMI,bmi_func)
bmi_cat

data <- cbind(data, bmi_cat)

View(data)

# ------- Income

range(data$INCOME)
income_brackets <- c(1, 20000, 50000, 100000, 150000, 200000, max(data$INCOME))
income_labels <- c("Under 20k", "under 50k", " under 100k","Under 150k", "Under 200k", "Above 200k")
income_cat <- cut(data$INCOME, breaks = income_brackets, labels = income_labels)

data <- cbind(data, income_cat)

# Plot the BMI distribution for the data

library(ggplot2)

label1 <- c(`0` = "Unemployed", `1` = "Employed")
label2 <- c(`0` = "Female", `1` = "Male")

ggplot(data) +
 aes(y = bmi_cat, fill = EMPL) +
 geom_bar(position = position_dodge(0.9)) +
 scale_fill_gradient() + labs(title = "Data distribution of bmi categories as per gender and employment status") +
 theme_minimal() + theme(legend.position = "none") +
 facet_grid(rows = vars(EMPL), cols = vars(SEX), 
            labeller = labeller(.rows = label1, .cols = label2))


# Plot based on group based summaries for Users Only
user_hist <- data %>% 
  select(starts_with("C_")) %>%
  mutate(C_SCORE = rowSums(user_hist[, -1]))

psy_data <- data %>% 
                  filter(PSY_USE_YN == 1) %>% 
                  group_by(Age_cat, bmi_cat) %>% 
                  summarise(Psy_User = sum(PSY_USE_YN == 1),
                            PM_User = sum(PM_USE_YN == 1), 
                            Income = median(INCOME),
                            Comorbid = mean(C_TOTAL),
                            C_score = mean(`user_hist$C_SCORE`),
                            CCI = mean(CCI_SCORE), 
                            Gen_AD = mean(GAD7_SCORE), 
                            PHQ = mean(PHQ9_SCORE), 
                            Phy_Health = mean(PCS12),
                            Men_Health = mean(MCS12), .groups = "drop")
summary(psy_data)
View(psy_data)

top_users <- arrange(psy_data, desc(PM_User))

# lm model
lin_mod <- lm(PM_User ~ Men_Health, data = psy_data)
plot(psy_data$Men_Health, psy_data$PM_User)
abline(lin_mod)

library(ggplot2)

ggplot(data = psy_data, aes(y = PM_User, x = Gen_AD, col = bmi_cat, size = C_score)) + geom_point() + 
  stat_smooth(method = "lm", col = "red") + 
  labs(title = "Linear Model for General Anxiety Disorder Scale and 
                               mushroom users of age cat 21-40 and others ") +
  facet_grid(cols = vars((Age_cat=="21-40")))

# Proportion of Participants with any history of medical condition.
user_no_hist <- colSums(user_hist==0)/nrow(user_hist) * 100
user_yes_hist <- colSums(user_hist>=1)/nrow(user_hist) * 100

qs <- rbind(user_no_hist,user_yes_hist)
data <- cbind(data, user_hist$C_SCORE)

psy_plot <- ggplot(psy_data) + aes( x = Phy_Health,
                                    y = Men_Health,
                                    colour = bmi_cat,
                                    size = PM_User, 
                                    alpha = 0.7) +
            geom_point(shape = "circle") + scale_color_viridis_d(option = "plasma", direction = 1) + 
            scale_size(range = c(1, 10)) + 
            labs(title = "General health Overview of participating Psychedelic Users") + theme_bw() +
            theme(plot.title = element_text(size = 15L, face = "bold.italic", hjust = 0.5))

psy_plot

# Subsetting Data and checking for correlation between two age groups

mat_2040 <- data %>% 
              select(PSY_USE_YN, PM_USE_YN, C_TOTAL, CCI_SCORE, GAD7_SCORE, 
                     PHQ9_SCORE, PCS12, MCS12, PSY1_POSITIVE_USE,
                     PSY2_GEN_HEALTH, EMPL) %>% 
              filter(Age_cat == "21-40")

sam_2040 = as.matrix(mat_2040[sample(nrow(mat_2040), 300, replace = FALSE), ])           

mat_4060 <- data %>% 
              select(PSY_USE_YN, PM_USE_YN, C_TOTAL, CCI_SCORE, GAD7_SCORE, 
                     PHQ9_SCORE, PCS12, MCS12, PSY1_POSITIVE_USE,
                     PSY2_GEN_HEALTH, EMPL) %>% 
              filter(Age_cat == "41-60")

sam_4060 <- as.matrix(mat_4060[sample(nrow(mat_4060), 300, replace = FALSE), ])

cor_mat <- cor(sam_2040, sam_4060, method = "pearson")

heatmap(cor_mat, Rowv = NA, scale = "column",symm = T)

corrplot(cor_mat, method = "color", order = "alphabet", col = COL2('BrBG', 30))

# Creating a distance matrix of the sample and plotting it for similarities in the participants.

dist_mat<- as.matrix(dist(sam_2040, method = "euclidean", diag = T, upper = T))
heatmap(dist_mat, scale = "column")
image(dist_mat, col = heat.colors(12))


dist_mat2 <- as.matrix(dist(sam_4060, method = "euclidean", diag = T, upper = T))
heatmap(dist_mat2, scale = "column")

# --------------- 


esquisser(lin_data)
# KNN algorithm to predict if a user is likely to be a user or not

normalize_data <- function(i) {
  return ((i - min(i)) / (max(i) - min(i)))
}

norm_sam2040 <- normalize_data(sam_2040)
sam_train <- sam_2040[1:210, ]
sam_test <- sam_2040[211:300, ]

train_label <- sam_2040[1:210,3]
test_label <- sam_2040[211:300,3]

test_pred <- knn(train = sam_train, test = sam_test, cl = train_label ,k = 18)
install.packages("gmodels")
library(gmodels)
CrossTable(x= test_label, y = test_pred, prop.chisq = FALSE)


# ---------------------------- #

# Check for Binary columns
is_binary <- apply(data,2,function(x) {all(x %in% 0:1)})
is_binary 

only_bin <- Filter(function(x) all(x %in% c(0, 1)), data)
View(only_bin)

jaccard_similarity <- function(A, B) {
  intersection = length(intersect(A, B))
  union = length(A) + length(B) - intersection
  return (intersection/union)
}


no2yes <- function(vec){
  percent <- colSums(vec==0)/nrow(vec) * 100
  print(paste(percent,"%"))
}

mat_2040 <- data %>% 
                group_by(Age_cat, bmi_cat) %>% 
                summarise(Psy_User = length(PSY_USE_YN == 1), 
                          Comorbid = mean(C_TOTAL), 
                          CCI = mean(CCI_SCORE), 
                          Gen_AD = mean(GAD7_SCORE), 
                          PHQ = mean(PHQ9_SCORE), 
                          Phy_Health = mean(PCS12),
                          Men_Health = mean(MCS12), .groups = "drop")
                

mat_2040

ggplot(mat_2040) +
  aes(
    x = Phy_Health,
    y = Men_Health,
    colour = bmi_cat,
    size = Psy_User
  ) +
  geom_point(shape = "circle") +
  scale_color_hue(direction = 1) +
  theme_minimal()
             
mat_4060 <- only_bin %>% 
              filter(Age_cat == "41-60")

mat_6080 <- data %>% 
              group_by(Age_cat) %>% 
              select(Age_cat == "61-80")
              
sum(which(psy_data$PM_User == 1))
sum(which(psy_data$Psy_User == 1))
sum(data$PSY_USE_YN == 0)

ggplot(data) +
  aes(x = BMI) +
  geom_density(adjust = 1L, fill = "#0C4C8A") +
  theme_minimal() 
