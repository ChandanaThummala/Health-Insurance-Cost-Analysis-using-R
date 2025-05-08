getwd()

#Read the dataset

data_file <- read.csv("Health-Insurance-Dataset.csv", head = TRUE)
data_file
attach(data_file)
head(data_file)
tail(data_file)
str(data_file)
summary(data_file)

# Check for missing values
print(colSums(is.na(data_file)))

# Calculating variance and standard deviation

numeric_data <- data_file[sapply(data_file, is.numeric)]
variances <- sapply(numeric_data, var, na.rm = TRUE)
standard_deviations <- sapply(numeric_data, sd, na.rm = TRUE)
print(variances)
print(standard_deviations)

#count for the variable "Sex"
sex_count <- table(data_file$sex)
sex_count

#Region count
region_count <- table(data_file$region)
region_count

#Smoker count
smoker_count <- table(data_file$smoker)
smoker_count

install.packages('ggplot')
library(ggplot2)

install.packages("patchwork")
library(patchwork)

# Load necessary libraries
library(ggplot2)
library(patchwork)


# Histogram for Age,BMI

ggplot(data_file, aes(x = age)) + geom_histogram(binwidth = 1, fill = "lightsalmon2", color = "black") + labs(title = "Age Distribution") + theme(plot.title = element_text(hjust = 0.5))
ggplot(data_file, aes(x = bmi)) + geom_histogram(binwidth = 1, fill = "light blue", color = "black") + labs(title = "BMI Distribution") + theme(plot.title = element_text(hjust = 0.5))
ggplot(data_file, aes(x = charges)) + geom_histogram(binwidth = 1000, fill = "plum3", color = "black") + labs(title = "Charges Distribution") + theme(plot.title = element_text(hjust = 0.5))

#Bar graph for Sex, Children, smoker, region

ggplot(data_file, aes(x = sex, fill = sex)) +geom_bar() +scale_fill_manual(values = c("male" = "pink2", "female" = "thistle3")) +ggtitle("Sex Distribution") + theme(plot.title = element_text(hjust = 0.5))
ggplot(data_file, aes(x = children)) + geom_bar(fill = "aquamarine3") + labs(title = "Children Distribution") + theme(plot.title = element_text(hjust = 0.5))
ggplot(data_file, aes(x = smoker, fill = smoker)) +   geom_bar()+   scale_fill_manual(values = c("No" = "lightgoldenrod3", "Yes" = "lightcyan3")) +   labs(title = "Smoker Distribution") +   theme(plot.title = element_text(hjust = 0.5))
ggplot(data_file, aes(x = region, fill = region)) +geom_bar() +scale_fill_manual(values = c("southwest" = "mediumpurple", "southeast" = "lightsteelblue2", "northwest" = "lightyellow3", "northeast" = "lightsalmon2")) +labs(title = "Region Distribution") +theme(plot.title = element_text(hjust = 0.5))


numerical_data <- data_file[, c("age", "bmi", "children", "charges")]

# finding correlation matrix
cor_matrix <- cor(numerical_data)
cor_matrix

#correlation test
cor.test(age,bmi,method="spearman")
cor.test(age,bmi,method="pearson")
cor.test(age,children,method="spearman")
cor.test(age,charges,method="spearman")
cor.test(bmi,children,method="pearson")
cor.test(bmi,children,method="spearman")
cor.test(bmi,charges,method="pearson")
cor.test(bmi,charges,method="spearman")
cor.test(children, charges, method="spearman")

#Chi-square test
chisq.test(data_file$smoker, data_file$charges)
chisq.test(sex,smoker)
chisq.test(region, smoker)
chisq.test(smoker, sex)

# Test independence between smoker and sex
table_smoker_sex <- table(data_file$smoker, data_file$sex)
chisq.test(table_smoker_sex)


reg_model <- lm(charges ~ age+bmi+children+region+dummy_smoker+dummy_sex, data = data_file)
summary(reg_model)

#Visualising linear regression model
install.packages('car')
library(car)

#Creating dummy variables for categorical variables
data_file$smoker_dummy = ifelse(data_file$smoker == "yes", 1,0)
data_file$sex_dummy = ifelse(data_file$sex == "female", 1,0)

model = lm(charges ~ age+bmi+children+sex_dummy+region+smoker_dummy, data = data_file)
model
summary(model)
crPlots(model)

#creating dummy variables for region
region_dummies <- model.matrix(~ region, data = data_file)[, -1]
data_file <- cbind(data_file, region_dummies)

# Fit the linear regression model
model <- lm(charges ~ age + sexmale + bmi + children + smokeryes + regionnorthwest + regionsoutheast + regionsouthwest, 
            data = data_file)
crPlots(model)

#creating dummy var for sex and smoker

sex_dummy <- model.matrix(~ sex, data = data_file)[, -1]
smoker_dummy <- model.matrix(~ smoker, data = data_file)[, -1]

# Combine dummy variables with original data
data_file <- cbind(data_file, sex_dummy)
data_file <- cbind(data_file, smoker_dummy)
View(data_file)
crPlots(model,ylab = "Charges")


#Create CHARGE_split variable based on median charge

median_charge <- median(data_file$charges)
data_file$CHARGE_split <- ifelse(data_file$charges <= median_charge, 'X','Y')
View(data_file)

# KS test for age

ks.test(data_file$age[data_file$CHARGE_split == 0], "pnorm", mean = mean(data_file$age[data_file$CHARGE_split == 0]), sd = sd(data_file$age[data_file$CHARGE_split == 0]))
ks.test(data_file$age[data_file$CHARGE_split == 1], "pnorm", mean = mean(data_file$age[data_file$CHARGE_split == 1]), sd = sd(data_file$age[data_file$CHARGE_split == 1]))

# KS test for bmi

ks.test(data_file$bmi[data_file$CHARGE_split == 0], "pnorm", mean = mean(data_file$bmi[data_file$CHARGE_split == 0]), sd = sd(data_file$bmi[data_file$CHARGE_split == 0]))
ks.test(data_file$bmi[data_file$CHARGE_split == 1], "pnorm", mean = mean(data_file$bmi[data_file$CHARGE_split == 1]), sd = sd(data_file$bmi[data_file$CHARGE_split == 1]))

# KS test for children

ks.test(as.numeric(data_file$children[data_file$CHARGE_split == 0]), "pnorm", mean = mean(as.numeric(data_file$children[data_file$CHARGE_split == 0])), sd = sd(as.numeric(data_file$children[data_file$CHARGE_split == 0])))
ks.test(as.numeric(data_file$children[data_file$CHARGE_split == 1]), "pnorm", mean = mean(as.numeric(data_file$children[data_file$CHARGE_split == 1])), sd = sd(as.numeric(data_file$children[data_file$CHARGE_split == 1])))

# KS test for charges

ks.test(as.numeric(data_file$charges[data_file$CHARGE_split == 0]), "pnorm", mean = mean(as.numeric(data_file$charges[data_file$CHARGE_split == 0])), sd = sd(as.numeric(data_file$charges[data_file$CHARGE_split == 0])))
ks.test(as.numeric(data_file$charges[data_file$CHARGE_split == 1]), "pnorm", mean = mean(as.numeric(data_file$charges[data_file$CHARGE_split == 1])), sd = sd(as.numeric(data_file$charges[data_file$CHARGE_split == 1])))

# Mann-Whitney test for a non-normally distributed variable

wilcox.test(data_file$age[data_file$CHARGE_split == 0], data_file$age[data_file$CHARGE_split == 1])
t.test(data_file$bmi[data_file$CHARGE_split == 0], data_file$bmi[data_file$CHARGE_split == 1])
wilcox.test(data_file$children[data_file$CHARGE_split == 0], data_file$children[data_file$CHARGE_split == 1])
wilcox.test(data_file$charges[data_file$CHARGE_split == 0], data_file$charges[data_file$CHARGE_split == 1])

#Chi-square test for 'sex', 'region',

chisq.test(data_file$sex[data_file$CHARGE_split == 0], data_file$sex[data_file$CHARGE_split == 1])
chisq.test(data_file$region[data_file$CHARGE_split == 0], data_file$region[data_file$CHARGE_split == 1])

# Box plot for 'age' by CHARGE-split

ggplot(data_file, aes(x = CHARGE_split, y = age, fill = CHARGE_split)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age by CHARGE-split", x = "CHARGE-split", y = "Age")

#Box plot for 'bmi' by CHARGE-split

ggplot(data_file, aes(x = CHARGE_split, y = bmi, fill = CHARGE_split)) +
  geom_boxplot() +
  labs(title = "Box Plot of BMI by CHARGE-split", x = "CHARGE-split", y = "bmi")

#Box plot for 'Children' by CHARGE-split

ggplot(data_file, aes(x = CHARGE_split, y = children, fill = CHARGE_split)) + geom_boxplot() + labs(title = "Box Plot of Children by CHARGE-split", x = "CHARGE-split", y = "children")

#Box plot for 'Charges' by CHARGE-split

ggplot(data_file, aes(x = CHARGE_split, y = charges, fill = CHARGE_split)) + geom_boxplot() + labs(title = "Box Plot of Charges by CHARGE-split", x = "CHARGE-split", y = "charges")


# For ANOVA on Age
anova_age <- aov(age ~ region, data = data_file)
summary(anova_age)

# ANOVA for BMI across different regions
anova_bmi <- aov(bmi ~ region, data = data_file)
summary(anova_bmi)

# Kruskal-Wallis test
kruskal_age <- kruskal.test(age ~ region, data = data_file)
kruskal_bmi <- kruskal.test(bmi ~ region, data = data_file)
print(kruskal_age)
print(kruskal_bmi)

# Tukey's HSD for BMI
TukeyHSD(anova_bmi)

boxplot(bmi ~ region, data = data_file, main = "BMI Distribution by Region", xlab = "Region", ylab = "BMI", col = c("#999999", "#E69F00", "#56B4E9", "#009E73"), frame = FALSE)

boxplot(age ~ region, data = data_file, main = "Age Distribution by Region",xlab = "Region", ylab = "Age",
        col = c("#999999", "#E69F00", "#56B4E9", "#009E73"),frame = FALSE)