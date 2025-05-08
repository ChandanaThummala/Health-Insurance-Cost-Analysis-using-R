# Health-Insurance-Cost-Analysis-using-R

A deep statistical exploration of how age, BMI, smoking habits, and geography affect insurance costs in the US.

This project uses **R** to explore and analyze a real-world health insurance dataset of 1,338 individuals. It combines **descriptive analysis**, **hypothesis testing**, **regression modeling**, and **visual storytelling** to uncover key drivers of healthcare charges.

---

## Objective

- Understand the influence of **age, BMI, children, smoker status, sex, and region** on health insurance charges.
- Apply various **statistical tests** (t-tests, chi-square, KS test, ANOVA).
- Build a **multiple linear regression model**.
- Use **CHARGE-split categorization** to explore distributions and relationships.

---

## Dataset Description

| Feature    | Description |
|------------|-------------|
| `age`      | Age of the individual (18–64 years) |
| `sex`      | Male/Female |
| `bmi`      | Body Mass Index (15.96–53.13) |
| `children` | Number of dependents (0–5) |
| `smoker`   | Yes/No |
| `region`   | U.S. region (northeast, northwest, southeast, southwest) |
| `charges`  | Annual medical insurance charges in USD |

---

## Analysis Performed

### Descriptive Analysis
- Summary statistics (mean, median, standard deviation, kurtosis)
- Distribution plots: histograms & bar charts

### Correlation & Hypothesis Testing
- **Pearson** & **Spearman** correlation
- **Chi-square** tests (e.g., smoker vs. sex)
- **KS test** for normality
- **T-tests** & **Mann-Whitney U tests** for group comparison

### Regression Modeling
- Multiple linear regression with predictors:
  - Age, BMI, Children, Smoker, Region, Sex
- Dummy variable encoding for categorical features
- Model evaluation via p-values and R²

### CHARGE-split Analysis
- Charges split into two groups (low vs high)
- Compared groups using boxplots & statistical tests

---

## Key Insights

| Variable | Insight |
|----------|---------|
| **Smoker** | Strongest predictor of higher insurance costs |
| **Age**    | Positively correlated with charges |
| **BMI**    | Significant in group difference tests |
| **Region** | Southeast region had notably higher BMI |

---


## Technologies Used

- **Language:** R
- **Libraries:** `ggplot2`, `car`, `patchwork`, `stats`, `base`, `datasets`

---
