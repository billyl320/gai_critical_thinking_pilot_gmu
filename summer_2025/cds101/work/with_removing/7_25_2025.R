###########################
##### ANOVA on CDS 101
###########################

##### Limit ourselves to 1-way ANOVA

##### Initial next step:

# i. Write code to then load the 2 csv files into R
treatment <- read.csv("treatment.csv")
control <- read.csv("control.csv")

# ii. Have the loaded 2 data objects combined into 1 file

cols_keep <- c(
  "Student", "Generative.AI.assignment", "Final.Project",
  "Final.Grade"
)

write.csv(
  rbind(
    transform(treatment[, cols_keep], Group = "Treatment"),
    transform(control[, cols_keep], Group = "Control")
  ),
  "combined_data.csv",
  row.names = FALSE
)
combined <- read.csv("combined_data.csv")
rows_rmv <- c(1, 2, 9, 10)
combined <- combined[-rows_rmv, ]
combined$Final.Project <- as.numeric(combined$Final.Project)
missing_rmv <- c(9, 10)
combined <- combined[-missing_rmv, ]

# iii. Have it perform the anova analysis including

### Null hypothesis and Alternative hypothesis
cat("Null Hypothesis (H0): The mean GAI HW grade is the same for all groups.")
cat("Alternative Hypothesis (H1): A least one group mean is different.")

# set significance level
alpha <- 0.05


## 1. boxplots
png("plots/boxplot_gai_hw.png", width = 1024, height = 768)

boxplot(Generative.AI.assignment ~ Group,
  data = combined,
  main = "GAI HW by Treatment vs. Control",
  xlab = "Group", ylab = "GAI HW"
)

dev.off()

png("plots/boxplot_final.png", width = 1024, height = 768)

boxplot(Final.Project ~ Group,
  data = combined,
  main = "Final Project by Treatment vs. Control",
  xlab = "Group", ylab = "Final Project"
)

dev.off()


## 2. ANOVA table
fit <- aov(Generative.AI.assignment ~ Group, data = combined)
summary(fit)

## 3. Assumption checks
# A1: Completely randomized designed?
# A2: Is error normally distributed? (check through QQ-Plot)
# A3: Are errors independent from one another? (shape of the errors)
# A4: Are variances equal?

# Assumption 1
# A1: Assume that it is completely randomized design.

# Assumption 2
png("plots/qqplot.png", width = 1024, height = 768, res = 200)
plot(fit, 2)
dev.off()

# Assumption 3 & 4
png("plots/residuals_fitted_plot.png", width = 1024, height = 768, res = 200)
plot(fit, 1)
dev.off()


## 4. Have the code print out an interpretation if statistically significant
## or another statement if not statistically significant
anova_result <- summary(fit)
p_value <- anova_result[[1]]["Group", "Pr(>F)"]

if (p_value < alpha) {
  cat("The treatment is statistically significant (p =", p_value, ") at alpha level", alpha, ".\n")
} else {
  cat("The treatment is not statistically significant (p =", p_value, ") at alpha level", alpha, ".\n")
}


# LaTex version
if (p_value < alpha) {
  cat("The treatment is statistically significant ($p =", p_value, "$) at $\\alpha =", alpha, "$.\n")
} else {
  cat("The treatment is not statistically significant ($p =", p_value, "$) at $\\alpha=", alpha, "$.\n")
}




##### Nonparametric version of ANOVA

# H0: All 2 continuous population distributions are the same
# H1: At least 1 differs

fit <- kruskal.test(Generative.AI.assignment ~ Group, data = combined)
fit


### print out the p-value and if it is statistically significant or not.

p_value_kw <- fit$p.value

if (p_value_kw < alpha) {
  cat("The treatment is statistically significant (p =", p_value_kw, ") at alpha level", alpha, ".\n")
} else {
  cat("The treatment is not statistically significant (p =", p_value_kw, ") at alpha level", alpha, ".\n")
}


# LaTex version
if (p_value_kw < alpha) {
  cat("The treatment is statistically significant ($p =", p_value_kw, "$) at $\\alpha =", alpha, "$.\n")
} else {
  cat("The treatment is not statistically significant ($p =", p_value_kw, "$) at $\\alpha=", alpha, "$.\n")
}
