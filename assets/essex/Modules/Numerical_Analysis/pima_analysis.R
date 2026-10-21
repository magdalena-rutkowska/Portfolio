# =====================================================================
# Pima Indian Diabetes Dataset - End of Module Assessment 1
# Numerical Analysis, University of Essex Online
# Full statistical analysis script (R 4.3)
# =====================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(car)                 # VIF, Levene test
  library(ResourceSelection)   # Hosmer-Lemeshow test
})

set.seed(2026)
out_dir <- "output"
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

sink(file.path(out_dir, "results.txt"), split = TRUE)
hdr <- function(x) cat("\n\n=====", x, "=====\n")

# ---------------------------------------------------------------------
# Chart style (validated reference palette)
# ---------------------------------------------------------------------
COL_NO   <- "#2a78d6"   # No diabetes  (categorical slot 1)
COL_YES  <- "#eb6834"   # Diabetes     (categorical slot 2)
COL_ONE  <- "#2a78d6"   # single series
INK      <- "#0b0b0b"
INK2     <- "#52514e"
GRID     <- "#e6e5e1"
SURF     <- "#fcfcfb"
theme_pres <- function() {
  theme_minimal(base_size = 15, base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = SURF, colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = GRID, linewidth = 0.4),
      axis.text  = element_text(colour = INK2),
      axis.title = element_text(colour = INK2),
      plot.title = element_text(colour = INK, face = "bold", size = 17),
      plot.subtitle = element_text(colour = INK2, size = 12),
      legend.position = "top", legend.justification = "left",
      legend.title = element_blank(), legend.text = element_text(colour = INK2),
      plot.margin = margin(12, 16, 12, 12)
    )
}
save_fig <- function(p, name, w = 8, h = 4.8) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h,
         dpi = 200, bg = SURF)
}

# ---------------------------------------------------------------------
# 1. Load data and treat impossible zeros as missing
# ---------------------------------------------------------------------
raw <- read.csv("diabetes.csv")
hdr("DATA STRUCTURE")
str(raw)

zero_cols <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
hdr("IMPOSSIBLE ZERO VALUES (count and % of 768)")
zc <- sapply(raw[zero_cols], function(v) sum(v == 0))
print(data.frame(zeros = zc, pct = round(100 * zc / nrow(raw), 1)))

d <- raw
for (v in zero_cols) d[[v]][d[[v]] == 0] <- NA
d$OutcomeF <- factor(d$Outcome, levels = c(0, 1), labels = c("No diabetes", "Diabetes"))

# Does missingness relate to outcome? (missing not at random check)
hdr("MISSINGNESS BY OUTCOME (Insulin and SkinThickness)")
for (v in c("Insulin", "SkinThickness")) {
  tb <- table(Missing = is.na(d[[v]]), Outcome = d$Outcome)
  print(tb); print(chisq.test(tb))
}

# ---------------------------------------------------------------------
# 2. Sample description
# ---------------------------------------------------------------------
hdr("SAMPLE SIZE")
cat("Individuals:", nrow(d), "\n")
hdr("OUTCOME PREVALENCE")
print(table(d$OutcomeF)); print(round(100 * prop.table(table(d$OutcomeF)), 2))

hdr("AGE DISTRIBUTION")
print(summary(d$Age))
cat("Skewness (Age):", round(mean((d$Age - mean(d$Age))^3) / sd(d$Age)^3, 3), "\n")
d$AgeGroup <- cut(d$Age, c(-Inf, 29, 39, 49, Inf), labels = c("<30", "30-39", "40-49", "50+"))
print(table(d$AgeGroup)); print(round(100 * prop.table(table(d$AgeGroup)), 1))

hdr("PREGNANCY STATUS")
d$EverPregnant <- factor(ifelse(d$Pregnancies == 0, "Never pregnant", "Ever pregnant"),
                         levels = c("Never pregnant", "Ever pregnant"))
print(table(d$EverPregnant)); print(round(100 * prop.table(table(d$EverPregnant)), 1))

# ---------------------------------------------------------------------
# 3. Descriptive statistics
# ---------------------------------------------------------------------
stat_mode <- function(x) {
  x <- x[!is.na(x)]; ux <- unique(x); tab <- tabulate(match(x, ux))
  m <- ux[tab == max(tab)]; paste(sort(m), collapse = "/")
}
describe <- function(x) {
  x <- x[!is.na(x)]
  q <- quantile(x, c(.25, .75))
  c(n = length(x), mean = mean(x), median = median(x), mode = stat_mode(x),
    min = min(x), max = max(x), range = max(x) - min(x), sd = sd(x),
    Q1 = q[[1]], Q3 = q[[2]], IQR = IQR(x),
    skew = mean((x - mean(x))^3) / sd(x)^3)
}
desc_vars <- list(
  Age = d$Age, BMI = d$BMI, Glucose = d$Glucose, BloodPressure = d$BloodPressure,
  Pregnancies_ever = d$Pregnancies[d$Pregnancies > 0],
  Pregnancies_all = d$Pregnancies,
  SkinThickness = d$SkinThickness, Insulin = d$Insulin,
  DiabetesPedigreeFunction = d$DiabetesPedigreeFunction)
desc_tab <- do.call(rbind, lapply(desc_vars, describe))
desc_df <- as.data.frame(desc_tab, stringsAsFactors = FALSE)
num_cols <- setdiff(names(desc_df), "mode")
desc_df[num_cols] <- lapply(desc_df[num_cols], function(z) round(as.numeric(z), 2))
hdr("DESCRIPTIVE STATISTICS (zeros treated as missing)")
print(desc_df)
write.csv(desc_df, file.path(out_dir, "descriptives.csv"))

# Raw (zeros included) comparison for the three affected key variables
hdr("DESCRIPTIVES IF ZEROS WERE LEFT IN (for comparison)")
print(round(sapply(raw[c("Glucose", "BloodPressure", "BMI")],
                   function(x) c(mean = mean(x), sd = sd(x), min = min(x))), 2))

# Descriptives by outcome
hdr("MEANS BY OUTCOME")
print(round(sapply(split(d[c("Glucose","BMI","BloodPressure")], d$OutcomeF),
                   function(s) colMeans(s, na.rm = TRUE)), 2))

# ---------------------------------------------------------------------
# 4. Outliers (1.5 x IQR rule) and implausible values
# ---------------------------------------------------------------------
hdr("OUTLIERS BY 1.5 x IQR RULE")
out_vars <- c("Age", "BMI", "Glucose", "BloodPressure", "Pregnancies",
              "SkinThickness", "Insulin", "DiabetesPedigreeFunction")
out_tab <- t(sapply(out_vars, function(v) {
  x <- d[[v]]; q <- quantile(x, c(.25, .75), na.rm = TRUE); i <- IQR(x, na.rm = TRUE)
  lo <- q[1] - 1.5 * i; hi <- q[2] + 1.5 * i
  c(lower_fence = unname(lo), upper_fence = unname(hi),
    n_low = sum(x < lo, na.rm = TRUE), n_high = sum(x > hi, na.rm = TRUE),
    max = max(x, na.rm = TRUE))
}))
print(round(out_tab, 2))
write.csv(round(out_tab, 2), file.path(out_dir, "outliers.csv"))
cat("\nExtreme individual values:\n")
cat("Max pregnancies:", max(d$Pregnancies), "| Max SkinThickness:", max(d$SkinThickness, na.rm=TRUE),
    "| Max Insulin:", max(d$Insulin, na.rm=TRUE), "| Max BMI:", max(d$BMI, na.rm=TRUE),
    "| Min BP:", min(d$BloodPressure, na.rm=TRUE), "| Max BP:", max(d$BloodPressure, na.rm=TRUE), "\n")
cat("Rows with SkinThickness > 60:\n"); print(d[which(d$SkinThickness > 60), 1:9])

# ---------------------------------------------------------------------
# 5. Diabetes rates by category
# ---------------------------------------------------------------------
d$BMICat <- cut(d$BMI, c(-Inf, 18.5, 25, 30, Inf), right = FALSE,
                labels = c("Underweight", "Normal", "Overweight", "Obese"))
d$PregGroup <- cut(d$Pregnancies, c(-Inf, 0, 2, 4, Inf), labels = c("0", "1-2", "3-4", "5+"))

rate_table <- function(g, name) {
  n <- tapply(d$Outcome, g, length); k <- tapply(d$Outcome, g, sum)
  ci <- t(mapply(function(k, n) binom.test(k, n)$conf.int, k, n))
  data.frame(variable = name, group = names(n), n = as.vector(n), diabetic = as.vector(k),
             rate_pct = round(100 * k / n, 1), ci_low = round(100 * ci[, 1], 1),
             ci_high = round(100 * ci[, 2], 1), row.names = NULL)
}
rates <- rbind(rate_table(d$AgeGroup, "Age group"),
               rate_table(d$BMICat, "BMI category"),
               rate_table(d$PregGroup, "Pregnancies"))
hdr("DIABETES RATE BY CATEGORY (exact 95% CI)")
print(rates)
cat("BMI missing (excluded from BMI categories):", sum(is.na(d$BMICat)), "\n")
write.csv(rates, file.path(out_dir, "rates.csv"), row.names = FALSE)

# ---------------------------------------------------------------------
# 6. Visualisations - descriptive
# ---------------------------------------------------------------------
hist_by <- function(v, bw, xlab, title, sub) {
  p <- ggplot(d[!is.na(d[[v]]), ], aes(x = .data[[v]], fill = OutcomeF)) +
    geom_histogram(binwidth = bw, colour = SURF, linewidth = 0.3, position = "stack") +
    scale_fill_manual(values = c(COL_NO, COL_YES)) +
    labs(x = xlab, y = "Number of women", title = title, subtitle = sub) +
    theme_pres()
  save_fig(p, paste0("hist_", v))
}
hist_by("Age", 3, "Age (years)", "Age distribution", "Right-skewed: median 29 years, 21-81 range")
hist_by("BMI", 2, "BMI (kg/m2)", "BMI distribution", "Most women fall in the obese range (30 or above)")
hist_by("Glucose", 8, "2-hour plasma glucose (mg/dL)", "Glucose distribution",
        "Diabetic cases concentrate above 140 mg/dL")
hist_by("BloodPressure", 4, "Diastolic blood pressure (mm Hg)", "Diastolic blood pressure",
        "Near-symmetric; extreme low values are implausible")

p <- ggplot(d, aes(x = factor(Pregnancies), fill = OutcomeF)) +
  geom_bar(colour = SURF, linewidth = 0.3) +
  scale_fill_manual(values = c(COL_NO, COL_YES)) +
  labs(x = "Number of pregnancies", y = "Number of women", title = "Number of pregnancies",
       subtitle = "Discrete, right-skewed count; 111 women never pregnant") + theme_pres()
save_fig(p, "bar_Pregnancies")

# Boxplots of the five key variables by outcome (standardised panel)
long <- do.call(rbind, lapply(c("Age","BMI","Glucose","BloodPressure","Pregnancies"), function(v)
  data.frame(variable = v, value = d[[v]], OutcomeF = d$OutcomeF)))
long <- long[!is.na(long$value), ]
p <- ggplot(long, aes(x = OutcomeF, y = value, fill = OutcomeF)) +
  geom_boxplot(width = 0.55, outlier.size = 1.2, outlier.alpha = 0.6, colour = INK2, linewidth = 0.4) +
  facet_wrap(~ variable, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c(COL_NO, COL_YES)) +
  labs(x = NULL, y = NULL, title = "Key variables by diagnosis",
       subtitle = "Points beyond the whiskers are 1.5 x IQR outliers") +
  theme_pres() + theme(axis.text.x = element_blank(), strip.text = element_text(colour = INK, face = "bold"))
save_fig(p, "box_by_outcome", w = 11, h = 4.8)

# Missing values chart
mz <- data.frame(var = names(zc), pct = 100 * zc / 768)
mz$var <- factor(mz$var, levels = mz$var[order(mz$pct)])
p <- ggplot(mz, aes(x = var, y = pct)) +
  geom_col(fill = COL_ONE, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.15, colour = INK2, size = 4.5) +
  coord_flip(ylim = c(0, 55)) +
  labs(x = NULL, y = "% of records recorded as zero",
       title = "Recorded zeros are missing data", subtitle = "Physiologically impossible values, n = 768") +
  theme_pres() + theme(panel.grid.major.y = element_blank(),
                       panel.grid.major.x = element_line(colour = GRID, linewidth = 0.4))
save_fig(p, "missing_zeros")

# Rates chart (three panels)
rates$group <- factor(rates$group, levels = unique(rates$group))
rates$variable <- factor(rates$variable, levels = c("Age group", "BMI category", "Pregnancies"))
p <- ggplot(rates, aes(x = group, y = rate_pct)) +
  geom_col(fill = COL_YES, width = 0.62) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.18, colour = INK2, linewidth = 0.4) +
  geom_text(aes(y = -3.5, label = paste0("n=", n)), colour = INK2, size = 3.4) +
  geom_hline(yintercept = 34.9, linetype = "dashed", colour = INK2, linewidth = 0.4) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = NULL, y = "Diabetes rate (%)", title = "Diabetes rate by group",
       subtitle = "Bars: % diagnosed; whiskers: exact 95% CI; dashed line: overall 34.9%") +
  theme_pres() + theme(strip.text = element_text(colour = INK, face = "bold"))
save_fig(p, "rates_by_group", w = 11, h = 4.8)

# ---------------------------------------------------------------------
# 7. Inferential statistics
# ---------------------------------------------------------------------
cohen_d <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  sp <- sqrt(((length(x)-1)*var(x) + (length(y)-1)*var(y)) / (length(x)+length(y)-2))
  (mean(y) - mean(x)) / sp
}

hdr("7a. GLUCOSE: DIABETIC vs NON-DIABETIC")
g0 <- d$Glucose[d$Outcome == 0]; g1 <- d$Glucose[d$Outcome == 1]
cat("Shapiro-Wilk (non-diabetic):"); print(shapiro.test(g0))
cat("Shapiro-Wilk (diabetic):");     print(shapiro.test(g1))
print(leveneTest(Glucose ~ OutcomeF, data = d))
print(t.test(Glucose ~ OutcomeF, data = d))           # Welch
print(wilcox.test(Glucose ~ OutcomeF, data = d, conf.int = TRUE))
cat("Cohen's d:", round(cohen_d(g0, g1), 3), "\n")

hdr("7b. PREGNANCIES: DIABETIC vs NON-DIABETIC")
p0 <- d$Pregnancies[d$Outcome == 0]; p1 <- d$Pregnancies[d$Outcome == 1]
print(tapply(d$Pregnancies, d$OutcomeF, function(x) c(mean = mean(x), median = median(x), sd = sd(x))))
mw <- wilcox.test(Pregnancies ~ OutcomeF, data = d, conf.int = TRUE, exact = FALSE)
print(mw)
z <- qnorm(mw$p.value / 2); cat("Effect size r = |Z|/sqrt(N):", round(abs(z) / sqrt(768), 3), "\n")
print(t.test(Pregnancies ~ OutcomeF, data = d))
cat("Cohen's d:", round(cohen_d(p0, p1), 3), "\n")

hdr("7c. CORRELATION MATRIX (pairwise complete)")
cont <- c("Pregnancies","Glucose","BloodPressure","SkinThickness","Insulin","BMI",
          "DiabetesPedigreeFunction","Age")
cm_p <- cor(d[cont], use = "pairwise.complete.obs", method = "pearson")
cm_s <- cor(d[cont], use = "pairwise.complete.obs", method = "spearman")
cat("Pearson:\n"); print(round(cm_p, 2))
cat("Spearman:\n"); print(round(cm_s, 2))
write.csv(round(cm_p, 3), file.path(out_dir, "correlation_pearson.csv"))
# p-values for Pearson
pm <- outer(cont, cont, Vectorize(function(a, b) if (a == b) NA else cor.test(d[[a]], d[[b]])$p.value))
dimnames(pm) <- list(cont, cont)
cat("Pearson p-values (<0.001 shown as 0):\n"); print(round(pm, 3))
cat("Point-biserial correlation with Outcome:\n")
print(round(sapply(cont, function(v) cor(d[[v]], d$Outcome, use = "complete.obs")), 3))

short <- c("Preg","Gluc","BP","Skin","Insulin","BMI","DPF","Age")
cmd <- expand.grid(x = short, y = short)
cmd$r <- as.vector(cm_p)
cmd$x <- factor(cmd$x, levels = short); cmd$y <- factor(cmd$y, levels = rev(short))
p <- ggplot(cmd, aes(x, y, fill = r)) +
  geom_tile(colour = SURF, linewidth = 1.2) +
  geom_text(aes(label = sprintf("%.2f", r), colour = abs(r) > 0.8), size = 4.2) +
  scale_colour_manual(values = c(INK, "white"), guide = "none") +
  scale_fill_gradient2(low = "#2a78d6", mid = "#f0efec", high = "#e34948", midpoint = 0,
                       limits = c(-1, 1), name = "Pearson r") +
  labs(x = NULL, y = NULL, title = "Correlation matrix", subtitle = "Pearson r, pairwise complete observations") +
  theme_pres() + theme(panel.grid = element_blank(), legend.position = "right")
save_fig(p, "correlation_heatmap", w = 8, h = 6.5)

hdr("7d. CHI-SQUARE: OUTCOME x AGE GROUP")
cramer_v <- function(tb) { cs <- suppressWarnings(chisq.test(tb, correct = FALSE))
  sqrt(cs$statistic / (sum(tb) * (min(dim(tb)) - 1))) }
tb_age <- table(d$AgeGroup, d$OutcomeF); print(tb_age)
cs_age <- chisq.test(tb_age); print(cs_age)
cat("Expected counts min:", round(min(cs_age$expected), 1), "\n")
cat("Standardised residuals:\n"); print(round(cs_age$stdres, 2))
cat("Cramer's V:", round(cramer_v(tb_age), 3), "\n")
cat("Chi-square test for trend (Cochran-Armitage via prop.trend.test):\n")
print(prop.trend.test(tb_age[, 2], rowSums(tb_age)))

hdr("7e. CHI-SQUARE: OUTCOME x BMI CATEGORY")
tb_bmi <- table(d$BMICat, d$OutcomeF); print(tb_bmi)
cs_bmi <- suppressWarnings(chisq.test(tb_bmi)); print(cs_bmi)
cat("Expected counts:\n"); print(round(cs_bmi$expected, 1))
cat("Fisher exact (because of small underweight cell):\n")
print(fisher.test(tb_bmi, simulate.p.value = TRUE, B = 1e5))
cat("Standardised residuals:\n"); print(round(cs_bmi$stdres, 2))
cat("Cramer's V:", round(cramer_v(tb_bmi), 3), "\n")
tb_bmi3 <- table(droplevels(factor(ifelse(d$BMICat %in% c("Underweight","Normal"), "Under/Normal",
                                    as.character(d$BMICat)), levels = c("Under/Normal","Overweight","Obese"))), d$OutcomeF)
cat("Merged (Under+Normal) 3x2 table:\n"); print(tb_bmi3); print(chisq.test(tb_bmi3))

hdr("7f. MEAN GLUCOSE ACROSS AGE GROUPS")
print(aggregate(Glucose ~ AgeGroup, d, function(x) round(c(n = length(x), mean = mean(x), sd = sd(x)), 1)))
aov_g <- aov(Glucose ~ AgeGroup, data = d); print(summary(aov_g))
ss <- summary(aov_g)[[1]]; cat("Eta squared:", round(ss$`Sum Sq`[1] / sum(ss$`Sum Sq`), 3), "\n")
print(leveneTest(Glucose ~ AgeGroup, data = d))
cat("Shapiro-Wilk on ANOVA residuals:"); print(shapiro.test(residuals(aov_g)))
print(oneway.test(Glucose ~ AgeGroup, data = d, var.equal = FALSE))     # Welch ANOVA
print(kruskal.test(Glucose ~ AgeGroup, data = d))
print(TukeyHSD(aov_g))
print(pairwise.t.test(d$Glucose, d$AgeGroup, pool.sd = FALSE, p.adjust.method = "holm"))

gm <- aggregate(Glucose ~ AgeGroup, d, function(x) c(m = mean(x), se = sd(x) / sqrt(length(x)), n = length(x)))
gm <- data.frame(AgeGroup = gm$AgeGroup, m = gm$Glucose[, "m"], se = gm$Glucose[, "se"], n = gm$Glucose[, "n"])
p <- ggplot(gm, aes(AgeGroup, m)) +
  geom_col(fill = COL_ONE, width = 0.6) +
  geom_errorbar(aes(ymin = m - 1.96 * se, ymax = m + 1.96 * se), width = 0.18, colour = INK2, linewidth = 0.4) +
  geom_text(aes(y = m + 1.96 * se + 4, label = sprintf("%.1f", m)), colour = INK, size = 4.5) +
  coord_cartesian(ylim = c(0, 160)) +
  labs(x = "Age group (years)", y = "Mean glucose (mg/dL)", title = "Mean glucose by age group",
       subtitle = "Whiskers: 95% CI of the mean") + theme_pres()
save_fig(p, "glucose_by_agegroup")

# ---------------------------------------------------------------------
# 8. Multiple linear regression: Glucose
# ---------------------------------------------------------------------
hdr("8. MULTIPLE LINEAR REGRESSION - GLUCOSE (complete cases)")
mlr_vars <- c("Glucose","Age","BMI","Pregnancies","BloodPressure","SkinThickness","Insulin","DiabetesPedigreeFunction")
dm <- na.omit(d[mlr_vars]); cat("n complete cases:", nrow(dm), "\n")
mlr <- lm(Glucose ~ Age + BMI + Pregnancies + BloodPressure + SkinThickness + Insulin + DiabetesPedigreeFunction, data = dm)
print(summary(mlr))
print(round(confint(mlr), 3))
cat("VIF:\n"); print(round(vif(mlr), 2))
dz <- as.data.frame(scale(dm))
mlr_z <- lm(Glucose ~ Age + BMI + Pregnancies + BloodPressure + SkinThickness + Insulin + DiabetesPedigreeFunction, data = dz)
cat("Standardised coefficients (beta):\n"); print(round(coef(mlr_z)[-1], 3))
cat("Shapiro-Wilk on residuals:"); print(shapiro.test(residuals(mlr)))
cat("Breusch-Pagan (ncvTest):"); print(ncvTest(mlr))
cat("Max Cook's distance:", round(max(cooks.distance(mlr)), 3), "\n")
# Log-insulin sensitivity model
mlr_log <- update(mlr, . ~ . - Insulin + log(Insulin))
cat("\nSensitivity: log(Insulin) instead of Insulin\n"); print(summary(mlr_log)$coefficients[, c(1,4)] |> round(4))
cat("R2 / adj R2:", round(summary(mlr_log)$r.squared, 3), round(summary(mlr_log)$adj.r.squared, 3), "\n")
# Model without Insulin and SkinThickness on larger sample
dm2 <- na.omit(d[c("Glucose","Age","BMI","Pregnancies","BloodPressure","DiabetesPedigreeFunction")])
mlr2 <- lm(Glucose ~ Age + BMI + Pregnancies + BloodPressure + DiabetesPedigreeFunction, data = dm2)
cat("\nSensitivity: without Insulin/SkinThickness, n =", nrow(dm2), "\n")
print(round(summary(mlr2)$coefficients, 4))
cat("R2 / adj R2:", round(summary(mlr2)$r.squared, 3), round(summary(mlr2)$adj.r.squared, 3), "\n")
# Insulin alone
cat("R2 of Insulin alone:", round(summary(lm(Glucose ~ Insulin, dm))$r.squared, 3), "\n")

beta <- data.frame(term = names(coef(mlr_z))[-1], b = coef(mlr_z)[-1],
                   p = summary(mlr)$coefficients[-1, 4])
beta$term <- c("Age","BMI","Pregnancies","Blood pressure","Skin thickness","Insulin","Pedigree (DPF)")
beta$sig <- ifelse(beta$p < 0.05, "p < 0.05", "Not significant")
beta$term <- factor(beta$term, levels = beta$term[order(beta$b)])
p <- ggplot(beta, aes(term, b, fill = sig)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.2f", b), hjust = ifelse(b >= 0, -0.2, 1.2)), colour = INK2, size = 4.2) +
  scale_fill_manual(values = c("p < 0.05" = COL_ONE, "Not significant" = "#c8c7c1")) +
  coord_flip(ylim = c(-0.15, 0.65)) +
  labs(x = NULL, y = "Standardised coefficient (beta)", title = "Predictors of glucose",
       subtitle = sprintf("Multiple linear regression, n = %d, adjusted R-squared = %.2f",
                          nrow(dm), summary(mlr)$adj.r.squared)) +
  theme_pres() + theme(panel.grid.major.y = element_blank(),
                       panel.grid.major.x = element_line(colour = GRID, linewidth = 0.4))
save_fig(p, "mlr_betas")

png(file.path(fig_dir, "mlr_diagnostics.png"), width = 1600, height = 1400, res = 200)
par(mfrow = c(2, 2)); plot(mlr); dev.off()

# ---------------------------------------------------------------------
# 9. Logistic regression: Outcome ~ BMI + Age + Glucose
# ---------------------------------------------------------------------
auc_fn <- function(y, p) {  # Mann-Whitney AUC
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
classify <- function(y, p, thr = 0.5) {
  pred <- as.integer(p >= thr)
  tp <- sum(pred == 1 & y == 1); tn <- sum(pred == 0 & y == 0)
  fp <- sum(pred == 1 & y == 0); fn <- sum(pred == 0 & y == 1)
  c(threshold = thr, TP = tp, FN = fn, FP = fp, TN = tn,
    accuracy = (tp + tn) / length(y), sensitivity = tp / (tp + fn),
    specificity = tn / (tn + fp), PPV = tp / (tp + fp), NPV = tn / (tn + fn))
}
or_table <- function(m) {
  ci <- suppressMessages(confint.default(m))
  round(cbind(OR = exp(coef(m)), lower = exp(ci[, 1]), upper = exp(ci[, 2]),
              p = summary(m)$coefficients[, 4]), 4)
}

dl <- na.omit(d[c("Outcome","BMI","Age","Glucose","Pregnancies")])
cat("\n"); hdr("9. LOGISTIC REGRESSION - Outcome ~ BMI + Age + Glucose")
cat("n complete cases:", nrow(dl), " diabetic:", sum(dl$Outcome), "\n")
lr <- glm(Outcome ~ BMI + Age + Glucose, data = dl, family = binomial)
print(summary(lr))
cat("Odds ratios (per 1 unit) with Wald 95% CI:\n"); print(or_table(lr))
cat("Odds ratios per clinically meaningful step: BMI +5, Age +10, Glucose +10\n")
print(round(exp(coef(lr)[-1] * c(5, 10, 10)), 3))
cat("McFadden pseudo R2:", round(1 - lr$deviance / lr$null.deviance, 3), "\n")
cat("Nagelkerke R2:", round((1 - exp((lr$deviance - lr$null.deviance) / nrow(dl))) /
                              (1 - exp(-lr$null.deviance / nrow(dl))), 3), "\n")
cat("Omnibus LR test vs null: chi2 =", round(lr$null.deviance - lr$deviance, 2), "df = 3 p =",
    signif(pchisq(lr$null.deviance - lr$deviance, 3, lower.tail = FALSE), 3), "\n")
hl <- hoslem.test(dl$Outcome, fitted(lr), g = 10); print(hl)
cat("HL observed vs expected:\n"); print(round(cbind(hl$observed, hl$expected), 1))
for (g in c(8, 12)) cat("HL with g =", g, ": p =", round(hoslem.test(dl$Outcome, fitted(lr), g = g)$p.value, 3), "\n")
cat("VIF:\n"); print(round(vif(lr), 2))

cat("\nClassification at threshold 0.5:\n")
cls50 <- classify(dl$Outcome, fitted(lr), 0.5); print(round(cls50, 3))
cat("AUC:", round(auc_fn(dl$Outcome, fitted(lr)), 3), "\n")

# Threshold tuning: Youden J and a sensitivity target of 0.80
thr_grid <- seq(0.05, 0.95, by = 0.01)
grid <- t(sapply(thr_grid, function(t) classify(dl$Outcome, fitted(lr), t)))
youden <- grid[which.max(grid[, "sensitivity"] + grid[, "specificity"] - 1), ]
cat("\nYouden-optimal threshold:\n"); print(round(youden, 3))
sens80 <- grid[max(which(grid[, "sensitivity"] >= 0.80)), ]
cat("Highest threshold with sensitivity >= 0.80:\n"); print(round(sens80, 3))

# 10-fold cross-validation (out-of-sample check of optimism)
folds <- sample(rep(1:10, length.out = nrow(dl)))
cv_p <- numeric(nrow(dl))
for (k in 1:10) {
  fit <- glm(Outcome ~ BMI + Age + Glucose, data = dl[folds != k, ], family = binomial)
  cv_p[folds == k] <- predict(fit, dl[folds == k, ], type = "response")
}
cat("\n10-fold CV at 0.5:\n"); print(round(classify(dl$Outcome, cv_p, 0.5), 3))
cat("10-fold CV at Youden threshold", youden["threshold"], ":\n")
print(round(classify(dl$Outcome, cv_p, youden["threshold"]), 3))
cat("CV AUC:", round(auc_fn(dl$Outcome, cv_p), 3), "\n")

# Calibration plot (deciles)
dec <- cut(fitted(lr), quantile(fitted(lr), 0:10 / 10), include.lowest = TRUE)
cal <- data.frame(pred = tapply(fitted(lr), dec, mean), obs = tapply(dl$Outcome, dec, mean))
p <- ggplot(cal, aes(pred, obs)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = INK2, linewidth = 0.4) +
  geom_line(colour = COL_ONE, linewidth = 0.8) +
  geom_point(colour = COL_ONE, size = 3, stroke = 0) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Predicted probability (decile mean)", y = "Observed diabetes rate",
       title = "Calibration: predicted vs observed",
       subtitle = sprintf("Hosmer-Lemeshow chi-sq = %.2f, df = 8, p = %.3f", hl$statistic, hl$p.value)) +
  theme_pres() + theme(panel.grid.major.x = element_line(colour = GRID, linewidth = 0.4))
save_fig(p, "logit_calibration", w = 6.5, h = 5.5)

# ROC curve
roc <- data.frame(fpr = 1 - grid[, "specificity"], tpr = grid[, "sensitivity"])
roc <- rbind(data.frame(fpr = 1, tpr = 1), roc, data.frame(fpr = 0, tpr = 0))
pts <- data.frame(fpr = 1 - c(cls50["specificity"], youden["specificity"]),
                  tpr = c(cls50["sensitivity"], youden["sensitivity"]),
                  lab = c(sprintf("0.50 threshold\nsens %.0f%%, spec %.0f%%", 100*cls50["sensitivity"], 100*cls50["specificity"]),
                          sprintf("%.2f threshold\nsens %.0f%%, spec %.0f%%", youden["threshold"], 100*youden["sensitivity"], 100*youden["specificity"])))
p <- ggplot(roc, aes(fpr, tpr)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = INK2, linewidth = 0.4) +
  geom_path(colour = COL_ONE, linewidth = 0.8) +
  geom_point(data = pts, colour = COL_YES, size = 3.5) +
  geom_text(data = pts, aes(label = lab), hjust = -0.08, vjust = 1.1, size = 3.6, colour = INK) +
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
  labs(x = "False positive rate (1 - specificity)", y = "Sensitivity",
       title = "ROC curve: BMI + Age + Glucose",
       subtitle = sprintf("AUC = %.3f (10-fold CV AUC = %.3f)", auc_fn(dl$Outcome, fitted(lr)), auc_fn(dl$Outcome, cv_p))) +
  theme_pres() + theme(panel.grid.major.x = element_line(colour = GRID, linewidth = 0.4))
save_fig(p, "logit_roc", w = 6.5, h = 5.5)

# ---------------------------------------------------------------------
# 10. Interaction BMI x Age
# ---------------------------------------------------------------------
hdr("10. INTERACTION BMI x AGE")
# centred versions so main effects are interpretable at the sample mean
bmi_c <- mean(dl$BMI); age_c <- mean(dl$Age)
dl$BMIc <- dl$BMI - bmi_c; dl$Agec <- dl$Age - age_c
cat("Centring values: BMI", round(bmi_c, 2), " Age", round(age_c, 2), "\n")

cat("\n(a) BMI + Age only\n")
ia0 <- glm(Outcome ~ BMIc + Agec, data = dl, family = binomial)
ia1 <- glm(Outcome ~ BMIc * Agec, data = dl, family = binomial)
print(anova(ia0, ia1, test = "LRT")); print(or_table(ia1))

cat("\n(b) BMI + Age + Glucose\n")
ib0 <- glm(Outcome ~ BMIc + Agec + Glucose, data = dl, family = binomial)
ib1 <- glm(Outcome ~ BMIc * Agec + Glucose, data = dl, family = binomial)
print(anova(ib0, ib1, test = "LRT")); print(or_table(ib1))

hdr("11. MODEL 1 vs MODEL 2")
m1 <- glm(Outcome ~ BMIc + Agec + Glucose + Pregnancies, data = dl, family = binomial)
m2 <- glm(Outcome ~ BMIc + Agec + Glucose + Pregnancies + BMIc:Agec, data = dl, family = binomial)
cat("MODEL 1\n"); print(summary(m1)); print(or_table(m1))
cat("MODEL 2\n"); print(summary(m2)); print(or_table(m2))
lrt <- anova(m1, m2, test = "LRT"); print(lrt)

cmp <- function(m, name) {
  pr <- fitted(m); c50 <- classify(dl$Outcome, pr, 0.5)
  hlm <- hoslem.test(dl$Outcome, pr, g = 10)
  data.frame(model = name, k = length(coef(m)), logLik = round(as.numeric(logLik(m)), 2),
             deviance = round(deviance(m), 2), AIC = round(AIC(m), 2), BIC = round(BIC(m), 2),
             McFadden = round(1 - m$deviance / m$null.deviance, 3),
             AUC = round(auc_fn(dl$Outcome, pr), 3), HL_p = round(hlm$p.value, 3),
             accuracy = round(c50["accuracy"], 3), sensitivity = round(c50["sensitivity"], 3),
             specificity = round(c50["specificity"], 3), row.names = NULL)
}
cmp_tab <- rbind(cmp(lr, "Base: BMI+Age+Glucose"), cmp(m1, "Model 1: +Pregnancies"),
                 cmp(m2, "Model 2: +BMI x Age"))
print(cmp_tab)
write.csv(cmp_tab, file.path(out_dir, "model_comparison.csv"), row.names = FALSE)

# Odds ratio of BMI (+5 units) at different ages, from Model 2
b <- coef(m2)
ages <- c(25, 35, 45, 55)
or_bmi_age <- sapply(ages, function(a) exp(5 * (b["BMIc"] + b["BMIc:Agec"] * (a - age_c))))
V <- vcov(m2)
se_bmi_age <- sapply(ages, function(a) {
  L <- c(BMIc = 5, `BMIc:Agec` = 5 * (a - age_c))
  sqrt(t(L) %*% V[names(L), names(L)] %*% L)
})
or_df <- data.frame(age = ages, OR_BMI_plus5 = round(or_bmi_age, 3),
                    lower = round(exp(log(or_bmi_age) - 1.96 * se_bmi_age), 3),
                    upper = round(exp(log(or_bmi_age) + 1.96 * se_bmi_age), 3))
cat("\nOdds ratio for +5 BMI units at selected ages (Model 2):\n"); print(or_df)
cat("Interaction OR per 1 BMI x 1 year:", round(exp(b["BMIc:Agec"]), 5),
    " | per 5 BMI x 10 years:", round(exp(b["BMIc:Agec"] * 50), 3), "\n")
write.csv(or_df, file.path(out_dir, "or_bmi_by_age.csv"), row.names = FALSE)

# Predicted probability plot (Model 2), glucose and pregnancies at their means
nd <- expand.grid(BMI = seq(20, 50, 0.5), Age = c(25, 40, 55))
nd$BMIc <- nd$BMI - bmi_c; nd$Agec <- nd$Age - age_c
nd$Glucose <- mean(dl$Glucose); nd$Pregnancies <- mean(dl$Pregnancies)
nd$p <- predict(m2, nd, type = "response")
nd$AgeF <- factor(paste("Age", nd$Age), levels = paste("Age", c(25, 40, 55)))
lab <- nd[nd$BMI == 50, ]
p <- ggplot(nd, aes(BMI, p, colour = AgeF)) +
  geom_line(linewidth = 0.9) +
  geom_text(data = lab, aes(label = AgeF), hjust = -0.1, size = 4, show.legend = FALSE) +
  scale_colour_manual(values = c("#2a78d6", "#eb6834", "#1baf7a")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  coord_cartesian(xlim = c(20, 54)) +
  labs(x = "BMI (kg/m2)", y = "Predicted probability of diabetes",
       title = "Near-parallel curves: no meaningful BMI x Age interaction",
       subtitle = "Glucose and pregnancies held at sample means") + theme_pres()
save_fig(p, "interaction_bmi_age")

# Sanity check of pregnancies-age collinearity
cat("\nCorrelation Age-Pregnancies:", round(cor(dl$Age, dl$Pregnancies), 3), "\n")
cat("VIF Model 2 (uncentred check not needed; centred):\n"); print(round(vif(m1), 2))


hdr("12. SUPPLEMENTARY CHECKS")
cat("(a) Non-linear age: add Age^2 to BMI + Age + Glucose\n")
q0 <- glm(Outcome ~ BMI + Age + Glucose, data = dl, family = binomial)
q1 <- glm(Outcome ~ BMI + Age + I(Age^2) + Glucose, data = dl, family = binomial)
print(anova(q0, q1, test = "LRT")); print(round(summary(q1)$coefficients, 5))
cat("Age at peak log-odds:", round(-coef(q1)["Age"] / (2 * coef(q1)["I(Age^2)"]), 1), "\n")
q2 <- glm(Outcome ~ BMI * Age + I(Age^2) + Glucose, data = dl, family = binomial)
cat("Interaction test after allowing curved age:\n"); print(anova(q1, q2, test = "LRT"))
cat("\n(b) Pregnancy groups adjusted for age\n")
pg <- glm(Outcome ~ PregGroup, data = d, family = binomial)
pga <- glm(Outcome ~ PregGroup + Age, data = d, family = binomial)
print(or_table(pg)); print(or_table(pga))
cat("Mean age by pregnancy group:\n"); print(round(tapply(d$Age, d$PregGroup, mean), 1))
cat("\n(c) Pregnancies in Model 1 vs Age: correlation", round(cor(d$Age, d$Pregnancies), 3), "\n")
sink()
cat("Done.\n")
