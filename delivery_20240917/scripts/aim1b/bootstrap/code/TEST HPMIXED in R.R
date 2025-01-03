# TEST HPMIXED in R



# Load packages ---------------------------------------------------------------
pacman::p_load(tidyverse,
               lme4,
               splines2)
library(magrittr, include = "%<>%")

# Load the SAS data set
data <- haven::read_sas("D:/PATHWEIGH/delivery_20240917/scripts/aim1b/bootstrap/analysis.sas7bdat")

control <- data %>% filter(Intervention == "Control")

# Prepare spline basis using natural cubic splines
# Define knots as per the SAS code
knots <- c(1, 4, 10)
spline_basis <- bSpline(control$month, knots = knots, degree = 3, intercept = FALSE)

# spline_basis <- naturalSpline(control$month, knots = knots, intercept = FALSE, integral = TRUE)

# Orthogonalize spline basis
qr_decomp <- qr(spline_basis)
orthogonal_basis <- qr.Q(qr_decomp)
colnames(orthogonal_basis) <- paste0("spl", 1:ncol(orthogonal_basis))
control <- cbind(control[, !colnames(control) %in% paste0("spl", 1:6)], orthogonal_basis)

# Convert spline basis to a data frame for compatibility
spline_df <- as.data.frame(spline_basis)

# Rename the data frame columns
colnames(spline_df) <- paste0("spl", 1:ncol(spline_df))

# Add spline columns to the dataset
control <- cbind(control, spline_df)

# Fit the mixed-effects model
# LEFT OFF HERE< COULDNT GET THE MODEL TO WORK, NOT SPECIFIED CORRECTLY
# Need to check if spline_basis is supposed to be a vector or a data frame
# formula <- as.formula(paste("Weight ~", paste(colnames(spline_df), collapse = " + "), 
#                             "+ (1 +", paste(colnames(spline_df), collapse = " + "), "| ID)"))
# model <- lmer(formula, data = control)

# Simplified random-effects structure
model <- lmer(Weight ~ spl1 + spl2 + spl3 + spl4 + spl5 + spl6 + (1 | ID), data = control)

summary(model)

# Another attempt
model <- lmer(Weight ~ spl1 + spl2 + (1 + spl1 + spl2 | ID), data = control)
summary(model)


# Extract fixed effects (Parameter Estimates)
parameter_estimates <- summary(model)$coefficients

# Extract random effects (Solution for Random Effects)
random_effects <- ranef(model)

# Process output for fixed effects
sf_1 <- data.frame(
  effect = rownames(parameter_estimates),
  overall = parameter_estimates[, "Estimate"]
)

# Process output for random effects
# The random effects include both intercept and spline terms
sr_1 <- do.call(rbind, lapply(random_effects$id, function(re) {
  data.frame(
    Subject = rownames(random_effects$id),
    effect = names(re),
    ssdev = as.numeric(re)
  )
}))

# Inspect the results
print(sf_1)
print(sr_1)
