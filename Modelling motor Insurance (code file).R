# =============================================================================
# Motor Insurance Claim Risk Modelling  (v4)
# Run top to bottom with the working directory set to the folder holding the CSV.
# Outputs: figures/fig01..fig18 (PNG) and tables/*.csv for the report.
#
# Research questions as refined after the data review:
#  Q1 How are vehicle safety features and engine characteristics associated with claim
#     probability? (analysed at car-model and variable-group level: only 11 distinct vehicles)
#  Q2 Can high-risk policy segments be identified using vehicle and customer characteristics?
#  Q3 How does policy duration affect the probability of a claim?
#  Q4 How can an individual risk score be constructed to support premium differentiation?
#  Q5 Which groups of vehicle characteristics carry the most claim-risk information?
#  Q6 How much of the variation in claim risk is explained by region compared with vehicle?
# Exploratory questions added after seeing the results:
#  E1 Does a boosted model materially beat the GLMs (what is the performance ceiling)?
#  E2 Is the vehicle age x duration interaction material?
# =============================================================================

# ---- 0. SETUP ---------------------------------------------------------------
set.seed(061024)

pkgs <- c("car", "pROC", "rpart", "rpart.plot", "glmnet", "ranger", "lme4", "gbm")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) try(install.packages(new), silent = TRUE)
for (p in setdiff(pkgs, "rpart.plot")) library(p, character.only = TRUE)  # rpart.plot optional
library(splines)

dir.create("figures", showWarnings = FALSE)
dir.create("tables",  showWarnings = FALSE)

fml <- function(resp, terms) as.formula(paste(resp, "~", paste(terms, collapse = " + ")))

# Draw a plot straight into figures/<name> (the plotting code runs after the device opens)
save_fig <- function(name, expr, w = 1800, h = 1200) {
  png(file.path("figures", name), width = w, height = h, res = 200)
  on.exit(dev.off())
  force(expr)
  invisible(NULL)
}

# ---- 1. LOAD AND CLEAN ------------------------------------------------------
Motor <- read.csv("Insurance claims data.csv", na.strings = c("", "NA"))

Motor <- Motor[, !grepl("^X(\\.[0-9]+)?$", names(Motor))]   # junk Excel columns
Motor <- Motor[!is.na(Motor$claim_status), ]                # empty rows
stopifnot(sum(is.na(Motor)) == 0)

names(Motor)[names(Motor) == "displacement"] <- "Engine_Size"

yn_cols <- c("is_tpms", "is_adjustable_steering", "is_brake_assist", "is_central_locking",
             "is_day_night_rear_view_mirror", "is_driver_seat_height_adjustable",
             "is_speed_alert", "is_ecw", "is_esc", "is_rear_window_wiper",
             "is_rear_window_washer", "is_rear_window_defogger", "is_power_steering",
             "is_power_door_locks", "is_parking_sensors", "is_parking_camera",
             "is_front_fog_lights")
Motor[yn_cols] <- lapply(Motor[yn_cols], function(x) x == "Yes")

Motor$claim_status <- Motor$claim_status == 1     # logical outcome
Motor$y <- as.numeric(Motor$claim_status)         # 0/1 copy for glmer / glmnet

cat_cols <- c("region_code", "segment", "fuel_type", "engine_type",
              "rear_brakes_type", "transmission_type", "steering_type", "model")
Motor[cat_cols] <- lapply(Motor[cat_cols], factor)

yn_dups <- yn_cols[duplicated(as.list(Motor[yn_cols]))]
cat("Duplicate flag columns removed:", yn_dups, "\n")
Motor <- Motor[, !(names(Motor) %in% yn_dups)]

Motor$age_band <- cut(Motor$customer_age, c(34, 39, 44, 49, 54, 59, Inf),
                      labels = c("35-39", "40-44", "45-49", "50-54", "55-59", "60+"))
Motor$sub_bin  <- cut(Motor$subscription_length, c(-Inf, 0, 2, 4, 6, 8, 10, 12, 14),
                      labels = c("0", "0-2", "2-4", "4-6", "6-8", "8-10", "10-12", "12-14"))

n_obs      <- nrow(Motor)
prob_claim <- mean(Motor$claim_status)
cat("Policies:", n_obs, "| Claims:", sum(Motor$claim_status),
    "| Claim rate:", round(prob_claim, 4), "\n")
saveRDS(Motor, "Motor_clean.rds")

# ---- 2. EDA -----------------------------------------------------------------
summary(Motor)
mean(Motor$ncap_rating)

num_eng <- c("Engine_Size", "cylinder", "max_power", "max_torque",
             "gross_weight", "length", "width", "turning_radius")
cm <- cor(Motor[, num_eng])
round(cm, 2)

# Claim rate with exact binomial CIs; HIGH/LOW = CI entirely above/below portfolio rate
rate_table <- function(d, var, min_n = 200) {
  d[[var]] <- droplevels(factor(d[[var]]))
  tab <- do.call(rbind, lapply(split(d$claim_status, d[[var]]), function(x) {
    n <- length(x); k <- sum(x)
    ci <- binom.test(k, n)$conf.int
    data.frame(n = n, claims = k, rate = k / n, lo = ci[1], hi = ci[2])
  }))
  tab$level <- rownames(tab)
  tab$flag  <- ifelse(tab$n >= min_n & tab$lo > prob_claim, "HIGH",
                      ifelse(tab$n >= min_n & tab$hi < prob_claim, "LOW", ""))
  tab[order(-tab$rate), ]
}

# Bar chart of claim rates with CI whiskers; dashed line = portfolio rate
plot_rates <- function(tab, main, ord = NULL, xlab = "") {
  tab <- if (is.null(ord)) tab[order(nchar(tab$level), tab$level), ] else tab[match(ord, tab$level), ]
  tab <- tab[!is.na(tab$rate), ]
  mids <- barplot(tab$rate, names.arg = tab$level, las = 2, col = "grey80",
                  ylim = c(0, max(tab$hi) * 1.15), ylab = "Claim rate", xlab = xlab, main = main)
  arrows(mids, tab$lo, mids, tab$hi, angle = 90, code = 3, length = 0.03)
  abline(h = prob_claim, lty = 2)
  invisible(tab)
}

rate_table(Motor, "segment")
rate_table(Motor, "fuel_type")
rate_table(Motor, "age_band")
rate_table(Motor, "sub_bin")
table(Motor$subscription_length == 0)

spec_cols <- intersect(c("segment", "fuel_type", "engine_type", "Engine_Size", "cylinder",
                         "max_power", "max_torque", "gross_weight", "ncap_rating", "airbags"),
                       names(Motor))
cat("Car models:", length(unique(Motor$model)),
    "| distinct spec combinations:", nrow(unique(Motor[spec_cols])), "\n")

# FIGURE 1: correlation among engine and size variables
save_fig("fig01_corr_engine_size.png", {
  par(mar = c(8, 8, 3, 2))
  k <- nrow(cm)
  image(1:k, 1:k, t(cm[k:1, ]), col = hcl.colors(20, "Blues", rev = TRUE), zlim = c(0.5, 1),
        axes = FALSE, xlab = "", ylab = "", main = "Correlation among engine and size variables")
  axis(1, 1:k, colnames(cm), las = 2)
  axis(2, 1:k, rev(rownames(cm)), las = 2)
  for (i in 1:k) for (j in 1:k) text(j, k - i + 1, sprintf("%.2f", cm[i, j]), cex = 0.7)
})

# ---- 3. TRAIN / TEST SPLIT --------------------------------------------------
idx   <- sample(n_obs, round(0.7 * n_obs))
train <- Motor[idx, ]
test  <- Motor[-idx, ]

for (v in cat_cols) test <- test[test[[v]] %in% unique(train[[v]]), ]
train <- droplevels(train)
test[cat_cols] <- Map(function(x, ref) factor(x, levels = levels(ref)),
                      test[cat_cols], train[cat_cols])

keep <- function(v) intersect(v, names(Motor))
safety_vars <- keep(c("airbags", "ncap_rating", "is_esc", "is_tpms", "is_brake_assist",
                      "is_parking_sensors", "is_parking_camera", "is_speed_alert"))
engine_vars <- keep(c("Engine_Size", "max_power", "fuel_type"))
controls    <- c("vehicle_age", "customer_age", "subscription_length")

auc_of <- function(y, p) as.numeric(auc(roc(as.numeric(y), p, quiet = TRUE)))

eval_glm <- function(m) {
  p  <- pmin(pmax(predict(m, test, type = "response"), 1e-12), 1 - 1e-12)
  y  <- as.numeric(test$claim_status)
  p0 <- mean(train$claim_status)
  ll  <- sum(y * log(p)  + (1 - y) * log(1 - p))
  ll0 <- sum(y * log(p0) + (1 - y) * log(1 - p0))
  data.frame(AUC = auc_of(y, p),
             McFadden_train = 1 - m$deviance / m$null.deviance,
             McFadden_test  = 1 - ll / ll0)
}

# Fit a logistic model; drop any variable whose coefficient is NA (aliased) and refit.
# Variables listed FIRST are kept in preference to those listed later.
drop_aliased <- function(vars, data, resp = "claim_status") {
  repeat {
    m  <- glm(fml(resp, vars), family = binomial, data = data)
    na <- is.na(coef(m))
    if (!any(na)) return(list(model = m, vars = vars))
    bad <- attr(terms(m), "term.labels")[unique(attr(model.matrix(m), "assign")[na])]
    message("Dropping aliased variable(s): ", paste(bad, collapse = ", "))
    vars <- setdiff(vars, bad)
  }
}

# ---- Tools shared by Q1 and Q5 (vehicle-level analysis) ---------------------
# Standardise numeric vehicle variables so odds ratios are "per 1 SD"
z_vars  <- keep(c("airbags", "ncap_rating", "Engine_Size", "max_power", "max_torque",
                  "cylinder", "gross_weight", "length", "width", "turning_radius"))
train_z <- train
train_z[z_vars] <- lapply(train[z_vars], function(x) as.numeric(scale(x)))
m_ctrl_z <- glm(fml("claim_status", controls), family = binomial, data = train_z)

# One vehicle variable at a time, adjusted only for the policy controls
one_at_a_time <- function(v) {
  m  <- glm(fml("claim_status", c(v, controls)), family = binomial, data = train_z)
  a  <- anova(m_ctrl_z, m, test = "Chisq")
  cf <- coef(m)
  nm <- grep(paste0("^", v), names(cf), value = TRUE)
  or <- if (length(nm) == 1) unname(exp(cf[nm])) else NA
  ci <- if (length(nm) == 1) exp(confint.default(m)[nm, ]) else c(NA, NA)
  data.frame(variable = v, df = a$Df[2], LRT = a$Deviance[2], p = a$`Pr(>Chi)`[2],
             OR = or, lo = unname(ci[1]), hi = unname(ci[2]))
}
marg_table <- function(vars) {
  out <- do.call(rbind, lapply(vars, one_at_a_time))
  out$p_holm   <- p.adjust(out$p, "holm")
  out$AIC_gain <- out$LRT - 2 * out$df      # fair across variables with different df
  out[order(-out$AIC_gain), ]
}

# A group of variables at a time: deviance and test AUC added beyond the controls
m_ctrl <- glm(fml("claim_status", controls), family = binomial, data = train)
group_fit <- function(g) {
  fit <- drop_aliased(c(g, controls), train)
  m   <- fit$model
  dd  <- m_ctrl$deviance - m$deviance
  dfd <- m_ctrl$df.residual - m$df.residual
  data.frame(dev_drop = dd, df = dfd, p = pchisq(dd, dfd, lower.tail = FALSE),
             AUC_test = eval_glm(m)$AUC,
             kept = paste(setdiff(fit$vars, controls), collapse = ", "))
}
group_table <- function(groups) {
  out <- do.call(rbind, lapply(groups, group_fit))
  out$group <- names(groups)
  rownames(out) <- NULL
  out[, c("group", "dev_drop", "df", "p", "AUC_test", "kept")]
}
cat("Controls-only test AUC:", round(eval_glm(m_ctrl)$AUC, 4), "\n")

# =============================================================================
# Q1 (refined): Association of safety features and engine characteristics with P(claim)
# -----------------------------------------------------------------------------
# Every vehicle spec is fixed per car model (11 models), so individual spec effects
# cannot be separated in one regression. We therefore analyse at the vehicle level:
#  (a) claim rate per car model next to its specs
#  (b) does the car model matter at all (beyond policy controls)?
#  (c) each safety/engine variable on its own, adjusted for policy controls only
#  (d) safety group vs engine group vs both
# =============================================================================
# (a) vehicle-level table
specs <- unique(Motor[c("model", "segment", "fuel_type", "Engine_Size", "max_power",
                        "airbags", "ncap_rating", "is_esc", "is_tpms", "is_brake_assist")])
specs <- specs[!duplicated(specs$model), ]
specs$model <- as.character(specs$model)
veh_tab <- rate_table(Motor, "model")
q1_tab  <- merge(veh_tab, specs, by.x = "level", by.y = "model")
q1_tab  <- q1_tab[order(-q1_tab$rate), ]
q1_tab

# (b) does car model matter beyond the policy controls?
m_model <- glm(fml("claim_status", c("model", controls)), family = binomial, data = train)
anova(m_ctrl, m_model, test = "Chisq")

# (c) one variable at a time
q1_vars <- keep(c(safety_vars, "Engine_Size", "max_power", "max_torque", "cylinder", "fuel_type"))
q1_marg <- marg_table(q1_vars)
q1_marg

# (d) groups
q1_groups <- list(safety = safety_vars,
                  engine = keep(c("Engine_Size", "max_power", "max_torque", "cylinder", "fuel_type")),
                  safety_plus_engine = keep(c(safety_vars, "Engine_Size", "max_power", "fuel_type")))
q1_group_tab <- group_table(q1_groups)
q1_group_tab

# FIGURE 2: claim rate by car model
save_fig("fig02_vehicle_model_rates.png", {
  par(mar = c(5, 5, 3, 1))
  plot_rates(veh_tab, "Claim rate by car model (95% CI)", xlab = "Car model")
})
# FIGURE 3: forest plot of one-at-a-time odds ratios (numeric per 1 SD; flags TRUE vs FALSE)
fp <- q1_marg[!is.na(q1_marg$OR), ]
fp <- fp[order(fp$OR), ]
save_fig("fig03_q1_marginal_or.png", {
  par(mar = c(4, 11, 3, 1))
  yy <- seq_len(nrow(fp))
  plot(fp$OR, yy, log = "x", xlim = range(c(fp$lo, fp$hi, 1)), yaxt = "n", pch = 19,
       xlab = "Odds ratio (log scale)", ylab = "",
       main = "Safety and engine variables, one at a time\n(adjusted for vehicle age, customer age, duration)")
  segments(fp$lo, yy, fp$hi, yy)
  abline(v = 1, lty = 2)
  axis(2, at = yy, labels = fp$variable, las = 1)
})

# =============================================================================
# Q2: High-risk policy segments
# =============================================================================
Motor$seg_fuel <- interaction(Motor$segment, Motor$fuel_type, drop = TRUE)
rate_table(Motor, "seg_fuel")

tree_vars <- keep(c("segment", "fuel_type", "engine_type", "customer_age", "vehicle_age",
                    "airbags", "ncap_rating", "Engine_Size", "max_power", "region_density",
                    "subscription_length", "is_esc", "is_tpms", "transmission_type"))
tree_full <- rpart(fml("factor(claim_status)", tree_vars), data = train, method = "class",
                   parms = list(prior = c(0.5, 0.5)),
                   control = rpart.control(cp = 0.0005, minbucket = 200))
printcp(tree_full)
best_cp <- tree_full$cptable[which.min(tree_full$cptable[, "xerror"]), "CP"]
tree    <- prune(tree_full, cp = best_cp)

if (requireNamespace("rpart.plot", quietly = TRUE) && nrow(tree$frame) > 1) {
  print(rpart.plot::rpart.rules(tree))   # NOTE: probabilities here use 50/50 priors
} else {
  print(tree)
}

# Leaf rates on TRAIN and TEST. 'node' matches the node numbers shown in the tree plot.
p_tree_train <- predict(tree, train, type = "prob")[, "TRUE"]
p_tree       <- predict(tree, test,  type = "prob")[, "TRUE"]
train$leaf   <- rownames(tree$frame)[tree$where]
train$leaf_p <- round(p_tree_train, 6)
test$leaf_p  <- round(p_tree, 6)
key    <- unique(data.frame(level = as.character(train$leaf_p), node = train$leaf,
                            stringsAsFactors = FALSE))
tab_tr <- rate_table(train, "leaf_p")
tab_te <- rate_table(test,  "leaf_p", min_n = 50)
leaf_cmp <- merge(merge(key, tab_tr[, c("level", "n", "rate", "lo", "hi")], by = "level"),
                  tab_te[, c("level", "n", "rate", "lo", "hi")], by = "level",
                  suffixes = c("_train", "_test"))
leaf_cmp <- leaf_cmp[order(-leaf_cmp$rate_train), ]
leaf_cmp
cat("Q2 tree test AUC:", round(auc_of(test$claim_status, p_tree), 4), "\n")

# FIGURE 4: decision tree (node numbers shown)
save_fig("fig04_tree.png", {
  if (requireNamespace("rpart.plot", quietly = TRUE) && nrow(tree$frame) > 1) {
    rpart.plot::rpart.plot(tree, type = 4, extra = 101, nn = TRUE)
  } else {
    plot(tree, uniform = TRUE, margin = 0.1); text(tree, use.n = TRUE, cex = 0.6)
  }
}, w = 2600, h = 1600)
# FIGURE 5: claim rate by segment and by customer age band
save_fig("fig05_segment_age_rates.png", {
  par(mfrow = c(1, 2), mar = c(6, 4, 3, 1))
  plot_rates(rate_table(Motor, "segment"),  "By vehicle segment")
  plot_rates(rate_table(Motor, "age_band"), "By customer age band", ord = levels(Motor$age_band))
}, w = 2400, h = 1100)
# FIGURE 6: leaf claim rates, train vs test
save_fig("fig06_leaf_rates_train_test.png", {
  par(mar = c(5, 5, 3, 1))
  barplot(rbind(leaf_cmp$rate_train, leaf_cmp$rate_test), beside = TRUE, names.arg = leaf_cmp$node,
          col = c("grey60", "grey85"), xlab = "Tree node", ylab = "Claim rate",
          main = "Claim rate in each tree leaf: train vs test")
  abline(h = prob_claim, lty = 2)
  legend("topright", c("Train", "Test"), fill = c("grey60", "grey85"), bty = "n")
})

# =============================================================================
# Q3: Policy duration and claim probability
# =============================================================================
cat("Zero-length policies:", sum(Motor$subscription_length == 0),
    "| claims among them:", sum(Motor$claim_status[Motor$subscription_length == 0]), "\n")

train3 <- subset(train, subscription_length > 0)
test3  <- subset(test,  subscription_length > 0)
ctrl3  <- drop_aliased(c("vehicle_age", "customer_age", "airbags", "ncap_rating", "Engine_Size"),
                       train3)$vars

m3_lin  <- glm(fml("claim_status", c("subscription_length", ctrl3)),
               family = binomial, data = train3)
m3_ns   <- glm(fml("claim_status", c("ns(subscription_length, 3)", ctrl3)),
               family = binomial, data = train3)
m3_off  <- glm(fml("claim_status", c("offset(log(subscription_length))", ctrl3)),
               family = binomial(link = "cloglog"), data = train3)
m3_free <- glm(fml("claim_status", c("log(subscription_length)", ctrl3)),
               family = binomial(link = "cloglog"), data = train3)

summary(m3_lin)$coefficients["subscription_length", ]
summary(m3_free)$coefficients["log(subscription_length)", ]   # near 1 => claims scale with exposure
AIC(m3_lin, m3_ns, m3_off, m3_free)
anova(m3_lin, m3_ns, test = "Chisq")

grid <- data.frame(subscription_length = seq(0.1, 14, length.out = 50))
for (v in ctrl3) grid[[v]] <- median(train3[[v]])
obs <- rate_table(Motor, "sub_bin")
obs <- obs[match(levels(Motor$sub_bin), obs$level), ]
mid <- c(0, 1, 3, 5, 7, 9, 11, 13)

# FIGURE 7: observed claim rate by subscription length band
save_fig("fig07_duration_observed.png", {
  par(mar = c(5, 5, 3, 1))
  plot_rates(rate_table(Motor, "sub_bin"), "Claim rate by subscription length (years, 95% CI)",
             ord = levels(Motor$sub_bin), xlab = "Subscription length band")
})
# FIGURE 8: fitted probability of a claim vs duration (spline vs linear) with observed bands
save_fig("fig08_duration_fitted.png", {
  par(mar = c(5, 5, 3, 1))
  p_ns  <- predict(m3_ns,  grid, type = "response")
  p_lin <- predict(m3_lin, grid, type = "response")
  plot(grid$subscription_length, p_ns, type = "l", lwd = 2,
       ylim = c(0, max(p_ns, p_lin, obs$hi, na.rm = TRUE)),
       xlab = "Subscription length (years)", ylab = "Probability of a claim",
       main = "Fitted claim probability vs duration")
  lines(grid$subscription_length, p_lin, lty = 2, lwd = 2)
  points(mid, obs$rate, pch = 19)
  arrows(mid, obs$lo, mid, obs$hi, angle = 90, code = 3, length = 0.03)
  legend("topleft", c("Spline (3 df)", "Linear", "Observed band (unadjusted)"),
         lty = c(1, 2, NA), pch = c(NA, NA, 19), lwd = c(2, 2, NA), bty = "n")
})

# =============================================================================
# Q4: Individual risk score for premium differentiation
# =============================================================================
fit4 <- drop_aliased(c(safety_vars, engine_vars, "segment", "vehicle_age",
                       "customer_age", "region_density"), train)
m4 <- glm(fml("claim_status", c(fit4$vars, "ns(subscription_length, 3)")),
          family = binomial, data = train)
score_model <- m4   # <- to build calibration/bands from a candidate below, change this line
test$score  <- predict(score_model, test, type = "response")
cat("Q4 score model test AUC:", round(auc_of(test$claim_status, test$score), 4), "\n")

test$decile <- cut(test$score, quantile(test$score, 0:10 / 10),
                   include.lowest = TRUE, labels = 1:10)
calib <- aggregate(cbind(observed = as.numeric(claim_status), predicted = score) ~ decile,
                   data = test, FUN = mean)
calib$n    <- as.vector(table(test$decile))
calib$lift <- calib$observed / mean(test$claim_status)
calib

test$band <- cut(as.integer(test$decile), c(0, 3, 7, 9, 10),
                 labels = c("Low", "Medium", "High", "Very high"))
bands <- aggregate(as.numeric(claim_status) ~ band, data = test, FUN = mean)
names(bands)[2] <- "observed_rate"
bands$n <- as.vector(table(test$band))
bands$relativity <- bands$observed_rate / mean(test$claim_status)
bands

lin_terms <- c(fit4$vars, "subscription_length")
x_tr <- model.matrix(fml("", lin_terms), train)[, -1]
x_te <- model.matrix(fml("", lin_terms), test)[, -1]
cv   <- cv.glmnet(x_tr, train$y, family = "binomial", alpha = 1, type.measure = "auc")
p_en <- as.numeric(predict(cv, x_te, s = "lambda.1se", type = "response"))
cat("Q4 lasso test AUC:", round(auc_of(test$claim_status, p_en), 4), "\n")

# Candidate score models: smoother terms, interaction, and car-model instead of specs
smooth_terms <- function(vars, smooth = controls, df = 3)
  ifelse(vars %in% smooth, sprintf("ns(%s, %d)", vars, df), vars)
inter <- "ns(vehicle_age, 3):ns(subscription_length, 3)"

seg_vars <- drop_aliased(c("segment", "fuel_type", "vehicle_age", "customer_age",
                           "region_density", "subscription_length"), train)$vars
mod_vars <- c("model", "vehicle_age", "customer_age", "region_density", "subscription_length")
cand <- list(spec_based = m4)
cand$segment_splines    <- glm(fml("claim_status", smooth_terms(seg_vars)), family = binomial, data = train)
cand$segment_interact   <- glm(fml("claim_status", c(smooth_terms(seg_vars), inter)), family = binomial, data = train)
cand$model_splines      <- glm(fml("claim_status", smooth_terms(mod_vars)), family = binomial, data = train)
cand$model_interact     <- glm(fml("claim_status", c(smooth_terms(mod_vars), inter)), family = binomial, data = train)

cand_tab <- do.call(rbind, lapply(names(cand), function(nm) {
  cbind(data.frame(model = nm, AIC_train = AIC(cand[[nm]])), eval_glm(cand[[nm]]))
}))
cand_tab <- rbind(cand_tab,
                  data.frame(model = "lasso", AIC_train = NA, AUC = auc_of(test$claim_status, p_en),
                             McFadden_train = NA, McFadden_test = NA),
                  data.frame(model = "tree", AIC_train = NA, AUC = auc_of(test$claim_status, p_tree),
                             McFadden_train = NA, McFadden_test = NA))
cand_tab
alts     <- setdiff(names(cand), "spec_based")
best_alt <- alts[which.min(sapply(cand[alts], AIC))]
cat("Best alternative GLM by training AIC:", best_alt, "\n")

# FIGURE 9: calibration by decile
save_fig("fig09_calibration.png", {
  par(mar = c(5, 5, 3, 1))
  lim <- range(c(calib$predicted, calib$observed))
  plot(calib$predicted, calib$observed, pch = 19, xlim = lim, ylim = lim,
       xlab = "Mean predicted claim probability", ylab = "Observed claim rate",
       main = "Calibration by risk decile (test set)")
  abline(0, 1, lty = 2)
})
# FIGURE 10: lift by decile
save_fig("fig10_lift_deciles.png", {
  par(mar = c(5, 5, 3, 1))
  barplot(calib$lift, names.arg = calib$decile, col = "grey80", xlab = "Risk decile (1 = lowest)",
          ylab = "Lift (observed rate / portfolio rate)", main = "Lift by risk decile (test set)")
  abline(h = 1, lty = 2)
})
# FIGURE 11: risk band relativities
save_fig("fig11_risk_bands.png", {
  par(mar = c(5, 5, 3, 1))
  barplot(bands$relativity, names.arg = bands$band, col = "grey80", xlab = "Risk band",
          ylab = "Relativity to portfolio rate", main = "Premium relativities by risk band")
  abline(h = 1, lty = 2)
})
# FIGURE 12: ROC curves
save_fig("fig12_roc_comparison.png", {
  r1 <- roc(as.numeric(test$claim_status), as.numeric(test$score), quiet = TRUE)
  r2 <- roc(as.numeric(test$claim_status), as.numeric(predict(cand[[best_alt]], test, type = "response")), quiet = TRUE)
  r3 <- roc(as.numeric(test$claim_status), p_tree, quiet = TRUE)
  r4 <- roc(as.numeric(test$claim_status), p_en, quiet = TRUE)
  plot(r1, col = "black", legacy.axes = TRUE, main = "ROC curves (test set)")
  plot(r2, col = "blue", add = TRUE); plot(r3, col = "red", add = TRUE); plot(r4, col = "darkgreen", add = TRUE)
  legend("bottomright", bty = "n", lwd = 2, col = c("black", "blue", "red", "darkgreen"),
         legend = c(sprintf("GLM (score model) %.3f", auc(r1)),
                    sprintf("%s %.3f", best_alt, auc(r2)),
                    sprintf("Tree %.3f", auc(r3)), sprintf("Lasso %.3f", auc(r4))))
})

# =============================================================================
# Q5 (refined): Which groups of vehicle characteristics carry the most information?
# -----------------------------------------------------------------------------
# Same constraint as Q1, so importance is assessed three ways:
#  (a) group of variables: deviance and test AUC added beyond policy controls
#  (b) each variable on its own (ranked by AIC gain), Holm-adjusted p-values
#  (c) random forest permutation importance (shares credit across correlated variables)
# =============================================================================
all_veh <- keep(c(safety_vars, "Engine_Size", "max_power", "max_torque", "cylinder", "fuel_type",
                  "gross_weight", "length", "width", "turning_radius", "segment", "engine_type",
                  "transmission_type", "steering_type", "rear_brakes_type"))

groups <- list(safety = safety_vars,
               engine = keep(c("Engine_Size", "max_power", "max_torque", "cylinder", "fuel_type")),
               size   = keep(c("gross_weight", "length", "width", "turning_radius")),
               body   = keep(c("segment", "transmission_type", "steering_type", "rear_brakes_type")))
q5_groups <- group_table(groups)
q5_groups

q5_marg <- marg_table(all_veh)
q5_marg

rf_vars <- c(all_veh, controls)
rf_dat  <- train[, rf_vars]
rf_dat[] <- lapply(rf_dat, function(x) if (is.logical(x)) factor(x) else x)
rf_dat$y <- factor(train$claim_status)
rf <- ranger(y ~ ., data = rf_dat, num.trees = 300, min.node.size = 50,
             importance = "permutation", probability = TRUE, seed = 061024)
rf_imp <- sort(rf$variable.importance, decreasing = TRUE)
rf_imp

# FIGURE 13: deviance added by each vehicle group
save_fig("fig13_group_deviance.png", {
  par(mar = c(5, 5, 3, 1))
  barplot(q5_groups$dev_drop, names.arg = sprintf("%s\n(%d df)", q5_groups$group, q5_groups$df),
          col = "grey80", ylab = "Deviance reduction vs controls-only model",
          main = "Information added by each group of vehicle variables")
})
# FIGURE 14: random forest permutation importance
save_fig("fig14_rf_importance.png", {
  par(mar = c(4, 11, 3, 1))
  barplot(rev(rf_imp), horiz = TRUE, las = 1, col = "grey80", xlab = "Permutation importance",
          main = "Random forest variable importance")
})

# =============================================================================
# Q6: Regional factors vs vehicle characteristics
# =============================================================================
veh_priority <- keep(c(safety_vars, engine_vars, "vehicle_age", "segment", "max_torque",
                       "gross_weight", "engine_type", "transmission_type",
                       "steering_type", "rear_brakes_type"))
veh_vars <- drop_aliased(veh_priority, train)$vars
veh_vars

num_veh <- veh_vars[sapply(train[veh_vars], is.numeric)]
mu  <- sapply(train[num_veh], mean)
sdv <- sapply(train[num_veh], sd)
train_s <- train; test_s <- test
for (v in num_veh) {
  train_s[[v]] <- (train[[v]] - mu[v]) / sdv[v]
  test_s[[v]]  <- (test[[v]]  - mu[v]) / sdv[v]
}

m6_reg   <- glm(claim_status ~ region_code, family = binomial, data = train)
m6_dens  <- glm(claim_status ~ log(region_density), family = binomial, data = train)
m6_veh   <- glm(fml("claim_status", veh_vars), family = binomial, data = train)
m6_both  <- glm(fml("claim_status", c(veh_vars, "region_code")), family = binomial, data = train)
# More stable representation of "vehicle": the car model itself (all specs at once)
m6_vmod  <- glm(claim_status ~ model + vehicle_age, family = binomial, data = train)
m6_vmreg <- glm(claim_status ~ model + vehicle_age + region_code, family = binomial, data = train)

res6 <- rbind(region_code      = eval_glm(m6_reg),
              region_density   = eval_glm(m6_dens),
              vehicle_only     = eval_glm(m6_veh),
              vehicle_region   = eval_glm(m6_both),
              carmodel_only    = eval_glm(m6_vmod),
              carmodel_region  = eval_glm(m6_vmreg))
res6
anova(m6_veh,  m6_both,  test = "Chisq")
anova(m6_vmod, m6_vmreg, test = "Chisq")

m6_null <- glmer(y ~ 1 + (1 | region_code), family = binomial, data = train_s, nAGQ = 0,
                 control = glmerControl(optimizer = "bobyqa"))
m6_mix  <- glmer(fml("y", c(veh_vars, "(1 | region_code)")), family = binomial,
                 data = train_s, nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
var_null <- as.numeric(VarCorr(m6_null)$region_code[1])
var_mix  <- as.numeric(VarCorr(m6_mix)$region_code[1])
cat("ICC (region only):", round(var_null / (var_null + pi^2 / 3), 4),
    "| ICC after vehicle vars:", round(var_mix / (var_mix + pi^2 / 3), 4),
    "| Share of regional variance explained by vehicle mix:", round(1 - var_mix / var_null, 3), "\n")

reg_tab <- rate_table(Motor, "region_code")
reg_tab

# FIGURE 15: claim rate by region
save_fig("fig15_region_rates.png", {
  par(mar = c(5, 5, 3, 1))
  plot_rates(reg_tab, "Claim rate by region (95% CI)", xlab = "Region code")
}, w = 2200)
# FIGURE 16: region vs vehicle on the test set
save_fig("fig16_q6_model_comparison.png", {
  par(mfrow = c(1, 2), mar = c(9, 4, 3, 1))
  barplot(res6$AUC, names.arg = rownames(res6), las = 2, col = "grey80", ylim = c(0.5, max(res6$AUC) * 1.05),
          xpd = FALSE, ylab = "Test AUC", main = "Discrimination")
  barplot(res6$McFadden_test, names.arg = rownames(res6), las = 2, col = "grey80",
          ylab = "Test McFadden R-squared", main = "Explained deviance")
}, w = 2400, h = 1300)


# =============================================================================
# E1 and E2 (exploratory): performance ceiling and the vehicle age x duration interaction
# -----------------------------------------------------------------------------
# E1: does a gradient boosted model materially beat the GLMs? If boosting only reaches
#     a similar AUC, the limit is the data, not the GLM specification.
# E2: is the vehicle age x duration interaction material?
# Method: repeated stratified 5-fold cross-validation on the full data, with DeLong
# confidence intervals and paired DeLong tests on out-of-fold predictions (repeat 1).
# (To use xgboost instead of gbm, replace fit_boost() and pred_boost() below.)
# =============================================================================
K <- 5; R_rep <- 3
y_all <- Motor$y

boost_vars <- unique(c(controls, "region_density", all_veh, "region_code"))
bdat <- Motor[, boost_vars]
bdat[] <- lapply(bdat, function(x) if (is.logical(x)) as.numeric(x) else x)
bdat$y <- Motor$y
n_trees <- 300
fit_boost <- function(d) gbm(y ~ ., data = d, distribution = "bernoulli", n.trees = n_trees,
                             interaction.depth = 3, shrinkage = 0.05, bag.fraction = 0.8,
                             n.minobsinnode = 50, verbose = FALSE)
pred_boost <- function(m, d) predict(m, newdata = d, n.trees = n_trees, type = "response")

model_names <- c("controls_glm", "spec_glm", "model_splines_glm", "model_interact_glm", "boosted")
cv_formulas <- list(controls_glm       = fml("claim_status", controls),
                    spec_glm           = formula(m4),
                    model_splines_glm  = formula(cand$model_splines),
                    model_interact_glm = formula(cand$model_interact))

make_folds <- function(y, K) {
  f <- integer(length(y))
  for (cl in unique(y)) { i <- which(y == cl); f[i] <- sample(rep(seq_len(K), length.out = length(i))) }
  f
}

oof <- array(NA_real_, dim = c(n_obs, R_rep, length(model_names)),
             dimnames = list(NULL, NULL, model_names))
fold_auc <- data.frame()
for (r in seq_len(R_rep)) {
  folds <- make_folds(y_all, K)
  for (k in seq_len(K)) {
    tr <- folds != k; te <- !tr
    for (nm in model_names) {
      p <- if (nm == "boosted") {
        pred_boost(fit_boost(bdat[tr, ]), bdat[te, ])
      } else {
        as.numeric(predict(glm(cv_formulas[[nm]], family = binomial, data = Motor[tr, ]),
                           Motor[te, ], type = "response"))
      }
      oof[te, r, nm] <- p
      fold_auc <- rbind(fold_auc, data.frame(model = nm, rep_id = r, fold = k,
                                             AUC = auc_of(y_all[te], p)))
    }
  }
  cat("Finished CV repeat", r, "of", R_rep, "\n")
}

ro_list <- lapply(model_names, function(nm) roc(y_all, oof[, 1, nm], quiet = TRUE))
names(ro_list) <- model_names
cv_summary <- do.call(rbind, lapply(model_names, function(nm) {
  a  <- fold_auc$AUC[fold_auc$model == nm]
  ci <- as.numeric(ci.auc(ro_list[[nm]]))          # DeLong: lower, AUC, upper
  data.frame(model = nm, AUC_cv_mean = mean(a), AUC_fold_sd = sd(a),
             AUC_oof = ci[2], AUC_lo = ci[1], AUC_hi = ci[3],
             Gini = 2 * ci[2] - 1, Gini_lo = 2 * ci[1] - 1, Gini_hi = 2 * ci[3] - 1)
}))
cv_summary

cmp <- function(a, b) {
  t <- roc.test(ro_list[[a]], ro_list[[b]], paired = TRUE, method = "delong")
  data.frame(comparison = paste(b, "vs", a),
             AUC_diff = as.numeric(auc(ro_list[[b]])) - as.numeric(auc(ro_list[[a]])),
             p = t$p.value)
}
cv_tests <- rbind(cmp("spec_glm", "model_splines_glm"),         # splines + car model vs specs
                  cmp("model_splines_glm", "model_interact_glm"), # E2: interaction
                  cmp("model_interact_glm", "boosted"),           # E1: boosted vs best GLM
                  cmp("spec_glm", "boosted"))
cv_tests

# E2 in-sample check on the training set
anova(cand$model_splines, cand$model_interact, test = "Chisq")

# Which variables does the boosted model use? (fitted on the training rows)
m_boost <- fit_boost(bdat[idx, ])
boost_imp <- summary(m_boost, plotit = FALSE, n.trees = n_trees)
head(boost_imp, 15)

# FIGURE 17: cross-validated AUC by model (15 folds each)
save_fig("fig17_cv_auc_comparison.png", {
  par(mar = c(9, 5, 3, 1))
  boxplot(AUC ~ model, data = fold_auc, las = 2, col = "grey85", xlab = "",
          ylab = "AUC per fold", main = "Repeated 5-fold cross-validation (3 repeats)")
}, w = 1800, h = 1300)
# FIGURE 18: boosted model variable importance
save_fig("fig18_boosted_importance.png", {
  par(mar = c(4, 11, 3, 1))
  top <- head(boost_imp, 15)
  barplot(rev(top$rel.inf), names.arg = rev(as.character(top$var)), horiz = TRUE, las = 1,
          col = "grey80", xlab = "Relative influence (%)", main = "Boosted model: top variables")
})

# ---- Save tables for the report --------------------------------------------
write.csv(q1_tab,       "tables/q1_vehicle_model_table.csv", row.names = FALSE)
write.csv(q1_marg,      "tables/q1_marginal_effects.csv",    row.names = FALSE)
write.csv(q1_group_tab, "tables/q1_group_table.csv",         row.names = FALSE)
write.csv(leaf_cmp,     "tables/q2_leaf_rates.csv",          row.names = FALSE)
write.csv(cand_tab,     "tables/q4_model_comparison.csv",    row.names = FALSE)
write.csv(calib,        "tables/q4_calibration.csv",         row.names = FALSE)
write.csv(bands,        "tables/q4_risk_bands.csv",          row.names = FALSE)
write.csv(q5_groups,    "tables/q5_group_importance.csv",    row.names = FALSE)
write.csv(q5_marg,      "tables/q5_variable_ranking.csv",    row.names = FALSE)
write.csv(data.frame(variable = names(rf_imp), importance = as.numeric(rf_imp)),
          "tables/q5_rf_importance.csv", row.names = FALSE)
write.csv(cbind(model = rownames(res6), res6), "tables/q6_model_comparison.csv", row.names = FALSE)
write.csv(reg_tab,      "tables/q6_region_rates.csv",        row.names = FALSE)
write.csv(cv_summary,   "tables/e1_cv_summary.csv",          row.names = FALSE)
write.csv(cv_tests,     "tables/e1_e2_paired_tests.csv",     row.names = FALSE)
write.csv(fold_auc,     "tables/e1_fold_auc.csv",            row.names = FALSE)
write.csv(boost_imp,    "tables/e1_boosted_importance.csv",  row.names = FALSE)

sessionInfo()