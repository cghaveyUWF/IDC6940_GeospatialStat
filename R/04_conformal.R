p <- as.matrix(read.csv("outputs/test_probs.csv",  check.names = FALSE))
y <- as.matrix(read.csv("outputs/test_labels.csv", check.names = FALSE))
classes <- colnames(p)
stopifnot(identical(classes, colnames(y)))
counts  <- read.csv("outputs/class_counts.csv", check.names = FALSE)
train_n <- counts$Train[match(classes, counts$Class)]
n <- nrow(p)

run_split <- function(seed, alpha) {
  set.seed(seed)
  cal <- sample(n, floor(n / 2))
  p_cal <- p[cal, ];  y_cal <- y[cal, ]
  p_ev  <- p[-cal, ]; y_ev  <- y[-cal, ]
  qhat <- sapply(seq_along(classes), function(k) {
    s <- 1 - p_cal[y_cal[, k] == 1, k]
    r <- ceiling((length(s) + 1) * (1 - alpha))
    if (r > length(s)) 1 else sort(s)[r]
  })
  sets <- sweep(1 - p_ev, 2, qhat, "<=")
  data.frame(
    rep = seed, alpha = alpha, class = classes, train_n = train_n,
    n_cal      = colSums(y_cal),
    min_prob   = 1 - qhat,
    coverage   = colSums(sets & y_ev == 1) / colSums(y_ev),
    false_incl = colSums(sets & y_ev == 0) / colSums(y_ev == 0),
    set_size   = mean(rowSums(sets))
  )
}

# 100 random splits x 3 coverage levels
res <- do.call(rbind, lapply(1:100, function(s)
  do.call(rbind, lapply(c(0.10, 0.05, 0.20), function(a) run_split(s, a)))))
write.csv(res, "outputs/conformal_reps.csv", row.names = FALSE)

# Average over splits, per class and coverage level
summ <- aggregate(cbind(n_cal, min_prob, coverage, false_incl) ~ class + train_n + alpha,
                  data = res, FUN = mean)
summ$coverage_sd <- aggregate(coverage ~ class + alpha, data = res, FUN = sd)$coverage[
  match(paste(summ$class, summ$alpha),
        paste(aggregate(coverage ~ class + alpha, data = res, FUN = sd)$class,
              aggregate(coverage ~ class + alpha, data = res, FUN = sd)$alpha))]
write.csv(summ, "outputs/conformal_summary.csv", row.names = FALSE)

# F1 on the full test partition (F1 does not use calibration)
pred <- p >= 0.5
tp <- colSums(pred & y == 1); fp <- colSums(pred & y == 0); fn <- colSums(!pred & y == 1)
f1_class <- 2 * tp / (2 * tp + fp + fn)
write.csv(data.frame(class = classes, f1 = f1_class), "outputs/f1_by_class.csv", row.names = FALSE)
cat("Macro F1:", round(mean(f1_class), 3),
    " Micro F1:", round(2 * sum(tp) / (2 * sum(tp) + sum(fp) + sum(fn)), 3), "\n")

# Overall set size by coverage level
print(aggregate(set_size ~ alpha, data = res[!duplicated(res[c("rep", "alpha")]), ], FUN = mean))

# Primary results at 90%, rarest class first
s90 <- subset(summ, alpha == 0.10)
s90 <- s90[order(s90$train_n), ]
print(s90[, c("class", "train_n", "n_cal", "min_prob", "coverage", "coverage_sd", "false_incl")],
      row.names = FALSE, digits = 3)

# Spearman tests at 90%
for (v in c("min_prob", "false_incl", "coverage", "coverage_sd")) {
  ct <- cor.test(s90$train_n, s90[[v]], method = "spearman", exact = FALSE)
  cat(sprintf("train_n vs %s: rho = %.2f, p = %.3f\n", v, ct$estimate, ct$p.value))
}