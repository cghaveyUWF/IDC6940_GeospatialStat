reticulate::use_virtualenv("C:/Users/Colin/venvs/r-reticulate", required = TRUE)
library(keras3)

OUT_DIR   <- "C:/bigearthnet/arrays"
MODEL_DIR <- "C:/bigearthnet/model"
dir.create(MODEL_DIR, showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)
set_random_seed(12345)

load_split <- function(s) list(
  x = readRDS(file.path(OUT_DIR, paste0(s, "_x.rds"))),
  y = readRDS(file.path(OUT_DIR, paste0(s, "_y.rds")))
)
tr <- load_split("train")
va <- load_split("validation")
classes <- readRDS(file.path(OUT_DIR, "classes.rds"))

# Per-band mean and SD from the training subset only
mu  <- apply(tr$x, 4, mean)
sdv <- apply(tr$x, 4, sd)

# CNN: four conv blocks (32-64-128-256), global average pooling, dropout, 19 sigmoid outputs
conv_block <- function(x, filters) {
  x |>
    layer_conv_2d(filters = filters, kernel_size = 3, padding = "same") |>
    layer_batch_normalization() |>
    layer_activation("relu") |>
    layer_max_pooling_2d(pool_size = 2)
}
input <- keras_input(shape = c(120, 120, 12))
output <- input |>
  layer_normalization(mean = mu, variance = sdv^2) |>
  conv_block(32) |> conv_block(64) |> conv_block(128) |> conv_block(256) |>
  layer_global_average_pooling_2d() |>
  layer_dropout(rate = 0.3) |>
  layer_dense(units = 19, activation = "sigmoid")
model <- keras_model(input, output)

model |> compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "binary_crossentropy",
  metrics = list(metric_auc(multi_label = TRUE, name = "auc"))
)
print(model)  # shows the parameter count

history <- model |> fit(
  tr$x, tr$y,
  validation_data = list(va$x, va$y),
  epochs = 50, batch_size = 32,
  callbacks = list(callback_early_stopping(
    monitor = "val_loss", patience = 5, restore_best_weights = TRUE
  ))
)

# Save model, normalization stats, and training history
save_model(model, file.path(MODEL_DIR, "cnn.keras"), overwrite = TRUE)
saveRDS(list(mu = mu, sd = sdv), file.path(MODEL_DIR, "norm_stats.rds"))
write.csv(as.data.frame(history), "outputs/training_history.csv", row.names = FALSE)

# Predict on the test partition for the conformal step
te <- load_split("test")
p_test <- predict(model, te$x, batch_size = 64)
colnames(p_test) <- classes
write.csv(p_test, "outputs/test_probs.csv", row.names = FALSE)
write.csv(te$y,   "outputs/test_labels.csv", row.names = FALSE)
message("Training complete.")