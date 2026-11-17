# 03_train_cnn.R: Train the CNN and save its test-set probabilities

# Setup
reticulate::use_virtualenv("C:/Users/Colin/venvs/r-reticulate", required = TRUE)
library(keras3)

OUT_DIR <- "C:/bigearthnet/arrays"
MODEL_DIR <- "C:/bigearthnet/model"
dir.create(MODEL_DIR, showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)
set_random_seed(12345)

# Load data
load_split <- function(s) list(
  x = readRDS(file.path(OUT_DIR, paste0(s, "_x.rds"))), # Training images
  y = readRDS(file.path(OUT_DIR, paste0(s, "_y.rds"))) # Label matrix
)
tr <- load_split("train")
va <- load_split("validation")
classes <- readRDS(file.path(OUT_DIR, "classes.rds"))

# Mean and SD of each band (dimension 4), from training data only
mu <- apply(tr$x, 4, mean)
sdv <- apply(tr$x, 4, sd)

# Convolution -> batch normalization -> ReLU -> 2 x 2 max pooling (halves the size)
conv_block <- function(x, filters) {
  x |>
    layer_conv_2d(filters = filters, kernel_size = 3, padding = "same") |>
    layer_batch_normalization() |>
    layer_activation("relu") |>
    layer_max_pooling_2d(pool_size = 2)
}

# Standardize bands -> 4 blocks (120 -> 60 -> 30 -> 15 -> 7 px) -> pool -> dropout
# -> 19 sigmoid outputs (independent probabilities, since a patch can have several classes)
input <- keras_input(shape = c(120, 120, 12))
output <- input |>
  layer_normalization(mean = mu, variance = sdv^2) |>
  conv_block(32) |> conv_block(64) |> conv_block(128) |> conv_block(256) |>
  layer_global_average_pooling_2d() |>
  layer_dropout(rate = 0.3) |>
  layer_dense(units = 19, activation = "sigmoid")
model <- keras_model(input, output)

# Binary cross-entropy (multi-label loss), Adam optimizer, AUC to monitor progress
model |> compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "binary_crossentropy",
  metrics = list(metric_auc(multi_label = TRUE, name = "auc"))
)
print(model)

# Stop when validation loss hasn't improved for 5 epochs; keep the best epoch's weights
history <- model |> fit(
  tr$x, tr$y,
  validation_data = list(va$x, va$y),
  epochs = 50, batch_size = 32,
  callbacks = list(callback_early_stopping(
    monitor = "val_loss", patience = 5, restore_best_weights = TRUE
  ))
)

# Save
save_model(model, file.path(MODEL_DIR, "cnn.keras"), overwrite = TRUE)
saveRDS(list(mu = mu, sd = sdv), file.path(MODEL_DIR, "norm_stats.rds"))
write.csv(as.data.frame(history), "outputs/training_history.csv", row.names = FALSE)

# Test probabilities and labels: all the conformal step needs
te <- load_split("test")
p_test <- predict(model, te$x, batch_size = 64)
colnames(p_test) <- classes
write.csv(p_test, "outputs/test_probs.csv", row.names = FALSE)
write.csv(te$y,   "outputs/test_labels.csv", row.names = FALSE)
message("Training complete.")
