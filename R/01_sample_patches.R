# 01_sample_patches.R: Draw a stratified subset of BigEarthNet v2.0 patches

library(arrow)
library(dplyr)
library(tidyr)

DATA_DIR <- "C:/bigearthnet" # Local folder containing metadata.parquet
set.seed(12345)

# Load metadata
meta <- read_parquet(file.path(DATA_DIR, "metadata.parquet"))

print(table(meta$split))
print(table(meta$contains_seasonal_snow, meta$contains_cloud_or_shadow))

# One row per (patch, label) pair
meta_long <- meta |>
  select(patch_id, split, labels) |>
  unnest(labels)

# Stratified sampling
sample_split <- function(split_name, n) {
  meta_long |>
    filter(split == split_name) |>
    group_by(labels) |>
    slice_sample(n = n) |>
    ungroup() |>
    distinct(patch_id) |>
    mutate(split = split_name)
}

sampled <- bind_rows(
  sample_split("train",      500),
  sample_split("validation", 100),
  sample_split("test",       400)
)

print(table(sampled$split))

# Re-attach labels
sampled <- sampled |>
  left_join(select(meta, patch_id, labels), by = "patch_id")

# Per-class counts per split
print(
  sampled |> unnest(labels) |> count(split, labels) |>
    pivot_wider(names_from = split, values_from = n)
)

# Save the sample
dir.create("outputs", showWarnings = FALSE)
saveRDS(sampled, file.path(DATA_DIR, "sampled_patches.rds"))
write.csv(select(sampled, patch_id, split), "outputs/sampled_patch_ids.csv",
          row.names = FALSE)

# Save per-class counts
sampled <- readRDS("C:/bigearthnet/sampled_patches.rds")
counts <- sampled |> unnest(labels) |> count(split, labels) |>
  pivot_wider(names_from = split, values_from = n) |>
  select(Class = labels, Train = train, Validation = validation, Test = test)
write.csv(counts, "outputs/class_counts.csv", row.names = FALSE)
print(counts, n = 19)
