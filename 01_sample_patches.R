library(arrow)
library(dplyr)
library(tidyr)

DATA_DIR <- "C:/bigearthnet"   # Local folder containing metadata.parquet
set.seed(12345)

meta <- read_parquet(file.path(DATA_DIR, "metadata.parquet"))

# Check the real split names and the quality flags before sampling
print(table(meta$split))
print(table(meta$contains_seasonal_snow, meta$contains_cloud_or_shadow))

meta_long <- meta |>
  select(patch_id, split, labels) |>
  unnest(labels)

# Up to n patches per class per split (smaller groups are returned whole)
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

# Attach the full label list to each sampled patch
sampled <- sampled |>
  left_join(select(meta, patch_id, labels), by = "patch_id")

# Per-class counts per split, to check rare classes survived
print(
  sampled |> unnest(labels) |> count(split, labels) |>
    pivot_wider(names_from = split, values_from = n)
)

dir.create("outputs", showWarnings = FALSE)
saveRDS(sampled, file.path(DATA_DIR, "sampled_patches.rds"))
write.csv(select(sampled, patch_id, split), "outputs/sampled_patch_ids.csv",
          row.names = FALSE)