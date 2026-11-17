# 02_build_arrays.R: Stack each patch's 12 bands into arrays for training

library(terra)
library(dplyr)

DATA_DIR <- "C:/bigearthnet"
SUBSET_DIR <- file.path(DATA_DIR, "subset")
OUT_DIR <- file.path(DATA_DIR, "arrays")
dir.create(OUT_DIR, showWarnings = FALSE)

# The 12 BigEarthNet bands (cirrus band B10 is excluded)
BANDS <- c("B01","B02","B03","B04","B05","B06","B07","B08","B8A","B09","B11","B12")

# Sampled patches and the 19 class names in alphabetical (column) order
sampled <- readRDS(file.path(DATA_DIR, "sampled_patches.rds"))
classes <- sort(unique(unlist(sampled$labels)))
stopifnot(length(classes) == 19) # Stop if any of the 19 classes is missing from the sample
 
# Read one patch: 12 bands, all resampled to the 120x120 10 m grid
read_patch <- function(pid) {
  f <- file.path(SUBSET_DIR, pid, paste0(pid, "_", BANDS, ".tif"))
  template <- rast(f[BANDS == "B02"]) # Uses B02 (blue) as template, since it's 120 x 120
  out <- array(0L, dim = c(120, 120, 12)) # Empty 120 x 120 x 12 integer array
  for (b in seq_along(BANDS)) {
    r <- rast(f[b])
    # Resample 20 m (60 x 60) and 60 m (20 x 20) bands onto the 120 x 120 grid
    if (nrow(r) != 120 || ncol(r) != 120) {
      r <- resample(r, template, method = "bilinear")
    }
    out[, , b] <- as.integer(round(as.matrix(r, wide = TRUE)))
  }
  out
}

# Build and save arrays for each split
for (s in c("train", "validation", "test")) {
  d <- filter(sampled, split == s)
  n <- nrow(d)

  # Images: n patches x 120 x 120 x 12 bands
  x <- array(0L, dim = c(n, 120, 120, 12))
  for (i in seq_len(n)) {
    x[i, , , ] <- read_patch(d$patch_id[i])
    if (i %% 200 == 0) message(s, ": ", i, "/", n)   # Progress every 200 patches
  }

  # Labels: n x 19 matrix, 1 if the class is present in the patch, 0 if not
  y <- t(sapply(d$labels, function(l) as.integer(classes %in% l)))
  colnames(y) <- classes

  # Row i of x, y, and the IDs all refer to the same patch
  saveRDS(x, file.path(OUT_DIR, paste0(s, "_x.rds")), compress = FALSE)
  saveRDS(y, file.path(OUT_DIR, paste0(s, "_y.rds")))
  saveRDS(d$patch_id, file.path(OUT_DIR, paste0(s, "_ids.rds")))
  message(s, " done: ", n, " patches")
}

# Class order, so later scripts know which column is which class
saveRDS(classes, file.path(OUT_DIR, "classes.rds"))