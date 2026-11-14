library(terra)
library(dplyr)

DATA_DIR   <- "C:/bigearthnet"
SUBSET_DIR <- file.path(DATA_DIR, "subset")
OUT_DIR    <- file.path(DATA_DIR, "arrays")
dir.create(OUT_DIR, showWarnings = FALSE)

BANDS <- c("B01","B02","B03","B04","B05","B06","B07","B08","B8A","B09","B11","B12")

sampled <- readRDS(file.path(DATA_DIR, "sampled_patches.rds"))
classes <- sort(unique(unlist(sampled$labels)))
stopifnot(length(classes) == 19)

# Read one patch: 12 bands, all resampled to the 120x120 10 m grid
read_patch <- function(pid) {
  f <- file.path(SUBSET_DIR, pid, paste0(pid, "_", BANDS, ".tif"))
  template <- rast(f[BANDS == "B02"])
  out <- array(0L, dim = c(120, 120, 12))
  for (b in seq_along(BANDS)) {
    r <- rast(f[b])
    if (nrow(r) != 120 || ncol(r) != 120) {
      r <- resample(r, template, method = "bilinear")
    }
    out[, , b] <- as.integer(round(as.matrix(r, wide = TRUE)))
  }
  out
}

for (s in c("train", "validation", "test")) {
  d <- filter(sampled, split == s)
  n <- nrow(d)
  x <- array(0L, dim = c(n, 120, 120, 12))
  for (i in seq_len(n)) {
    x[i, , , ] <- read_patch(d$patch_id[i])
    if (i %% 200 == 0) message(s, ": ", i, "/", n)
  }
  y <- t(sapply(d$labels, function(l) as.integer(classes %in% l)))
  colnames(y) <- classes
  
  saveRDS(x, file.path(OUT_DIR, paste0(s, "_x.rds")), compress = FALSE)
  saveRDS(y, file.path(OUT_DIR, paste0(s, "_y.rds")))
  saveRDS(d$patch_id, file.path(OUT_DIR, paste0(s, "_ids.rds")))
  message(s, " done: ", n, " patches")
}
saveRDS(classes, file.path(OUT_DIR, "classes.rds"))