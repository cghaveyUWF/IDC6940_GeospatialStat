# extract_subset.py: Extract only the sampled patches from the BigEarthNet archive

import csv
import os
import tarfile

import zstandard as zstd

DATA_DIR = "C:/bigearthnet"
ARCHIVE = os.path.join(DATA_DIR, "BigEarthNet-S2.tar.zst") # Compressed archive
OUT_DIR = os.path.join(DATA_DIR, "subset") # Where patches are written

# Patch IDs chosen by 01_sample_patches.R
with open("outputs/sampled_patch_ids.csv", newline="") as f:
    wanted = {row["patch_id"] for row in csv.DictReader(f)}

# Stream through the archive once and write only the wanted patches' files
n_found = 0
with open(ARCHIVE, "rb") as fh:
    stream = zstd.ZstdDecompressor().stream_reader(fh, read_across_frames=True)
    with tarfile.open(fileobj=stream, mode="r|") as tar:
        for member in tar:
            parts = member.name.split("/")  # BigEarthNet-S2/<tile>/<patch_id>/<band>.tif
            if member.isfile() and len(parts) >= 2 and parts[-2] in wanted:
                dest = os.path.join(OUT_DIR, parts[-2])
                os.makedirs(dest, exist_ok=True)
                with tar.extractfile(member) as src, open(os.path.join(dest, parts[-1]), "wb") as out:
                    out.write(src.read())
                n_found += 1

print(f"Done: {n_found} files written")
