import csv
import os
import sys
import tarfile

import zstandard as zstd
# 
DATA_DIR = "C:/bigearthnet"
ARCHIVE = os.path.join(DATA_DIR, "BigEarthNet-S2.tar.zst")
OUT_DIR = os.path.join(DATA_DIR, "subset")
IDS_CSV = "outputs/sampled_patch_ids.csv"
PEEK = len(sys.argv) > 1 and sys.argv[1] == "peek"
# 
with open(IDS_CSV, newline="") as f:
    wanted = {row["patch_id"] for row in csv.DictReader(f)}
print(f"{len(wanted)} patches wanted", flush=True)

os.makedirs(OUT_DIR, exist_ok=True)
found, n_files = set(), 0

with open(ARCHIVE, "rb") as fh:
    reader = zstd.ZstdDecompressor().stream_reader(fh, read_across_frames=True)
    with tarfile.open(fileobj=reader, mode="r|") as tar:
        for i, member in enumerate(tar):
            if PEEK:
                print(member.name)
                if i >= 30:
                    break
                continue
            if i % 200000 == 0:
                print(f"{i} members scanned, {len(found)} patches found", flush=True)
            if not member.isfile():
                continue
            parts = member.name.split("/")
            if len(parts) < 2 or parts[-2] not in wanted:
                continue
            dest = os.path.join(OUT_DIR, parts[-2])
            os.makedirs(dest, exist_ok=True)
            with tar.extractfile(member) as src, open(os.path.join(dest, parts[-1]), "wb") as out:
                out.write(src.read())
            found.add(parts[-2])
            n_files += 1

if not PEEK:
    print(f"done: {len(found)}/{len(wanted)} patches, {n_files} files")
    missing = sorted(wanted - found)
    if missing:
        print("missing (first 10):", missing[:10])
