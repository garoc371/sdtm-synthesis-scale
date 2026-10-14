#!/bin/sh
# Generate, check and score one data folder.
#   bench/eval/run.sh <label> [data folder]      (default folder: data/pilot)
# Writes bench/out/eval/<label>_*; prints failing relations (excluding those the real data,
# as the app reads it, also fails), SDTMIG conformance against the real data's, and marginal
# fidelity. SYNTH_MODEL=tree for trees.
set -eu
E=$(cd "$(dirname "$0")" && pwd)
L=$1
export DATA=${2:-$E/../../data/pilot}
O=${OUT:-$E/../out/eval}
mkdir -p "$O"
Rscript "$E/gen.R" "real_$L" 0 1
Rscript "$E/check.R" "real_$L" "$O/real_${L}_seed1.rds"
Rscript "$E/gen.R" "$L" "${N:-254}" 1 2 3 > "$O/gen_$L.log" 2>&1
Rscript "$E/check.R" "$L" "$O/${L}_seed1.rds" "$O/${L}_seed2.rds" "$O/${L}_seed3.rds"
Rscript "$E/coherence.R" "$L" "$O/${L}_seed1.rds" "$O/${L}_seed2.rds" "$O/${L}_seed3.rds"
Rscript "$E/fidelity.R" "$L" "$O/${L}_seed1.rds" "$O/${L}_seed2.rds" "$O/${L}_seed3.rds" | sed -n 1p
REF="real_$L" SHOW=${SHOW:-0} Rscript "$E/summary.R" "$L"
