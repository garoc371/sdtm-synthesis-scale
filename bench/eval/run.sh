#!/bin/sh
# Generate and score one data folder: the generator is built on a random half of the subjects
# (members) and the other half is held out.
#   bench/eval/run.sh <label> [data folder]      (default folder: data/pilot)
# Writes bench/out/eval/<label>_*; prints whether the first seed generated again is identical,
# the SDTM rules the synthetic data break (of those the members' real data keep), marginal
# fidelity and privacy. N sets the synthetic subjects per seed (default: one per member),
# SPLIT the half, SHOW=1 also lists the rules the real data break. SYNTH_MODEL=tree for trees.
set -eu
E=$(cd "$(dirname "$0")" && pwd)
L=$1
export DATA=${2:-$E/../../data/pilot}
O=${OUT:-$E/../out/eval}
mkdir -p "$O"
S="$O/${L}_seed1.rds $O/${L}_seed2.rds $O/${L}_seed3.rds"
Rscript "$E/gen.R" "real_$L" 0 1
Rscript "$E/gen.R" "$L" "${N:-members}" 1 2 3 > "$O/gen_$L.log" 2>&1
grep "generated again" "$O/gen_$L.log"
Rscript "$E/rules.R" "$L" $S
Rscript "$E/fidelity.R" "$L" $S | sed -n 1p
Rscript "$E/privacy.R" "$L" $S
