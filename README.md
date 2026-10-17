# Synthetic SDTM

A Shiny app that reads the SDTM datasets of a clinical study and writes synthetic datasets with the same structure: the same domains, variables, code lists and links between records. A synthetic subject has a DM record, visits in SV, elements in SE, adverse events, lab results and so on, and the records agree with each other as SDTM requires.

User guide (using the app, how data are made, checks, privacy, known limitations): https://claude.ai/artifact/D94MY9WjojMpcFsyzzFSHN

## Quick start

```r
shiny::runApp()
```

Run from the repository root. `global.R` lists the packages and stops with the `install.packages()` call for any that are missing. Nothing is sent to an outside service (the graph renderer and the font are in `www/`).

1. **Load.** On the start page, pick an example study (`data/pilot`, `data/bench28`, `data/bench72`), upload files, or give a folder. xpt, sas7bdat and csv are read; a define.xml is optional but supplies labels, domain keys and code lists. DM is required.
2. **Wait for the build.** Every dataset is built in background workers, DM first. The overview shows a tile per dataset and a **Needs a look** list: a failed build, or generated records that break an SDTM rule the source keeps.
3. **Generate and download.** Set subjects and seed in the Generation card, then download the synthetic data (xpt or csv, zipped) or the specification (`.rds`).

To generate without the app:

```r
generate_study(readRDS("specification.rds"), n = 200, seed = 1)
```

The specification holds every source subject's shifted timeline, so handle it like the source data.

## Options

Set before `runApp()`.

| Option | Default | Effect |
|---|---|---|
| `synth.cores` | up to 8 | background workers |
| `synth.cache` | `cache/` | build cache, keyed by the content of the records, DM, the code and the decisions |
| `synth.model` / `SYNTH_MODEL` | `"auto"` | `"auto"` for linear models, `"tree"` for CART |
| `synth.min_subjects` / `SYNTH_MIN_SUBJECTS` | 5 | k: the fewest subjects a stored or drawn value must belong to |

## How data are made

Two parts: the **spine** (when things happen) and the **content** (what the records say).

**Spine** (`R/spine.R`). Dates, visits, study days, record keys, --SPID and the arm. Drawn, never modelled, and never copied from one source subject:

- Dates are shifted per subject so RFSTDTC is 2020-01-01; day 1 is then put on a random calendar date within the source's first and last visit.
- Each synthetic subject gets a profile (arm, last visit, death, which datasets and categories they have), drawn from cells of at least k source subjects.
- Visits fall on their planned day (TV's VISITDY, else the source median) plus a drawn deviation.
- SV is drawn first. Every other record sits at a visit the subject attended and within that visit's SV dates where the source always is.
- SE follows the arm's path in TA. EPOCH is the epoch of the element holding the date.
- Other dates are a landmark (a visit, a DM date) plus a gap drawn from the source.
- DM dates come from the records: DMDTC the first visit, RFXENDTC the last exposure.
- A linked dataset follows its parent (TR after TU), keeps the parent's link ids, and copies the columns it shares with the parent (TRMETHOD is TUMETHOD).

**Content** (`R/study.R`, `R/rules.R`, `R/models.R`). Each dataset is a graph of its variables. A variable is calculated by a rule found in the data (LBTEST from LBTESTCD, a unit conversion, a questionnaire total), or drawn from a model given the arm, the timeline and the previous value. A rule is used when it reproduces at least 90% of the source records (the overview's slider). Values held by fewer than k subjects are never drawn.

**Record conventions** (`R/generate.R`). A record that repeats the define.xml keys is dropped where the source never repeats them. A test NOT DONE has no result. --STRESN is --STRESC as a number.

## What is checked

`sdtm_rules()` in `R/review.R` is a fixed list of SDTMIG rules: keys, subjects in DM, date order, study days, exposure dates, death, parent records (SUPP--, RELREC, --LNKID, --LNKGRP), visits against SV and TV, code/name pairs, results, trial design (TA, TE, EPOCH), AE flags, code lists.

The same rules run on the source when it is loaded. A rule is run on the synthetic data only where the source keeps it in every record. Rules the source breaks are listed apart, not scored. No rule is mined from the data.

Not checked: clinical associations (placebo with no drug concentration, stable weight within a subject, a medication that comes with an adverse event). See the guide's Known limitations.

## Code map

| File | What |
|---|---|
| `global.R`, `ui.R`, `server.R` | packages, page layout, polling the build |
| `R/pipeline.R` | load, build in workers, cache, bundle |
| `R/source.R`, `R/supp.R`, `R/domains.R` | reading, SUPP-- folding, dataset classes and order |
| `R/spine.R` | dates, schedule, profile, visits, records, SE, EPOCH |
| `R/study.R` | record structure, links, graph nodes |
| `R/rules.R` | rule search and scoring |
| `R/models.R` | node models (`auto`, `tree`) |
| `R/generate.R` | generation and output |
| `R/review.R` | SDTM rules and comparisons |
| `R/mod_*.R`, `R/ui_helpers.R`, `www/` | app pages |

The header comment of each file explains its part in detail.

## Benchmark

```sh
Rscript bench/build_bench.R                    # writes data/bench28 and data/bench72 (needs pharmaversesdtm)
SHOW=1 bench/eval/run.sh b28 data/bench28      # one study, seeds 1–3
```

`run.sh` splits the subjects in half, builds from one half (members), generates as many subjects, and measures with code kept apart from the app's: SDTM rules the members keep (`rules.R`), marginal fidelity (`fidelity.R`), and linkage and membership tests (`privacy.R`). Output goes to `bench/out/eval/` (or `OUT`).

- **pilot**: CDISCPILOT01, 10 datasets, 306 subjects.
- **bench28**: the pilot plus pharmaversesdtm's DS, EG, PC, PP, OE, RS, SC, TR, TU, SUPP-- and trial design, joined onto the pilot's subjects.
- **bench72**: bench28 with QS, LB, OE split by category and 34 renamed copies of domains.

Latest run (153 members, seeds 1–3):

| | pilot | bench28 | bench72 |
|---|---|---|---|
| SDTM rules run (not run: the source breaks them) | 79 (12) | 276 (30) | 607 (52) |
| broken | 0 | 0 | 1, OEVA OESEQ repeated; fixed, checked on OEVA, not rerun |
| link: a member's timeline matches one synthetic subject | 0.081 | 0.044 | 0.017 |
| membership accuracy (0.5 is chance) | 0.495 | 0.485 | 0.496 |

Data: [CDISC pilot project](https://github.com/cdisc-org/sdtm-adam-pilot-project), study CDISCPILOT01; [pharmaversesdtm](https://github.com/pharmaverse/pharmaversesdtm).
