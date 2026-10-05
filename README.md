# Synthetic SDTM

A Shiny app that learns a generator from a set of SDTM datasets and samples linked synthetic data. Each dataset gets one directed acyclic graph (DAG): variables are calculated by deterministic rules, sampled from models fitted by maximum likelihood, or set as identifiers. Datasets are generated in a fixed order (DM, SV, then the others) and may use DM variables, per-subject record counts and the subject's timeline from datasets generated earlier: SDTM date conventions (DM reference dates pinned to EX, DS and SV; records at visits the subject attended in SV; dates within or on their SV visit; orders and bounds against earlier dates) that hold on the source become edges of a timeline DAG, and the coherence checks test each edge on the generated data.

## Run

```r
install.packages(c("shiny", "bslib", "DT", "ggplot2", "haven", "xml2", "nnet", "MASS", "zip", "mirai", "digest", "jsonlite"))
shiny::runApp()
```

Tests: `Rscript tests/test_supp.R` (needs `testthat` and `pharmaversesdtm`).

Options: `synth.cores` (background workers, default up to 8) and `synth.cache` (cache folder, default `cache/`). The graph renderer (viz.js) and the font are bundled in `www/`, so nothing is sent to an external service.

## How it works

Loading a study is the only required step. Nothing waits for a confirmation.

- **One policy.** Found rules that reproduce at least the policy share of checked records (90% by default) are accepted in every dataset, the others rejected. Standard SDTM rules are accepted at 90%. The sidebar slider changes the share for the whole study.
- **Background build.** The source is read in a worker; DM is built first (later starting graphs use its arm), then every other dataset in parallel, largest first: rules found, resolved by the policy, starting graph built, checked and fitted. Once nothing is left to build, the study is sampled and its coherence checked. The app only submits jobs and collects results, so it answers while workers compute.
- **Cache by content.** A dataset's build is stored under a hash of its records, DM's, the code, and what the policy decides for its found rules. Loading the same study again takes seconds; a policy change rebuilds only the datasets where it changes a decision; an edit rebuilds only that dataset.
- **Exceptions, not approvals.** The board shows every dataset as a tile, the datasets that need a look (a build error, a failed check of the graph, which leaves the dataset out of the generated data, or a failed coherence check), and a ledger of every decision ranked by the number of source records it changes.
- **Changes by hand.** Any dataset can be opened from a tile or the ledger: rules, variable types, graph, and a comparison of source and synthetic data. A change is marked as made by hand, kept when the policy changes, and rebuilt at once; "Back to the policy" drops it.

Save: the synthetic data (xpt/csv) or a specification bundle without source records (`generate_study(readRDS(file), n, seed)`).

## Rule discovery

Rules the SDTM standard defines (`STANDARD_RULES` in `R/rules.R`: VISIT from VISITNUM, study days, --TEST from --TESTCD, the MedDRA hierarchy, …) are checked first and accepted when they reproduce 90% of checked records. The remaining columns are searched on a sample of whole subjects (at most 5,000 records); the best few candidates per column are then checked on all records, so the shares shown are exact.

Searched rules come from a fixed set of operations (constant, sequence, study day, range flag, record flag, date offset, unit conversion, lookup, R expression). Each operation proposes inputs from column kinds and the record structure only, learns its parameters from the source, and is judged by the share of records it reproduces.

A rule's share counts only *checked* records, meaning records that could contradict it. For a lookup these are records whose key value occurs at least twice, because a key seen once fits any table. A rule is proposed when it reproduces at least 90% of checked records, including half of the records that the most common value gets wrong, so a skewed target is not "explained" by an arbitrary input. Lookup keys are categories shared by at least two subjects per value on average, so a subject's own code is never a key; record numbers (--SEQ) are never keys either. With few records there are few checked records, and the rules table shows both numbers.

## Record structures

DM has one record per subject. Findings (--TESTCD) and datasets with one record per subject and visit follow a schedule of planned keys. Otherwise, a category that every subject has the same number of records of (one DSCAT "DISPOSITION EVENT" each in DS) is planned the same way, and values are modelled per category. Everything else is a count of records per subject. Category values whose per-subject count never varies in the source are coherence checks on the generated data.

Data: [CDISC pilot project](https://github.com/cdisc-org/sdtm-adam-pilot-project), study CDISCPILOT01.
