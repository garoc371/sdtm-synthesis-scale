# Synthetic SDTM

A Shiny app that learns a generator from a set of SDTM datasets and samples linked synthetic data. Timing is not modelled: every date is shifted per subject so that RFSTDTC falls on 2020-01-01 (PHUSE-style; screening keeps its negative days), and each synthetic subject copies the time spine of one source subject, its donor: its arm, its records in every dataset, their visits, study days, time points and dates. Each dataset gets one directed acyclic graph (DAG) for the content: variables are calculated by deterministic rules, sampled from models (maximum-likelihood linear models, or CART trees as in synthpop), or set as identifiers. Datasets are generated in SDTM class order (DM, special-purpose, interventions, events, findings, then by name), a dataset after the one its --LNKID points to (TR after TU), and may use DM variables, per-subject record counts of earlier datasets, the spine and category columns of the linked record at its visit (LINK.TULOC in TR) and spine features (treatment status, death, last visit among the visits that mark progress, which datasets the subject has records in, the record's phase, which DM dates it falls on and which of its own dates it has).

## Run

```r
shiny::runApp()
```

`global.R` lists the packages and stops with the `install.packages()` call for any that are missing. The start page loads uploaded files or a folder (xpt, sas7bdat, csv, define.xml), or one of the example studies in `data/` (the CDISC pilot, bench28, bench72).

Options: `synth.cores` (background workers, default up to 8), `synth.cache` (cache folder, default `cache/`) and `synth.model` (model of a new node, `"auto"` for the linear models by default, or `"tree"`; also `SYNTH_MODEL` in the environment). The graph renderer (viz.js) and the font are bundled in `www/`, so nothing is sent to an external service.

## How it works

Loading a study is the only required step. Nothing waits for a confirmation.

- **One policy.** Found rules that reproduce at least the policy share of checked records (90% by default) are accepted in every dataset, the others rejected. The slider on the overview changes the share for the whole study.
- **Background build.** The source is read in a worker; DM is built first (later starting graphs use its arm), then every other dataset in parallel, largest first: rules found, resolved by the policy, starting graph built, checked and fitted. Once nothing is left to build, the study is sampled and checked against the SDTM rules (below). The app only submits jobs and collects results, so it answers while workers compute.
- **Cache by content.** A dataset's build is stored under a hash of its records, DM's, the code, and what the policy decides for its found rules. Loading the same study again takes seconds; a policy change rebuilds only the datasets where it changes a decision; an edit rebuilds only that dataset.
- **Exceptions, not approvals.** The board shows every dataset as a tile, the datasets that need a look (a build error, a failed check of the graph, which leaves the dataset out of the generated data, or a broken SDTM rule), and a ledger of every decision ranked by the number of source records it changes.
- **Changes by hand.** Any dataset can be opened from a tile or the ledger: rules, variable types, graph, and a comparison of source and synthetic data. A change is marked as made by hand, kept when the policy changes, and rebuilt at once; "Back to the policy" drops it.

Save: the synthetic data (xpt/csv) or a specification bundle (`generate_study(readRDS(file), n, seed)`). The bundle holds model coefficients and the shifted spine of every source subject (arm, record keys, visits, dates), so it is as private as that timing, not more.

## Rule discovery

No rule is named in advance: every column is searched, on a sample of whole subjects (at most 5,000 records); the best few candidates per column are then checked on all records, so the shares shown are exact.

Searched rules come from a fixed set of operations (constant, sequence, study day, range flag, record flag, date offset, unit conversion, lookup, R expression). A unit conversion carries a result that is not a number ("PRESENT", "<5") over as written. Each operation proposes inputs from column kinds and the record structure only, learns its parameters from the source, and is judged by the share of records it reproduces.

A rule's share counts only *checked* records, meaning records that could contradict it. A lookup is scored on subjects it was not learnt from: the table is learnt on the odd-numbered subjects and checked on the even ones, then the other way round, so checked records are those whose key also occurs in the other half. Otherwise a table of many keys memorises which keys happened to carry a rare value (AELLT to AESHOSP: the hospitalised terms) without predicting it. A rule is proposed when it reproduces at least 90% of checked records, including half of the records that the most common value gets wrong, so a skewed target is not "explained" by an arbitrary input. Lookup keys are categories shared by at least two subjects per value on average, so a subject's own code is never a key; record numbers (--SEQ) are never keys either, nor is a number with more values than a category model keeps unless the spine copies it (DM.AGE is drawn anew, so a table keyed by age cannot cover the synthetic ages). In a scheduled dataset all its record keys together are also tried as one key, for values set by the schedule (RSLNKGRP by visit, test and evaluator). With few records there are few checked records, and the rules table shows both numbers.

A result can also be calculated from other records of its group: the subject's records at one visit, split further by --CAT, --SPEC, --LNKGRP, --EVAL or --EVALID where every test records it. Per test, discovery tries a sum of other tests (chosen by least squares, then one test at a time while fewer records are missed), a + b·x or a / x of one other test, and a lookup of up to three other tests' values, scored on held-out subjects like any lookup; a test is derived when it reproduces at least 90% of its checked groups (bench28: ACTOT and NPTOT as item sums, SUMDIAM as the lesion sum per TRLNKGRP, LAMZHL = 0.693 / LAMZ, OVRLRESP by target, non-target and new-lesion response, an NPI item's score 0 when it is ABSENT). Expressions and lookups use only tests with one record per group. A test is not derived from itself through others; the one with the higher share chooses first. Generation draws the group's other tests, then calculates; the rules table shows the calculation of each test.

## Time spine

`R/spine.R`. Dates are read at day resolution; a time of day is dropped. A partial date gets day 15 when it has a month, July 15 when it has only a year; one without a year stays missing. The imputed day is then moved, within the stated month or year, to keep the orders that every complete date of the source keeps (at least 99% of at least 10 records): start before end and collection date, a record within the subject's DM dates (a history date before the first dose), a visit's dates after the previous visit's. A bound outside the stated period is a contradiction of the source, and the date keeps its imputed day.

The spine columns of a dataset are its dates, timing variables (VISITNUM, VISIT, --DY, --TPT, …), record keys, a series' --SPID, and in DM the arm. They are copied from the donor; donors are drawn without replacement while there are enough. Content is generated given the spine. Where few records carry a pattern no model keeps, implications learnt from the source are enforced: a key value that always comes with one value (a DS date on DTHDTC gives DEATH), or a value that only occurs at some key values (SCREEN FAILURE only for untreated subjects). Each holds on every source record, in at least two subjects, and would hold by chance with probability below 10⁻⁴ (a set of levels picked after the fact is penalised by the number of such sets). The last visit counts only visits that mark progress: in number order, a visit whose subjects mostly attended the latest counted visit with at least as many subjects, so an AE FOLLOW-UP after an early stop does not make a week-8 dropout look like a completer. A node modelled per test has its implications found per test: the last visit gives PROTOCOL COMPLETED to the disposition event, not to the subject's other DS records. A category that a lookup derives from the node alone (CMROUTE from CMTRT) carries its implications through the node.

## Node models

A sampled node is fitted per test and per stage (first record of a series, or a later one given the previous value). `auto` is the default: categories (multinomial, missing being a value) for at most 10 distinct values, otherwise a number: a model of whether it is recorded, then a linear model of its normal score (its percentile in the source, on the normal scale). A drawn score is read back through the test's 101 source percentiles, so a value keeps the source's shape, range and repeated values (a dose of 54 mg, a zero). `tree` matches distributions about as closely (bench28 mean KS 0.025 against 0.029) and, on seeds 1–3, broke more source relations (pilot 4 against 1, bench28 20 against 17). A category with more than 30 values has too many to model; when an accepted lookup derives a coarser category from it alone, that one is modelled and the value drawn within it by its frequencies (DSDECOD on arm and last visit, then the verbatim DSTERM within it), otherwise it is drawn from its frequencies. `tree` fits a classification tree for categories and a regression tree for numbers (with a classification tree of whether it is recorded); leaves have at least 5 records, and a value is drawn from its leaf's class counts or, for a number, between its leaf's 21 quantiles. Implications are kept with either model. A tree stores split points and per-leaf counts or quantiles, so a leaf of 5 records discloses more about those records than a coefficient does; `auto` stores a test's 101 percentiles, whose ends are its source minimum and maximum.

## Record structures

DM has one record per subject. Findings (--TESTCD) and datasets with one record per subject and visit follow a schedule of planned keys: the visit and test, then the categories that best separate repeated records. When these leave records unseparated, a text column that separates 99% of a subject's records and whose values recur across subjects (TULNKID, TRLNKID: T01, NT01, NEW01) is a key whatever its number of values, so the records linking TR to TU come from one donor. Otherwise, a category that every subject has the same number of records of (one DSCAT "DISPOSITION EVENT" each in DS) is planned the same way, and values are modelled per category. Everything else is a count of records per subject.

## SDTM rules

The generated data are checked against a fixed list of SDTMIG rules (`sdtm_rules()`, R/review.R), read from the output datasets and the define.xml alone, never from the fitted specification: one DM record per subject and every subject in DM; the define.xml's domain keys unique; --SEQ unique per subject; --STDTC on or before --ENDTC; --DY equal to the days from RFSTDTC, with no day 0; EX dates within RFXSTDTC to RFXENDTC; DTHFL = Y exactly when DTHDTC is given; every SUPP-- and RELREC record pointing to an existing parent; values in the define.xml's code lists. Dates are compared for order only. The same function runs on the source, so a rule the source already breaks shows as such.

## Benchmark

`bench/`. `Rscript bench/build_bench.R` writes two folders to `data/` (needs `pharmaversesdtm`):

- **bench28**: the pilot's DM, EX, LB, MH, QS, SV, VS, AE, CM; pharmaversesdtm's DS, EG, PC, PP, and the ophthalmology and oncology OE, RS, SC, TR, TU; SUPP datasets; the pilot's trial design.
- **bench72**: bench28 with QS, LB and OE split by category (QSAD, LBCH, OEVA, …) and 34 custom-domain replicas (XA…YA: a domain's records for 70% of its subjects, with its SUPP).

`bench/eval/run.sh <label> [folder]` (default `data/pilot`) splits the real subjects at random in half (`SPLIT=1`), builds the study from one half (the members) with the app's code, generates as many subjects as there are members for seeds 1–3, and measures them with code kept apart from the app's:

- **SDTM rules** (`rules.R`): every rule above, on the members' real data and each seed. Required: a rule the synthetic data break more often than the members' data is a failure of the generator; one the source breaks as often is the source's.
- **Relation score** (`check.R`): relations held by at least `SUPPORT` members (5 by default) are mined on the members: date orders, functional dependencies, implications within and between datasets. A relation is preserved when the share of synthetic subjects breaking it is at most the held-out half's share plus `TOL` (0.05), so the bar is how well a fresh real sample keeps it. Reported: the share preserved, overall, for content and timing, and per kind; `SHOW=1` lists the relations not preserved in any seed.
- **Fidelity** (`fidelity.R`): mean total variation distance of categories and Kolmogorov–Smirnov distance of numbers, per variable.
- **Privacy** (`privacy.R`), on the same split. Timeline: a subject's dates as days from their first visit. Correct link: synthetic subjects whose timeline matches exactly one member, that member being their donor. Membership accuracy: a real subject is called a member when a synthetic subject has their timeline, balanced over members and held out. Singleton values: text values held by one member and no held-out subject, and the share reproduced. Exact records: per dataset, synthetic records equal to a member's record and to a held-out subject's, without identifiers and --SEQ.

bench28, 153 members, seeds 1–3:

| | |
|---|---|
| SDTM rules broken | 12 of 172; 1 more often than the members' data: MH keys USUBJID, MHTERM, MHSTDTC not unique in 398 of 919 records (MHTERM is not generated) |
| relations | 43,781 mined, 43,669 scored; preserved 1.000 (dates 0.999); not preserved in any seed: CM.CMCLAS=SYSTEMIC HORMONAL PREPARATIONS ends by RFXENDTC (held out 0.000, seeds 0.09–0.20) |
| fidelity | mean TVD 0.044, KS 0.033 |
| correct link | 0.941 (the members unique on their timeline) |
| membership accuracy | 0.941 (members found 1.000, held out matched 0.118) |
| singleton values reproduced | 0.199 of 4,622 |
| exact records, member / held out | EX, SE, SV, TU, SUPP 1.000 / 0.24–1.000; DS 0.84 / 0.40; QS 0.75 / 0.40; LB 0.16 / 0.08; AE, CM, DM, MH under 0.01 |

The source's own failures: CM keys not unique (3,557 of 4,132), DS and SC values outside the pilot's code lists (pharmaversesdtm's DSDECOD, DSCAT, SCCAT), --DY not from RFSTDTC in EG, RS, TR, TU (studies spliced with another RFSTDTC), OESEQ repeated, 4 EX records after RFXENDTC, one repeated SV visit. The held-out half breaks 1.0% of relations at all; synthetic seed 1 breaks 0.3%: the synthetic data keep the members' relations more tightly than a fresh real sample does. With 8 workers bench72 builds in 26 s; a 2,540-subject study generated from bench28 (3.3 million records, QS 1 million) builds in 126 s and generates in 81 s; its longest job is QS.

Data: [CDISC pilot project](https://github.com/cdisc-org/sdtm-adam-pilot-project), study CDISCPILOT01.
