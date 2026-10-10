# Synthetic SDTM

A Shiny app that learns a generator from a set of SDTM datasets and samples linked synthetic data. Timing is not modelled: every date is shifted per subject so that RFSTDTC falls on 2020-01-01 (PHUSE-style; screening keeps its negative days), and each synthetic subject copies the time spine of one source subject, its donor: its arm, its records in every dataset, their visits, study days, time points and dates. Each dataset gets one directed acyclic graph (DAG) for the content: variables are calculated by deterministic rules, sampled from models (maximum-likelihood linear models, or CART trees as in synthpop), or set as identifiers. Datasets are generated in SDTM class order (DM, special-purpose, interventions, events, findings, then by name) and may use DM variables, per-subject record counts of earlier datasets and spine features (treatment status, death, last visit among the visits that mark progress, which datasets the subject has records in, the record's phase, which DM dates it falls on and which of its own dates it has).

## Run

```r
install.packages(c("shiny", "bslib", "DT", "ggplot2", "haven", "xml2", "nnet", "MASS", "rpart", "zip", "mirai", "digest", "jsonlite"))
shiny::runApp()
```

Options: `synth.cores` (background workers, default up to 8), `synth.cache` (cache folder, default `cache/`) and `synth.model` (model of a new node, `"auto"` for the linear models by default, or `"tree"`; also `SYNTH_MODEL` in the environment). The graph renderer (viz.js) and the font are bundled in `www/`, so nothing is sent to an external service.

## How it works

Loading a study is the only required step. Nothing waits for a confirmation.

- **One policy.** Found rules that reproduce at least the policy share of checked records (90% by default) are accepted in every dataset, the others rejected. The sidebar slider changes the share for the whole study.
- **Background build.** The source is read in a worker; DM is built first (later starting graphs use its arm), then every other dataset in parallel, largest first: rules found, resolved by the policy, starting graph built, checked and fitted. Once nothing is left to build, the study is sampled and its coherence checked. The app only submits jobs and collects results, so it answers while workers compute.
- **Cache by content.** A dataset's build is stored under a hash of its records, DM's, the code, and what the policy decides for its found rules. Loading the same study again takes seconds; a policy change rebuilds only the datasets where it changes a decision; an edit rebuilds only that dataset.
- **Exceptions, not approvals.** The board shows every dataset as a tile, the datasets that need a look (a build error, a failed check of the graph, which leaves the dataset out of the generated data, or a failed coherence check), and a ledger of every decision ranked by the number of source records it changes.
- **Changes by hand.** Any dataset can be opened from a tile or the ledger: rules, variable types, graph, and a comparison of source and synthetic data. A change is marked as made by hand, kept when the policy changes, and rebuilt at once; "Back to the policy" drops it.

Save: the synthetic data (xpt/csv) or a specification bundle (`generate_study(readRDS(file), n, seed)`). The bundle holds model coefficients and the shifted spine of every source subject (arm, record keys, visits, dates), so it is as private as that timing, not more.

## Rule discovery

No rule is named in advance: every column is searched, on a sample of whole subjects (at most 5,000 records); the best few candidates per column are then checked on all records, so the shares shown are exact.

Searched rules come from a fixed set of operations (constant, sequence, study day, range flag, record flag, date offset, unit conversion, lookup, R expression). A unit conversion carries a result that is not a number ("PRESENT", "<5") over as written. Each operation proposes inputs from column kinds and the record structure only, learns its parameters from the source, and is judged by the share of records it reproduces.

A rule's share counts only *checked* records, meaning records that could contradict it. A lookup is scored on subjects it was not learnt from: the table is learnt on the odd-numbered subjects and checked on the even ones, then the other way round, so checked records are those whose key also occurs in the other half. Otherwise a table of many keys memorises which keys happened to carry a rare value (AELLT to AESHOSP: the hospitalised terms) without predicting it. A rule is proposed when it reproduces at least 90% of checked records, including half of the records that the most common value gets wrong, so a skewed target is not "explained" by an arbitrary input. Lookup keys are categories shared by at least two subjects per value on average, so a subject's own code is never a key; record numbers (--SEQ) are never keys either. In a scheduled dataset all its record keys together are also tried as one key, for values set by the schedule (RSLNKGRP by visit, test and evaluator). With few records there are few checked records, and the rules table shows both numbers.

## Time spine

`R/spine.R`. Dates are read at day resolution; a time of day is dropped. A partial date gets day 15 when it has a month, July 15 when it has only a year; one without a year stays missing. The imputed day is then moved, within the stated month or year, to keep the orders that every complete date of the source keeps (at least 99% of at least 10 records): start before end and collection date, a record within the subject's DM dates (a history date before the first dose), a visit's dates after the previous visit's. A bound outside the stated period is a contradiction of the source, and the date keeps its imputed day.

The spine columns of a dataset are its dates, timing variables (VISITNUM, VISIT, --DY, --TPT, …), record keys, a series' --SPID, and in DM the arm. They are copied from the donor; donors are drawn without replacement while there are enough. Content is generated given the spine. Where few records carry a pattern no model keeps, implications learnt from the source are enforced: a key value that always comes with one value (a DS date on DTHDTC gives DEATH), or a value that only occurs at some key values (SCREEN FAILURE only for untreated subjects). Each holds on every source record, in at least two subjects, and would hold by chance with probability below 10⁻⁴ (a set of levels picked after the fact is penalised by the number of such sets). The last visit counts only visits that mark progress: in number order, a visit whose subjects mostly attended the latest counted visit with at least as many subjects, so an AE FOLLOW-UP after an early stop does not make a week-8 dropout look like a completer. A node modelled per test has its implications found per test: the last visit gives PROTOCOL COMPLETED to the disposition event, not to the subject's other DS records. A category that a lookup derives from the node alone (CMROUTE from CMTRT) carries its implications through the node.

## Node models

A sampled node is fitted per test and per stage (first record of a series, or a later one given the previous value). `auto` is the default. On the pilot `tree` broke as many source relations as `auto` (11 each) but matched numeric distributions more closely (mean KS 0.02 against 0.08; CMDOSE 0.05 against 0.38). `auto` picks categories (multinomial, missing being a value) for at most a few distinct values, otherwise a normal or log-normal linear model with a model of whether the value is recorded. `tree` fits a classification tree for categories and a regression tree for numbers (with a classification tree of whether it is recorded); leaves have at least 5 records, and a value is drawn from its leaf's class counts or, for a number, between its leaf's 21 quantiles. Implications are kept with either model. A tree stores split points and per-leaf counts or quantiles, so a leaf of 5 records discloses more about those records than a coefficient does.

## Record structures

DM has one record per subject. Findings (--TESTCD) and datasets with one record per subject and visit follow a schedule of planned keys: the visit and test, then the categories that best separate repeated records. When these leave records unseparated, a text column that separates 99% of a subject's records and whose values recur across subjects (TULNKID, TRLNKID: T01, NT01, NEW01) is a key whatever its number of values, so the records linking TR to TU come from one donor. Otherwise, a category that every subject has the same number of records of (one DSCAT "DISPOSITION EVENT" each in DS) is planned the same way, and values are modelled per category. Everything else is a count of records per subject. Category values whose per-subject count never varies in the source are coherence checks on the generated data.

Data: [CDISC pilot project](https://github.com/cdisc-org/sdtm-adam-pilot-project), study CDISCPILOT01.
