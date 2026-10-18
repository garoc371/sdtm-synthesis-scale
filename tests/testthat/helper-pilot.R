# The pilot study's DM, discovered and resolved by a policy of 100%: its rules are STUDYID,
# DOMAIN, DTHFL (a lookup), AGEU and COUNTRY, all accepted; its variables not calculated are
# USUBJID, SUBJID (identifiers), SITEID, AGE, SEX, RACE and ETHNIC (sampled).
study <- load_study(test_path("../../data/pilot/dm.xpt"))
dm <- resolve_rules(discover_dataset(study, "DM")$datasets$DM, 1)
reactives <- list(dataset = reactive(dm), study = reactive(study), share = reactive(1))
