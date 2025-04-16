# Setup -------------------------------------------------------------------
# This _targets.R file defines the {targets} pipeline.
# Run tar_make() to run the pipeline, tar_make(target) to run up to a defined target, and tar_read(target) to view the results.
library(targets)
library(tarchetypes)
library(crew)

# Define external paths
source("paths/paths.R")

# Source all functions from the "R" folder
sapply(list.files("R", full.names = TRUE), source, .GlobalEnv)



# Distributed Computing ---------------------------------------------------
# Enable the controller in tar_options_set to run targets in parallel
controller <- crew::crew_controller_local(
  name = "my_controller",
  workers = 8,
  seconds_idle = 10,
  launch_max = 10000
)
#
# tar_config_set(
# 	seconds_meta_append = 15,
# 	seconds_reporter = 0.5
# )

# Set target-specific options such as packages.
tar_option_set(
  # controller = controller,
  storage = "worker", retrieval = "worker",
  memory = "transient", garbage_collection = TRUE,
  # error = "null",
  # workspace_on_error = TRUE, # Save a workspace file for a target that errors out.
  packages = c(
    "arrow", # For reading parquet files
    "cli", # For producing command line interface elements such as loading bars
    "survival", # For survival analysis including Cox regression
    "haven", # To read and write Stata .dta
    "openxlsx", # To read and write Excel .xlxs
    "broom", # To clean regression output
    "janitor", # To clean table names
    "lubridate", # To manage dates
    "tidyverse", # For data management
    "collapse", # For fast data management
    "gtsummary" # To produce summary tables
  )
)



# List of target objects.
list(
  # Specifications -------------------------------------------------------------

  tar_target(exposure, "exposed"),
  tar_target(exclusion, c("outcome before indexdate", "consultation in year before indexdate")),

  # ┠ Models -----
  tar_target( # Specify models for regression
    model,
    c(
      "crude" = "",
      "A" = " + ps"
    )
  ),

  # ┠ Models -----
  tar_target( # Specify models for regression
    model_time_updated,
    c(
      "A" = paste(c("", outcome), collapse = " + "),
      "H" = paste(c("", "hospital", outcome), collapse = " + ")
    )
  ),

  # ┠ Study dates -----

  tar_target(study_start, as.Date("1997-04-01")),
  tar_target(study_end, as.Date("2023-03-31")),


  # Inputs ------------------------------------------------------------------

  # ┠ File paths -----------------------------------------------------------------

  # Codebrowsers
  tar_target(icd_codes, read_tsv("sensitive_input/ICD10_Edition5_CodesAndTitlesAndMetadata_GB_20160401.txt", col_types = cols(.default = "c")) |> clean_names()),


  # Denominator, define and extract
  tar_target(denominator_file, if (dummy_define) path_denominator),
  tar_target(define_obs_files, if (dummy_define) dir(path_define, pattern = "Define_Inc1_Observation.*\\.parquet", full.names = TRUE)),
  tar_target(define_drug_files, if (dummy_define) dir(path_define, pattern = "Define_Inc1_Drug.*\\.parquet", full.names = TRUE)),
  tar_target(define_non_disease_files, if (dummy_define) dir(path_define, pattern = "Define_Inc1_Observation.*\\.parquet", full.names = TRUE)),
  tar_target(files, if (dummy_extract) dir(path_extract, pattern = ".parquet", full.names = TRUE)),

  # HES eligibility (all)
  tar_target(hes_eligibility_all, if (dummy_define) read_dta(path_hes_eligibility), format = "parquet"),
  # Hes eligibility (linked)
  tar_target(hes_eligibility, if (dummy_define) read_tsv_arrow(paste0(path_linked_data, "23_002665_linkage_eligibility_aurum.txt")), format = "parquet"),

  # Phecodes
  tar_target(phecodes_raw, read_csv("input/Phecode_map_v1_2_icd10_beta.csv", col_types = cols(.default = "c")) |> clean_names()),
  tar_target(
    phecodes,
    phecodes_raw |>
      select(icd10, phecode) |>
      filter(!is.na(icd10)) |>
      filter(icd10 %in% icd_codes$code) |>
      arrange(phecode)
  ),

  # GBD codes
  tar_target(gbd_codes_raw, read_csv("input/WHO_GHE_cause_categories_and_ICD_10_codes.csv")),
  tar_target(
    gbd_codes,
    gbd_codes_raw |>
      separate_longer_delim(icd10, delim = ",") |>
      mutate(icd10 = str_trim(icd10)) |>
      mutate(
        start = str_split_i(icd10, "-", 1), end = str_split_i(icd10, "-", 2),
        start_suffix = as.numeric(str_sub(start, 2)), end_suffix = as.numeric(str_sub(end, 2)),
        start_prefix = str_sub(start, 1, 1), end_prefix = str_sub(end, 1, 1)
      ) |>
      rowwise() |>
      mutate(icd10 = ifelse(
        !is.na(end_suffix),
        paste0(str_sub(icd10, 1, 1), format_icd(seq(from = start_suffix, to = end_suffix, by = 0.1)), collapse = ","),
        start
      )) |>
      separate_longer_delim(icd10, delim = ",") |>
      filter(!(icd10 %in% str_trim(str_split(minus, ",")))) |>
      select(gbdcode, icd10) |>
      filter(icd10 %in% icd_codes$code)
  ),

  # ┠ Codelists for Define---------------------------------------------------------
  # Codelists must be in the correct folder as .csv files and have the same name as specified here

  tar_target( # Codelists used to define cohorts of exposed individuals
    codelists_for_define,
    tribble(
      ~name, ~codevar, ~extract_from,
      # MedCodeIds
      "eczema", "MedCodeId", "observation",
      "psoriasis", "MedCodeId", "observation",
      "phototherapy", "MedCodeId", "observation",
      # ProdCodeIds
      "oral_glucocorticoids", "ProdCodeId", "drugissue",
      "emollients", "ProdCodeId", "drugissue",
      "topical_glucocorticoids", "ProdCodeId", "drugissue",
      "systemic_immunosupressants", "ProdCodeId", "drugissue",
      "topical_calcineurin_inhibitors", "ProdCodeId", "drugissue",
      "systemic_treatments_for_psoriasis", "ProdCodeId", "drugissue"
    ) |>
      mutate(
        path = paste0("codelists/Aurum/", codevar, "/", name, " codelist.csv"),
        full = map(path, ~ read_csv(.x, col_types = cols(.default = "c"))),
        codes = map2(path, codevar, ~ read_csv(.x, , col_types = cols(.default = "c"))[[.y]])
      )
  ),
  tar_target(
    drug_codelists,
    codelists_for_define[which(codelists_for_define$name %in% c("oral_glucocorticoids", "systemic_immunosupressants")), ]
  ),
  tar_target(
    non_disease_codelist,
    tribble(~name, ~codevar, ~extract_from, "non_disease", "MedCodeId", "observation") |>
      mutate(
        path = paste0("codelists/Aurum/", codevar, "/", name, " codelist.csv"),
        full = map(path, ~ read_csv(.x, col_types = cols(.default = "c"))),
        codes = map2(path, codevar, ~ read_csv(.x, , col_types = cols(.default = "c"))[[.y]])
      )
  ),
  tar_target(drugs, drug_codelists$name),


  # ┠ Codelists ---------------------------------------------------------

  tar_target( # Codelists used to extract eventdata on outcomes
    codelists,
    tribble(
      ~name, ~label, ~codevar, ~extract_from,
      # MedCodeIds
      "asthma", "Asthma", "MedCodeId", "observation",
      "food_allergy", "Food allergy", "MedCodeId", "observation",
      "allergic_rhinitis", "Allergic Rhinitis", "MedCodeId", "observation",
      "allergic_conjunctivitis", "Allergic Conjunctivitis", "MedCodeId", "observation",
      "eosinophilic_esophagitis", "Eosinophilic Eosophagitis", "MedCodeId", "observation",
      "alopecia_areata", "Alopecia Areata", "MedCodeId", "observation",
      "urticaria", "Urticaria", "MedCodeId", "observation",
      "anxiety", "Anxiety", "MedCodeId", "observation",
      "depression", "Depression", "MedCodeId", "observation",
      "alcohol_abuse", "Alcohol abuse", "MedCodeId", "observation",
      "cigarette_smoking", "Cigarette smoking", "MedCodeId", "observation",
      "adhd", "ADHD", "MedCodeId", "observation",
      "autism", "Autism spectrum disorders", "MedCodeId", "observation",
      "hypertension", "Hypertension", "MedCodeId", "observation",
      "coronary_artery_disease", "Coronary artery disease", "MedCodeId", "observation",
      "peripheral_artery_disease", "Peripheral artery disease", "MedCodeId", "observation",
      "myocardial_infarction", "Myocardial infarction", "MedCodeId", "observation",
      "stroke", "Stroke", "MedCodeId", "observation",
      "heart_failure", "Heart failure", "MedCodeId", "observation",
      "thromboembolic_diseases", "Thromboembolic diseases", "MedCodeId", "observation",
      "obesity", "Obesity", "MedCodeId", "observation",
      "dyslipidemia", "Dyslipidemia", "MedCodeId", "observation",
      "diabetes", "Diabetes mellitus", "MedCodeId", "observation",
      "metabolic_syndrome", "Metabolic syndrome", "MedCodeId", "observation",
      "fractures_hip", "Hip fracture", "MedCodeId", "observation",
      "fractures_pelvis", "Pelvis fracture", "MedCodeId", "observation",
      "fractures_spine", "Spine fracture", "MedCodeId", "observation",
      "fractures_wrist", "Wrist fracture", "MedCodeId", "observation",
      "osteoporosis", "Osteoporosis", "MedCodeId", "observation",
      "molluscum_contagiosum", "Molluscum contagiosum", "MedCodeId", "observation",
      "impetigo", "Impetigo", "MedCodeId", "observation",
      "herpes_simplex", "Herpes simplex", "MedCodeId", "observation",
      "dermatophyte_infection", "Dermatophyte infection", "MedCodeId", "observation",
      "cutaneous_warts", "Cutaneous warts", "MedCodeId", "observation",
      "cancer_lung", "Lung cancer", "MedCodeId", "observation",
      "cancer_breast", "Breast cancer", "MedCodeId", "observation",
      "cancer_prostate", "Prostate cancer", "MedCodeId", "observation",
      "cancer_pancreas", "Pancreatic cancer", "MedCodeId", "observation",
      "non_hodkin_lymphoma", "Non-hodkin lymphoma", "MedCodeId", "observation",
      "hodkin_lymphoma", "Hodkin lymphoma", "MedCodeId", "observation",
      "myeloma", "Myeloma", "MedCodeId", "observation",
      "cancer_cns", "Central nervous system cancers", "MedCodeId", "observation",
      "melanoma", "Melanoma", "MedCodeId", "observation",
      "nonmelanoma_skin_cancer", "Nonmelanoma skin cancer", "MedCodeId", "observation",
      "dementia_alzheimers", "Alzheimer's dementia", "MedCodeId", "observation",
      "dementia_vascular", "Vascular dementia", "MedCodeId", "observation",
      "abdominal_hernia", "Abdominal hernia", "MedCodeId", "observation",
      "appendicitis", "Appendicitis", "MedCodeId", "observation",
      "autoimmune_liver_disease", "Autoimmune liver disease", "MedCodeId", "observation",
      "barretts_oesophagus", "Barett's oesophagus", "MedCodeId", "observation",
      "cholecystitis", "Cholecystitis", "MedCodeId", "observation",
      "coeliac_disease", "Coeliac disease", "MedCodeId", "observation",
      "crohns_disease", "Crohn's disease", "MedCodeId", "observation",
      "diverticular_disease_of_intestine", "Diverticular disease of intestine", "MedCodeId", "observation",
      "fatty_liver", "Fatty liver", "MedCodeId", "observation",
      "gastritis_and_duodenitis", "Gastritis and duodenitis", "MedCodeId", "observation",
      "gastro_oesophageal_reflux_disease", "Gastro oesophageal reflux disease", "MedCodeId", "observation",
      "irritable_bowel_syndrome", "Irritable bowel syndrome", "MedCodeId", "observation",
      "liver_fibrosis_sclerosis_cirrhosis", "Liver fibrosis, sclerosis and cirrhosis", "MedCodeId", "observation",
      "oesophageal_varices", "Oesophageal varices", "MedCodeId", "observation",
      "oesophagitis_and_oesophageal_ulcer", "Oesophagiitis and oesophageal ulcer", "MedCodeId", "observation",
      "pancreatitis", "Pancreatitis", "MedCodeId", "observation",
      "peptic_ulcer_disease", "Peptic ulcer disease", "MedCodeId", "observation",
      "peritonitis", "Peritonitis", "MedCodeId", "observation",
      "ulcerative_colitis", "Ulcerative colitis", "MedCodeId", "observation",
      "epilepsy", "Epilepsy", "MedCodeId", "observation",
      "migraine", "Migraine", "MedCodeId", "observation",
      "multiple_sclerosis", "Multiple sclerosis", "MedCodeId", "observation",
      "parkinsons_disease", "Parkinson's disease", "MedCodeId", "observation",
      "peripheral_neuropathies", "Peripheral neuropathies", "MedCodeId", "observation",
      "copd", "COPD", "MedCodeId", "observation"
    ) |>
      mutate(
        path = paste0("codelists/Aurum/", codevar, "/", name, " codelist.csv"),
        full = map(path, ~ read_csv(.x, col_types = cols(.default = "c"))),
        codes = map2(path, codevar, ~ read_csv(.x, col_types = cols(.default = "c"))[[.y]]),
        sum_obs = map_vec(full, \(x) x |>
          pull(any_of(c("Observations", "DrugIssues"))) |>
          as.numeric() |>
          sum()),
        icd_path = paste0("codelists/HES/", name, " icd-10 codelist.csv"),
        icd_full = map(icd_path, ~ read_csv(.x, col_types = cols(.default = "c"))),
        icd_codes = map2(icd_path, "code", ~ read_csv(.x, col_types = cols(.default = "c"))[[.y]])
      )
  ),
  tar_target(outcome, codelists$name),
  tar_target(n_outcomes, length(outcome)),

  # ┠ Dummy data --------------------------------------------------------------

  tar_target(dummy_define, make_dummy_define_aurum(codelists_for_define)),
  tar_target(dummy_extract, make_dummy_extract_aurum(codelists, bind_rows(cohort_matched), outcome)),
  tar_target(dummy_outcome,
    tibble(
      patid = sample(cohort_defined$patid, nrow(cohort_defined) / 2),
      obsdate = as_date(sample(study_start:study_end, nrow(cohort_defined) / 2, replace = TRUE)),
      MedCodeId = 1
    ),
    pattern = slice(cohort_defined, 1)
  ),


  # Data management ---------------------------------------------------------

  # ┠ Read denominator and define------------------------------------------------

  tar_target(
    denominator,
    open_dataset(denominator_file) |>
      filter(patid %in% hes_eligible_patids) |>
      collect(),
    format = "parquet"
  ),
  tar_target(yobs, denominator |> collapse::fselect(patid, yob), format = "parquet"),
  tar_target(
    exposed_obs_defined,
    open_dataset(define_obs_files) |>
      filter(patid %in% hes_eligible_patids) |>
      select(patid, pracid, obsdate, medcodeid) |> collect() |>
      mutate(obsdate = as.Date(obsdate, "%d/%m/%Y")),
    format = "parquet"
  ),
  tar_target(exposed_drug_defined,
    open_dataset(define_drug_files) |>
      filter(patid %in% hes_eligible_patids) |>
      select(patid, prodcodeid, issuedate, dosageid, quantity, quantunitid, duration) |> collect() |>
      mutate(issuedate = as.Date(issuedate, "%d/%m/%Y")),
    format = "parquet"
  ),

  # ┠ Read linked data -----------------------------------------------
  tar_target(
    hes_diagnosis,
    if (dummy_define) {
      read_parquet(paste0(path_linked_data, "hes_diagnosis_epi_23_002665_DM.parquet")) |>
        ftransform(epistart = as.Date(epistart, "%d/%m/%Y")) |>
        ftransform(epiend = as.Date(epiend, "%d/%m/%Y"))
    },
    format = "parquet"
  ),
  tar_target(hes_diagnosis_eczema,
    hes_diagnosis |>
      fsubset(ICD %in% c("L20", "L20.0", "L20.8", "L20.9")) |>
      mutate(epistart = as.Date(epistart, "%d/%m/%Y")),
    format = "parquet"
  ),
  tar_target(
    hes_dates_all,
    hes_diagnosis |> fselect(patid, epistart) |> funique.data.frame(),
    format = "parquet"
  ),
  tar_target(
    hes_dates_first,
    hes_dates_all |> roworder(epistart) |> fgroup_by(patid) |> ffirst(),
    format = "parquet"
  ),
  tar_target(ons_death, if (dummy_define) read_tsv_arrow(paste0(path_linked_data, "death_patient_23_002665_DM.txt"), schema = schema(patid = string(), dod = string()), skip = 1)),
  tar_target(imd_patient, if (dummy_define) read_tsv_arrow(paste0(path_linked_data, "patient_2019_imd_23_002665.txt"), schema = schema(patid = string(), pracid = int64(), e2019_imd_5 = int64()), skip = 1)),
  tar_target(imd_practice, if (dummy_extract) read_tsv_arrow(paste0(path_linked_data, "practice_imd_23_002665.txt"))),


  # ┠ Make HES Hierarchy ---------------------------------------------
  # Categories
  tar_target(
    hes_categories,
    open_dataset(paste0(path_linked_data, "hes_diagnosis_epi_23_002665_DM.parquet")) |>
      select(ICD) |>
      distinct() |>
      mutate(category = str_sub(ICD, 1, 3)) |>
      arrange(category) |>
      collect() |>
      group_by(category) |>
      targets::tar_group(),
    iteration = "group"
  ),
  tar_target(hes_outcomes, unique(hes_categories$category)),
  tar_target(
    hes_eventdata,
    open_dataset(paste0(path_linked_data, "hes_diagnosis_epi_23_002665_DM.parquet")) |>
      select(patid, epistart, ICD) |>
      filter(ICD %in% hes_categories$ICD) |>
      filter(epistart != "") |>
      mutate(epistart = as.Date(epistart, "%d/%m/%Y")) |>
      collect(),
    pattern = map(hes_categories),
    iteration = "list",
    format = "parquet",
    deployment = "main"
  ),

  # GBD codes
  tar_target(gbd_outcomes, unique(gbd_codes$gbdcode)),
  tar_target(gbd_patterns, gbd_codes |> group_by(gbdcode) |> summarise(patterns = paste(icd10, collapse = "|")) |> pull(patterns)),
  tar_target(
    gbd_eventdata,
    open_dataset(paste0(path_linked_data, "hes_diagnosis_epi_23_002665_DM.parquet")) |>
      select(patid, epistart, ICD) |>
      filter(str_starts(ICD, gbd_patterns)) |>
      filter(epistart != "") |>
      mutate(epistart = as.Date(epistart, "%d/%m/%Y")) |>
      collect(),
    pattern = map(gbd_patterns),
    iteration = "list",
    format = "parquet",
    deployment = "main"
  ),

  # phecodes
  tar_target(phecode_outcomes, unique(phecodes$phecode)),
  tar_target(phecode_patterns, phecodes |> group_by(phecode) |> summarise(patterns = paste(icd10, collapse = "|")) |> pull(patterns)),
  tar_target(
    phecode_eventdata,
    open_dataset(paste0(path_linked_data, "hes_diagnosis_epi_23_002665_DM.parquet")) |>
      select(patid, epistart, ICD) |>
      filter(str_starts(ICD, phecode_patterns)) |>
      filter(epistart != "") |>
      mutate(epistart = as.Date(epistart, "%d/%m/%Y")) |>
      collect(),
    pattern = map(phecode_patterns),
    iteration = "list",
    format = "parquet",
    deployment = "main"
  ),


  # ┠ Define Exposed ------------------------------------------------

  tar_target( # Define a cohort of people that have eczema & 2 prescriptions for eczema and join the denominator
    cohort_eczema_all,
    define_cohort_eczema(
      codelists_for_define,
      exposed_obs_defined,
      exposed_drug_defined,
      study_start,
      study_end
    ),
    format = "parquet"
  ),
  tar_target(
    cohort_eczema_adults,
    yobs |>
      left_join(cohort_eczema_all, by = "patid") |>
      collapse::ftransform(exposed_date = pmax(exposed_date, as.Date(paste0(yob + 18, "-06-01")))) |>
      collapse::fsubset(!is.na(exposed_date), -yob),
    format = "parquet"
  ),
  tar_target(
    cohort_eczema_older_adults,
    yobs |>
      left_join(cohort_eczema_all, by = "patid") |>
      collapse::ftransform(exposed_date = pmax(exposed_date, as.Date(paste0(yob + 40, "-06-01")))) |>
      collapse::fsubset(!is.na(exposed_date), -yob),
    format = "parquet"
  ),
  tar_target(
    cohort_eczema_children,
    yobs |>
      left_join(cohort_eczema_all, by = "patid") |>
      fsubset(exposed_date < as.Date(paste0(yob + 18, "-06-01")), -yob),
    format = "parquet"
  ),
  tar_target(
    cohort_eczema_hosp,
    cohort_eczema_all |>
      join(hes_dates_first, on = "patid", how = "left", multiple = FALSE) |>
      fsubset(!is.na(epistart)),
    format = "parquet"
  ),
  tar_target(cohort_defined,
    list(cohort_eczema_all, cohort_eczema_adults, cohort_eczema_older_adults, cohort_eczema_children, cohort_eczema_hosp),
    iteration = "list"
  ),
  tar_target(
    cohort_labels,
    c("eczema_all", "eczema_18", "eczema_40", "eczema_child", "eczema_hosp")
  ),


  # ┠ Create Cohort ------------------------------------------------------------

  tar_target(
    cohort_eligible,
    create_cohort_eligible(denominator, cohort_defined, study_start, study_end, cohort_labels, hes_dates_first),
    pattern = map(cohort_defined, cohort_labels),
    iteration = "list",
    format = "parquet"
  ),
  tar_target(
    cohort_matchable,
    create_cohort_matchable(cohort_eligible),
    pattern = cohort_eligible,
    iteration = "list",
    format = "parquet"
  ),
  tar_target(
    cohort_matchable_grouped,
    split(cohort_matchable, cohort_matchable$pracid),
    pattern = cohort_matchable,
    iteration = "list"
  ),
  tar_target(
    cohort_matched,
    create_cohort_matched(cohort_matchable_grouped),
    pattern = cohort_matchable_grouped,
    iteration = "list",
    format = "parquet",
    deployment = "main"
  ),


  # ┠ Extract eventdata -------------------------------------------------------

  tar_target( # Get eventdata for every codelist
    eventdata,
    open_dataset(files) |>
      select(patid, obsdate, medcodeid) |>
      filter(medcodeid %in% unlist(codelists$codes)) |>
      arrange(obsdate) |>
      collect() |>
      collapse::ftransform(obsdate = as.Date(obsdate, "%d/%m/%Y")) |>
      collapse::fsubset(obsdate >= study_start & obsdate <= study_end),
    pattern = map(codelists),
    iteration = "list",
    format = "parquet"
  ),
  tar_target( # Get eventdata for all drug codelists
    drug_eventdata,
    exposed_drug_defined |>
      fsubset(patid %in% patids_from_all_cohorts) |>
      fsubset(prodcodeid %in% unlist(drug_codelists$codes)) |>
      roworder(issuedate) |>
      fsubset(issuedate >= study_start & issuedate <= study_end),
    pattern = map(drug_codelists),
    iteration = "list",
    format = "parquet"
  ),


  # ┠ Create analysis cohort --------------------------------------------------

  tar_target(
    pre_index_vars,
    create_pre_index_vars(cohort_matched, eventdata, codelists),
    pattern = cohort_matched,
    iteration = "list",
    format = "parquet",
    deployment = "main"
  ),
  tar_target(
    hes_vars_table,
    crosstab_hes_vars(cohort_wide, cohort_labels, hes_eventdata, hes_outcomes),
    pattern = map(cohort_wide, cohort_labels),
    format = "parquet",
    deployment = "main"
  ),
  tar_target(
    drug_vars,
    create_pre_index_drug_vars(cohort_matched, drug_eventdata, drug_codelists),
    pattern = cohort_matched,
    iteration = "list",
    format = "parquet",
  ),
  tar_target(
    obs_dates,
    open_dataset(define_non_disease_files) |>
      filter(medcodeid %in% unlist(non_disease_codelist$codes)) |>
      filter(patid %in% patids_from_all_cohorts) |>
      select(patid, obsdate) |>
      distinct() |>
      mutate(obsdate = as.Date(obsdate, "%d/%m/%Y")) |>
      collect(),
    pattern = define_non_disease_files,
    format = "parquet"
  ),
  tar_target(
    obs_dates_unique,
    funique.data.frame(obs_dates),
    format = "parquet"
  ),
  tar_target(
    cons_in_year_pre_index,
    alg_cons_in_year_pre_index(cohort_matched, obs_dates_unique),
    pattern = cohort_matched,
    iteration = "list",
    format = "parquet",
  ),
  tar_target(
    cohort_wide,
    bind_cols(cohort_matched, pre_index_vars, drug_vars, cons_in_year_pre_index),
    pattern = map(cohort_matched, pre_index_vars, drug_vars, cons_in_year_pre_index),
    iteration = "list",
    format = "parquet"
  ),
  tar_target(
    cohort_wide_sample,
    sample_frac(cohort_wide, 0.5) |> fsubset(cons_in_year_pre_index == TRUE),
    pattern = map(cohort_wide),
    iteration = "list",
    format = "parquet"
  ),


  # Analysis -------------------------------------------------------------------

  # ┠ Hazard ratios, rates and follow-up-----------------------------------------------------------------

  tar_target(
    results,
    analysis_results(hes_eventdata, hes_outcomes, cohort_ps, cohort_labels, exposure, model, n_outcomes),
    pattern = cross(
      map(hes_eventdata, hes_outcomes),
      map(cohort_ps, cohort_labels)
    ),
    format = "parquet"
  ),
  tar_target(
    results_selected,
    analysis_results(hes_eventdata, hes_outcomes, cohort_ps, cohort_labels, exposure, model, n_outcomes),
    pattern = slice(cross(
      map(hes_eventdata, hes_outcomes),
      map(cohort_ps, cohort_labels)
    ), c(431, 435, 1131, 1135, 1231, 1235, 2086, 2090, 2611, 2615, 2866, 2870, 3206, 3210, 3236, 3240, 3376, 3380, 3726, 3730, 4056, 4060, 4291, 4295, 4681, 4685, 4896, 4900, 6461, 6465, 7836, 7840)),
    format = "parquet"
  ),
  tar_target(
    results_excl,
    analysis_results(hes_eventdata, hes_outcomes, cohort_ps_excl, cohort_excl_labels, exposure, model, n_outcomes),
    pattern = cross(
      map(hes_eventdata, hes_outcomes),
      map(cohort_ps_excl, cohort_excl_labels)
    ),
    format = "parquet"
  ),
  tar_target(
    results_excl_selected,
    analysis_results(hes_eventdata, hes_outcomes, cohort_ps_excl, cohort_excl_labels, exposure, model, n_outcomes),
    pattern = slice(cross(
      map(hes_eventdata, hes_outcomes),
      map(cohort_ps_excl, cohort_excl_labels)
    ), c(431, 435, 1131, 1135, 1231, 1235, 2086, 2090, 2611, 2615, 2866, 2870, 3206, 3210, 3236, 3240, 3376, 3380, 3726, 3730, 4056, 4060, 4291, 4295, 4681, 4685, 4896, 4900, 6461, 6465, 7836, 7840)),
    format = "parquet"
  ),
  tar_target(
    results_gbd,
    analysis_results(gbd_eventdata, gbd_outcomes, cohort_ps, cohort_labels, exposure, model, n_outcomes),
    pattern = cross(
      map(gbd_eventdata, gbd_outcomes),
      map(cohort_ps, cohort_labels)
    ),
    format = "parquet"
  ),
  tar_target(
    results_phecode,
    analysis_results(phecode_eventdata, phecode_outcomes, cohort_ps, cohort_labels, exposure, model, n_outcomes),
    pattern = cross(
      map(phecode_eventdata, phecode_outcomes),
      map(cohort_ps, cohort_labels)
    ),
    format = "parquet"
  ),

  # ┠ Propensity score----------------------------------------------------------------

  tar_target(
    propensity_score_model,
    lm(as.formula(paste("exposed ~ ", paste(outcome, collapse = " + "))), data = cohort_wide),
    pattern = cohort_wide,
    iteration = "list"
  ),
  tar_target(
    propensity_score,
    predict(propensity_score_model, type = "response"),
    pattern = propensity_score_model,
    iteration = "list"
  ),
  tar_target(
    cohort_ps,
    cohort_matched |> mutate(ps = as.double(propensity_score)),
    pattern = map(cohort_matched, propensity_score),
    iteration = "list",
    format = "parquet"
  ),
  tar_target(
    cohort_ps_sample,
    sample_frac(cohort_ps, 0.3),
    pattern = map(cohort_ps),
    iteration = "list",
    format = "parquet"
  ),
  tar_target(
    cohort_ps_excl,
    cohort_ps |> bind_cols(cons_in_year_pre_index) |> fsubset(cons_in_year_pre_index == TRUE),
    pattern = map(cohort_ps, cons_in_year_pre_index),
    iteration = "list",
    format = "parquet"
  ),
  tar_target(
    cohort_excl_labels,
    c("eczema_all_excl", "eczema_18_excl", "eczema_40_excl", "eczema_child_excl", "eczema_hosp_excl")
  ),



  # ┠ Baseline & Flow ------------------------------------------------------------------

  tar_target(
    cohort_flow,
    analysis_cohort_flow(denominator, cohort_defined, cohort_labels, cohort_matched, cohort_eligible),
    pattern = map(cohort_defined, cohort_labels, cohort_matched, cohort_eligible)
  ),
  tar_target(
    baseline_chars,
    cohort_wide |>
      left_join(denominator[c("patid", "gender", "region", "yob")], by = "patid") |>
      mutate(
        N = 1,
        indexdate_num = as.numeric(indexdate),
        enddate_num = as.numeric(enddate),
        age_at_index = year(indexdate) - yob,
        futime_baseline = (enddate_num - indexdate_num) / 365.25
      ) |>
      select(N, gender, age_at_index, yob, region, futime_baseline, everything(), -patid, -setid) |>
      tbl_summary(by = exposed) |>
      modify_header(all_stat_cols() ~ "{level}") |>
      add_difference(everything() ~ "smd") |>
      as_tibble() |>
      mutate(cohort = cohort_labels),
    pattern = map(cohort_wide, cohort_labels),
    iteration = "vector",
  ),
  tar_target(
    age_dist,
    cohort_wide[c("patid", "exposed", "indexdate")] |>
      fsubset(exposed == 1) |>
      join(denominator[c("patid", "yob")], on = "patid", how = "left", multiple = FALSE) |>
      ftransform(age_at_index = year(indexdate) - yob) |>
      fcount(age_at_index) |>
      roworder(age_at_index) |>
      mutate(cohort = cohort_labels),
    pattern = map(cohort_wide, cohort_labels)
  ),
  tar_target(
    fu_dist,
    cohort_wide[c("patid", "exposed", "indexdate", "enddate")] |>
      fsubset(exposed == 1) |>
      ftransform(indexdate_num = as.numeric(indexdate)) |>
      ftransform(enddate_num = as.numeric(enddate)) |>
      ftransform(futime_baseline = (enddate_num - indexdate_num) / 365.25) |>
      fcount(futime_baseline) |>
      roworder(futime_baseline) |>
      mutate(cohort = cohort_labels),
    pattern = map(cohort_wide, cohort_labels)
  ),

  # ┠ Checks ------------------------------------------------------------------

  tar_target( # Make lists of the most common codes for each eventdata
    common_codes,
    map2(eventdata, codelists$full, .progress = TRUE, \(x, y)
    x |>
      group_by(medcodeid) |>
      tally() |>
      left_join(y[c("MedCodeId", "Term")], by = c("medcodeid" = "MedCodeId")) |>
      arrange(desc(n)) |>
      filter(n > 10)) |>
      set_names(codelists$name) |>
      bind_rows(.id = "column_label")
  ),
  tar_target(
    common_codes_drugs,
    map(codelists_for_define[which(codelists_for_define$codevar == "ProdCodeId"), ]$full, \(x)
    x |>
      left_join(exposed_drug_defined, by = c("ProdCodeId" = "prodcodeid")) |>
      group_by(termfromemis) |>
      tally() |>
      arrange(desc(n)) |>
      filter(n > 10))
  ),
  tar_target(
    common_codes_aurum,
    map(
      codelists$full,
      \(x) x |>
        mutate(across(any_of(c("Observations", "DrugIssues")), as.numeric)) |>
        arrange(desc(pick(any_of(c("Observations", "DrugIssues"))))) |>
        mutate(across(any_of(c("Observations", "DrugIssues")), \(x) if_else(is.na(x), 0, x))) |>
        mutate(running_perc = cumsum(pick(any_of(c("Observations", "DrugIssues")))) /
          sum(pick(any_of(c("Observations", "DrugIssues"))))) |>
        unnest_wider(running_perc, names_sep = "_") |>
        rename(any_of(c(running_perc = "running_perc_Observations", running_perc = "running_perc_DrugIssues"))) |>
        filter(running_perc <= 0.9 | row_number() < 6) |>
        select(any_of(c(
          "running_perc", "Observations", "Term", "DrugIssues", "Term from EMIS",
          "ProductName", "Formulation", "RouteOfAdministration", "DrugSubstanceName"
        )))
    ) |>
      set_names(str_sub(codelists$name, end = 31))
  ),

  # Write outputs -----------------------------------------------------------

  # ┠ for Define----------------------------------------------------

  # Codelists as single line for define
  tar_target(psoriasis_medcodes_for_define, codelists_for_define |> filter(name == "psoriasis") |> pull(codes) |> unlist()),
  tar_file(psoriasis_medcodes_for_define_file, write_single_line_and_return_path(psoriasis_medcodes_for_define)),
  tar_target(non_diseases_medcodes_for_define, non_disease_codelist |> pull(codes) |> unlist()),
  tar_file(non_diseases_medcodes_for_define_file, write_single_line_and_return_path(non_diseases_medcodes_for_define)),
  tar_target(eczema_medcodes_for_define, codelists_for_define |> filter(name == "eczema") |> pull(codes) |> unlist()),
  tar_file(eczema_medcodes_for_define_file, write_single_line_and_return_path(eczema_medcodes_for_define)),
  tar_target(phototherapy_medcodes_for_define, codelists_for_define |> filter(name == "phototherapy") |> pull(codes) |> unlist()),
  tar_file(phototherapy_medcodes_for_define_file, write_single_line_and_return_path(phototherapy_medcodes_for_define)),
  tar_target(eczemaRx_prodcodes_for_define, codelists_for_define |>
    filter(name %in% c(
      "oral_glucocorticoids",
      "emollients",
      "topical_glucocorticoids",
      "systemic_immunosupressants",
      "topical_calcineurin_inhibitors"
    )) |>
    pull(codes) |> unlist() |> unique()),
  tar_file(eczemaRx_prodcodes_for_define_file, write_single_line_and_return_path(eczemaRx_prodcodes_for_define)),
  tar_target(psoriasisSysRx_prodcodes_for_define, codelists_for_define |> filter(name == "systemic_treatments_for_psoriasis") |> pull(codes) |> unlist() |> unique()),
  tar_file(psoriasisSysRx_prodcodes_for_define_file, write_single_line_and_return_path(psoriasisSysRx_prodcodes_for_define)),
  tar_target(all_medcodes_for_define, codelists_for_define[which(codelists_for_define$codevar == "MedCodeId"), ] |> pull(codes) |> unlist() |> unique()),
  tar_file(all_medcodes_for_define_file, write_single_line_chunked_and_return_path(all_medcodes_for_define, max = 3000)),
  tar_target(all_prodcodes_for_define, codelists_for_define[which(codelists_for_define$codevar == "ProdCodeId"), ] |> pull(codes) |> unlist() |> unique()),
  tar_file(all_prodcodes_for_define_file, write_single_line_chunked_and_return_path(all_prodcodes_for_define, max = 3000)),
  tar_target(medcodes_chunked_by_observations, write_medcodes_chunked_by_observations_and_return_path(codelists, maxfiles = 20)),

  # ┠ for Extract-----------------------------------------------------
  tar_target(patids_from_all_cohorts, cohort_matched |> bind_rows() |> pull(patid) |> unique()),
  tar_target(patids_for_extract,
    cohort_matched[1],
    pattern = cohort_matched, # Combine patids from all matched cohorts
    iteration = "vector"
  ),
  tar_target(patids_for_extract_unique, patids_for_extract |> filter(!duplicated(patid))),
  tar_file(patids_for_extract_file, write_delim_and_return_path(patids_for_extract_unique)),
  tar_target(patids_for_extract_chunked_files, write_delim_compressed_in_chunks_and_return_paths(patids_for_extract_unique, max = 1000000)),

  # ┠ for Linkage----------------------------------------------------
  tar_target(hes_eligible_patids, hes_eligibility |> filter(hes_apc_e == 1 & ons_death_e == 1) |> pull(patid) |> as.character()),
  tar_target(hes_eligible_pats, patids_for_extract_unique |>
    left_join(hes_eligibility_all, by = "patid") |>
    fselect(patid, hes_apc_e, ons_death_e, lsoa_e) |>
    fsubset(hes_apc_e == 1) |>
    fsubset(ons_death_e == 1)),
  tar_file(hes_eligible_pats_file, write_delim_and_return_path(hes_eligible_pats)),
  tar_target(hes_eligible_pats_chunked_files, write_delim_chunked_and_return_path(hes_eligible_pats, 5000000)),


  # ┠ Results----------------------------------------------------

  # Tables as .csv
  tar_file(results_file, write_and_return_path(results)),
  tar_file(results_excl_file, write_and_return_path(results_excl)),
  tar_file(results_excl_selected_file, write_and_return_path(results_excl_selected)),
  tar_file(results_selected_file, write_and_return_path(results_selected)),
  tar_file(results_gbd_file, write_and_return_path(results_gbd)),
  tar_file(results_phecode_file, write_and_return_path(results_phecode)),


  # tar_file(results_chapters_file, write_and_return_path(results_chapters)),
  # tar_file(results_sections_file, write_and_return_path(results_sections)),
  # tar_file(results_long_file, write_and_return_path(results_long)),
  tar_file(common_codes_file, write_and_return_path(common_codes)),
  tar_file(cohort_flow_file, write_and_return_path(cohort_flow)),
  tar_file(baseline_chars_file, write_and_return_path(baseline_chars)),
  tar_file(age_dist_file, write_and_return_path(age_dist)),
  tar_file(fu_dist_file, write_and_return_path(fu_dist)),
  tar_target(hes_vars_table_redacted, hes_vars_table |> group_by(cohort, var) |> mutate(redact = if_else(any(n < 20), TRUE, FALSE)) |> ungroup() |> mutate(n = ifelse(redact, NA, n))),
  tar_files(hes_vars_table_file, write_and_return_path(hes_vars_table_redacted)),

  # Vectors as text
  tar_file(exposure_file, write_lines_and_return_path(exposure)),
  tar_file(outcome_file, write_lines_and_return_path(outcome)),
  tar_file(model_file, write_lines_and_return_path(model)),
  tar_file(model_names_file, write_lines_and_return_path(names(model))),

  # Checks as Excel files
  tar_file(common_codes_aurum_file, write_xlsx_and_return_path(common_codes_aurum))
)
