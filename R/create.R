#' Extract a cohort of people with eczema
#' @description 
#' Get the first date someone has at least 1 diagnosis for eczema 
#' and 2 prescriptions for eczema on two seperate days.
#' @references 
#' Abuabara K, Magyari AM, Hoffstad O, Jabbar-Lopez ZK, Smeeth L, Williams HC, Gelfand JM, Margolis DJ, Langan SM. Development and Validation of an Algorithm to Accurately Identify Atopic Eczema Patients in Primary Care Electronic Health Records from the UK. J Invest Dermatol. 2017 Aug;137(8):1655-1662. doi: 10.1016/j.jid.2017.03.029. Epub 2017 Apr 18. PMID: 28428130; PMCID: PMC5883318.
define_cohort_eczema <- function(codelists_for_define,
																 exposed_obs_defined,
																 exposed_drug_defined,
																 study_start,
																 study_end) {
	
	#Make list of codes for eczema prescriptions
	eczemaRx_codes <- unlist(codelists_for_define$codes[which(codelists_for_define$name %in% 
																								 	c("oral_glucocorticoids",
																								 		"emollients",
																								 		"topical_glucocorticoids",
																								 		"systemic_immunosupressants",
																								 		"topical_calcineurin_inhibitors"))])
	
	#Get the first record for eczema for each person, if one exists
	eczema_obs <- exposed_obs_defined |> 
		select(patid, obsdate, medcodeid) |> 
		filter(medcodeid %in% codelists_for_define$codes[[which(codelists_for_define$name=="eczema")]],
					 obsdate>=as.Date(study_start),
					 obsdate<=as.Date(study_end)) |>
		arrange(obsdate) |> 
		filter(!duplicated(patid)) |> 
		select(patid, eczema_date=obsdate)
	
	#Get the second prescription for eczema, on separate days, for each person, if it exists
	eczemaRx_obs <- exposed_drug_defined |> 
		select(patid, issuedate, prodcodeid) |> 
		filter(prodcodeid %in% eczemaRx_codes,
					 issuedate>=as.Date(study_start),
					 issuedate<=as.Date(study_end),
					 patid %in% eczema_obs$patid) |> 
		arrange(issuedate) |> 
		group_by(patid) |> 
		filter(!duplicated(issuedate)) |> 
		slice(2) |> 
		ungroup() |>
		select(patid, second_prescription_date=issuedate)
	
	#Get the latest of the eczema diagnosis and the second prescription
	eczema_obs |> 
		left_join(eczemaRx_obs, by="patid") |>  
		mutate(exposed_date=pmax(eczema_date, second_prescription_date)) |> 
		filter(!is.na(exposed_date)) |> 
		select(patid, exposed_date)
}


#' Extract a cohort of people with moderate to severe eczema
#' @description
#' Takes an already defined cohort of people with eczema and identifies those that get phototherapy or a prescription indicating more severe eczema
define_cohort_mod_sev_eczema <- function(cohort_eczema_all,
																				 codelists_for_define,
																				 exposed_obs_defined,
																				 exposed_drug_defined,
																				 study_start,
																				 study_end) {
	
	#Get codes for phototherapy
	phototherapy_medcodes <- codelists_for_define$codes[[which(codelists_for_define$name=="phototherapy")]]
	
	#Pull codes for potent topical steroids
	pot_top_cs_prodcodes <- codelists_for_define$full[[which(codelists_for_define$name=="topical_glucocorticoids")]] |> 
		filter(potency %in% c("potent", "very potent")) |> 
		pull(ProdCodeId)
	
	#Make list of codes for prescriptions for moderate to severe eczema (topical calcineurin inhibitors, systemic immunosupressants and potent and very potent topical steroids)
	mod_sev_eczemaRx_prodcodes <- c(codelists_for_define$codes[[which(codelists_for_define$name=="topical_calcineurin_inhibitors")]],
																	codelists_for_define$codes[[which(codelists_for_define$name=="systemic_immunosupressants")]],
																	pot_top_cs_prodcodes)
	
	#Get all record for phototherapy
	phototherapy_obs <- exposed_obs_defined |> 
		select(patid, obsdate, medcodeid) |> 
		filter(medcodeid %in% phototherapy_medcodes,
					 obsdate>=as.Date(study_start),
					 obsdate<=as.Date(study_end),
					 patid %in% cohort_eczema_all$patid) |> 
		arrange(obsdate) |> 
		select(patid, mod_sev_date=obsdate)
	
	#Get all prescription for drugs indicating moderate to severe eczema for each exposed person, if it exists
	eczemaRx_mod_sev_obs <- exposed_drug_defined |> 
		select(patid, issuedate, prodcodeid) |> 
		filter(prodcodeid %in% mod_sev_eczemaRx_prodcodes,
					 issuedate>=as.Date(study_start),
					 issuedate<=as.Date(study_end),
					 patid %in% cohort_eczema_all$patid) |> 
		arrange(issuedate) |> 
		select(patid, mod_sev_date=issuedate)
	
	#Bind phototherapy and prescription codes
	mod_sev_obs <- bind_rows(phototherapy_obs, eczemaRx_mod_sev_obs) |> 
		arrange(mod_sev_date) |> 
		filter(!duplicated(mod_sev_date), .by=patid)
	
	
	#Define people as exposed (having moderate-to-severe eczema) on the date of the first phototherapy or prescription for a drug indicating moderate to severe eczema, after meeting the eczema algortihm
	cohort_eczema_all |>  
		left_join(mod_sev_obs, by="patid") |>  
		filter(mod_sev_date>=exposed_date) |> 
		slice(1, .by = patid) |> 
		mutate(exposed_date=pmax(exposed_date, mod_sev_date)) |> 
		filter(!is.na(exposed_date)) |> 
		select(patid, exposed_date)

}


#' Create a cohort eligible for matching
#' @param cohort_defined A cohort with one row per person
#' @description
#' A short description...
#' 
create_cohort_eligible <- function(denominator, cohort_defined, study_start, study_end, cohort_labels, hes_dates_first) {
	
	# For cohort where people are only eligible a year from their first hospital record, filter the denominator
	if (cohort_labels=="eczema_hosp") {
		denominator <- denominator |> 
			join(hes_dates_first, on="patid", how="left", multiple = FALSE) |> 
			fsubset(!is.na(epistart)) |> 
			ftransform(regstartdate=pmax(regstartdate, epistart)) }
	
	denominator |> 
		join(cohort_defined, on="patid", how="left", multiple=FALSE) |> 
		# collapse::ftransform(dob=as.Date(paste0(yob, "-06-01"))) |> 
		# #Adult date: 18 years old
		# collapse::ftransform(adult_date=as.Date(paste0(yob+18, "-06-01"))) |> 
		#Date of Eligibility for matching: latest of: practice registration date plus 1 year, study start date (January 1, 1998). There is no up-to-standard date in CPRD Aurum (https://cprd.com/sites/default/files/2022-02/CPRD%20Aurum%20FAQs%20v2.2.pdf)
		ftransform(startdate=pmax(regstartdate+365, as.Date(study_start))) |> 
		#Not used: Adult date of eligibility for matching: latest of: startdate or adult date
		# collapse::ftransform(startdate=pmax(startdate, adult_date)) |> 
		#Enddate: earliest of: end of registration, date of death, date of the last data collection from the practice, or the end of the study (31st January 2020).
		ftransform(enddate=pmin(regenddate, cprd_ddate, lcd, as.Date(study_end), na.rm = TRUE)) |> 
		#Indexdate: latest of being exposed and startdate
		ftransform(indexdate=pmax(exposed_date, startdate)) |> 
		#Set indexdates to NA that occur after the enddate
		ftransform(indexdate=if_else(indexdate>=enddate, NA_Date_, indexdate)) |> 
		#Keep only people with follow-up
		fsubset(enddate > startdate) |> 
		fselect(patid, yob, gender, pracid, indexdate, startdate, enddate, exposed_date)
}


# variables required in memory before running (correctly named):
# patid:          CPRD patient id [nb if using Aurum data, patid MUST be stored as a "double" precision variable]
# indexdate:      date of "exposure" for exposed patients (missing for potential controls)
# gender:         gender, numerically coded (e.g. 1=male, 2=female)
# startdate:      date of start of CPRD follow-up
# enddate:        date of end of follow-up as a potential control, generally = end of CPRD follow-up, but see "important note" below
# exposed:        indicator: 1 for exposed patients, 0 for potential controls
# yob:            year of birth
# IMPORTANT NOTE: in most cases it is desirable to allow exposed patients 
# to be available as controls prior to their date of first exposure. Such 
# patients should be included in the dataset twice (i.e. two separate rows):
# once as exposed (exposed = 1, startdate = start of CPRD follow-up, 
# indexdate = date of first exposure, enddate = end of CPRD follow-up),
# and once as a potential control (exposed = 0, startdate = start of CPRD 
# follow-up, indexdate = missing, enddate = date of first exposure-1)
create_cohort_matchable <- function(cohort_eligible) {

	cohort_matchable <- cohort_eligible |> 
		ftransform(exposed=if_else(is.na(indexdate), 0, 1))
	
	# Get exposed patients to be available as controls prior to their date of first exposure
	pre_exposure_ppl <- cohort_matchable |> 
		fsubset(exposed==1) |> 
		mutate(enddate=indexdate-1, indexdate=NA_Date_, exposed=0) |> 
		fsubset(enddate > startdate)
	
	bind_rows(cohort_matchable, pre_exposure_ppl)
}

#' Create matched cohort using sequential trials matching
#' @description each daily trial includes all n eligible people who 
#' become exposed on that day (exposed=1) and
#' a sample of n eligible controls (exposed=0) who:
#' - had not been exposed on or before that day (still at risk of becoming exposed);
#' - still at risk of an outcome (not left the study); 
#' - had not already been selected as a control in a previous trial
#' @param grouped_cohort_to_match A data frame with one or two rows per participant with:
#'  1. $patid: The patient ID
#'  2. $startdate: the date people become eligible 
#'  3. $indexdate: the date eligible people got exposed 
#'  4. $enddate: the date people leave the study 
#'  @param dayspriorreg days prior registration required for controls
create_cohort_matched <- function(cohort_matchable_grouped, dayspriorreg=0) {
	
	#Map across every practice group
	library(future)
	plan(multisession, workers = 8)
	furrr::future_map_dfr(cohort_matchable_grouped, .progress = TRUE, \(x) {

	#Sort by indexdate, and then randomly
	cohort_matchable <- x |>
		ftransform(sortunique=runif(nrow(x))) |> 
		roworder(exposed, indexdate, sortunique) |> 
		fselect(-sortunique)

		exposed <- cohort_matchable |> fsubset(exposed==1) |> ftransform(setid=patid)
		unexposed <- cohort_matchable |> fsubset(exposed==0)
		
		matched <- exposed[0,] #Make empty dataframe with same columns to be filled
		
		#Loop through all people that ever get exposed (each one gets matched to people who are unexposed at the same time)
		for (i in 1:nrow(exposed)) {
			
			exposed_pat <- exposed[i,]
			matchday <- exposed_pat$indexdate
			
			#Drop people that can't be matched anymore (either because they have already been matched or they have passed the study end date)
			unexposed <- unexposed |> 
				fsubset(enddate > matchday & !(patid %in% matched$patid))
			
			#Perform matching
			new <- unexposed |> 
				fsubset(gender==exposed_pat$gender) |> 
				fsubset((startdate+dayspriorreg) <= matchday) |> 
				ftransform(age_difference=abs(yob-exposed_pat$yob)) |> 
				fsubset(age_difference<=2) |> 
				roworder(age_difference) |> # the closest matches are given priority
				slice(1:5) |> 
				ftransform(setid=exposed_pat$patid,
									 indexdate=as_date(matchday)) |>  #Set the indexdate for everyone to the day the exposed individual got exposed
				fselect(-age_difference)
			if (nrow(new)>0) matched <- bind_rows(matched, exposed_pat, new)
			# cli::cli_progress_update()
		}
		matched |> 
			roworder(setid, -exposed) |> 
			select(patid, exposed, indexdate, enddate, setid)})
}

#' Create pre-index event variables
#' @return A of dataframes each with one row per patient, with patient ID and a TRUE/FALSE variable if they had the event in the variable name prior to index date
create_pre_index_vars <- function(cohort, eventdata, codelists) {
	map2_dfc(eventdata, codelists$name, .progress=TRUE, \(eventdata, codelist_name) {
	# Get the first occuring event for each patient (that has an event)
	first_event <- eventdata |> 
		fselect(patid, obsdate) |> 
		fgroup_by(patid) |> 
		fmin()
	
	# Join the first events to the cohort of all patients
	temp <- cohort[c("patid", "exposed", "indexdate")] |> 
		left_join(first_event, by=c("patid"))
	
	stopifnot(identical(temp[c("patid", "exposed")],cohort[c("patid", "exposed")]))
	
	# Make a variable that is TRUE when the first event occur before the index date
	temp[codelist_name] <- ifelse(temp$obsdate < temp$indexdate, TRUE, FALSE)
	temp[codelist_name][is.na(temp[codelist_name])] <- FALSE
	temp[codelist_name]
	})
	
}

create_pre_index_hes_vars <- function(cohort, hes_eventdata, hes_outcomes) {
	map2_dfc(hes_eventdata, hes_outcomes, .progress=TRUE, \(hes_eventdata, hes_outcomes) {
	# Get the first occuring event for each patient (that has an event)
	first_event <- hes_eventdata |> 
		fselect(patid, epistart) |> 
		fgroup_by(patid) |> 
		fmin()
	
	# Join the first events to the cohort of all patients
	temp <- cohort[c("patid", "exposed", "indexdate")] |> 
		left_join(first_event, by=c("patid"))
	
	stopifnot(identical(temp[c("patid", "exposed")],cohort[c("patid", "exposed")]))
	
	# Make a variable that is TRUE when the first event occur before the index date
	temp[hes_outcomes] <- ifelse(temp$epistart < temp$indexdate, TRUE, FALSE)
	temp[hes_outcomes][is.na(temp[hes_outcomes])] <- FALSE
	temp[hes_outcomes]
	})
	
}

crosstab_hes_vars <- function(cohort, cohort_labels, hes_eventdata, hes_outcomes) {
	map2_dfr(hes_eventdata, hes_outcomes, .progress=TRUE, \(hes_eventdata, hes_outcomes) {
	# Get the first occuring event for each patient (that has an event)
	first_event <- hes_eventdata |> 
		fselect(patid, epistart) |> 
		roworder(epistart)
	
	# Join the first events to the cohort of all patients
cohort[c("patid", "exposed", "indexdate")] |>
  join(first_event, on = c("patid"), how = "left", multiple = FALSE, verbose = FALSE) |>
  fmutate(recorded_pre_index = ifelse(epistart < indexdate & !is.na(epistart), TRUE, FALSE)) |>
  fselect(exposed, recorded_pre_index) |>
  qtable() |>
  as_tibble() |>
  mutate(var = hes_outcomes, cohort=cohort_labels)

	})
}


#' Create pre-index event variables
#' @return A of dataframes each with one row per patient, with patient ID and a TRUE/FALSE variable if they had the event in the variable name prior to index date
create_pre_index_drug_vars <- function(cohort, drug_eventdata, drug_codelists) {
	map2_dfc(drug_eventdata, drug_codelists$name, .progress=TRUE, \(drug_eventdata, codelist_name) {
		# Get the first occuring event for each patient (that has an event)
		first_event <- drug_eventdata |> 
			fselect(patid, issuedate) |> 
			fgroup_by(patid) |> 
			fmin()
		
		# Join the first events to the cohort of all patients
		temp <- cohort[c("patid", "exposed", "indexdate")] |> 
			left_join(first_event, by=c("patid"))
		
		stopifnot(identical(temp[c("patid", "exposed")],cohort[c("patid", "exposed")]))
		
		# Make a variable that is TRUE when the first event occur before the index date
		temp[codelist_name] <- ifelse(temp$issuedate < temp$indexdate, TRUE, FALSE)
		temp[codelist_name][is.na(temp[codelist_name])] <- FALSE
		temp[codelist_name]
	})
	
}


alg_cons_in_year_pre_index <- function(cohort_matched, obs_dates_unique) {
  
	temp <- cohort_matched[c("patid", "exposed", "indexdate")] |> 
		left_join(obs_dates_unique, by="patid") |> 
		fgroup_by(patid, exposed) |> 
		fsummarise(cons_in_year_pre_index=any(as.integer(indexdate)-as.integer(obsdate)<365.25 &
							as.integer(indexdate)-as.integer(obsdate)>=0, na.rm=TRUE))
		
  
	cohort_matched[c("patid", "exposed")] |> 
    left_join(temp, by=c("patid", "exposed")) |> 
    fselect(cons_in_year_pre_index)
  
}

