# A script to create dummy CPRD style flat files

make_dummy_define_aurum <- function(codelists_for_define, n=200000) {
	library(tidyverse)
	
	# Specifications ----------------------------------------------------------
	
	#Set size
	#Number of patients and rows per file set as function argument
	n_files <- 1:4 #Number of files per filetype
	
	#Set patids & pracids
	pracids <- 100:999
	patids_short <- 1:n
	pracids_sample <- sample(pracids, length(patids_short), replace = TRUE)
	patids <- paste0(patids_short, pracids_sample)
	n_patids <- length(patids)
	
	#Set daterange
	daterange <- as.Date(as.Date("1998-01-02"):as.Date("2020-01-31"), origin = "1970-01-01")
	daterange_head <- head(as.Date(as.Date("1998-01-02"):as.Date("2020-01-31"), origin = "1970-01-01"), length(daterange)/2)
	daterange_tail <- tail(as.Date(as.Date("1998-01-02"):as.Date("2020-01-31"), origin = "1970-01-01"), length(daterange)/2)
	daterange_w_nas <- c(daterange, rep(NA, 10*length(daterange)))
	
	# Process codes -----------------------------------------------------------
	
	#Make lists of all medcodes and prodcodes
	medcodes <- codelists_for_define$codes[which(codelists_for_define$codevar=="MedCodeId")]
	prodcodes <- codelists_for_define$codes[which(codelists_for_define$codevar=="ProdCodeId")]
	
	#If there is only one code then duplicate it to avoid downstream issues with sampling (since sample with an element of length 1 is different than with an element length >1)
	medcodes[which(map(medcodes, length)==1)] <- map(medcodes[which(map(medcodes, length)==1)], ~c(.x, .x))
	prodcodes[which(map(prodcodes, length)==1)] <- map(prodcodes[which(map(prodcodes, length)==1)], ~c(.x, .x))
	
	#Make medcodes that sample evenly (make a sample of each code that is the length of the longest codelist)
	longest_codelist_length <- max(map_int(c(medcodes, prodcodes), length))
	medcodes_even <- unlist(map(medcodes, sample, size=longest_codelist_length, replace=TRUE), use.names = FALSE)
	prodcodes_even <- unlist(map(prodcodes, sample, size=longest_codelist_length, replace=TRUE), use.names = FALSE)
	
	
	
	# Make dummy data ---------------------------------------------------------
	
	#Make sample function where replace=TRUE
	rsample <- function(x, size) {
		sample(x, size, replace = TRUE)
	}
	
	#Make dummy Observation define
			for (i in n_files) {
				tibble(
					patid=rsample(patids, n/length(n_files)), consid=NA_integer_, pracid=NA_character_,
					obsid=NA_character_, obsdate=format(rsample(daterange_head, n/length(n_files)), "%d/%m/%Y"), enterdate=NA_character_,
					staffid=NA_character_, parentobsid=NA_character_, medcodeid=rsample(medcodes_even, n/length(n_files)),
					value=NA_integer_, numunitid=NA_character_, obstypeid=NA_character_,
					numrangelow=NA_integer_, numrangehigh=NA_integer_, probobsid=NA_character_
				) |> 
					write_parquet(paste0("dummy_data/define/", "Define_Inc1_Observation", i, ".parquet"))
			}
	
	#Make dummy DrugIssue define
			for (i in n_files) {
				tibble(
					patid=rsample(patids, n/length(n_files)),
					issueid=NA_integer_, 
					pracid=NA_integer_, 
					probobsid=NA_integer_,
					drugrecid=NA_integer_,
					issuedate=format(rsample(daterange_head, n/length(n_files)), "%d/%m/%Y"),
					enterdate=NA_character_, 
					staffid=NA_character_,
					prodcodeid=rsample(prodcodes_even, n/length(n_files)), 
					dosageid=NA_character_, 
					quantity=NA_integer_, 
					quantunitid=NA_integer_, 
					duration=NA_integer_, 
					estnhscost=NA_integer_
				) |> 
					write_parquet(paste0("dummy_data/define/", "Define_Inc1_DrugIssue", i, ".parquet"))
			}
	
	# Make dummy denominator
	tibble(
		patid=patids,
		pracid=pracids_sample,
		gender=rsample(factor(c("M", "F")), n_patids),
		yob=rsample(1900:2000, n_patids),
		mob=rsample(1:12, n_patids),
		patienttypeid="Regular",
		acceptable=1,
		uts=rsample(daterange, length(patids)),
		region=rsample(factor(c("London", "Not London")), length(patids)),
		lcd=rsample(daterange, length(patids)),
		regstartdate=rsample(daterange, n_patids),
		emis_ddate=rsample(daterange_w_nas, n_patids),
		regenddate=rsample(daterange_w_nas, n_patids),
		cprd_ddate=NA
	) |> write_parquet(paste0("dummy_data/denominator/", "AcceptablePats", ".parquet"))
	
# Make dummy linkage eligibility files
tibble(
	patid=patids,
	pracid=NA,
	linkdate=as.Date("1990-01-01"),
	hes_apc_e=1, ons_death_e=1, lsoa_e=1, sgss_e=1, chess_e=1, hes_op_e=1, hes_ae_e=1, hes_did_e=1, cr_e=1, sact_e=1, rtds_e=1, mhds_e=1, icnarc_e=1
) |> write_dta(paste0("dummy_data/linkage/", "Aurum_enhanced_eligibility_January_2022.dta"))

tibble(
	patid=patids,
	pracid=NA,
	linkdate=as.Date("1990-01-01"),
	hes_apc_e=1, ons_death_e=1, lsoa_e=1, sgss_e=1, chess_e=1, hes_op_e=1, hes_ae_e=1, hes_did_e=1, cr_e=1, sact_e=1, rtds_e=1, mhds_e=1, icnarc_e=1
) |> write_tsv(paste0("dummy_data/linked_data/", "23_002665_linkage_eligibility_aurum.txt"))

# Make dummy ONS death
tibble(
	patid=patids,
	dod=format(rsample(daterange_w_nas, n), "%d/%m/%Y")
) |> write_tsv(paste0("dummy_data/linked_data/", "death_patient_23_002665_DM.txt"))

tibble(
	patid=patids,
	pracid=NA,
	e2019_imd_5=rsample(1:5, n_patids)
) |> write_tsv(paste0("dummy_data/linked_data/", "patient_2019_imd_23_002665.txt"))

tibble(
	pracid=patids,
	country="England",
	e2019_imd_5=rsample(1:5, n_patids),
	ni2017_imd_5=rsample(1:5, n_patids),
	s2020_imd_5=rsample(1:5, n_patids),
	w2019_imd_5=rsample(1:5, n_patids)
) |> write_tsv(paste0("dummy_data/linked_data/", "practice_imd_23_002665.txt"))
	
icdcodes <- read_tsv_arrow("sensitive_input/ICD10_Edition5_CodesAndTitlesAndMetadata_GB_20160401.txt") |> 
	pull(CODE)

	# Make dummy HES data
	tibble(
		patid=rsample(patids, n),
		spno=1:n,     
		epikey=1:n, 
		epistart=format(rsample(daterange_head, n), "%d/%m/%Y"),
		epiend=format(rsample(daterange_head, n), "%d/%m/%Y"),  
		ICD=rsample(icdcodes, n),  
		ICDx=""
	) |> 
		write_parquet(paste0("dummy_data/linked_data/", "hes_diagnosis_epi_23_002665_DM.parquet"))

TRUE
}


make_dummy_extract_aurum <- function(codelists, cohort, outcome, n=200000) {
	library(tidyverse)
	
	# Specifications ----------------------------------------------------------
	
	#Set size
	#Number of patients and rows per file set as function argument
	n_files <- 1:4 #Number of files per filetype
	
	#Set patids
	patids <- unique(cohort$patid)
	n_patids <- length(patids)
	
	#Set daterange
	daterange <- as.Date(as.Date("1998-01-02"):as.Date("2020-01-31"), origin = "1970-01-01")
	daterange_head <- head(as.Date(as.Date("1998-01-02"):as.Date("2020-01-31"), origin = "1970-01-01"), length(daterange)/2)
	daterange_tail <- tail(as.Date(as.Date("1998-01-02"):as.Date("2020-01-31"), origin = "1970-01-01"), length(daterange)/2)
	daterange_w_nas <- c(daterange, rep(NA, 10*length(daterange)))
	
	#Set outcomes (where eventdates should occur later than other variables)
	outcomes <- outcome
	
	
	
	# Process MedCodes -----------------------------------------------------------
	
	#Make lists of all medcodes and prodcodes
	medcodes <- codelists$codes[which(codelists$codevar=="MedCodeId")]

	#If there is only one code then duplicate it to avoid downstream issues with sampling (since sample with an element of length 1 is different than with an element length >1)
	medcodes[which(map(medcodes, length)==1)] <- map(medcodes[which(map(medcodes, length)==1)], ~c(.x, .x))

	#Make vectors of all outcome codes
	outcomes_med <- unlist(codelists$codes[which(codelists$name %in% outcomes & codelists$codevar=="MedCodeId")], use.names = FALSE)

	#Make medcodes that sample evenly (make a sample of each code that is the length of the longest codelist)
	longest_codelist_length <- max(map_int(c(medcodes), length))
	medcodes_even <- unlist(map(medcodes, sample, size=longest_codelist_length, replace=TRUE), use.names = FALSE)
	
	
	# Make dummy data ---------------------------------------------------------
	
	#Make sample function where replace=TRUE
	rsample <- function(x, size) {
		sample(x, size, replace = TRUE)
	}
	
	#Make dummy medcode data
	for (red in c("", "_reduced")) {
		for (type in c("Observation")) {
			for (i in n_files) {
				tibble(
					patid=rsample(patids, n),
					obsdate=rsample(daterange_head, n),
					medcodeid=rsample(medcodes_even, n)
				) |> mutate(obsdate=if_else(medcodeid %in% outcomes_med, rsample(daterange_tail, n), obsdate)) |> 
					write_parquet(paste0("dummy_data/extract/", type, i, red, ".parquet"))
			}
		}
	}
	
	# Make dummy ONS death data
	tibble(
		patid=sample(patids, n/100),
		dod=format(rsample(daterange, n/100), "%d/%m/%Y"),
	) |> 
		write_tsv(paste0("dummy_data/linked_data/", "death_patient_23_002665_DM.txt"))
	
	TRUE
}