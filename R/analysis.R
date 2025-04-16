analysis_results <- function(hes_eventdata, hes_outcomes, cohort_wide, cohort_labels, exposure, model, n_outcomes) {

	outcome <- hes_outcomes

	#Get events
	events <- hes_eventdata[c("patid", "epistart")] |> 
		funique() |> #Only keep one row if there are multiple events for the same patient on the same day
		roworder(epistart)
	
	#Join events to the cohort
	temp <- cohort_wide |> 
		join(events, on="patid", how = "left", verbose=0, multiple = FALSE) |> 
		fsubset(is.na(epistart) | epistart > indexdate)
	
	
	#Set enddate and remove patients without follow-up time
	temp <- temp |>
		ftransform(enddate=pmin(enddate, epistart, na.rm = TRUE)) |> 
		ftransform(event=if_else(epistart==enddate, 1, 0, missing=0)) |> 
		fsubset(enddate>indexdate)
	
	#Remove matched sets that have become incomplete (i.e. no longer containing at least 1 exposed and 1 unexposed person)
	complete_sets <- temp |> 
		fgroup_by(setid) |> 
		get_vars("exposed") |> 
		fmean() |> 
		fsubset(exposed!=0 & exposed !=1) |> 
		pull(setid)
	
	cohort_post_exclusion <- temp |> fsubset(setid %in% complete_sets)
	
	#make_survival_object
	survival_object <- 	survival::Surv(
		time = as.numeric(cohort_post_exclusion$enddate), 
		origin = as.numeric(cohort_post_exclusion$indexdate),
		event = cohort_post_exclusion$event
	)
	
	#Analysis rates
	results_rates <- pyears(survival_object ~ exposed, data = cohort_post_exclusion, scale = 365.25) |>
		tidy() |> 
		mutate(rate=(event/pyears)*1000) |> 
		cbind(tibble(term="exposed",
								 group=paste0("exposed", levels(as.factor(cohort_post_exclusion$exposed))), 
								 cohort=cohort_labels,
								 outcome=outcome))
	
	#Get unexposed person years in the same row
	results_rates <- results_rates |> 
		group_by(cohort, outcome) |>
		mutate(pyears_unexposed=pyears[1],
					 n_unexposed=n[1],
					 event_unexposed=event[1],
					 rate_unexposed=rate[1],
					 ratio=1/(pyears[2]/pyears[1]),
					 ratio_text=paste0("1:", round(ratio))) |>
		ungroup() |> 
		slice(2)
	
	#analysis_regression
	results_regression <- map2_df(model, names(model), \(model, model_name)
																survival::coxph(formula(paste("survival_object ~", exposure, model, "+ strata(setid)")), 
																								data=cohort_post_exclusion, id=patid) |>  #TO VERIFY: Does id=patid need to be included?
																	broom::tidy(exponentiate=TRUE, conf.int=TRUE, conf.level=0.99) |> 
																	dplyr::mutate(cohort=cohort_labels,
																								exposure=exposure,
																								outcome=outcome,
																								model=model_name,
																								bonf=ifelse(p.value<(0.05/n_outcomes), "*", ""), #Report if p value is smaller than bonferroni-corrected alpha
																								# estimate=1,conf.low=0.9, conf.high=1.1, bonf="*") #DUMMY RESULTS
																	))
	
	results_regression |> 
		left_join(results_rates, by=c("cohort", "outcome")) |> 
		rename(term=term.x) |> 
		select(-term.y)
}


analysis_time_updated_results <- function(outcome, eventdata, cohort_wide, cohort_labels, exclusion, exposure, model, n_outcomes, time_updated_vars, time_updated_vars_spec, hes_eligible_patids, hes_dates_all) {
	#Get events
	events <- eventdata[c("patid", "obsdate")] |> 
		collapse::funique.data.frame() |> #Only keep one row if there are multiple events for the same patient on the same day
		roworder(obsdate)
	
	#Join events to the cohort
	temp <- cohort_wide |> 
		fsubset(patid %in% hes_eligible_patids) |> 
		join(events, on="patid", how = "left", verbose=0, multiple = FALSE) #collapse::join() with the default argument multiple=FALSE only joins the first matching row of the events dataset to each row of the cohort, which is intended behaviour here
	
	#Exclude individuals with an event before indexdate
	if (exclusion=="outcome before indexdate") {
		temp <- temp |> fsubset(eval(sym(outcome))==FALSE)
	} else {temp <- temp |> ftransform(obsdate=if_else(obsdate<indexdate, NA_Date_, obsdate))}
	
	#Set enddate and remove patients without follow-up time
	temp <- temp |>
		ftransform(enddate=pmin(enddate, obsdate, na.rm = TRUE)) |> 
		ftransform(event=if_else(obsdate==enddate, 1, 0, missing=0)) |> 
		fsubset(enddate>indexdate)
	
	#Remove matched sets that have become incomplete (i.e. no longer containing at least 1 exposed and 1 unexposed person)
	complete_sets <- temp |> 
		fgroup_by(setid) |> 
		get_vars("exposed") |> 
		fmean() |> 
		fsubset(exposed!=0 & exposed !=1) |> 
		pull(setid)
	
	cohort_post_exclusion <- temp |> fsubset(setid %in% complete_sets)
	
	hes_dates_all <- cohort_post_exclusion[c("patid", "exposed", "indexdate")] |> 
		join(hes_dates_all, on="patid", verbose=0, multiple = TRUE) |> 
		roworder(epistart) |> 
		fsubset(!is.na(indexdate)) |> 
		fsubset(epistart >= indexdate) |> # Only events indicating moderate or severe that occur on or after indexdate
		fgroup_by(patid, exposed) |> 
		ffirst() |> 
		fselect(patid, epistart)
	
	# tmerge eczema severity
	exposed <- cohort_post_exclusion |> fsubset(exposed==1)
	mod_sev <- 	time_updated_vars[[1]] |>
		join(exposed[c("patid", "indexdate")], on="patid", verbose=0, multiple = FALSE) |> 
		roworder(eventdate) |> 
		fsubset(!is.na(indexdate)) |> 
		fsubset(eventdate >= indexdate) |> # Only events indicating moderate or severe that occur on or after indexdate
		fgroup_by(patid, eczema_severity) |> 
		ffirst() |> 
		slice(if(any(eczema_severity==3)) 1:which(eczema_severity==3) else 1, .by=patid) #Get rid of moderate if severe has already occured
	
	exposed <- tmerge(exposed, exposed, id=patid, tstart = indexdate, tstop = enddate)
	exposed <- tmerge(exposed, exposed, id=patid, time_dep_event=event(tstop, event))
	exposed <- tmerge(exposed, mod_sev, id=patid, eczema_severity=tdc(eventdate, eczema_severity, 1))
	exposed <- tmerge(exposed, hes_dates_all, id=patid, hospital=tdc(epistart))
	
	
	unexposed <- cohort_post_exclusion |> 
		fsubset(exposed==0) |> 
		ftransform(eczema_severity=0)
	
	unexposed <- tmerge(unexposed, unexposed, id=patid, tstart = indexdate, tstop = enddate)
	unexposed <- tmerge(unexposed, unexposed, id=patid, time_dep_event=event(tstop, event))
	unexposed <- tmerge(unexposed, hes_dates_all, id=patid, hospital=tdc(epistart))
	
	
	cohort_post_exclusion_long <- bind_rows(exposed, unexposed) |> 
		ftransform(eczema_severity=factor(eczema_severity, levels=c(0,1,2,3), 
																			labels=c("no eczema", "mild", "moderate", "severe"))) |> 
		ftransform(time_in_study=as.numeric(tstop-indexdate)) |> 
		ftransform(hospital=factor(hospital))
	
	rm(cohort_wide, eventdata, events, temp, exposed, unexposed, cohort_post_exclusion)
	gc()
	
	# Analysis
	
	#Make survival object for long cohort
	survival_object <- survival::Surv(
		time = as.numeric(cohort_post_exclusion_long$tstart),
		time2 = as.numeric(cohort_post_exclusion_long$tstop),
		event = cohort_post_exclusion_long$time_dep_event
	)
	
	#Analysis rates
	results_rates <- pyears(survival_object ~ eczema_severity, data = cohort_post_exclusion_long, scale = 365.25) |>
		tidy() |> 
		mutate(rate=(event/pyears)*1000) |> 
		cbind(tibble(term=paste0("eczema_severity", levels(as.factor(cohort_post_exclusion_long$eczema_severity))), 
								 cohort=cohort_labels,
								 outcome=outcome,
								 exclusion=exclusion))
	
	#Get unexposed person years in the same row
	results_rates <- results_rates |> 
		group_by(cohort, outcome, exclusion) |>
		mutate(pyears_unexposed=pyears[1],
					 n_unexposed=n[1],
					 event_unexposed=event[1],
					 rate_unexposed=rate[1],
					 ratio=1/(pyears[2]/pyears[1]),
					 ratio_text=paste0("1:", round(ratio))) |>
		ungroup()
	
	
	#analysis_regression
	results_regression <- map2_df(model, names(model), \(model, model_name)
																survival::coxph(formula(paste("survival_object ~", "eczema_severity", model, "+ strata(setid) + cluster(patid)")), 
																								data=cohort_post_exclusion_long, id=patid) |>  #TO VERIFY: Does id=patid need to be included?
																	broom::tidy(exponentiate=TRUE, conf.int=TRUE, conf.level=0.99) |> 
																	dplyr::mutate(cohort=cohort_labels,
																								exposure=exposure,
																								outcome=outcome,
																								model=model_name,
																								exclusion=exclusion,
																								bonf=ifelse(p.value<(0.05/n_outcomes), "*", ""), #Report if p value is smaller than bonferroni-corrected alpha
																								# estimate=1,conf.low=0.9, conf.high=1.1, bonf="*") #DUMMY RESULTS
																	))
	
	results_regression |> 
		left_join(results_rates, by=c("cohort", "outcome", "exclusion", "term"))
	
	
}



#' Calculate participant counts at various stages
#' @param files 
#' @param extract 
#' @param extract_ocs 
#' @return
analysis_cohort_flow <- function(denominator, cohort_defined, cohort_labels, cohort_matched, cohort_eligible) {
	db_pop <- denominator |> 
		nrow() |> 
		as_tibble() |> 
		mutate(step="Database population") |> 
		rename(n=value)
	
	defined_pop <- cohort_defined |> 
		filter(!is.na(exposed_date)) |> 
		count() |> 
		mutate(step="who ever have eczema")
	
	cohort_matched_pop <- cohort_matched |> 
		count() |> 
		mutate(step="with Eczema matched to people without Eczema")
	
	cohort_eligible_pop <- cohort_eligible |> 
		count() |> 
		mutate(step="eligible for matching")
	
	bind_rows(db_pop, defined_pop, cohort_eligible_pop, cohort_matched_pop) |> 
		mutate(cohort=cohort_labels)
	
}


create_cohort_long <- function(outcome, eventdata, cohort_wide, cohort_labels, exclusion, n_outcomes, time_updated_vars, time_updated_vars_spec, hes_eligible_patids, hes_dates_all) {
	#Get events
	events <- eventdata[c("patid", "obsdate")] |> 
		collapse::funique.data.frame() |> #Only keep one row if there are multiple events for the same patient on the same day
		roworder(obsdate)
	
	#Join events to the cohort
	temp <- cohort_wide |> 
		fsubset(patid %in% hes_eligible_patids) |> 
		join(events, on="patid", how = "left", verbose=0, multiple = FALSE) #collapse::join() with the default argument multiple=FALSE only joins the first matching row of the events dataset to each row of the cohort, which is intended behaviour here
	
	#Exclude individuals with an event before indexdate
	if (exclusion=="outcome before indexdate") {
		temp <- temp |> fsubset(eval(sym(outcome))==FALSE)
	} else {temp <- temp |> ftransform(obsdate=if_else(obsdate<indexdate, NA_Date_, obsdate))}
	
	#Set enddate and remove patients without follow-up time
	temp <- temp |>
		ftransform(enddate=pmin(enddate, obsdate, na.rm = TRUE)) |> 
		ftransform(event=if_else(obsdate==enddate, 1, 0, missing=0)) |> 
		fsubset(enddate>indexdate)
	
	#Remove matched sets that have become incomplete (i.e. no longer containing at least 1 exposed and 1 unexposed person)
	complete_sets <- temp |> 
		fgroup_by(setid) |> 
		get_vars("exposed") |> 
		fmean() |> 
		fsubset(exposed!=0 & exposed !=1) |> 
		pull(setid)
	
	cohort_post_exclusion <- temp |> fsubset(setid %in% complete_sets)
	
	hes_dates_all <- cohort_post_exclusion[c("patid", "exposed", "indexdate")] |> 
		join(hes_dates_all, on="patid", verbose=0, multiple = TRUE) |> 
		roworder(epistart) |> 
		fsubset(!is.na(indexdate)) |> 
		fsubset(epistart >= indexdate) |> # Only events indicating moderate or severe that occur on or after indexdate
		fgroup_by(patid, exposed) |> 
		ffirst() |> 
		fselect(patid, epistart)
	
	# tmerge eczema severity
	exposed <- cohort_post_exclusion |> fsubset(exposed==1)
	mod_sev <- 	time_updated_vars[[1]] |>
		join(exposed[c("patid", "indexdate")], on="patid", verbose=0, multiple = FALSE) |> 
		roworder(eventdate) |> 
		fsubset(!is.na(indexdate)) |> 
		fsubset(eventdate >= indexdate) |> # Only events indicating moderate or severe that occur on or after indexdate
		fgroup_by(patid, eczema_severity) |> 
		ffirst() |> 
		slice(if(any(eczema_severity==3)) 1:which(eczema_severity==3) else 1, .by=patid) #Get rid of moderate if severe has already occured
	
	exposed <- tmerge(exposed, exposed, id=patid, tstart = indexdate, tstop = enddate)
	exposed <- tmerge(exposed, exposed, id=patid, time_dep_event=event(tstop, event))
	exposed <- tmerge(exposed, mod_sev, id=patid, eczema_severity=tdc(eventdate, eczema_severity, 1))
	exposed <- tmerge(exposed, hes_dates_all, id=patid, hospital=tdc(epistart))
	
	
	unexposed <- cohort_post_exclusion |> 
		fsubset(exposed==0) |> 
		ftransform(eczema_severity=0)
	
	unexposed <- tmerge(unexposed, unexposed, id=patid, tstart = indexdate, tstop = enddate)
	unexposed <- tmerge(unexposed, unexposed, id=patid, time_dep_event=event(tstop, event))
	unexposed <- tmerge(unexposed, hes_dates_all, id=patid, hospital=tdc(epistart))
	
	
	bind_rows(exposed, unexposed) |> 
		ftransform(eczema_severity=factor(eczema_severity, levels=c(0,1,2,3), 
																			labels=c("no eczema", "mild", "moderate", "severe"))) |> 
		ftransform(time_in_study=as.numeric(tstop-indexdate)) |> 
		ftransform(hospital=factor(hospital))
	
}


analysis_long <- function(outcome, cohort_long, cohort_labels, exclusion, exposure, model, n_outcomes) {
	#Make survival object for long cohort
	survival_object <- survival::Surv(
		time = as.numeric(cohort_long$tstart),
		time2 = as.numeric(cohort_long$tstop),
		event = cohort_long$time_dep_event
	)
	
	#Analysis rates
	results_rates <- pyears(survival_object ~ eczema_severity, data = cohort_long, scale = 365.25) |>
		tidy() |> 
		mutate(rate=(event/pyears)*1000) |> 
		cbind(tibble(term=paste0("eczema_severity", levels(as.factor(cohort_long$eczema_severity))), 
								 cohort=cohort_labels,
								 outcome=outcome,
								 exclusion=exclusion))
	
	#Get unexposed person years in the same row
	results_rates <- results_rates |> 
		group_by(cohort, outcome, exclusion) |>
		mutate(pyears_unexposed=pyears[1],
					 n_unexposed=n[1],
					 event_unexposed=event[1],
					 rate_unexposed=rate[1],
					 ratio=1/(pyears[2]/pyears[1]),
					 ratio_text=paste0("1:", round(ratio))) |>
		ungroup()
	
	
	#analysis_regression
	results_regression <- survival::coxph(formula(paste("survival_object ~", "eczema_severity", model, "+ strata(setid) + cluster(patid)")), 
																								data=cohort_long, id=patid) |>  #TO VERIFY: Does id=patid need to be included?
																	broom::tidy(exponentiate=TRUE, conf.int=TRUE, conf.level=0.99) |> 
																	dplyr::mutate(cohort=cohort_labels,
																								exposure=exposure,
																								outcome=outcome,
																								model=names(model),
																								exclusion=exclusion,
																								bonf=ifelse(p.value<(0.05/n_outcomes), "*", ""), #Report if p value is smaller than bonferroni-corrected alpha
																								# estimate=1,conf.low=0.9, conf.high=1.1, bonf="*") #DUMMY RESULTS
																	)
	
	results_regression |> 
		left_join(results_rates, by=c("cohort", "outcome", "exclusion", "term"))
	
	
}
