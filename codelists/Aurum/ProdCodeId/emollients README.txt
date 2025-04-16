# -----------------------------------------------------------------------------
Clinical codelist for emollients and moisturisers

Author: Julian Matthewman
Created on 12 July 2023
Created by searching the 2023_03 CPRD Aurum Medical code browser
Terminologies: CPRD MedCode, SNOMED-CT, Read	
Short description: Emollients and moisturisers without any other active ingredients
Reviwed by: Julian Matthewman
# -----------------------------------------------------------------------------


# Search strategy --------------------------------

Terms identified descendants of the SNOMED-CT concept "48279009 Emollient agent" and from https://bnf.nice.org.uk/drugs/emollient-creams-and-ointments-paraffin-containing/medicinal-forms/. Iteratively modified terms used to search and exclude codes, while checking for codes with the same ProductName and codes with a descendant BNF chapters.


# Includes --------------------------------------

Emollients and moisturisers


# Excludes --------------------------------------

Excluded emollients containing an active ingredient such as salicylic acid, coal tar, zinc, calamine, steroids.


# Checks ----------------------------------------

This codelist has 100 million drug issues.
Only including terms with a non-missing formulation, results in 88 million drug issues.
Only including codes with a cutaneous route of administration, results in 74 million drug issues.
Searching for "Self or descendant" of SNOMED-CT entry "48279009 Emollient agent" in shrimp browser (https://ontoserver.csiro.au/shrimp/ecl/?fhir=https://r4.ontoserver.csiro.au/fhir) and using the codes to identify codes by dmdid in Aurum resulted in codes with 59 million drug issues

