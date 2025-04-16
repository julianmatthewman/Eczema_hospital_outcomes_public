# Code and codelists for "Mapping risks of hospital-recorded health conditions in people with eczema"
Pre-print available at: https://doi.org/10.1101/2025.03.24.25324470

Dashboard available at: https://julianmatthewman.github.io/Eczema_hospital_outcomes_public/

## How to run

1.  Place the NHS ICD-10 5th Edition data file in the `sensitive_input/` folder (e.g., available via NHS TRUD service).
2.  Open the R console and call `renv::restore()` to install the required R packages.
3.  Call `library(targets)`
4.  Call `tar_make(dummy_define)` to make dummy data.
5.  Call `tar_make()` to run the pipeline.


## How to inspect targets

-   Call `tar_read(target)` to retrieve a specified target.
-   Call `tar_visnetwork(targets_only = TRUE)` to visualise the pipeline.

## Data availability

No real data is provided in this repository. Running the pipeline will automatically generate dummy data. To run with real data provide paths to the real data in the `paths.R` file in the `paths/` folder (see [README](paths/README.md)).


## Typical run time and hardware requirements

With dummy data the pipeline takes approximately 5 minutes to complete on a normal (ca 2025) desktop or laptop computer. With real CPRD Aurum and HES data, the pipeline may take more than 1 week to complete when branches are not run in parallel. Approximately 64 GB of RAM are required.

## Files

| File                                   | Purpose                                                                                                        |
|--------------------|----------------------------------------------------|
| [\_targets.R](_targets.R)              | Declares the [`targets`](https://docs.ropensci.org/targets) pipeline.                                          |
| [R/](R/)                               | Contains R scripts with functions to be used in the pipeline.                                                  |
| [codelists/](codelists/)               | Contains all codelists (and some that were not used).                                                          |
| [dummy_data/](dummy_data/)             | When the pipeline is run, dummy data will be placed here.                                                      |
| [input/](input/)                       | Contains phecode and GBD maps.                                                                                 |
| [sensitive_input/](sensitive_input/)   | The NHS ICD-10 5th Edition data file should be placed here.                                                    |
| [output/](output/)                     | When the pipeline is run, outputs will be placed here.                                                         |
| [sensitive_output/](sensitive_output/) | When the pipeline is run, sensitive outputs will be placed here (lists for CPRD Aurum define and extract).     |
| [paths/](paths/)                       | Paths to the dummy data or real data.                                                                          |
| [renv.lock](renv.lock)                 | The [`renv`](https://rstudio.github.io/renv/articles/renv.html) lock file that specifies all package versions. |
| [docs/](docs/)                         | Containes the rendered quarto dashboard.                                                                       |
