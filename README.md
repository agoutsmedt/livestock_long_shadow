# A scientometric retrospective of the Livestock Long Shadow Report - R Scripts

Programming files to produce the quantitative analyses and figures for the article [A scientometric retrospective of the Livestock Long Shadow Report](https://www.frontiersin.org/journals/sustainable-food-systems/articles/10.3389/fsufs.2025.1656562) by Océane Duluins, Aurélien Goutsmedt, Noé Vandevoorde and Philippe Baret.

# scripts — brief overview

This section briefly describes the purpose of each R script in the `scripts/` folder. 

Usage notes
- Source `paths_and_packages.R` first to set file paths and load required packages.
- Scripts are generally written to be run interactively in order, or called from a driver script such as `running_networks_and_report.R`.

Files

- `paths_and_packages.R`
  - Sets project file paths, options and loads required CRAN/ Bioconductor packages used across the scripts.

- `helper_functions.R`
  - Collection of utility functions reused across scripts (IO helpers, plotting wrappers, data helpers, small wrappers to keep main scripts concise).

- `creating_usable_data.R`
  - Reads scropus data and performs cleaning and transformations to produce the core analysis-ready datasets used by downstream scripts.

- `running_networks_and_report.R`
  - Orchestrates running network analyses (possibly in batches) in `creating_networks.R` and compiles outputs into reports (HTML/Quarto). Acts as a driver for choosing final parameters.

- `creating_networks.R`
  - Builds network objects from the cleaned data. Contains the pipeline for constructing adjacency/edge lists and basic network attributes. Topic model is ran thanks to `background_topic_model.R`.

- `background_topic_model.R`
  - Supporting code for the topic model (background setup, helper objects or exploratory checks used prior to fitting models).

- `creating_networks_final_parameters.R`
  - Variant of `creating_networks.R` that runs network creation with final/tuned parameters used for main analyses and figures.

- `implementing_topic_model.R`
  - Runs the topic modeling pipeline on prepared text data: model fitting, saving model objects and primary outputs.

- `analysing_topic_model.R`
  - Analyses and visualises topic model outputs (topic prevalence, topic-term distributions, coherence diagnostics and plots).

- `exploring_thematic_groups_attribute.R`
  - Exploratory analysis focused on thematic groups / attributes: summary tables, group-level plots and checks used in the exploratory stage.

- `preparing_data_publication.R`
  - Formats and tidies final datasets and tables intended for reporting or publication (e.g. column selection, labels, export to CSV/RDS).


Notes and best practices
- Run scripts in the order above for a reproducible workflow.
- Prefer sourcing individual scripts during interactive work so you can inspect intermediate objects. 

Contact
- For questions about these scripts, don't hesitate to reach me or raise an issue.

