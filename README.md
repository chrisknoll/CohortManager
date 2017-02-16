# CohortManager
========
An R tool that lets you manage cohort definitions sourced from CSVs

 
Getting Started
===============

1. in R, use the following commands to install CohortManager (if you have prior package installations of aony of these packages, you may need to first unistall them using the command remove.packages()).
  ````r
    install.packages("devtools")
    library(devtools)
    install_github("chrisknoll/CohortManager")
  ````
  
2. If you do not have existing CSV files, create an empty directory to store these, and execute the following to create empty files:
  ````r
    directoryPath <- "{path to empty directory}";
    CohortManager::initCsv(directoryPath)
  ````
3. Edit the CSV files. The CSV files needed for cohort manager are:
 * ConcpetSets.csv: Contains the names and IDs of the concept set expressions used across all cohort definitions. One row per concept set.
 * ConceptSetItems.csv: Contains the items, defining the concept_id and if you should include/exclude, use descendants or use mapped. Many rows per concept set.
 * CohortDefinitions.csv: Contains the cohort definitions, specifying the event limits, prior/post observation requirements, and end date strategy.  One row per cohort definition.
 * CohortCriteria.csv: Contains the universe of cohort criteria referenced in the cohort definitions.
 * CohortPrimaryEvents.csv: Specifies which CohortCriteria should be associated to a cohort as the 'primary events' used to define the cohort start date.
 * Cohort CorrelatedCriteria: Specifies one or more criteria that validates the Primary Events. These criteria are AND'd together when validating the Primary Event.
 * CohortDemographicCriteria.csv: Specifies one or more demographic criteria (used to specify age, gender or time window of the primary event.
 * DateOffsetStrategy.csv: Specifies the data used when the cohort defintion exit strategy is set to 'DateOffset'. One row per cohort definition.
 * CustomEraStrategy.csv: Specifies the data used when the cohort defintion exit strategy is set to 'CustomEra'. One row per cohort definition.
 * CohortCensoringEvents.csv: Specifies one or more cohort criteria to use as censoring events.
4. Set the CohortManager options for the WebAPI path and sourceKey:
  ````
    directoryPath <- {path to empty directory};
    CohortManager::initCsv(directoryPath)
  ````

5. Call CohortManager::setDataDirectory(path).  This will verify all tequired files and columns are found.
6. Call CohortManager::sync() to create/update cohort definitions hosted in the specified WebAPI.

Troubleshooting
===============
Ensure that all files are saved before calling sync().
Do hot have the CohortDefinitions.csv file open in excell when calling sync() because the file is modified with he server-side cohort definition IDs, and if the file is open, it will report 'access deined'.

License
=======
CohortManager is licensed under Apache License 2.0

Development
===========
CohortManager is being developed in R Studio.

