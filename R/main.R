#' @export
cacheEnv <- new.env()

#' @export
buildCsvDef <- function (name, columns) {
  csvDef <- {};
  csvDef$name <- name;
  csvDef$columns <- columns;

  return(csvDef);
}

#' @export
resolveConcept <- function(conceptId) {
  jsonlite::fromJSON(paste(cacheEnv$webAPI, "vocabulary", cacheEnv$sourceKey, "concept", conceptId, sep="/"));
}

#' @export
isFileValid <- function(directoryPath, csvDef) {
  csvDF <- NULL
  try (csvDF <- read.csv(paste(directoryPath,csvDef$name, sep=""), stringsAsFactors = FALSE, nrows=1), silent=TRUE);
  if (is.null(csvDF)) {
    return(FALSE);
  }

  if (length(names(csvDef$columns)[! names(csvDef$columns) %in% names(csvDF)]) > 0)
    return(FALSE);

  return(TRUE);
}

buildDateOffsetStrategy <- function(targetCohortId) {
  dateOffsetCsv <- cacheEnv$csvData[["DateOffsetStrategy.csv"]];
  dateOffsetRow <- dateOffsetCsv[dateOffsetCsv$cohortId == targetCohortId,];

  if (seq_len(nrow(dateOffsetRow)) == 0) {
    Warning(paste("No date offset strategy data found for cohort:", targetCohortId));
    return(NULL);
  }

  dateOffset <- list("DateField" = dateOffsetRow$dateField, "Offset" = dateOffsetRow$offset);
  return (list("DateOffset" = dateOffset));
}

buildCustomEraStrategy <- function(targetCohortId) {
  customEraCsv <- cacheEnv$csvData[["CustomEraStrategy.csv"]];
  customEraRow <- customEraCsv[customEraCsv$cohortId == targetCohortId,];

  if (seq_len(nrow(customEraRow)) == 0) {
    Warning(paste("No date custm era strategy data found for cohort:", targetCohortId));
    return(NULL);
  }

  customEra <- list("DrugCodesetId" = customEraRow$drugCodesetId, "GapDays" = customEraRow$gapDays, "Offset" = customEraRow$offset);
  return (list("CustomEra" = customEra));
}


#' @export
refreshCsv <- function() {
  if (is.null(cacheEnv$workingDir))
    stop("Working directory is not set.  call CohortManager::setWorkingDirectory(path) before processCohort().")

  directoryPath <- cacheEnv$workingDir;

  csvData <- {};
  for (def in cacheEnv$requiredFiles) {
    # load each CSV file and store in csvData
    currentCsv <- read.csv(paste(directoryPath, def$name, sep=""),
                           header = TRUE,
                           stringsAsFactors = FALSE,
                           colClasses = def$columns);
    csvData[[def$name]] <- currentCsv;
  }

  cacheEnv$csvData <- csvData;

  message("CSV Contents loaded.")
}

#' @export
setDataDirectory <- function(directoryPath) {

  if (!dir.exists(directoryPath))
    stop(paste("Directory does not exist:", directoryPath));

  if (stringr::str_sub(directoryPath, -1, -1) != "/")
    directoryPath <- paste(directoryPath, "/", sep = "");

  # check for files to exist
  fileExists <- lapply(cacheEnv$requiredFiles, function (x) { file.exists(paste(directoryPath, x$name, sep=""))})
  missingFilenames <- lapply(cacheEnv$requiredFiles[which(fileExists == FALSE)], function(x) x$name);
  if (length(fileExists[which(fileExists == FALSE)]) > 0)
  {
    stop(paste("The following files are required:", paste(collapse=",", missingFilenames)));
  }

  # check for valid files
  validFiles <- lapply(cacheEnv$requiredFiles, function(x) { isFileValid(directoryPath, x) })
  invalidFiles <- cacheEnv$requiredFiles[which(validFiles == FALSE)];
  if (length(invalidFiles) > 0)
  {
    stop (paste("The following files are missing required columns:",
                paste(lapply(invalidFiles, function (x) { paste(x$name, "[", paste(names(x$columns), collapse=","), "]") } ),
                      collapse=",")
                )
          );
  }

  cacheEnv$workingDir = directoryPath;
  message(paste("Working directory set to: ", directoryPath));
  refreshCsv();

}

#' @export
initCsv <- function (directoryPath, overwrite = FALSE) {
  if (!dir.exists(directoryPath))
    stop(paste("Directory does not exist:", directoryPath));

  if (stringr::str_sub(directoryPath, -1, -1) != "/")
    directoryPath <- paste(directoryPath, "/", sep = "");

  for (csvDef in cacheEnv$requiredFiles) {
    if (!file.exists(paste(directoryPath, csvDef$name, sep="")) || overwrite == TRUE)
    {
      cat(paste(paste(names(csvDef$columns), collapse=","), "\n", sep=""), file=paste(directoryPath, csvDef$name, sep=""));
    }
    else
    {
      warning(paste("File already exists: ", csvDef$name, ". Skipping...", sep="" ));
    }
  }
  message(paste("The required files were created at path:", directoryPath));

}

#' @export
processCohort <- function (targetCohortId) {

  if (is.null(cacheEnv$workingDir))
    stop("Working directory is not set.  call CohortManager::setWorkingDirectory(path) before processCohort().")

  cohortDefCsv <- cacheEnv$csvData[["CohortDefinitions.csv"]];
  primaryEventsCsv <- cacheEnv$csvData[["CohortPrimaryEvents.csv"]]
  censorEventsCsv <- cacheEnv$csvData[["ChortCensoringEvents.csv"]]
  correlatedCriteriaCsv <- cacheEnv$csvData[["CohortCorrelatedCriteria.csv"]]
  demographicCriteriaCsv <- cacheEnv$csvData[["CohortDemographicCriteria.csv"]]
  criteriaCsv <- cacheEnv$csvData[["CohortCriteria.csv"]]

  conceptSetsCsv <- cacheEnv$csvData[["ConceptSets.csv"]];
  conceptSetItemsCsv <- cacheEnv$csvData[["ConceptSetItems.csv"]]

  cohortDefRow <- cohortDefCsv[cohortDefCsv$cohortId == targetCohortId,];
  conceptSetIds <- c(); # this will contain the set of concept set IDs we discover as we build each criteria.

  # Build PrimaryCriteria

  # build Primary Events
  PrimaryEvents <- list();
  primaryEventRows <- subset(merge(primaryEventsCsv, criteriaCsv, by="criteriaId"), cohortId == targetCohortId);

  by(primaryEventRows, seq_len(nrow(primaryEventRows)), function(peRow) {
    conceptSetIds <<- c(conceptSetIds, as.numeric(peRow$conceptSetId));
    criteriaParams <- {}
    if (!is.na(peRow$conceptSetId)) criteriaParams$CodesetId <- as.numeric(peRow$conceptSetId);
    if (!is.na(peRow$useFirst) && as.numeric(peRow$useFirst) > 0) criteriaParams$First <- TRUE;

    criteria <- {};
    criteria[peRow$criteriaType] <- list(criteriaParams);

    # add criteria to PrimaryEvents list
    PrimaryEvents <<- c(PrimaryEvents, list(criteria));
  });

  # Build Primary Events Observation Window
  ObservationWindow <- {};
  ObservationWindow$PriorDays <- as.numeric(cohortDefRow$priorObservation);
  ObservationWindow$PostDays <- as.numeric(cohortDefRow$postObservation);

  # add elements to Primarycriteria
  PrimaryCriteria = list ("CriteriaList" = PrimaryEvents, "ObservationWindow" = ObservationWindow, "PrimaryCriteriaLimit" = list("Type" = cohortDefRow$peLimitType));

  # build AdditionalCriteria

  # build Qualfiying Criteria events
  QualifyEvents <- list();
  qualifyEventRows <- subset(merge(correlatedCriteriaCsv, criteriaCsv, by="criteriaId"), cohortId == targetCohortId);

  by(qualifyEventRows, seq_len(nrow(qualifyEventRows)), function(qeRow) {
    CorrelatedCriteria <- {};
    conceptSetIds <<- c(conceptSetIds, as.numeric(qeRow$conceptSetId));

    # create criteria object
    criteria <- {};
    criteria[qeRow$criteriaType] <- list(list(CodesetId=as.numeric(qeRow$conceptSetId)));
    CorrelatedCriteria$Criteria <- criteria;

    # create window
    startElement <- {};
    if (is.na(qeRow$windowStart)) {
      startElement$Coeff <- as.numeric(-1);
    } else {
      startElement$Coeff <- if(qeRow$windowStart != 0) as.numeric(sign(qeRow$windowStart)) else as.numeric(-1);
      startElement$Days <- as.numeric(abs(qeRow$windowStart));
    }

    endElement <- {};
    if (is.na(qeRow$windowEnd)) {
      endElement$Coeff <- as.numeric(1);
    } else {
      endElement$Coeff <- if(qeRow$windowEnd != 0) as.numeric(sign(qeRow$windowEnd)) else as.numeric(1);
      endElement$Days <- as.numeric(abs(qeRow$windowEnd));
    }
    CorrelatedCriteria$StartWindow <- list(Start=startElement, End=endElement);

    # create occurrence
    occurrenceElement <- {};
    occurrenceElement$Type <- switch(qeRow$type, "EXACTLY"=as.numeric(0), "AT MOST"=as.numeric(1), "AT LEAST"=as.numeric(2));
    occurrenceElement$Count <- as.numeric(qeRow$occurrences)
    CorrelatedCriteria$Occurrence <- occurrenceElement;
    QualifyEvents <<- c(QualifyEvents, list(CorrelatedCriteria));
  });

  # build Demographic Criteria
  DemographicCriteria <- c();
  demographicRows <- subset(demographicCriteriaCsv, cohortId == targetCohortId);

  by(demographicRows, seq_len(nrow(demographicRows)), function(dRow) {
    criteria <- {};
    ageElement <- {};
    if (!is.na(dRow$ageMin) && !is.na(dRow$ageMax)) {
      ageElement$op <- "bt";
      ageElement$Value <- as.numeric(dRow$ageMin);
      ageElement$Extent <- as.numeric(dRow$ageMax);
    } else if (!is.na(dRow$ageMin)) {
      ageElement$op <- "gte";
      ageElement$Value <- as.numeric(dRow$ageMin);
    } else if (!is.na(dRow$ageMax)) {
      ageElement$op <- "lte";
      ageElement$Value <- as.numeric(dRow$ageMax);
    }

    genderElement <- c();
    if (!is.na(dRow$gender)) {
      genderElement <- lapply(jsonlite::fromJSON(dRow$gender), function (conceptId) resolveConcept(conceptId));
    }

    startDateElement <- {};
    if (nchar(dRow$startDate) > 0 && nchar(dRow$endDate) > 0) {
      startDateElement$op <- "bt";
      startDateElement$Value <- dRow$startDate;
      startDateElement$Extent <- dRow$endDate;
    } else if (nchar(dRow$startDate) > 0) {
      startDateElement$op <- "gte";
      startDateElement$Value <- dRow$startDate;
    } else if (nchar(dRow$endDate) > 0) {
      startDateElement$op <- "lte";
      startDateElement$Value <- dRow$endDate;
    }

    if (length(ageElement) > 0) criteria$Age <- ageElement;
    if (length(startDateElement) > 0) criteria$OccurrenceStartDate <- startDateElement;
    if (length(genderElement) > 0) criteria$Gender <- genderElement;

    DemographicCriteria <<- c(DemographicCriteria, list(criteria));
  });

  # add elements to AdditionalCriteria

  AdditionalCriteria <- list("Type" = "ALL");
  AdditionalCriteria$CriteriaList <- if (length(QualifyEvents) > 0) QualifyEvents else list();
  AdditionalCriteria$DemographicCriteriaList <- if (length(DemographicCriteria) > 0) DemographicCriteria else list();

  # build CensoringCriteria
  CensoringEvents <- list();
  censorEventRows <- subset(merge(censorEventsCsv, criteriaCsv, by="criteriaId"), cohortId == targetCohortId);

  by(censorEventRows, seq_len(nrow(censorEventRows)), function(ceRow) {
    conceptSetIds <<- c(conceptSetIds, as.numeric(ceRow$conceptSetId));
    criteriaParams <- {}
    if (!is.na(ceRow$conceptSetId)) criteriaParams$CodesetId <- as.numeric(ceRow$conceptSetId);
    if (!is.na(ceRow$useFirst) && as.numeric(ceRow$useFirst) > 0) criteriaParams$First <- TRUE;

    criteria <- {};
    criteria[ceRow$criteriaType] <- list(criteriaParams);

    # add criteria to PrimaryEvents list
    CensoringEvents <<- c(CensoringEvents, list(criteria));
  });


  EndStrategy <- NULL;
  # build EndStrategy
  if (nchar(cohortDefRow$endStrategy) > 0) {
    buildStrategy <- switch(cohortDefRow$endStrategy, "DateOffset"=buildDateOffsetStrategy, "CustomEra"=buildCustomEraStrategy);
    if (!is.null(buildStrategy)) {
      EndStrategy <- buildStrategy(cohortDefRow$cohortId);
    }
    else {
      warning(paste("Invalid EndStrategy specified for cohort:", cohortDefRow$cohortId));
    }
  }

  # build concept set list from conceptSetIDs
  conceptSetIds <- unique(conceptSetIds);
  conceptSetRows <- subset(conceptSetsCsv, conceptSetId %in% conceptSetIds);

  ConceptSets <- {};

  by(conceptSetRows, seq_len(nrow(conceptSetRows)), function(csRow) {

    conceptSet <- list("id"=as.numeric(csRow$conceptSetId), "name"=csRow$name);

    csvItems <- subset(conceptSetItemsCsv, conceptSetId == csRow$conceptSetId);
    conceptSetItems <- {};
    by(csvItems, seq_len(nrow(csvItems)), function(csiRow) {
      item <- {};
      item$concept <- resolveConcept(csiRow$conceptId);
      if (csiRow$isExcluded == 1) item$isExcluded <- TRUE;
      if (csiRow$includeDescendants == 1) item$includeDescendants <- TRUE;
      if (csiRow$includeMapped == 1) item$includeMapped <- TRUE;
      conceptSetItems <<- c(conceptSetItems, list(item));
    });
    conceptSet$expression <- list("items" = conceptSetItems);
    ConceptSets <<- c(ConceptSets, list(conceptSet));
  });

  # create cohortExpression

  cohortExpression <- list("ConceptSets"= if (length(ConceptSets) > 0) ConceptSets else list(),
                           "PrimaryCriteria"= if (length(PrimaryCriteria) > 0) PrimaryCriteria else list(),
                           "QualifiedLimit" = list("Type"=cohortDefRow$qeLimitType),
                           "ExpressionLimit" = list("Type"=cohortDefRow$cohortLimitType),
                           "InclusionRules" = list(),
                           "CensoringCriteria" = if (length(CensoringEvents) > 0) CensoringEvents else list()
  );

  if (!is.null(EndStrategy)) cohortExpression$EndStrategy <- EndStrategy;

  # do not set AdditionalCriteria if there are no qualifiy events or demographic criteria
  if (length(QualifyEvents) > 0 || length(DemographicCriteria) > 0) {
    cohrotExpression$AdditionalCriteria <- AdditionalCriteria
  }

  # Create cohort definition object

  expressionJSON <- jsonlite::toJSON(cohortExpression, auto_unbox = TRUE);
  cohortDefinition <- list("name" = cohortDefRow$cohortName,
                           "description" = cohortDefRow$cohortDescription,
                           "expressionType"="SIMPLE_EXPRESSION",
                           "expression" = expressionJSON);

  # set up a result object to return the reslut of this cohort sync
  result <- {};

  if (is.na(cohortDefRow$cohortDefinitionId)) {
    # create the cohort definition on the server and set the result as 'new'
    jsonBody <- jsonlite::toJSON(cohortDefinition, auto_unbox=TRUE);
    httpheader <- c(Accept = "application/json; charset=UTF-8", "Content-Type" = "application/json");
    url <- paste(cacheEnv$webAPI, "cohortdefinition", sep = "/");
    createResponse <- jsonlite::fromJSON(RCurl::postForm(url, .opts = list(httpheader = httpheader, postfields = jsonBody)));

    # update CohortDefinitions csvData
    cacheEnv$csvData$CohortDefinitions.csv[which(cacheEnv$csvData$CohortDefinitions.csv$cohortId == targetCohortId),"cohortDefinitionId"] <- createResponse$id;

    # assign result status as 'add'
    result$cohortId <- cohortDefRow$cohortId;
    result$cohortDefinitionId <- createResponse$id;
    result$status <- "add";

    write.csv(cacheEnv$csvData$CohortDefinitions.csv,
              file=paste(cacheEnv$workingDir, "CohortDefinitions.csv", sep=""),
              na="",
              row.names = FALSE);
  } else {
    # assign cohort definiton id to constructed cohort definition
    cohortDefinition$id <- cohortDefRow$cohortDefinitionId;

    # read existing cohort definition.
    url <- paste(cacheEnv$webAPI, "cohortdefinition", as.numeric(cohortDefRow$cohortDefinitionId), sep = "/");
    existingCohortDef <- jsonlite::fromJSON(url);

    if (existingCohortDef$expression != cohortDefinition$expression) {
      # modify if expression doesn't match.

      jsonBody <- jsonlite::toJSON(cohortDefinition, auto_unbox=TRUE);
      httpheader <- c(Accept = "application/json; charset=UTF-8", "Content-Type" = "application/json");
      updateResponse <- RCurl::httpPUT(url,jsonBody, "httpheader" = httpheader);

      # assign result status as 'modify'
      result$cohortId <- cohortDefRow$cohortId;
      result$cohortDefinitionId <- cohortDefRow$cohortDefinitionIdd;
      result$status <- "update";
    } else if (existingCohortDef$name != cohortDefinition$name) {
      # rename if definition name has changed.
      jsonBody <- jsonlite::toJSON(cohortDefinition, auto_unbox=TRUE);
      httpheader <- c(Accept = "application/json; charset=UTF-8", "Content-Type" = "application/json");
      updateResponse <- RCurl::httpPUT(url,jsonBody, "httpheader" = httpheader);

      # assign result status as 'rename'
      result$cohortId <- cohortDefRow$cohortId;
      result$cohortDefinitionId <- cohortDefRow$cohortDefinitionIdd;
      result$status <- "rename";
    } else {
      # no action taken
      result$cohortId <- cohortDefRow$cohortId;
      result$cohortDefinitionId <- cohortDefRow$cohortDefinitionId;
      result$status <- "none";
    }
  }

  return(result);
}

#' @export
sync <- function() {
  if (is.null(cacheEnv$workingDir))
    stop("Working directory is not set.  call CohortManager::setWorkingDirectory(path) before sync().")

  # get the list of cohorts to process (filter Cohort Defs DF by isManual = FALSE)
  cohortDefCsv <- cacheEnv$csvData[["CohortDefinitions.csv"]];
  cohortIds <- cohortDefCsv[cohortDefCsv$isManual == "0","cohortId"]
  results <- lapply(cohortIds, function(x) processCohort(x));

  message(paste("Sync complete.",
                length(Filter(function(item) { item$status == "add"}, results)), "added,",
                length(Filter(function(item) { item$status == "update"}, results)), "updated,",
                length(Filter(function(item) { item$status == "rename"}, results)), "renamed,",
                length(Filter(function(item) { item$status == "none"}, results)), "unchanged.", sep=" "));
}

#' @export
setOptions <- function(webAPI, sourceKey) {
  if (!missing(webAPI)) cacheEnv$webAPI <- webAPI;
  if (!missing(sourceKey)) cacheEnv$sourceKey <- sourceKey;
}
.onLoad <- function(libname, pkgname) {
  cacheEnv$requiredFiles <- list(buildCsvDef("CohortDefinitions.csv", c("cohortId"="integer", "cohortDefinitionId"="integer",	"conceptId"="integer", "cohortName"="character", "isManual"="integer", "priorObservation"="integer", "postObservation"="integer", "peLimitType"="character", "qeLimitType"="character",	"cohortLimitType"="character", "endStrategy"="character", "cohortDescription"="character")),
                     buildCsvDef("CohortCriteria.csv", c("criteriaId"="integer", "name"="character", "criteriaType"="character", "conceptSetId"="integer", "useFirst" = "integer")),
                     buildCsvDef("CohortPrimaryEvents.csv", c("cohortId"="integer", "criteriaId"="integer")),
                     buildCsvDef("CohortCorrelatedCriteria.csv", c("cohortId"="integer", "criteriaId"="integer", "type"="character", "occurrences"="integer", "windowStart"="integer", "windowEnd"="integer")),
                     buildCsvDef("CohortDemographicCriteria.csv", c("cohortId"="integer", "name"="character", "gender"="character", "ageMin"="integer", "ageMax"="integer", "startDate"="character", "endDate"="character")),
                     buildCsvDef("DateOffsetStrategy.csv", c("cohortId"="integer", "name"="character", "dateField"="character", "offset"="integer")),
                     buildCsvDef("CustomEraStrategy.csv", c("cohortId"="integer", "name"="character", "drugCodesetId"="integer", "gapDays"="integer", "offset"="integer")),
                     buildCsvDef("ChortCensoringEvents.csv", c("cohortId"="integer", "name"="character", "criteriaId"="integer")),
                     buildCsvDef("ConceptSets.csv", c("conceptSetId"="integer", "name"="character", "description"="character")),
                     buildCsvDef("ConceptSetItems.csv", c("conceptSetId"="integer", "conceptId"="integer", "isExcluded"="integer", "includeDescendants"="integer", "includeMapped"="integer"))
  );

  cacheEnv$workingDir <- NULL;
  cacheEnv$webAPI <- "http://localhost:8080/WebAPI";
  cacheEnv$sourceKey <- "VOCABULARY_20161218";

}
