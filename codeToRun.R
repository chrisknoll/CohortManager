directoryPath <- "C:/Documents/Projects/ActiveSurveilance/data/";
CohortManager::setDataDirectory(directoryPath);
CohortManager::setOptions(webAPI = "http://rndusrdhit01.jnj.com:9090/WebAPI", sourceKey = "VOCABULARY_20161218");
CohortManager::sync()
