## Data extraction tool



# Environment set up ----



# Packages
{if (!require("pacman")) install.packages("pacman")
pkgs = c("bib2df", "openxlsx", "flextable", "janitor", "gridExtra", "rio", "here", "tidyverse")
pacman::p_load(pkgs, character.only = T)}


# Current date for updating saved xlsx
current_date <- as.character(Sys.Date())


# Pathogen data
incRefs <- bib2df(here("Citations", "included_refs.bib"))


# Reservoir data
reservoirRefs <- bib2df(here("Citations", "reservoir_data.bib"))


# Vector data
vectorRefs <- bib2df(here("Citations", "vector_data.bib"))



# Formatting citation data ----
cols <- c("AUTHOR", "YEAR", "TITLE", "JOURNAL", "ABSTRACT", 
          "DOI", "VOLUME", "KEYWORDS", "BIBTEXKEY", "LANGUAGE")


formatFnc <- function(data) {
  data %>% 
    select(all_of(cols)) %>% 
    clean_names()}


{incRefs <- incRefs %>% formatFnc() %>% mutate(data_type = "pathogen", data_type_other = "")
reservoirRefs <- reservoirRefs %>% formatFnc() %>% mutate(data_type = "reservoir", data_type_other = "")
vectorRefs <- vectorRefs %>% formatFnc() %>% mutate(data_type = "vector", data_type_other = "")}



# Sheet 1: Create data dictionary ----


# Create and define variables
study_data_vars <- as.data.frame(
  cbind(
    c("study_data"),
    c("study_id", "include", "exclude_reason", "data_type", "data_type_other", "first_author", "year", "title", "abstract", "journal",
      "volume", "study_year_start", "study_year_end", "study_design", "country", "doi", "full_text_link"),
    c("Unique study id", "Included for data extraction", "Reason for exclusion", "Type of data being extracted", "Other data type", 
      "First author of paper", "Year of publication", "Title of publication", "Abstract of publication", 
      "Journal published in", "Journal volumen", "Year study started", "Year study ended", "Study design", 
      "Country of study", "Publication DOI", "Link to full text")
  )
)


pathogen_data_vars <- as.data.frame(
  cbind(
    c("pathogen_data"),
    c("study_id", "pathogen_id", "associated_reservoir_id", "associated_vector_id", "year", "month", "pathogen_tested", "population_tested", "sample_size", 
      "num_pos_pcr", "num_pos_ser", "num_pos_other", "test_other", "country", "location_name", "topography", "ecoregion", "gps_longitude", "gps_latitude", "comments"),
    c("Unique study id", "Unique id for pathogen record", "Id from any associated reservoir data", "Id from any associated vector data", "Year data collected", "Month Data Collected",
      "Name of pathogen tested for", "What population was tested", "Number of individuals tested", "Number of individuals positive by PCR", "Number of individuals positive by serology", 
      "Number of individuals positive by other method", "Name of other method", "Country where study took place", "Location where study took place", 
      "Topography where study took place","Ecoregion of study", "GPS latidudinal coordinates", "GPS longitudinal coordinates", "Any comment important to note")
  )
)


reservoir_data_vars <- as.data.frame(
  cbind(
    c("reservoir_data"),
    c("study_id", "reservoir_id", "associated_pathogen_id", "associated_vector_id", "year", "month", "genus", "species", "common_name",  "sample_size",
      "sampling_method", "country", "location_name", "topography", "ecoregion", "gps_longitude", "gps_latitude", "comments"),
    c("Unique study id", "Unique id for reservoir record", "Id from any associated pathogen data", "Id from any associated vector data", "Year data collected", "Month Data Collected",
      "Genus of reservoir", "Species of reservoir", "Non-taxonomic animal name", "Number of reservoirs sampled", "Method used to trap or document reservoir", 
      "Country where study took place", "Location where study took place", "Topography where study took place", "Ecoregion of study", "GPS latidudinal coordinates", "GPS longitudinal coordinates", "Any comment important to note")
  )
)


vector_data_vars <- as.data.frame(
  cbind(
    c("vector_data"),
    c("study_id", "vector_id", "associated_pathogen_id", "associated_reservoir_id", "year", "month", "genus", "species", "sample_size",
      "sampling_method", "country", "location_name", "topography", "ecoregion", "gps_longitude", "gps_latitude", "comments"),
    c("Unique study id", "Unique id for vector record", "Id from any associated pathogen data", "Id from any associated reservoir data", "Year data collected", "Month Data Collected",
      "Genus of vector", "Species of vector", "Number of vectors sampled","Method used to trap or document vector", "Country where study took place", "Location where study took place",
      "Topography where study took place", "Ecoregion of study", "GPS latidudinal coordinates", "GPS longitudinal coordinates", "Any comment important to note")
  )
)


{study_data_vars <- study_data_vars %>% rename(`Type of data` = V1, `Variable name` = V2, `Variable description` = V3)
pathogen_data_vars <- pathogen_data_vars %>% rename(`Type of data` = V1, `Variable name` = V2, `Variable description` = V3)
reservoir_data_vars <- reservoir_data_vars %>% rename(`Type of data` = V1, `Variable name` = V2, `Variable description` = V3)
vector_data_vars <- vector_data_vars %>% rename(`Type of data` = V1, `Variable name` = V2, `Variable description` = V3)}


sheet1 <- bind_rows(study_data_vars, pathogen_data_vars, reservoir_data_vars, vector_data_vars)



# Sheet 2-4: Pathogen, reservoir and vector refs ----


# Custom function
refsFnc <- function(data) {
  data %>% 
    mutate(#study_id = row_number(),
           include  = "",
           exclude_reason = "",
           study_year_start = "",
           study_year_end = "",
           study_design = "",
           country = "",
           full_text_link = "") %>%
    select(include, exclude_reason, data_type, data_type_other, author, year, title, abstract, journal, volume,
           study_year_start, study_year_end, study_design, country, doi, full_text_link) %>%
    mutate(first_author = map_chr(author, 1)) %>% 
    select(-c(author)) %>% 
    relocate(first_author, .after = exclude_reason)
}


{incRefsSheet2 <- incRefs %>% refsFnc()
reservoirRefsSheet3 <- reservoirRefs %>% refsFnc()
vectorRefsSheet4 <- vectorRefs %>% refsFnc()}


{sheet2 <- incRefsSheet2 %>% 
  mutate(study_id = row_number()) %>% 
  relocate(study_id, .before = include)

  
sheet3 <- reservoirRefsSheet3 %>% 
  mutate(study_id = row_number()) %>% 
  relocate(study_id, .before = include)


sheet4 <- vectorRefsSheet4 %>% 
  mutate(study_id = row_number()) %>% 
  relocate(study_id, .before = include)}



# Sheet 5: Pathogens ----


# Choose columns and create sheet
pathogenCols <- c("study_id", "pathogen_id", "associated_reservoir_id", "associated_vector_id", "year", "month", "pathogen_tested", "population_tested", "sample_size", "num_pos_pcr", "num_pos_ser", 
                  "num_pos_other", "test_other", "country", "location_name", "topography", "ecoregion", "gps_latitude", "gps_longitude", "comment")

sheet5 <- tibble(!!!setNames(rep(list(""), length(pathogenCols)), pathogenCols))



# Sheet 6: Reservoirs ----


# Choose columns and create sheet
reservoirCols <- c("study_id", "reservoir_id", "associated_pathogen_id", "associated_vector_id", "year", "month", "genus", "species", "common_name", "sample_size",
                    "sampling_method", "country", "location_name",  "topography", "ecoregion", "gps_latitude", "gps_longitude", "comment")

sheet6 <- tibble(!!!setNames(rep(list(""), length(reservoirCols)), reservoirCols))



# Sheet 7: Vectors ----


# Choose columns and create sheet
vectorCols <- c("study_id", "vector_id", "associated_pathogen_id", "associated_reservoir_id", "year", "month", "genus", "species", "sample_size", "sampling_method", 
                "location_name", "country", "topography", "ecoregion", "gps_latitude", "gps_longitude", "comment")

sheet7 <- tibble(!!!setNames(rep(list(""), length(vectorCols)), vectorCols))




# Create data extraction spreadsheet ----


# Merge all sheets together into .xlsx
sheet_list <- list("data dictionary" = sheet1,
                   "pathogen_refs" = sheet2,
                   "reservoir_refs" = sheet3, 
                   "vector_refs" = sheet4, 
                   "pathogen_data" = sheet5,
                   "reservoir_data" = sheet6,
                   "vector_data" = sheet7)


export(sheet_list, here("Extracted Data", paste0("data_extraction_table_", current_date, ".xlsx")))



# End ----