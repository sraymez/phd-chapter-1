## Querying references from lit review

# Data
reservoir_data_summaries <- import(here("Working Data", "reservoir_data_complete_coords.csv"))
reservoir_refs <- import(here("Working Data", "reservoir_refs.csv"))

# Find studies where lots of data was extracted
rodents <- reservoir_data_summaries %>% 
  filter(genus %in% c("Arvicanthis", "Cricetomys", "Crocidura", "Mastomys",
                      "Mus", "Mus_nannomys", "Rattus", "Praomys", "Steatomys",
                      "Taterillus", "Thryonomys", "Uranomys")) %>% 
  group_by(study_id) %>% 
  count()

# Finding common first authors
rodent_refs <- reservoir_refs %>% 
  filter(study_id %in% rodents$study_id) %>% 
  group_by(first_author) %>% 
  count()

# Finding references on rift valley
rvf <- pathogen_data_summaries %>% 
  filter(pathogen_tested == "Rift_valley_fever") %>% 
  select(study_id, year, sample_size, num_pos_pcr, num_pos_ser) %>% 
  count(study_id, year)

# End 