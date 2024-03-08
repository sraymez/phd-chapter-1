reservoir_data_summaries <- read.csv("R/Working Data/reservoir_data_complete_coords.csv", stringsAsFactors = F)
reservoir_refs <- read.csv("R/Working Data/reservoir_refs.csv", stringsAsFactors = F)

rodents <- reservoir_data_summaries %>% 
  filter(genus %in% c("Arvicanthis", "Cricetomys", "Crocidura", "Mastomys",
                      "Mus", "Mus_nannomys", "Rattus", "Praomys", "Steatomys",
                      "Taterillus", "Thryonomys", "Uranomys")) %>% 
  group_by(study_id) %>% 
  count()



rodent_refs <- reservoir_refs %>% 
  filter(study_id %in% rodents$study_id) %>% 
  group_by(first_author) %>% 
  count()


rvf <- pathogen_data_summaries %>% 
  filter(pathogen_tested == "Rift_valley_fever") %>% 
  select(study_id, year, sample_size, num_pos_pcr, num_pos_ser) %>% 
  count(study_id, year)
