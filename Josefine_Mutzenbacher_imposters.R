library(stylo)

# choose distance for analysis
### read configuration file
config <- read.csv("Analysis_configuration.csv", stringsAsFactors = F)

### define variables for the analysis
chosen_distance <- config$value[which(config$feature == "imposters_distance")]

# find document-term matrixes
all_imposters_files <- list.files("corpus/Imposters", pattern = ".csv", full.names = T)
# extract names of candidates
all_candidate_names <- gsub(pattern = "corpus/Imposters/Imposters_|.csv", replacement = "", x = all_imposters_files)

# run analysis for each candidate
for(i in 1:length(all_candidate_names)){
  
  my_candidate <- all_candidate_names[i]
  
  # read data from d-t matrix
  data <- read.csv(all_imposters_files[i], stringsAsFactors = F, row.names = 1)
  
  # get titles of all texts
  all_texts <- rownames(data)

  # get names of all authors
  all_authors <- strsplit(all_texts, "_")
  all_authors <- unlist(lapply(all_authors, function(x) x[1]))
  all_authors <- unique(all_authors)
  
  # isolate imposters
  my_imposters <- all_authors[-c(1,2)]
  
  # identify test text (always the first here) and texts of candidate author
  test_id <- 1
  candidate_id <- which(grepl(pattern = my_candidate, x = all_texts))
  
  # use imposters on selected candidate
  data <- as.matrix(data) # dataframe has to be trasformed into matrix to be read by stylo
  imposters_results <- imposters(reference.set = data[-c(test_id, candidate_id),], test = data[test_id,], candidate.set = data[candidate_id,], distance = chosen_distance)

  # save results
  filename <- paste("Imposters_full_attribution_", my_candidate, ".txt", sep = "")  
  cat(my_candidate, "probability:", imposters_results, "\nDistance:", chosen_distance, " MFW:", dim(data)[2], "\nImposters:", my_imposters, file = filename, sep = " ")

}
