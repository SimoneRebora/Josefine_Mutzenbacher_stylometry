library(stylo)

chosen_distance <- "wurzburg"
chosen_mfw <- 2000

### read document-term matrixes
all_imposters_files <- list.files("corpus/Imposters", pattern = ".csv", full.names = T)
all_candidate_names <- gsub(pattern = "corpus/Imposters/Imposters_|.csv", replacement = "", x = all_imposters_files)

### separate imposter of interest
for(i in 1:length(all_candidate_names)){
  
  my_candidate <- all_candidate_names[i]
  
  data <- read.csv(all_imposters_files[i], stringsAsFactors = F, row.names = 1)
  
  all_texts <- rownames(data)
  all_authors <- strsplit(all_texts, "_")
  all_authors <- unlist(lapply(all_authors, function(x) x[1]))
  all_authors <- unique(all_authors)
  
  my_imposters <- all_authors[-c(1,2)]
  
  filename <- paste("Imposters_full_attribution_", my_candidate, ".txt", sep = "")
  
  test_id <- 1
  candidate_id <- which(grepl(pattern = my_candidate, x = all_texts))
  
  # use imposters on selected candidate
  data <- as.matrix(data)
  imposters_results <- imposters(reference.set = data[-c(test_id, candidate_id),], test = data[test_id,], candidate.set = data[candidate_id,], distance = chosen_distance)
    
  cat(my_candidate, "probability:", imposters_results, "\nDistance:", chosen_distance, " MFW:", dim(data)[2], "\nImposters:", my_imposters, file = filename, sep = " ")

}
