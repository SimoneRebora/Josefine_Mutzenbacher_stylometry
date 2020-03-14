### Josefine_Mutzenbacher_stylo
### code for running a stylometric analysis of the novel "Josefine Mutzenbacher"

library(stylo)
library(tidyverse)
library(reshape2)

### variables for the analysis
culling_percentage <- 0
samples_length <- 5000
mfw_selection <- (1:40)*50 # which means from 50 to 2,000 with steps of 50 words
distances_selection <- c("dist.delta", "dist.eder", "dist.canberra", "dist.wurzburg")

### save all methods in a grid
methods_combination <- expand.grid(mfw_selection, distances_selection, stringsAsFactors = FALSE)

### prepare variables to save results
author_full_attr <- list()
author_full_attr_detailed <- list()
simple_score <- numeric()

### Main iteration starts
for(i in 1:dim(methods_combination)[1]){
  
  mfw_choice <- methods_combination[,1][i]
  dist_choice <- methods_combination[,2][i]
  
  ### first part: test quality of attribution method
  
  my_results <- stylo(gui = F, 
                      frequencies = "corpus/table_with_frequencies_test.txt", # texts are read from the term-document matrix
                      corpus.lang = "German",
                      encoding = "UTF-8",
                      mfw.min = mfw_choice,
                      mfw.max = mfw_choice,
                      mfw.incr = mfw_choice,
                      culling.min = culling_percentage,
                      culling.max = culling_percentage,
                      analysis.type = "CA",
                      distance.measure = dist_choice,
                      write.png.file = F
  )
  
  my_results_classes <- names(my_results$distance.table[1,])
  my_results_classes <- strsplit(my_results_classes, split = "_")
  my_results_classes <- unlist(lapply(my_results_classes, function(x) x[1]))
  
  my_results_unique_classes <- unique(my_results_classes)
  
  simple_score[i] <- 0
  
  standardized_df <- my_results$distance.table
  standardized_df <- (standardized_df-mean(standardized_df))/sd(standardized_df)
  
  for(my_class in my_results_unique_classes){
    
    cat("Checking results for class", my_class, "...\n")
    
    in_group_df_ids <- which(my_results_classes == my_class)
    in_group_df <- standardized_df[in_group_df_ids,in_group_df_ids]
    
    out_group_df <- standardized_df[-in_group_df_ids,-in_group_df_ids]
    
    simple_score_for_class = mean(out_group_df) - mean(in_group_df)
    print(simple_score_for_class)
    
    simple_score[i] <- simple_score[i]+simple_score_for_class
    
  }
  
  simple_score[i] <- simple_score[i]/length(my_results_unique_classes)
  
  ### second part: attribution
  
  stylo_results <- stylo(gui = F, 
                         frequencies = "corpus/table_with_frequencies_full.txt", # texts are read from the term-document matrix
                         culling.min = culling_percentage,
                         culling.max = culling_percentage,
                         corpus.lang = "German",
                         encoding = "UTF-8",
                         mfw.min = mfw_choice,
                         mfw.max = mfw_choice,
                         mfw.incr = mfw_choice,
                         analysis.type = "CA",
                         distance.measure = dist_choice,
                         write.png.file = F
  )
  
  ### using centroid for attribution
  all_authors_attributions <- strsplit(names(stylo_results$distance.table[1,]), "_")
  all_authors_attributions <- unlist(lapply(all_authors_attributions, function(x) x[1]))
  author_attr <- list()
  author_full_attr_tmp <- numeric()
  
  authors_selection <- unique(all_authors_attributions[-1])
  
  for(author_id in 1:length(authors_selection)){
    my_author <- authors_selection[author_id]
    author_ids <- which(all_authors_attributions == my_author)
    author_attr[[author_id]] <- sort(stylo_results$distance.table[1,][author_ids])[1:3]
    author_full_attr_tmp[author_id] <- mean(author_attr[[author_id]])
  }
  
  author_full_attr_detailed[[i]] <- author_attr
  
  names(author_full_attr_tmp) <- authors_selection
  
  author_full_attr[[i]] <- author_full_attr_tmp
  
  cat("##############\n##############\n##############\n", i, "##############\n##############\n##############\n")
  
  unlink("*_EDGES.csv")
  
}

### save results in a dataframe
josefine_results <- as.data.frame(do.call(rbind, author_full_attr))
josefine_results$MFW <- methods_combination[,1]
josefine_results$distance <- methods_combination[,2]
josefine_results$simple_score <- simple_score

josefine_results$attribution <- ""
for(i in 1:length(josefine_results$Salten)){
  josefine_results$attribution[i] <- names(which.min(josefine_results[i,1:length(authors_selection)]))
}

### define outfile title
datestamp <- gsub(pattern = "\\W", replacement = "", x = Sys.time())
out_file <- paste("Stylo_results_samples", samples_length, "_culling",culling_percentage,"_date", datestamp, sep = "")

### write csv of results
write.csv(x = josefine_results, file = paste(out_file, ".csv", sep = ""))
### save all
save.image(file = paste(out_file, ".RData", sep = ""))

### make graph for evolution of attribution with a single measure
chosen_distances <- c("dist.delta", "dist.eder", "dist.canberra", "dist.wurzburg")
for(chosen_distance in chosen_distances){
  
  josefine_results_sel <- josefine_results[which(josefine_results$distance == chosen_distance),]
  
  josefine_results_sel <- melt(josefine_results_sel, measure.vars =  authors_selection)
  
  p1 <- ggplot(josefine_results_sel, aes(x=MFW, y=value)) + 
    geom_line(aes(color = variable)) +
    geom_point(aes(color = variable, shape = variable)) +
    ggtitle(chosen_distance) +
    scale_shape_manual(values=1:length(authors_selection))
  
  ggsave(p1, filename = paste(out_file, "_", chosen_distance, ".png", sep = ""), width = 16, height = 9, dpi = 300, scale = 0.7)
  
}

print("Delta analysis complete!!!")
