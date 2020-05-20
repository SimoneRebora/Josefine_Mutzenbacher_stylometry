# Josefine_Mutzenbacher_stylometry
Scripts and corpora for authorship attribution of the novel *Josefine Mutzenbacher* (1906)

## Scripts

The **"Josefine_Mutzenbacher_stylo.R"** file performs the stylometric analyses with the seven candidate authors.\
The **"Josefine_Mutzenbacher_imposters.R"** file verifies attributions with the "impostors method".

Features for the analyses are listed in the **"Analysis_configuration.txt"** file. Please open and modify it if you want to perform different analyses. These are the available features:

*randomize*, which performs randomization of the words in the analyzed texts\
*mfw_min*, which is the lowest number of most frequent words (MFW) for the analysis\
*mfw_max*, which is the highest number of most frequent words (MFW) for the analysis\
*mfw_incr*, which is the MFW increment for analyses on frequency strata\
*culling_percentage*, which performs culling on the list of MFW (i.e. it keeps only words that are shared by a given percentage of texts)\
*distances_selection*, which combines different stylometric distances (written in a single line, separated by spaces)

## Corpora

Due to copyright limitations with the work of Ernst Klein, only document-term matrixes are shared in the **"corpus"** folder.\
Information on texts and authors are in the **"Metadata_candidates.csv"** and **"Metadata_imposters.csv"** files. 
