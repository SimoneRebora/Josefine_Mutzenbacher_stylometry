# Josefine_Mutzenbacher_stylometry
Scripts and corpora for authorship attribution of the novel *Josefine Mutzenbacher* (1906)

## Scripts

The **"Josefine_Mutzenbacher_candidates.R"** file performs the stylometric analyses with the seven candidate authors.\
The **"Josefine_Mutzenbacher_imposters.R"** file verifies attributions with the "impostors method".

Features for the analyses are listed in the **"Analysis_configuration.csv"** file. Please open and modify it if you want to perform different analyses.

### Available features

For the **"Josefine_Mutzenbacher_candidates.R"** file:
- *finale*, which performs analyses only on the finale of the novel (i.e. the last 4420 words)
- *randomize*, which performs randomization of words in the texts by candidates
- *distances_selection*, which combines different stylometric distances (written in a single line, separated by spaces)
- *mfw_min*, which is the lowest number of most frequent words (MFW) for the analysis
- *mfw_max*, which is the highest number of most frequent words (MFW) for the analysis
- *mfw_incr*, which is the MFW increment for analyses on frequency strata
- *culling_percentages* (written in a single line, separated by spaces), which perform different cullings on the list of MFW (i.e. keeping only words that are shared by a given percentage of texts)

For the **"Josefine_Mutzenbacher_imposters.R"** file:
- *finale*, which performs analyses only on the finale of the novel (i.e. the last 4420 words)
- *imposters_distance*, which chooses a stylometric distance to perform the "imposters" analysis

## Corpora

Due to copyright limitations with the work of Ernst Klein, only document-term matrixes are shared in the **"corpus"** folder.\
Different document-term matrixes are provided for:
- text attribution vs. method efficiency testing
- analysis of the full text vs. analysis of the finale
- analysis with or without word randomization
- rolling delta analysis

Information on texts and authors are in the **"Metadata_candidates.csv"** and **"Metadata_imposters.csv"** files. 
