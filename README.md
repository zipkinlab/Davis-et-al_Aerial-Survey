# [Errors in aerial survey count data: identifying pitfalls and solutions](https://onlinelibrary.wiley.com/doi/10.1002/ece3.8733)

### Ecology and Evolution

### [Kayla L. Davis](https://davisk93.github.io/), Emily D. Silverman, [Allison L. Sussman](https://github.com/asussman52), R. Randy Wilson,  & [Elise F. Zipkin](https://ezipkin.github.io/)

### Code/Data DOI: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6038240.svg)](https://doi.org/10.5281/zenodo.6038240)

### Please contact the first author for questions about the code or data: Kayla L. Davis (davisk93@msu.edu)
__________________________________________________________________________________________________________________________________________

## Abstract:  
Accurate estimates of animal abundance are essential for guiding effective management, and poor survey data can produce misleading inferences. Aerial surveys are an efficient survey platform, capable of collecting wildlife data across large spatial extents in short timeframes. However, these surveys can yield unreliable data if not carefully executed. Despite a long history of aerial survey use in ecological research, problems common to aerial surveys have not yet been adequately resolved. Through an extensive review of aerial survey literature over the last 50 years, we evaluated how common problems encountered in the data including non-detection, counting error, and species misidentification can manifest, the potential problems conferred, and the history of how these challenges have been addressed. Additionally, we used a double-observer case study focused on waterbird data collected via aerial surveys and an online group (flock) counting quiz to explore the potential extent of each challenge and possible resolutions. We found that nearly three quarters of the aerial survey methodology literature focused on accounting for non-detection errors, while issues of counting error and misidentification were less commonly addressed. Through our case study, we demonstrated how these challenges can prove problematic by detailing the extent and magnitude of potential errors. Using our online quiz, we showed that aerial observers typically undercount group size and that the magnitude of counting errors increases with group size. Our results illustrate how each issue can act to bias inferences, highlighting the importance of considering individual methods for mitigating potential problems separately during survey design and analysis. We synthesized the information gained from our analyses to evaluate strategies for overcoming the challenges of using aerial survey data to estimate wildlife abundance, such as digital data collection methods, pooling species records by family, and ordinal modeling using binned data. Recognizing conditions that can lead to data collection errors and having reasonable solutions for addressing errors can allow researchers to allocate resources to effectively mitigate the most significant challenges for obtaining reliable aerial survey data.


## Data

[gommapps_aerialSurvey_Feb2018_birds.csv](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/gommapps_aerialSurvey_Feb2018_birds.csv): File containing count data from winter 2018 marine bird surveys. 

[gommapps_aerialSurvey_July2018.csv](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/gommapps_aerialSurvey_July2018.csv): File containing count data from summer 2018 marine bird surveys.

[gommapps_aerialSurvey_Feb2019.csv](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/gommapps_aerialSurvey_Feb2019.csv): File containing count data from winter 2019 marine bird surveys.

[specieslists.csv](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/specieslists.csv): List containing all species observed during GoMMAPPS aerial surveys.


## Code

[AmbiguousGrpsFunction.R](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/AmbiguousGrpsFunction.R): This script contains a function to identify ambiguous species identifications. Run this before running any of the "Unreconciled-DO" scripts.

[BinMatching-Function.R](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/Matching-Function.R): This script contains a function to automate matching of double observer records. Run this before running any of the "Unreconciled-DO" scripts.

[Unreconciled-DO_F18_comments.R](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/Unreconciled-DO_F18_comments.R): This script performs matching of double observer records for winter 2018 survey and creates summary statistics for crew member detection and counting.

[Unreconciled-DO_J18_comments.R](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/Unreconciled-DO_J18_comments.R): This script performs matching of double observer records for summer 2018 survey and creates summary statistics for crew member detection and counting.

[Unreconciled-DO_F19_comments.R](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/Unreconciled-DO_F19_comments.R): This script performs matching of double observer records for summer 2019 survey and creates summary statistics for crew member detection and counting.

## Post-processing

[Figures](https://github.com/davisk93/Davis-et-al_Aerial-Survey/tree/main/Figures): Folder containing scripts needed to recreate figures 2 and 3 

[Appendix 2](https://github.com/davisk93/Davis-et-al_Aerial-Survey/tree/main/Appendix%202): Folder containing data and code needed to recreate appendix 2

