# Errors in aerial survey count data: identifying pitfalls and solutions

### [Kayla L. Davis](https://davisk93.github.io/), Emily D. Silverman, [Allison L. Sussman](https://github.com/asussman52), R. Randy Wilson,  & [Elise F. Zipkin](https://ezipkin.github.io/)

### Code/Data DOI: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4685098.svg)](https://doi.org/10.5281/zenodo.4685098)

### Please contact the first author for questions about the code or data: Kayla L. Davis (davisk93@msu.edu)
__________________________________________________________________________________________________________________________________________

## Abstract:  
1.	Accurate estimates of animal abundance are essential for guiding effective management strategies. Poor survey data can lead to biased estimates or misleading inferences on population abundance and the effects of environmental variables. 
2.	Aerial surveys are an efficient survey platform, capable of collecting wildlife data across large spatial extents in relatively short timeframes. However, aerial surveys can also yield unreliable data if their design and/or implementation are not carefully executed. 
3.	Through an extensive review of the aerial survey literature over the last 50 years, we examined common challenges in the data including non-detection, counting error, and species misidentification issues, as well as how each issue can manifest and how researchers have addressed these issues within current and historical literature. Additionally, we used a case study focused on marine bird data collected via aerial surveys in the Gulf of Mexico to explore the potential extent of each challenge and possible resolutions.
4.	We found that nearly three quarters of the aerial survey methodological literature focused on accounting for non-detection errors (74%), while the issues of counting error and misidentification were less commonly addressed (occurring in 28% and 11% of the reviewed papers, respectively). Our case study demonstrates how each of these challenges may prove problematic for inference by detailing the magnitude of potential error each challenge may bring to count data. Our results illustrate how each challenge can act independently to bias inferences; thus, highlighting the need to consider methods for mitigating each issue separately during survey design and/or analysis. 
5.	We synthesize the information gained during our literature review and analysis of our case study data to evaluate strategies for overcoming the challenges of using aerial survey data to estimate wildlife abundance. Strategies examined include use of digital data collection methods, pooling species records by family, and using an ordinal modeling approach to model binned counts. Our goal is to provide clarity on the possible errors that can be introduced in aerial survey data by documenting these errors, guiding researchers and monitoring practitioners to reasonable approaches to ameliorate ongoing count data issues, and identifying areas for future research. 


## Data

[gommapps_aerialSurvey_Feb2018_birds.csv](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/gommapps_aerialSurvey_Feb2018_birds.csv): File containing count data from winter 2018 marine bird surveys. 

[gommapps_aerialSurvey_July2018.csv](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/gommapps_aerialSurvey_July2018.csv): File containing count data from summer 2018 marine bird surveys.

[gommapps_aerialSurvey_Feb2019.csv](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/gommapps_aerialSurvey_Feb2019.csv): File containing count data from winter 2019 marine bird surveys.

[specieslists.csv](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/specieslists.csv): List containing all species observed during GoMMAPPS aerial surveys.


[Figures](https://github.com/davisk93/Davis-et-al_Aerial-Survey/tree/main/Figures): Folder containing data needed to recreate figures 2 and 3 

[Appendix 2](https://github.com/davisk93/Davis-et-al_Aerial-Survey/tree/main/Appendix%202): Folder containing data and code needed to recreate appendix 2

## Code

[AmbiguousGrpsFunction.R](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/AmbiguousGrpsFunction.R): This script contains a function to identify ambiguous species identifications. Run this before running any of the "Unreconciled-DO" scripts.

[Matching-Function.R](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/Matching-Function.R): This script contains a function to automate matching of double observer records. Run this before running any of the "Unreconciled-DO" scripts.

[Unreconciled-DO_F18_comments.R](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/Unreconciled-DO_F18_comments.R): This script performs matching of double observer records for winter 2018 survey and creates summary statistics for crew member detection and counting.

[Unreconciled-DO_J18_comments.R](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/Unreconciled-DO_J18_comments.R): This script performs matching of double observer records for summer 2018 survey and creates summary statistics for crew member detection and counting.

[Unreconciled-DO_F19_comments.R](https://github.com/davisk93/Davis-et-al_Aerial-Survey/blob/main/Unreconciled-DO_F19_comments.R): This script performs matching of double observer records for summer 2019 survey and creates summary statistics for crew member detection and counting.


[Figures](https://github.com/davisk93/Davis-et-al_Aerial-Survey/tree/main/Figures): Folder containing scripts needed to recreate figures 2 and 3 

[Appendix 2](https://github.com/davisk93/Davis-et-al_Aerial-Survey/tree/main/Appendix%202): Folder containing data and code needed to recreate appendix 2

