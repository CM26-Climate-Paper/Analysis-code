# Analysis-code
In progress code to summarize changes to the seasonal movements of Mid-Atlantic species over the next 80 years


Background reading: threshold selcection methods for presence/absence cutoffs.

1. http://www.biogeografia.org/biogeografia/images/documentos/jorge%20lobo/2007.%20Jimenez%20&%20lobo.%20Threshold.%20AO.pdf
"In this paper four
threshold criteria are compared for a wide range of sample sizes and prevalences, modeling
a virtual species in order to avoid the omnipresent error sources that the use of real species
data  implies.  In  general,  sensitivity–specificity  difference  minimizer  and  sensitivity–
specificity  sum  maximizer criteria produced  the  most accurate  predictions. The  widely-
used  0.5  fixed  threshold  and  Kappa-maximizer  criteria  are  the  worst  ones  in  almost  all
situations. Nevertheless, whatever the criteria used, the threshold value chosen and the
research goals that determined its choice must be stated."

2. http://rspatial.org/sdm/rst/5_sdm_models.html
"We can extract several things from the objects in ‘e’, but let’s restrict ourselves to the AUC values and the “maximum of the sum of the sensitivity (true positive rate) and specificity (true negative rate)” threshold “spec_sens” (this is sometimes uses as a threshold for setting cells to presence or absence)."

Example code for how to extract MAX SSS

3. https://rdrr.io/cran/dismo/man/threshold.html
Package documentation for spec-sens
