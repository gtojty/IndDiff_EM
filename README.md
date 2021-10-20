## How Skill Differences Affect Online Reading Behavior via Interactions with  Lexical Properties: An Eye-Movement Experiment

***

This is the set of data and processing codes for the above academic paperfor studying interactions
between lexical properties and skill differences in eye movement patterns during online sentence
reading. 

The raw and pre-processed data are stored in the subfolder ./data/res_F. The processing R codes include: 

* resF_preparedata.r: This code reads the raw data and does the preprocessing, inclduing transformations of lexical properties and skill measures, drawing correlation figures, and summarizes eye-movement measures, skill measures, and lexical properties.

* resF_analysis.r: This code calculates the five mixed-effects regression models and stores the results

* IndInfluence.r: This code calculates the "leave-one-subject-out" percentage

The other codes, including R-squared.r, drawMainEffect_Int.r, and mer.r are called by resF_preparedata.r and resF_analysis.r.

Please cite the paper when using the data and codes shared here.
Tao Gong & Lan Shuai. Segmented relations between online reading behaviors, text properties, and reader-text interactions: An eye-movement experiment.
