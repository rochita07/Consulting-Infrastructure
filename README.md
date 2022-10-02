# ConsultingInfrastructure

This study is the CRISP study (Critical Resilient Interdependent Infrastructure Systems and Processes ) that focuses on infrastructure in Texas.  There is a special focus on how infrstructure responds in a national disaster like Hurricane Harvey.

This Github was created by Brittany Alexander for Stat 684 Consulting. I have served as a student research assistant for the Institute for Science Technology and Public Policy who is the consulting client for the past two years. I have not spent much time with this data set, but I have done similar projects with them in the past.  If you are unfamiliar with working with public opinion survey data and have questions let me know.  

This data was collected by Ipsos, a private survey firm.  I have worked with their data files in the past which are stata files.  They can be read with the haven R packages.
We are waiting on Ipsos to give us a file with Census track for each respondent. That is the only missing component from the data. 

The data is weighted.  Ipsos tries to get a representative sample but response rates vary by demographic characteristics.  The goal of this analysis is to infer about the population from this non-representative sample.  Ipsos uses raking which is a method that weights the data so that the sample's characteristics match the population. The weights must be used in all inferences about the population and to make the graphics.  For analysis of census tracks, the weights aren't necessary.

There are basically two samples.  The first sample is of all of Texas, and the second sample is of Harris county. 

There is item level non-response.  We could possibly impute this data, or we could focus on complete cases. 

In this github I have uploaded the (tentative) data, the questionaire and the project report. The project report includes documentation on what the colnames of the data are, and does include details about response rates and the weighting method. 

The goal of this project is to summarize this survey using regressions, visualizations, and some spatial statistics. We might want to split into team regression and team spatial statistics. The clients love visualizations especially the look of ggplots. EDA is a important role for both teams. Differences in demographics or political party or political ideology are usually of interest. The regression team will also need to perform variable selection.  The survey questions are believed to be related. This work will get published in social science journals. 






