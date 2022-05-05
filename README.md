# autotest
automatic inferential statistics test seleciton, execution and visualization.

by Tsz-Hong Chan

In inferential statistiscs, parametric tests are often preferred over non-parametric tests for being more powerful. 

The current version contains 5 functions that automates the selection between T-test, ANOVA, Simple Linear modelling,  Chi square test, Pearsons's correlation and their non-parametric counter parts, before returning the test results with graphical visualizaitons.

For example to test for significant difference in population central tendencies, like population mean or median, the auto.t(x,y,...) function requires the input of two numeric vectors arguments containing the continuous data of two sample groups, two Q-Q plots will be produced and a prompt requires user feedback on the normality of the data distribution, if the user replies that the data is not normal, Wilcoxon Rank Sum Test results and boxplots with sample medians will be returned. If the user replies that the data is normal, Leven's Test result is returned. If the Leven's test result has a P value greater than or equal to 0.05, Student's T-test result will be returned, otherwise Welche's T-test result will be returned.

To install the 'autotest' package use the following code:

devtools::install_github(THChan11/autotest)

use the following codes to call deatiled instructions

?auto.chisq

?auto.t

?auto.anova

?auto.lm

?auto.cor
