# *************************** Libraries*****************************************************************************

library(tidyverse)
library(olsrr)
library(car)







df_newYorkRivers <- read.table("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/newYorkRivers.txt", header = TRUE, sep = '\t')

View(df_newYorkRivers) 



lm_newYorkRivers <- lm(Nitrogen ~., df_newYorkRivers[, 2:6] )  

summary(lm_newYorkRivers)


residualPlots(lm_newYorkRivers)
              

ols_plot_resid_lev(lm_newYorkRivers) 



ols_plot_resid_pot(lm_newYorkRivers) 




ols_plot_cooksd_bar(lm_newYorkRivers)



ols_plot_dfbetas(lm_newYorkRivers) # not Included in the Article



ols_plot_dffits(lm_newYorkRivers)




ols_plot_hadi(lm_newYorkRivers)





lm_riversNot45 <- update(lm_newYorkRivers, subset = -c(4, 5, 7,19))
summary(lm_riversNot45)



residualPlots(lm_riversNot45)
























































































