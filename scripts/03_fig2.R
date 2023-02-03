"

Figure 2: Relationship between perceptions and political interest

@authors:
Douglas J. Ahler
doug.ahler@gmail.com

Gaurav Sood
gsood07@gmail.com

"

# Set the working directory to directory containing the data  
# setwd("data/")
	
# Load libs
library(haven)
library(ggplot2)
library(grid)
library(gridExtra)

# Load data
data <- read_dta("pcomp_yougov_data.dta")

# Individual grid elements
dblack <- ggplot(data, aes(x = newsint_r, y = dem_black, colour = "#42C4C7")) + 
          stat_smooth(method = loess, size = 1.5, fullrange = TRUE, alpha = 0.1, level = 0.95) + 
          theme(panel.background = element_rect(fill = "white"), legend.position = "none") + 
          xlab("Interest in Political News") + 
          ylab("Average Perception") + 
          ggtitle("Dems. - Black")
          
daa    <- ggplot(data,aes(x = newsint_r, y = dem_aa, colour = "#42C4C7")) + 
          stat_smooth(method = loess, size = 1.5, fullrange = TRUE, alpha = 0.1, level = 0.95) + 
          theme(panel.background = element_rect(fill = "white"), legend.position = "none") + 
          xlab("Interest in Political News") + 
          ylab("Average Perception") + 
          ggtitle("Dems. - Ath./Ag.")
        	
dlgb   <- ggplot(data,aes(x = newsint_r, y = dem_lgb, colour = "#42C4C7")) + 
          stat_smooth(method = loess, size = 1.5, fullrange = TRUE, alpha = 0.1, level = 0.95) + 
          theme(panel.background = element_rect(fill = "white"), legend.position = "none") + 
	 	  xlab("Interest in Political News") + 
          ylab("Average Perception") + 
          ggtitle("Dems. - LGB")

dunion <- ggplot(data,aes(x = newsint_r, y = dem_union, colour = "#42C4C7")) + 
          stat_smooth(method = loess, size = 1.5, fullrange = TRUE, alpha = 0.1, level = 0.95) + 
          theme(panel.background = element_rect(fill = "white"), legend.position = "none") + 
          xlab("Interest in Political News") + 
          ylab("Average Perception") + 
          ggtitle("Dems. - Union")

revang <- ggplot(data,aes(x = newsint_r, y = rep_evang, colour = "#42C4C7")) + 
          stat_smooth(method = loess, size = 1.5, fullrange = TRUE, alpha = 0.1, level = 0.95) + 
          theme(panel.background = element_rect(fill = "white"),legend.position = "none") + 
          xlab("Interest in Political News") + 
          ylab("Average Perception") + 
          ggtitle("Reps. - Evang.")

rsouth <- ggplot(data,aes(x = newsint_r, y = rep_south, colour = "#42C4C7")) + 
          stat_smooth(method = loess, size = 1.5, fullrange = TRUE, alpha = 0.1, level = 0.95) + 
          theme(panel.background = element_rect(fill = "white"),legend.position = "none") + 
          xlab("Interest in Political News") + 
          ylab("Average Perception") + 
          ggtitle("Reps. - Southern")

rold <- ggplot(data,aes(x = newsint_r, y = rep_old, colour  =  "#42C4C7")) + 
        stat_smooth(method = loess, size = 1.5, fullrange = TRUE, alpha = 0.1, level = 0.95) + 
        theme(panel.background = element_rect(fill = "white"), legend.position = "none") + 
        xlab("Interest in Political News") + 
        ylab("Average Perception") + 
        ggtitle("Reps. - 65+")

rrich <- ggplot(data,aes(x = newsint_r, y = rep_rich, colour = "#42C4C7")) + 
         stat_smooth(method = loess, size = 1.5, fullrange = TRUE, alpha = 0.1, level = 0.95) + 
         theme(panel.background = element_rect(fill = "white"), legend.position = "none") + 
         xlab("Interest in Political News") + 
         ylab("Average Perception") + 
         ggtitle("Reps. - $250K+")

grid.arrange(dblack, daa, dlgb, dunion, revang, rsouth, rold, rrich, ncol = 4)
