"

Figure 1: Perception of Partisan Composition

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
library(car)
library(ggplot2)
library(grid)

# Load data
# fig_1_data.dta is generated on line 155 of pcomp_replication_main.do
dem <- read_dta("fig_1_data.dta")
dem <- as.data.frame(dem)
names(dem) <- tolower(names(dem))

#Group labels
dem$groupl <- recode(dem$group, "'LGB' = 'Gay, Lesbian, Or Bisexual'; '$250K+ Income' = 'Earn Over $250k'; 'Age 65+' = 'Over 65'; 'Union' = 'Union Members'; 'Atheist/Agnostic' = 'Atheist Or Agnostic'")

# Get data in long form	
deml	  <- reshape(dem, varying = list(c("true_mean", "perc_mean"), c("true_se", "perc_se")), direction = "long")
deml$time <- c(rep("True %", 8), rep("Mean Perceived %", 8))
	
# Plot 
# ~~~~~~~~~
ggplot(data = deml, aes(y = group, x = true_mean, xmin = true_mean + 1.96*true_se, xmax = true_mean - 1.96*true_se, colour = time)) + 
geom_point(size = .75) + 
geom_errorbarh(height = 0, size = .25) +
scale_colour_manual("", values = c("#A84E1C", "#42C4C7")) +
labs(x = "", y = "") + 
theme_minimal() +
theme(panel.grid.major.y = element_line(colour = "#e3e3e3", linetype = "dotted"),
	  panel.grid.minor.x = element_blank(),
	  panel.grid.major.x = element_line(colour = "#f1f1f1", linetype = "solid"),
	  panel.border       = element_blank(),
	  legend.position  = "bottom",
	  legend.key       = element_blank(),
	  legend.key.width = unit(1,"cm"),
 	  title        = element_text(size = 8),
	  axis.title   = element_text(size = 8),
	  axis.text    = element_text(size = 8),
	  axis.ticks.y = element_blank(),
	  axis.ticks.x = element_line(colour = '#f1f1f1'),
	  strip.text.x =  element_text(size = 9),
	  legend.text=element_text(size = 8))  +  
facet_wrap(~ party, ncol = 1, drop = TRUE, scales = "free_y") +
theme(strip.background = element_rect(fill = 'white', color='white'))
