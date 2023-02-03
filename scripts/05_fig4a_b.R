"

Figure 4: Effect of Misperceptions on Perceived Extremity

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
# Install goji for nolead0s
#library(devtools)
#install_github("soodoku/goji")
library(goji)

"
Figure 4a
"

# ft_maineffects_plot_data.csv is generated on line 705 of pcomp_replication_main.do
res <- read_dta("ft_maineffects_plot_data.dta")

# Cleaner conditions
res$condition <- factor(res$condition, levels = c("control", "ask", "tell"), labels = c("Control", "Ask", "Tell"))

par(mar=c(0,0,0,0))
ggplot(res, aes(y = as.factor(condition), x = mean, xmin = mean + 1.96*se, xmax = mean - 1.96*se)) +
geom_point(color = "#42C4C7") + 
geom_errorbarh(height = 0, size = .25, color = "#42C4C7") +
scale_x_continuous(name = "Out-Party Dislike (Reverse-Coded FT)", limits = c(.64, .76), breaks = seq(.65, .75, .05), labels = nolead0s(seq(.65, .75, .05))) + 
ylab("Experimental Condition") + 
theme_minimal() +
theme(panel.grid.major.y = element_line(colour = "#f3f3f3", linetype = "solid"),
	  panel.grid.minor.x = element_blank(),
	  panel.grid.major.x = element_line(colour = "#f3f3f3", linetype = "solid"),
	  panel.border       = element_blank(),
	  legend.position  = "bottom",
	  panel.background = element_rect(colour = "#d7d7d7"),
	  legend.key       = element_blank(),
	  legend.key.width = unit(1,"cm"),
	  title        = element_text(size = 12),
	  axis.title   = element_text(size = 15),
	  axis.text    = element_text(size = 12),
	  axis.ticks.y = element_blank(),
	  axis.ticks.x = element_line(colour = "#d7d7d7"),
	  axis.title.x = element_text(vjust = -1),
	  axis.title.y = element_text(vjust =  1),
	  plot.margin = unit(c(0, .5, .5, .5), "cm"))  +  
annotate("text", x = res$mean[1], y = 2.75, label = paste0(nolead0s(round(res$mean[1],2)),  " (", nolead0s(round(res$se[1],2)), ")", "\n", "n = ", res$n[1]), size=4) + 
annotate("text", x = res$mean[2], y = 1.75, label = paste0(nolead0s(round(res$mean[2],2)),  " (", nolead0s(round(res$se[2],2)), ")", "\n", "n = ", res$n[2]), size=4) +
annotate("text", x = res$mean[3], y = .75,  label = paste0(nolead0s(round(res$mean[3],2)),  " (", nolead0s(round(res$se[3],2)), ")", "\n", "n = ", res$n[3]), size=4)


"
Figure 4b
"

## Feeling thermometer plot B
data <- read_dta("extremity_exp_data.dta")

		partisans <- subset(data,(pid_3 == 1 | pid_3 == 3))
xlabel <- paste("Average Perceptual Bias (Magnitude of Correction)")

#Reverse-coding the FT
partisans$ft_rev <- (100 - partisans$ft_op) / 100

par(mar=c(0,0,0,0))
ggplot(partisans, aes(x = avg_error, y = ft_rev, colour = factor(condition))) + 
stat_smooth(method = lm, size = 1.5, span = 1.5, fullrange=TRUE, alpha = 0.1, level=0.95) + 
scale_colour_manual("", values = c("#cc5858" , "#49a68a", "#2e2f63")) +
scale_x_continuous(name = xlabel, 
				   limits = c(0, 1), 
				   breaks = seq(0, 1, .2), 
				   labels = nolead0s(seq(0, 1, .2))) + 
scale_y_continuous(name = "Out-Party Dislike (Reverse-Coded FT)") + 
theme_minimal() +
theme(panel.grid.major.y = element_line(colour = "#f3f3f3", linetype = "solid"),
		  panel.grid.minor.x = element_blank(),
		  panel.grid.major.x = element_line(colour = "#f3f3f3", linetype = "solid"),
		  panel.border       = element_blank(),
		  title        = element_text(size=12),
		  axis.title   = element_text(size=16),
		  axis.text    = element_text(size=12),
		  panel.background   = element_rect(colour = "#d7d7d7"),
		  axis.ticks         = element_line(colour = '#d7d7d7'),
		  legend.position="none",
		  axis.title.x = element_text(size = 14, vjust=-1),
		  axis.title.y = element_text(size = 14, vjust= 1),
		  plot.margin = unit(c(0,.5,.5,.5), "cm"))  +  
annotate("text", x = 0.87, y = .66, label = "Tell", size=5) 	+ 
annotate("text", x = 0.87, y = .65, label = "(b = 0.01)", size=4) +
annotate("text", x = 0.9, y = 0.72, label = "Ask", size=5) +
annotate("text", x = 0.9, y = 0.71, label = "(b = 0.11)", size=4) + 
annotate("text", x = 0.87, y=.78, label = "Control", size=5) + 
annotate("text", x = 0.87, y = .77, label = "(b = 0.18)", size=4)	+
geom_vline(xintercept = .316, colour = "#999999", linetype = "longdash")
