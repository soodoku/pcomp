"

Figure 5: Effect of Misperceptions on Social Distance

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
library(plyr)
library(ggplot2)
# Install goji for nolead0s
#library(devtools)
#install_github("soodoku/goji")
library(goji)

"
Figure 5a
"

# Load data
dat <- read_dta("affect_exp_data.dta")
dat <- subset(dat, exp_cond != "")

res <- ddply(dat[dat$pid_3 != "I",], ~ exp_cond, summarize, 
	                   mean = mean(sd_index, na.rm = T), 
	                   se = sd(sd_index, na.rm = T)/sqrt(length(exp_cond)), 
	                   n = length(exp_cond))

# Order Groups
res$exp_cond <- factor(res$exp_cond, levels = c("Control", "Ask", "Tell"))

# Plot
par(mar=c(0, 0, 0, 0))
ggplot(res, aes(y = exp_cond, x = mean, xmin = mean + 1.96*se, xmax = mean - 1.96*se)) +
geom_point(color = "#42C4C7") + 
geom_errorbarh(height = 0, size = .25, color = "#42C4C7") +
scale_x_continuous(name = "Partisan Social Distance", 
	               limits = c(.62, .69), 
	               breaks = seq(.6, .7, .02), 
	               labels = nolead0s(seq(.6, .7, .02))) + 
ylab("Experimental Condition") + 
theme_minimal() +
theme(panel.grid.major.y = element_line(colour = "#f3f3f3", linetype = "solid"),
	  panel.grid.minor.x = element_blank(),
	  panel.grid.major.x = element_line(colour = "#f3f3f3", linetype = "solid"),
	  panel.border       = element_blank(),
	  legend.position    = "none",
	  title        = element_text(size = 12),
	  axis.title   = element_text(size = 16),
	  axis.text    = element_text(size = 12),
	  axis.ticks.y = element_blank(),
	  panel.background = element_rect(colour = "#d7d7d7"),
	  axis.ticks.x = element_line(colour = "#d7d7d7"),
	  axis.title.x = element_text(vjust = -1),
	  axis.title.y = element_text(vjust = 1),
	  plot.margin = unit(c(0,.5,.5,.5), "cm"))  +  
annotate("text", x = res$mean[2], y = .85, label = paste0(nolead0s(round(res$mean[2],2)),  " (", nolead0s(round(res$se[1],2)), ")", "\n", "n= ", res$n[2]), size=4) + 
annotate("text", x = res$mean[1], y = 1.85, label = paste0(nolead0s(round(res$mean[1],2)),  " (", nolead0s(round(res$se[2],2)), ")", "\n", "n= ", res$n[1]), size=4) + 
annotate("text", x = res$mean[3], y = 2.85,  label = paste0(nolead0s(round(res$mean[3],2)),   " (", nolead0s(round(res$se[3],2)), ")", "\n", "n= ", res$n[3]), size=4) 

		
"
Figure 5b
"

# Load and subset data
data <- read_dta("affect_exp_data.dta")
partisans <- subset(data,(pid_3 == "D" | pid_3 == "R"))

# Plot
xlabel <- paste("Average Perceptual Bias (Magnitude of Correction)", sep = "")

par(mar=c(0, 0, 0, 0))
ggplot(partisans, aes(x = avg_error,y = sd_index, colour = factor(exp_cond))) + 
stat_smooth(method = lm, size = 1.5, fullrange = TRUE, alpha = 0.1, level = 0.95) + 
scale_colour_manual("", values = c("#cc5858" , "#49a68a", "#2e2f63")) +
scale_x_continuous(name = xlabel, 
				   limits = c(0, 1), 
				   breaks = seq(0, 1, .2), 
				   labels = nolead0s(seq(0, 1, .2))) + 
scale_y_continuous(name = "Partisan Social Distance", 
	               breaks = seq(.54, .85, .02), 
	               labels = nolead0s(seq(.55, .85, .02))) + 
theme_minimal() +
theme(panel.grid.major.y = element_line(colour = "#f3f3f3", linetype = "solid"),
	  panel.grid.minor.x = element_blank(),
	  panel.grid.major.x = element_line(colour = "#f3f3f3", linetype = "solid"),
	  panel.border       = element_blank(),
	  title        = element_text(size = 12),
	  axis.title   = element_text(size = 14),
	  axis.text    = element_text(size = 12),
	  panel.background   = element_rect(colour = "#d7d7d7"),
	  axis.ticks         = element_line(colour = '#d7d7d7'),
	  legend.position="none",
	  axis.title.x = element_text(vjust = -1),
	  axis.title.y = element_text(vjust = 1),
	  plot.margin = unit(c(0, .5, .5, .5), "cm")) +  
annotate("text", x = 0.90, y = 0.631, label = "Tell", size=5) + 
annotate("text", x = 0.90, y = 0.625, label = "(b = -.00)", size=4) + 
annotate("text", x = 0.82, y = 0.704, label = "Ask", size=5) + 
annotate("text", x = 0.82, y = 0.697, label = "(b = .10)", size=4) + 
annotate("text", x = 0.92, y = 0.682, label = "Control", size=5) + 
annotate("text", x = 0.92, y = 0.675, label = "(b = .05)", size=4) +
		geom_vline(xintercept = .197, colour = "#999999", linetype = "longdash")
