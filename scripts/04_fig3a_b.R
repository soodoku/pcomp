"

Figure 3: Effect of Misperceptions on Perceived Extremity

@authors:
Douglas J. Ahler
doug.ahler@gmail.com

Gaurav Sood
gsood07@gmail.com

"

# Set the working directory to directory containing the data  
# setwd("data/")
	
# Load libs
library(grid)
library(ggplot2)
library(gridExtra)
library(haven)
# Install goji for nolead0s
#library(devtools)
#install_github("soodoku/goji")
library(goji)

"
Figure 3a
"

# Load data
# ep_maineffects_plot_data.dta is generated on line 660 of pcomp_replication_main.do
res <- read_dta("ep_maineffects_plot_data.dta")
# Cleaner conditions
res$condition <- factor(res$condition, levels = c("control", "ask", "tell"), labels = c("Control", "Ask", "Tell"))

par(mar = c(0, 0, 0, 0))
ggplot(res, aes(y = as.factor(condition), x = mean, xmin = mean + 1.96*se, xmax = mean - 1.96*se)) +
geom_point(color = "#42C4C7") + 
geom_errorbarh(height = 0, size = .25, color = "#42C4C7") +
scale_x_continuous(name = "Proportion of Extreme Out-Party Policy Placements", 
	               limits = c(.24, .40), 
	               breaks = seq(.24, .40, .04), 
	               labels = nolead0s(seq(.24, .40, .04))) + 
ylab("Experimental Condition") + 
theme_minimal() +
theme(panel.grid.major.y = element_line(colour = "#f3f3f3", linetype = "solid"),
	  panel.grid.minor.x = element_blank(),
	  panel.grid.major.x = element_line(colour = "#f3f3f3", linetype = "solid"),
	  panel.border       = element_blank(),
	  legend.position  = "bottom",
	  panel.background = element_rect(colour = "#d7d7d7"),
	  legend.key       = element_blank(),
	  legend.key.width = unit(1, "cm"),
	  title        = element_text(size = 12),
	  axis.title   = element_text(size = 15),
	  axis.text    = element_text(size = 12),
	  axis.ticks.y = element_blank(),
	  axis.ticks.x = element_line(colour = "#d7d7d7"),
	  axis.title.x = element_text(vjust = -1),
	  axis.title.y = element_text(vjust = 1),
	  plot.margin = unit(c(0,.5,.5,.5), "cm")) +  
annotate("text", x = res$mean[1], y = 2.75, label = paste0(nolead0s(round(res$mean[1], 2)),  " (", nolead0s(round(res$se[1], 2)), ")", "\n", "n = ", res$n[2], "\n", "(", res$n_part[1], " respondents)"), size = 4) + 
annotate("text", x = res$mean[2], y = 1.75, label = paste0(nolead0s(round(res$mean[2], 2)),  " (", nolead0s(round(res$se[2], 2)), ")", "\n", "n = ", res$n[1], "\n", "(", res$n_part[1], " respondents)"), size = 4) +
annotate("text", x = res$mean[3], y = .75,  label = paste0(nolead0s(round(res$mean[3], 2)),  " (", nolead0s(round(res$se[3], 2)), ")", "\n", "n = ", res$n[3], "\n", "(", res$n_part[3], " respondents)"), size = 4)

"
Figure 3b
"

# Load libs
library(dplyr)

# Fit the model
library(lme4)
library(optimx)

# Stata results for confirmation
# read.csv("xperceive/PartisanComposition/tabs/ep_clustered_slopeintercept.csv")

# Load and subset
data <- read_dta("extremity_exp_data.dta")
partisans <- subset(data,(pid_3 == 1 | pid_3 == 3))
mod <- with(partisans, glmer(perc_extreme ~ avg_error*condition + policy + (1 | responseid), 
					   control = glmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb")),
                       family = "binomial"))

library(coefplot)
coefplot(mod)

# Get the predictions
library(arm)
library(magrittr)

pframe0 <- with(partisans, expand.grid(perc_extreme = levels(factor(perc_extreme)), avg_error = levels(factor(avg_error)), condition = levels(factor(condition)), policy=levels(factor(policy))))
pframe0[,c("perc_extreme", "avg_error")] <- sapply(pframe0[,c("perc_extreme", "avg_error")], function(x) as.numeric(as.character(x)))

# Rescale error
pframe0$avg_error <- zero1(pframe0$avg_error)

# Get model matrix
mm <- model.matrix( ~ avg_error*condition + policy, data = pframe0)

# Get standard error and ypred
pframe1 <- data.frame(pframe0,eta=mm%*%fixef(mod))
pframe1 <- with(pframe1, data.frame(pframe1, perc_extreme = invlogit(eta)))
pframe1$pse <- diag(mm %*% tcrossprod(vcov(mod), mm))

# Get hi/low
pframe1$hi  <- invlogit(with(pframe1, eta + 2*pse))
pframe1$low <- invlogit(with(pframe1, eta - 2*pse))

# Group by avg_error, condition to average out
pframe2 <- pframe1 %>% 
		   group_by(avg_error, condition) %>% 
		   summarise(hi = mean(hi), low = mean(low), petreme = mean(perc_extreme.1))

# Plot
xlabel <- paste("Average Perceptual Bias (Magnitude of Correction)", sep = "")

par(mar=c(0,0,0,0))
ggplot() + 
geom_line(data=pframe2, size = 1.5, aes(x = pframe2$avg_error, y = pframe2$petreme, colour = as.factor(pframe2$condition))) + 
geom_ribbon(data=pframe2[pframe2$condition == "Ask",], aes(x = avg_error, ymin = low, ymax = hi), alpha = .1) + 
geom_ribbon(data=pframe2[pframe2$condition == "Control",], aes(x = avg_error, ymin = low, ymax = hi), alpha = .1) + 
geom_ribbon(data=pframe2[pframe2$condition == "Tell",], aes(x = avg_error, ymin = low, ymax = hi), alpha = .1) + 
scale_x_continuous(name = xlabel, 
				   limits = c(0, 1), 
				   breaks = seq(0, 1, .2), 
				   labels = nolead0s(seq(0, 1, .2))) + 
				   scale_colour_manual("", values = c("#cc5858" , "#49a68a", "#2e2f63")) +
scale_y_continuous(name ="Proportion of Extreme Out-Party Policy Placements", 
	               breaks = seq(0, .65, .05), 
	               labels = nolead0s(seq(0, .65, .05))) + 
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
annotate("text", x = 0.90, y = 0.292, label = "Tell", size = 5) + 
annotate("text", x = 0.90, y = 0.272, label = "(Approx. slope = .16)", size = 4) + 
annotate("text", x = 0.91, y = 0.414, label = "Ask", size = 5) + 
annotate("text", x = 0.91, y = 0.394, label = "(Approx. slope = .32)", size = 4) + 
annotate("text", x = 0.81, y = 0.590, label = "Control", size = 5) + 
annotate("text", x = 0.81, y = 0.570, label = "(Approx. slope = .39)", size=4) +
geom_vline(xintercept = .316, colour = "#999999", linetype = "longdash")
