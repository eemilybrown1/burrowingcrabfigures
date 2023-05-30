#Figures for Burrowing Crab Paper

library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(ggpmisc)
library(gridExtra)

#Figure 2
fig2data <- read.csv("fig2data.csv")
colnames(fig2data)[1] <- "area"
fig2data <- mutate(fig2data, burrowcount_perm2 = (burrowcount_perquarterm2*4))
  
figure2 <-
fig2data %>%
  ggplot(aes(x=area, y=burrowcount_perm2, group=area)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(color = "black", size = 1.5, shape = 21, fill = NA) +
  xlab("Site number") +
  scale_x_continuous(breaks=seq(0,13,1)) +
  ylab(bquote("Crab burrows per m"^2)) +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        text=element_text(family="sans", size = 17))

figure2
  

#Figure 3
fig3data <- read.csv("fig3data.csv")
colnames(fig3data)[1] <- "burrowcount_perm2"

figure3 <- 
  fig3data %>%
  ggplot(aes(x = liveveg_percentcover, y= burrowcount_perm2)) +
  geom_point(color = "black", size = 1.5) +
  stat_poly_line(formula = y~x, se=TRUE) +
  stat_poly_eq(use_label(c("eq", "P")), formula = y~x, size = 5, label.x = 50) +
  xlab("Live vegetation (% cover)") +
  ylab(bquote("Crab burrows per m"^2)) +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        text=element_text(family="sans", size = 17)) +
  coord_cartesian(ylim= c(12, 300), xlim= c(4.75, 100))



  
  
#  stat_regline_equation(label.x = 75, label.y = 270, aes(x=liveveg_percentcover, y=yline,
#                                                         label = after_stat(eq.label))) +
#  annotate("text", x=82, y=260, label = lbl1, parse = TRUE)
#  geom_ribbon(aes(ymin=lowerCI, ymax=upperCI), linetype = 2, alpha=0.2)

figure3


###fitting exponential curve

mdl1 <- lm(burrowcount_perm2~liveveg_percentcover, data = fig3data)
mdl2 <- lm(burrowcount_perm2~liveveg_percentcover + I(liveveg_percentcover^2), data = fig3data)
mdl3 <- lm(burrowcount_perm2~liveveg_percentcover + I(liveveg_percentcover^2) + I(liveveg_percentcover^3), data = fig3data)
mdl4 <- lm(burrowcount_perm2~I(liveveg_percentcover^2), data = fig3data)

result <- fig3data
result$mdl1 <- predict(mdl1, newdata = fig3data)
result$mdl2 <- predict(mdl2, newdata = fig3data)
result$mdl3 <- predict(mdl3, newdata = fig3data)
result$mdl4 <- predict(mdl4, newdata = fig3data)

result <- melt(result, id.vars = "liveveg_percentcover", variable.name = "model",
               value.name = "fitted")

ggplot(result, aes(x = liveveg_percentcover, y = fitted)) +
  theme_bw() +
  geom_point(data = fig3data, aes(x=liveveg_percentcover, y=burrowcount_perm2)) +
  geom_line(aes(colour = model), size = 1)


###LOG TRANSFORMED

fig3data %>%
  ggplot(aes(x = liveveg_percentcover, y= burrowcount_perm2)) +
  geom_point(color = "black", size = 1.5) +
#  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  xlab("Live vegetation (% cover)") +
  ylab(bquote("Crab burrows per m"^2)) +
  scale_y_log10() +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        text=element_text(family="sans", size = 17)) +
  stat_poly_line(formula = y~x, se=TRUE) +
  stat_poly_eq(use_label(c("eq", "P")), size = 5, label.x = 50)

figure3
  

#Figure 4
fig4data <- read.csv("fig4data.csv")
colnames(fig4data) <- c("date", "exclosure02", "exclosure04", "exclosure06", "exclosure08", "exclosure10", "exclosure12")
fig4data$date <- as.Date(fig4data$date, "%Y-%m-%d")


figure4 <- 
fig4data %>%
  gather(key = "exclosure", value = "crabs", 
         exclosure02, 
         exclosure04, 
         exclosure06, 
         exclosure08,
         exclosure10,
         exclosure12) %>%
  ggplot(aes(x=date, y=crabs, color = exclosure, group = exclosure)) +
  geom_line(linewidth=1) +
  xlab("Date") +
  ylab(bquote("Number of crabs removed per 9 m"^2)) +
  scale_color_manual(name = "Exclosure", 
                      values = c("exclosure02" = "red", 
                                 "exclosure04" = "orange", 
                                 "exclosure06" = "green", 
                                 "exclosure08" = "lightblue", 
                                 "exclosure10" = "blue", 
                                 "exclosure12" = "purple")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 150)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text=element_text(family="sans", size = 17))
figure4

#Figure 5
fig5data <- read.csv("fig5data.csv")
colnames(fig5data)[1] <- "burrows"

figure5 <- 
  fig5data %>%
  gather(key = "Species", value = "pccover", 
         salicornia_spp, 
         other_forbs) %>%
  ggplot(aes(x=burrows, y=pccover, group = Species, shape = Species, color = Species, linetype = Species)) +
  geom_point(size = 3) +
  stat_poly_line(formula = y~0+x, se=FALSE) +
  stat_poly_eq(use_label(c("eq", "P")), formula = y~0+x, size = 5) +
  xlab("Average # burrows") +
    ylab("Percent ground cover") +
    scale_color_manual(labels = c("Other forbs", "Salicornia species"),
                       values = c("darkorange1", "blue")) +
    scale_shape_manual(labels = c("Other forbs", "Salicornia species"),
                       values = c(16, 17)) +
    scale_linetype_manual(labels = c("Other forbs", "Salicornia species"),
                          values = c("solid", "dashed")) +
    scale_y_continuous(expand = c(0,0), limits = c(-1, 12)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line = element_line(colour = "black"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(), 
          panel.border = element_blank(),
          text=element_text(family="sans", size = 17),
          legend.key.size = unit(3, "line"))
  
figure5
  
#Figure 6 
fig6data <- read.csv("fig6data.csv")
colnames(fig6data)[1] <- "burrows"

figure6 <- 
  fig6data %>%
  gather(key = "Cover", value = "pccover", 
         wrack, 
         bare) %>%
  ggplot(aes(x=burrows, y=pccover, group = Cover, shape = Cover, color = Cover, linetype = Cover)) +
  geom_point(size = 3) +
  stat_poly_line(method = "lm", inherit.aes = TRUE, se=FALSE) +
  stat_poly_eq(use_label(c("eq", "P")), size = 5) +
  xlab("Average # burrows") +
  ylab("Percent ground cover") +
  scale_color_manual(labels = c("Bare", "Wrack"),
                     values = c("darkorange1", "blue")) +
  scale_shape_manual(labels = c("Bare", "Wrack"),
                     values = c(16, 17)) +
  scale_linetype_manual(labels = c("Bare", "Wrack"),
                        values = c("solid", "dashed")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 45)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        text=element_text(family="sans", size = 17),
        legend.key.size = unit(3, "line")) 

figure6

comb5and6 <- ggarrange(plotlist = list(figure5, figure6),
                       ncol = 2,
                       nrow = 1,
                       labels = "auto",
                       legend = "bottom")
comb5and6

#Table for salicornia, other forbs (forced through zero), wrack, bare (not forced through zero)

lm_salicornia = lm(
  salicornia_spp ~ 0+burrows,
  data = fig5data
)
salcoef <-
  lm_salicornia$coefficients
saleq <- paste("Y =", paste(round(salcoef[1],2), paste("x")))
saleq

salr2 <- summary(lm_salicornia)$r.squared
salp <- summary(lm_salicornia)$coefficients[,4]


lm_otherforbs = lm(
  other_forbs ~ 0+burrows,
  data = fig5data
)
forbcoef <-
  lm_otherforbs$coefficients
forbeq <- paste("Y =", paste(round(forbcoef[1],2), paste("x")))
forbeq

forbr2 <- summary(lm_otherforbs)$r.squared
forbp <- summary(lm_otherforbs)$coefficients[,4]


lm_wrack = lm(
  wrack ~ burrows,
  data = fig6data
)
wrackcoef <-
  lm_wrack$coefficients
wrackeq <- 
  paste("Y =", paste(round(wrackcoef[1],2), paste(round(wrackcoef[-1],2), "x", sep=" * ", collapse=" + "), sep=" + "))
wrackeq

wrackr2 <- summary(lm_wrack)$r.squared
  wrackp <- summary(lm_wrack)$coefficients[,4][2]


lm_bare = lm(
  bare ~ burrows,
  data = fig6data
)
barecoef <-
  lm_bare$coefficients
bareeq <- 
  paste("Y =", paste(round(barecoef[1],2), paste(round(barecoef[-1],2), "x", sep=" * ", collapse=" + "), sep=" + "))
bareeq

barer2 <- summary(lm_bare)$r.squared
barep <- summary(lm_bare)$coefficients[,4][2]

cover_category <- c("Salicornia sp.", "Other forbs", "Wrack", "Bare")
equation <- c(saleq, forbeq, wrackeq, bareeq)
r2 <- c(salr2, forbr2, wrackr2, barer2)
p <- c(salp, forbp, wrackp, barep)

equationstable <-
data.frame(cover_category,
           equation,
           r2,
           p)

gridExtra::grid.table(equationstable)

#Figure 7
fig7data <- read.csv("fig7data.csv")

colnames(fig7data)[1] <- "treatment"

fig7data <- fig7data %>%
  mutate(vegspeciescount = rowSums(fig7data[,c(4:13)] != 0))

fig7data <- fig7data[c(1:12),]

###this table got messed up. need to fix
fig7summary <-
  fig7data %>%
  group_by(treatment) %>%
  summarize(mean = mean(vegspeciescount), se = sd(vegspeciescount)/
                                                  (sqrt(length(vegspeciescount))))
  

figure7 <-
  fig7summary %>%
  ggplot(aes(x=treatment, y=mean, fill = treatment)) +
  geom_bar(stat = "identity", width=.5, alpha=0.7) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2) +
  xlab("Treatment") +
  ylab("Species richness") +
  scale_fill_manual(values = c("blue", "darkorange1")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 9)) +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5), 
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        text=element_text(family="sans", size = 20))

figure7


#Figure 8
fig8data <- read.csv("fig8data.csv")
colnames(fig8data)[1] <- "site"

fig8summary <-
  fig8data %>%
  pivot_longer(cols = c(-site, -plot, -subplot, -treatment),
               names_to = "species",
               values_to = "c_pccover") %>%
  group_by(treatment, species) %>%
  summarize(mean = mean(c_pccover),
            se = sd(c_pccover)/
                     (sqrt(length(c_pccover)))) %>%
  filter(species %in% c("spal", "suaeda", "salicornia", "disp", "iva", "sppa", "forbs", "bare", "dead", "wrack")) %>%
  mutate(significant = (ifelse(species %in% c("spal", 
                                           "suaeda",
                                           "iva", 
                                           "bare"), 
                              TRUE,
                              FALSE)))
     

figure8 <-
  fig8summary %>%
  ggplot(aes(x=species, y=mean, fill = treatment)) +
  geom_bar(stat = "identity", width=.6, position = "dodge") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(.6)) +
  geom_hline(yintercept = 0) +
  xlab("Species") +
  ylab("Change in Percent Cover ") +
  scale_fill_manual(name = "Treatment", 
    values = c("blue", "darkorange1")) +
  scale_x_discrete(limits = c("spal", "suaeda", "salicornia", "disp", "iva", "sppa", "forbs", "bare", "dead", "wrack"),
    labels = c("spal" = ~italic("Spartina alterniflora"),
                              "suaeda" = ~italic("Suaeda maritima"),
                              "salicornia" = ~italic("Salicornia sp."),
                              "disp" = ~italic("Distichlis spicata"),
                              "iva" = ~italic("Iva frutescens"),
                              "sppa" = ~italic("Spartina patens"),
                              "forbs" = "Other forbs",
                              "bare" = "Bare",
                              "dead" = "Dead",
                              "wrack" = "Wrack")) +
  scale_y_continuous(expand = c(0,0), limits = c(-40, 30)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text=element_text(family="sans", size = 20))

figure8
