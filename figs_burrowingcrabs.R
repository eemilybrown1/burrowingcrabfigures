#Figures for Burrowing Crab Paper

library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)

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
  ggtitle("Burrow density") +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        text=element_text(family="sans"))

figure2
  

#Figure 3
fig3data <- read.csv("fig3data.csv")
colnames(fig3data)[1] <- "burrowcount_perm2"

lbl1<- paste("R ^ 2 == 0.2004")

figure3 <- 
  fig3data %>%
  ggplot(aes(x = liveveg_percentcover, y= burrowcount_perm2)) +
  geom_point(color = "black", size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color="blue") +
  xlab("Live vegetation (% cover)") +
  ylab(bquote("Crab burrows per m"^2)) +
  coord_cartesian(ylim= c(12, 300), xlim= c(4.75, 100)) +
  ggtitle("Crab burrows and live vegetation") +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        text=element_text(family="sans")) +
  stat_regline_equation(label.x = 75, label.y = 270, aes(x=liveveg_percentcover, y=yline,
                                                         label = after_stat(eq.label))) +
  annotate("text", x=82, y=260, label = lbl1, parse = TRUE) +
  geom_ribbon(aes(ymin=lowerCI, ymax=upperCI), linetype = 2, alpha=0.2)

figure3
  

#Figure 4
fig4data <- read.csv("fig4data.csv")
colnames(fig4data)[1] <- "date"

figure4 <- 
fig4data %>%
  gather(key = "exclosure", value = "crabs", 
         exclosure12.2, 
         exclosure12.4, 
         exclosure12.6, 
         exclosure13.2,
         exclosure13.4,
         exclosure13.6) %>%
  ggplot(aes(x=date, y=crabs, color = exclosure, group = exclosure)) +
  geom_line(linewidth=1) +
  xlab("Date") +
  ylab(bquote("Number of crabs removed per 9 m"^2)) +
  scale_color_manual(name = "Exclosure", 
                      values = c("exclosure12.2" = "red", 
                                 "exclosure12.4" = "orange", 
                                 "exclosure12.6" = "green", 
                                 "exclosure13.2" = "lightblue", 
                                 "exclosure13.4" = "blue", 
                                 "exclosure13.6" = "purple")) +
  ggtitle("Crab removal through time") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 150)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text=element_text(family="sans"))
figure4

#Figure 5
fig5data <- read.csv("fig5data.csv")
colnames(fig5data)[1] <- "burrows"

figure5 <- 
  fig5data %>%
  gather(key = "plant", value = "pccover", 
         salicornia_spp, 
         other_forbs) %>%
  ggplot(aes(x=burrows, y=pccover, color = plant, group = plant)) +
  geom_point(size = 2) +
    labs(color = "") + 
    xlab("Average # burrows") +
    ylab("Vegetation % cover") +
    scale_color_manual(labels = c("Other forbs", "Salicornia species"),
                       values = c("blue", "darkorange1")) +
    ggtitle("Salicornia spp. and Other Forbs by Average Burrow Count") +
    scale_y_continuous(expand = c(0,0), limits = c(-1, 12)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.line = element_line(colour = "black"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(), 
          panel.border = element_blank(),
          text=element_text(family="sans")) +
    stat_poly_line(formula = y~0+x, se=FALSE, linetype = "dashed") +
    stat_poly_eq(use_label(c("eq", "P")), formula = y~0+x, sep = "  ")
  
figure5
  
#Figure 6
fig6data <- read.csv("fig6data.csv")
colnames(fig6data)[1] <- "burrows"

figure6 <- 
  fig6data %>%
  gather(key = "gcov", value = "pccover", 
         wrack, 
         bare) %>%
  ggplot(aes(x=burrows, y=pccover, color = gcov, group = gcov)) +
  geom_point(size = 2) +
  labs(color = "") + 
  xlab("Average # burrows") +
  ylab("Cover category %") +
  scale_color_manual(labels = c("Bare", "Wrack"),
                     values = c("darkorange1", "blue")) +
  ggtitle("Wrack and Bare Ground by Average Burrow Count") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 45)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        text=element_text(family="sans")) +
#  stat_cor(label.x = 18) +
#  stat_regline_equation() +
  stat_poly_line(method = "lm", se=FALSE, linetype = "dashed") +
  stat_poly_eq(use_label(c("eq", "P")))

figure6

#Figure 7
fig7data <- read.csv("fig7data.csv")
colnames(fig7data)[1] <- "treatment"

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
  ggtitle("Species Richness by Treatment") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 9)) +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5), 
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        text=element_text(family="sans"))

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
  ggtitle("Change in Vegetation Species Percent Cover After Crab Exclusion") +
  scale_y_continuous(expand = c(0,0), limits = c(-40, 30)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text=element_text(family="sans"))

figure8
