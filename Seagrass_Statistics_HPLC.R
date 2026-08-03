
library(cowplot)
library(dplyr)
library(factoextra)
library(ggplot2)
library(ggpmisc)
library(ggrepel)
library(reshape2) 
library(tidyverse)
library(vegan)

# Data wrangling ------------------------------------------------------------------------------------

OriginalDB <- read.csv("Supplemental_File_2_FFA_data.csv")
OriginalDB <- OriginalDB[-c(1,2,27),]
OriginalDB <- OriginalDB[!(OriginalDB$Species %in% "Unknown bird"),]
OriginalDB <- OriginalDB[!(OriginalDB$Species %in% "Blank"),]
OriginalDB <- OriginalDB[!(OriginalDB$Species %in% "Pool"),]


# PCA of the full FFA results ---------------------------------------------------------------------

PCA_df <- OriginalDB
rownames(PCA_df) <- PCA_df$Sample 
PCA_df[, 4:41] <- sapply(PCA_df[, 4:41], as.numeric)

forPCA <- PCA_df[, (4:41)]

myPCA <- prcomp(forPCA[,-1], center = TRUE, retx = TRUE)

percentage <- round(myPCA$sdev / sum(myPCA$sdev) * 100, 2)
percentage <- paste(colnames(myPCA), "(", paste( as.character(percentage), "%", ")", sep="") )

PCA_df$Sample <- rownames(PCA_df)

PCA.df <- as.data.frame(myPCA$x, row.names = FALSE)
PCA.df$Sample <- rownames(PCA_df)
PCA_df_total <- dplyr::left_join(PCA.df, PCA_df, by = "Sample")

Full_PCA_plot <- ggplot(PCA_df_total, aes(x = PC1, y = PC2, fill = Treatment, group = Treatment)) + 
  geom_vline(colour = "#000000", xintercept = 0) + 
  geom_hline(colour = "#000000", yintercept = 0) + 
  stat_ellipse(aes(fill = Treatment), geom = "polygon", type = "t", level = 0.95, alpha = 0.1) + 
  geom_point(colour = "black", shape = 21) + 
  xlab(paste0("PC1", percentage[1])) + ylab(paste0("PC2", percentage[2])) + 
  guides(size = FALSE, fill = guide_legend(override.aes = list(size = 3))) + 
  ggtitle("Principle Components Analysis - Full FFAs")
Full_PCA_plot

fviz_pca_biplot(myPCA, label ="var", col.var="purple", col.ind="grey", 
                              labelsize = 3, repel = TRUE, 
                select.var = list(cos2 = 0.6), 
                select.ind = list(cos2 = 0.6)) + 
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) + 
  labs(title ="", x = percentage[1], y = percentage[2])


#PCA of normalized FAs ---------------------------------------------------------------------------

norm_perTot <- OriginalDB
norm_perTot[, 4:41] <- (norm_perTot[, 4:41] / rowSums(norm_perTot[, 4:41]))*100

perTotPCA_df <- norm_perTot

rownames(perTotPCA_df) <- perTotPCA_df$Sample 

names(perTotPCA_df) <- gsub(x = names(perTotPCA_df), pattern = "X", replacement = "")  
names(perTotPCA_df) <- gsub(x = names(perTotPCA_df), pattern = "n\\.", replacement = "n")  
names(perTotPCA_df) <- gsub(x = names(perTotPCA_df), pattern = "\\.", replacement = ":")  

perTotPCA_df[, 4:41] <- sapply(perTotPCA_df[, 4:41], as.numeric)
forperTotPCA <- perTotPCA_df[, (4:41)]

myperTotPCA <- prcomp(forperTotPCA[,-1], center = TRUE, retx = TRUE)

percentage <- round(myperTotPCA$sdev / sum(myperTotPCA$sdev) * 100, 2)
percentage <- paste(colnames(myperTotPCA), "(", paste( as.character(percentage), "%", ")", sep="") )

perTotPCA_df$Sample <- rownames(perTotPCA_df)

perTotPCA.df <- as.data.frame(myperTotPCA$x, row.names = FALSE)
perTotPCA.df$Sample <- rownames(perTotPCA_df)
perTotPCA_df_total <- dplyr::left_join(perTotPCA.df, perTotPCA_df, by = "Sample")

perTotPCA_df_total$Species_simple <- gsub(" \\(.*", "", perTotPCA_df_total$Species)

Treatment_Hull_PCA <- perTotPCA_df_total %>% group_by(Treatment) %>% slice(c(chull(PC1, PC2), chull(PC1, PC2)[1]))
Species_Hull_PCA <- perTotPCA_df_total %>% group_by(Species_simple) %>% slice(c(chull(PC3, PC2), chull(PC3, PC2)[1]))

Treatment_label_PCA <- perTotPCA_df_total %>% 
  group_by(Treatment) %>% 
  dplyr::summarize(label_X = mean(PC1), label_Y = mean(PC2))
Species_label_PCA <- perTotPCA_df_total %>% 
  group_by(Species_simple) %>% 
  dplyr::summarize(label_X = mean(PC3), label_Y = mean(PC2))


perTotPCA_plot_Treatment <- ggplot(perTotPCA_df_total, aes(x = PC1, y = PC2, fill = Treatment, group = Treatment)) + 
  geom_vline(colour = "#000000", xintercept = 0, linetype = "dashed") + 
  geom_hline(colour = "#000000", yintercept = 0, linetype = "dashed") + 
  scale_fill_manual(values = c(
    "Mudflat" = "#786956", 
    "Healthy meadow" = "#12a138")) + 
  scale_colour_manual(values = c(
    "Mudflat" = "#786956", 
    "Healthy meadow" = "#12a138")) + 
  stat_ellipse(aes(fill = Treatment), geom = "polygon", type = "t", level = 0.80, alpha = 0.2) + 
  geom_point(colour = "black", shape = 21, size = 4) + 
  geom_label_repel(dat = Treatment_label_PCA, fontface = "bold", size = 3, color = "white",
                   segment.color = NA, aes(label = Treatment,
                                           x = label_X, y = label_Y, fill = Treatment, 
                                           family = "Times")) +
  xlab(paste0("PC1", percentage[1])) + ylab(paste0("PC2", percentage[2])) +
  theme_linedraw() + 
  ggtitle(" ") + 
  xlim(-50, 60) + ylim(-30, 50) + 
  theme(legend.position = "none", legend.title = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        text = element_text(family = "Times", size = 15))
perTotPCA_plot_Treatment

perTotPCA_plot_Species <- ggplot(perTotPCA_df_total, aes(x = PC3, y = PC2, fill = Species_simple, group = Species_simple)) + 
  geom_vline(colour = "#000000", xintercept = 0, linetype = "dashed") + 
  geom_hline(colour = "#000000", yintercept = 0, linetype = "dashed") + 
  geom_polygon(data = Species_Hull_PCA, aes(colour = Species_simple), alpha = 0.3) +
  geom_point(colour = "black", shape = 21, size = 4) + 
  geom_label_repel(dat = Species_label_PCA, fontface = "bold", size = 3, color = "white",
                   segment.color = NA, aes(label = Species_simple,
                   x = label_X, y = label_Y, fill = Species_simple, 
                   family = "Times")) +
  ylab(paste0("PC2", percentage[2])) + xlab(paste0("PC3", percentage[3])) + 
  theme_linedraw() + 
  ggtitle(" ") + 
  theme(legend.position = "none", legend.title = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        text = element_text(family = "Times", size = 15))
perTotPCA_plot_Species

Eigen_eco <- fviz_pca_biplot(myperTotPCA, axes = c(1, 2), label = "var", 
                             col.var = "purple", col.ind = "grey", 
                             labelsize = 3, repel = TRUE, 
                             select.var = list(cos2 = 0.6)) + 
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
  text = element_text(family = "Times", size = 15)) + 
  xlim(-50, 60) + ylim(-30, 50) + 
  labs(title ="", x = paste0("PC1", percentage[1]), y = paste0("PC2", percentage[2]))
Eigen_eco

Eigen_birds <- fviz_pca_biplot(myperTotPCA, axes = c(3, 2), label = "var", col.var="purple", col.ind="grey", 
                labelsize = 3, repel = TRUE, 
                select.var = list(cos2 = 0.6)) + 
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    text = element_text(family = "Times", size = 15)) + 
  labs(title ="", x = paste0("PC3", percentage[3]), y = paste0("PC2", percentage[2]))
Eigen_birds

PCA_all_plots <- plot_grid(perTotPCA_plot_Treatment, Eigen_eco, 
                           perTotPCA_plot_Species, Eigen_birds, 
                           nrow = 2, labels = c("A", "B", "C", "D"))
PCA_all_plots
ggsave("PCA_all_plots.png", plot = PCA_all_plots, width = 10, height = 10, units = c("in"))
ggsave("Figure_3.pdf", plot = PCA_all_plots, width = 10, height = 10, units = c("in"))


# Linegraph of FAs per bird ---------------------------------------------------------------------------

norm_perTot_line <- norm_perTot
names(norm_perTot_line) <- gsub(x = names(norm_perTot_line), pattern = "X", replacement = "")  
names(norm_perTot_line) <- gsub(x = names(norm_perTot_line), pattern = "n.", replacement = "n")  
names(norm_perTot_line) <- gsub(x = names(norm_perTot_line), pattern = "\\.", replacement = ":")  

norm_perTot_line_melt <- melt(norm_perTot_line, id = c("Species","Treatmen", "Sample")) 

mean_data <- group_by(norm_perTot_line_melt, Species, variable) %>%
  summarise(value = mean(value, na.rm = TRUE))
mean_data$Species_simple <- gsub(" \\(.*", "", mean_data$Species)


FA_linegraph <- ggplot(mean_data, aes(x = variable, 
                                      y = value, 
                                      color = Species_simple, group = Species_simple)) + 
  geom_vline(colour = "#000000", xintercept = 0) + 
  geom_hline(colour = "#000000", yintercept = 0) + 
  geom_line() + 
  geom_point(aes(shape = Species_simple)) + 
  ylab("proportion of total FFA") + xlab("") + 
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  theme(text = element_text(family = "Times", size = 12)) + 
  guides(shape = FALSE, size = FALSE, color = guide_legend(title = "Species"), 
         fill = guide_legend(override.aes = list(size = 3))) + 
  ggtitle("FFA values averaged per species")
FA_linegraph
ggsave("FA_linegraph.png", plot = FA_linegraph, width = 8.5, height = 3,
       units = c("in"), dpi = 500)
ggsave("Figure_2.pdf", plot = FA_linegraph, width = 8.5, height = 3,
       units = c("in"), dpi = 500)




# Scatterplots/barplots of FAs ----------------------------------------------------------------

orig_unknowns <- read.csv("FA_PCA.csv") # this redoes the normalization to include unknown species
orig_unknowns <- orig_unknowns[-c(27),]
temp_norm_perTot <- orig_unknowns
temp_norm_perTot[, 4:41] <- (temp_norm_perTot[, 4:41] / rowSums(temp_norm_perTot[, 4:41]))*100
temp_norm_perTot$Facet <- ifelse(grepl("eelgrass", temp_norm_perTot$Species), paste0("Seagrass controls"), paste0("Avian fecal samples"))

scatterplot_label <- temp_norm_perTot %>% 
  group_by(Treatment) %>% 
  dplyr::summarize(label_X = mean(X24.0  + X24.1n.9 + X25.0  + X26.0  + X28.0  + X29.0 + X30.0), 
                   label_Y = mean(X22.6n.3))

DHA_LCFA_scatterplot <- ggplot(temp_norm_perTot, aes(x = (X24.0  + X24.1n.9 + X25.0  + X26.0  + X28.0  + X29.0 + X30.0), 
                                                y = X22.6n.3, 
                                                fill = Treatment, group = Treatment)) + 
  geom_vline(colour = "#000000", xintercept = 0) + 
  geom_hline(colour = "#000000", yintercept = 0) + 
  scale_color_manual(values = c(
    "Mudflat" = "#786956", 
    "Healthy meadow" = "#12a138", 
    "Healthy" = "#00cf37", 
    "Wasting disease" = "#4f3b27")) + 
  scale_fill_manual(values = c(
    "Mudflat" = "#786956", 
    "Healthy meadow" = "#12a138", 
    "Healthy" = "#00cf37", 
    "Wasting disease" = "#4f3b27")) + 
  stat_ellipse(aes(fill = Treatment), geom = "polygon", type = "t", level = 0.80, alpha = 0.2) +
  geom_point(colour = "black", shape = 21, size = 5) + 
  geom_label_repel(dat = scatterplot_label, fontface = "bold", size = 3, color = "white",
                   segment.color = NA, aes(label = Treatment,
                   x = label_X, y = label_Y, fill = Treatment, 
                   family = "Times")) +
  theme(text = element_text(family = "Times", size = 12)) + 
  xlab("LCFAs (% total FFA)") + ylab("DHA (% total FFA)") + 
  theme(legend.position = "none") + 
  ggtitle("")
DHA_LCFA_scatterplot
ggsave("DHA_vs_LCFA_FAs.png", plot = DHA_LCFA_scatterplot, width = 6, height = 6,
       units = c("in"), dpi = 500)

DHA_LCFA_barplot <- ggplot(temp_norm_perTot, aes(x = Treatment, 
                                           y = ((X22.6n.3)/
                                                  (X22.6n.3 + X24.1n.9 + X24.0  + X25.0  + X26.0  + 
                                                     X28.0  + X29.0 + X30.0)), 
                                           fill = Treatment, group = Treatment)) + 
  facet_grid( ~Facet, scales = "free", space = "free") +
  geom_hline(yintercept = 0, linetype = 1, color = "#878787") + 
  scale_color_manual(values = c(
    "Mudflat" = "#786956", 
    "Healthy meadow" = "#12a138", 
    "Healthy" = "#08c43a", 
    "Wasting disease" = "#4f3b27")) + 
  scale_fill_manual(values = c(
    "Mudflat" = "#786956", 
    "Healthy meadow" = "#12a138", 
    "Healthy" = "#08c43a", 
    "Wasting disease" = "#4f3b27")) + 
  geom_boxplot(aes(color = Treatment, fill = Treatment, alpha = 0.01), 
               position = position_dodge2(width = 1, 
               preserve = "single"), varwidth = FALSE, outlier.shape = NA) + 
  xlab(" ") + 
  geom_point(alpha = 0.5, position = position_jitterdodge()) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  ylab("DHA / DHA + LCFA") + 
  theme_bw() +
  theme(text = element_text(family = "Times", size = 16)) + 
  guides(alpha = "none", fill = "none", colour = "none") 
DHA_LCFA_barplot
ggsave("DHA_LCFA_barplot.png", plot = DHA_LCFA_barplot, width = 6, height = 6,
       units = c("in"), dpi = 500)


temp_norm_perTot$Species_simple <- gsub(" \\(.*", "", temp_norm_perTot$Species)

DHA_LCFA_barplot_species <- ggplot(filter(temp_norm_perTot, Facet == "Avian fecal samples"), 
                                   aes(x = Species_simple, 
                                                 y = ((X22.6n.3)/
                                                        (X22.6n.3 + X24.0  + X24.1n.9 + X25.0  + X26.0  + 
                                                         X28.0  + X29.0 + X30.0)), 
                                                 fill = Species, group = Species)) + 
  facet_grid( ~Treatment, scales = "free", space = "free") +
  geom_hline(yintercept = 0, linetype = 1, color = "#878787") + 
  geom_boxplot(aes(color = Species, fill = Species, alpha = 0.01), 
               position = position_dodge2(width = 1, 
                                          preserve = "single"), varwidth = FALSE, outlier.shape = NA) + 
  xlab(" ") + 
  geom_point(alpha = 0.5, position = position_jitterdodge()) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  ylab("DHA / DHA + LCFA") + 
  theme_bw() +
  theme(text = element_text(family = "Times", size = 16)) + 
  guides(alpha = "none", fill = "none", colour = "none") 
DHA_LCFA_barplot_species

bar_proxy_plots <- plot_grid(DHA_LCFA_barplot, DHA_LCFA_barplot_species,
                            nrow = 2 , labels = c("A", "B"))
bar_proxy_plots
ggsave("bar_proxy_plots.png", plot = bar_proxy_plots, width = 5, height = 8, units = c("in"))
ggsave("Figure_4.pdf", plot = bar_proxy_plots, width = 5, height = 8, units = c("in"))




# ANOVA for location vs species -------------------------------------------------

# ANOVA for all FFAs
aov_FA_all <- aov(X10.0 + X11.0  + X12.0  + X13.0  + X14.0 + X14.1n.9 + X15.0  + X15.1n.2 + X16.0  + 
                    X16.1n.7 + X17.0  + X18.0  + X18.1n.9 + X18.2n.6 + X18.3n.3 + X19.0  + X20.0  + 
                    X20.2n.6 + X20.3n.3 + X20.3n.6 + X20.4n.6 + X20.4n.7 + X20.5n.3 + X21.0  + X22.0 +
                    X22.1n.9 + X22.2n.6 + X22.4n.6 + X22.5n.3 + X22.6n.3 + X23.0  + X24.0  + X24.1n.9 + 
                    X25.0  + X26.0  + X28.0  + X29.0 + X30.0
                    ~ Treatment, data = norm_perTot)
hist(resid(aov_FA_all), breaks = 5)
summary(aov_FA_all)

# ANOVA for SFAs
aov_SFA <- aov(X10.0 + X11.0  + X12.0  + X13.0  + X14.0 + X15.0  + X16.0  + 
                 X17.0  + X18.0  + X19.0  + X20.0  + X21.0  + X22.0 +
                 X23.0  + X24.0  + X25.0  + X26.0  + X28.0  + X29.0 + X30.0 ~ 
                 Treatment, data = norm_perTot)
hist(resid(aov_SFA), breaks = 5)
summary(aov_SFA)

# ANOVA for MUFAs
aov_MUFA <- aov(X14.1n.9 + X15.1n.2 + X16.1n.7 + X18.1n.9 + X22.1n.9 + X24.1n.9 ~ 
                  Treatment, data = norm_perTot)
hist(resid(aov_MUFA), breaks = 5)
summary(aov_MUFA)

# ANOVA for PUFAs
aov_PUFA <- aov(X18.2n.6 + X18.3n.3 + X20.2n.6 + X20.3n.3 + X20.3n.6 + X20.4n.6 + X20.4n.7 + X20.5n.3 + 
                  X22.2n.6 + X22.4n.6 + X22.5n.3 + X22.6n.3 ~ 
                  Treatment, data = norm_perTot)
hist(resid(aov_PUFA), breaks = 5)
summary(aov_PUFA)

# ANOVA for LCFAs
LCFA_aov <- aov(X24.0  + X24.1n.9 + X25.0  + X26.0  + X28.0  + X29.0 + X30.0 ~ 
                  Treatment, data = norm_perTot)
hist(resid(LCFA_aov), breaks = 5)
summary(LCFA_aov)

# ANOVA for seagrass lipids
seagrass_aov <- aov(X18.2n.6 + X18.3n.3 + X28.0 ~ Treatment, data = norm_perTot)
hist(resid(seagrass_aov), breaks = 5)
summary(seagrass_aov)

# ANOVA for DHA normalized to LCFAs
DHA_LCFA_aov <- aov( (X22.6n.3 / 
                        (X22.6n.3 + X24.0 + X24.1n.9 + X25.0 + X26.0 + X28.0 + X29.0 + X30.0)) ~ 
                       Treatment, data = norm_perTot)
hist(resid(DHA_LCFA_aov), breaks = 5)
summary(DHA_LCFA_aov)

# ANOVA for terrestrial vs marine PUFAs following Colombo et al. 2017
terr_vs_mar_aov <- aov(((X18.2n.6 + X18.3n.3) / 
                       (X20.5n.3 + X22.6n.3 + X20.4n.6)) ~ 
                        Treatment, data = norm_perTot)
hist(resid(terr_vs_mar_aov), breaks = 5)
summary(terr_vs_mar_aov)




# PCA of all lipids --------------------------------------------------------------------------------

FullLipidsDB <- read.csv("Supplemental_File_1_HPLC_data.csv")
str(FullLipidsDB)

FullLipidsDB <- FullLipidsDB[-c(1,2,27),]
FullLipidsDB <- FullLipidsDB[!(FullLipidsDB$Species %in% "Unknown bird"),]
FullLipidsDB <- FullLipidsDB[!(FullLipidsDB$Species %in% "Blank"),]
FullLipidsDB <- FullLipidsDB[!(FullLipidsDB$Species %in% "Pool"),]

PCA_totLip_df <- FullLipidsDB
rownames(PCA_totLip_df) <- PCA_totLip_df$Sample 
PCA_totLip_df[, 4:1067] <- sapply(PCA_totLip_df[, 4:1067], as.numeric)

forPCA_totLip <- PCA_totLip_df[, (4:1067)]

myPCA_totLip <- prcomp(forPCA_totLip[,-1], center = TRUE, retx = TRUE)

percentage <- round(myPCA_totLip$sdev / sum(myPCA_totLip$sdev) * 100, 2)
percentage <- paste(colnames(myPCA_totLip), "(", paste( as.character(percentage), "%", ")", sep="") )

PCA_totLip_df$Sample <- rownames(PCA_totLip_df)

PCA_totLip.df <- as.data.frame(myPCA_totLip$x, row.names = FALSE)
PCA_totLip.df$Sample <- rownames(PCA_totLip_df)
PCA_totLip_df_total <- dplyr::left_join(PCA_totLip.df, PCA_totLip_df, by = "Sample")


Treatment_Hull_PCA <- PCA_totLip_df_total %>% group_by(Treatment) %>% slice(c(chull(PC2, PC3), chull(PC2, PC3)[1]))
Species_Hull_PCA <- PCA_totLip_df_total %>% group_by(Species) %>% slice(c(chull(PC2, PC3), chull(PC2, PC3)[1]))

PCA_totLip_plot <- ggplot(PCA_totLip_df_total, aes(x = PC2, y = PC3, fill = Treatment, group = Treatment)) + 
  geom_vline(colour = "#000000", xintercept = 0) + 
  geom_hline(colour = "#000000", yintercept = 0) + 
  geom_polygon(data = Treatment_Hull_PCA, aes(colour = Treatment), alpha = 0.3) + 
  geom_point(colour = "black", shape = 21, size = 2.5) + 
  scale_fill_manual(values = c(
    "Mudflat" = "#786956", 
    "Healthy meadow" = "#12a138")) + 
  scale_colour_manual(values = c(
    "Mudflat" = "#786956", 
    "Healthy meadow" = "#12a138")) + 
  xlab(paste0("PC2", percentage[2])) + ylab(paste0("PC3", percentage[3])) + 
  guides(size = FALSE, fill = guide_legend(override.aes = list(size = 3))) + 
  theme(text = element_text(family = "Times", size = 12)) + 
  ggtitle("Supplemental Figure 1: PCA of total lipids")
PCA_totLip_plot
ggsave("Supp Figure 1.pdf", plot = PCA_totLip_plot, width = 6, height = 6, units = c("in"))


