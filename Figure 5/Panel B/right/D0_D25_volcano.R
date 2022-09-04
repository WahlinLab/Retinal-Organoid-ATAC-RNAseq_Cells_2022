
# Load packages -----------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
})

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- "./raw/D0D25_dropped_duplicates.csv"

# Import data  ------------------------------------------------------------
results <- read.csv(path, header = TRUE)
# Format data  ------------------------------------------------------------

# Create columns from the column numbers specified
results <- results %>% mutate(pval = .[[4]],
                              log2fc = .[[3]],
                              labels = .[[2]])

# Get names for legend
down <- unlist(strsplit('Down,Not Sig,Up', split = ","))[1]
notsig <- unlist(strsplit('Down,Not Sig,Up', split = ","))[2]
up <- unlist(strsplit('Down,Not Sig,Up', split = ","))[3]

# Set colours
colours <- setNames(c("#67ab8e", "grey", "#a98de8"), c(down, notsig, up))

# Create significant (sig) column
results <- mutate(results, sig = case_when(
  pval < 0.1 & log2fc > 0.58 ~ up,
  pval < 0.1 & log2fc < -0.58 ~ down,
  TRUE ~ notsig))
write.csv(results, "./out/volcano.csv")


# Specify genes to label --------------------------------------------------

## Extract into vector ##

custlabels <- c("ESRP1", "L1TD1", "TDGF1", "VRTN", "CER1", "EEF1E1", "DPPA4", "CUZD1", "HES5", "BICD1", "SIX3", "DCT", "MKRN3")

# Label just the top genes in results table

results <- mutate(results, labels = ifelse(labels %in% custlabels, labels, ""))


# Create plot -------------------------------------------------------------

# Open file to save plot as PDF
pdf(file="./out/volcano.pdf", width=7,height=7)

# Set up base plot
p <- ggplot(data = results, aes(x = log2fc, y = -log10(pval))) +
  geom_point(aes(colour = sig)) +
  scale_color_manual(values = colours) +
  theme(axis.line = element_line(colour = "black"))

# Add gene labels
p <- p + geom_label_repel(data = filter(results, labels != ""), aes(label = labels),
                          min.segment.length = 0,
                          max.overlaps = Inf,
                          show.legend = FALSE)


# Set legend title
p <- p + theme(legend.title = element_blank())

# Print plot
print(p)

# Close PDF graphics device
dev.off()

