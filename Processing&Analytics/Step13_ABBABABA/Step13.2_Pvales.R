#################################################
# NOTES: This script was run in RStudio. See the associated study for information on R version and further details. This script determines the relative p-values and significance of Z-score outputs from ABBABABA analyses run in Step13.1.
#################################################


##############################################
# Install and load required packages
##############################################
setwd("C:/Users/Franc/Documents/Hong Kong University Work/Projects/Cockatoos/Paper2/Results/abbababa/Regions/revised/")
# Vector of required packages
packages <- c("dplyr", "data.table")
# Install any missing packages
installed <- rownames(installed.packages())
for (pkg in packages) {
if (!(pkg %in% installed)) {
install.packages(pkg, dependencies = TRUE)
}
}
# Load the packages
library(dplyr)
library(data.table)
abba <- fread("Triton_ABBABABA_scores.txt", header = TRUE, sep = "\t")
# compute raw two-tailed p-values from Z if not present
abba$p_raw <- 2 * (1 - pnorm(abs(abba$Z)))
# corrections
abba$p_bonf  <- p.adjust(abba$p_raw, method = "bonferroni")
abba$p_holm  <- p.adjust(abba$p_raw, method = "holm")   # sequential Bonferroni (Holm)
abba$p_BH    <- p.adjust(abba$p_raw, method = "BH")     # Benjamini-Hochberg (FDR)
# significance flags at alpha = 0.05
alpha <- 0.05
abba$sig_raw  <- abba$p_raw < alpha
abba$sig_bonf <- abba$p_bonf < alpha
abba$sig_holm <- abba$p_holm < alpha
abba$sig_BH   <- abba$p_BH < alpha
# summary counts
cat("N tests:", nrow(abba), "\n")
cat("Significant (raw):", sum(abba$sig_raw), "\n")
cat("Significant (Bonferroni):", sum(abba$sig_bonf), "\n")
cat("Significant (Holm):", sum(abba$sig_holm), "\n")
cat("Significant (BH):", sum(abba$sig_BH), "\n")
# write out results
fwrite(abba, "Triton_ABBABABA_adjusted_pvalues.txt", sep = "\t")
