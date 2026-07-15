# fetches and caches the NCI60 tumor microarray data (Hastie et al., Elements
# of Statistical Learning) used in the Cluster Validation section of Chapter 6.
# data/ is gitignored, so this needs to be re-run any time data/ is rebuilt
# from scratch, matching the convention in manifold_examples.R.
setwd("/Users/alexyoung/Documents/Teaching/Stat 185 - Unsupervised Learning/IntroUL/examples/")

nci.data <- t(as.matrix(read.table(url("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/nci.data.csv"),
                                    sep = ",", row.names = 1, header = TRUE)))
nci.label <- read.table(url("https://hastie.su.domains/ElemStatLearn/datasets/nci.label.txt"))
rownames(nci.data) <- NULL

write.csv(nci.data, "../data/nci_data.csv", row.names = FALSE)
write.csv(nci.label, "../data/nci_label.csv", row.names = FALSE)
