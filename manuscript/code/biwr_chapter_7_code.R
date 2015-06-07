# Chapter 7: Pattern Discovery and Dimension Reduction: Proximity, Clusters, and Outliers
 
## Mapping multivariate relationships with non-metric multidimensional scaling (nMDS) and PCA

require(vegan)
banks = read.table("Data/quiebra.csv", header=T, sep=",")

banks_mds = metaMDS(banks[,3:11], distance="euclidean", autotransform=FALSE, 
    noshare=FALSE, wascores=FALSE)

par(mar=c(1.5,0,1,0))
banks.stress = round(banks_mds$textdollar$stress, digits=2)
# Set up color scheme
groups = ifelse(banks$\textdollar$Solvente == 0, "darkred", "blue")
# Create an empty plot
ordiplot(banks_mds, type="n", xaxt="n", yaxt="n", xlab=" ", ylab=" ", 
    display="sites")
# Add banks by number and color (solvency)
orditorp(banks_mds, display="sites", col=groups, air=0.01, cex=0.75)
# Add legend
legend("topleft", legend=c("Bankrupt", "Solvent"), bty="n", 
    col=c("darkred","blue"), title=paste("Stress: ",banks.stress), pch=21, 
    pt.bg=c("darkred", "blue"), cex=0.75)

ordiplot(banks_mds, type="n", xlab=" ", ylab=" ", display="sites", 
    xlim=c(0,0.07), ylim=c(-0.1, 0.095))
orditorp(banks_mds, display="sites", col=groups, air=0.01, cex=0.75)
legend("topleft", legend=c("Bankrupt", "Solvent"), bty="n", col=c("darkred",
    "blue"), title=paste("Stress: ",banks.stress), pch=21, pt.bg=c("darkred", 
    "blue"), cex=0.75)

par(mar=c(1.5,4,1,4))
euromap = metaMDS(eurodist, distance="euclidean", autotransform=FALSE, 
    noshare=FALSE, wascores=FALSE)
euromap$\textdollar$points[,1] = -euromap$\textdollar$points[,1]
ordiplot(euromap, display="sites", xaxt="n", yaxt="n", xlab=" ", ylab=" ", 
    type="t")

banks.stress = round(banks_mds$\textdollar$stress, digits=2)
banks.stress

stressplot(banks_mds, xaxt="n", yaxt="n")


### Vector mapping influential variables over the nMDS plot

banks.vectors = envfit(banks_mds, banks[,3:11])
ordiplot(banks_mds, type="n", xaxt="n", yaxt="n", xlab=" ", ylab=" ", 
    display="sites")
orditorp(banks_mds, display="sites", col=groups, air=0.01, cex=0.75)
plot(banks.vectors, col="gray40", cex=0.75)
legend("topleft", legend=c("Bankrupt", "Solvent"), bty="n", 
    col=c("darkred","blue"), title=paste("Stress: ",banks.stress), pch=21, 
    pt.bg=c("darkred", "blue"), cex=0.75)


### Contour mapping influential variables over the nMDS plot

par(mar=c(1.5,0,1,0))
# Empty plot
ordiplot(banks_mds, type="n", xaxt="n", yaxt="n", xlab=" ", ylab=" ", 
    display="sites")
# Add contour of variable R8 (Cost of Sales:Sales ratio)
# Putting it in tmp suppresses console output
tmp = ordisurf(banks_mds, banks$\textdollar$R8, add=TRUE, col="gray30")
# Add contour of variable R4 (Reserves:Loans ratio)
tmp = ordisurf(banks_mds, banks$\textdollar$R4, add=TRUE, col="gray70")
# Plot nMDS solution
orditorp(banks_mds, display="sites", col=groups, air=0.01, cex=0.75)
legend("topleft", legend=c("Bankrupt", "Solvent"), bty="n", 
    col=c("darkred","blue"), title=paste("Stress: ",banks.stress), pch=21, 
    pt.bg=c("darkred", "blue"), cex=0.75)


## Principal Components Analysis (PCA)

banks_pca = princomp(banks[,3:11], cor=TRUE)
summary(banks_pca, loadings=TRUE)

biplot(banks_pca, col=c("black", "gray40"))

# require(devtools)
# install_github("vqv/ggbiplot")
require(ggbiplot)
ggbiplot(banks_pca, labels =  rownames(banks), ellipse=T, groups=
    as.factor(banks$\textdollar$Solvente)) + 
    theme_bw()

variances = data.frame(vars=banks_pca$sdev^2, var_exp = 
    vars/sum(vars),_cumsum = cumsum(vars), PCs=1:length(banks_pca$sdev))

ggplot(variances, aes(PCs, var_cumsum)) +
    scale_x_discrete(limits=c(1:9)) +
    ylab("Var. Explained (bars) and Cumulative (line) by PC") +
    geom_bar(aes(y=var_exp), stat="identity", fill="gray", alpha=.5) +
    geom_line() +
    theme_bw()


## nMDS for Categories: Correspondence Analysis

require(ca)
smoke_ca = ca(smoke)
summary(smoke_ca)

plot(smoke_ca, arrows=c("F","T"), mass = c(TRUE, TRUE))

 
## Grouping observations with hierarchical clustering

banks = read.csv("Data/quiebra.csv", header=T)

banks_dist = dist(banks[,3:11])
banks_hclust = hclust(banks_dist, method="ward.D")
plot(banks_hclust, hang=-1, labels=paste(banks$Banco, banks$Numero, sep="-"))


### Plotting a cluster dendrogram with ggplot

require(NeatMap)
require(ggplot2)

ggplot() +
    draw.dendrogram(banks_hclust) +
    scale_color_manual(name="Status: ",values=c("darkred", "blue"),
                        labels=c("Bankrupt ", "Solvent ")) +
    geom_text(aes(x=0.75, y=banks_hclust$\textdollar$order, 
      label=banks$\textdollar$Numero, color=factor(banks$\textdollar$Solvente)),
      size=4) +
    ggtitle("Cluster Dendrogram (Ward's) - Spanish Banking Crisis") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank(),
          axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), legend.position="top")

banks_scaled = scale(banks[,3:11])
banks_dist = dist(banks_scaled)
banks_hclust = hclust(banks_dist, method="ward.D")
# Now rerun the ggplot commands as above


### Exploring hierarchical clustering of nMDS results

require(NeatMap)
banks_pos = banks_mds$points # nMDS results from previous recipe
draw.dendrogram3d(banks_hclust, banks_pos, labels=banks$\textdollar$Numero, 
  label.colors=banks$\textdollar$Solvente+3, label.size=1)


## Identifying and describing group membership with kMeans and PAM

require(cluster)
banks = read.csv("Data/quiebra.csv", header=T)

banks_pam = pam(banks[,3:11], k=2)
banks_pam

banks_kmeans = kmeans(banks[,3:11], centers=2, nstart=50)
banks_kmeans

banks$pam2 = banks_pam$clustering
banks$km2 = banks_kmeans$cluster

require(NeatMap)
plot(banks_pam, which.plot=1, xlab="", ylab="")

require(useful)
plot(banks_kmeans, data=banks, class="Solvente", xlab="", ylab="")


## How to choose an optimal number of clusters with bootstrapping

require(cluster)
banks_gap = clusGap(banks[,3:11], FUNcluster=pam, K.max=10, B=500)
banks_gap_df = as.data.frame(banks_gap$Tab)

ggplot(banks_gap_df, aes(x=1:nrow(banks_gap_df))) +
    scale_x_discrete(limits=c(1:10)) +
    xlab("Clusters") +
    geom_point(aes(y=gap), size=3) +
    geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim))


## How to partition the results of a hierarchical cluster analysis

bank_clusters = cutree(banks_hclust, k=2)
table(bank_clusters)
aggregate(banks[,3:11], by=list(cluster=bank_clusters), median)

plot(banks_hclust, hang=-1, labels=paste(banks$\textdollar$Banco, 
    banks$\textdollar$Numero, sep="-"), cex=0.9)
rect.hclust(banks_hclust, k=2)


## Determining optimal numbers of groups with model-based clustering

require(mclust)
breast_cancer = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", 
    sep=",", header=F, col.names=c("Sample", "Clump_Thickness", 
    "Uniformity_Cell_Size", "Uniformity_Cell_Shape", "Marginal_Adhesion", 
    "Single_Epithelial_Cell_Size", "Bare_Nuclei", "Bland_Chromatin", 
    "Normal_Nucleoli", "Mitoses", "Class"))
breast_cancer$Diagnosis = ifelse(breast_cancer$Class==2, "benign", 
    ifelse(breast_cancer$Class==4, "malignant", "unk"))
bc_wisc =  breast_cancer[,2:10]

bc_wisc_mc = Mclust(bc_wisc)
summary(bc_wisc_mc)


## Identifying group membership in non-linear clusters with DBSCAN



## Exploring outliers



## Finding associations in shopping carts



## Finding associations with collaborative filtering


## Non-negative matrix factorization for topic detection and clustering - DELETE?



