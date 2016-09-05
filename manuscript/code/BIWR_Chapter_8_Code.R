#####################################################################
# Business Intelligence with R
# Dwight Barry
# Chapter 8 Code: Patterns and Outliers
#
# https://github.com/Rmadillo/business_intelligence_with_r/blob/master/manuscript/code/BIWR_Chapter_8_Code.R
#
######################################################################


## Mapping multivariate relationships

### Non-metric multidimensional scaling (nMDS)

require(vegan)

banks = read.csv("https://raw.githubusercontent.com/Rmadillo/business_intelligence_with_r/master/manuscript/code/quiebra.csv", encoding = "UTF-8", stringsAsFactors = FALSE, header = TRUE)

banks_mds = metaMDS(banks[,3:11], distance="euclidean", autotransform=FALSE, noshare=FALSE, wascores=FALSE)

# You may need to mess with par for the margins. Note: Clearing the 
# plots in RStudio (broom icon) returns par to defaults
# par(mar=c(1.5,0,1,0))

# Save the stress value for the legend
banks.stress = round(banks_mds$stress, digits=2)

# Set up color scheme
groups = ifelse(banks$Solvente == 0, "darkred", "blue")

# Create an empty plot
ordiplot(banks_mds, type="n", xaxt="n", yaxt="n", xlab=" ", ylab=" ", display="sites")

# Add banks by number and color (solvency)
orditorp(banks_mds, display="sites", col=groups, air=0.01, cex=0.75)

# Add legend
legend("topleft", legend=c("Bankrupt", "Solvent"), bty="n", col=c("darkred","blue"), title=paste0("Stress: ", banks.stress), pch=21, pt.bg=c("darkred", "blue"), cex=0.75)

# Zoom in 
ordiplot(banks_mds, type="n", xlab=" ", ylab=" ", display="sites", xlim=c(0,0.07), ylim=c(-0.1, 0.095))

orditorp(banks_mds, display="sites", col=groups, air=0.01, cex=0.75)

legend("topleft", legend=c("Bankrupt", "Solvent"), bty="n", col=c("darkred", "blue"), title=paste0("Stress: ", banks.stress), pch=21, pt.bg=c("darkred", "blue"), cex=0.75)

### Euromap nMDS example
  
# par(mar=c(1.5,4,1,4))

euromap = metaMDS(eurodist, distance="euclidean", autotransform=FALSE, noshare=FALSE, wascores=FALSE)

euromap$points[,1] = -euromap$points[,1]

ordiplot(euromap, display="sites", xaxt="n", yaxt="n", xlab=" ", ylab=" ", type="t")

### Diagnostics for nMDS results

banks_stress = round(banks_mds$stress, digits=2)

banks_stress

stressplot(banks_mds, xaxt="n", yaxt="n")

### Vector mapping influential variables over the nMDS plot

banks_vectors = envfit(banks_mds, banks[,3:11])

ordiplot(banks_mds, type="n", xaxt="n", yaxt="n", xlab=" ", ylab=" ", display="sites")

orditorp(banks_mds, display="sites", col=groups, air=0.01, cex=0.75)

plot(banks_vectors, col="gray40", cex=0.75)

legend("topleft", legend=c("Bankrupt", "Solvent"), bty="n", col=c("darkred","blue"), title=paste0("Stress: ", banks.stress), pch=21, pt.bg=c("darkred", "blue"), cex=0.75)

### Contour mapping influential variables over the nMDS plot

# par(mar=c(1.5,0,1,0))

# Empty plot
ordiplot(banks_mds, type="n", xaxt="n", yaxt="n", xlab=" ", ylab=" ", display="sites")
  
# Add contour of variable R8 (Cost of Sales:Sales ratio)
# Putting it in tmp suppresses console output
tmp = ordisurf(banks_mds, banks$R8, add=TRUE, col="gray30")

# Add contour of variable R4 (Reserves:Loans ratio)
tmp = ordisurf(banks_mds, banks$R4, add=TRUE, col="gray70")

# Plot nMDS solution
orditorp(banks_mds, display="sites", col=groups, air=0.01, cex=0.75)

legend("topleft", legend=c("Bankrupt", "Solvent"), bty="n", col=c("darkred","blue"), title=paste0("Stress: ", banks.stress), pch=21, pt.bg=c("darkred", "blue"), cex=0.75)


### Principal Components Analysis (PCA)

banks_pca = princomp(banks[,3:11], cor=TRUE)

summary(banks_pca, loadings=TRUE)

# devtools::install_github("vqv/ggbiplot")

require(ggbiplot)

ggbiplot(banks_pca, labels=rownames(banks), ellipse=T, 
  groups= as.factor(banks$Solvente)) + 
  theme_bw()

# Get variances
vars = banks_pca$sdev^2

# Get proportion of variance
var_exp = vars/sum(vars)

# Get cumulative variance and make into data frame
variances = data.frame(vars = vars, var_exp = var_exp, 
  var_cumsum = cumsum(var_exp), PCs = 1:length(banks_pca$sdev))

# Plot variance explained and cumulative variance
ggplot(variances, aes(PCs, var_cumsum)) +
  scale_x_discrete(limits=c(1:9)) +
  ylab("Var. Explained (bars) and Cumulative (line) by PC") +
  geom_bar(aes(y=var_exp), stat="identity", fill="gray", alpha=.5) +
  geom_line() +
  theme_bw()


### nMDS for Categories: Correspondence Analysis

require(ca)

smoke_ca = ca(smoke)

summary(smoke_ca)

plot(smoke_ca, arrows=c("F","T"), mass = c(TRUE, TRUE))


## Grouping observations with hierarchical clustering

banks_dist = dist(banks[,3:11])

banks_hclust = hclust(banks_dist, method="ward.D")

plot(banks_hclust, hang=-1, labels=paste(banks$BANCO, row.names(banks), sep="-"))


### Plotting a cluster dendrogram with ggplot

require(NeatMap)

ggplot() +
  draw.dendrogram(banks_hclust) +
  scale_color_manual(name="Status: ", values=c("darkred", "blue"),
    labels=c("Bankrupt ", "Solvent ")) +
  geom_text(aes(x=0.75, y=banks_hclust$order, label=row.names(banks), 
    color=factor(banks$Output)), size=4) +
  ggtitle("Cluster Dendrogram (Ward's) - Spanish Banking Crisis") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank(),
    axis.title = element_blank(), axis.text = element_blank(),
    axis.ticks = element_blank(), legend.position="top")

banks_scaled = scale(banks[,3:11])

banks_dist_sc = dist(banks_scaled)

banks_hclust_sc = hclust(banks_dist, method="ward.D")

# Now rerun the ggplot commands as above
ggplot() +
  draw.dendrogram(banks_hclust_sc) +
  scale_color_manual(name="Status: ", values=c("darkred", "blue"),
    labels=c("Bankrupt ", "Solvent ")) +
  geom_text(aes(x=0.75, y=banks_hclust$order, label=row.names(banks), 
    color=factor(banks$Output)), size=4) +
  ggtitle("Cluster Dendrogram (Ward's) - Spanish Banking Crisis") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank(),
    axis.title = element_blank(), axis.text = element_blank(),
    axis.ticks = element_blank(), legend.position="top")


### Exploring hierarchical clustering of nMDS results

require(NeatMap)

# nMDS results from above
banks_pos = banks_mds$points 

draw.dendrogram3d(banks_hclust, banks_pos, labels=row.names(banks), label.colors=banks$Solvente+3, label.size=1)


## How to partition the results of a hierarchical cluster analysis

# Run cutree for 2 groups
bank_clusters = cutree(banks_hclust, k=2)

# View results
table(bank_clusters)

# Show variable medians for each group
aggregate(banks[,3:11], by=list(cluster=bank_clusters), median)

# Plot dendrogram
plot(banks_hclust, hang=-1, labels=paste(banks$BANCO, row.names(banks), sep="-"), cex=0.9)

# Add cluster boxes to dendrogram
rect.hclust(banks_hclust, k=2)


## Identifying and describing group membership with kMeans and PAM

require(cluster)

banks_pam = pam(banks[,3:11], k=2)

banks_pam

banks_kmeans = kmeans(banks[,3:11], centers=2, nstart=50)

banks_kmeans

banks$pam2 = banks_pam$clustering

banks$km2 = banks_kmeans$cluster

table(banks$Output, banks$pam2)

table(banks$Output, banks$km2)

plot(banks_pam, which.plot=1, xlab="", ylab="")

require(useful)

plot(banks_kmeans, data=banks, class="Output", xlab="", ylab="")


## How to choose an optimal number of clusters with bootstrapping

banks_gap = clusGap(banks[,3:11], FUNcluster=pam, K.max=10, B=500)

banks_gap_df = as.data.frame(banks_gap$Tab)

ggplot(banks_gap_df, aes(x=1:nrow(banks_gap_df))) +
  scale_x_discrete(limits=c(1:10)) +
  xlab("Clusters") +
  geom_point(aes(y=gap), size=3) +
  geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim))


## Determining optimal numbers of clusters with model-based clustering

require(mclust)
data("diabetes")

diabetes_mclust = Mclust(diabetes[,2:4])

summary(diabetes_mclust)

diabetes_mclust$BIC

# Model comparison plot
plot(diabetes_mclust, what="BIC")

# Cluster results 
plot(diabetes_mclust, what="classification")

# Bivariate density (basic contour plot)
plot(diabetes_mclust, what="density")

# Bivariate density (perspective plot)
plot(diabetes_mclust, what = "density", type = "persp", border = adjustcolor(grey(0.01), alpha.f = 0.15))

# Cluster uncertainty plot
plot(diabetes_mclust, what="uncertainty")

# Add classification and uncertainty to data set
diabetes$mclust_cluster = diabetes_mclust$classification

diabetes$mclust_uncertainty = diabetes_mclust$uncertainty

# Plot the uncertainty as a histogram and table
require(googleVis)

diabetes_histogram = gvisHistogram(data.frame(diabetes$mclust_uncertainty), options=list(width=800, height=300, legend="none", title="Distribution of Mclust Uncertainty Values"))

diabetes_table = gvisTable(diabetes, options=list(width=800, height=300))

HT = gvisMerge(diabetes_histogram, diabetes_table) 

plot(HT)


## Identifying group membership with irregular clusters

data("multishapes", package="factoextra")

ggplot(multishapes, aes(x, y)) +
  geom_point(aes(shape=as.factor(shape), 
    color=as.factor(shape))) +
  scale_shape_discrete(name="Known\nGrouping", labels = 
    levels(as.factor(multishapes$shape))) +
  scale_color_discrete(name="Known\nGrouping", labels = 
    levels(as.factor(multishapes$shape))) +
  theme_bw()

require(dbscan)

# Estimate eps with kNN distance plot
kNNdistplot(multishapes[,1:2], k=3)

# Try an eps value of 0.15
abline(h=0.15, col="blue", lty=3)

multishapes_dbscan = dbscan(multishapes[,1:2], eps = 0.15, minPts = 3)

multishapes_dbscan

multishapes$dbscan = multishapes_dbscan$cluster

table(multishapes$shape, multishapes$dbscan)

ggplot(multishapes, aes(x, y)) +
  geom_point(aes(color=as.factor(dbscan), shape=as.factor(shape))) +
  labs(color="DBSCAN\nGrouping", shape="Known\nGrouping") +
  theme_bw()


## Variable selection in cluster analysis

require(clustvarsel)

# Download the Wisconsin Breast Cancer data
breast_cancer = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", sep=",", na.strings = c("NA", "?"), header=F, col.names=c("Sample", "Clump_Thickness", "Uniformity_Cell_Size", "Uniformity_Cell_Shape", "Marginal_Adhesion", "Single_Epithelial_Cell_Size", "Bare_Nuclei", "Bland_Chromatin", "Normal_Nucleoli", "Mitoses", "Class"))

# Label the outcomes
breast_cancer$Diagnosis = ifelse(breast_cancer$Class==2, "benign", ifelse(breast_cancer$Class==4, "malignant", "unk"))

# Remove incomplete rows, then create data-only object
bc_wisc = na.omit(breast_cancer[,c(2:10, 12)])

bc_wisc_data = bc_wisc[,1:8]

# Perform variable selection (with parallel processing)
varsel_fwd = clustvarsel(bc_wisc_data, parallel = TRUE)

varsel_fwd

# Create data frame of the date with only the chosen variables
bc_fwd = bc_wisc_data[,varsel_fwd$subset]

# Run Mclust on subsetted data frame
bc_mclust_fwd = Mclust(bc_fwd)

# Add clustering result to data
bc_wisc$mclust_fwd_cluster = bc_mclust_fwd$classification

# Review Mclust results
summary(bc_mclust_fwd)


## Error checking cluster results with known outcomes 

classError(bc_wisc$Diagnosis, bc_wisc$mclust_fwd_cluster)

adjustedRandIndex(bc_wisc$Diagnosis, bc_wisc$mclust_fwd_cluster)

# Run Mclust on the full variable set of the Wisconsin Breast Cancer data
bc_mclust = Mclust(bc_wisc_data)

bc_wisc$mclust_all = bc_mclust$classification

classError(bc_wisc$Diagnosis, bc_wisc$mclust_all)

adjustedRandIndex(bc_wisc$Diagnosis, bc_wisc$mclust_all)


## Exploring outliers

### Identifying outliers with distance functions

require(mvoutlier)

quiebra_outliers = uni.plot(banks[,3:11], symb = T)

# Assign outlier grouping to main data
banks$mv_outlier = quiebra_outliers$outliers

# Add Mahalanobis distance results to main data
banks$mv_md = quiebra_outliers$md

# Add Euclidean distance results to main data
banks$mv_euclidean = quiebra_outliers$euclidean

require(ggplot2)
require(plotly)
  
p1 = ggplot(banks, aes(mv_md, text = paste("Bank: ",
    row.names(banks), "<br>Solvency: ", Output))) +
  xlab("Mahalanobis Distance") + 
  geom_histogram() + 
  facet_wrap(~mv_outlier, ncol=1, scales="free_y") 

ggplotly(p1)

uni.plot(bike_share_daily[,10:13], pch=as.numeric(bike_share_daily$weathersit))


### Identifying outliers with the local outlier factor

require(DMwR)

# Calculate the local outlier factor values
banks$lof = lofactor(banks[,3:11], k=4)

# Plot the lof results interactively
p_lof = ggplot(banks, aes(lof, text = paste("Bank: ",
    row.names(banks), "<br>Solvency: ", Output))) +
  xlab("Local Outlier Factor") + 
  geom_histogram() 

ggplotly(p_lof)


## Anomaly detection

# devtools::install_github("twitter/AnomalyDetection")

require(AnomalyDetection)
data(raw_data)

# Run the algorithm, include expected values and plotting features
raw_data_anomalies = AnomalyDetectionTs(raw_data, direction='both', 
    e_value=TRUE, plot=TRUE)

# Plot the time series and the anomalies
raw_data_anomalies$plot

# Extract data and expected values
raw_data_anomalies_df = raw_data_anomalies$anoms 


## Extreme value analysis

require(dplyr) 

data("damage", package="extRemes")

hurricane_cost = damage %>% group_by(Year) %>% summarise(total_damage = sum(Dam))

yearz = data.frame(Year = seq(1926, 1995, 1))

hurricane_cost = full_join(yearz, hurricane_cost)

hurricane_cost$total_damage[is.na(hurricane_cost$total_damage)] = 0

require(extremeStat)

hurr_fit = distLfit(hurricane_cost$total_damage)

distLplot(hurr_fit, cdf=TRUE, main="Total Annual Hurricane Damages (in billion USD)",
xlab="Damages ($1B USD)")

distLprint(hurr_fit)
 
hurr_fit$gof

# Calculate quantiles for different models
hurr_quant = distLquantile(hurricane_cost$total_damage, 
    probs=c(0.80, 0.90, 0.95, 0.99), returnlist=TRUE)

# Look at the top five models only
hurr_quant$quant[1:5,]

# Plot the cdf of top five models with their quantiles
distLplot(hurr_quant, cdf=T, main="Total Annual Hurricane Damages (in billion USD)", 
    xlab="Damages ($1B USD)", qlines=TRUE)

# Calcluate return intervals. 
hurr_returns = distLextreme(hurricane_cost$total_damage, RPs = c(2,10,50,100))

# View top five model results
hurr_returns$returnlev[1:5,]

# Plot return intervals
distLextremePlot(hurr_returns, main="Total Annual Hurricane Damages (in billion USD)",
ylab="Damages ($1B USD)")


## Finding associations in shopping carts

require(arules)
require(arulesViz)

# Download data and convert to a transactions object
basket_url = "http://dmg.org/pmml/pmml_examples/baskets1ntrans.csv"

basket_trans = read.transactions(basket_url, format = "single", sep = ",", 
  cols = c("cardid", "Product"), rm.duplicates=TRUE)

summary(basket_trans)

itemFrequencyPlot(basket_trans, topN=11, type="absolute") 

basket_rules_custom = apriori(basket_trans, parameter = list(support = 0.01, 
  confidence = 0.01, target="rules"))

inspect(head(sort(basket_rules_custom, by="lift"), 10))

plot(basket_rules_custom, interactive = T)

beer_rules = apriori(data=basket_trans, parameter=list(supp=0.01, 
  conf = 0.01, minlen=2), appearance = list(default="rhs", lhs="beer"))

inspect(head(sort(beer_rules, by="lift"), 10))

plot(beer_rules, method="graph", interactive=T)

write(basket_rules_custom, file = "basket_rules_custom.csv", 
  sep = ",", row.names=FALSE)


##### End of File #####
