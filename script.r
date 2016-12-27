# Copyright (c) Microsoft Corporation.  All rights reserved.

# Third Party Programs. This software enables you to obtain software applications from other sources. 
# Those applications are offered and distributed by third parties under their own license terms.
# Microsoft is not developing, distributing or licensing those applications to you, but instead, 
# as a convenience, enables you to use this software to obtain those applications directly from 
# the application providers.
# By using the software, you acknowledge and agree that you are obtaining the applications directly
# from the third party providers and under separate license terms, and that it is your responsibility to locate, 
# understand and comply with those license terms.
# Microsoft grants you no license rights for third-party software or applications that is obtained using this software.


##PBI_R_VISUAL: VIZGAL_CLUSTERING_WITH_OUTLIERS  Graphical display of a clustering applied to point cloud 
# Computes and visualizes a clustering performed with DBSCAN clustering algorithm. 
# Allows user to control granularity of clusters or to find it automatically. 
# Provides several options for scaling the data and for visualization of clusters. 
# INPUT: 
# The input dataset should include at least two numerical non-constant columns  
#
#
# WARNINGS:  Time consuming for large datasets
#
# CREATION DATE: 11/12/2016
#
# LAST UPDATE: 11/22/2016
#
# VERSION: 0.0.1
#
# R VERSION TESTED: 3.2.2
# 
# AUTHOR: pbicvsupport@microsoft.com
#
# REFERENCES: https://cran.r-project.org/package=dbscan
#             https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Clustering/Density-Based_Clustering

############ User Parameters #########

#DEBUG 
#save(list = ls(all.names = TRUE), file='C:/Users/boefraty/projects/PBI/R/tempData.Rda')
#load(file='C:/Users/boefraty/projects/PBI/R/tempData.Rda')

###############Library Declarations###############

libraryRequireInstall = function(packageName)
{
  if(!require(packageName, character.only = TRUE)) 
    warning( paste ("*** Failed to install '", packageName, "'  ***", sep = ""))
}


libraryRequireInstall("scales")
libraryRequireInstall("fpc")
libraryRequireInstall("car")
libraryRequireInstall("dbscan")

###############Internal parameters definitions#################
##PBI_PARAM: the random number generator (RNG) state for random number generation 
#Type: numeric, Default:42, Range:NA, PossibleValues:NA, Remarks: NA
randSeed = 42

##PBI_PARAM: transparency of points on plot, 0 is invisible, 1 is opaque
#Type: numeric, Default:0.25, Range:[0, 1], PossibleValues:NA, Remarks: NA
#pointOpacity = 0.5

##PBI_PARAM: minimum required samples (rows in data table)
#Type: positive integer, Default:10, Range:[5, 100], PossibleValues:NA, Remarks: NA
minSamplesToRun = 12 

##PBI_PARAM: maximum samples to use inside autoNumClusters function
#Type: positive integer, Default:1200, Range:[100, 10000], PossibleValues:NA, Remarks: NA
maxSamples4autoGranularity = 1200

##PBI_PARAM: insignificant principle component threshold
# If PCA is applied all dimensions, that explain less than insigPC percentage of variance are removed
#Type: positive numeric, Default:0.05, Range:[0, 1], PossibleValues:NA, Remarks: NA
insigPC = 0.05


##PBI_PARAM: type for outlier marker
#Type: integer, Default:4, Range:[1:20], PossibleValues:NA, Remarks: NA
outLierPch = 4  


##PBI_PARAM:  size for legend text
#Type: float, Default:1, Range:[0:5], PossibleValues:NA, Remarks: NA
legendTextSize = 1

##PBI_PARAM:  size for warning text
#Type: float, Default:0.8, Range:[0:2], PossibleValues:NA, Remarks: NA
warningCex = 0.8

###############Internal functions definitions#################

# if not enough space --> do not show the legend
validateIfToShowLegend = function(numClust, textSize)
{
  ppp = par()$din
  horFlag = (2.5*textSize < ppp[1])
  verFlag = (0.35*numClust*textSize < ppp[2])
  return(horFlag && verFlag) 
}

# if not enough space replace "long text" by "long te..."
cutStr2Show = function(strText, strCex = 0.8, abbrTo = 100, isH = TRUE, maxChar = 0, partAvailable = 1)
{
  # strText = text to modify 
  # strCex = font size 
  # abbrTo = very long string will be abbreviated to "abbrTo" characters
  # isH = "is horizontal" ?
  # maxChar = text smaller than maxChar is replaced by NULL
  # partAvailable = which portion of window is available for text, in [0,1]
  
  if(is.null(strText))
    return (NULL)
  
  SCL = 0.094*strCex
  pardin = par()$din
  gStand = partAvailable*(isH*pardin[1]+(1-isH)*pardin[2]) /SCL
  
  # if very very long abbreviate
  if(nchar(strText)>abbrTo && nchar(strText)> 1)
    strText = abbreviate(strText, abbrTo)
  
  # if looooooong convert to lo...
  if(nchar(strText)>round(gStand) && nchar(strText)> 1)
    strText = paste(substring(strText,1,floor(gStand)),"...",sep="")
  
  # if shorter than maxChar remove 
  if(gStand<=maxChar)
    strText = NULL
  
  return(strText) 
}




#function finds average distance of np1'th to np2'th neighbour, for example:
# if np1=np2=1, will find distance to the nearest neighbour
# in np1=np2=3, will find distance to the third nearest neighbour
# in np1=1, np2=3, will find average distance to the three nearest neighbours 
avgNearestDist <- function(data, np1 = 1, np2 = np1 )
{
  nn <- dbscan::kNN(data, k = np2)
  distances  =  nn$dist[, c(np1:np2)]
  if( np1<np2 )
  {
    res <- sort(apply (distances, 1, mean) )
  }else{
    res = sort(distances)
  }
  return(res)
}

#sum of square errors for linear fit 
SSE = function(x, y) {sum( abs( lm( formula = y ~ x, data = data.frame(x = x, y = y) )$residuals )^2)}

# find knee point which corresponds to best cut-point of two linear fits
findKnee <- function( inX, inY )
{
  orderX = order( inX )
  inX = inX[orderX];inY = inY[orderX]
  
  L = length(inX)
  resV = rep(Inf, L)
  first = 3
  last = L-3
  
  for (i in (first+2):(last-2))
  {
    x = inX[first:(i-1)]
    y = inY[first:(i-1)]
    resid = SSE(x, y)
    x = inX[(i+1):last]
    y = inY[(i+1):last]
    resV[i]=resid+SSE(x, y)
  }
  
  mi = which.min(resV)-1
  return( c(inX[mi], inY[mi]) )
}


#verify if the column is numeric and non-constant
correctColumn <- function(someColumn){ is.numeric(someColumn)&&length(unique(someColumn)) > 1 }

#euclidean distance between two points
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

#plot convex hull
plotCH = function(xcoord, ycoord, lcolor){
  hpts <- chull(x = xcoord, y = ycoord)
  hpts <- c(hpts, hpts[1])
  lines( xcoord[hpts], ycoord[hpts], col = lcolor, lty = 3 )
}  

# get datapoints closest to centers 
getDelegates <- function(orig_data, clean_data, cluster_centers)
{
  nc  <-  nrow(cluster_centers)
  dc <- ncol(cluster_centers)  
  nr <- nrow(clean_data)
  delegates <- NULL
  for(clus in seq(1, length.out = nc))
  {
    B  <-  matrix(rep(cluster_centers[clus, ], times = nr), nrow = nr, ncol = dc, byrow = TRUE)
    D  <-  clean_data[, c(1:dc)]-B
    ed <- apply(D^2, 1, sum)
    delegates <- rbind(delegates, orig_data[which.min(ed), ])
  }
  return(delegates) 
}

# heuristic method for minPts parameter in dbscan 
# larger number of points for high dimensionality 
autoParamMinPtsFunc <- function(mydata, defaultMinPts = 5, extraPointPerRowCount = 250)
{
  nc <- ncol( mydata )
  nr <- nrow( mydata )
  
  minPts <- max(defaultMinPts, (2*nc-1)) + floor( nr/extraPointPerRowCount )
  return(minPts)
}


#autoGranularity
autoEpsFunc <- function(mydata, maxPoints, reachMinPts, alpha = 0.6)
{
  if(nrow(mydata) > maxPoints)
    mydata <- mydata[sample(nrow(mydata), maxPoints), ]
  
  avgNNmp <- avgNearestDist(mydata, 1, reachMinPts) # to reachMinPts nearest 
  avgNN1 <- avgNearestDist(mydata, 1, 1) # to  nearest 
  avgNN =  alpha*avgNN1+(1-alpha)*avgNNmp
  eps = findKnee(1:length(avgNNmp), avgNNmp)[2]
  #eps = findKnee(1:length(avgNN1), avgNN1)[2]
  return(eps)
}

#granularity2eps
granularity2epsMinMax <- function(mydata, g, maxPoints, reachMinPts)
{
  #subsample if needed
  if(ncol(mydata) > maxPoints)
    mydata <- mydata[sample(ncol(mydata), maxPoints), ]
  
  avgNN <- avgNearestDist(mydata, reachMinPts, reachMinPts)
  
  resEps = as.numeric(quantile(avgNN, g))
  return(resEps)
  
}

#compute eps for dbscan (not in use)
getEps <- function(mydata, frac = 0.04){euc.dist(sapply(mydata, max), sapply(mydata, min))*frac}

#get centers from clusters
centersFromClusters <- function(mydata, clusters)
{
  NC <- max(clusters)
  centers <- NULL
  for (c in seq(1, length.out = NC))
  {
    centers = rbind(centers, apply(mydata[clusters == c, ], 2, mean))
  }
  return(centers)
}

myPallete = function(n=100,palleteType = "rainbow")
{
  mp = rainbow(n)
  
  if(palleteType == "heat")
    mp = heat.colors(n)
  
  if(palleteType == "terrain")
    mp = terrain.colors(n)
  
  if(palleteType == "topo")
    mp = topo.colors(n)
  
  if(palleteType == "cm")
    mp = cm.colors(n+1)[-1] #remove white
  
  if(palleteType == "gray")
    mp = gray(0:n/ n)
  
  return(mp)
  
}

# variables to come from group:show
addLabel2clusterDelegate = TRUE
addLabel2points = TRUE 
addLegend = TRUE


#################################################################################
###   The fuction to produce visual
#################################################################################

#********* PBI Parameters Block ***************
if(!exists("Values"))
  Values = NULL 

if(!exists("PointLabels"))
  PointLabels = NULL 

addLabel2points = TRUE  #default
if (exists("settings_labeling_params_show")) 
{
  addLabel2points = settings_labeling_params_show
}

addLabel2clusterDelegate = FALSE  #default
if (exists("settings_representative_params_show")) 
{
  addLabel2clusterDelegate = settings_representative_params_show
}

addLegend = TRUE  #default
if (exists("settings_legend_params_show")) 
{
  addLegend = settings_legend_params_show
}

if(exists("settings_prepocessing_params_show") && settings_prepocessing_params_show == FALSE)
  rm(list= ls(pattern = "settings_prepocessing_params_" ))

if(exists("settings_clusterNum_params_show") && settings_clusterNum_params_show == FALSE)
  rm(list= ls(pattern = "settings_clusterNum_params_" ))

if(exists("settings_viz_params_show") && settings_viz_params_show == FALSE)
  rm(list= ls(pattern = "settings_viz_params_" ))

if(exists("settings_labeling_params_show") && settings_labeling_params_show == FALSE)
  rm(list= ls(pattern = "settings_labeling_params_" ))

if(exists("settings_representative_params_show") && settings_representative_params_show == FALSE)
  rm(list= ls(pattern = "settings_representative_params_" ))

if(exists("settings_legend_params_show") && settings_legend_params_show == FALSE)
  rm(list= ls(pattern = "settings_legend_params_" ))

if(exists("settings_additional_params_show") && settings_additional_params_show == FALSE)
  rm(list= ls(pattern = "settings_additional_params_" ))

##PBI_PARAM: display_name: Scale data, tooltip:Used to standardize the range of features of data
# Type: bool, default:FALSE, 
# Min: , Max:
scaleData = FALSE  #default
if (exists("settings_prepocessing_params_scaleData")) 
{
  scaleData = settings_prepocessing_params_scaleData
}

##PBI_PARAM: display_name: Apply PCA, tooltip:Recommended for data with more than two dimensions
# Type: bool, default:FALSE, 
# Min: , Max:
applyPCA = FALSE  #default
if (exists("settings_prepocessing_params_applyPCA")) 
{
  applyPCA = settings_prepocessing_params_applyPCA
}

##PBI_PARAM: display_name: Granularity method, tooltip:Select preferable method to set granularity parameter(eps)
# Type: enumeration, default:'scale', 
# Min: , Max:
# enumeration options: auto ,scale ,manual ,
granularityParameterType = 'auto'  #default
if (exists("settings_clusterNum_params_granularityParameterType")) 
{
  granularityParameterType = settings_clusterNum_params_granularityParameterType
}

##PBI_PARAM: display_name: Granularity, tooltip:User-defined granularity level, used only if  autoGranularity = FALSE. Smaller values correspond to more clusters
# Type: numeric, default:50, 
# Min: 1, Max:100
granularityLevel = 50  #default
if (exists("settings_clusterNum_params_percentile")) 
{
  granularityLevel = settings_clusterNum_params_percentile
  granularityLevel = max( min (granularityLevel, 100), 1)
}

##PBI_PARAM: display_name: Reachability distance, tooltip:How close points should be to each other to be considered a part of a cluster (eps)
# Type: numeric, default:0, 
# Min: 0, Max:1e+08
eps = 0  #default
if (exists("settings_clusterNum_params_eps")) 
{
  eps = settings_clusterNum_params_eps
  eps = max( min (eps, 1e+08), 0)
}

##PBI_PARAM: display_name: Find minimum points automatically, tooltip:
# Type: bool, default:TRUE, 
# Min: , Max:
autoParamMinPts = TRUE  #default
if (exists("settings_clusterNum_params_autoParamMinPts")) 
{
  autoParamMinPts = settings_clusterNum_params_autoParamMinPts
}

##PBI_PARAM: display_name: Minimum points per cluster, tooltip:User-defined minimum points parameter. Smaller values correspond to more clusters
# Type: numeric, default:5, 
# Min: 1, Max:1000
minPtsParam = 5  #default
if (exists("settings_clusterNum_params_minPtsParam")) 
{
  minPtsParam = settings_clusterNum_params_minPtsParam
  minPtsParam = max( min (minPtsParam, 1000), 1)
}

##PBI_PARAM: display_name: Draw ellipse, tooltip:
# Type: bool, default:FALSE, 
# Min: , Max:
drawEllipse = FALSE  #default
if (exists("settings_viz_params_drawEllipse")) 
{
  drawEllipse = settings_viz_params_drawEllipse
}

##PBI_PARAM: display_name: Draw convex hull, tooltip:
# Type: bool, default:FALSE, 
# Min: , Max:
drawConvexHull = FALSE  #default
if (exists("settings_viz_params_drawConvexHull")) 
{
  drawConvexHull = settings_viz_params_drawConvexHull
}

##PBI_PARAM: display_name: Draw centroid, tooltip:
# Type: bool, default:FALSE, 
# Min: , Max:
drawCenters = FALSE  #default
if (exists("settings_viz_params_drawCenters")) 
{
  drawCenters = settings_viz_params_drawCenters
}

##PBI_PARAM: display_name: Point opacity, tooltip:
# Type: numeric, default:30, 
# Min: 0, Max:100
pointOpacity = 30  #default
if (exists("settings_viz_params_percentile")) 
{
  pointOpacity = settings_viz_params_percentile
  pointOpacity = max( min (pointOpacity, 100), 0)
}

##PBI_PARAM: display_name: Point size, tooltip:
# Type: numeric, default:10, 
# Min: 1, Max:50
pointSize = 10  #default
if (exists("settings_viz_params_weight")) 
{
  pointSize = settings_viz_params_weight
  pointSize = max( min (pointSize, 50), 1)
}

##PBI_PARAM: display_name: Font size, tooltip:
# Type: numeric, default:8, 
# Min: 8, Max:40
labelingFontSize = 8  #default
if (exists("settings_labeling_params_textSize")) 
{
  labelingFontSize = settings_labeling_params_textSize
  labelingFontSize = max( min (labelingFontSize, 40), 8)
}

##PBI_PARAM: display_name: Label opacity, tooltip:
# Type: numeric, default:100, 
# Min: 0, Max:100
labelOpacity = 100  #default
if (exists("settings_labeling_params_percentile")) 
{
  labelOpacity = settings_labeling_params_percentile
  labelOpacity = max( min (labelOpacity, 100), 0)
}

##PBI_PARAM: display_name: Limit label length, tooltip:Abbreviate labels to a limited length
# Type: numeric, default:5, 
# Min: 1, Max:100
maxLenPointLabel = 5  #default
if (exists("settings_labeling_params_maxLenPointLabel")) 
{
  maxLenPointLabel = settings_labeling_params_maxLenPointLabel
  maxLenPointLabel = max( min (maxLenPointLabel, 100), 1)
}

##PBI_PARAM: display_name: Percentage of points labeled, tooltip:Avoids cluttering text
# Type: numeric, default:100, 
# Min: 0, Max:100
percPointsLabeled = 100  #default
if (exists("settings_labeling_params_percentile1")) 
{
  percPointsLabeled = settings_labeling_params_percentile1
  percPointsLabeled = max( min (percPointsLabeled, 100), 0)
}

##PBI_PARAM: display_name: Font size, tooltip:
# Type: numeric, default:8, 
# Min: 1, Max:40
representativeFontSize = 8  #default
if (exists("settings_representative_params_textSize")) 
{
  representativeFontSize = settings_representative_params_textSize
  representativeFontSize = max( min (representativeFontSize, 40), 1)
}

##PBI_PARAM: display_name: Limit label length, tooltip:Abbreviate labels to a limited length
# Type: numeric, default:30, 
# Min: 1, Max:100
maxLenDelegate = 30  #default
if (exists("settings_representative_params_maxLenDelegate")) 
{
  maxLenDelegate = settings_representative_params_maxLenDelegate
  maxLenDelegate = max( min (maxLenDelegate, 100), 1)
}

##PBI_PARAM: display_name: Pallete type, tooltip:Color pallete type
# Type: enumeration, default:'rainbow', 
# Min: , Max:
# enumeration options: rainbow ,gray ,cm ,topo ,terrain ,heat ,
palleteType = 'rainbow'  #default
if (exists("settings_legend_params_palleteType")) 
{
  palleteType = settings_legend_params_palleteType
}

##PBI_PARAM: display_name: Color of outliers, tooltip:
# Type: fill, default:'black', 
# Min: , Max:
outLierCol = 'black'  #default
if (exists("settings_legend_params_outLierCol")) 
{
  outLierCol = settings_legend_params_outLierCol
}

##PBI_PARAM: display_name: Show warnings, tooltip:
# Type: bool, default:TRUE, 
# Min: , Max:
showWarnings = TRUE  #default
if (exists("settings_additional_params_showWarnings")) 
{
  showWarnings = settings_additional_params_showWarnings
}



{
  
  
  
  if(eps ==0) #not valid "eps" => "auto" mode
    eps = NULL
  
  #addLegend = TRUE
  delegateCex = representativeFontSize/10
  
  skipLabel2points = max(100/as.numeric(percPointsLabeled) , 1)
  labelOpacity = labelOpacity/100
  pointOpacity = pointOpacity/100
  cexLabel2points = labelingFontSize/10
  pointMarkerSize = pointSize/10  
  drawPoints = TRUE
  
  
  if(!is.null(Values))
    dataset = Values
  
  if(!is.null(PointLabels))
    PointLabels[,1] = as.character(PointLabels[,1])
  
  if(!is.null(PointLabels) && !is.null(Values))
    dataset = cbind(PointLabels, dataset)
  
  if(!is.null(PointLabels) && is.null(Values))
    dataset = PointLabels
  
  
  if(addLabel2points && is.null(PointLabels))
    addLabel2points = FALSE
  
  ###############Upfront input correctness validations (where possible)#################
  minPtsParam = round(minPtsParam)
  pbiWarning <- NULL
  
  dataset  <-  na.omit(dataset) # deletion of missing
  orig_dataset <- dataset #used later for delegates
  
  # verify correctness of dataset
  useColumns <- sapply(dataset, correctColumn)
  
  if(showWarnings && sum(useColumns[-1])<ncol(dataset)-1)
    pbiWarning <- "At least one of the columns was not numeric, or constant"
  
  #exclude defect columns
  dataset  <-  as.data.frame(dataset[, useColumns])
  nc <- ncol(dataset)
  nr <- nrow(dataset)
  
  checkDimiensionality <- TRUE
  if(nc < 2 || nr < minSamplesToRun || is.null(Values))
  {
    checkDimiensionality <- FALSE
    if(showWarnings)
      pbiWarning <- paste(pbiWarning, "\nNot enough input dimensions");
  }
  
  if(!autoParamMinPts && minPtsParam >= nr)
  {
    checkDimiensionality <- FALSE
    if(showWarnings)
      pbiWarning <- paste(pbiWarning, "\nParameter minPts is out of limits");
  }
  
  ##############Main Visualization script###########
  
  maxGLevel = 100  # for "scale" mode maxGLevel can be used to squeeze the range 
  set.seed(randSeed)
  
  if(!checkDimiensionality)
  {
    plot.new()
  }else
  {
    if(scaleData)
    {
      dataset <- as.data.frame(scale(dataset))
      names(dataset) = paste(names(dataset), "scaled", sep = ".")
    }
    if(applyPCA)
    {
      dataset.pca  <-  prcomp(dataset, center =  TRUE, scale =  F) 
      pExplained <- dataset.pca$sdev^2/sum(dataset.pca$sdev^2)
      flags <- (pExplained > insigPC); flags[1:2] = TRUE #at least 2 dimensions
      dataset = as.data.frame(dataset.pca$x[, flags])#reduce dimensions with less than 5% variance
    }
    
    if(autoParamMinPts)
      minPtsParam = autoParamMinPtsFunc(dataset, extraPointPerRowCount = 175)
    
    
    #find eps
    if(granularityParameterType == "auto")
      eps = autoEpsFunc(dataset, maxSamples4autoGranularity, minPtsParam)  
    
    
    if(granularityParameterType == "scale")
      eps <- granularity2epsMinMax(dataset, granularityLevel/maxGLevel, maxSamples4autoGranularity, minPtsParam)
    
    if(is.null(eps)) 
      eps = autoEpsFunc(dataset, maxSamples4autoGranularity, minPtsParam)  
    
    
    #DBSCAN call 
    cl <- dbscan::dbscan(dataset, eps, minPts = minPtsParam, scale = FALSE, borderPoints = TRUE)
    
    numOfClusters = max(cl$cluster)
    
    
    cl$centers <- centersFromClusters(dataset, cl$cluster) 
    
    drawColors <- c(outLierCol, myPallete(numOfClusters,palleteType = palleteType))
    drawColorsLeg <- c(rainbow(numOfClusters), outLierCol)
    drawPch <- c(outLierPch, rep(19, numOfClusters))
    
    
    
    #visualize 2 first coordinates 
    if(drawPoints)  colpoints = drawColors[cl$cluster+1] else colpoints = NULL 
    
    pchPoints = drawPch[cl$cluster+1]
    
    #in case of legend extend xlim to the right by 20%
    xrange = range(dataset[, 1])
    drange = xrange[2]-xrange[1]
    xlim = c(xrange[1]-0.01*drange, xrange[2]+0.01*drange+drange*0.20*addLegend)
    
    plot(dataset[, 1], dataset[, 2], col = alpha(colpoints, pointOpacity), pch = pchPoints, 
         xlab = cutStr2Show(names(dataset)[1], strCex =1.1, isH = TRUE), 
         ylab = cutStr2Show(names(dataset)[2], strCex =1.1, isH = FALSE), 
         xlim = xlim, cex = pointMarkerSize)
    
    leg <- NULL
    if(!is.null(cl$centers))
      leg <- paste("Cluster ", seq(1, length.out  = numOfClusters)) #text
    
    pc <- c(rep(19, numOfClusters)) # markers 
    colMarkers <- drawColors[-1]
    
    if(drawCenters && !is.null(cl$centers))
    {
      points(cl$centers, pch = 7, col = drawColors[-1])
      leg <- cbind(leg, paste("Cluster center " , seq(1, length.out  = numOfClusters)))
      pc <- cbind(pc, rep(7, numOfClusters))
      colMarkers <- rep(colMarkers, 2)
    }
    
    leg <- c(leg, "Outlers")
    pc = c(pc, outLierPch)
    colMarkers <- c(colMarkers, outLierCol)
    
    if(drawEllipse)
    {
      for(clus in seq(1, length.out = numOfClusters))
      {
        iii <- (cl$cluster == clus)
        if(sum(iii) > 2)
          dataEllipse(dataset[iii, 1], dataset[iii, 2], add = T, plot.points = F, levels = 0.85, col = drawColorsLeg[clus], lwd = 1, 
                      fill = TRUE, fill.alpha = 0.075, center.pch = NULL)
      }
    }
    
    if(drawConvexHull)
    {
      for(clus in seq(1, length.out = numOfClusters))
      {
        iii <- (cl$cluster == clus)
        if(sum(iii) > 2)
          plotCH(dataset[iii, 1], dataset[iii, 2], lcolor = drawColorsLeg[clus])
      }
    }
    
    if(addLabel2clusterDelegate)
    {
      clean_data = dataset
      cluster_centers = (cl$centers)
      if(!is.null(cluster_centers))
      {
        deleg <- getDelegates(orig_dataset, dataset, cl$centers)
        
        delegateText = abbreviate(apply(deleg, 1, toString),maxLenDelegate)
        delegateText = sapply(delegateText, cutStr2Show, strCex = delegateCex, partAvailable = 0.75)
        
        
        text(x = cl$centers[, 1], y = cl$centers[, 2], 
             delegateText, 
             col = "black", 
             cex = delegateCex)
      }
    }
    
    if(addLabel2points)
    {
      iii=sample(1:nrow(dataset),max(1,floor(nrow(dataset)/skipLabel2points)))
      text(x = dataset[iii, 1], y = dataset[iii, 2], labels = abbreviate(orig_dataset[iii,1],maxLenPointLabel),
           col = alpha(colpoints[iii], labelOpacity), cex = cexLabel2points)
    }
    
    if(addLegend && validateIfToShowLegend(numClust = numOfClusters, textSize = legendTextSize ))
      legend("topright", legend = leg, pch = pc, col = alpha(colMarkers, 1), cex = legendTextSize)
    
  }
  if(showWarnings && !is.null(pbiWarning))
  {
    pbiWarning = cutStr2Show(pbiWarning, strCex = warningCex)
    title(main = NULL, sub = pbiWarning, outer = FALSE, col.sub = "gray50", cex.sub = warningCex)
  }
  
}
