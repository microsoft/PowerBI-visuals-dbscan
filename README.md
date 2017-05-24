# PowerBI-visuals-dbscan
A PowerBI visualization for density-based spatial clustering of applications with noise.

![DBScan screenshot](https://az158878.vo.msecnd.net/marketing/Partner_21474836617/Product_42949680842/Asset_f542dedb-c6c7-4b3c-b7bb-066ffea7184d/ClusteringWithOutliersscreensh.png)
# Overview
Everyone is trying to make sense of their data. In the real world, data is often not easy to separate, and patterns are not usually obvious. Clustering helps you find similarity groups in your data and it is one of the most common tasks in the Data Science. Finding the "outliers", which are the observations in your data isolated from the rest of observations, is often a non-easy analytics task by its own. It explains why the density-based clustering, which find similarity groups and outliers in your data simultaniously, is one of the most common clustering algorithms.

You can control the algorithm parameters and the visual attributes to suit your needs.

Here is how it works:
* Define the fields to be used in clustering (two or more numerical variables)
* Optionally, provide the labels to be shown on top of each observation
* If the dimensionality of data is higher than two, consider data preprocessing
* DBSCAN algorithm requires 2 parameters to control the granularity of clusters. They can be set manually by user (recommended) or automatically by underlying algorithm
* When you are sattisfied with clustering output, use numerous formatting controls to refine the visual apperance of the plot

R package dependencies(auto-installed): scales, fpc, car, dbscan

Supports R versions: R 3.3.1, R 3.3.0, MRO 3.3.1, MRO 3.3.0, MRO 3.2.2

See also [Clustering With Outliers at Microsoft Office store](https://store.office.com/en-us/app.aspx?assetid=WA104380889&sourcecorrid=9c305128-56bd-428c-a81d-a8068f454e5d&searchapppos=0&ui=en-US&rs=en-US&ad=US&appredirect=false)