# reading file

AIcrawls <- read.csv(file = "crawlReport-all.csv", header = TRUE, sep = ",")

# converting dates to vector

FinishedOn <- strptime(AIcrawls$Completed, "%m/%d/%Y %H:%M")
hist(FinishedOn, "months", format = "%d %b") # a histogram based on weights

# add to dataframe
AIcrawls$FinishedOn <- FinishedOn

# create new dataframe with finished and non-test crawls
# status = Finished will not inclued all crawls that timeed out or reached document limits
finishedCrawls <- AIcrawls[which(Status=="Finished" & Test.Crawl.=="No"),]
table(finishedCrawls$Collection)

# table of collections with crawls finished in each month
table(Collection, months(FinishedOn))

# create vector for shorter collection names, using levels from finishedCrawls
levels(Collection)
CollectionNames <- c("Alberta Education", "Alberta Oil Sands", "Can. Business Grey Lit.", "Circumpolar", "Energy and Environment", "Government Information", "Health Sciences Grey Lit.", "Humanities Comptuing", "W. Canadian Francophonie", "Ottawa Shooting 2014", "Prairie Provinces Politics & Economics", "UofA Websites", "Web Archive - General")

#barplot of number of finished crawls by collection
par(mar=c(10,3,1,1)) # extends bottom margin to include long collection names
par(mar=c(10,4,3,1)) # readjusting so barplot title and y-axis fits

# las=2 : perpendicular labels
# cex.names : scaling factor for labels
# names.arg : vector with the shortened CollectionNames
barplot(table(Collection), 
        ylab = "Total Number of Crawls", 
        las=2, 
        cex.names = 0.5, 
        names.arg = CollectionNames, 
        main = "2014 Finished Crawls by Collection")

# crawl types
# table
table(Frequency)
# barplot
barplot(table(Frequency), 
        las=2, 
        main = "Finished Crawl Types in 2014")

# averages and graphing for total data crawled, still using finishedCrawl
TotalGBCrawled <- sapply(Total.Data.Crawled..bytes., bytes2gb)
AIcrawls$TotalGB <- TotalGBCrawled
by(TotalGB, Frequency, mean )
by(TotalGB, Frequency, summary )

boxplot(TotalGB~Frequency, 
        data = AIcrawls, 
        las=2,
        main = "Size of Finished Crawls by Frequency",
        ylab = "Gigabytes",
        varwidth = TRUE,
        par(mar = c(7,4,2,2)))

# same plot by by collection
boxplot(TotalGB~Collection, 
        data = AIcrawls, 
        las=2,
        varwidth = TRUE,
        names = CollectionNames,
        main = "Size of Finished Crawls by Collection",
        ylab = "Gigabytes",
        par (mar = c(7,4,2,2), cex.axis = 0.5))

# scatterplots using the lattice library
xyplot(FinishedOn ~ AIcrawls$TotalGB, 
       group=Collection, 
       data = AIcrawls, 
       auto.key = list(space = "right"), 
       jitter.x=TRUE, 
       jitter.y=TRUE,
       ylab = "Date Crawl Finished",
       xlab = "GB of Total Data Crawled")

# creating a mean doc size variable
AIcrawls$MeanDocSizeBytes <- Total.Data.Crawled..bytes. / Total.Documents.Crawled

# testing some correlations for MeanDocSize
cor(AIcrawls$MeanDocSizeBytes, AIcrawls$TotalGB, use = "complete.obs")
cor(AIcrawls$MeanDocSizeBytes, AIcrawls$Total.Documents.Crawled, use = "complete.obs")


