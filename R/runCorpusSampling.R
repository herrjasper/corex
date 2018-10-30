set.seed(20180921)

library(parallel)
library(plyr)

# parallel setup:
no_cores <- detectCores() - 1
cluster <- makeCluster(no_cores, type ="FORK")

tablename <- "uk_corex"; ndocs <- 23057609; dbname <- 'dereko'
tablename <- "uk_corex_min100"; ndocs <- 16763354; dbname <- 'dereko'
tablename <- "decow16b_meta"; ndocs <- 16266522; dbname <- 'decow'
tablename <- "decow16b_meta_min100"; ndocs <- 15119452; dbname <- 'decow'

connectiondetails_dereko <- c('ukviewer', '', 'dereko', 'gramlinux02')
connectiondetails_decow <- c('ukviewer', '', 'decow', 'gramlinux02')

samplesize = 1000
maxchunksize = 70
replacement = FALSE

docs <- sample.int(ndocs, size = samplesize, replace = replacement)
docchunks <- split(docs, ceiling(seq_along(docs)/maxchunksize))
docchunks

corpussample_naked(docchunks$`1`, connectiondetails_dereko, tablename="uk_corex")

# with lapply, no parallelization:
system.time(dat <- lapply(docchunks, corpussample_naked, connectiondetails_dereko,  tablename="uk_corex"))
df <- ldply(dat, data.frame)
df <- df[2:length(df)]
head(df)
# parallelization (does NOT work):
#dat.par <- parLapply(cluster, docchunks, corpussample_naked, connectiondetails_dereko, mintokens=0, tablename="uk_corex")


# parallelization, works:
system.time({dat.par <- mclapply(docchunks, corpussample_naked, mc.cores = 7, connectiondetails_dereko, tablename="uk_corex")
             dat.par <- ldply(dat.par, data.frame)
             dat.par <- dat.par[2:length(dat.par)]})

head(dat.par)
nrow(dat.par)



system.time(df <- corpussample3(size=1000, mydb, mintokens=0, replacement = FALSE, tablename="uk_corex", ndocs=ndocs, maxchunksize=1000))
nrow(df)
head(df)


mycon <- getconnection(connectiondetails_dereko)
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
