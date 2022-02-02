set.seed(20180829)
#sum(meta[,c(5,6,8:120)][1,] * fa.n12$loadings[,1])

library(RMySQL)
library(parallel)

colnames <- c("id", "ttrat", "wlen", 
              "slen", "modal", "vv", "vaux", 
             "vfin", "cn", "prep", "inf", "imp", "adv",
             "adj", "subjs", "subji", "conj", "wh",
             "dem", "poss", "neg", "answ", "zuinf",
             "parta", "card", "itj", "nonwrd", "def",
             "indef", "neper", "neloc", "neorg", "emo",
             "dq", "clitindef", "vpast", "vpres",
             "vpressubj", "wpastsubj", "vvpastsubj",
             "pper_1st", "pper_2nd", "pper_3rd", 
             "simpx", "psimpx", "rsimpx", "v2", "vlast",
             "vflen", "esvf", "clausevf", "cmpnd", "unkn",
             "short", "qsvoc", "cnloan", "vvieren",
             "sapos", "pass", "perf", "plu", "forum")  




# MySQL connection data:
user <- 'ukviewer'
password = ''
host = 'gramlinux02.ids-mannheim.de'

tablename <- "uk_corex"; ndocs <- 23057609; dbname <- 'dereko'
tablename <- "uk_corex_min100"; ndocs <- 16763354; dbname <- 'dereko'
tablename <- "decow16b_meta"; ndocs <- 16266522; dbname <- 'decow'
tablename <- "decow16b_meta_min100"; ndocs <- 15119452; dbname <- 'decow'


# connect to db:
mydb = dbConnect(MySQL(), user=user, password=password, dbname=dbname, host=host)


# List open connections:
#dbListConnections( dbDriver( drv = "MySQL"))
# close all open connections:
#lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)


#dbListTables(mydb)

# get column names:
fieldnames <- dbListFields(mydb, tablename)

# get number of rows in data base:
ndocs_rs <- dbSendQuery(mydb, paste("SELECT COUNT(*) FROM", tablename))
ndocs <- dbFetch(ndocs_rs, n=-1)
ndocs



############
### split large liste of sampled doc nums into smaller chunks, 
### run query musltiple times, concatenate results.


corpussample3 <- function(size=10, dbconnection, mintokens=0, replacement = FALSE, tablename="uk_corex_min100", ndocs = 16763354, maxchunksize=50000){
  
  cat("Sampling from table: ", tablename, "\n")
  cat("Size of master corpus: ", ndocs, "documents\n")
  cat("Requested sample size: ", size, "documents\n")
  cat("Minimum document length: ", mintokens, "tokens\n")
  cat("Sampling with replacement: ", replacement, "\n")
  cat("Maximal number of documents per MySQL query: ", maxchunksize, "\n")
  
  
  # add a little to the requested sample size
  # to allow for document filtering by token count:
  
  samplesize <- size
  if(!endsWith(tablename, "_min100")){
#  samplesize = round(size + size*0.5,0)
  samplesize = size
  cat("Increasing sample size to: ", samplesize, "(very short documents will be discarded)\n")
  }
  
  
  # make an empty matrix:
  m <- matrix(NA, nrow = 0, ncol = length(colnames))
  colnames(m) <- colnames
  # convert to data frame:
  df <- data.frame(m)
  
  # make a vector of random numbers
  docs <- sample.int(ndocs, size = samplesize, replace = replacement)
  docchunks <- split(docs, ceiling(seq_along(docs)/maxchunksize))
  
  if (length(docchunks) > 1){
    cat("Splitting query into", length(docchunks), "parts\n")
  }
  
  start_time <- Sys.time()
  
  counter = 0
  for (chunk in docchunks){
    counter <- counter +1
    cat("Running query #", counter, "\n")
#    cat(chunk,"\n")
    docs <- paste("(", paste(chunk, collapse = ", "), ")")
#    cat(docs,"\n")
  

  # start retrieving document meta data:
  start_time <- Sys.time()
  querytext = paste("select", paste(colnames, collapse = ", "), " from ", tablename, " where num in ", docs)
  rs <- dbSendQuery(dbconnection, querytext)
  data <- dbFetch(rs, n=-1)
  df <- rbind.data.frame(df,data)
  }
  
  cat("Original sample: ", nrow(df), "documents\n")
  df <- subset(df, tokc >= mintokens)
  cat("Documents >=", mintokens, "tokens: ", nrow(df), "\n")
  
  if (nrow(df) >= size){
    selection <- sample(1:nrow(df),size)
    df = df[selection,]
    cat("Sampling ", size, "documents\n")
  }
  else{
    cat("WARNING: Sample size is ", nrow(df), "docs (requested:", size, "docs)\n")
  }
  
  end_time <- Sys.time()
  cat(end_time - start_time, "\n")
  return(df)
}




corpussample_naked <- function(chunk, connectiondetails, tablename="uk_corex_min100", selectby="num"){
  
  user <- connectiondetails[1]
  password <- connectiondetails[2]
  dbname <- connectiondetails[3]
  host <- connectiondetails[4]
  
  mydb = dbConnect(MySQL(), user=user, password=password, dbname=dbname, host=host)
  
  docs <- paste("(", paste(chunk, collapse = ", "), ")")
#  docs <- paste("(", paste(chunk$`1`, collapse = ", "), ")")
  querytext = paste("select", paste(colnames, collapse = ", "), " from ", tablename, " where ", selectby, " in ", docs)
#  cat(querytext,"\n")
  rs <- dbSendQuery(mydb, querytext)
  df <- dbFetch(rs, n=-1)
  dbDisconnect(mydb)
  return(df)
}


getconnection <- function(connectiondetails){
  user <- connectiondetails[1]
  password <- connectiondetails[2]
  dbname <- connectiondetails[3]
  host <- connectiondetails[4]
  
  mydb = dbConnect(MySQL(), user=user, password=password, dbname=dbname, host=host)
  return(mydb)
}


parallel_from_dereko <- function(chunks, selectby){
  system.time({df <- mclapply(chunks, corpussample_naked, mc.cores = 7, connectiondetails_dereko, tablename="uk_corex_min100", selectby=selectby)
  df <- ldply(df, data.frame)
  df <- df[2:length(df)]
  }
  )
  cat("Data frame from dereko has", nrow(df), "rows,", ncol(df), "columns.\n")
  return(df)
}

parallel_from_decow <- function(chunks, selectby){
  system.time({df <- mclapply(chunks, corpussample_naked, mc.cores = 7, connectiondetails_decow, tablename="decow16b_meta_min100", selectby=selectby)
  head(df)
  df <- ldply(df, data.frame)
  df <- df[2:length(df)]})
  cat("Data frame from decow has", nrow(df), "rows,", ncol(df), "columns.\n")
  return(df)
}



chunker <- function(vec, maxchunksize=7000){
  chunks <- split(vec, ceiling(seq_along(vec)/maxchunksize))
  cat("Split vector into", length(chunks), "chunks.\n")
  return(chunks)
}


format_mysql_listquery <- function(vec){
  formatted <- gsub("^","'",gsub("$","'", vec))
  return(formatted)
} 

