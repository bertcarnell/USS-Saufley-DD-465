## Load the outline of the War Diaries and After Action Reports

X <- read.csv("data/USS Saufley War Reports.csv", header = TRUE)

X$start_num <- NA
X$start_base_str <- NA
X$start_dir <- NA

# start at 66, skip 106
for (i in c(66:105, 107:nrow(X)))
{
  start_addr <- strsplit(X$First.Image[i], split = "[/]")[[1]]
  X$start_num[i] <- as.numeric(gsub("[.]jpg", "", start_addr[length(start_addr)]))
  if (i == 107) X$start_num <- 126
  start_base <- start_addr[3:(length(start_addr)-1)]
  X$start_base_str[i] <- paste0("https://", paste(start_base, sep = "", collapse = "/"))
  X$start_dir[i] <- paste0(i, "_", X$Date[i])
}

## Download Images for the War Diaries and After Action Reports

if (!dir.exists("img"))
{
  dir.create("img")
}
if (!dir.exists("img/war_history"))
{
  dir.create("img/war_history")
}

  
for (i in c(66:105, 107:nrow(X)))
{
  if (!dir.exists(file.path("img", "war_history", X$start_dir[i])))
  {
    dir.create(file.path("img", "war_history", X$start_dir[i]))
  }
  print(i)
  for (j in 1:X$Images[i])
  {
    base_file_name <- paste0(formatC(X$start_num[i] + j - 1, width = 4, flag = "0"), 
                             ".jpg")
    download_url <- paste0(X$start_base_str[i], "/", base_file_name)
    if (i == 73 & j == 8) download_url <- gsub("[.]jpg", "b.jpg", download_url)
    if (i == 107 & j == 1) download_url <- gsub("[.]jpg", "a.jpg", download_url)
    bdone <- FALSE
    while (!bdone)
    {
      tryCatch({
        download.file(url = download_url,
                      destfile = file.path("img", "war_history", X$start_dir[i], base_file_name),
                      mode = "wb")
        bdone <- TRUE}, error = function(e) Sys.sleep(1))
    }
  }
}

if (!requireNamespace("paws")) {
  install.packages("paws")
}

if (FALSE) {
  ################################################################################
  ## Get Data
  
  require(paws)
  
  # secrets
  source("~/bertcarnell_aws_info.R")
  
  s3bucket <- "saufley-war-reports"
  
  # https://docs.aws.amazon.com/textract/latest/dg/API_Reference.html
  
  textract <- paws::textract()
  s3 <- paws::s3()
  
  s3files_1 <- s3$list_objects(Bucket = s3bucket, Prefix = "muster/muster-00")
  s3files_2 <- s3$list_objects(Bucket = s3bucket, Prefix = "muster/muster-01")
  s3files <- c(sapply(s3files_1$Contents, function(x) x$Key, USE.NAMES = FALSE),
               sapply(s3files_2$Contents, function(x) x$Key, USE.NAMES = FALSE))
  
  s3files_groups <- sapply(s3files, function(x) gsub("[/].*", "", x), USE.NAMES = FALSE)
  
  # start all the textract text extractions and save the jobids
  
  s3files_jobids <- character(length(s3files))
  
  for (i in seq_along(s3files))
  {
    cat(paste("\nTrying", i, s3files[i]))
    
    # catch connection errors and loop until a successful submission
    resp <- NULL
    while (is.null(resp))
    {
      tryCatch({
        resp <- textract$start_document_text_detection(
          DocumentLocation = list(
            S3Object = list(Bucket = s3bucket, Name = s3files[i])
          ))
      }, error = function(e) print(e))
      Sys.sleep(1)
    }
    s3files_jobids[i] <- resp$JobId
  }
  
  # get results from all jobs
  
  stopifnot(all(!is.na(s3files_jobids)))
  
  results <- vector("list", length = length(s3files))
  for (i in seq_along(s3files))
  {
    cat(paste("\nTrying", i, s3files[i]))
    bdone <- FALSE
    while(!bdone)
    {
      while (is.null(results[[i]]))
      {
        tryCatch({
          results[[i]] <- textract$get_document_text_detection(
            JobId = s3files_jobids[i]
          )}, error = function(e) print(e))
        Sys.sleep(1)
      }
      if (results[[i]]$JobStatus != "IN_PROGRESS")
      {
        bdone <- TRUE
      }
      Sys.sleep(1)
    }
    # check that there are not multiple parts
    if (length(results[[i]]$NextToken) != 0) {
      cat(paste("\n\tMulti-part in ", i, s3files[i]))
    }
  }
  
  stopifnot(all(!is.null(results)))
  
  stopifnot(all(sapply(results, function(x) length(x$Warnings) == 0)))
  stopifnot(all(sapply(results, function(x) length(x$StatusMessage) == 0)))
  stopifnot(all(sapply(results, function(x) length(x$NextToken) == 0)))
  stopifnot(all(sapply(results, function(x) x$DocumentMetadata$Pages) == 1))
  
  save(s3files, s3files_groups, s3files_jobids,
       results, file = file.path("src", "output", "ActionReports_and_muster.Rdata"))
}

################################################################################

load(file.path("src", "output", "ActionReports_and_muster.Rdata"))

unique(sapply(results[[1]]$Blocks, function(x) x$BlockType))

################################################################################

get_lines <- function(textract_result) {
  ind <- which(sapply(textract_result$Blocks, function(x) x$BlockType) == "LINE")
  sapply(textract_result$Blocks[ind], function(x) x$Text)
}

################################################################################

s3files_short <- sapply(s3files, function(x) {
  gsub(".*[/]", "", x)
}, USE.NAMES = FALSE)

results_lines <- lapply(results, get_lines)

results_lines2 <- lapply(results_lines, function(x) {
  c(x, "", "", "", "", "")
})

if (!dir.exists(file.path("src", "output"))) 
  dir.create(file.path("src", "output"))

writeLines(unlist(results_lines2),
           file.path("src", "output", "ActionReports_and_muster.txt"))


################################################################################

if (FALSE) {
  ################################################################################
  ## Get Data
  
  require(paws)
  
  # secrets
  source("~/bertcarnell_aws_info.R")
  
  s3bucket <- "saufley-war-reports"
  
  # https://docs.aws.amazon.com/textract/latest/dg/API_Reference.html
  
  textract <- paws::textract()
  s3 <- paws::s3()
  
  bdone <- FALSE
  s3files <- s3$list_objects(Bucket = s3bucket)
  s3files <- sapply(s3files$Contents, function(x) x$Key, USE.NAMES = FALSE)
  while (!bdone)
  {
    print(length(s3files))
    temp <- s3$list_objects(Bucket = s3bucket, Marker = s3files[length(s3files)])
    temp <- sapply(temp$Contents, function(x) x$Key, USE.NAMES = FALSE)
    s3files <- c(s3files, temp)
    if (length(temp) < 1000)
      bdone <- TRUE
  }
  
  print(length(s3files))
  
  s3files_groups <- sapply(s3files, function(x) gsub("[/].*", "", x), USE.NAMES = FALSE)
  
  # Cut out the muster ones since we already did those
  s3files <- s3files[-which(s3files_groups == "muster")]
  s3files_groups <- s3files_groups[-which(s3files_groups == "muster")]
  
  # cut out the stuff I have already OCR'd (up to folder 66)
  s3files_num <- as.numeric(sapply(s3files_groups, function(x) strsplit(x, "[_]")[[1]][1]))
  s3files <- s3files[which(s3files_num >= 66)]
  s3files_groups <- s3files_groups[which(s3files_num >= 66)]
  
  # start all the textract text extractions and save the jobids
  
  s3files_jobids <- character(length(s3files))
  #for (i in seq_along(s3files))
  for (i in 1539:length(s3files))
  {
    cat(paste("\nTrying", i, s3files[i]))
    
    # catch connection errors and loop until a successful submission
    resp <- NULL
    while (is.null(resp))
    {
      tryCatch({
        resp <- textract$start_document_text_detection(
          DocumentLocation = list(
            S3Object = list(Bucket = s3bucket, Name = s3files[i])
          ))
      }, error = function(e) print(e))
      Sys.sleep(1)
    }
    s3files_jobids[i] <- resp$JobId
  }
  
  # get results from all jobs
  
  results <- vector("list", length = length(s3files))
  
  for (i in 938:1538)
  {
    if (is.na(s3files_jobids[i])) next
    cat(paste("\nTrying", i, s3files[i]))
    bdone <- FALSE
    while(!bdone)
    {
      while (is.null(results[[i]]))
      {
        tryCatch({
          results[[i]] <- textract$get_document_text_detection(
            JobId = s3files_jobids[i]
          )}, error = function(e) print(e))
        Sys.sleep(1)
      }
      if (results[[i]]$JobStatus != "IN_PROGRESS")
      {
        bdone <- TRUE
      }
      Sys.sleep(1)
    }
    # check that there are not multiple parts
    if (length(results[[i]]$NextToken) != 0) {
      cat(paste("\n\tMulti-part in ", i, s3files[i]))
    }
  }
  
  for (i in 1539:length(s3files))
  {
    cat(paste("\nTrying", i, s3files[i]))
    
    # catch connection errors and loop until a successful submission
    resp <- NULL
    while (is.null(resp))
    {
      tryCatch({
        resp <- textract$start_document_text_detection(
          DocumentLocation = list(
            S3Object = list(Bucket = s3bucket, Name = s3files[i])
          ))
      }, error = function(e) print(e))
      Sys.sleep(1)
    }
    s3files_jobids[i] <- resp$JobId
  }
  
  for (i in 1539:length(s3files))
  {
    if (is.na(s3files_jobids[i])) next
    cat(paste("\nTrying", i, s3files[i]))
    bdone <- FALSE
    while(!bdone)
    {
      while (is.null(results[[i]]))
      {
        tryCatch({
          results[[i]] <- textract$get_document_text_detection(
            JobId = s3files_jobids[i]
          )}, error = function(e) print(e))
        Sys.sleep(1)
      }
      if (results[[i]]$JobStatus != "IN_PROGRESS")
      {
        bdone <- TRUE
      }
      Sys.sleep(1)
    }
    # check that there are not multiple parts
    if (length(results[[i]]$NextToken) != 0) {
      cat(paste("\n\tMulti-part in ", i, s3files[i]))
    }
  }
  
  stopifnot(all(!is.null(results)))
  
  stopifnot(all(sapply(results, function(x) length(x$Warnings) == 0)))
  stopifnot(all(sapply(results, function(x) length(x$StatusMessage) == 0)))
  stopifnot(all(sapply(results, function(x) length(x$NextToken) == 0)))
  stopifnot(all(sapply(results, function(x) x$DocumentMetadata$Pages) == 1))
  
  save(s3files, s3files_groups, s3files_jobids,
       results, file = file.path("src", "output", "DesronActionReports.Rdata"))
}

################################################################################

load(file.path("src", "output", "DesronActionReports.Rdata"))

unique(sapply(results[[1]]$Blocks, function(x) x$BlockType))

################################################################################

get_lines <- function(textract_result) {
  ind <- which(sapply(textract_result$Blocks, function(x) x$BlockType) == "LINE")
  sapply(textract_result$Blocks[ind], function(x) x$Text)
}

################################################################################

s3files_short <- sapply(s3files, function(x) {
  gsub(".*[/]", "", x)
}, USE.NAMES = FALSE)

results_lines <- lapply(results, get_lines)

results_lines2 <- lapply(results_lines, function(x) {
  c(x, "", "", "", "", "")
})

if (!dir.exists(file.path("src", "output"))) 
  dir.create(file.path("src", "output"))

writeLines(unlist(results_lines2),
           file.path("src", "output", "DesronActionReports.txt"))
