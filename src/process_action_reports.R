if (FALSE) {
  install.packages("paws")
}

################################################################################
## Get Data

require(paws)

# secrets
source("~/bertcarnell_aws_info.R")

s3bucket <- "saufley-war-reports"

# https://docs.aws.amazon.com/textract/latest/dg/API_Reference.html

textract <- paws::textract()
s3 <- paws::s3()

s3files <- s3$list_objects(Bucket = s3bucket)
s3files <- sapply(s3files$Contents, function(x) x$Key, USE.NAMES = FALSE)

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

# 58 failed because it is a text file
stopifnot(all(sapply(results, function(x) x$JobStatus)[-58] == "SUCCEEDED"))

stopifnot(all(sapply(results[-58], function(x) length(x$Warnings) == 0)))
stopifnot(all(sapply(results[-58], function(x) length(x$StatusMessage) == 0)))
stopifnot(all(sapply(results[-58], function(x) length(x$NextToken) == 0)))
stopifnot(all(sapply(results[-58], function(x) x$DocumentMetadata$Pages) == 1))

save(s3files, s3files_groups, s3files_jobids,
     results, file = "ActionReports.Rdata")

################################################################################

unique(sapply(results[[1]]$Blocks, function(x) x$BlockType))

################################################################################

get_lines <- function(textract_result) {
  ind <- which(sapply(textract_result$Blocks, function(x) x$BlockType) == "LINE")
  sapply(textract_result$Blocks[ind], function(x) x$Text)
}

add_div_to_begin_end <- function(x) {
  x[1] <- paste0("&lt;div&gt;", x[1])
  x[length(x)] <- paste0(x[length(x)], "&lt;/div&gt;")
  return(x)
}

add_div_to_parts <- function(x) {
  ind <- grep("^[0-9]+[.]$", trimws(x))
  if (length(ind) > 0) x[ind] <- paste0("&lt;/div&gt;&lt;div&gt;", x[ind])
  ind <- grep("^[A-Z]+[.]$", trimws(x))
  if (length(ind) > 0) x[ind] <- paste0("&lt;/div&gt;&lt;div&gt;", x[ind])
  ind <- grep("^PART [IXV]+", trimws(x))
  if (length(ind) > 0) x[ind] <- paste0("&lt;/div&gt;&lt;div&gt;", x[ind])
  return(x)
}

add_div_to_times <- function(x) {
  ind <- grep("^[0-2][0-9][0-5][0-9]$", trimws(x))
  if (length(ind) > 0) x[ind] <- paste0("&lt;/div&gt;&lt;div&gt;", x[ind])
  return(x)
}

################################################################################
load("ActionReports.Rdata")

s3files_short <- sapply(s3files, function(x) {
  gsub(".*[/]", "", x)
}, USE.NAMES = FALSE)

results_lines <- lapply(results, get_lines)

# group up the results_lines and process in groups

ugroups <- unique(s3files_groups)
subjects <- character(length(ugroups))
results_groups <- vector("list", length(ugroups))
output <- character(1)
counter <- 1
for (i in seq_along(ugroups))
{
  ind_group <- which(s3files_groups == ugroups[i])
  ind_subj <- grep("^[sS]ubje[ce]t[ ]*:$", results_lines[[ind_group[1]]])
  ind_ref <- grep("^Reference[s]*:$", results_lines[[ind_group[1]]])
  ind_enc <- grep("^Enclosure[s]*:$", results_lines[[ind_group[1]]])
  results_groups[[i]]$Group <- ugroups[i]
  if (length(ind_subj) > 0 & length(ind_ref) > 0)
  {
    results_groups[[i]]$Subject <- paste(results_lines[[ind_group[1]]][(ind_subj + 1):(ind_ref - 1)], collapse=" ")
  } else if (length(ind_subj) > 0 & length(ind_enc) > 0) 
  {
    results_groups[[i]]$Subject <- paste(results_lines[[ind_group[1]]][(ind_subj + 1):(ind_enc - 1)], collapse=" ")
  } else
  {
    print(results_lines[[ind_group[1]]])
  }
  
  results_groups[[i]]$Text <- results_lines[ind_group]

  if (results_groups[[i]]$Subject == "War Diary.")
    next

  output[counter] <- results_groups[[i]]$Subject
  output[counter+1] <- "<images>"
  counter <- counter + 2
  for (j in ind_group) {
    output[counter] <- "\t<image>"
    output[counter+1] <- paste0("\t\t<file>", s3files_short[j], "</file>")
    output[counter+2] <- "\t\t<items>"
    output[counter+3] <- "\t\t\t<item>"
    output[counter+4] <- "\t\t\t\t<date></date>"
    output[counter+5] <- "\t\t\t\t<description>"
    counter <- counter + 6
    len <- length(results_lines[[j]])
    if (len > 0) {
      temp <- add_div_to_begin_end(results_lines[[j]])
      temp <- add_div_to_parts(temp)
      temp <- add_div_to_times(temp)
      output[counter:(counter + len - 1)] <- temp
      counter <- counter + len + 2
    }
    output[counter] <- "\t\t\t\t</description>"
    output[counter+1] <- "\t\t\t</item>"
    output[counter+2] <- "\t\t</items>"
    output[counter+3] <- "\t</image>"
    counter <- counter + 4
  }
  output[counter] <- "</images>"
  counter <- counter + 1
}

if (!dir.exists(file.path("src", "output"))) 
  dir.create(file.path("src", "output"))
writeLines(output, con = file.path("src", "output", "AfterAction.txt"))

writeLines(unlist(results_groups[[which(sapply(results_groups, function(x) x$Subject == "War Diary."))]]$Text),
           file.path("src", "output", "WarDiary.txt"))


