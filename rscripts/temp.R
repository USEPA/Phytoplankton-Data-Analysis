

###
txt <- "Location; Sample.Date; Sample.Time; Sample.Depth; Sample.ID; Sample.Type; Chlorophyll; Collected.By; Disposition; Collection.Method; Preparation.Method"
id <- grepl(txt, OUT$sheetNames)


id <- grepl(txt, OUT$sheetNames)

### files to explicitly skip first go around.
id2 <- grepl("graph", OUT$full_file_name,ignore.case=TRUE)
id3 <- grepl("organized", OUT$full_file_name,ignore.case=TRUE)
id4 <- grepl("Data Q&A", OUT$full_file_name,ignore.case=TRUE) #### do not include because all files are parsed, split up and contain figures.

OUTsub2 <- OUT[id  & !OUT$processed, ]

OUT$processed[OUT$full_file_name %in% OUTsub2$full_file_name] <- TRUE
shortList <- OUT[id, ]



dim(shortList)
OUT$batch[id] <- "batch3"

batch3 <- readSelectSheets(shortList)
dim(batch3)
write.table(batch3, "../../../output/step1/batch3.csv", sep = ",",row.names=FALSE)



