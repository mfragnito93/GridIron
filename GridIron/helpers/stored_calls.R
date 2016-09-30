##Stored Calls

##RP HASH FIELD
getRPHashField <- function(data,field){
  return(getTable(filter(addFieldBucket(data),FIELD_BUCKET==field),c("PLAY_TYPE","HASH")))
}
