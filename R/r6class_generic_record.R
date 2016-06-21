###
###
###
###   Purpose:   R6 Class for Generic Data Record
###   started:   2016/06/16 (pvr)
###
### ################################################ ###

#' @title R6 Class for Generic Data Records
#'
#' @description
#' The R6 class \code{R6ClassGenericRecord} represents
#' objects of generic data records. These generic objects
#' can be used derive more specific data records such as
#' milk yield records, fat percentage records, etc.
#'
#' @export R6ClassGenericRecord
#' @usage  R6ClassGenericRecord$new()
#' @return Object of \code{\link{R6Class}} R6ClassGenericRecord
#' @examples
#' r6objGenRec <-  R6ClassGenericRecord$new()
#' r6objGenRec$setRecordType(recordType = "MilkYield")
#' r6objGenRec$setRecordValue(recordValue = 32.6)
#' r6objGenRec$setRecordUnit(recordUnit = "kg")
#' \dontrun{
#' r6objGenRec$toTsvString()
#' }
#' @field recordType    name of record type
#' @field recordDate    date of recording
#' @field recordValue   value of the record
#' @field recordUnit    unit associated to record value
#' @section Public methods:
#' \describe{
#'   \item{\code{new()}}{Instantiation of an R6ClassGenericRecord object}
#'   \item{\code{sampleUnifValue(minValue = 0, maxValue = 1)}}{Sample a value from a uniform
#'                 distribution between \code{minValue} and \code{maxValue}}
#'   \item{\code{toTsvString()}}{Conversion of a generic record to a string using TABs as field
#'                 separators. The order in which the fields are pasted together is fixed to
#'                 Type Date Value Unit.}
#' }
R6ClassGenericRecord <- R6::R6Class( classname = "R6ClassGenericRecord",
                                     public    = list(
                                       initialize      = function(){
                                         private$recordDate <- format(Sys.Date(), "%Y%m%d")
                                       },
                                       sampleUnifValue = function(minValue = 0, maxValue = 1, digits = 2){
                                         self$setRecordValue(recordValue = round(runif(n=1, min = minValue, max = maxValue), digits = digits))
                                       },
                                       toTsvString     = function(){
                                         return(private$toSepString(psSep = "\t"))
                                       },
                                       toCsv2String   = function(){
                                         return(private$toSepString(psSep = ";"))
                                       },
                                       setRecordType  = function(recordType){private$recordType <- recordType},
                                       getRecordType  = function(){return(private$recordType)},
                                       setRecordDate  = function(recordDate){private$recordDate <- recordDate},
                                       getRecordDate  = function(){return(private$recordDate)},
                                       setRecordValue = function(recordValue){private$recordValue <- recordValue},
                                       getRecordValue = function(){return(private$recordValue)},
                                       setRecordUnit  = function(recordUnit){private$recordUnit <- recordUnit},
                                       getRecordUnit  = function(){return(private$recordUnit)}
                                     ),
                                     private   = list(
                                       recordType  = NULL,
                                       recordDate  = NULL,
                                       recordValue = NULL,
                                       recordUnit  = NULL,
                                       toSepString     = function(psSep){
                                         return(paste(self$getRecordType(),
                                                      self$getRecordDate(),
                                                      self$getRecordValue(),
                                                      self$getRecordUnit(), sep = psSep, collapse = psSep))
                                       }
                                     ) )

