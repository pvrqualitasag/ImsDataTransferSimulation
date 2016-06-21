###
###
###
###   Purpose:   R6 Class for Composite Milk Record
###   started:   2016/06/16 (pvr)
###
### ################################################ ###


#' @title R6Class for Composite Milk Record
#'
#' @description
#' The R6Class \code{R6ClassCompositeMilkRecord} represents objects
#' for modelling composite Milk records. A composite milk record
#' is defined to consist of the following components
#' \itemize{
#'   \item milkYield
#'   \item fatPercent
#'   \item proteinPercent
#'   \item sccValue
#'   \item ureaContent
#'   \item lactoseContent
#' }
#' Each of those components is modelled as objects of \code{R6ClassGenericRecord}.
#'
#' @export R6ClassCompositeMilkRecord
R6ClassCompositeMilkRecord <- R6::R6Class( classname = "R6ClassCompositeMilkRecord",
                                           public    = list(
                                             initialize     = function(){
                                               private$milkYield      <- R6ClassGenericRecord$new()
                                               private$milkYield$setRecordType(recordType = "MilkYield")
                                               private$milkYield$setRecordUnit(recordUnit = "kg")
                                               private$fatPercent     <- R6ClassGenericRecord$new()
                                               private$fatPercent$setRecordType(recordType = "FatPercent")
                                               private$fatPercent$setRecordUnit(recordUnit = "%")
                                               private$proteinPercent <- R6ClassGenericRecord$new()
                                               private$proteinPercent$setRecordType(recordType = "ProteinPercent")
                                               private$proteinPercent$setRecordUnit(recordUnit = "%")
                                               private$sccValue       <- R6ClassGenericRecord$new()
                                               private$sccValue$setRecordType(recordType = "SccValue")
                                               private$sccValue$setRecordUnit(recordUnit = "1000/ml")
                                               private$ureaContent    <- R6ClassGenericRecord$new()
                                               private$ureaContent$setRecordType(recordType = "UreaContent")
                                               private$ureaContent$setRecordUnit(recordUnit = "mg/l")
                                               private$lactoseContent <- R6ClassGenericRecord$new()
                                               private$lactoseContent$setRecordType(recordType = "LactoseContent")
                                               private$lactoseContent$setRecordUnit(recordUnit = "mg/l")
                                             },
                                             sampleUnifRecord = function(){
                                               sCurDate <- format(Sys.time(), "%Y%m%d%H%M%S")
                                               private$milkYield$sampleUnifValue(minValue = 15, maxValue = 50)
                                               private$milkYield$setRecordDate(recordDate = sCurDate)
                                               private$fatPercent$sampleUnifValue(minValue = 1, maxValue = 6)
                                               private$fatPercent$setRecordDate(recordDate = sCurDate)
                                               private$proteinPercent$sampleUnifValue(minValue = 1, maxValue = 6)
                                               private$proteinPercent$setRecordDate(recordDate = sCurDate)
                                               private$sccValue$sampleUnifValue(minValue = 10, maxValue = 100)
                                               private$sccValue$setRecordDate(recordDate = sCurDate)
                                               private$ureaContent$sampleUnifValue(minValue = 1, maxValue = 4)
                                               private$ureaContent$setRecordDate(recordDate = sCurDate)
                                               private$lactoseContent$sampleUnifValue(minValue = 1, maxValue = 4)
                                               private$lactoseContent$setRecordDate(recordDate = sCurDate)
                                             },
                                             toTsvString = function(){
                                               return(paste(c(private$milkYield$toTsvString(),
                                                              private$fatPercent$toTsvString(),
                                                              private$proteinPercent$toTsvString(),
                                                              private$ureaContent$toTsvString(),
                                                              private$lactoseContent$toTsvString()),
                                                            sep = "\t",
                                                            collapse = "\t"))
                                             },
                                             toCsv2String = function(){
                                               return(paste(c(private$milkYield$toCsv2String(),
                                                              private$fatPercent$toCsv2String(),
                                                              private$proteinPercent$toCsv2String(),
                                                              private$ureaContent$toCsv2String(),
                                                              private$lactoseContent$toCsv2String()),
                                                            sep = ";",
                                                            collapse = ";"))
                                             },
                                             writeTsvStringToFile = function(psFileName = "composite_milk_record.tsv",
                                                                             pbAppend   = FALSE){
                                               cat(" * Writing record to file:", psFileName, "\n")
                                               cat(self$toTsvString(),"\n",
                                                   file   = psFileName,
                                                   sep    = "",
                                                   append = pbAppend)
                                             },
                                             writeCsv2StringToFile = function(psFileName = "more_milk_record.csv",
                                                                              pbAppend   = FALSE){
                                               cat(" * Writing record to file:", psFileName, "\n")
                                               cat(self$toCsv2String(),"\n",
                                                   file   = psFileName,
                                                   sep    = "",
                                                   append = pbAppend)
                                             }
                                           ),
                                           private   = list(
                                             milkYield      = NULL,
                                             fatPercent     = NULL,
                                             proteinPercent = NULL,
                                             sccValue       = NULL,
                                             ureaContent    = NULL,
                                             lactoseContent = NULL
                                           ) )

