#' @title An API to Download HSI Options Data
#'
#' @description
#' \code{getHSIOptions} returns HSI options in dataframe.
#' 
#' @details
#' The function is to retrieve HSI options data from the HKEX website.
#' @keywords hkex.api
#' @import XML RCurl httr
#' @references https://www.hkex.com.hk
#' @examples 
#' \dontrun{
#'    getHSIOptions()
#' }

getHSIOptions=function()
{
  #library(XML)
  #library(httr)
  #library(RCurl)
  
  domain="https://www.hkex.com.hk/"
  webpage="eng/ddp/contract_details.asp"
  argument="?PId=2"
  indexCode="HSI"
  urlLink=paste(domain,webpage,argument,sep="")
  page= RCurl::getURL(urlLink)
  hp=XML::htmlParse(page)
  
  # find the date indicated on the webpage
  x <- XML::xpathApply(hp, "//*[contains(@class, 'verd_black11')]/text()") #useful
  sDate=XML::xmlValue(x[[3]])
  sTime=sub(":","",substring( sDate, 26))
  sTime=substr(sTime,1,4)
  sDate= substr(sDate, 15, 24)
  sDate=as.character(strptime(  sDate , "%d/%m/%Y"))
  
  htmlTable=XML::readHTMLTable(page)
  aTable=htmlTable$table
  
  header=c("mix", "o", "b", "a", "lt", "h", "l", "v", "pp", "chg", "oi")
  names(aTable)=header 
  
  aTable$tmp=indexCode
  tbl=data.frame(code=aTable$tmp, aTable)
  tbl$tmp=NULL  
  
  tbl=data.frame(lapply(tbl, as.character), stringsAsFactors=FALSE)
  
  tbl$type=substring(tbl[,2],1,1)
  tbl$date=substring(tbl[,2],3,8)
  tbl$strike=as.numeric(substring(tbl[,2],12))
  tbl[,2]=NULL
  tbl$udate=sDate
  tbl$utime=as.numeric(sTime) 
  #str(tbl)
  return(tbl)
}  
