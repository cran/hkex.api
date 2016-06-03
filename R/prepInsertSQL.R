#' @title A Function to Generate a List of SQL statements from Dataframe. 
#'
#' @description
#' \code{prepInsertSQL} returns a list of insert SQL statements
#' 
#' @details
#' The function is to generate insert SQL statements with data.frame data
#' @param  tableName a table name
#' @param  df dataframe that provides data
#' @param quote (Optional) it is a vector indicates whether a data element should be quoted or not. 1 means quoted while 0 means not.
#' @keywords hkex.api
#' @examples 
#' \dontrun{
#'    testData=data.frame(name=c("peter","john"), age=c(18,23))
#'    sqlStatement=prepInsertSQL(tableName="student", testData, quote=c(1,0))
#' }
#' 
prepInsertSQL=function(tableName="", df=NULL, quote=NULL )
{ 
  if (!(!is.null(quote) && length(unique(quote))<=2 && all(unique(quote) %in% c(1,0)))) 
    stop("error: quote can be either 0 or 1")
  
  if (!is.null(quote) && ncol(df)!=length(quote))
    stop("error: the length of quote should be the same as the number of columns")
  sql=paste("insert into ",tableName, " values(",sep="")

  sql.ls=list()
  for (k in 1:nrow(df))
  {
    m="," 
    sql2=""
    for (i in 1:ncol(df))
    {
      q="'"
      if (!is.null(quote) && quote[i]==0) q=""
      if (i == ncol(df)) m=""
      sql2=paste(sql2,q,df[1,i],q,m,sep="")
    }
    sql2=paste(sql2, ")", sep="")
    sql.ls[[k]]=paste(sql, sql2,sep="")
  }
  return(sql.ls)
} 