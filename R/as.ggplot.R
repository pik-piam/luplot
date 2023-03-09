#' as.ggplot
#' 
#' Converts a MAgPIE object or a list of MAgPIE objects into a dataframe usable
#' for ggplot
#' 
#' 
#' @usage as.ggplot(x,scenario="default",asDate=T,rev=F,useDimNames=FALSE)
#' @param x MAgPIE object or list of MAgPIE objects. For a list of MAgPIE
#' objects the name of the list entry (has to be character) is used as scenario
#' name.
#' @param scenario Name of the scenario (only used if x is not a list) or
#' position of the entry in the third dimension that contains the scenario name
#' (only if the scenario name is stored in the third dimension) or NULL
#' (automatic detection)
#' @param asDate Format the years as date (TRUE) or keep as is (FALSE);
#' deprecated (only kept for compatibility with older scripts)
#' @param rev reverse legend order (TRUE) or not (FALSE)
#' @param useDimNames Use dim names of 3rd dimension instead Data1, Data2,
#' Data3, Data4; works only if x is a MAgPIE object (no list) and scenario=NULL
#' @return Dataframe usable for ggplot
#' @author Florian Humpenoeder, David Klein
#' @examples
#' 
#' \dontrun{
#' as.ggplot(croparea(gdx))}
#' \dontrun{
#' prices <- list()
#' prices[["Scenario1"]] <- prices("Scenario1.gdx",crops=c("tece","maiz"))
#' prices[["Scenario2"]] <- prices("Scenario2.gdx",crops=c("tece","maiz"))
#' as.ggplot(prices)}
#' @export
#' @importFrom magclass fulldim collapseNames getRegions as.data.frame
#' @importFrom reshape2 melt

as.ggplot <- function(x,scenario="default",asDate=T,rev=F,useDimNames=FALSE) {
  #require("reshape2", quietly = TRUE)
  #require("Hmisc", quietly = TRUE)
  if(is.numeric(scenario)) {
    temp <- x
    x <- list()
    for(scen in fulldim(temp)[[2]][[2+scenario]]) {
      x[[scen]] <- collapseNames(temp[,,scen],scenario)
    }
  }
  if (!is.list(x) & !is.null(scenario)) {
    temp <- x
    x <- list()
    x[[scenario]] <- temp
  }
  if (is.list(x)) {
    scenario_order <- names(x)
    region_order <- getRegions(x[[1]])
    data_dims <- NULL
  } else {
    scenario_order <- getNames(x,dim=1)
    region_order <- getRegions(x)
    data_dims <- names(fulldim(x)[[2]])[-1][-1]
    # capitalize data_dims
    capped <- grep("^[A-Z]", data_dims, invert = TRUE)
    substr(data_dims[capped], 1, 1) <- toupper(substr(data_dims[capped], 1, 1))
  }
  if (rev) scenario_order <- rev(scenario_order)
  #check if coordinates exist as attribute in all MAgPIE objects
  if (all(unlist(lapply(x,function(x) !is.null(attr(x, "coordinates")))))) {
    #check if all coordinates are consistent
    if (all(unlist(lapply(lapply(x,function(x) attr(x, "coordinates")),identical,attr(x[[1]], "coordinates"))))) {
      xy_coord <- attr(x[[1]], "coordinates")
    } else stop("Inconsistent coordinates")
  } else if (any(unlist(lapply(x,function(x) !is.null(attr(x, "coordinates")))))) {
    stop("Missing coordinates for some MAgPIE objects")
  }
  if (is.list(x)) x <- lapply(x,as.data.frame) else x <- as.data.frame(x)
  if (exists("xy_coord")) {
    x <- lapply(x, function(x) {
      x$x <- xy_coord[,1]
      x$y <- xy_coord[,2]      
      return(x)
    })
  }
  x <- suppressMessages(melt(x,measure.vars="Value",value.name="Value"))
  x$variable <- NULL
  if(all(is.na(x$Cell))) x$Cell <- NULL
  names(x)[which(names(x)=="L1")] <- "Scenario"
  names(x)[which(names(x) %in% c("value","Value"))] <- "Value"
  x$Region <- factor(x$Region,region_order)
  if(!is.null(scenario)) x$Scenario <- factor(x$Scenario,scenario_order)
  # if(is.null(scenario)) x$Scenario <- NULL
  x <- x[order(x$Year,x$Region),]
  #if (asDate) x$Year <- as.Date(paste(x$Year, "-01-01",sep = ""), format = "%Y-%m-%d")
  if (asDate) x$Year <- as.numeric(as.character(x$Year))
  if(useDimNames & !is.null(data_dims)) {
    for (i in 1:length(data_dims)) {
      if(nchar(data_dims[i]) > 0) names(x)[which(names(x)==paste0("Data",i))] <- data_dims[i]
    }
    a <- which(names(x)=="Scenario")
    if(length(a) == 1) x <- x[c(a,setdiff(1:dim(x)[2],a))]
    a <- which(names(x)=="Model")
    if(length(a) == 1) x <- x[c(a,setdiff(1:dim(x)[2],a))]
  }
  return(x)
}