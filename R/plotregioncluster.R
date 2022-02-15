#' Plots world map with regions and clusters
#' 
#' Function to plot world map showing world regions and cluster.
#' 
#' @param x a vector with 59199 cells in MAgPIE order and format \code{region.cluster} where \code{region}
#' is the name of the region and \code{cluster} is the number of the cluster.
#' @seealso \code{\link{plotspam}}
#' @importFrom data.table data.table
#' @importFrom ggplot2 scale_fill_manual theme_linedraw scale_alpha_continuous geom_tile
#' @examples 
#'   x <- paste(rep(paste0("REG",1:5),each=12000),rep(1:25,each=2400),sep=".")[1:59199]
#'   plotregionscluster(x)
#' @export

plotregionscluster <- function(x) {
  
  prepData <- function(x) {
    # add lon/lat information
    x <- data.table(cbind(mag05cells,cluster=x))
    
    # add region column
    x$region <- sub("\\..*$","",x$cluster)
    x$region <- as.factor(x$region)
    
    # convert cluster column to integers
    x$cluster <- as.factor(x$cluster)
    
    z <- data.frame(reg=sub("\\..*$","",levels(x$cluster)),cluster=as.numeric(sub("^.*\\.","",levels(x$cluster))))
    ncluster <- NULL
    for(r in levels(z$reg)) {
      ncluster[r] <- sum(z$reg==r)
      z$cluster[z$reg==r] <- round(seq(from = 1/ncluster[r], to = 1, by=1/ncluster[r]),2)
    }
    x$alpha <- z$cluster[x$cluster]
    legend_text <- paste0(paste(levels(x$region),ncluster[levels(x$region)],sep=" ("),")")
    names(legend_text) <- levels(x$region)
    attr(x,"legend_text") <- legend_text
    return(x)
  }
  
  wrld_simpl_df <- NULL
  data("world", envir = environment(), package="luplot")
  
  x <- prepData(x)
  
  map <- ggplot(x,aes_(~lon,~lat)) + 
         geom_polygon(data=wrld_simpl_df, aes_(~long,~lat, group=~group, fill=~hole), fill="lightgrey") + 
         geom_tile(aes_(fill=~region,alpha=~alpha)) + 
         geom_path(data=wrld_simpl_df, aes_(~long, ~lat, group=~group, fill=NULL), color="grey10", size=0.2) +
         scale_alpha_continuous(range=range(x$alpha)) +
         scale_fill_manual(values=plotstyle(levels(x$region)), labels=attr(x,"legend_text")) +
         guides(fill=guide_legend(title="Region (number of cluster)",nrow=2), alpha="none") +
         theme_linedraw() + 
         theme(aspect.ratio=0.6, legend.position=c(0.01,0.01), legend.justification = c(0,0), legend.background = element_rect(color = "black", fill = "white", size = 1, linetype = "solid")) 
  return(map)
}
