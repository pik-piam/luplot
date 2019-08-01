#' Creates bar plot to display given colors
#' 
#' For a quick translation of hexadecimal color codes into visible colors this
#' function simply creates a bar plot with the hexadecimal colors specified by
#' the user. It takes a vector of strings containing the hexadecimal color
#' codes, e.g. "#D0DD1E".
#' 
#' @usage showcolors(...)
#' @param ... One ore more strings or a vector of strings containing
#' hexadecimal color codes.
#' @author David Klein
#' @examples
#' 
#' \dontrun{showcolors("#D0DD1E")}
#' \dontrun{showcolors("#beefee")}
#' \dontrun{showcolors(c("#beefee","#D0DD1E"),"#FFFFFF")}
#' @importFrom ggplot2 coord_flip
#' @export
showcolors <- function(...) {
  colors <- c(...)
  labels <- colors
  # if ... is a named vector add names to the labels
  if(!is.null(names(colors))) labels <- paste(names(colors),colors,sep="\n")
  # create data frame
  df<-data.frame(x=labels, c=colors)
  # prevent ggplot from sorting it alphabetically by giving order explicitly here
  # using original order of rownames. Reversing it because the bar plot reverses it again
  # To yield the correct mapping between colors and labels the colors have to be also reversed
  df$x<-factor(df$x,levels=rev(labels)) 
  # create bar plot
  p1 <- ggplot(data=df, aes_string(x="x")) + geom_bar(fill=rev(df$c)) + coord_flip() + theme(axis.title.x = element_blank(),axis.title.y = element_blank()) 
  #print(p1)
  return(p1)
}