#' nice_colors
#' 
#' Nice Color Schemes
#' 
#' 
#' @usage nice_colors(style="contrast_area",saturation=1,alpha=1,value=1)
#' @param style pre-defined Color-Scheme.  "contrast_area" returns
#' partial-complementary colors, with reduced saturation with rising number of
#' colors. This is useful for area-diagramms, where the focus of the eye shall
#' be forced to the bottom of the diagramm.
#' @param saturation saturation (0-1)
#' @param alpha Transparency (Attention, there are problems with certain
#' graphic devices or printers!
#' @param value Brightness. If brightness is part of the color-scheme, value is
#' a reduction factor of existing brightness.
#' @return Vector of Hexa-Format of colors.
#' @author Benjamin Bodirsky
#' @examples
#' \dontrun{
#' pie(1:12,col=nice_colors())
#' }
#' @export
#' @importFrom grDevices col2rgb rgb2hsv hsv rgb 
#' 
nice_colors<-function(style="contrast_area",saturation=1,alpha=1,value=1) {
  if (style=="contrast_area") {
    farbe<-c("#00BAFF","#F24200","#00E539","#D900BC","#B5BF00","#000FA6","#8C5400","#007362","#730002","#175900","#1F0040","#261E00")  
    RGBColors <- col2rgb(farbe)
    HSVColors <- rgb2hsv(RGBColors)
    HSVColors[2,]<-HSVColors[2,]*saturation
    HSVColors[3,]<-HSVColors[3,]*value
    farbe<-hsv(HSVColors[1,],HSVColors[2,],HSVColors[3,])
    RGBColors <- col2rgb(farbe) 
    farbe<-rgb(RGBColors[1,],RGBColors[2,],RGBColors[3,],alpha=alpha*255,maxColorValue = 255)   
    return(farbe)
  } else {stop("color style not known")}
}
