

#' nitrogen_sankey
#' 
#' ...
#'
#' @param x ...
#' @param svg_in ...
#' @param svg_out ...
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' ...
#' }
#' 
#' @export
#' @importFrom xml2 read_xml xml_find_all xml_attr write_xml xml_attr<-
#'
#'  
nitrogen_sankey<-function(x,svg_in,svg_out="sankey.svg"){
  svg<-read_xml(svg_in)
  svg2<-xml_find_all(svg, "//svg:g")
  paths<-xml_find_all(svg2, ".//svg:path")
  
  xml_attr(paths,attr="id")
  
  setstyle<-function(id,value,unit="px",style="stroke-width"){
    path=paths[which(xml_attr(paths,attr="id")==id)]
    text = xml_attr(path,attr="style")
    text=strsplit(text,split = ";")[[1]]
    pos<-which(substring(text,first = 0,last=nchar(style))==style)
    text[pos]<-paste0(style,":",value,unit)
    text<-paste(text,collapse=";")
    xml_attr(path,"style")<-text
  }
  
  setstyle(id="pfeil1",value=x,unit="px",style="stroke-width")
  write_xml(x = svg,file = svg_out)
}

