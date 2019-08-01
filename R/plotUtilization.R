#' @title plotUtilization
#' @description Plot to visualize the utilisation of products for different purposes. Input should be an object derived from a mif file or similar.
#'
#' @param x A magpie or quitte object, or a list of magpie or quitte objects. 
#' @param products Products
#' @param categories Categories
#'
#' @return Plot
#' @author Benjamin Leon Bodirsky
#' @examples

#' 
#
#' \dontrun{ 
#' require(mip)
#' require(moinput)
#' kall<-reportingnames(findset("kall"))
#' budget<-calcOutput("ValidDemand",nutrient="nr",aggregate = "GLO")
#' budget2<-reportingReverse(budget,select="Agricultural demand",namedims=3, endoftree=T)
#' plotUtilization(budget2,products = kall,categories=c("Food","Feed","Processing"))
#' budget2a<-budget2[,1:5]
#' budget2b<-budget2[,3:10]*1.1
#' getNames(budget2b,dim=1)<-"projection"
#' x<-list(colSums(budget2a),colSums(budget2b))
#' plotUtilization(x,products = kall,categories=c("Food","Feed","Processing"))
#' plotUtilization(x,products = findset("kall_agg1"),categories=c("Food","Feed","Processing"))
#' plotUtilization(x,products = findset("kall_agg2"),categories=c("Food","Feed","Processing"))
#' productsx=setdiff(findset("kall_agg1"),c("pasture","kres","foddr","bioenergycrops"))
#' plotUtilization(x,products = productsx,categories=c("Food","Feed","Processing"))
#' a<-calcOutput("ValidCroparea",detail=T)
#' a<-reportingReverse(a,select="Land Cover",namedims=3, endoftree=T)
#' plotUtilization(colSums(a),products = kall,categories = "Cropland")
#' }
#' 
#' @export
#' @importFrom magclass is.magpie fulldim
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom magpiesets findset reportingnames



plotUtilization<-function(x,products="kall",  
                          categories=c("Food","Feed","Processing",
                                       "Seed","Supply Chain Loss",
                                       "Material","Domestic Balanceflow")){
  order<-c("Crops",
              "Cereals",
                "Temperate cereals","Tropical cereals","Rice","Maize",
              "Oil crops",
                "Oilpalms","Groundnuts","Sunflower","Rapeseed","Pulses","Soybean",
                "Cotton",
              "Other crops",
                "Fruits Vegetables Nuts",
                "Tropical roots","Potatoes",  
              "Sugar crops",
                "Sugar beet","Sugar cane",
           "Livestock products",
              "Poultry meat","Eggs","Ruminant milk","Pig meat","Ruminant meat",
           "Fish",
           "Secondary products",
              "Oils","Sugar","Alcoholic beverages",
              "Oilcakes","Molasses","Brans","Distillers grains",
              "Microbial protein",
              "Ethanol","Cotton lint",
           "Crop residues",
              "Straw","Other fibrous crop residues","Non fibrous crop residues",
           "Forage",
           "Pasture",
           "Bioenergy crops",
              "Short rotation grasses","Short rotation trees"
    )
  
  products <- findset(products, noset="original")
  
  if(!all(products%in%order)){
    products<-reportingnames(products)
  }
  
  if(!all(products%in%order)){stop(paste(
    "Products unknown. Order list hast to be updated, does not contain all products. Missing products:",
    products[!products%in%order],
    collapse=TRUE))
  }
  #sort products
  products<-order[which(order%in%products)]
  
  if(is.magpie(x)){
    x.plot<-quitte::as.quitte(x)
  } else if (quitte::is.quitte(x)){
    x.plot <- x
  } else if(is.list(x)){
    x.plot<-NULL
    for (i in 1:length(x)){
      if(is.magpie(x[[i]])){
        check<-length(fulldim(x[[1]])[[1]])
        if((length(fulldim(x[[i]])[[1]]))!=check){stop(paste("List element ",i," of object x has a different dimensionality than the others"))}
      }
      x.plot<-rbind(x.plot,quitte::as.quitte(x[[i]]))
    }
  } 
  
  dimnames(x.plot)[[2]]=c("model","scenario","region","variable","unit","period","value","data2","data3")
  x.plot<-x.plot[which(x.plot$data2%in%categories),]
  x.plot<-x.plot[which(x.plot$data3%in%products),]
  
  x.plot$data3<-factor(x.plot$data3,levels = products[products%in%levels(x.plot$data3)])
  x.plot$data2<-factor(x.plot$data2,levels = categories[categories%in%levels(x.plot$data2)])
  
  x.plot<-x.plot[order(x.plot$data2),]
  x.plot<-x.plot[order(x.plot$data3),]
  x.plot$data1<-x.plot$data2
  x.plot$data2<-x.plot$data3
  x.plot$data3<-NULL
  
  colorsout<-reportingnames(products,from = "reportingnames",to = "reportingcolors")
  p <- mipAreaOverlapFirstNameFacetSecondNameStacked(x.plot,alpha=0.7)
  p <- p + scale_fill_manual(values=c(colorsout))
  return(p)
}  