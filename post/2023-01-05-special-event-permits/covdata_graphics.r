

#` colors for styling data in graphs (points, bars, etc.)
covington_colors <<- c("Covington.Blue" = "#0047ba",
                    "Covington.Navy" = "#211261",
                    "Covington.Dark.Blue" = "#000b8c",
                    "Covington.Light.Blue" = "#00c1de",
                    "Covington.Green" = "#38d430",
                    "Covington.Red" = "#ea0029",
                    "Covington.Yellow" = "#ffc600",
                    "Covington.Black" = "#000000",
                    "Covington.Gray" = "#878787",
                    "Covington.Light.Gray" = "#cccccc",
                    "Covington.Plain" = "#ffffff",
                    "Covington.OG.Blue" = "#46b5d2")


#` function to assign color from 'covdata_colors' vector to geoms in graph
covdata_color <<- function(...) {
  cols <- c(...)
  if (is.null(cols)) {
    return (covington_colors)
    
  } else
  covington_colors[cols]
}

#covdata_color("Covington.Plain")


# ggplot color palettes

covdata_palette_colors <<- list(
  Brand = c("#1100ff", "#030055", "#09008a", "#00c4f7", "#00e900", "#ff0000", "#ffc500"),
  Pantones = c("#0047ba", "#211261", "#000b8c", "#00c1de", "#38d430", "#ea0029", "#ffc600"),
  Neutrals = c("#000000", "#878787", "#cccccc", "#ffffff"),
  Blues = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", 
            "#4292C6", "#2171B5", "#08519C", "#08306B"),
  Greens = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"),
  Reds = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#A50F15", "#67000D"),
  Greys = c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373", "#525252", "#252525", "#000000"),
  YlOrRd = c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026"),
  YlGnBu = c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58"),
  RdYlBu = c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", 
             "#FFFFBF", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"),
  RdBu = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7",
           "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"),
  Spectral = c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", 
               "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2"),
  Paired = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
             "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
             "#FFFF99", "#B15928"),
  Set3 = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3",
           "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD",
           "#CCEBC5", "#FFED6F"))


# function to create a color palette for display
covdata_palettes <<- function(name, 
                            n, 
                            all_palettes = covdata_palette_colors, 
                            type = c("discrete", "continuous"),
                            reverse = FALSE) {
  palette <- all_palettes[[name]]
  if (missing(n)) {                              # return length of palette if number of colors not provided
    n = length(palette)                          # discrete type can not return additional colors in palette
  }
  
  if(reverse) palette <- rev(palette)            # reverse order of palette colors if reverse is TRUE
  
  type <- match.arg(type)
  out <- switch(type,
               continuous = grDevices::colorRampPalette(palette)(n), # set n to interpolate palette for more colors
               discrete = palette[1:n] 
  )
  
  structure(out, name = name, class = "palette")
}


# function to color with discrete color palette
covdata_color_d <<- function(name, reverse = FALSE){
  if(reverse) {
  ggplot2::scale_colour_manual(values = covdata_palettes(name,
                                                         type = "discrete",
                                                         reverse = TRUE))
  }else
  ggplot2::scale_colour_manual(values = covdata_palettes(name,
                                                         type = "discrete"))
}


# function to fill color with discrete color palette
covdata_fill_d <<- function(name, reverse = FALSE){
  if(reverse) {
    ggplot2::scale_fill_manual(values = covdata_palettes(name,
                                                           type = "discrete",
                                                           reverse = TRUE))
  }else
    ggplot2::scale_fill_manual(values = covdata_palettes(name,
                                                           type = "discrete"))
}

# function to color with continuous color palette
covdata_color_c <<- function(name, reverse = FALSE){
  if(reverse) {
    ggplot2::scale_colour_gradientn(colours = covdata_palettes(name,
                                                           type = "continuous",
                                                           reverse = TRUE))
  }else
    ggplot2::scale_colour_gradientn(colours = covdata_palettes(name,
                                                           type = "continuous"))
}

# function to color with continuous color palette
covdata_fill_c <<- function(name, reverse = FALSE){
  if(reverse) {
    ggplot2::scale_fill_gradientn(colours = covdata_palettes(name,
                                                               type = "continuous",
                                                               reverse = TRUE))
  }else
    ggplot2::scale_fill_gradientn(colours = covdata_palettes(name,
                                                               type = "continuous"))
}



# function to print one color palette and modify by type and number of colors
# calls the covdata_palettes function to create palette
covdata_palettes_print <<- function(name, n, ...){
  
  pal <- covdata_palettes(name, n, ...)
  
  show_col(pal)
  
}

#covdata_palettes_print("RdBu", n = 86, type = "continuous")

#show_col(covdata_palettes("Set3", n = 30, type = "discrete"))

#test_brand <- covdata_palettes("Set3", n = 20, type = "discrete")
#show_col(test_brand)


# function to print all color paletttes in a list
covdata_palettes_all <<- function(palette_list, palette_names){
  nr <- length(palette_list)
  nc <- max(lengths(palette_list))
  ylim <- c(0, nr)
  oldpar <- par(mgp = c(2, 0.25, 0))
  on.exit(par(oldpar))
  plot(1, 1, xlim = c(0, nc), ylim = ylim, type = "n", axes = FALSE, 
       bty = "n", xlab = "", ylab = "")
  for (i in 1:nr) {
    nj <- length(palette_list[[i]])
    shadi <- palette_list[[i]]
    rect(xleft = 0:(nj - 1), ybottom = i - 1, xright = 1:nj, 
         ytop = i - 0.1, col = shadi, border = "light grey")
  }
  text(rep(-0.1, nr), (1:nr) - 0.6, labels = palette_names, xpd = TRUE, 
       adj = 1)
}

#plot.new()

palette_list <<- list(covdata_palette_colors[[14]], 
                     covdata_palette_colors[[13]], 
                     covdata_palette_colors[[12]], 
                     covdata_palette_colors[[11]],
                     covdata_palette_colors[[10]],
                     covdata_palette_colors[[9]],
                     covdata_palette_colors[[8]],
                     covdata_palette_colors[[7]],
                     covdata_palette_colors[[6]],
                     covdata_palette_colors[[5]],
                     covdata_palette_colors[[4]],
                     covdata_palette_colors[[3]],
                     covdata_palette_colors[[2]],
                     covdata_palette_colors[[1]])


palette_names <<- rev(c("Brand", 
                   "Pantones", 
                   "Neutrals", 
                   "Blues", 
                   "Greens", 
                   "Reds", 
                   "Greys", 
                   "YlOrRd", 
                   "YlGnBu",
                   "RdYlBu",
                   "RdBu",
                   "Spectral",
                   "Paired",
                   "Set3"))

#covdata_palettes_all(palette_list, palette_names)




#` function to fill plot background
covdata_theme <<- function(plot.background.fill = c(Covington.Plain,
                                                    Covington.Black, 
                                                    Covington.Blue, 
                                                    Covington.Navy, 
                                                    Covington.Dark.Blue,
                                                    Covington.Light.Blue,
                                                    Covington.Green,
                                                    Covington.Yellow,
                                                    Covington.Gray,
                                                    Covington.Light.Gray,
                                                    Covington.OG.Blue),
                           panel.background.fill = c(Covington.Plain, 
                                                     Covington.Light.Gray, 
                                                     Covington.Gray,
                                                     Covington.Black,
                                                     Covington.Blue,
                                                     Covington.Light.Blue),
                           text.color = Covington.Black){      
  
  # only allow these colors to fill background
  
  #Color names to pass to fills 
  Covington.Blue         <<- "#0047ba"
  Covington.Navy         <<- "#211261"
  Covington.Dark.Blue    <<- "#000b8c"
  Covington.Light.Blue   <<- "#00c1de"
  Covington.Green        <<- "#38d430"
  Covington.Yellow       <<- "#ffc600"
  Covington.Black        <<- "#000000"
  Covington.Gray         <<- "#878787"
  Covington.Light.Gray   <<- "#cccccc"
  Covington.OG.Blue      <<- "#46b5d2"
  Covington.Plain        <<- "#ffffff"
  
  #assign font family up front
  font <- "Franklin Gothic Medium"                
  
  #replace elements we want to change
  theme_bw() %+replace%                          
   
    theme(
      
      #text elements
      title = element_text(color = text.color),
      
      axis.text = element_text(colour = text.color),
      
      axis.title.y = element_blank(),
      axis.title.x = element_text(margin = margin(t=8, b=15)),
      
      text = element_text(family = font),          
      
      #color fill for plot background
      plot.background = element_rect(fill = match.arg(plot.background.fill), colour = NA),
      
      #color fill for panel background
      panel.background = element_rect(fill = match.arg(panel.background.fill)),
     
      #plot margin
      plot.margin = margin(t = 0.3, r = 0.2, b = 0.3, l = 0.1, unit = "cm"),
      
      #caption position
      plot.caption = element_text(margin = margin(b=0), hjust = 1, size = 9)
      
      #will update other features as necessary
      #many elements require manual tweaking, such as legend,
      #geom aesthetics etc., so these are not part of the template
    )
}

covdata_finalize <<- function(plot, logo =  c(City.Seal.Black, 
                                              City.Seal.White, 
                                              City.Seal.Navy, 
                                              COV.Logo.Blue,
                                              COV.Logo.White,
                                              COV.Seal.Logo.White,
                                              COV.Seal.Logo.Blue), 
                              logo_scale = 0.3,
                              footer_shade = FALSE,
                              shade_color = "Covington.Gray",
                              save_name,
                              save_width = 550,
                              save_height = 450) {
  
  
  rect <- grid::rectGrob(
    x = 0.0,
    y = 0.15,
    width = 1.0,
    height = 0.15,
    hjust = 0, vjust = 1, 
    gp = gpar(fill = covdata_color(shade_color), alpha = 0.35, col = NA)) 
  
  footer_color <- cowplot::ggdraw()+
    cowplot::draw_grob(rect)
  
  City.Seal.Black       <<- "city seal_BLACK.png"
  City.Seal.White       <<- "city seal_WHITE.png"
  City.Seal.Navy        <<- "city seal_NAVY.png"
  COV.Logo.Blue         <<- "COV_Logo_Trademark.png"
  COV.Logo.White        <<- "COV_Logo_Trademark_WHITE.png"
  COV.Seal.Logo.White   <<- "COV_Seal_Logo_White.png"
  COV.Seal.Logo.Blue    <<- "COV_Seal_Logo_Blue.png"
  
  logo_file <- image_read(match.arg(logo))
  logo_sized <- image_resize(logo_file, 200)
  
  footer_logo <- ggdraw()+
    draw_image(logo_sized, x = 0.01, y = 0.01, hjust = 0, vjust = 0, halign = 0, valign = 0,
               scale = logo_scale)
  
  
  if(footer_shade == FALSE){
    
    main_plot <- cowplot::ggdraw()+
      cowplot::draw_plot(plot)+
      cowplot::draw_plot(footer_logo)
    
    main_plot
    
  }
  
  else {
    
    main_plot <- cowplot::ggdraw()+
      cowplot::draw_plot(plot)+
      cowplot::draw_plot(footer_color)+
      cowplot::draw_plot(footer_logo)
    
    main_plot
  }
  
  
  if(!missing(save_name))
    
    ggplot2::ggsave(paste(save_name, "_", lubridate::today(), ".png", sep = ""), main_plot, 
                    device = "png", width = save_width, height = save_height, unit = "px", dpi = 120)
  
  main_plot
  
}























