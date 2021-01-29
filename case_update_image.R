
# Export image file ####


#install.packages('jpeg')
library(jpeg)
wscc_logo <- readJPEG(paste0(github_repo_dir, '/images/west sussex logo.jpg'))

down_img <- readPNG(paste0(github_repo_dir, '/images/double-down-arrows green.png'))
up_img <- readPNG(paste0(github_repo_dir, '/images/double-up-arrow red.png'))
same_img <- readPNG(paste0(github_repo_dir, '/images/equals-sign.png'))

# This will be the coordinate system for placing our objects in grid later
vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)

# might have to wrap jpeg call arround the newpage
# 
# #install.packages('extrafont')
library(extrafont)
loadfonts( device = 'win')

jpeg(paste0(output_directory_x, '/Daily_infographic_socials.jpg'), 
     width = 9, 
     height = 4, 
     units = "in",
     # pointsize = 14,
     res = 240, 
     quality = 100)

grid.newpage()
pushViewport(viewport(layout = grid.layout(30, 20))) 

grid.rect(gp = gpar(fill = "#0071B6", col = "#0071B6"))

grid.rect(x = unit(0.04, "npc"), 
          y = unit(0.9, "npc"), 
          width = unit(.17, "npc"), 
          height = unit(0.06, "npc"), 
          just = "left", 
          default.units = "npc", 
          gp=gpar(fill = "#F8E821", col = "#F8E821"), 
          draw = TRUE, 
          vp = NULL)

grid.text('CORONAVIRUS',
          just = "left",  
          x = unit(0.05, "npc"), 
          y = unit(.9, "npc"), 
          gp = gpar(col = "#0071B6", 
                    fontsize = "12", 
                    fontface = "bold",
                    fontfamily = 'Verdana'))

grid.text('COVID-19 CASE UPDATE',
          just = "left",  
          x = unit(0.04, "npc"), 
          y = unit(0.82, "npc"), 
          gp = gpar(col = "#ffffff", 
                    fontsize = "30", 
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))

grid.roundrect(x = unit(0.04, "npc"), 
          y = unit(0.61, "npc"), 
          width = unit(0.2, "npc"), 
          height = unit(0.24, "npc"), 
          just = "left", 
          default.units = "npc", 
          gp=gpar(fill = "#ffffff",
                  col = "#ffffff"), 
          vp = NULL)

grid.text('New confirmed cases',
          just = "centre",  
          x = unit(0.14, "npc"), 
          y = unit(0.7, "npc"), 
          gp = gpar(col = "#000000", 
                    fontsize = "10", 
                    fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text('this week*',
          just = "centre",  
          x = unit(0.14, "npc"), 
          y = unit(0.66, "npc"), 
          gp = gpar(col = "#000000", 
                    fontsize = "10", 
                    fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(format(cases_this_week$Rolling_7_day_new_cases, big.mark = ','),
          just = "centre",  
          x = unit(0.14, "npc"), 
          y = unit(0.58, "npc"), 
          gp = gpar(col = "#000000", 
                    fontsize = "35", 
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))

grid.roundrect(x = unit(0.28, "npc"), 
          y = unit(0.61, "npc"), 
          width = unit(.2, "npc"), 
          height = unit(0.24, "npc"), 
          just = "left", 
          default.units = "npc", 
          gp=gpar(fill = "#ffffff",
                  col = "#ffffff"), 
          vp = NULL)

grid.text('Change since',
          just = "centre",  
          x = unit(0.38, "npc"), 
          y = unit(0.7, "npc"), 
          gp = gpar(col = "#000000", 
                    fontsize = "10", 
                    fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text('last week',
          just = "centre",  
          x = unit(0.38, "npc"), 
          y = unit(0.66, "npc"), 
          gp = gpar(col = "#000000", 
                    fontsize = "10", 
                    fontfamily = 'Verdana',
                    fontface = 'bold'))

if(change_direction_between_weeks == 'DOWN'){
grid.raster(down_img, 
            x = unit(0.29, "npc"),
            y = unit(0.58, "npc"), 
            just = 'left',
            width = .04)

grid.text(format(change_between_weeks, big.mark = ','),
          just = "left",  
          x = unit(0.34, "npc"), 
          y = unit(0.58, "npc"), 
          gp = gpar(col = "#000000", 
                    fontsize = "33", 
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))  

grid.text('Falling',
          just = "left",  
          x = unit(0.29, "npc"), 
          y = unit(0.52, "npc"), 
          gp = gpar(col = "#000000", 
                    fontsize = "8", 
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))  
}

if(change_direction_between_weeks == 'UP'){
  grid.raster(up_img, 
              x = unit(0.29, "npc"),
              y = unit(0.58, "npc"), 
              just = 'left',
              width = .04)
  
  grid.text(paste0('+',format(change_between_weeks, big.mark = ',')),
            just = "left",  
            x = unit(0.34, "npc"), 
            y = unit(0.58, "npc"), 
            gp = gpar(col = "#000000", 
                      fontsize = "33", 
                      fontfamily = 'Bahnschrift',
                      fontface = 'bold'))
  
  grid.text('Rising',
            just = "left",  
            x = unit(0.29, "npc"), 
            y = unit(0.52, "npc"), 
            gp = gpar(col = "#000000", 
                      fontsize = "8", 
                      fontfamily = 'Bahnschrift',
                      fontface = 'bold'))  
}

if(change_direction_between_weeks == 'SAME'){
  grid.raster(same_img, 
              x = unit(0.29, "npc"),
              y = unit(0.58, "npc"), 
              just = 'left',
              width = .04)
  
  grid.text('No change in case numbers',
            just = "left",  
            x = unit(0.34, "npc"), 
            y = unit(0.58, "npc"), 
            gp = gpar(col = "#000000", 
                      fontsize = "16", 
                      fontfamily = 'Bahnschrift',
                      fontface = 'bold'))  
}


grid.text(paste0('In the seven days to ', format(complete_date, '%A '), ordinal(as.numeric(format(complete_date, '%d'))), format(complete_date, ' %B'), ',  there were'),
          just = "left",  
          x = unit(0.04, "npc"), 
          y = unit(0.45, "npc"), 
          gp = gpar(col = "#ffffff", 
                    fontsize = "9", 
                    fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0(format(cases_this_week$Rolling_7_day_new_cases, big.mark = ','), ' new confirmed COVID-19 cases in West Sussex.'),
          just = "left",  
          x = unit(0.04, "npc"), 
          y = unit(0.41, "npc"), 
          gp = gpar(col = "#ffffff", 
                    fontsize = "9", 
                    fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(ifelse(change_direction_between_weeks == 'SAME', 'Confirmed cases remain the same as they did in the previous week.', ifelse(change_direction_between_weeks == 'DOWN', 'Confirmed cases are falling compared to the previous week.', ifelse(change_direction_between_weeks == 'UP', 'Confirmed cases are rising compared to the previous week.', NA))),
          just = "left",  
          x = unit(0.04, "npc"), 
          y = unit(0.36, "npc"), 
          gp = gpar(col = "#ffffff", 
                    fontsize = "9", 
                    fontfamily = 'Verdana',
                    fontface = 'bold'))


grid.text(paste0('Since the start of the COVID-19 pandemic, there have been'),
          just = "left",  
          x = unit(0.04, "npc"), 
          y = unit(0.3, "npc"), 
          gp = gpar(col = "#ffffff", 
                    fontsize = "9", 
                    fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0(format(total_so_far$Cumulative_cases, big.mark = ','), ' confirmed cases in West Sussex (as of ', ordinal(as.numeric(format(last_date, '%d'))), format(last_date, ' %B'),').'),
          just = "left",  
          x = unit(0.04, "npc"), 
          y = unit(0.26, "npc"), 
          gp = gpar(col = "#ffffff", 
                    fontsize = "9", 
                    fontfamily = 'Verdana',
                    fontface = 'bold'))


grid.rect(x = unit(0.5, "npc"), 
          y = unit(0.05, "npc"), 
          width = unit(1, "npc"), 
          height = unit(0.15, "npc"), 
          just = "centre",
          default.units = "npc", 
          gp=gpar(fill = "#F8E821", 
                  col = "#F8E821"), 
          draw = TRUE, 
          vp = NULL)

grid.text('PUBLICATION DATE:',
          just = "left",  
          x = unit(0.04, "npc"), 
          y = unit(0.07, "npc"), 
          gp = gpar(col = "#000000", 
                    fontsize = "18", 
                    fontfamily = 'Bahnschrift',
                    fontface = 'bold'))

grid.text(format(last_date + 1 , '%d %B %Y'),
          just = "left",  
          x = unit(0.3, "npc"), 
          y = unit(0.07, "npc"), 
          gp = gpar(col = "#0071B6", 
                    fontsize = "20", 
                    fontfamily = 'Verdana',
                    fontface = 'bold'))

grid.text(paste0('*We measure change by looking at cases in this week'),
          just = "left",  
          x = unit(0.65, "npc"), 
          y = unit(0.098, "npc"), 
          gp = gpar(col = "#000000", 
                    fontsize = "7", 
                    fontfamily = 'Verdana'))

grid.text(paste0('(seven days to ', format(complete_date, '%A '), ordinal(as.numeric(format(complete_date, '%d'))), format(complete_date, ' %B %Y'), ') compared'),
          just = "left",  
          x = unit(0.65, "npc"), 
          y = unit(0.073, "npc"), 
          gp = gpar(col = "#000000", 
                    fontsize = "7", 
                    fontfamily = 'Verdana'))

grid.text(paste0('to the last week (seven days to ', ordinal(as.numeric(format(complete_date - 7, '%d'))), format(complete_date - 7, ' %B %Y'), '). This is '),
          just = "left",  
          x = unit(0.65, "npc"), 
          y = unit(0.048, "npc"), 
          gp = gpar(col = "#000000", 
                    fontsize = "7", 
                    fontfamily = 'Verdana'))

grid.text(paste0('because data for more recent days are considered incomplete.'),
          just = "left",  
          x = unit(0.65, "npc"), 
          y = unit(0.025, "npc"), 
          gp = gpar(col = "#000000", 
                    fontsize = "7", 
                    fontfamily = 'Verdana'))

# grid.text(paste("West Sussex Public Health Outcomes (Data correct as of ", format(Sys.Date(), "%B %Y"), ")", sep = ""), just = "left",  x = unit(0.025, "npc"), y = unit(.97, "npc"), gp = gpar(col = "#ffffff", fontsize = "12", fontface = "bold"))

# Icon for the week
# grid.circle(x = 0.775, y = 0.94  , r = 0.04, default.units = "npc", name = NULL, gp = gpar(fill = '#ffffff', col = "#333333"), draw = TRUE, vp = NULL)

grid.raster(wscc_logo, 
            y = unit(0.92, "npc"), 
            x = unit(0.85, "npc"), 
            vjust = 1, 
            hjust = 0, 
            width = .12)
# grid.text("http://jsna.westsussex.gov.uk",just = "left", x = unit(0.05, "npc"), y = unit(.05, "npc"), gp = gpar(col = "#1c8ccd", fontsize = "11", fontface = "bold"))

# grid.text("Infographic images designed by Freepik and OCHA from Flaticon",just = "centre", x = unit(0.5, "npc"), y = unit(.05, "npc"), gp = gpar(col = "#333333", fontsize = "8"))

dev.off()

