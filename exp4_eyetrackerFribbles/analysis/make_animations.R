frames <- 20

rename <- function(x){
  if (x < 10) {
    return(name <- paste('000',i,'plot.png',sep=''))
  }
  if (x < 100 && i >= 10) {
    return(name <- paste('00',i,'plot.png', sep=''))
  }
  if (x >= 100) {
    return(name <- paste('0', i,'plot.png', sep=''))
  }
}



for (i in 1:length(timeSeq)){
  name <- rename(i)
  eyetracker[eyetracker$screen_index=="feature" & eyetracker$time <= timeSeq[i],]-> data2
  
  ggplot(data2, aes(x, y))+
    xlim(0,1)+
    ylim(0,1)+
    facet_wrap( label ~ frequency)+
    geom_point()+
    stat_density2d(aes(fill = ..level..), alpha = .7, geom = "polygon", contour = T)+
    scale_fill_distiller(palette = "Spectral", direction = -1)+
    geom_density_2d(size = 0.25, colour = "black")
  ggsave(name, dpi=300)

}

list.files(path=getwd(), pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("FileName.gif") # write to current dir
