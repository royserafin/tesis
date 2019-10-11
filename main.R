########################################
########################################
########################################
## = Autores
##
## - Luis Manuel Román García
## - Miguel Alonso Vilchis
##
## -------------------------------------
## = Descripción
##
## Código para construcción de red
## basada en distancias carreteras.
##
########################################
########################################
########################################

## Uncomment if running main from different directory: 
## setwd(getSrcDirectory(function(x) {x}))

########################################
## Libraries
########################################
source("00-load.R")
source("01-utils.R")
source("02-cluster.R")

###############################################
##########         MAIN        ################
###############################################
##Ensure reproducible simulations
set.seed(123454321)
##
data        <- read.csv("./data/data_censo.csv",
                       stringsAsFactors = FALSE)
## data[,1]    <- NULL
names(data) <- c("ent",
                "mun",
                "nom_mun",
                "nom_loc",
                "lon",
                "lat",
                "pob")

## Work with Chiapas
## 7 <- chis
# chis_mun    <- dplyr::filter(data, ent == 7 & mun %in% c(107,108)  )
# Encoding(chis_mun$nom_loc) <- "UTF-8"
# chis_mun$nom_loc <- iconv(chis_mun$nom_loc, "UTF-8", "UTF-8",sub='')
# chis_points <- dplyr::select(chis_mun, lon, lat, pob,nom_loc)

municipios_sanluis    <- dplyr::filter(data, ent == 24 & mun %in% c(4,30,55)  )
Encoding(municipios_sanluis$nom_loc) <- "UTF-8"
municipios_sanluis$nom_loc <- iconv(municipios_sanluis$nom_loc, "UTF-8", "UTF-8",sub='')
chis_points <- dplyr::select(municipios_sanluis, lon, lat, pob,nom_loc)

if (!file.exists("distance_matrix.RData")) {
    distance_matrix <- new.env(hash = TRUE)
}else {
    attach("distance_matrix.RData")
}

if (!file.exists("road_hash.RData")) {
  road_hash <- new.env(hash = TRUE)
}else {
  attach("road_hash.RData")
}

data              <- chis_points
distance_matrix_  <- distance_matrix
road_hash_        <- road_hash

min_pop_centroids <-  c(5000,725,100,50)
mode              <- 'driving'
plot_with_labels <- FALSE
show_history_plot <- TRUE
with_real_distance <- TRUE
build_with_road   <- FALSE
min_pop_criterion <- c(FALSE,TRUE)
pop_criterion <- min_pop_criterion


## --------------------------------------------------
## Using differents parameters
## --------------------------------------------------

run_test <- function(pop_criterion) {
  #Constants
  min_pop_centroids <-  c(10000,5000,2500,750)
  mode              <- 'driving'
  plot_with_labels <- FALSE
  show_history_plot <- TRUE
  without_road <- iterative_clustering(data,
                               distance_matrix_,
                               road_hash_,
                               #min_pop_centroids = min_pop_centroids,
                               min_pop_criterion = pop_criterion,
                               mode = mode,
                               build_with_road = FALSE,
                               plot_with_labels = plot_with_labels,
                               show_history_plot = show_history_plot,
                               with_real_distance = TRUE)
  performance_data <- rbind(c(0,0),data.frame(net = without_road$net, pob = without_road$pob))
  plot <- ggplot(data=performance_data, aes(x = net , y = pob))+
          geom_point(size = 2, alpha = .7) +
          theme(panel.background = element_blank(),
          axis.title   = element_text(face = "bold",color = "#434343"),
          legend.title = element_text(face = "bold",color = "#424242"),
          panel.grid.major = element_line(colour = "#BDBDBD",
                                          linetype = "dotted"),
          panel.grid.minor = element_line(colour = "#E0E0E0",
                                          linetype = "dotted")) +
          ylab("Population ")+xlab("Network length (m)")+ ggtitle("Performance graph")+
          geom_line()+
          geom_hline(yintercept = without_road$pob_partition,
                color="red")
  
    return (list("plot"=without_road$plot, "pob" = without_road$pop, "net" = without_road$net, "performance"=plot ))
}  
max_min_min <- run_test(c(FALSE,TRUE))
max_min_min$plot


max_pop_always  <- run_test(c(FALSE))
max_pop_always$plot



min_pop_always   <- run_test(c(TRUE))
min_pop_always$plot

ggsave('./Imagenes/final_with_towers.png')



## --------------------------------------------------
## Save hash table
## --------------------------------------------------
save(distance_matrix,
     file = "distance_matrix.RData")
save(road_hash,
     file = "road_hash.RData")

###############################################
##########         MAPS        ################
###############################################
register_google("AIzaSyCZdE992xxbW9ub0vTXENECHUW6gHsk1QY")
map <- get_map(location = c(lon = mean(chis_points$lon),
                            lat = mean(chis_points$lat)),
               zoom = 11,
               maptype = "roadmap",
               source = "google")
ggmap(map) +
    geom_point(data = chis_points,
               aes(x = lon,
                   y = lat,
                   size = pob),
               alpha = .4,
               color = '#41006D') +
    scale_size(range = c(1, 10)) +
    theme(
        panel.background = element_blank(),
        axis.title = element_text(face = 'bold',
                                  colour = 'black',
                                  size = 0),
        axis.text = element_text(face = 'bold',
                                 colour = '#484848',
                                 size = 6),
        axis.title.y = element_text(margin = margin(t = 0,
                                                    r = 20,
                                                    b = 0,
                                                    l = 0)),
        axis.title.x = element_text(margin = margin(t = 20,
                                                    r = 0,
                                                    b = 0,
                                                    l = 0))
    ) + xlab('Month') + ylab('Top sellers - Top devs') +
    guides(size = guide_legend(title="Population"))
ggsave('./Imagenes/pop_dist.png')


