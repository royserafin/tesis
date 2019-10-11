##################################################
##################################################
## General distance functions
##################################################
##################################################

google_keys <-  readLines('google_keys.key')
this_key    <- 1

## ----------------------------------------
## Handles Queries
## Parámetros:
## base: link base de api, ejemplo: "https://maps.googleapis.com/maps/api/directions/json?"
## Función que hace requests necesarias al API especificada por el parámetro 'base'
## Nota: la base debe contener los parámetros especificados por la API para hacer
## la petición HTTP
## Returns
## ----------------------------------------
handle_queries <- function(base, key){
  url      <- paste(base,
                    google_keys[key],
                    sep = "&key=")
  #print(url)
  resp <- GET(url)
  if(resp$status_code == 403){
    print("ERROR 403")
    return(-1)
  }else if( resp$status_code == 200){
    res <- content(resp, as = "text")
    res <- gsub("([\\])","", res)
    res  <- RJSONIO::fromJSON(res)
    if(res$status != "OVER_QUERY_LIMIT"){
      return(res)
    } else {
      key <- key + 1
      if(key <= length(google_keys)){
        handle_queries(base, key)
      }else{
        print('NO MORE QUERIES!!!')
        return(-1)
      }
    }
  }else{
    print ("Otro codigo ")
    print ( resp$status_code)
    return(-1)
  }
}

##-------------------------------------
## Función que calcula la distancia carretera entre dos puntos dados
## usando la API de Google directions.
## Parámetros:
## origen - punto geográfico en formato (latitud, longitud)
## destino - punto geográfico en formato (latitud, longitud)
## Returns:
## Distancia en metros
##-------------------------------------
distancia_origen_destino <- function(origin, destiny, distance_matrix){
  
  mode <- 'driving'
  ## Checa si la distancia ya está registrada en 'distance_matrix'
  key_part1 <- paste(origin, collapse = ",")
  key_part2 <- paste(destiny, collapse = ",")
  key_1     <- paste(key_part1, key_part2, sep = "->")
  key_2     <- paste(key_part2, key_part1, sep = "->")
  if (!is.null(distance_matrix[[key_1]] )) {
    return (distance_matrix[[key_1]])
  }
  if (!is.null(distance_matrix[[key_2]])) {
    return (distance_matrix[[key_2]])
  }
  if (is.na(destiny[1]) || is.na(origin[1])) {
    return(-1)
  }
  ## Default distance value
  distance <- -1
  ## Creación de base para mandar a llamar a función handle_queries
  base        <- "https://maps.googleapis.com/maps/api/directions/json?"
  origin_str  <- paste0("origin=", key_part1)
  destiny_str <- paste0("destination=", key_part2)
  mode        <- paste0("mode=", mode)
  query       <- paste(base, origin_str, destiny_str, mode, sep = "&")
  # Llamado a función handle_queries, que hace petición a API
  #print(query)
  resp        <- handle_queries(query, 1)
  if(length(resp) > 1){ # si la API otorga respuesta
    
    if(resp$status == "OK"){ # si se obtiene respuesta no vacía por parte de la API,
      # se extrae el parámetro de respuesta
      distance <- resp$routes[[1]]$legs[[1]]$distance$value
    } 
    # else { # si se obtiene respuesta vacía, se calcula distancia de Haversine
    #   distance <- distm (c(origin[,2], origin[,1]),
    #                      c(destiny[,2], destiny[,1]),
    #                      fun = distHaversine)[1]
    # }
  }
  # else { # si la API no otorga respuesta
  #   distance <- distm (c(origin[,2], origin[,1]),
  #                      c(destiny[,2], destiny[,1]),
  #                      fun = distHaversine)[1]
  # }
  if(distance >= 0) {
    #assign(key_1, distance, pos = distance_matrix)
    #print("Registra distancia")
    #distance_matrix[[key_1]] <- distance
    distance_matrix[[key_1]] <- distance
  } else {
    print("No se pudo calcular distancia")
    ##print(c(origin,destiny))
  }
  #print(distance)
  return(distance)
  #distance
}


distancias_carreteras <- function(puntos, distance_matrix){
  ## Función que calcula distancias carreteras para pares origen-destino
  ## Parámetros:
  ## puntos - tabla de datos origen-destino con columnas en el formato:
  ##          lat_origen | lon_origen | lat_destino | lon_destino
  return(apply(origenes_destinos, 1, function(t){
    distancia_origen_destino(t[1:2], t[3:4], distance_matrix)
  } ))
  
}


## ----------------------------------------
## Get Distance To Road
## ----------------------------------------
distance_to_road <- function(point_, road_hash_){
    ## ----------------------------------------
    ## This function uses Google's API Roads to
    ## calculate the nearest road to a given point.
    ## point = geografic point in (lat, lon) format (vector)
    ## Sets the result distance in 'road_hash_'
    ## Returns: distance from 'point_' to nearest road
    ## ----------------------------------------
    if (! with_real_distance){
      return 
    }
    point_key <- paste(point_, collapse = ",")
    if (!is.null(road_hash_[[point_key]] )) {
      return (road_hash_[[point_key]])
    }
    
    res   <- -1
    base  <- "https://roads.googleapis.com/v1/nearestRoads?"
    point <- paste0("points=", paste(point_, collapse = ","))
    query <- paste0(base, point)
    resp  <- RJSONIO::fromJSON(handle_queries(query))
    if(length(resp) > 0 && length(resp$snappedPoints) > 0){
        nearest_road <- resp$snappedPoints[[1]]$location
        res          <-  distGeo(point_[2:1],
                                as.numeric(nearest_road)[2:1])
        road_hash_[[point_key]] <- res
    }
    res
}

## ----------------------------------------
## Get Distance To Road (bulk)
## Function that returns the distance to the nearest road
## for a series of points and sets it as a hash in the 'road_hash_' matrix
## ----------------------------------------
distances_to_road <- function(points, road_hash_){
    apply(points, 1, function(t) t <- distance_to_road(t, road_hash_))
}


##-------------------------------------
## get distance
##-------------------------------------
get_num_distance <- function(origin, destiny, distance_matrix_, mode = 'driving', with_real_distance){
    ##-------------------------------------
    ## This function uses Google's API directions to
    ## calculate the driving distance between two given points.
    ## origin  = geografic point in (latitude, longitude) format
    ## destiny = geografic point in (latitude, longitude) format
    ## returns -1 if some point is null
    ## returns distance in meters (driving distance if 'with_real_distance' is 
    ## set, in other case returns Haversine distance)
    ## sets distance in distance_matrix_ environment
    ##-------------------------------------
    if (!with_real_distance){
      distance <- distm (c(origin[,2], origin[,1]),
                         c(destiny[,2], destiny[,1]),
                         fun = distHaversine)[1]
      print("Entra aquí 1")
      return (distance)
    } else {
      return (distancia_origen_destino(origin, destiny, distance_matrix_))
    }
    
    #distance
}

## ------------------------------------
## vanilla clusterize
## ------------------------------------
vanilla_get_clusters <- function(points, centers, mode = 'driving', distance_matrix_, with_real_distance=TRUE){
    ## Weights
    weights      <- points[,3]/sum(points[,3])
    clust_assign <- c()
    for(i in 1:nrow(points)){
        ## Init Distances
        cent_dist <- plyr::dlply(centers, 1,
                           function(t) t <- get_num_distance(origin  = points[i,2:1],
                                                            destiny = t,
                                                            distance_matrix_ = distance_matrix_,
                                                            mode = 'driving',
                                                            with_real_distance = with_real_distance
                                                            )/weights[i]
                           )
        clust_assign[i] <- which(cent_dist == min(unlist(cent_dist)))[1]
    }
    clust_assign
}

## ------------------------------------
## vanilla update centers
## ------------------------------------
vanilla_update_centers <- function(points, assign){
    points$assign  <- assign
    p              <- data.table(points)
    centers        <- p[, list('lat'= mean(lat),
                              'lon'= mean(lon)),
                       by = assign]
    centers$assign <- NULL
    centers
}


##-------------------------------------
## Vanilla Distance
##-------------------------------------
vanilla_k_means <- function(points, n_centers,
                           mode  = 'driving',
                           distance_matrix_,
                           max_iter = 100){
    ## ----------------------------------------
    ## points  = lon, lat, pob
    ## centers = n_centers
    ## No need for distance_matrix...changes
    ## every time
    ## ----------------------------------------

    ## Initial Centers
    centers <- data.frame('lat' = sample(seq(min(points$lat),
                                            max(points$lat),
                                            by = .001), n_centers),
                         'lon' = sample(seq(min(points$lon),
                                            max(points$lon),
                                            by = .001), n_centers)
                         )
    ## Initial Assignment
    clust_assign <- vanilla_get_clusters(points,
                                        centers,
                                        mode,
                                        distance_matrix_)
    iters        <- 1
    repeat{
        centers        <- vanilla_update_centers(points, clust_assign)
        n_clust_assign <- vanilla_get_clusters(points,
                                              centers,
                                              mode,
                                              distance_matrix_)
        if(n_clust_assign == clust_assign || iters >= max_iter){
            break
        }
        ## Update values
        clust_assign   <- n_clust_assign
        iters          <- iters + 1
    }
    ## Return results
    list('centers' = centers, 'clusts' = n_clust_assign)
}

##-------------------------------------
## get distance matrix
##-------------------------------------
get_distance_matrix <- function(points, distance_matrix_, mode = 'driving', coords_cols = 2:1, with_real_distance=TRUE){
  ##-------------------------------------
  ## This function uses Google's API directions to
  ## calculate the driving distance between each point.
  ## points  = geografic points in (latitude, longitude) format
  ## RETURNS:
  ## 1.- Upper triangular matrix with driving distance
  ## between the points.
  ## 2.- Tree Matrix
  ##-------------------------------------
  ## Tree Matrix
  tree_matrix <- c()
  ## Distance Matrix
  dist_matrix <- matrix(nrow = length(points[[1]]),
                        ncol = length(points[[1]]))
  ## Fill in matrices
  for(i in 1:(length(points[[1]]) - 1)){
    for(j in (i + 1):length(points[[1]])){
      ## Distance Matrix
      if (points[i,coords_cols][[coords_cols[1]]] == points[j,coords_cols][[coords_cols[1]]] &&
          points[i,coords_cols][[coords_cols[2]]] == points[j,coords_cols][[coords_cols[2]]]
      ) {
        next
      }

      dist_matrix[i, j] <- get_num_distance(points[i, coords_cols],
                                            points[j, coords_cols],
                                            distance_matrix_ ,
                                            mode, 
                                            with_real_distance)
      dist_matrix[j, i] <- dist_matrix[i, j]
      ## Tree Matrix
      tree_matrix         <- rbind(tree_matrix,
                                   data.frame(
                                     'x'    = points[i, 2],
                                     'y'    = points[i, 1],
                                     'xend' = points[j, 2],
                                     'yend' = points[j, 1],
                                     'p'    = dist_matrix[i, j]
                                   ))
      ## Make sure of simmetry
      tree_matrix         <- rbind(tree_matrix,
                                   data.frame(
                                     'xend'  = points[i, 2],
                                     'yend'  = points[i, 1],
                                     'x'     = points[j, 2],
                                     'y'     = points[j, 1],
                                     'p'     = dist_matrix[i, j]
                                   ))
    }
  }
  ## Diag = 0
  diag(dist_matrix) <- 0
  ## Dissimilarity object
  dist_matrix <- as.dist(dist_matrix)
  ## Result
  result <- list()
  result[[1]] <- dist_matrix
  result[[2]] <- tree_matrix
  ## Return
  result
}


##-------------------------------------
## Prim
##-------------------------------------
## G = [x, y, xend, yend, p]
prim <- function(G){
  ## Generate set of vertex
  G$id_o  <- paste0(G[,1],G[,2])
  G$id_d  <- paste0(G[,3],G[,4])
  nodes   <- unique(G$id_o)
  n_nodes <- length(nodes)
  node_0  <- sample(length(nodes),1)
  V0      <- nodes[node_0]
  T       <- c()
  k       <- 1
  ## Start iteration
  while(k <= (n_nodes - 1)){
    candidates <- dplyr::filter(G, id_o %in% V0 & !(id_d %in% V0))
    enter      <- candidates[which(candidates$p == min(candidates$p))[1], ]
    T          <- rbind(T, enter)
    V0         <- c(V0, enter$id_d)
    k          <- k + 1
  }
  T
}




#####################################################################
###############            PLOT FUNCTIONS             ###############
#####################################################################

plot_init_cluster <- function (points){
  factpal <- colorFactor(
    palette = c('red', 'blue', 'purple'),
    domain = points$cluster
  )
  #max_value <- max(points$pob)
  map <-leaflet(data= points) %>% addProviderTiles(providers$CartoDB.Positron) #addTiles(group = "OSM(default)")

  for(i in unique(points$cluster)){
    data_clust <- dplyr::filter(points, cluster == i)
    this_pob   <- sum(data_clust$pob)
    ch <- chull(data_clust)
    map<-addPolygons(map,data = data_clust[c(ch, ch[1]),],   
                     opacity = 0,
                     lng = ~lon, 
                     lat = ~lat, 
                     weight = 4,
                     popup = ~as.character(this_pob),
                     label = ~as.character(this_pob)
                     )
  }
  map <-  addCircleMarkers(map,
      radius = 3 ,#~lapply(pob, function(x){range_quantil(x,max_value)}),
      color = ~factpal(cluster),
      stroke = FALSE, 
      fillOpacity =1,
      opacity=0.6,
      popup = ~as.character(nom_loc),
      label = ~as.character(nom_loc)
    )
  return (map)
}

mark_as_connected_plot <- function (last_plot, tree) {
  for(i in 1:nrow(tree)){

    last_plot <- addCircleMarkers(last_plot, lat =as.numeric(tree[i, c(1)]), 
                            lng = as.numeric(tree[i, c(2)]),
                            radius =3, color= "green", fillOpacity =1, opacity =1
    ) 
    last_plot <- addCircleMarkers(last_plot, lat =as.numeric(tree[i, c(3)]), 
                            lng = as.numeric(tree[i, c(4)]),
                            radius =3, color= "green", fillOpacity = 1, opacity =1
    )
    
    last_plot <- addCircles(last_plot, lat =as.numeric(tree[i, c(3)]), 
                                    lng = as.numeric(tree[i, c(4)]),
                                    radius =1000, color= "#66ff99", fillOpacity = 0.3, opacity = 0.3
    )
    last_plot <- addCircles(last_plot, lat =as.numeric(tree[i, c(1)]), 
                            lng = as.numeric(tree[i, c(2)]),
                            radius =1000, color= "#66ff99", fillOpacity =0.3, opacity = 0.3
    ) 
    
      last_plot <- addPolylines(last_plot, lat = as.numeric(tree[i, c(1, 3)]),  weight = 3,
                                opacity = 3,
                                lng = as.numeric(tree[i, c(2, 4)]), color = "black")
     

    
  }
  
    return (last_plot) 
}
add_tree_plot <- function (last_plot, points, tree, only_one_point=FALSE, iter_index = 0, with_labels=FALSE) {
  if (only_one_point){
    last_plot <- addCircleMarkers(last_plot, lat =points$lat, lng = points$lon,
                                  radius =3, color= "green", fillOpacity = 1, opacity = 1,
                                  popup = ~as.character(points$nom_loc))
    
  }else {
   
    color <- colorRampPalette(c( "#ff9933","#ff5050","#990033","#ffff00"))((iter_index %% 4)+1)
    for(i in unique(points$cluster)){
      data_clust <- dplyr::filter(points, cluster == i)
      this_pob   <- sum(data_clust$pob)
      if (nrow(data_clust) ==2 ){#Create a line with white color 
        #last_plot <- addPolylines(last_plot, lat = as.numeric(data_clust[c(1,2), 2]), 
        #                          lng = as.numeric(data_clust[c(1,2), 1]), color = color)
      }else if (nrow(data_clust) == 1) {
        last_plot <- addCircleMarkers(last_plot, lat =data_clust$lat, lng = data_clust$lon,
                                      radius =1, color= color, fillOpacity = 1, opacity = 1)
      }else {
      ch <- chull(data_clust)
      last_plot<-addPolygons(last_plot,data = data_clust[c(ch, ch[1]),],   
                             opacity = 0,
                             lng = ~lon, 
                             lat = ~lat, 
                             weight = 4,
                             popup = ~as.character(this_pob),
                             label = ~as.character(this_pob),
                             color = color)
      }
    }
    
    
    
  }
  return (last_plot)
}

