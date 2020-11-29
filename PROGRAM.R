rm(list = ls())

# Loading Required Information --------------------------------------------
library(purrr)

map(c("data.table", "dplyr", "stringr", 
      "ggplot2", "plotly", "tidyr", "glue", "rvest", "tidygeocoder", 
      "leaflet", "leaflet.extras", "randomcoloR", "DiagrammeR"), require, 
    character.only = TRUE)


#GENERAL
#' @description
#' @param
#' @return
volumen_correction <- function(path, result = c("table", "output")){
  
  df <- apply_model(path = path, result = "table")
  
  walk(1:6, function(i){
    
    max <- df[,.(VOL = sum(VOL_ENTREGA)), by = "LABEL"] %>% .[,max(VOL)]
    
    value <- (max-df[LABEL == i, sum(VOL_ENTREGA)])/(df[LABEL == i, .N])
    
    df[LABEL == i, VOL_ENTREGA:=(VOL_ENTREGA+value)] %>% 
      return()
  })
  
  splited <- df %>% split(f = .[,ID_CLIENTE])
  
  df <- map(splited, function(i){
    i <- i[1,] %>% .[,':='(VOL_ENTREGA=i[,sum(VOL_ENTREGA)],
                           FRECUENCIA = i[,sum(FRECUENCIA)])]
  }) %>% rbindlist() %>% 
    .[,ID_CLIENTE:=as.integer(ID_CLIENTE)] %>% setorder("ID_CLIENTE") %>% 
    .[,ID_CLIENTE:=as.character(ID_CLIENTE)] %>% 
    .[,-c("KNN", "LABEL")] %>% 
    setnames(old = names(.), 
             new = c("Id_Cliente", "id_Agencia",	"Frecuencia",	"Vol_Entrega", "lat", "lon")
    )
  
  write.csv(x = df, file = "FILES/a.csv", row.names = F)
  
  results <- map(c("table", "output"),
                 ~ apply_model(path = "FILES/a.csv", result = .x)) %>% 
    set_names("table", "output")
  
  purrr::pluck(results, result) %>% return()
}
#DESAGREGACIÓN
# d <- volumen_correction(path = "INFORMATION/FILES/ubicaciones.csv",
#                    result = "table")
# d[,sum(VOL_ENTREGA), by = "LABEL"]
# d[,mean(VOL_ENTREGA), by = "LABEL"]
# d[,sum(FRECUENCIA), by = "LABEL"]
# d[,.N, by = "LABEL"]
#d <- volumen_correction(path = "C:/Users/actje/Desktop/HACKATHON/FILES/ubicaciones.csv", result = "output")
#d <- volumen_correction(path = "C:/Users/actje/Desktop/HACKATHON/FILES/ubicaciones.csv", result = "table")
#write.csv(x = d, file = "FILES/table.csv", row.names = FALSE)

#GENERAL
#' @description
#' @param
#' @return
apply_model <- function(path, result = c("table", "output")){
  
  df <- map(c("CORRECT", "INCORRECT"), ~ desagregate_dataset(path = path,
                                                             choice = .x)) %>% 
    set_names("CORRECT", "INCORRECT")
  
  numeric_integer_variables <- c("ADJUSTED_VOL_ENTREGA", "KNN")
  
  scale_numeric_integer_variables <- str_c("SCALE", numeric_integer_variables,
                                           sep = "_")
  walk(df, function(i){
    walk2(numeric_integer_variables, scale_numeric_integer_variables,
          ~ i[,(.y):=(get(.x)-mean(get(.x)))/sd(get(.x))]
    )
  })
  
  vars_model <- c("FRECUENCIA", scale_numeric_integer_variables)
  
  df <- map2(df, c("CORRECT", "INCORRECT"),
             ~ .x[,LABEL:= kmeanSame_size(df = .x, 
                                          vars_model = vars_model, 
                                          choice = .y) %>% 
                    as.factor()] %>% 
               .[,c(scale_numeric_integer_variables, "ADJUSTED_VOL_ENTREGA"):=NULL]
  ) %>% rbindlist() %>% 
    .[,ID_CLIENTE:=as.integer(ID_CLIENTE)] %>% setorder("ID_CLIENTE") %>% 
    .[,ID_CLIENTE:=as.character(ID_CLIENTE)]
  
  if(result == "table"){
    result <- df
    
  }else if(result == "output"){
    
    vars <- c("ID_CLIENTE", "LABEL")
    
    each_label <- df %>% split(f = .[,"LABEL"])
    D_labels <- glue("D{1:6}")
    
    each_label <- map2(each_label, D_labels, ~.x[,vars, with = FALSE] %>% 
                         .[,ID_CLIENTE:=as.integer(ID_CLIENTE)] %>% 
                         setnames(old = "LABEL", new = .y))
    
    output <- iterative_merge(dfs_list = each_label, key = "ID_CLIENTE", 
                              all = TRUE)  %>% 
      setorder("ID_CLIENTE") %>% unique()
    
    walk(D_labels, 
         ~ output[,(.x):=map_chr(get(.x), ~ (if(is.na(.x)) 0 else 1)) %>% 
                    as.integer()]
    )
    output %>% setnames(old = "ID_CLIENTE", new = "Id_Cliente")
    
    result <- output
  }
  result
}
#d <- apply_model(path = "INFORMATION/FILES/ubicaciones.csv", result = "table")

#PARTICULAR 1
#' @SUBFUNCTION
#' @description
#' @param
#' @return
desagregate_dataset <- function(path, choice = c("CORRECT", 
                                                 "INCORRECT")){
  
  df <- read.csv(file = path,
                 header = TRUE,
                 na.strings = "NA") %>% as.data.table() %>%
    setnames(old = "Id_Cliente", "id_cliente") %>%
    setnames(toupper(names(.)))
  
  walk(str_subset(string = names(df), "(?:ID|LAT|LON).*"),
       ~df[,(.x):=as.character(get(.x))])
  
  df[,KNN:=Knn_DIRECTIONS(df = df)]
  
  df <- list(CORRECT = df[FRECUENCIA == 1] %>% 
               .[,ADJUSTED_VOL_ENTREGA := VOL_ENTREGA],
             INCORRECT = df[FRECUENCIA > 1])
  
  cv <- purrr::pluck(df, "INCORRECT")[,sd(VOL_ENTREGA)/mean(VOL_ENTREGA)]
  
  purrr::pluck(df, "INCORRECT") <- 
    map_df(purrr::pluck(df, "INCORRECT")[,ID_CLIENTE] %>% unique(), function(i){
      
      each <- purrr::pluck(df, "INCORRECT")[ID_CLIENTE == i]
      
      if(every(purrr::pluck(df, "INCORRECT")[,FRECUENCIA], ~.x == 1)) each
      else{
        each <- map_df(1:each[,FRECUENCIA], ~ each) %>% as.data.table() %>%
          .[,FRECUENCIA:=1]
      }
      if(each[,.N] == 2){
        variation <- list(1,cv)
        walk(1:each[,.N], function(i)
          each[i, ADJUSTED_VOL_ENTREGA:=(variation[[i]]*VOL_ENTREGA)])
        
      }else if(each[,.N] == 3){
        variation <- list(1,cv,(2*cv))
        walk(1:each[,.N], function(i)
          each[i, ADJUSTED_VOL_ENTREGA:=(variation[[i]]*VOL_ENTREGA)])
      }
      return(each)
    }) %>% as.data.table() %>% setcolorder("ID_CLIENTE")
  
  purrr::pluck(df, choice) %>% return()
}
# desagregate_dataset(path = "INFORMATION/FILES/ubicaciones.csv",
#                     choice = "INCORRECT")

Knn_DIRECTIONS <- function(df){
  
  df <- df[,.(LAT, LON)] %>% .[,lapply(.SD, as.numeric)] 
  
  scale_directions <- str_c("SCALE", names(df), sep = "_")
  
  walk2(names(df), scale_directions,
        ~ df[,(.y):=(get(.x)-mean(get(.x)))/sd(get(.x))]
  )
  
  purrr::pluck(kmeans(x = df[,scale_directions,with = FALSE], 
                      centers = 6, nstart = 25, algorithm = "Hartigan-Wong"), 
               "cluster") %>% 
    return()
}


#PARTICULAR 2
#' @SUBFUNCTION
#' @description
#' @param
#' @return
kmeanSame_size <- function(df, vars_model, choice = c("CORRECT", "INCORRECT")){
  
  k <- 6
  
  df_incorrect <- df[,ID_CLIENTE]
  
  df <- df[, vars_model,with = FALSE]
  
  optimal_samples <- kmeans(x = df, centers = k, nstart = 25)
  
  distances_centroid <- map(1:k, function(i) {
    points_dif_df <- t(t(df)-purrr::pluck(optimal_samples, "centers")[i,]) %>%
      as.data.table()
    
    walk(names(points_dif_df), ~ points_dif_df[,(.x):=(get(.x)^2)])
    
    points_dif_df[,.(sqrt(rowSums(.SD))), .SDcols = names(points_dif_df)] %>%
      setnames(glue("EUCLIDEAN_DISTANCE_{i}")) %>% return()
  }) %>% cbind.data.frame() %>% data.table()
  
  if(choice == "CORRECT"){#FRECUENCIA 1
    
    cardinality_sample <- df[,.N]/k 
    number_elements_in_clusters <- rep(0, k)
    LABEL = rep(NA, df[,.N])
    
    for(i in 1:distances_centroid[,.N]){
      
      BestCluster <- which.min(distances_centroid[i,])
      
      LABEL[i] <- BestCluster
      
      number_elements_in_clusters[BestCluster] <- number_elements_in_clusters[BestCluster] + 1
      
      if(number_elements_in_clusters[BestCluster] >= cardinality_sample){
        
        distances_centroid[,BestCluster] = NA
      }
    }
    result <- LABEL
    
  }else if(choice == "INCORRECT"){#FRECUENCIA >1
    
    distances_centroid[,ID_CLIENTE:=as.integer(df_incorrect)]
    
    result <- map(distances_centroid[,ID_CLIENTE] %>% unique(), function(i){
      
      each <- copy(distances_centroid[ID_CLIENTE == i])
      
      LABEL <- map_int(1:each[,.N], ~ which.min(each[.x]))
      
      LABEL <- analyze_labels(LABEL = LABEL, df = each)
      LABEL
    }) %>% flatten_int()
  }
  result
}
# kmeanSame_size(df = purrr::pluck(df, "INCORRECT"),
#                vars_model = vars_model,
#                choice = "INCORRECT")

#' @SUBFUNCTION
#' @description
#' @param
#' @return
analyze_labels <- function(LABEL, df){
  
  check_duplicated_label <- LABEL %>% duplicated()
  
  duplicated_col <- LABEL[check_duplicated_label] %>% unique()
  duplicated_row <- which(LABEL == duplicated_col)
  
  no_duplicated_col <- LABEL[LABEL != duplicated_col]
  
  if(any(check_duplicated_label)){
    
    max_duplicated_change_row <- df[duplicated_row, duplicated_col, with = FALSE] %>% 
      unlist() %>% unname() %>% which.max()
    max_duplicated_change_row <- duplicated_row[max_duplicated_change_row]
    
    LABEL[max_duplicated_change_row] <- df[max_duplicated_change_row, 
                                           -c(duplicated_col, 
                                              no_duplicated_col),
                                           with = FALSE] %>%  unlist() %>% 
      which.min() %>% names() %>% 
      str_extract(pattern = "(?<=EUCLIDEAN_DISTANCE_).*") %>% as.integer()
    result <- LABEL
    
  }else result <- LABEL
  
  if(any(result %>% duplicated())) result <- analyze_labels(LABEL = result, 
                                                            df = df)
  else result
  
  result 
}

#' @description
#' @param
#' @return
iterative_merge <- function(dfs_list, key, ...){
  Reduce(function(x,y) merge(x, y, by = key, ...),
         x = dfs_list)
}


map_generator <- function(df, lng, lat, lng1, lng2, lat1, lat2){
  
  labels_study <- df[,unique(LABEL)] %>% sort()
  
  factor_color <- colorFactor(
    palette = map(1:length(labels_study), 
                  function(i) i %>% randomColor() %>% unique()) %>% map_chr(1),
    levels = labels_study
  )
  
  map <- leaflet(options = leafletOptions(dragging = TRUE, minZoom = 4)) %>% 
    addTiles(group = "OSM") %>% 
    addProviderTiles("CartoDB", group= "Carto Light") %>% 
    addProviderTiles("CartoDB.DarkMatter", group = "Carto Dark") %>% 
    addProviderTiles("Esri", group = "Esri") %>%  
    setView(lng = lng,
            lat = lat,
            zoom = 4) %>% 
    setMaxBounds(lng1 = lng1, lat1 = lat1,
                 lng2 = lng2 , lat2 = lat2) %>% 
    
    addMarkers(df = df,
               label = labels_study[1],
               factor_color = factor_color) %>% 
    addMarkers(df = df,
               label = labels_study[2],
               factor_color = factor_color) %>% 
    addMarkers(df = df,
               label = labels_study[3],
               factor_color = factor_color) %>% 
    addMarkers(df = df,
               label = labels_study[4],
               factor_color = factor_color) %>% 
    addMarkers(df = df,
               label = labels_study[5],
               factor_color = factor_color) %>% 
    addMarkers(df = df,
               label = labels_study[6],
               factor_color = factor_color) 
  
  map %>% addLegend(title = "Cluster", 
                    pal = factor_color,
                    values = labels_study,
                    opacity = 0.5,
                    position = "bottomleft") %>% 
    
    addLayersControl(overlayGroups = 1:6,
                     baseGroups = c("OSM","Carto Light", "Carto Dark", "Esri"),
                     position = "topright")%>% 
    
    addResetMapButton() %>% 
    addMiniMap(toggleDisplay = TRUE) %>% 
    return()
}

addMarkers <- function(map, df, label, factor_color){
  map %>% 
    addCircleMarkers(data = df[LABEL == label], 
                     lng = ~ LON,
                     lat = ~ LAT,
                     label = ~ ID_CLIENTE,
                     color = ~ factor_color(LABEL), 
                     group = label,
                     radius = 4,
                     opacity = 0.4) %>% 
    addCircleMarkers(data = df[LABEL == label], 
                     lng = ~ LON,
                     lat = ~ LAT,
                     label = ~ ID_CLIENTE,
                     color = ~ factor_color(LABEL), 
                     group = label,
                     radius = 4,
                     opacity = 0.4,
                     clusterOptions = markerClusterOptions()) %>% 
    addPolygons(data = df[LABEL == label],
                lng = ~ LON,
                lat = ~ LAT,
                label = ~ ID_CLIENTE,
                color = ~ factor_color(LABEL),
                group = label,
                fill = FALSE, 
                weight = 1, 
                highlightOptions = highlightOptions(color = "white",
                                                    weight = 2,
                                                    bringToFront = TRUE))
}

df <- read.csv(file = "C:/Users/actje/Desktop/HACKATHON/FILES/table.csv",
               header = TRUE,
               na.strings = "NA") %>% as.data.table()

# library(dplyr) ; library(htmlwidgets) ; library(leaflet) ; library(leaflet.extras)
# saveWidget(map_generator(df = df,
#                          lng = -100,
#                          lat = 22.5,
#                          lng1 = -117.12776,
#                          lng2 = -86.811982388,
#                          lat1 = 14.5388286402,
#                          lat2 = 32.72083), file="web_map.html")
