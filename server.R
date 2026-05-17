# import data limit administrative
districtLimit <- readRDS("./data/limit/ifanadiana.rds")
fokontanyLimit <- st_read("./data/limit/fokontany.gpkg")

# import data vegetated water
ev_stat_smooth <- readRDS("./data/ev/ev_rice_summary_final_XY.rds")
ev_cluster <- readRDS("./data/ev/ev_cluster_400m2_ev_final_XY.rds")

# import data flooding (vegetated water+open water)
flood_stat_smooth <- readRDS("./data/flood/flood_rice_summary_final_XY.rds")
flood_cluster <- readRDS("./data/flood/flood_cluster_400m2_flood_final_XY.rds")

# set default colors to plot data EV
myColorsPlot_ev <- c("Inactive"="#FDEC5B",
                     "Very low flood"="#ABE173",
                     "Low flood with two seasons per year"="#69C99B", 
                     "Low to moderate flood with two seasons per year"="#59ACA9",
                     "Moderate flood"="#648DA9",
                     "High flood"="#746CA2",
                     "Permanent flood post-2018"="#73417F", 
                     "unclassified"="#DCDCDC")

# set default colors to mapview data ev
colorClustMap_ev <- colorFactor(c("#FDEC5B", "#ABE173", "#69C99B","#59ACA9","#648DA9",
                                   "#746CA2", "#73417F", "#DCDCDC"), 
                                c("Inactive", 
                                  "Very low flood", 
                                  "Low flood with two seasons per year", 
                                  "Low to moderate flood with two seasons per year", 
                                  "Moderate flood", 
                                  "High flood", 
                                  "Permanent flood post-2018", 
                                  "unclassified"), 
                                 ordered=TRUE)

# set default colors to plot data flood
myColorsPlot_flood <- c("Inactive"="#FEEB45",
                        "Inactive mid-year 2018 with low flooded"="#B2DF4E",
                        "Low flooding"="#5BC481", "Low-Moderately flooded"="#40AF99",
                        "Moderately flooded"="#47929F",
                        "Moderately flooded with two seasons per year"="#496994",
                        "High flooded"="#5E4E8E", "Very high flooded"="#5C226A",
                        "unclassified"="#DCDCDC")

# set default colors to mapview data flood
colorClustMap_flood <- colorFactor(c("#FEEB45", "#B2DF4E", "#5BC481","#40AF99","#47929F",
                                     "#496994", "#5E4E8E", "#5C226A", "#DCDCDC"), 
                                     c("Inactive", 
                                       "Inactive mid-year 2018 with low flooded", 
                                       "Low flooding", "Low-Moderately flooded", 
                                       "Moderately flooded", 
                                       "Moderately flooded with two seasons per year", 
                                       "High flooded", "Very high flooded", "unclassified"), 
                                     ordered=TRUE)

# function to connect at PostgreSQL with package R (PostGIS)
fun.connexionDB <- function(status, typeConnexion=NULL) {
  
  if (is.null(typeConnexion)) { drv <- RPostgres::Postgres() } else { drv <- DBI::dbDriver("PostgreSQL") }
  conn <- dbConnect(
    drv, 
    dbname=Sys.getenv("POSTGRES_DB_LSM","lsm"), 
    host=Sys.getenv("POSTGRES_HOST", "postgis_db"), #postgis_db
    port=Sys.getenv("POSTGRES_PORT_INTERNAL", "5432"), #5432
    user=Sys.getenv("POSTGRES_USER","shiny"), 
    password=Sys.getenv("POSTGRES_PASSWORD","sh1nY@pp"))
  if (status=="start") {
    return(conn)
  }else {
    return(dbDisconnect(conn))
  }
  
}


## backend app
server <- function(input, output) {
  #bs_themer()
  
  # event change value one each variable selected 
  varDt.stat_use <- reactiveVal();
  observeEvent(c(input$inp_var_saison), {
    
    #ui select filter
    if (input$inp_var_saison %in% c("primary", "secondary")) {
      
      output$slc_ditrib_season <- renderUI({
        
        selectInput('inp_var_riz', 'Distribution', 
                    c("Month of peak flooding"="peak", #Mois d'inondation plus élevé
                      "Beginning of season"="start", 
                      "End of season"="end"
                    ), selected = "peak")
        
      })
      
    } else {
      
      output$slc_ditrib_season <- renderUI({ NULL})
      
    }
    
    # info data to show visualization
    varColumnSmooth <- c("full_id", "num_season","year_2season",
                         "num_inactive","start_month_mode_seasonPrim",
                         "end_month_mode_seasonPrim","peak_month_mode_seasonPrim",
                         "dur_day_mean_seasonPrim","dur_day_median_seasonPrim",
                         "amp_mean_seasonPrim", "perc_max", "superficie", 
                         "X", "Y", "comm_fkt")
    
    if (input$inp_anl_data=="flood"){ # flooding
      
      if (input$inp_var_saison=="household") {
        dt.stat_use <- flood_cluster[,c("full_id","num_build","class_flood","color_flood","comm_fkt")]
      } else {
        dt.stat_use <- flood_stat_smooth[,varColumnSmooth]
      }
      
    }else { # vegetated water
      
      if (input$inp_var_saison=="household") {
        dt.stat_use <- ev_cluster[,c("full_id","num_build","class_ev","color_ev","comm_fkt")]
      } else {
        dt.stat_use <- ev_stat_smooth[,varColumnSmooth]
      }
      
    }
    
    # add to variable global the data result filter 
    varDt.stat_use(dt.stat_use)
    
    # map function to show variable indicator
    fun.carteSelect <- function(flood_out, var_title_legend, pal) {

      leafletProxy('mapRiz') %>% clearControls() %>%
        addPolygons(data = flood_out,
                    fillColor = ~pal(var_agg),
                    fillOpacity = 0.7,
                    color ="gray",
                    group = "Fokontany District", layerId = ~comm_fkt,
                    weight = 1, opacity = 0.5, dashArray = "0",
                    highlightOptions = highlightOptions(
                      weight = 5,
                      color = "yellow",
                      dashArray = "",
                      bringToFront = TRUE),
                    label = ~comm_fkt,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "12px",
                      direction = "auto")) %>%
        addLegend(pal = pal, values = flood_out$var_agg,
                  opacity = 0.7, title = var_title_legend, position = "bottomright")
    }
    
    # distribution
    # household (building)
    if (input$inp_var_saison=="household") {
      
      flood_out <- data.frame(fokontanyLimit[,c("num_build", "comm_fkt")]) |> 
        group_by(comm_fkt) |>
        summarise(var_agg = sum(num_build, na.rm = T)) |>
        ungroup() |>
        left_join(fokontanyLimit) |>
        st_as_sf()
      
      pal <- colorNumeric( palette = "OrRd", domain = flood_out$var_agg, 
                           na.color = "#708090") #, reverse = TRUE
      var_title_legend <- HTML("Number of housholds<br>close to rice fiels<br>(500m)")
      
      # show map
      fun.carteSelect(flood_out, var_title_legend, pal)
    }
    
    # inactive ricefield
    if (input$inp_var_saison=="inactive") {
      
      flood_out <- dt.stat_use[,c("num_inactive", "comm_fkt")] |> 
        group_by(comm_fkt) |>
        summarise(var_agg = ceiling(mean(num_inactive, na.rm = T))) |>
        ungroup() |>
        left_join(fokontanyLimit) |>
        st_as_sf()
      
      pal <- colorNumeric( palette = "Greys", domain = flood_out$var_agg, 
                           na.color = "#708090") #, reverse = TRUE
      var_title_legend <- HTML("Average number of<br>inactive seasons<br>(over 6 years)")
      
      # show map
      fun.carteSelect(flood_out, var_title_legend, pal)
    }
    
    # proportion of seasonal crops per year
    if (input$inp_var_saison=="prop_2_culture") {
      
      flood_out <- dt.stat_use[,c("year_2season", "comm_fkt")] |>
        group_by(comm_fkt) |>
        summarise(var_agg = mean(year_2season>1, na.rm = T)) |> #*100
        ungroup() |>
        left_join(fokontanyLimit) |>
        st_as_sf()
      
      pal <- colorNumeric( palette = "BrBG", domain = flood_out$var_agg, 
                           na.color = "#708090", reverse = TRUE) #, reverse = TRUE
      var_title_legend <- HTML("Proportion of rice fields<br>with at least 2 active<br>seasons (%)")
      
      # show map
      fun.carteSelect(flood_out, var_title_legend, pal)
    }
    
    # clear ui list graphic and maps with shapefile rice fields selected
    output$uiAbsolutePanel <- renderUI({NULL})

    # clear buffer 500m when click rice field
    if (!is.null(varIDRizSelectLast())) {
      varIDRemove <- varIDRizSelectLast()$full_id
      leafletProxy( mapId = "mapRiz" ) %>% removeShape(varIDRemove) %>% removeControl("class_legend")
    }

    if (!is.null(varIDRizBuffSelectLast()) & !is.null(varIDBuildBuffSelectLast())) {

      varIDRemove <- varIDRizBuffSelectLast()$full_id
      varIDBuildRemove <- varIDBuildBuffSelectLast()$build_id

      leafletProxy( mapId = "mapRiz" ) %>%
        removeShape(paste0(varIDRemove,"_buffer")) %>%
        removeShape(paste0(varIDBuildRemove,"_buffer"))

    }

    # clear administrative limit selected (sub-dustrict) with their information
    if (!is.null(varIDFokontanySelectLast())) {

      leafletProxy( mapId = "mapRiz" ) %>% removeShape("shpClick")

    }

    # emprise map after other filter is selected
    leafletProxy('mapRiz') %>% fitBounds(bboxLimit[1], bboxLimit[2], bboxLimit[3], bboxLimit[4])
    
  })
  
  # filter by primary and secondary
  observeEvent(c(input$inp_var_riz), {

    # info data to show
    if (input$inp_anl_data=="ev") {
      dt.stat_use <- ev_stat_smooth
    }

    if (input$inp_anl_data=="flood") {
      dt.stat_use <- flood_stat_smooth
    }

    # season
    if (input$inp_var_saison=="primary") {
      dt.stat_use <- dt.stat_use[,c("full_id", "num_season","year_2season",
                                    "num_inactive","start_month_mode_seasonPrim",
                                    "end_month_mode_seasonPrim","peak_month_mode_seasonPrim",
                                    "dur_day_mean_seasonPrim","dur_day_median_seasonPrim",
                                    "amp_mean_seasonPrim", "perc_max", "superficie", 
                                    "X", "Y", "comm_fkt")]
    }

    if (input$inp_var_saison=="secondary") {
      dt.stat_use <- dt.stat_use[,c("full_id", "num_season","year_2season",
                                    "num_inactive","start_month_mode_seasonSec",
                                    "end_month_mode_seasonSec","peak_month_mode_seasonSec",
                                    "dur_day_mean_seasonSec","dur_day_median_seasonSec",
                                    "amp_mean_seasonSec", "perc_max", "superficie", 
                                    "X", "Y", "comm_fkt")]
    }

    varDt.stat_use(dt.stat_use) # add data result to varibale global

    # map dynamic data season pimary and secondary
    funCarteSeason <- function(fokontanyLimitData, pal, lab_out) {

      leafletProxy('mapRiz') %>% clearControls() %>%
        addPolygons(data = fokontanyLimitData,
                    fillColor = ~pal(out.value),
                    fillOpacity = 0.7,
                    color ="gray",
                    group = "Fokontany District", layerId = ~comm_fkt,
                    weight = 1, opacity = 0.5, dashArray = "0",
                    highlightOptions = highlightOptions(
                      weight = 5,
                      color = "yellow",
                      dashArray = "",
                      bringToFront = TRUE),
                    label = ~comm_fkt,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "12px",
                      direction = "auto")) %>%
        addLegend(pal = pal, values = fokontanyLimitData$out.value,
                  opacity = 0.7, title = lab_out, position = "bottomright")
      
    }
    
    # distribution
    # maximum flood area
    if (input$inp_var_riz=="peak") {

      colnames(dt.stat_use)[7] <- "peak"
      fokontanyLimitPeak <- dt.stat_use[,c(7,15)] |>
        group_by(comm_fkt) |>
        summarise(out.value = median(peak, na.rm = T)) |>
        ungroup() |>
        left_join(fokontanyLimit) |>
        st_as_sf()

      # color pallete
      pal <- colorNumeric( palette = "RdBu", domain = fokontanyLimitPeak$out.value, 
                           na.color = NA) # na.color = "#708090", reverse = TRUE
      lab_peak <- "Month"
      
      # use fonction map dynamic
      funCarteSeason(fokontanyLimitPeak, pal, lab_peak) 

    }

    # month start crop ricefield
    if (input$inp_var_riz=="start") {

      colnames(dt.stat_use)[5] <- "start_s1"
      fokontanyLimitStartS1 <- dt.stat_use[,c(5,15)] |>
        group_by(comm_fkt) |>
        summarise(out.value = median(start_s1, na.rm = T)) |>
        ungroup() |>
        left_join(fokontanyLimit) |>
        st_as_sf()
      
      # color pallete
      pal <- colorNumeric( palette = "Blues", domain = fokontanyLimitStartS1$out.value, 
                           na.color = NA, reverse = TRUE) #, reverse = TRUE
      lab_start <- "Month"
      
      # use fonction map dynamic
      funCarteSeason(fokontanyLimitStartS1, pal, lab_start)
      
    }

    # month end crop ricefield
    if (input$inp_var_riz=="end") {

      colnames(dt.stat_use)[6] <- "end_s1"
      fokontanyLimitEndS1 <- dt.stat_use[,c(6,15)] |>
        group_by(comm_fkt) |>
        summarise(out.value = median(end_s1, na.rm = T)) |>
        ungroup() |>
        left_join(fokontanyLimit) |>
        st_as_sf()
      
      # color pallete
      pal <- colorNumeric( palette = "Blues", domain = fokontanyLimitEndS1$out.value, 
                           na.color = NA, reverse = TRUE) #, reverse = TRUE
      lab_end <- "Month"
      
      # use fonction map dynamic
      funCarteSeason(fokontanyLimitEndS1, pal, lab_end)
      
    }
    
    # clear ui list graphic and maps with shapefile rice fields selected
    output$uiAbsolutePanel <- renderUI({NULL})

    # clear buffer 500m when click rice field
    if (!is.null(varIDRizSelectLast())) {
      varIDRemove <- varIDRizSelectLast()$full_id
      leafletProxy( mapId = "mapRiz" ) %>% removeShape(varIDRemove) %>% removeControl("class_legend")
    }

    if (!is.null(varIDRizBuffSelectLast()) & !is.null(varIDBuildBuffSelectLast())) {

      varIDRemove <- varIDRizBuffSelectLast()$full_id
      varIDBuildRemove <- varIDBuildBuffSelectLast()$build_id

      leafletProxy( mapId = "mapRiz" ) %>%
        removeShape(paste0(varIDRemove,"_buffer")) %>%
        removeShape(paste0(varIDBuildRemove,"_buffer"))

    }

    # clear administrative limit selected (sub-dustrict) with their information
    if (!is.null(varIDFokontanySelectLast())) {

      leafletProxy( mapId = "mapRiz" ) %>% removeShape("shpClick")

    }

    # emprise map after other filter is selected
    leafletProxy('mapRiz') %>% fitBounds(bboxLimit[1], bboxLimit[2], bboxLimit[3], bboxLimit[4])

  })

  ## Interactive Map ###########################################
  
  # map to shows result analysis
  bboxLimit <- st_bbox(districtLimit) %>% as.vector() # emprise shape district
  output$mapRiz <- renderLeaflet({
    leaflet(options = leafletOptions(maxZoom = 17)) %>% #setView(zoom = 15) %>% , attributionControl=FALSE 
      addTiles(group="OpenStreetMap (OSM)", options = providerTileOptions(noWrap = TRUE)) %>% 
      #addControl("Base map and data from OpenStreetMap and OpenStreetMap Foundation.", position = "bottomright", className = "info") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addPolygons(data = districtLimit, color ="purple", fill = F, 
                  group = "District Limit", layerId = "districtLimit")  %>% 
      addPolygons(data = fokontanyLimit, color = "gray", fill = F, 
                  group = "Fokontany Limit", layerId = ~fokontanyLimit$comm_fkt) %>% 
      fitBounds(bboxLimit[1], bboxLimit[2], bboxLimit[3], bboxLimit[4]) %>% 
      addScaleBar(position = c("bottomright")) %>%
      addLayersControl(baseGroups = c("OpenStreetMap (OSM)","Satellite"),
                       options = layersControlOptions(collapsed = FALSE))
  })
  
  
  # Event to click a limit area to mapview
  varCurentFkt <- reactiveVal(NULL); varDataSmoothShpClik <- reactiveVal(NULL);
  varIDRizSelectLast <- reactiveVal(NULL); varIDRizBuffSelectLast <- reactiveVal(NULL);
  varIDBuildBuffSelectLast <- reactiveVal(NULL); varIDFokontanySelectLast <- reactiveVal(NULL);

  observeEvent(c(input$mapRiz_shape_click, input$mapRiz_click), {
    
    click <- req(input$mapRiz_shape_click)
    viewGraphe <- list()
    
    if (click$group == "Fokontany District") {
      
      # progressBar
      withProgress(message = "Get ricefield in progress !",
                   detail = 'This may take a while...', value = 0, 
         {
                     
      fktShp <- click$id; varCurentFkt(fktShp)
      boundShp <- subset(fokontanyLimit, comm_fkt==fktShp); extend.boundShp <- extent(boundShp)
      leafletProxy( mapId = "mapRiz" ) %>%  addPolygons(data = boundShp,
                                                        color ="yellow", group = "shpClick", layerId = "shpClick",
                                                        weight = 3, dashArray = "3", fillOpacity = 0.01, opacity = 1,
                                                        label = ~comm_fkt,
                                                        labelOptions = labelOptions(noHide = T, #offset = c(0, -12),
                                                                                    style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                                    textsize = "12px",
                                                                                    direction = "auto")) %>%
        fitBounds(extend.boundShp[1], extend.boundShp[3], extend.boundShp[2], extend.boundShp[4])
      
      varIDFokontanySelectLast("shpClick")
      
      # filter data to get rice field shape selected in database
      varRizXYFillter <- subset(varDt.stat_use(), comm_fkt==fktShp)
      varRizSelected <- unique(varRizXYFillter$full_id);
      
      options(useFancyQuotes = FALSE)
      varRizSelected <- paste0("(", paste(sQuote(sub("","", unlist(strsplit(varRizSelected,split = ",")))), collapse = ","),")")
      
      #fake_progress()
      
      conn <- fun.connexionDB("start")
      riceShpLimitClick <- pgGetGeom(conn, query = paste0("select * from ricefield where full_id in ", varRizSelected))
      fun.connexionDB("end")
      
      incProgress(1/3)
      
      if (!is.null(varIDRizSelectLast())) {
        print(varIDRizSelectLast())
        varIDRemove <- varIDRizSelectLast()$full_id
        leafletProxy( mapId = "mapRiz" ) %>% removeShape(varIDRemove) %>% removeControl("class_legend")
      }
      
      if (!is.null(varIDRizBuffSelectLast()) & !is.null(varIDBuildBuffSelectLast())) {
        
        varIDRemove <- varIDRizBuffSelectLast()$full_id
        varIDBuildRemove <- varIDBuildBuffSelectLast()$build_id
        
        leafletProxy( mapId = "mapRiz" ) %>% 
          removeShape(paste0(varIDRemove,"_buffer")) %>% 
          removeShape(paste0(varIDBuildRemove,"_buffer"))
        
      }

      if (input$inp_anl_data=="ev") {
        
        # add to variable global the ricefiel last select
        varIDRizSelectLast(riceShpLimitClick)
        
        riceShpLimitClick$class_ev <-  factor(riceShpLimitClick$class_ev,
                                             levels = c("Inactive", 
                                                        "Vegetation with very low water", 
                                                        "Low water vegetation with two seasons per year", 
                                                        "Low to moderate water vegetation with two seasons per year", 
                                                        "Vegetation with moderate water", 
                                                        "Vegetation with high water", 
                                                        "Vegetation with very high water before 2018 and still filled with water without activity", 
                                                        "unclassified"),
                                             labels = c("Inactive", 
                                                        "Very low flood", 
                                                        "Low flood with two seasons per year", 
                                                        "Low to moderate flood with two seasons per year", 
                                                        "Moderate flood", 
                                                        "High flood", 
                                                        "Permanent flood post-2018", 
                                                        "unclassified")
                                             )
        
        leafletProxy("mapRiz", data = riceShpLimitClick)%>%
          addPolygons(color ="white",
                      fillColor = ~colorClustMap_ev(class_ev), fillOpacity = 1,
                      weight = 2, opacity = 0.5, dashArray = "0",
                      group = "Ricefield", layerId = ~full_id, 
                      popup = ~paste("Rizière :",full_id)) %>%
          addLegend("bottomleft", pal = colorClustMap_ev, values = ~class_ev, 
                    labels = ~class_ev, layerId = "class_legend", title = "Class rice")
        
      }
      
      if (input$inp_anl_data=="flood") {
        
        # add to variable global the ricefiel last select
        varIDRizSelectLast(riceShpLimitClick)
        riceShpLimitClick$class_flood <- ifelse(riceShpLimitClick$class_flood=="Inactive_2018 with low flooded",
                                                "Inactive mid-year 2018 with low flooded",riceShpLimitClick$class_flood)
        riceShpLimitClick$class_flood <-  factor(riceShpLimitClick$class_flood,
                                                 levels = c("Inactive", 
                                                            "Inactive mid-year 2018 with low flooded", 
                                                            "Low flooding", "Low-Moderately flooded", 
                                                            "Moderately flooded", 
                                                            "Moderately flooded with two seasons per year", 
                                                            "High flooded", "Very high flooded"))
        
        leafletProxy("mapRiz", data = riceShpLimitClick)%>%
          addPolygons(color ="white",
                      fillColor = ~colorClustMap_flood(class_flood), fillOpacity = 1,
                      weight = 2, opacity = 0.5, dashArray = "0",
                      group = "Ricefield", layerId = ~full_id, 
                      popup = ~paste("Rizière :",full_id)) %>%
          addLegend("bottomleft", pal = colorClustMap_flood, values = ~class_flood, 
                    labels = ~class_flood, layerId = "class_legend", title = "Class rice")
      }
      
      
      ########################
      ## FOKONTANY GRAPHICS
      ########################

      dataClickShp <- varDt.stat_use()

      # # distribution
      # if (input$inp_var_riz=="peak") {
      #   colnames(dataClickShp)[7] <- "peak"
      # }

      # # Title graphics
      # viewGraphe[[length(viewGraphe)+1]] <- column(width = 12, style="text-align: center;",
      #                                              h4(fktShp), hr())

      # info data to show
      if (input$inp_anl_data=="ev") {
        sqlSmoothShpClik <- paste0("select ev.full_id as idriz, ev.date_dt as x, ev.smooth as y, ev.class_ev from evsmooth ev where full_id in ", varRizSelected)
      }

      if (input$inp_anl_data=="flood") {
        sqlSmoothShpClik <- paste0("select fld.full_id as idriz, fld.date_dt as x, fld.smooth as y, fld.class_flood from floodsmooth fld where full_id in ", varRizSelected)
      }


      conn <- fun.connexionDB("start")
      dataSmoothShpClik <- dbGetQuery(conn, sqlSmoothShpClik)
      fun.connexionDB("end")

      incProgress(2/3)
      
      # add to variable global data rice selcted
      varDataSmoothShpClik(dataSmoothShpClik)

      # data temporal
      viewGraphe[[length(viewGraphe)+1]] <- column(width = 12,
                                                   #HTML("<b>", "Various classes of flooded rice", "</b>"),
                                                   plotlyOutput("plotlyLineSmooth", height = 400))

      if (input$inp_anl_data=="ev") { 
        
        output$plotlyLineSmooth <- renderPlotly({
          
          # create ggplot with summary data flood
          agg.dataSmoothShpClik <-
            dataSmoothShpClik[,c("x","y","class_ev")] %>%
            group_by(x, class_ev) %>% 
            dplyr::summarize(mean = mean(y, na.rm = TRUE),
                             lower = quantile(y, na.rm = TRUE, probs = 0.25), 
                             upper= quantile(y, na.rm = TRUE, probs = 0.75)
            )  %>% arrange(class_ev) 

          agg.dataSmoothShpClik$class_ev <-  factor(agg.dataSmoothShpClik$class_ev,
                                                    levels = c("Inactive", 
                                                               "Vegetation with very low water", 
                                                               "Low water vegetation with two seasons per year", 
                                                               "Low to moderate water vegetation with two seasons per year", 
                                                               "Vegetation with moderate water", 
                                                               "Vegetation with high water", 
                                                               "Vegetation with very high water before 2018 and still filled with water without activity", 
                                                               "unclassified"),
                                                    labels = c("Inactive", 
                                                               "Very low flood", 
                                                               "Low flood with two seasons per year", 
                                                               "Low to moderate flood with two seasons per year", 
                                                               "Moderate flood", 
                                                               "High flood", 
                                                               "Permanent flood post-2018", 
                                                               "unclassified"))
          
          # plot symmary data by fokontany
          p <- ggplot(data=agg.dataSmoothShpClik, aes(x=x, y=mean)) +
            geom_line(aes(color=class_ev), alpha=0.85, linewidth=1.5) +
            geom_ribbon(aes(ymin = lower, ymax = upper, 
                            fill = class_ev), alpha=0.1) +
            scale_color_manual(values=myColorsPlot_ev) +
            scale_fill_manual(values=myColorsPlot_ev) +
            scale_x_date(date_labels="%m-%Y",date_breaks  ="3 month", expand =c(0,0)) +
            guides(fill = FALSE) +
            labs(x = "Month and year",
                 y = "Proportion of vegetated water area (%)",
                 color="Vegetated water class : ") #+
          
          
          # wrap ggplot object with ggplotly
          ggplotly(p) %>%
            plotly::layout(margin = list(l=40, r=20, t=20, b=20, pad=20),
                           legend = list(orientation = "h", y = 1.5, x = 0),
                           xaxis = list(tickangle=45))
        })
        
      }
      
      if (input$inp_anl_data=="flood") { 
        
        output$plotlyLineSmooth <- renderPlotly({
  
          # create ggplot with summary data flood
          agg.dataSmoothShpClik <-
            dataSmoothShpClik[,c("x","y","class_flood")] %>%
            group_by(x, class_flood) %>% 
            dplyr::summarize(mean = mean(y, na.rm = TRUE),
                             lower = quantile(y, na.rm = TRUE, probs = 0.25), 
                             upper= quantile(y, na.rm = TRUE, probs = 0.75)
            )  %>% arrange(class_flood) 
          
          agg.dataSmoothShpClik$class_flood <- ifelse(agg.dataSmoothShpClik$class_flood=="Inactive_2018 with low flooded",
                                                      "Inactive mid-year 2018 with low flooded",agg.dataSmoothShpClik$class_flood)
          agg.dataSmoothShpClik$class_flood <-  factor(agg.dataSmoothShpClik$class_flood,
                                                       levels = c("Inactive", 
                                                                  "Inactive mid-year 2018 with low flooded", 
                                                                  "Low flooding", "Low-Moderately flooded", 
                                                                  "Moderately flooded", 
                                                                  "Moderately flooded with two seasons per year", 
                                                                  "High flooded", "Very high flooded"))
          # plot symmary data by fokontany
          p <- ggplot(data=agg.dataSmoothShpClik, aes(x=x, y=mean)) +
                geom_line(aes(color=class_flood), alpha=0.85, linewidth=1.5) +
                geom_ribbon(aes(ymin = lower, ymax = upper, 
                                fill = class_flood), alpha=0.1) +
                scale_color_manual(values=myColorsPlot_flood) +
                scale_fill_manual(values=myColorsPlot_flood) +
                scale_x_date(date_labels="%m-%Y",date_breaks  ="3 month", expand =c(0,0)) +
                guides(fill = FALSE) +
                labs(x = "Month and year",
                     y = "Proportion of flooded area (%)",
                     color="Flood class : ") #+
                
  
          # wrap ggplot object with ggplotly
          ggplotly(p) %>%
            plotly::layout(margin = list(l=40, r=20, t=20, b=20, pad=20),
                           legend = list(orientation = "h", y = 1.5, x = 0),
                           xaxis = list(tickangle=45))
        })
      
      }
      
      incProgress(3/3)
      
      })
      
    } 
    
    if (click$group == "Ricefield") {
      
      #req(click$id)
      
      rizShp <- click$id;
      boundShp <- varIDRizSelectLast()[varIDRizSelectLast()$full_id==rizShp,]; 
      extend.boundShp <- extent(boundShp)
      
      #print(boundShp)
      
      # convert XY to data shapefile
      flood_cluster_sf <- flood_cluster %>% sf::st_as_sf(coords = c("X", "Y"), crs = 4326)
      buffer_rizShp <- sf::st_buffer(flood_cluster_sf[flood_cluster_sf$full_id==rizShp,], 500, singleSide = TRUE)
      
      #print(buffer_rizShp$full_id)
      
      # get building associate ricefiel selcted (near by 500m)
      conn <- fun.connexionDB("start")
      sqlRizShpClik <- pgGetGeom(conn, query = paste0("WITH ricefield_buiding AS (select * from ricefield_build_500m where full_id='",rizShp,"') 
                                                      select rb.full_id as full_id, b.build_id as build_id, b.geom as geom from ricefield_buiding rb, 
                                                      building b where b.build_id IN(rb.build_id) "))
      fun.connexionDB("end")
      
      if (!is.null(varIDRizBuffSelectLast()) & !is.null(varIDBuildBuffSelectLast())) {
        
        varIDRemove <- varIDRizBuffSelectLast()$full_id
        varIDBuildRemove <- varIDBuildBuffSelectLast()$build_id
        leafletProxy( mapId = "mapRiz" ) %>% 
          removeShape(paste0(varIDRemove,"_buffer")) %>% 
          removeShape(paste0(varIDBuildRemove,"_buffer"))
        
      }

      # add to variable global the ricefiel last select
      varIDRizBuffSelectLast(buffer_rizShp)
      varIDBuildBuffSelectLast(sqlRizShpClik)
      
      leafletProxy( mapId = "mapRiz" ) %>%  addPolygons(data = boundShp,
                                                        color ="yellow", group = "rizClick", layerId = "rizClick",
                                                        weight = 5, dashArray = "3", fillOpacity = 0.01, opacity = 0.75,
                                                        labelOptions = labelOptions(noHide = T, #offset = c(0, -12),
                                                                                    style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                                    textsize = "12px",
                                                                                    direction = "auto")) %>% 
                                            addPolygons(data = buffer_rizShp, fillColor ="grey", 
                                                        fillOpacity = 0.01, 
                                                        opacity = 0.25, fill = "black", 
                                                        layerId = ~paste0(full_id,"_buffer"),
                                                        weight = 5, dashArray = 20,
                                                        options = pathOptions(clickable = FALSE)) %>% 
                                            addPolygons(data = sqlRizShpClik, 
                                                        fillColor ="grey", 
                                                        fillOpacity = 1, 
                                                        opacity = 1, 
                                                        layerId = ~paste0(build_id,"_buffer"),
                                                        fill = "black", stroke = FALSE) %>% 
        fitBounds(extend.boundShp[1], extend.boundShp[3], extend.boundShp[2], extend.boundShp[4]) 
      
      
      ########################
      ## RICE GRAPHICS
      ########################
      
      viewGraphe <- list()
      
      # data temporal
      viewGraphe[[length(viewGraphe)+1]] <- column(width = 12,
                                                   plotlyOutput("plotlyLineSmoothRiz", height = 350))
      
      if (input$inp_anl_data=="flood") { 
        varTitleAxisYPlot <- "Proportion of flooded area (%)"
      } else {
        varTitleAxisYPlot <- "Proportion of vegetated water area (%)"
      }
      
      library(lubridate)
      output$plotlyLineSmoothRiz <- renderPlotly({

        # plot time series by year rice fields
        p <- 
        varDataSmoothShpClik() %>% 
                filter(idriz==rizShp) %>% #"w669958370" #rizShp
                mutate(date_year=as.factor(format(x,'%Y')), 
                       fake_date = make_date(year = min(year(x)), day = day(x), month = month(x))) %>%
                ggplot(aes(x=fake_date)) +
                geom_line(aes(y=y, color=date_year), alpha=0.85, linewidth=1.5) +
                scale_x_date(date_labels="%b", date_breaks  ="1 month",
                             expand =c(0,0))+
                labs(title = 'Time series rice field by year', 
                     x = 'Month', y = varTitleAxisYPlot, 
                     color="Year")
        
        # wrap ggplot object with ggplotly
        ggplotly(p) %>%
        plotly::layout(legend = list(title=list(text='<b> Year </b>'),
                                     orientation = "v", y = .5, x = -.2), 
                       yaxis = list(side ="right"))
      })
      
    } 
    
    output$uiAbsolutePanel <- renderUI({

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 325, left = "auto", right = 120, bottom = "auto",
                    width = 650, height = "auto",
                    fluidRow( viewGraphe )
      )

    })
    
  })

}