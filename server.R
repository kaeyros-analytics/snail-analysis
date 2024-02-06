
server = function(input, output, session) {

  observeEvent(input$loc,{
    if(input$loc == "All"){
      choice = c("All")
    } else if(input$loc == "Njombe Penja"){
      choice = unique(data_f$Season[data_f$locality == input$loc])
    } else{
      choice = c("All",as.character(unique(data_f$Season)))
    }
    updateSelectInput(session, "season", choices = choice)
  })

  filteblue_data <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f
    } else if (input$loc != "All") { #& input$season !="All"
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season)
      }
    }
    }, ignoreNULL = FALSE)

  #display the valuebox of number of collectors
  output$nb_col <- renderValueBox({
    valueBox(value=nrow(filteblue_data()),
             subtitle = "Number of collectors", color = "blue")
   })

  #displays the valuebox of average of the householsize
  # output$nb_house <- renderValueBox({
  #   valueBox(subtitle = "Mean of household size",value=round(mean(filteblue_data()$householdsize)),
  #            color="blue")
  # })

  output$nb_house <- renderValueBox({
    if (nrow(filteblue_data())==0){
      valueBox(subtitle = "Mean of household size",value= paste("No data"),
               color="blue" )
    } else {
      valueBox(subtitle = "Mean of household size",value=round(mean(filteblue_data()$householdsize)),
                          color="blue")
    }

  })

  #displays the valuebox of average of the collect experience
  output$coll_exp <- renderValueBox({
    if (nrow(filteblue_data())==0){
      valueBox(subtitle = "Mean of collector's experience",value= paste("No data"),
               color="blue" )
    } else {
      valueBox(subtitle = "Mean of collector's experience", value=round(mean(filteblue_data()$Collectexp)),
               color="blue")
    }
  })

  #displays the valuebox of average of buckets of snails
  output$snail_bucket <- renderValueBox({
    valueBox(subtitle = "average snail bucket", value = round(mean(filteblue_data()$`Quantity(bucket)`)),
             color="blue")
  })

  #data for the barchart of marital status
  data_marsta <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        dplyr::count(marstat_label, name = "n")%>%
        dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                      pct1 = paste0(percentage, "%"))
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          dplyr::count(marstat_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          dplyr::count(marstat_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

#barchart of marital status
  output$mar_sta <- renderPlotly({
    validate(
      need(nrow(data_marsta()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'of marital status')))
    plotly::plot_ly(data_marsta(), x = ~marstat_label,
                    type = "bar",
                    y = ~percentage,
                    #marker = list(color = c("#0B5345", "#148F77", "#196F3D", "#52BE80", "#7DCEA0", "#CA6F1E")),
                    marker = list(color = c("#483D8B", "slateblue","#0077BE", "#5696CC",
                                            "#76B7DA",  "#A6DAFF")),
                    #colors = "darkviolet",
                    #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                    text = paste(data_marsta()$pct1, sep = ""), textposition = 'outside',
                    hovertext = paste("<br>Number of persons :", data_marsta()$n, "<br>",
                                      "Marital status :",data_marsta()$pct1, data_marsta()$marstat_label),
                                      #"<br>Percentage :", data_marsta()$pct1),
                    hoverinfo = 'text') %>%
      layout(title = "Marital status",
             #legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b> </b>')),
             uniformtext=list(minsize=10, mode='show'),
             xaxis = list(title = "<b> Marital status </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14),
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= -45, showgrid = FALSE),
             yaxis = list(title = "<b> Percentage </b>",
                          titlefont = list(size = 16),
                          # change x-axix size
                          tickfont = list(size = 14),
                          ticksuffix = "%", showgrid = FALSE)
      ) %>%
      config(displayModeBar = F,
             scrollZoom = T)
  })

  #data for the piechart of collector's gender
  data_gender <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        dplyr::count(gender_label, name = "n")%>%
        dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                      pct1 = paste0(percentage, "%"))
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          dplyr::count(gender_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          dplyr::count(gender_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

  #piechart of collector's gender
  output$gender <- renderPlotly({
    validate(
      need(nrow(data_gender()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'of gender')))
    plot_ly(data_gender(), labels= ~data_gender()$gender_label,
                        values= ~data_gender()$n, type="pie",
            hoverinfo = 'text',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            text = ~paste("Gender :", data_gender()$gender_label,
                          "<br>Number of persons :", data_gender()$n),
            marker = list(colors = c( "#5D8AA8","#20B2AA"), # "#1da1f2","#2774AE"
                          line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)) %>%
      layout(title="Gender",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
            #colors= c("darkgreen","darkviolet")
  })

  #data for thebarchart of collector's age
  data_age <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        dplyr::count(age_label, name = "n")%>%
        dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                      pct1 = paste0(percentage, "%"))
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          dplyr::count(age_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          dplyr::count(age_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

  #barchart of collector's age
  output$age <- renderPlotly({
    validate(
      need(nrow(data_age()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'of age')))
    plotly::plot_ly(data_age(), x = ~age_label,
                    type = "bar",
                    y = ~percentage,
                    #marker = list(color = c("#229954", "#D68910","#148F77", "#196F3D", "#21618C")),
                    marker = list(color = c("#008B8B", "#01796F", "#20B2AA" ,"#29AB87", "#66CDAA")),
                    #colors = "darkviolet",
                    #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                    text = paste(data_age()$pct1, sep = ""), textposition = 'outside',
                    hovertext = paste("Age :",data_age()$age_label,
                                      "<br>Number of persons :", data_age()$n,
                                      "<br>Percentage :", data_age()$pct1),
                    hoverinfo = 'text') %>%
      layout(title = "Age",
             legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b> </b>')),
             uniformtext=list(minsize=10, mode='show'),
             xaxis = list(title = "<b> Age (in years) </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14),
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= -45,showgrid = FALSE),
             yaxis = list(title = "<b> Percentage </b>",
                          titlefont = list(size = 16),
                          # change x-axix size
                          tickfont = list(size = 14),
                          ticksuffix = "%",showgrid = FALSE)
      ) %>%
      config(displayModeBar = F,
             scrollZoom = T)

  })

  #data of the barchart of collector's level of education
  data_edulevel <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        dplyr::count(edulevel_label, name = "n")%>%
        dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                      pct1 = paste0(percentage, "%"))
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          dplyr::count(edulevel_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          dplyr::count(edulevel_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

  #barchart of collector's level  of education
  output$edu_level <- renderPlotly({
    validate(
      need(nrow(data_edulevel()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'of level of education')))
    plotly::plot_ly(data_edulevel(), x = ~edulevel_label,
                    type = "bar",
                    y = ~percentage,
                    #marker = list(color =c("#229954", "#D68910","#148F77", "#196F3D", "#21618C" )),
                    #marker = list(color =c("#6C7C59","#679267" , "#009E60", "#00A693", "#A9BA9D" )),
                    marker = list(color =c("#008080", "#20B2AA", "#00A693")),
                    #colors = "darkviolet",
                    #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                    text = paste(data_edulevel()$pct1, sep = ""), textposition = 'outside',
                    hovertext = paste("Education level :",data_edulevel()$edulevel_label,
                                      "<br>Number of persons :", data_edulevel()$n,
                                      "<br>Percentage:", data_edulevel()$pct1),
                    hoverinfo = 'text') %>%
      layout(title = "Level of education",
             legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b> </b>')),
             uniformtext=list(minsize=10, mode='show'),
             xaxis = list(title = "<b> Education level </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14),
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= -45,showgrid = FALSE),
             yaxis = list(title = "<b> Percentage </b>",
                          titlefont = list(size = 16),
                          # change x-axix size
                          tickfont = list(size = 14),
                          ticksuffix = "%",showgrid = FALSE)
             #tickvals = df_most_visited_service_month_year$most_visited_service)
             #hoverlabel=list(bgcolor="gainsboro")
             #width = 500, autosize=F,
             #bargap = 0.1, bargroupgap = 0.1,
      ) %>%
      config(displayModeBar = F,
             scrollZoom = T)

  })

  #barchart of collector's main activity
  data_mainact <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        dplyr::count(mainact_label, name = "n")%>%
        dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                      pct1 = paste0(percentage, "%"))
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          dplyr::count(mainact_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          dplyr::count(mainact_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

  #barchart of collector's main activity
  output$main_ac <- renderPlotly({
    validate(
      need(nrow(data_mainact()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'of main activity')))
    plotly::plot_ly(data_mainact(), x = ~mainact_label,
                    type = "bar",
                    y = ~percentage,
                    #marker = list(color = c("#229954", "#D68910","#148F77", "#196F3D", "#21618C")),
                    marker = list(color = c( "#5D8AA8","#5072A7", "#1D428A","#6082B6", "#B0C4DE")),
                    #colors = "darkviolet",
                    #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                    text = paste(data_mainact()$pct1, sep = ""), textposition = 'outside',
                    hovertext = paste("Main activity :",data_mainact()$mainact_label,
                                      "<br>Number of persons :", data_mainact()$n,
                                      "<br>Percentage :", data_mainact()$pct1),
                    hoverinfo = 'text') %>%
      layout(title = "Main activity",
             legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b> </b>')),
             uniformtext=list(minsize=10, mode='show'),
             xaxis = list(title = "<b> Main activity </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14),
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= -45,showgrid = FALSE),
             yaxis = list(title = "<b> Percentage </b>",
                          titlefont = list(size = 16),
                          # change x-axix size
                          tickfont = list(size = 14),
                          ticksuffix = "%",showgrid = FALSE)
             #tickvals = df_most_visited_service_month_year$most_visited_service)
             #hoverlabel=list(bgcolor="gainsboro")
             #width = 500, autosize=F,
             #bargap = 0.1, bargroupgap = 0.1,
      ) %>%
      config(displayModeBar = F,
             scrollZoom = T)

  })

  #data for the barchart of Number of buckets collected per zones
  data_colzone <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        dplyr::count(colzone_label, name = "n")%>%
        dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                      pct1 = paste0(percentage, "%"))
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          dplyr::count(colzone_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          dplyr::count(colzone_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

  #barchart of Number of buckets collected per zones
  output$col_zone <- renderPlotly({
    validate(
      need(nrow(data_colzone()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'of number of buckets collected per zones')))
    plotly::plot_ly(data_colzone(), x = ~colzone_label,
                    type = "bar",
                    y = ~n,
                    #marker = list(color = c("#229954", "#D68910")),
                    #marker = list(color = c("#6A5ACD", "#324AB2", "#018749", "#2774AE", "#008B8B")),
                    marker = list(color = c("#3E8EDE", "#20B2AA")),
                    #colors = "darkviolet",
                    #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                    text = paste(data_colzone()$n, sep = ""), textposition = 'outside',
                    hovertext = paste("Collect zone :",data_colzone()$colzone_label,
                                      "<br>Number of buckets :", data_colzone()$n),
                    hoverinfo = 'text') %>%
      layout(title = "Number of buckets collected per zones",
             legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b> </b>')),
             uniformtext=list(minsize=10, mode='show'),
             xaxis = list(title = "<b> Zone of collect </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14),
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= 0,showgrid = FALSE),
             yaxis = list(title = "<b> Frequency </b>",
                          titlefont = list(size = 16),
                          # change x-axix size
                          tickfont = list(size = 14),showgrid = FALSE
                          )
      ) %>%
      config(displayModeBar = F,
             scrollZoom = T)

  })

  #data of barchart of Number of buckets collected by month
  data_month <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        dplyr::count(Month, name = "n")%>%
        dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                      pct1 = paste0(percentage, "%"))
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          dplyr::count(Month, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          dplyr::count(Month, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

  #barchart of Number of buckets collected by month
  output$month <- renderPlotly({
    validate(
      need(nrow(data_month()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'of number of buckets collected by month')))
    plotly::plot_ly(data_month(), x = ~Month,
                    type = "bar",
                    y = ~n,
                    #marker = list(color = c("#229954", "#D68910","#148F77", "#196F3D", "#21618C", "#7DCEA0", "#CA6F1E")),
                    marker = list(color = c("#55508D", "#726DA8", "#7D8CC4", "#2774AE", "#1da1f2","#AFDBF5",
                                            "#367588", "#81D8D0", "#A0D2DB")),
                    #colors = "darkviolet",
                    #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                    text = paste(data_month()$n, sep = ""), textposition = 'outside',
                    hovertext = paste("Month :",data_month()$Month,
                                      "<br>Number of buckets :", data_month()$n),
                    hoverinfo = 'text') %>%
      layout(title = "Number of buckets collected by month",
             legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b> </b>')),
             uniformtext=list(minsize=10, mode='show'),
             xaxis = list(title = "<b>Months</b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14),
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= -45,showgrid = FALSE),
             yaxis = list(title = "<b> Frequency </b>",
                          titlefont = list(size = 16),
                          # change x-axix size
                          tickfont = list(size = 14),showgrid = FALSE)
             #tickvals = df_most_visited_service_month_year$most_visited_service)
             #hoverlabel=list(bgcolor="gainsboro")
             #width = 500, autosize=F,
             #bargap = 0.1, bargroupgap = 0.1,
      ) %>%
      config(displayModeBar = F,
             scrollZoom = T)

  })

  ##data of barchart of Number of buckets collected per time of the day
  data_time <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        dplyr::count(time_label, name = "n")%>%
        dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                      pct1 = paste0(percentage, "%"))
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          dplyr::count(time_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          dplyr::count(time_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

#barchart of Number of buckets collected per time of the day
  output$time <- renderPlotly({
    validate(
      need(nrow(data_time()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'number of buckets collected per time of the day')))
    plotly::plot_ly(data_time(), x = ~time_label,
                    type = "bar",
                    y = ~n,
                    #marker = list(color = c("#229954", "#D68910","#148F77")),
                    marker = list(color = c("#324AB2", "#2774AE","#1da1f2")),
                    #colors = "darkviolet",
                    #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                    text = paste(data_time()$n, sep = ""), textposition = 'outside',
                    hovertext = paste("Time :",data_time()$time_label,
                                      "<br>Number of buckets :", data_time()$n),
                    hoverinfo = 'text') %>%
      layout(title = "Number of buckets collected per time of the day",
             legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b></b>')),
             uniformtext=list(minsize=10, mode='show'),
             xaxis = list(title = "<b>  </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14),
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= 0,showgrid = FALSE),
             yaxis = list(title = "<b> Frequency </b>",
                          titlefont = list(size = 16),
                          # change x-axix size
                          tickfont = list(size = 14),showgrid = FALSE)
             #tickvals = df_most_visited_service_month_year$most_visited_service)
             #hoverlabel=list(bgcolor="gainsboro")
             #width = 500, autosize=F,
             #bargap = 0.1, bargroupgap = 0.1,
      ) %>%
      config(displayModeBar = F,
             scrollZoom = T)


  })

  #data of barchart of Number of buckets collected by best zones
  data_bestzone <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        dplyr::count(bestzone_label, name = "n")%>%
        dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                      pct1 = paste0(percentage, "%"))
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          dplyr::count(bestzone_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          dplyr::count(bestzone_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

  #barchart of Number of buckets collected by best zones
  output$best_zone <- renderPlotly({
    validate(
      need(nrow(data_bestzone()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'of number of buckets collected by best zones')))
    plotly::plot_ly(data_bestzone(), x = ~bestzone_label,
                    type = "bar",
                    y = ~n,
                    #marker = list(color = c("#229954", "#D68910","#148F77", "#196F3D", "#21618C", "#7DCEA0", "#CA6F1E")),
                    #marker = list(color = c("#726DA8",  "#2774AE", "#1da1f2","#AFDBF5", "#367588", "#81D8D0", "#A0D2DB")),
                    marker = list(color = c("#6A5ACD", "#324AB2", "#018749", "#2774AE", "#008B8B")),
                    #colors = "darkviolet",
                    #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                    text = paste(data_bestzone()$n, sep = ""), textposition = 'outside',
                    hovertext = paste("Zone :",data_bestzone()$bestzone_label,
                                      "<br>Number of buckets :", data_bestzone()$n),
                    hoverinfo = 'text') %>%
      layout(title = "Number of buckets collected by best zones",
             legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b></b>')),
             uniformtext=list(minsize=5, mode='show'),
             xaxis = list(title = "<b> Best zones of collect </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14),
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= -45,showgrid = FALSE),
             yaxis = list(title = "<b> Frequency </b>",
                          titlefont = list(size = 16),
                          # change x-axix size
                          tickfont = list(size = 14),showgrid = FALSE)
             #tickvals = df_most_visited_service_month_year$most_visited_service)
             #hoverlabel=list(bgcolor="gainsboro")
             #width = 500, autosize=F,
             #bargap = 0.1, bargroupgap = 0.1,
      ) %>%
      config(displayModeBar = F,
             scrollZoom = T)

  })

  #data for the barchart of Quantity of buckets per best zones
  data_buckets_bzone <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        group_by(`Quantity(bucket)`, bestzone_label) %>%
        count(`Quantity(bucket)`)
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          group_by(`Quantity(bucket)`, bestzone_label) %>%
          count(`Quantity(bucket)`)
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          group_by(`Quantity(bucket)`, bestzone_label) %>%
          count(`Quantity(bucket)`)
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

  # barchart of Quantity of buckets per best zones
  output$buckets_bzone <- renderPlotly({
    validate(
      need(nrow(data_buckets_bzone()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'of quantity of buckets per best zones')))
    plotly::plot_ly(data_buckets_bzone(), x = ~`Quantity(bucket)`,
                    type = "bar",
                    y = ~n, color = ~bestzone_label,
                    #colors = c("#229954", "#D68910","#148F77", "#196F3D", "#21618C", "#7DCEA0", "#CA6F1E"),
                    #colors = c( "#720e9e","#EE82EE",  "#DDA0DD", "#D8BFD8"),
                    colors = c( "#4B61D1","#4169E1",  "#76ABDF", "#87CEEB"),
                    text = ~data_buckets_bzone()$n, textposition = 'outside',
                    hovertext = paste("Quantity of buckets :", data_buckets_bzone()$`Quantity(bucket)`,
                                      "<br>Zone :", data_buckets_bzone()$bestzone_label,
                                      "<br>Frequency :", data_buckets_bzone()$n
                    ),
                    hoverinfo = 'text') %>%
      layout(title = "Quantity of buckets per best zones",
             legend = list(x = 100, y = 0.60, title=list(color= "blue", text='<b>Best zones</b>')),
             uniformtext=list(minsize=15, mode='show'),
             xaxis = list(title = "<b> Quantity of buckets </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14),
                          # change x-title size
                          titlefont = list(size = 16),
                          tickangle= -45, tickvals = data_buckets_bzone()$`Quantity(bucket)`,showgrid = FALSE),
             yaxis = list(title = "<b> Frequency</b>",
                          titlefont = list(size = 16),
                          tickfont = list(size = 14),showgrid = FALSE)
      ) %>%
      config(displayModeBar = T, displaylogo = FALSE,  modeBarButtonsToRemove = list(
        'sendDataToCloud',
        #'toImage',
        #'autoScale2d',
        'toggleSpikelines',
        'resetScale2d',
        'lasso2d',
        'zoom2d',
        'pan2d',
        'select2d'#,
        #'hoverClosestCartesian'#,
        #'hoverCompareCartesian'
      ),
      scrollZoom = T)

  })

  #data for the barchart of Effect of chemicals on quantity of snails
  data_pestqt <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        dplyr::count(pestqty_label, name = "n")%>%
        dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                      pct1 = paste0(percentage, "%"))
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          dplyr::count(pestqty_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          dplyr::count(pestqty_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

  #barchart of Effect of chemicals on quantity of snails
  output$pestqty <- renderPlotly({
    validate(
      # Old Code
      #need(nrow(data_pestqt() = 0), 'No data exists, please select a Category'),
      # With parentheses fix
      #need(nrow(data_pestqt()) > 0, 'No data exists, please select a Category')
      need(nrow(data_pestqt()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'of effect of chemicals on quantity of snails'))
    )
    plotly::plot_ly(data_pestqt(), x = ~pestqty_label,
                    type = "bar",
                    y = ~percentage,
                    # marker = list(color = c("#229954", "#D68910","#148F77", "#196F3D", "#21618C",
                    #                         "#7DCEA0", "#CA6F1E")),
                    marker = list(color = c("#1da1f2", "#17B169")),
                    #colors = "darkviolet",
                    #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                    text = paste(data_pestqt()$pct1, sep = ""), textposition = 'outside',
                    hovertext = paste("Effect :",data_pestqt()$pestqty_label,
                                      "<br>Frequency :", data_pestqt()$n,
                                      "<br>Percentage :", data_pestqt()$pct1),
                    hoverinfo = 'text') %>%
      layout(title = "Effect of chemicals on quantity of snails",
             legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b></b>')),
             uniformtext=list(minsize=10, mode='show'),
             xaxis = list(title = "<b>  </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14),
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= 0,showgrid = FALSE),
             yaxis = list(title = "<b> Percentage </b>",
                          titlefont = list(size = 16),
                          # change x-axix size
                          tickfont = list(size = 14),
                          ticksuffix="%",showgrid = FALSE)
             #tickvals = df_most_visited_service_month_year$most_visited_service)
             #hoverlabel=list(bgcolor="gainsboro")
             #width = 500, autosize=F,
             #bargap = 0.1, bargroupgap = 0.1,
      ) %>%
      config(displayModeBar = F,
             scrollZoom = T)

  })

  #data for the barchart of Effect of chemicals on size of snails
  data_pestsize <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        dplyr::count(snailsize_label, name = "n")%>%
        dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                      pct1 = paste0(percentage, "%"))
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          dplyr::count(snailsize_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          dplyr::count(snailsize_label, name = "n")%>%
          dplyr::mutate(percentage = round(100*(n/sum(n)),2),
                        pct1 = paste0(percentage, "%"))
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

  #barchart of Effect of chemicals on size of snails
  output$pestsize <- renderPlotly({
    validate(
      need(nrow(data_pestsize()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'of effect of chemicals on size of snails')))

    plotly::plot_ly(data_pestsize(), x = ~snailsize_label,
                    type = "bar",
                    y = ~percentage,
                    # marker = list(color = c("#229954", "#D68910","#148F77", "#196F3D", "#21618C",
                    #                         "#7DCEA0", "#CA6F1E")),
                    marker = list(color = c("#0070BB", "#018749")),
                    #colors = "darkviolet",
                    #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                    text = paste(data_pestsize()$pct1, sep = ""), textposition = 'outside',
                    hovertext = paste("Effect :",data_pestsize()$snailsize_label,
                                      "<br>Frequency :", data_pestsize()$n,
                                      "<br>Percentage :", data_pestsize()$pct1),
                    hoverinfo = 'text') %>%
      layout(title = "Effect of chemicals on size of snails",
             legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b></b>')),
             uniformtext=list(minsize=10, mode='show'),
             xaxis = list(title = "<b>  </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14),
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= 0,showgrid = FALSE),
             yaxis = list(title = "<b> Percentage </b>",
                          titlefont = list(size = 16),
                          # change x-axix size
                          tickfont = list(size = 14),
                          ticksuffix = "%",showgrid = FALSE)
             #tickvals = df_most_visited_service_month_year$most_visited_service)
             #hoverlabel=list(bgcolor="gainsboro")
             #width = 500, autosize=F,
             #bargap = 0.1, bargroupgap = 0.1,
      ) %>%
      config(displayModeBar = F,
             scrollZoom = T)

  })

  #data for the barchart of Quantity of buckets per gender
  data_genderbucket <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        group_by(`Quantity(bucket)`, gender_label) %>%
        count(`Quantity(bucket)`)
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          group_by(`Quantity(bucket)`, gender_label) %>%
          count(`Quantity(bucket)`)
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          group_by(`Quantity(bucket)`, gender_label) %>%
          count(`Quantity(bucket)`)
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

  #barchart of Quantity of buckets per gender
  output$genderbucket <- renderPlotly({
    validate(
      need(nrow(data_genderbucket()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'of quantity of buckets per gender')))
    plotly::plot_ly(data_genderbucket(), x = ~`Quantity(bucket)`,
                    type = "bar",
                    y = ~n, color = ~gender_label,
                    #colors = c("#229954", "#D68910","#148F77", "#196F3D", "#21618C", "#7DCEA0", "#CA6F1E"),
                    colors = c("#4DB39A", "#6A5ACD"),
                    text = ~data_genderbucket()$n, textposition = 'outside',
                    hovertext = paste("Quantity of buckets :", data_genderbucket()$`Quantity(bucket)`,
                                      "<br>Gender :", data_genderbucket()$gender_label,
                                      "<br>Frequency :", data_genderbucket()$n
                    ),
                    hoverinfo = 'text') %>%
      layout(title = "Quantity of buckets per gender",
             legend = list(x = 100, y = 0.60, title=list(color= "blue", text='<b>gender</b>')),
             uniformtext=list(minsize=15, mode='show'),
             xaxis = list(title = "<b>  </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14),
                          # change x-title size
                          titlefont = list(size = 16),
                          tickangle= -45, tickvals = data_genderbucket()$`Quantity(bucket)`),
             yaxis = list(title = "<b> Frequency</b>",
                          titlefont = list(size = 16),
                          tickfont = list(size = 14))
      ) %>%
      config(displayModeBar = T, displaylogo = FALSE,  modeBarButtonsToRemove = list(
        'sendDataToCloud',
        #'toImage',
        #'autoScale2d',
        'toggleSpikelines',
        'resetScale2d',
        'lasso2d',
        'zoom2d',
        'pan2d',
        'select2d'#,
        #'hoverClosestCartesian'#,
        #'hoverCompareCartesian'
      ),
      scrollZoom = T)

  })

#data for the barchart of Quantity of buckets per origin region
  data_regionbucket <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        group_by(`Quantity(bucket)`, Region) %>%
        count(`Quantity(bucket)`)
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          group_by(`Quantity(bucket)`, Region) %>%
          count(`Quantity(bucket)`)
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          group_by(`Quantity(bucket)`, Region) %>%
          count(`Quantity(bucket)`)
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

  #barchart of Quantity of buckets per origin region
  output$regionbucket <- renderPlotly({
    validate(
      need(nrow(data_regionbucket()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'of quantity of buckets per origin region')))
    plotly::plot_ly(data_regionbucket(), x = ~`Quantity(bucket)`,
                    type = "bar",
                    y = ~n, color = ~Region,
                    #colors = c("#229954", "#D68910","#148F77", "#196F3D", "#21618C", "#7DCEA0", "#CA6F1E"),
                    # colors = c("#4B0082", "#483D8B", "slateblue", "#0077BE", "#5696CC", "#82B7FE", "#1da1f2",
                    #            "#76B7DA", "#A6DAFF", "#B7E5FF", "#AFDBF5", "#AFDBF5", "#E6E6FA"),
                    colors = c("#4B0082", "#483D8B", "slateblue", "#0077BE", "#5696CC", "#82B7FE", "#1da1f2"),
                    text = ~data_regionbucket()$n, textposition = 'outside',
                    hovertext = paste("Quantity of buckets :", data_regionbucket()$`Quantity(bucket)`,
                                      "<br>Region :", data_regionbucket()$Region,
                                      "<br>Frequency :", data_regionbucket()$n
                    ),
                    hoverinfo = 'text') %>%
      layout(title = "Quantity of buckets per origin region",
             legend = list(x = 100, y = 0.60, title=list(color= "blue", text='<b>Origin region</b>')),
             uniformtext=list(minsize=15, mode='show'),
             xaxis = list(title = "<b>  </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14),
                          # change x-title size
                          titlefont = list(size = 16),
                          tickangle= -45, tickvals = data_regionbucket()$`Quantity(bucket)`),
             yaxis = list(title = "<b> Frequency</b>",
                          titlefont = list(size = 16),
                          tickfont = list(size = 14))
      ) %>%
      config(displayModeBar = T, displaylogo = FALSE,  modeBarButtonsToRemove = list(
        'sendDataToCloud',
        #'toImage',
        #'autoScale2d',
        'toggleSpikelines',
        'resetScale2d',
        'lasso2d',
        'zoom2d',
        'pan2d',
        'select2d'#,
        #'hoverClosestCartesian'#,
        #'hoverCompareCartesian'
      ),
      scrollZoom = T)

  })

  #data for the barchart of Quantity of buckets per main activity
  data_mainactbucket <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f %>%
        group_by(`Quantity(bucket)`, mainact_label) %>%
        count(`Quantity(bucket)`)
      dat <- as.data.frame(dat)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          group_by(`Quantity(bucket)`, mainact_label) %>%
          count(`Quantity(bucket)`)
        dat <- as.data.frame(dat)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          group_by(`Quantity(bucket)`, mainact_label) %>%
          count(`Quantity(bucket)`)
        dat <- as.data.frame(dat)
      }
    }
  }, ignoreNULL = FALSE)

  #barchart of Quantity of buckets per main activity
  output$mainactbucket <- renderPlotly({
    validate(
      need(nrow(data_mainactbucket()) > 0, paste('No data exists for the combination region = ', input$loc,
                                          'and season = ', input$season, 'of quantity of buckets per main activity')))
    plotly::plot_ly(data_mainactbucket(), x = ~`Quantity(bucket)`,
                    type = "bar",
                    y = ~n, color = ~mainact_label,
                    #colors = c("#229954", "#D68910","#148F77", "#196F3D", "#21618C", "#7DCEA0", "#CA6F1E"),
                    colors = c("#F7C56F", "#4B61D1", "#0070BB", "#4DB39A", "#6A5ACD"),
                    text = ~data_mainactbucket()$n, textposition = 'outside',
                    hovertext = paste("Quantity of buckets :", data_mainactbucket()$`Quantity(bucket)`,
                                      "<br>Main activity :", data_mainactbucket()$mainact_label,
                                      "<br>Frequency :", data_mainactbucket()$n
                    ),
                    hoverinfo = 'text') %>%
      layout(title = "Quantity of buckets per main activity",
             legend = list(x = 100, y = 0.60, title=list(color= "blue", text='<b>Main activity</b>')),
             uniformtext=list(minsize=15, mode='show'),
             xaxis = list(title = "<b>  </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14),
                          # change x-title size
                          titlefont = list(size = 16),
                          tickangle= -45, tickvals = data_mainactbucket()$`Quantity(bucket)`),
             yaxis = list(title = "<b> Frequency</b>",
                          titlefont = list(size = 16),
                          tickfont = list(size = 14))
      ) %>%
      config(displayModeBar = T, displaylogo = FALSE,  modeBarButtonsToRemove = list(
        'sendDataToCloud',
        #'toImage',
        #'autoScale2d',
        'toggleSpikelines',
        'resetScale2d',
        'lasso2d',
        'zoom2d',
        'pan2d',
        'select2d'#,
        #'hoverClosestCartesian'#,
        #'hoverCompareCartesian'
      ),
      scrollZoom = T)

  })

  #data for the correlation matrix
  data_cormat <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      mat <- as.matrix(data_f %>% select(householdsize,Collectexp,`Quantity(bucket)`))
      res2 <- rcorr(mat)
      flat_matrix<-flattenCorrMatrix(round(res2$r,digits = 2), round(res2$P,digits = 2))
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc) %>%
          select(householdsize,Collectexp,`Quantity(bucket)`)
        mat <- as.matrix(dat)
        res2 <- rcorr(mat)
        flat_matrix<-flattenCorrMatrix(round(res2$r,digits = 2), round(res2$P,digits = 2))
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          select(householdsize,Collectexp,`Quantity(bucket)`)
        mat <- as.matrix(dat)
        res2 <- rcorr(mat)
        flat_matrix<-flattenCorrMatrix(round(res2$r,digits = 2), round(res2$P,digits = 2))
      }
    }
  }, ignoreNULL = FALSE)

  #correlation matrix
  output$cor_mat <- DT::renderDataTable({
    if (input$season !="rainy season" & input$season !="All") {
      datatable(data.frame(Message = "No correlation table because no data for the combination region:" ,
                           input$loc, " and season:", input$season))
    } else {
      datatable(data_cormat())
    }

  })

  # output$cor_mat <- DT::renderDataTable({
  #   data <- data_cormat()
  #   validate(
  #       need(nrow(data$mat) > 0, paste('No data exists for the combination region = ', input$loc,
  #                                           'and season = ', input$season, 'of correlation matrix')))
  #   datatable(data_cormat())
  #
  # })

  #data for the correlogram
  data_corr <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      data <- data_f %>% select(householdsize,Collectexp,`Quantity(bucket)`)
      cor <- cor(data)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        data <-data_f %>%
          filter(locality == input$loc) %>%
          select(householdsize,Collectexp,`Quantity(bucket)`)
        cor <- cor(data)
      } else {
        data <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season) %>%
          select(householdsize,Collectexp,`Quantity(bucket)`)
        cor <- cor(data)
      }
    }
  }, ignoreNULL = FALSE)

  #plot of the correlogram
  output$corr <- renderPlot({
    validate(
      need(data_corr()!="NA", paste('No data exists for the combination region = ', input$loc,
                                        'and season = ', input$season, 'for the correlation matrix')))
    corrplot(data_corr(), method="circle", order = "hclust", col=brewer.pal(n=8, name="RdBu"),tl.col="black")

  })

  #data for tests
  filteblue_datat1 <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f
      chi <- chisq.test(table(dat$edulevel_label, dat$pestqty_label))
      v <- unlist(chi[3], use.names=FALSE)
      v1 <- round(v, 2)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc)
        chi <- chisq.test(table(dat$edulevel_label, dat$pestqty_label))
        v <- unlist(chi[3], use.names=FALSE)
        v1 <- round(v, 2)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season)
        chi <- chisq.test(table(dat$edulevel_label, dat$pestqty_label))
        v <- unlist(chi[3], use.names=FALSE)
        v1 <- round(v, 2)
      }
    }
  }, ignoreNULL = FALSE)

  #test1
  output$test1 <- renderPrint({
    if (input$season !="rainy season" & input$season !="All") {
      paste("Not available because of no data")
    } else {
      if (filteblue_datat1()>0.05) {
        paste("The probability of the chi-2 test is:", filteblue_datat1(), "so the variables are not linked")
      } else {
        paste("The probability of the chi-2 test is:", filteblue_datat1(), "so there is a link between them")
      }
    }

  })

  filteblue_data2 <- eventReactive(input$action,{
    req(input$loc, input$season)
    if (input$loc == "All" & input$season =="All") {
      dat <- data_f
      chi <- chisq.test(table(dat$edulevel_label, dat$snailsize_label))
      v <- unlist(chi[3], use.names=FALSE)
      v1 <- round(v, 2)
    } else if (input$loc != "All") {
      if (input$season =="All") {
        dat <-data_f %>%
          filter(locality == input$loc)
        chi <- chisq.test(table(dat$edulevel_label, dat$snailsize_label))
        v <- unlist(chi[3], use.names=FALSE)
        v1 <- round(v, 2)
      } else {
        dat <-data_f %>%
          filter(locality == input$loc,
                 Season == input$season)
        chi <- chisq.test(table(dat$edulevel_label, dat$snailsize_label))
        v <- unlist(chi[3], use.names=FALSE)
        v1 <- round(v, 2)
      }
    }
  }, ignoreNULL = FALSE)

  output$test2 <- renderPrint({
    if (input$season !="rainy season" & input$season !="All") {
      paste("Not available because of no data")
    } else {
      if (filteblue_data2()>0.05) {
        paste("The probability of the chi-2 test is:", filteblue_data2(), "so the variables are not linked")
      } else {
        paste("The probability of the chi-2 test is:", filteblue_data2(), "so there is a link between them")
      }
    }

  })

  output$conclusion <- renderUI({
    HTML(paste("<div style='text-align: justify;'>","<span style='font-size: 20px;'>", "For this study, we surveyed <span style='color: #6A5ACD;'>211</span> snail collectors from major collecting areas such
               as  <span style='color: #6A5ACD;'>Limbe, Buea, Dibamba, Njombe Penja, Mbanga and Kribi </span>. The majority of collectors
               are men <span style='color: #6A5ACD;'>(around 70% men and 30% women)</span>. On average, these collectors live with four
               people and have an average experience of seven years in snail collecting. In terms of age, most collectors
               <span style='color: #6A5ACD;'>(41%)</span> are under <span style='color: #6A5ACD;'>30</span>, followed by those in the
               <span style='color: #6A5ACD;'>40-50</span> age bracket <span style='color: #6A5ACD;'>(25%)</span>, and very few
               <span style='color: #6A5ACD;'>(0.47%)</span> are over <span style='color: #6A5ACD;'>70</span>. In terms of marital status,
               <span style='color: #6A5ACD;'>49%</span> of collectors are married, while <span style='color: #6A5ACD;'>45%</span> are single.
               Around <span style='color: #6A5ACD;'>40% of collectors left school at secondary level</span>, and
               <span style='color: #6A5ACD;'>23% have reached tertiary level</span>.","</span>","</div>",
               "<div style='text-align: justify;'>","<span style='font-size: 20px;'>", tags$br(), "The main activity of the majority of collectors is agriculture <span style='color: #6A5ACD;'>(around 40%)</span>. Snail
     collection is mainly concentrated in the areas surrounding the village waste <span style='color: #6A5ACD;'>(89 buckets)</span>, with very
     little collection around houses <span style='color: #6A5ACD;'>(3 buckets)</span>. The months of <span style='color: #6A5ACD;'>October</span>
     record the highest amount of snail collection <span style='color: #6A5ACD;'>(63 buckets)</span>, while
     <span style='color: #6A5ACD;'>July</span> records the lowest. Snail collection takes place mainly <span style='color: #6A5ACD;'>at night
     (197 buckets)</span> compared with the <span style='color: #6A5ACD;'>morning (13 buckets)</span>.
     Some <span style='color: #6A5ACD;'>58%</span> of collectors believe that the use of chemicals affects the quantity of snails collected,
     and <span style='color: #6A5ACD;'>61%</span> believe that these chemicals influence snail size.", "</span>","</div>",
               "<div style='text-align: justify;'>", "<span style='font-size: 20px;'>", tags$br(), "The data indicate that there is no link between the quantity
                of snails collected and the collecting experience. Furthermore,
    the belief that the use of chemicals does or does not affect the quantity or size of snails does not depend on the collector's

    level of education. In other words, collection experience does not influence the quantity of snails collected, and education
    level has no impact on collectors' perception of the effect of chemicals on snail quantity or size.","</span>","</div>"
               ))
                # sep="<br/>"    ))
  })

}

