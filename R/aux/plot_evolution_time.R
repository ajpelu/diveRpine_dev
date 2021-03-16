output$plot_evolution <- renderPlotly({

  df <- data.frame()
  for (i in seq(1:input$timeRange)){
    aux <- rich_pp() + propagule()*i
    aux.df <- data.frame(
      time = i,
      richness = cellStats(aux, "mean"),
      sd =  cellStats(aux, "sd"),
      veg = "pp"
    )
    df <- rbind(df, aux.df)
  }

  dfnf <- data.frame(time= seq(1:input$timeRange),
                     mean = cellStats(rich_nf(), "mean"),
                     sd =  cellStats(rich_nf(), "sd"),
                     veg ="nf")

  g <- ggplot(dfnf, aes(x=time, y=mean)) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean +sd), fill = "#F39C12", alpha=0.4) +
    geom_line(col="#F39C12", size=3) +
    geom_ribbon(data = df, aes(ymin = mean - sd, ymax = mean +sd), fill = "#0A9650", alpha=.4) +
    geom_line(data = df, col="#0A9650", size=3) +
    theme_minimal() +
    xlab("Years")
})
