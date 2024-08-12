library(shiny)
library(networkD3)
library(dplyr)
library(igraph)
library(visNetwork)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(rintrojs)
library(ggplot2)
library(qdapTools)
library(RColorBrewer)
library(tibble)
library(htmlwidgets)
library(ggtext)

server <- function(input, output, session){
  ###########################
  ### Layout ###
  ###########################

  # Navbar ------------------------------------------------------------------
  shinyjs::addClass(id = "navBar", class = "navbar-right")
  
  # DT Options --------------------------------------------------------------
  options(DT.options = list( lengthMenu = c(10, 20),
                             dom = 'tl'
  ))  # table and lengthMenu options
  

   observeEvent(input$btn_MA, {
    shinyjs::runjs('fakeClick("MA")')
  })
  observeEvent(input$btn_WH, {
    shinyjs::runjs('fakeClick("WH")')
  })
  observeEvent(input$btn_fa, {
    shinyjs::runjs('fakeClick("fa")')
  })
  observeEvent(input$btn_IP, {
    shinyjs::runjs('fakeClick("IP")')
  })
  observeEvent(input$btn_DIY, {
    shinyjs::runjs('fakeClick("DIY")')
  })

  ###########################
  ### Home ###
  ###########################

  main_nodes <- data.frame(id = 1:6, 
                           label = c("Home", "Database", "Steps", "Steps: Parameters", "Individual Article", "Your Own Pipeline"), 
                           value = c(60, 60, 60, 60, 60, 60), 
                           title = "Click to see information", 
                           shape = "dot",
                           color = "#174c92c9")
  main_edges <- data.frame(from = c(1, 1, 1, 1, 1), 
                           to = c(2, 3, 4, 5, 6))

  output$network_home <- renderVisNetwork({
    visNetwork(main_nodes, main_edges, width = "100%") %>%
      visEvents(click = "function(properties) {
        var nodeId = properties.nodes[0];
        if(nodeId) {
          var label = this.body.data.nodes.get(nodeId).label;
          Shiny.onInputChange('node_clicked', label);
        }
      }")
  })

  observeEvent(input$node_clicked, {
    if (input$node_clicked == "Database") {
      subnodes <- data.frame(id = 7:11, 
                             label = c("PRISMA", "List Review1", "List Review2", "List Steps", "List Parameters"), 
                             value = c(30, 30, 30, 30, 30), 
                             title = "Click to see information", 
                             shape = "dot", 
                             color = "lightblue")
      subedges <- data.frame(from = rep(2, 5), 
                             to = 7:11)
      
      visNetworkProxy("network_home") %>%
        visUpdateNodes(nodes = subnodes) %>%
        visUpdateEdges(edges = subedges)
    } 
    
    else if (input$node_clicked == "Steps") {
    subnodes <- data.frame(id = 12:15, 
                           label = c("Aggregated Pipelines", "Individual Steps", "Combination", "Order"), 
                           value = c(30, 30, 30, 30), 
                           title = "Click to see information", 
                           shape = "dot", 
                           color = "lightblue")
    subedges <- data.frame(from = rep(3, 4), 
                           to = 12:15)
    
    visNetworkProxy("network_home") %>%
      visUpdateNodes(nodes = subnodes) %>%
      visUpdateEdges(edges = subedges)
    }

    else if (input$node_clicked == "Individual Article") {
    subnodes <- data.frame(id = 16:17, 
                           label = c("Step Visualization", "Parameter Visualization"), 
                           value = c(30, 30), 
                           title = "Click to see information", 
                           shape = "dot", 
                           color = "lightblue")
    subedges <- data.frame(from = rep(5, 2), 
                           to = 16:17)
    
    visNetworkProxy("network_home") %>%
      visUpdateNodes(nodes = subnodes) %>%
      visUpdateEdges(edges = subedges)
    }

    info <- switch(input$node_clicked,
      "Database" = "The multiverse has been identified by mining the literature. All information, the article information, and the preprocessing steps and their respective parameters can be found here.",
      "Steps" = "Explore which preprocessing steps have been used and which combinations and orders are common.",
      "Steps: Parameters" = "Explore which parameters for the respective preprocessing steps have been used by previous studies.",
      "Individual Article" = "Check out preprocessing pipelines and their chosen parameters for individual article.",
      "Your Own Pipeline" = "Construct your own pipeline and compare it to the ones in the database.",
      "Home" = "Home is this page. Click the Home button at the top to go back to this page and explore other features.",
      "PRISMA" = "Details of the included artcles reported in PRISMA diagram",
      "List Review1" = "List of included articles for fMRI preprocessing steps (review 1).",
      "List Review1" = "List of included articles for graph fMRI and behavior studies (review 2).",
      "List Steps" = "List of identified steps",
      "List Parameters" = "List of identified parameters",
      "Aggregated Pipelines" = "Explore the aggregated pipelines across studies",
      "Individual Steps" = "Explore how individual steps are used across studies",
      "Combination" = "Explore pairs of preprocessing steps used in combination.",
      "Order" = "Explore the order of pairs of preprocessing steps.",
      "Step Visualization" = "Visualize the usage of steps in individual articles.",
      "Parameter Visualization" = "Visualize the usage of parameters in individual articles."
    )

    output$node_description <- renderUI({
      wellPanel(
        style = "background-color: #f0f0f0; padding: 10px; border: 1px solid #ddd;",  # Light background with padding and border
        div(style = "font-size: 18px; font-weight: bold;", input$node_clicked),  # Smaller title
        div(style = "font-size: 14px;", info),
        div(style = "margin-top: 20px;", actionButton("go_to_tab", "Go to Tab"))  # Space between text and button
      )
    })
  })

  observeEvent(input$go_to_tab, {
    tab_value <- switch(input$node_clicked,
      "Database" = "MA",
      "Steps" = "WH",
      "Steps: Parameters" = "fa",
      "Individual Article" = "IP",
      "Your Own Pipeline" = "DIY",
      "Home" = "home",
      "PRISMA" = "MA",
      "List Review1" = "MA",
      "List Review2" = "MA",
      "List Steps" = "MA",
      "List Parameters" = "MA",
      "Aggregated Pipelines" = "WH",
      "Individual Steps" = "WH",
      "Combination" = "WH",
      "Order" = "WH",
      "Step Visualization" = "IP",
      "Parameter Visualization" = "IP",
      "home"  # Default to home if no match
    )
    updateTabsetPanel(session, "navBar", selected = tab_value)
  })

  ###########################
  ### Database ###
  ###########################

    output$list_paper <- DT::renderDataTable(
    p_inf, escape = FALSE,
    #extensions = "FixedHeader",
    style="bootstrap",
    filter = 'top',
    options = list(
      dom = 'Bfrtip',
      pageLength = 20,
      scrollX=TRUE,
      autoWidth = TRUE,
      paging=TRUE,
      searching=TRUE,
      ordering=TRUE
      #fixedHeader = TRUE,
    )
  )

  output$list_paper1 <- DT::renderDataTable(
    p_inf1, escape = FALSE,
    #extensions = "FixedHeader",
    style="bootstrap",
    filter = 'top',
    options = list(
      dom = 'Bfrtip',
      pageLength = 20,
      scrollX=TRUE,
      autoWidth = TRUE,
      paging=TRUE,
      searching=TRUE,
      ordering=TRUE
      #fixedHeader = TRUE,
    )
  )

  output$list_steps <- DT::renderDataTable(
    steps,
    #extensions = "FixedHeader",
    style="bootstrap",
    filter = 'top',
    options = list(
      dom = 'Bfrtip',
      pageLength = 20,
      scrollX=TRUE,
      autoWidth = TRUE,
      paging=TRUE,
      searching=TRUE,
      ordering=TRUE
      #fixedHeader = TRUE,
    )
  )
  output$list_decisions <- DT::renderDataTable(
    steps_op,
    #extensions = "FixedHeader",
    style="bootstrap",
    filter = 'top',
    options = list(
      dom = 'Bfrtip',
      pageLength = 20,
      scrollX=TRUE,
      autoWidth = TRUE,
      paging=TRUE,
      searching=TRUE,
      ordering=TRUE
      #fixedHeader = TRUE,
    )
  )
  

  ###########################
  ### Steps ###
  ###########################

  output$selected_paper <- renderText({ 
  sel_pap_st <- input$selectPapers
  pap_st <- p_inf[p_inf$Key == sel_pap_st, ]
  paste("You have selected this article:", "Author:", pap_st$Author,
        "Title:", pap_st$Title, "Year:", pap_st$Year,
        "Publisher:", pap_st$Publisher)
  })

  output$table_step <- DT::renderDataTable({
  sel_p <- input$selectPapers
  pipes_p <- dat_vis[which(dat_vis$Key == sel_p), ]
  pipes_p <- pipes_p %>%
    select(where(~ all(!is.na(.))))
  datatable(pipes_p,
            options = list(
              dom = 'Bfrtip',
              pageLength = 1,
              scrollX = TRUE,
              autoWidth = TRUE,
              paging = TRUE,
              searching = TRUE,
              ordering = TRUE
              # fixedHeader = TRUE
            )
  )
  })

    output$WP <- renderForceNetwork({
    thr <- input$Thr
    ndWP <- input$Node_WP
    nodes2$size2 <- nodes2$size/(max(nodes2$size)/50)
    links2$color <- "gray"
    links2$color[links2$value>5] <- "blue"
    links2$color[links2$value>20] <- "red"
    links2 <- links2[links2$value > thr, ]
    links2$value2 <- links2$value/(max(links2$value)/10)
    
    links3 <- data.frame(source = match(links2$source, nodes2$Names) - 1,
                         target = match(links2$target, nodes2$Names) - 1,
                         value = links2$value,
                         value2 = links2$value2)
    if (ndWP == "All"){
      links_vis <- links3
    }
    else {
      ndWP <- nodes$Names[nodes$Names_vis==ndWP]
      id_ndWP <- which(nodes2$Names == ndWP)-1
      links_vis <- links3[links3$source == id_ndWP | links3$target == id_ndWP, ]
    }
    
    script <- 'alert("Name: " + d.Names + "\\n" +
              "Definiton: " + d.Definition + "\\n" + "Used by: " + d.size + "articles out of 220 articles");'
    
    fn = forceNetwork(Links = links_vis, Nodes = nodes2,
                 Source = "source", Target = "target",
                 Value = "value", NodeID = "Names_vis", linkWidth = JS("function(d) { return Math.sqrt(d.value)/3; }"),
                 Nodesize = "size2", Group = "Groups", radiusCalculation = JS("Math.sqrt(d.nodesize)+6"),
                 opacity = 0.9, zoom = TRUE, fontSize = 8, arrows = TRUE, legend = TRUE, bounded = TRUE, clickAction = script, linkDistance = 100, 
                 colourScale = JS('d3.scaleOrdinal().domain(["Software","Structural_preprocessing", "Functional_preprocessing", "Noise_removal", "FC_def", "Graph_analysis"]).
                                  range(["#808080", "#1f77b4", "#2ca02c", "#ff7f0e", "#9467bd", "#e31a1c"])'))
    
    fn$x$nodes$Names <- nodes2$Names_vis
    fn$x$nodes$Definition <- nodes2$Definition
    fn$x$nodes$size <- nodes2$size
    #radiusCalculation: "Math.sqrt(d.nodesize)+6"
    
    htmlwidgets::onRender(fn, jsCode = '
    function (el, x) {
          d3.select("svg").append("g").attr("id", "legend-layer");
          var legend_layer = d3.select("#legend-layer");
          d3.selectAll(".legend")
            .each(function() { legend_layer.append(() => this); });
          d3.select(el)
            .selectAll(".link")
            .append("title")
            .text(d => d.value);
          var link = d3.selectAll(".link")
          var node = d3.selectAll(".node")
      
          var options = { opacity: 1,
                          clickTextSize: 10,
                          opacityNoHover: 0.1,
                        }
      
          var unfocusDivisor = 4;
      
          var links = HTMLWidgets.dataframeToD3(x.links);
          var linkedByIndex = {};
      
          links.forEach(function(d) {
            linkedByIndex[d.source + "," + d.target] = 1;
            linkedByIndex[d.target + "," + d.source] = 1;
          });
      
          function neighboring(a, b) {
            return linkedByIndex[a.index + "," + b.index];
          }
      
      
          function mouseover(d) {
            var unfocusDivisor = 4;
      
            link.transition().duration(200)
              .style("opacity", function(l) { return d != l.source && d != l.target ? +options.opacity / unfocusDivisor : +options.opacity });
      
            node.transition().duration(200)
              .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? +options.opacity : +options.opacity / unfocusDivisor; });
      
      
            node.select("text").transition()
              .duration(750)
              .attr("x", 13)
              .style("stroke-width", ".5px")
              .style("font", 5 + "px ")
              .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? 1 : 0; });
          }
      
          function mouseout() {
            node.style("opacity", +options.opacity);
            link.style("opacity", +options.opacity);
      
            node.select("text").transition()
              .duration(1250)
              .attr("x", 0)
              .style("font", options.fontSize + "px ")
              .style("opacity", 0);
          }
      
          d3.selectAll(".node").on("mouseover", mouseover).on("mouseout", mouseout);
            
      }')
    
    
  })

    output$plot_IS_crowd <- renderPlot({
    # Create the lollipop plot
    st_dat <- nodes2
    st_dat$ID <- nodes$ID
    st_dat$col <- nodes$col
    st_dat <- st_dat %>%
      mutate(Names_vis = factor(Names_vis, levels = unique(Names_vis)))  # Convert Names_vis to a factor with original order
    # Limit y-axis labels to 5 characters
    #st_dat$Names_or <- stringr::str_sub(st_dat$Names_or, 1, 20)
    par(mar=c(10,4,4,1)+.1)
    color_map <- setNames(st_dat$col, st_dat$Groups)

    ggplot(st_dat, aes(x = Names_vis, y = size, color = Groups)) +
      geom_segment(aes(xend = Names_vis, yend = 0), size = 1) +
      geom_point(size = 4, alpha = 0.6) +
      geom_text(aes(label = size), vjust = 0.5, hjust = -0.5, size = 4, color = "black") +
      scale_color_manual(values = color_map) +  # Use the color map
      scale_x_discrete(limits = rev(levels(st_dat$Names_vis))) + 
      theme_light() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12, family = "Arial"),
        axis.text.y = element_text(),
      ) +
      coord_flip() +
      labs(
        x = "Steps",
        y = "Number of articles (out of 220 articles) used the step",
        color = "Groups"
      )
  })

   output$selected_IS <- renderText({
    paste("You have selected step", input$select_IS, "which was used by articles:")
  })
  
  
  output$table_IS <- DT::renderDataTable({
    dec_IS <- input$select_IS
    dec_IS <- nodes$Names[which(nodes$Names_vis == dec_IS)]
    id_dec_IS <- which(apply(dat, 1, function(x2) all(dec_IS %in% x2)))
    new_tab_IS <- p_inf[id_dec_IS, ]
    
    datatable(new_tab_IS, escape = FALSE,
              filter = 'top',
              options = list(
                dom = 'Bfrtip',
                pageLength = 10,
                scrollX = TRUE,
                autoWidth = TRUE,
                paging = TRUE,
                searching = TRUE,
                ordering = TRUE,
                columnDefs = list(
                  list(width = '300px', targets = c(2,3))
                )
                # fixedHeader = TRUE
              )
    )
  })
  
  output$plot_YN <- renderPlot(
  width = 800, height = 800, res = 100,
  {
    input$newplot
    st_sel <- input$selectDecisionYN
    st_dat <- mat_yn[st_sel, ]
    st_dat <- data.frame(st_dat)
    st_dat$name <- row.names(st_dat)
    colnames(st_dat) <- c("value", "name")
    st_dat$Groups <- nodes$Groups
    st_dat$Groups <- factor(st_dat$Groups, levels = unique(st_dat$Groups))
    st_dat$col <- nodes$col
    st_dat <- st_dat %>%
      mutate(Names_or = fct_reorder(name, desc(nodes$ID)))
    label_colors <- ifelse(st_dat$Names_or == st_sel, "red", "black")
    names(label_colors) <- st_dat$Names_or
    par(mar=c(10,4,4,1)+.1)

    # Create the ggplot
    ggplot(st_dat, aes(x = Names_or, y = value, color = Groups)) +
      geom_segment(aes(xend = Names_or, yend = 0), size = 1) +
      geom_point(size = 4, alpha = 0.6) +
      geom_text(aes(label = value), vjust = 0.5, hjust = -0.5, size = 4, color = "black") +
      scale_color_manual(values = unique(st_dat$col)) +
      #scale_y_discrete(labels = st_dat$Names_or, breaks = st_dat$Names_or, limits = st_dat$Names_or) +
      theme_light() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12, family = "Arial"),
        axis.text.y = element_text(color = label_colors[st_dat$Names_or]),
      ) +
      coord_flip() +
      labs(
        x = "Steps",
        y = paste("Number of articles (out of 220 articles) used it together with", st_sel),
        color = "Groups"
      )
  })
  
  output$plot_OR <- renderPlot(
  width =800, height = 800, res = 100,
  {
    input$newplot
    st_sel_OR <- input$selectDecisionOR
    st_dat_OR <- mat_or[st_sel_OR, ]
    st_dat_OR <- data.frame(st_dat_OR)
    st_dat_OR$name <- row.names(st_dat_OR)
    colnames(st_dat_OR) <- c("value", "name")
    st_dat_OR$Groups <- nodes$Groups
    st_dat_OR$Groups <- factor(st_dat_OR$Groups, levels = unique(st_dat_OR$Groups))
    st_dat_OR$col <- nodes$col
    st_dat_OR <- st_dat_OR %>%
      mutate(Names_or = fct_reorder(name, desc(nodes$ID)))
    label_colors <- ifelse(st_dat_OR$Names_or == st_sel_OR, "red", "black")
    names(label_colors) <- st_dat_OR$Names_or
    par(mar=c(10,4,4,1)+.1)
    ggplot(st_dat_OR, aes(x = Names_or, y = value, color = Groups)) +
      geom_segment(aes(xend = Names_or, yend = 0), size = 1) +
      geom_point(size = 4, alpha = 0.6) +
      geom_text(aes(label = value), vjust = 0.5, hjust = -0.5, size = 4, color = "black") +
      scale_color_manual(values = unique(st_dat_OR$col)) +
      #scale_y_discrete(labels = st_dat_OR$Names_or, breaks = st_dat_OR$Names_or, limits = st_dat_OR$Names_or) +
      theme_light() +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12, family = "Arial"),
        axis.text.y = element_text(color = label_colors[st_dat_OR$Names_or]),
      ) +
      labs(
        x = "Steps",
        y = paste("Number of articles (out of 220 articles) used it after", st_sel_OR),
        color = "Groups"
      )
  })

  ###########################
  ### Steps: Parameters ###
  ###########################

  output$selected_paper_cv <- renderText({ 
  sel_pap_cv <- input$selectPapers_cv
  pap_cv <- p_inf[p_inf$Key == sel_pap_cv, ]
  paste("You have selected this article:", "Author:", pap_cv$Author,
        "Title:", pap_cv$Title, "Year:", pap_cv$Year,
        "Publisher:", pap_cv$Publisher)
  })
  
  output$table_option <- DT::renderDataTable({
  sel_p_op <- input$selectPapers_cv
  pipes_p_op <- dat_op_or_vis[which(dat_op_or_vis$Key == sel_p_op), ]
  pipes_p_op <- pipes_p_op %>%
    select(where(~ all(!is.na(.))))
  datatable(pipes_p_op,
            options = list(
              dom = 'Bfrtip',
              pageLength = 1,
              scrollX = TRUE,
              autoWidth = TRUE,
              paging = TRUE,
              searching = TRUE,
              ordering = TRUE
              # fixedHeader = TRUE
            )
  )
  })

  output$plot_group_decision <- renderPlot(
  width = 450, height = 450, res = 100,
  {
    input$newplot
    gr_dec <- input$selectGroup
    gr_dec <- nodes$Names[nodes$Names_vis == gr_dec]
    gr_dat <- dat_op_or[ ,c(gr_dec)]
    gr_ds <- colSums(mtabulate(gr_dat))
    gr_ds <- data.frame(gr_ds)
    gr_ds$name <- row.names(gr_ds)
    colnames(gr_ds) <- c("value", "name")
    gr_ds$name <- fct_relevel(gr_ds$name, "Not_reported", "Not_used")
    custom_colors <- c("Not_reported" = "red", "Not_used" = "blue")
    
    par(mar=c(10,4,4,1)+.1)
    ggplot(gr_ds, aes(x = name, y = value)) +
      geom_segment(aes(xend = name, yend = 0, color = name), size = 1) +
      geom_point(aes(color = name), size = 4, alpha = 0.6) +
      geom_text(aes(label = value), vjust = 0.5, hjust = -0.5, size = 4, color = "black") +
      scale_color_manual(values = custom_colors, guide = "none") +  # Remove the legend
      theme_light() +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12, family = "Arial")
      ) +
      labs(x = "Parameters", y = "Number of articles (out of 220 articles)")
    
  })
  
  output$selectDecision <- renderUI({
    st_sel <- input$selectGroup
    opts2 <- which(nodes_op$Groups_vis==st_sel)
    opts_2 <- nodes_op[opts2, ]
    selectInput("selectDecision2",
                label   = "Select the parameter",
                choices =  c(opts_2$Names, "Not_used", "Not_reported"),
                selected = opts_2$Names[1]
    )
  })
  
  output$selected_decision <- renderText({
    dec <- input$selectDecision2
    id_dec <- which(dat_op == dec, arr.ind = T)
    paste("You have selected parameter of", input$selectDecision2, "which was used by articles:")
  })

  output$table <- DT::renderDataTable({
  dec <- input$selectDecision2
  dec1 <- input$selectGroup
  dec1 <- nodes$Names[which(nodes$Names_vis == dec1)]
  dat_op_or_sel <- dat_op_or[, dec1]
  id_dec <- which(dat_op_or_sel == dec, arr.ind = TRUE)
  new_tab <- p_inf[id_dec[, 1], ]
  
  datatable(new_tab, escape = FALSE,
            filter = 'top',
            options = list(
              dom = 'Bfrtip',
              pageLength = 10,
              scrollX = TRUE,
              autoWidth = TRUE,
              paging = TRUE,
              searching = TRUE,
              ordering = TRUE,
              columnDefs = list(
                list(width = '300px', targets = c(2,3))
              )
              # fixedHeader = TRUE
            )
  )
  })
  

  ###########################
  ### Individual Article ###
  ###########################

  output$plot <- renderPlot(
  width = 1000, height = 800, res = 100,
  {
    input$newplot
    sel_p <- input$selectPapers
    id_p <- which(dat$Key == sel_p)
    links_ind <- list_df[[id_p]]
    g <- graph_from_data_frame(d=links_ind, vertices=nodes, directed=T)
    first_nd <- links_ind$source[1]
    id_fn <- which(nodes$Names == first_nd)
    V(g)$color <- nodes$col
    V(g)$label <- nodes2$Names_vis
    #V(g)$color[id_fn] <- "white"
    cdi <- readRDS("coordinates_steps_shiny3.RDS")
    cdi <- cdi[1:nrow(cdi)-1, ]
    plot(g, 
          layout = cdi,
          vertex.frame.color = "black",                 # Node border color
          vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
          vertex.size=10,                               # Size of the node (default is 15)
          
          # === vertex label
          vertex.label.color="black",
          vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
          vertex.label.cex=0.7,                           # Font size (multiplication factor, device-dependent)
          vertex.label.dist=0,                          # Distance between the label and the vertex
          vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
          
          # === Edge
          edge.color="blue",                            # Edge color
          edge.width=1,                                 # Edge width, defaults to 1
          edge.arrow.size=0.6,                            # Arrow size, defaults to 1
          edge.arrow.width=3,                           # Arrow width, defaults to 1
          edge.lty="solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
          edge.curved=0.3    ,                          # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
          asp = 0.7
    )
    
    legend(x=-1, y=-1.1, c("FC definition","Functional_preprocessing", "Graph_analysis", "Noise_removal","Software", "Structural_preprocessing"), pch=21,
            col="#777777", pt.bg=clrs, pt.cex=2, cex=1, bty="n", ncol=1)
    
  })

  output$plot_cv <- renderPlot(
  width = 1000, height = 800, res = 100,
  {
    input$newplot
    sel_p_op <- input$selectPapers_cv
    id_p_op <- which(dat_op$Key == sel_p_op)
    links_op <- list_df_op[[id_p_op]]
    g_op <- graph_from_data_frame(d=links_op, vertices=nodes_op, directed=T)
    first_nd_op <- links_op$source[1]
    id_fn_op <- which(nodes_op$Names == first_nd_op)
    V(g_op)$color <- clrs_op[V(g_op)$Groups.type]
    V(g_op)$color[id_fn_op] <- "white"
    cdi_op <- readRDS("coordinates_options_shiny3.RDS")
    cdi_op <- cdi_op[1:nrow(cdi_op)-1, ]
    plot(g_op, 
          layout = cdi_op,
          vertex.frame.color = "black",                 # Node border color
          vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
          vertex.size=10,                               # Size of the node (default is 15)
          
          # === vertex label
          vertex.label.color="black",
          vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
          vertex.label.cex=0.7,                           # Font size (multiplication factor, device-dependent)
          vertex.label.dist=0,                          # Distance between the label and the vertex
          vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
          
          # === Edge
          edge.color="blue",                            # Edge color
          edge.width=1,                                 # Edge width, defaults to 1
          edge.arrow.size=0.6,                            # Arrow size, defaults to 1
          edge.arrow.width=3,                           # Arrow width, defaults to 1
          edge.lty="solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
          edge.curved=0.3    ,                          # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
          asp = 0.7
    )
    
    legend(x=-0.5, y=-1.1, c("Atlas definition","Combine block", "Compute Connectivity", "Head motion correction", "Motion_regression",
                              "Removal initial volume","Spatial normalization", "Spatial smoothing", "Temporal detrending", "Temporal filtering",
                              "Graph characteristic","Negative correlation", "Network definition", "Result aggregation", "Software", "Sparsity control", "Time series ROI"), pch=21,
            col="#777777", pt.bg=clrs_op, pt.cex=2, cex=.8, bty="n", ncol=3)
    
  })

  ###########################
  ### Your Own Pipeline ###
  ###########################
    
  output$selectDecision_DIY <- renderUI({
    st_sel_DIY <- input$selectStep_DIY
    opts <- which(nodes_op$Groups_vis==st_sel_DIY)
    opts_ <- nodes_op[opts, ]
    selectInput("selectDecision_DIY2",
                label   = "Select the parameter",
                choices =  c(opts_$Names,"Any"),
                selected = opts_$Names[1]
    )
  })
  
  
  tableValues <- reactiveValues(df = data.frame(Names = as.character(), Parameters = as.character(), 
                                                check.names = FALSE))
  
  
  observeEvent(input$add, {
    selected_row <- input$table_DIY_rows_selected
    temp <- tableValues$m
    if (length(selected_row) == 0) {
      # If no row is selected, append the new row to the end
      newRow <- data.frame(Names = input$selectStep_DIY, Parameters = input$selectDecision_DIY2, 
                           check.names = FALSE)
      temp <- rbind(temp, newRow)
    } else {
      # If a row is selected, insert the new row after the selected row
      newRow <- data.frame(Names = input$selectStep_DIY, Parameters = input$selectDecision_DIY2, 
                           check.names = FALSE)
      temp <- rbind(temp[1:selected_row, ], newRow, temp[(selected_row + 1):nrow(temp), ])
    }
    rownames(temp) <- NULL
    tableValues$m <- temp
    
  })
  
  observeEvent(input$delete,{
    selected_row <- input$table_DIY_rows_selected
    temp <- tableValues$m
    
    if (length(selected_row) == 0) {
      temp <- head(temp,-1)
      
    } else {
      temp  <- temp [-as.numeric(input$table_DIY_rows_selected),]
    }
    rownames(temp) <- NULL
    tableValues$m <- temp
  })
  
  
  observeEvent(input$count, {
    # # Check if tableValues is empty
    # if(nrow(tableValues$m) == 0) {
    #   # If it is, use another table where Names is "Software" and Options is "Any"
    #   tableValues <- data.frame(Names = "Software", Options = "Any", check.names = FALSE)
    # }

    table_DIY <- tableValues$m
    step_DIY1 <- c(table_DIY$Names)
    step_DIY <- nodes$Names[match(step_DIY1, nodes$Names_vis, nomatch = 0)] 
    #step_DIY <- nodes$Names[sapply(nodes$Names_vis, function(x) x %in% step_DIY1)]
    option_DIY <- c(table_DIY$Parameters)
    option_DIY <- option_DIY[option_DIY!=""]
    
    ###Find only step first
    order_not <- input$order
    if (order_not == T){
      row_stepDIY <- which(apply(dat, 1, function(x1) {
        if (length(x1) < length(step_DIY)) {
          return(FALSE)
        }
        idx <- match(step_DIY, x1)
        all(!is.na(idx)) && all(diff(idx) == 1)
      }))
    }
    else {
      row_stepDIY <- which(apply(dat, 1, function(x2) all(step_DIY %in% x2)))
    }
    
    paper_opt <- dat_op[row_stepDIY, ]
    
    ###Find also with option
    id_D_all <- list()
    for (na in 1:length(option_DIY)){
      opt_D <- option_DIY[na]
      if (opt_D == "Any"){
        id_D_all[[na]] <- 1:nrow(dat_op_or)
      }
      
      else {
        st_D1 <- table_DIY$Names[table_DIY$Parameters == opt_D]
        st_D <- nodes$Names[nodes$Names_vis %in% st_D1]
        id_D <- which(dat_op_or[,st_D] == opt_D)
        id_D_all[[na]] <- id_D
      }
    }
    
    vals <- unlist(id_D_all)
    row_optDIY <- which(tabulate(vals) >= length(id_D_all))
    row_finDIY <- intersect(row_stepDIY, row_optDIY)
    count <- length(row_finDIY)
    
    output$counted_paper <- renderText({
      paste(c("Your selected pipeline is used by", count, "articles (out of 220 articles):"), collapse = " ")
    })
    
    output$table_DIY2 <- DT::renderDataTable({
      table_DIYfin <- p_inf[row_finDIY, ]
      datatable(table_DIYfin, escape = FALSE,
                filter = 'top',
                options = list(
                  dom = 'Bfrtip',
                  pageLength = 20,
                  scrollX = TRUE,
                  autoWidth = TRUE,
                  paging = TRUE,
                  searching = TRUE,
                  ordering = TRUE
                  # fixedHeader = TRUE
      ))
    })

    ###Output plot time
    output$plot_time <- renderPlot(
      width = 1000, height = 400, res = 100,
      {
        table_DIYfin <- p_inf[row_finDIY, ]

        # Count the number of papers for each year for the selected pipeline
        count_data <- table(table_DIYfin$Year)

        # Create a data frame from the count data
        count_df <- data.frame(
          Year = as.numeric(names(count_data)),
          Count = as.vector(count_data)
        )

        # Count the number of papers for each year for all papers
        total_count_data <- table(p_inf$Year)

        # Create a data frame from the total count data
        total_count_df <- data.frame(
          Year = as.numeric(names(total_count_data)),
          Count = as.vector(total_count_data)
        )

        # Filter the data for the years 2010 to 2023
        count_df <- count_df[count_df$Year >= 2010 & count_df$Year <= 2023, ]
        total_count_df <- total_count_df[total_count_df$Year >= 2010 & total_count_df$Year <= 2023, ]

        # Create the plot
        ggplot() +
          geom_bar(data = total_count_df, aes(x = Year, y = Count), stat = "identity", fill = "gray", alpha = 0.3, width = 0.5) +
          geom_line(data = count_df, aes(x = Year, y = Count), color = "#0703e070", size = 1) +
          scale_x_continuous(breaks = 2010:2023) +  # Set the x-axis breaks
          expand_limits(x = 2010:2023) +  # Expand the x-axis limits
          labs(x = "Year", y = "Number of articles") +
          theme_minimal() +
          theme(panel.grid = element_blank())
        }
      )
  
  })
  
  output$table_DIY <- DT::renderDataTable({
    table_data <- tableValues$m
    datatable(table_data,
              options = list(
                "pageLength" = 30)
    )
  })
  
    
  output$download <- downloadHandler(
    filename = function(){"table.csv"},
    content = function(fname){
      table_data <- tableValues$m
      write.csv(table_data, fname)
    }
  )
    

}


  ###########################
  ### Add on ###
  ###########################

    # # Render the force network plot
    # output$WP <- renderForceNetwork({
    #   thr <- input$Thr
    #   ndWP <- input$Node_WP
      
    #   # Adjust the size of the nodes based on their original size
    #   nodes2$size2 <- nodes2$size/(max(nodes2$size)/50)
      
    #   # Set the color of the links based on their value
    #   links2$color <- "gray"
    #   links2$color[links2$value>5] <- "blue"
    #   links2$color[links2$value>20] <- "red"
      
    #   # Filter the links based on the threshold value
    #   links2 <- links2[links2$value > thr, ]
      
    #   # Adjust the value of the links for visualization purposes
    #   links2$value2 <- links2$value/(max(links2$value)/10)
      
    #   # Create a new data frame for the filtered links
    #   links3 <- data.frame(source = match(links2$source, nodes2$Names) - 1,
    #                        target = match(links2$target, nodes2$Names) - 1,
    #                        value = links2$value,
    #                        value2 = links2$value2)
      
    #   # Filter the links based on the selected node
    #   if (ndWP == "All") {
    #     links_vis <- links3
    #   } else {
    #     ndWP <- nodes$Names[nodes$Names_vis==ndWP]
    #     id_ndWP <- which(nodes2$Names == ndWP)-1
    #     links_vis <- links3[links3$source == id_ndWP | links3$target == id_ndWP, ]
    #   }
      
    #   # Create a graph from the filtered links and nodes
    #   g <- igraph::graph_from_data_frame(links_vis, vertices = nodes2, directed = TRUE)
      
    #   # Create a 3D plot of the graph using graphjs
    #   graphjs(g)
    # })