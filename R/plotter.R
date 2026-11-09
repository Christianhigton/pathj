diagram_node_labels <- function(labels, width=12L) {
  vapply(labels, function(label) {
    words <- strwrap(gsub("_", " ", label, fixed=TRUE), width=width)
    lines <- unlist(lapply(words, function(word) {
      if (nchar(word)<=width) return(word)
      starts <- seq.int(1L,nchar(word),by=width)
      substring(word,starts,pmin(starts+width-1L,nchar(word)))
    }), use.names=FALSE)
    paste(lines,collapse="\n")
  }, character(1), USE.NAMES=FALSE)
}

diagram_boxes_overlap <- function(box, others, gap=0) {
  if (!nrow(others)) return(logical())
  box[1] < others[,2]+gap & box[2] > others[,1]-gap &
    box[3] < others[,4]+gap & box[4] > others[,3]-gap
}

# Work in physical inches, so collision checks use the rendered text size.
diagram_place_labels <- function(anchors, widths, heights, nodes, bounds, gap=.035) {
  positions <- anchors
  boxes <- matrix(NA_real_,nrow(anchors),4)
  placed <- matrix(numeric(),0,4)
  offsets <- as.matrix(expand.grid(x=(-8:8)*.07,y=(-8:8)*.07))
  offsets <- offsets[order(rowSums(offsets^2)),,drop=FALSE]
  # Place longer labels first: they have fewer usable gaps between nodes.
  for (i in order(widths*heights,decreasing=TRUE)) {
    candidates <- sweep(offsets,2,anchors[i,],"+")
    candidate_boxes <- cbind(candidates[,1]-widths[i]/2,
                             candidates[,1]+widths[i]/2,
                             candidates[,2]-heights[i]/2,
                             candidates[,2]+heights[i]/2)
    penalties <- vapply(seq_len(nrow(candidates)),function(j) {
      box <- candidate_boxes[j,]
      outside <- box[1]<bounds[1] | box[2]>bounds[2] |
        box[3]<bounds[3] | box[4]>bounds[4]
      1000*outside+100*sum(diagram_boxes_overlap(box,nodes,gap))+
        100*sum(diagram_boxes_overlap(box,placed,gap))+
        sum((candidates[j,]-anchors[i,])^2)
    },numeric(1))
    chosen <- which.min(penalties)
    positions[i,] <- candidates[chosen,]
    boxes[i,] <- candidate_boxes[chosen,]
    placed <- rbind(placed,boxes[i,])
  }
  list(positions=positions,boxes=boxes,anchors=anchors,nodes=nodes)
}

# A small bow can still run through an intervening node in a tree layout.
diagram_route_edges <- function(graph) {
  coords <- graph$layout
  edges <- graph$Edgelist
  for (i in seq_along(edges$from)) {
    from <- edges$from[i]; to <- edges$to[i]
    delta <- coords[to,]-coords[from,]
    if (!sum(delta^2)) next
    fraction <- as.vector(sweep(coords,2,coords[from,],"-") %*% delta)/sum(delta^2)
    projected <- outer(fraction,delta)+matrix(coords[from,],nrow(coords),2,byrow=TRUE)
    intervenes <- fraction>.05 & fraction<.95 &
      abs(projected[,1]-coords[,1])<.20 & abs(projected[,2]-coords[,2])<.16
    if (any(intervenes)) {
      old <- graph$graphAttributes$Edges$curve[i]
      graph$graphAttributes$Edges$curve[i] <- if (old<0) min(old,-3) else max(old,3)
    }
  }
  graph
}

plot_path_diagram <- function(graph) {
  graph <- diagram_route_edges(graph)
  edge <- graph$graphAttributes$Edges
  if (is.logical(edge$labels) || !any(nzchar(as.character(edge$labels)))) {
    graph$graphAttributes$Edges$labels <- rep("",length(graph$Edgelist$from))
    plot(graph)
    return(invisible(NULL))
  }
  # qgraph's postExpression runs after its geometry has been resolved on the
  # actual device. Use those anchors, including curves and residual loops,
  # rather than trying to predict their locations from node centres.
  hook <- new.env(parent=emptyenv())
  hook$placement <- NULL
  hook$draw <- function(layout, midpoints, indices, widths, heights, shapes,
                        labels, cex, font) {
    if (!length(indices)) return()
    node_xy <- cbind(graphics::grconvertX(layout[,1],"user","inches"),
                     graphics::grconvertY(layout[,2],"user","inches"))
    # qgraph node dimensions are in units of 7/2.16 * csi/17.5 inches.
    # widths/heights already include qgraph's device normalization.
    radius <- 7/2.16*graphics::par("csi")/17.5
    heights[shapes %in% c("circle","square")] <- widths[shapes %in% c("circle","square")]
    nodes <- cbind(node_xy[,1]-radius*widths,node_xy[,1]+radius*widths,
                   node_xy[,2]-radius*heights,node_xy[,2]+radius*heights)
    anchors <- cbind(graphics::grconvertX(midpoints[indices,1],"user","inches"),
                     graphics::grconvertY(midpoints[indices,2],"user","inches"))
    cex <- rep(cex,length.out=length(labels))[indices]
    font <- rep(font,length.out=length(labels))[indices]
    labs <- labels[indices]
    w <- vapply(seq_along(indices),function(i)
      graphics::strwidth(labs[[i]],units="inches",cex=cex[i],font=font[i]),numeric(1))+.04
    h <- vapply(seq_along(indices),function(i)
      graphics::strheight(labs[[i]],units="inches",cex=cex[i],font=font[i]),numeric(1))+.04
    usr <- graphics::par("usr")
    bounds <- c(graphics::grconvertX(usr[1:2],"user","inches"),
                graphics::grconvertY(usr[3:4],"user","inches"))
    placement <- diagram_place_labels(anchors,w,h,nodes,bounds)
    placement$labels <- labs
    placement$indices <- indices
    hook$placement <- placement
    xy <- placement$positions
    x <- graphics::grconvertX(xy[,1],"inches","user")
    y <- graphics::grconvertY(xy[,2],"inches","user")
    colors <- edge$label.color
    if (is.null(colors)) colors <- edge$color
    colors <- rep(colors,length.out=length(labels))[indices]
    moved <- rowSums((xy-anchors)^2)>.04^2
    # Short leader lines keep displaced labels attached to their own paths.
    graphics::segments(midpoints[indices[moved],1],midpoints[indices[moved],2],
                       x[moved],y[moved],col=colors[moved],lwd=.6)
    boxes <- placement$boxes
    graphics::rect(graphics::grconvertX(boxes[,1],"inches","user"),
                   graphics::grconvertY(boxes[,3],"inches","user"),
                   graphics::grconvertX(boxes[,2],"inches","user"),
                   graphics::grconvertY(boxes[,4],"inches","user"),
                   col="white",border=NA)
    for (i in seq_along(indices))
      graphics::text(x[i],y[i],labs[[i]],cex=cex[i],font=font[i],col=colors[i])
  }
  graph$graphAttributes$Edges$label.color <- rep("transparent",length(edge$labels))
  graph$graphAttributes$Edges$label.bg <- rep(NA_character_,length(edge$labels))
  callback <- paste0("attr(postExpression, 'pathj_hook')$draw(layout, cbind(midX,midY), ",
    "edgesort2[plotEdgeLabel[edgesort2]], vsize, vsize2, shape, ",
    "edge.labels, edge.label.cex, edge.label.font)")
  graph$plotOptions$postExpression <- structure(
    paste(c(graph$plotOptions$postExpression,callback),collapse="\n"),pathj_hook=hook)
  plot(graph)
  invisible(hook$placement)
}

Plotter <- R6::R6Class(
  "Plotter",
  cloneable=FALSE,
  class=FALSE,
  inherit = Dispatch,
  public=list(
      semPathsOptions=NULL,
      initialize=function(options,datamatic,operator,resultsplots) {
            super$initialize(options=options,vars=unlist(c(options$endogenous,options$factors,options$covs)))
            private$.plotgroup<-resultsplots
            private$.operator<-operator
            private$.datamatic<-datamatic
      },

      initPlots=function() {
           if (!self$options$diagram)
               return()

           if (is.something(private$.datamatic$multigroup))  {
                for (level in private$.datamatic$multigroup$levels) {
                     title <- paste(private$.datamatic$multigroup$var, "=", level)
                     private$.plotgroup$diagrams$addItem(level)
                     private$.plotgroup$diagrams$get(key = level)$setTitle(title)
                }
           } else {
                private$.plotgroup$diagrams$addItem("0")
                private$.plotgroup$diagrams$get(key = "0")$setTitle("")
      }
      
      },
      preparePlots=function(image, ggtheme, theme, ...) {
        
        if (!self$options$diagram)
          return()
        
        model<-private$.operator$model
        ### this is due to the fact that semPaths seems to fail when an inequality constraints is in ###
        ### check for more lavaanian solution ###
        check<-grep(">|<",model@ParTable$op,invert = T)
        par<-model@ParTable
        model@ParTable<-sapply(par, function(x) x[check],simplify = F)
        ### end ###
      
        
        ## prepare the semPlotModel object to be passed to .rendefun where
        ## semPaths will do the actual diagrams
        
        ## At the moment (mid-2021) semPaths invokes the graphical device even when DoNotPlot is TRUE,
        ## which causes a window to pop up in windows and mac when run in jamovi. So, we need to prepare a semPlot object 
        ## here and make the actual plot in the .renderfun of pathj.b.R with semPaths. 
        ## for multigroup, we trick the .renderfun semPaths() to believe that there's always only one group
        ## per call, because the .renderfun is called for each group.
        
        
        sm<-semPlot::semPlotModel(model)
        groups<-unique(sm@Pars$group)
        images<-private$.plotgroup$diagrams
        
        
        if (all(groups=="")) {
          image<-images$get(key = "0")
          image$setState(list(semModel = sm))
          } else {
          for (i in seq_along(images$itemKeys)) {
            image<-images$get(key = images$itemKeys[[i]])
            .sm<-sm
            .sm@Pars<-.sm@Pars[.sm@Pars$group==groups[i],]
             image$setState(list(semModel = .sm))
          }

          }

        ### images are set ####
        
        ## now we prepare the options for semPaths

        labs<-self$options$diag_paths
        nodeLabels<-model@pta$vnames$ov.num[[1]]
        nodeLabels<-fromb64(nodeLabels)
        if (self$options$diag_abbrev!="0")
            nodeLabels<-abbreviate(nodeLabels,minlength = as.numeric(self$options$diag_abbrev),strict = T)
        
        nodeLabels<-diagram_node_labels(nodeLabels)
        label_lines<-max(lengths(strsplit(nodeLabels,"\n",fixed=TRUE)))
        size<-12
        if (self$options$diag_labsize=="small") size<-8
        if (self$options$diag_labsize=="large") size<-18
        if (self$options$diag_labsize=="vlarge") size<-24
        
        nNodes<-length(nodeLabels)
        size<-size*exp(-nNodes/80)+1
        
        layout<-self$options$diag_type
        rotation<-as.numeric(self$options$diag_rotate)
        if (layout %in% c("circle") & rotation %in% c(2,4)) {
            rotation<-1
            self$warnings<-list(topic="diagram",message=glue::glue(PLOT_WARNS[["rotation"]],var="Circle"))
        }
        if (layout %in% c("tree2") & rotation %in% c(2,4)) {
          rotation<-1
          self$warnings<-list(topic="diagram",message=glue::glue(PLOT_WARNS[["rotation"]],var="Tree-alternative"))
        }

        
        self$semPathsOptions<-list(
                      layout = private$.layout(),
                      residuals = self$options$diag_resid,
                      rotation = rotation ,
                      intercepts = F
                      ,nodeLabels=nodeLabels
                      ,whatLabels=labs
                      ,sizeMan = size
                      ,sizeMan2=size/2*max(1,label_lines/2)
                      , curve=1
                      , shapeMan=self$options$diag_shape
                      ,edge.label.cex =.85
                      ,label.cex=switch(self$options$diag_labsize, small=.8, large=1.1, vlarge=1.25, .95)
                      ,label.scale=FALSE
                      ,nCharNodes=0
                      ,edge.label.position=.5
                      ,edge.label.bg="white")
        

        
        return()
      }
      
      
  ), # end of public
  private = list(
    .diagrams=NULL,
    .datamatic=FALSE,
    .plotgroup=NULL,
    .operator=NULL,
    .semPaths=function(options) {

      ### we need this because semPaths does not work with do.call() in windows
        res<-try_hard({
        semPlot::semPaths(object = options$object,
                      layout =private$.layout(),
                      residuals = options$residuals,
                      rotation = options$rotation,
                      intercepts = options$intercepts,
                      nodeLabels= options$nodeLabels,
                      whatLabels=options$whatLabels,
                      sizeMan = options$sizeMan,
                      sizeMan2=options$sizeMan2,
                      curve=options$curve,
                      shapeMan=options$shapeMan,
                      edge.label.cex =options$edge.label.cex,
                      label.cex=options$label.cex, label.scale=options$label.scale,
                      nCharNodes=options$nCharNodes,
                      edge.label.position=options$edge.label.position,
                      edge.label.bg=options$edge.label.bg,
                      DoNotPlot=TRUE)

      })
      if (is.something(res$error))
          self$warnings<-list(topic="diagram",message=res$error)
      
      return(res)
    },
    .layout=function() {
      
      if (self$options$diag_type!="mediation")
        return(self$options$diag_type)
      
      ie<-private$.operator$ieffects
      
      if (is.null(ie))
         return("tree")

      fit<-private$.operator$model  
      pt<-fit@ParTable
      lhs<-pt$lhs[pt$op!=":="]
      xcoo<-sapply(unique(lhs), function(x) {
        max((unlist(lapply(ie, function(xx) which(xx==x)))))
      })
      xcoo[!is.finite(xcoo)]<-1
     
      
      q<-cbind(seq_along(xcoo),order(xcoo))
      orig_order<-q[order(q[,2]),1]
      xcoo<-xcoo[order(xcoo)]
      if (length(unique(xcoo))==length(xcoo)) {
        ycoo<-rep(.80,length(xcoo))
        ycoo[xcoo==min(xcoo)]<-ycoo[xcoo==max(xcoo)]<-.2
      } else {
        ycoo<-unlist(lapply(unique(xcoo), function(x) {
          nvars<-length(xcoo[xcoo==x])
          1:nvars/(nvars+1)
        }))
      }
      p<-cbind(x=xcoo,y=ycoo)
      p<-p[orig_order,]
      p
      
    }

  ) # end of private
) # end of class

