diagram_test_graph <- function(model, group, rotation="2", large=FALSE, residuals=FALSE) {
  fit <- workplace_result(model)$model
  images <- new.env(parent=emptyenv())
  for (name in c("Control","Training")) {
    img <- new.env(parent=emptyenv())
    img$setState <- local({target<-img; function(state) target$state<-state})
    images[[name]] <- img
  }
  options <- pathj:::pathjOptions$new(diagram=TRUE,diag_abbrev="0",
    diag_rotate=rotation,diag_labsize=if (large) "large" else "medium",
    diag_resid=residuals)
  plotter <- pathj:::Plotter$new(options,
    list(multigroup=list(var="condition",levels=c("Control","Training"))),
    list(model=fit),list(diagrams=list(itemKeys=c("Control","Training"),
      get=function(key) images[[key]])))
  plotter$preparePlots()
  o <- plotter$semPathsOptions
  sm <- images[[group]]$state$semModel
  graph <- semPlot::semPaths(sm,layout=o$layout,residuals=o$residuals,
    rotation=o$rotation,intercepts=FALSE,nodeLabels=o$nodeLabels,
    whatLabels=o$whatLabels,sizeMan=o$sizeMan,sizeMan2=o$sizeMan2,
    curve=o$curve,shapeMan=o$shapeMan,edge.label.cex=o$edge.label.cex,
    label.cex=o$label.cex,label.scale=FALSE,nCharNodes=0,
    edge.label.position=.5,edge.label.bg="white",style="ram",DoNotPlot=TRUE)
  graph$graphAttributes$Edges$lty[graph$Edgelist$bidirectional] <- 2
  graph$graphAttributes$Edges$curve[graph$Edgelist$bidirectional] <- .6
  graph
}

expect_diagram_clear <- function(placement, info) {
  testthat::expect_true(nrow(placement$boxes)>0L,info=info)
  for (i in seq_len(nrow(placement$boxes))) {
    testthat::expect_false(any(pathj:::diagram_boxes_overlap(
      placement$boxes[i,],placement$nodes)),info=info)
    testthat::expect_false(any(pathj:::diagram_boxes_overlap(
      placement$boxes[i,],placement$boxes[-i,,drop=FALSE])),info=info)
  }
}

testthat::test_that("crowded labels move clear of boxes and each other", {
  anchors <- matrix(rep(c(2,2),3),ncol=2,byrow=TRUE)
  nodes <- matrix(c(1.8,2.2,1.8,2.2),nrow=1)
  placement <- pathj:::diagram_place_labels(anchors,rep(.4,3),rep(.15,3),
    nodes,c(0,4,0,4))
  expect_diagram_clear(placement,"three coincident labels")
  testthat::expect_equal(placement$anchors,anchors)
})

testthat::test_that("rendered workplace labels avoid nodes and labels across rotations", {
  file <- tempfile(fileext=".png")
  grDevices::png(file,width=700,height=500,res=96)
  on.exit({grDevices::dev.off(); unlink(file)},add=TRUE)
  for (model in c("A","B")) for (group in c("Control","Training")) {
    for (rotation in as.character(1:4)) {
      graph <- diagram_test_graph(model,group,rotation)
      original <- graph$graphAttributes$Edges$labels
      # Exercise the longest labels produced by the significance-star option.
      graph$graphAttributes$Edges$labels <- ifelse(nzchar(original),paste0(original,"***"),"")
      placement <- pathj:::plot_path_diagram(graph)
      expect_diagram_clear(placement,paste(model,group,rotation))
      testthat::expect_equal(placement$labels,
        graph$graphAttributes$Edges$labels[placement$indices])
    }
    graph <- diagram_test_graph(model,group,large=TRUE,residuals=TRUE)
    placement <- pathj:::plot_path_diagram(graph)
    expect_diagram_clear(placement,paste(model,group,"large, residuals"))
    graph$graphAttributes$Edges$labels <- FALSE
    testthat::expect_null(pathj:::plot_path_diagram(graph))
  }
})
