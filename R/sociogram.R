#'@title Sociogram with actor information
#'@name actorSociogram
#'
#'@description Function to obtain the circle diagram with actor relationship from an sociomatrix.
#'
#'@param smatrix sociomatrix
#'
#'@details The function actorSociogram is obtained by the sociometric matrix.
#'         Return a circular diagram with actor relationship between the animals, where
#'         the arrow shows the direction of the relationship and the line thickness 
#'         indicates the number of encounters (the thicker the line, the greater
#'         the number of encounters).
#'
#'@return Circular plot with actor information
#'
#'@importFrom data.table as.data.table
#'@importFrom circlize circos.clear
#'@importFrom circlize circos.par
#'@importFrom circlize chordDiagram
#'@importFrom circlize circos.track
#'@importFrom circlize circos.text
#'@importFrom graphics par
#'@importFrom graphics strwidth
#'
#'@import circlize
#'
#'@author Julia P. S. Valente, Matheus Deniz, Karolini T. de Sousa.
#'
#'@examples "There is no example"
#'
#'NULL
#'
#'@export
actorSociogram <- function(smatrix){
  smatrix[lower.tri(smatrix)] <- 0
  freq <- data.table::as.data.table(smatrix)
  circlize::circos.clear()
  circlize::circos.par(start.degree = 0 ,
             gap.degree = 0.01 ,
             gap.after = 0.01 ,
             track.margin=c(0.01,0.01),
             cell.padding =  c(0.02, 1, 0.02, 1.00),
             unit.circle.segments = 500 ,
             track.height = 0.2 ,
             points.overflow.warning = TRUE,
             canvas.xlim = c(-1,1),
             canvas.ylim = c(-1,1),
             circle.margin = c(0.01,0.01,0.01,0.01),
             clock.wise = T,
             xaxis.clock.wise = TRUE,
             points.overflow.warning = TRUE
  )
  
  graphics::par(mar = rep(0, 4))
  circlize::chordDiagram(freq, directional = 1, direction.type = c("diffHeight", "arrows"), 
               link.arr.type = "big.arrow", annotationTrack = "grid", 
               preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(freq))))))
  
  circlize::circos.track(track.index = 1, 
                         panel.fun = function(x, y) {
                           circlize::circos.text(CELL_META$xcenter, 
                                       CELL_META$ylim[1], 
                                       CELL_META$sector.index, 
                                       facing = "clockwise", 
                                       niceFacing = TRUE, 
                                       adj = c(0, 0.5),
                                       cex = 0.5)
                         },
                         bg.border = NA)
  
}

#'@title Sociogram plot with reactor information
#'@name reactorSociogram
#'
#'@description Function to obtain the circle diagram with reactor relationship from an sociomatrix.
#'
#'@param smatrix sociomatrix
#'
#'@details The function reactorCircleDiagram is obtained by the sociometric matrix.
#'         Return a circular diagram with reactor relationship between the animals, where
#'         the arrow shows the direction of the relationship and the line thickness 
#'         indicates the number of encounters (the thicker the line, the greater
#'         the number of encounters).
#'          
#'
#'@return Circular plot with reactor information
#'
#'@importFrom data.table as.data.table
#'@importFrom circlize circos.clear
#'@importFrom circlize circos.par
#'@importFrom circlize chordDiagram
#'@importFrom circlize circos.track
#'@importFrom circlize circos.text
#'@importFrom graphics par
#'@importFrom graphics strwidth
#'
#'@import circlize
#'
#'@author Julia P. S. Valente, Matheus Deniz, Karolini T. de Sousa.
#'
#'@examples "There is no example"
#'
#'NULL
#'
#'@export
#'
#'
reactorSociogram <- function(smatrix){
  smatrix[upper.tri(smatrix)] <- 0
  freq <- data.table::as.data.table(smatrix)
  circlize::circos.clear()
  circlize::circos.par(start.degree = 0 ,
             gap.degree = 0.01 ,
             gap.after = 0.01 ,
             track.margin=c(0.01,0.01),
             cell.padding =  c(0.02, 1, 0.02, 1.00),
             unit.circle.segments = 500 ,
             track.height = 0.2 ,
             points.overflow.warning = TRUE,
             canvas.xlim = c(-1,1),
             canvas.ylim = c(-1,1),
             circle.margin = c(0.01,0.01,0.01,0.01),
             clock.wise = T,
             xaxis.clock.wise = TRUE,
             points.overflow.warning = TRUE
  )
  graphics::par(mar = rep(0, 4))
  circlize::chordDiagram(freq, directional = 1, direction.type = c("diffHeight", "arrows"), 
               link.arr.type = "big.arrow", annotationTrack = "grid", 
               preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(freq))))))
  
  circlize::circos.track(track.index = 1, panel.fun = function(x, y) {
    circlize::circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
                facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.5)
  }, bg.border = NA)
  
}
