approach <- function(file_out){
  
  if (!dir.exists(dirname(file_out))) {
    dir.create(dirname(file_out),recursive = TRUE)
  }
  
  grViz("
digraph nicegraph {
  graph[layout = dot, rankdir = TB, fontsize = 16]

  node[fontname = Ubuntu,style = filled, fontcolor = black,color = white]
    A[label = 'Raw data\n- mzML format\n- centroided']
    B[label = 'Infusion profile scan detection']
    C[label = <<I>m/z  </I>binned to 0.00001 amu>]
    D[label = <Removal of single scan<BR/><I>m/z </I>events>]
    E[label = <<I>m/z </I> binned to 0.01<BR/>amu>]
    F[label = 'Abundances aggregation across infusion scans']
    G[label = <Accurate   <I>  m/z</I>assignment>]
  
  edge[color = black]
    A -> B
    B -> C
    C -> D [color = red]
    D -> {E F} [color = red]
    E -> F [color = blue]
    F -> G [color = blue]
    F -> G [color = red]
}") %>%
    export_svg() %>% 
    charToRaw() %>% 
    rsvg_pdf(file_out,
             height = 325,
             width = 250)
  
  return(file_out)
} 