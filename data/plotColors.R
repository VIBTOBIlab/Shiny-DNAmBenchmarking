# Define plot styling

theme_celine<-theme_classic()+
  theme(plot.title = element_text(size = 12,color = "gray10"),
        plot.subtitle = element_text(size=8, color= "gray 30"),
        axis.text = element_text(size=8,color = "gray10"),
        axis.title = element_text(size=10,color = "gray10"),
        legend.text = element_text(size=8,color = "gray10"),
        legend.title = element_text(size=10,color = "gray10"),
        axis.line = element_line(color="gray50"),
        axis.ticks = element_line(color="gray50"))

theme_point<-theme_celine+
                theme(strip.background = element_blank())
  
theme_bar<-theme_celine+
                theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
                              strip.background = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.line.x = element_blank())

theme_boxplot<-theme_celine+
                  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),strip.background = element_blank(),axis.ticks.x = element_blank(),axis.title.x = element_blank(),axis.line.x = element_blank(),legend.position = "none")

readGMT<-function(filename){
  gmtLines<-strsplit(readLines(filename),"\t")
  gmtLines_genes <- lapply(gmtLines, tail, -2)
  names(gmtLines_genes) <- sapply(gmtLines, head, 1)
  return(gmtLines_genes)
}

writeGMTfromDEresults<-function(markersList,filename){
  markersList
  write.table(sapply(names(markersList),function(x) paste(x,paste(rownames(markersList[[x]]),collapse="\t"),sep="\t")),filename,quote = FALSE,row.names = TRUE,col.names = FALSE,sep='\t')
}

library(foreach)
library(doParallel)
HyperGTestGeneEnrichment<-function(gmtfile,testgmtfile,NrCores,ref.numb.genes=45956){
  
  `%dopar%` <- foreach::`%dopar%`
  `%do%` <- foreach::`%do%`
  test.gmt<-readGMT(testgmtfile) # our gmt_file_output
  gmt.path<-readGMT(gmtfile)  # gmt libraries
  
  ###########################  Parallelizing :
  cluster <- parallel::makeCluster(c(rep("localhost", NrCores)), type = "SOCK")
  doParallel::registerDoParallel(cluster,cores=NrCores)
  
  resultloop<-c()
  resultloop<-foreach(j=1:length(test.gmt), .combine='rbind') %do% {
    #print(j)
    foreach(i=1:length(gmt.path),.combine='rbind') %dopar% {
      #print(i)
      # for(j in 1:length(test.gmt)){
      #   print(paste0("test_gmt = ",j))
      #   for(i in 1:length(gmt.path)){
      l<-length(gmt.path[[i]])
      k<-sum(gmt.path[[i]] %in% test.gmt[[j]])
      m<-ref.numb.genes
      n<-length(test.gmt[[j]])
      p1<-stats::phyper(k-1,l,m-l,n,lower.tail=FALSE)
      
      if (k>0){
        overlapping.genes<-gmt.path[[i]][gmt.path[[i]] %in% test.gmt[[j]]]
        overlapping.genes<-paste(overlapping.genes,collapse = ', ')
        c(Geneset=names(gmt.path[i]),Testset=names(test.gmt[j]),Geneset_length=l,Testset_length = n, p_value=p1,n_Overlapping=k,Overlapping_genes=overlapping.genes)
      } else {
        c(Geneset=names(gmt.path[i]),Testset=names(test.gmt[j]),Geneset_length=l,Testset_length = n, p_value=p1,n_Overlapping=0,Overlapping_genes="")
      }
    }
  }
  
  parallel::stopCluster(cluster)
  resultloop<-as_tibble(resultloop)
  resultloop$p_value<-as.numeric(resultloop$p_value)
  resultloop$Testset_length<-as.numeric(resultloop$Testset_length)
  resultloop$n_Overlapping<-as.numeric(resultloop$n_Overlapping)
  resultloop$Geneset_length<-as.numeric(resultloop$Geneset_length)
  resultloop<- resultloop %>% dplyr::mutate(p_adj=stats::p.adjust(resultloop$p_value,method='BH'))
  return(resultloop)
}

drawplot<-function(seuratObj,reductionType,columnName,titleInfo){
  
  coordsTable <- as.data.frame(seuratObj@reductions[[reductionType]]@cell.embeddings, stringsAsFactors = F)
  coordsTable <- left_join(rownames_to_column(coordsTable,"cellname"),rownames_to_column(seuratObj@meta.data,"cellname"))
  dim_1 <- colnames(coordsTable)[2]
  dim_2 <- colnames(coordsTable)[3]

  p <- ggplot(coordsTable) +
    geom_point(aes(x=get(dim_1),y=get(dim_2),color=get(columnName)),size=2,shape=20)+ 
    labs(title= paste0(columnName," (",reductionType,")"), color=columnName) +
    theme_point +
    theme(plot.title=element_text(hjust=0.5),
          legend.position="right")
  
  return(p)
}


drawplothl<-function(seuratObj,reductionType,columnName,titleInfo,highlight){
  
  coordsTable <- as.data.frame(seuratObj@reductions[[reductionType]]@cell.embeddings, stringsAsFactors = F)
  coordsTable <- left_join(rownames_to_column(coordsTable,"cellname"),rownames_to_column(seuratObj@meta.data,"cellname"))
  dim_1 <- colnames(coordsTable)[2]
  dim_2 <- colnames(coordsTable)[3]
  
  #if (!is.na(highlight)) {
  if (!missing(highlight)) {
    coordsTable <- coordsTable %>% mutate(highlightc=ifelse(get(columnName) %in% highlight, get(columnName), "no"))
  } 
  
  p <- ggplot(coordsTable) +
    geom_point(aes(x=get(dim_1),y=get(dim_2),color=highlightc),size=2,shape=20)+ 
    labs(title= paste0(columnName," (",reductionType,")"), color=columnName) +
    theme_point +
    theme(plot.title=element_text(hjust=0.5),
          legend.position="right")
  
  return(p)
}

scatterplotseurat<-function(seuratObj,xfeature,yfeature, cfeature=NULL){
  
  metaTable <- rownames_to_column(seuratObj@meta.data,"cellname")
  if (!is.null(cfeature)){
  p <- ggplot(metaTable) +
    geom_point(aes(x=get(xfeature),y=get(yfeature),color=get(cfeature)),size=2,shape=20)+ 
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title= paste0(xfeature," - ",yfeature),x=xfeature, y=yfeature, color=cfeature) +
    theme_point +
    theme(plot.title=element_text(hjust=0.5),
          legend.position="right")
  } else {
  p <- ggplot(metaTable) +
    geom_point(aes(x=get(xfeature),y=get(yfeature)),size=2,shape=20)+ 
    labs(title= paste0(xfeature," - ",yfeature),x=xfeature, y=yfeature) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_point +
    theme(plot.title=element_text(hjust=0.5),
            legend.position="right")
  }
  return(p)
}

##### Function drawVlnPlotSeurat_split
VlnfeaturePlot <-function(seuratObj, features){
  
  metaDataTable <- seuratObj@meta.data %>% 
    dplyr::select(orig.ident, features) %>% 
    pivot_longer(cols=features,names_to="feature",values_to="value")
  
  p<- ggplot(metaDataTable,aes(x=orig.ident,y=value,fill=orig.ident)) +
    geom_jitter(height=0,width=0.1,alpha=0.2,color="gray",fill="white") +
    geom_violin(aes(fill=orig.ident),alpha=0.6) +
    facet_wrap(~feature, scales="free_y")+
    theme_boxplot
  return(p)
}

GeneReductionPlot<-function(seuratObj, gene, reductionType = "umap", assay = "RNA"){
  coordsTable <- rownames_to_column(as.data.frame(seuratObj[[reductionType]]@cell.embeddings, stringsAsFactors = F),"cellname")
  geneTable <- rownames_to_column(as.data.frame((seuratObj[[assay]]$counts)[gene,]),"cellname")
  colnames(geneTable) <- c("cellname",gene)
  metaTable <- rownames_to_column(seuratObj@meta.data,"cellname")
  metaTable <- left_join(left_join(metaTable, geneTable),coordsTable)
  if (reductionType == "tsne") {
    dim_1 <- "tSNE_1"
    dim_2 <- "tSNE_2"
  } else if ( reductionType == "umap"){
    dim_1 <- "UMAP_1"
    dim_2 <- "UMAP_2"
  }else if ( reductionType == "wnn.umap"){
    dim_1 <- "wnnUMAP_1"
    dim_2 <- "wnnUMAP_2"
  }
  mean_cluster_positions <- metaTable %>% dplyr::select(seurat_clusters,dim_1,dim_2) %>% group_by(seurat_clusters) %>% summarise(xposition= mean(get(dim_1)),yposition=mean(get(dim_2)))
  p<-ggplot()+
    geom_point(data=metaTable,aes(x=get(dim_1),y=get(dim_2), color=log(get(gene)+1,10)), alpha=0.6, cex=0.2)+
    geom_text(data=mean_cluster_positions,aes(x=xposition,y=yposition,label=seurat_clusters), size=6)+
    labs(x="dimension1",y="dimension2",size=gene,color="cluster")+
    guides(color = FALSE) +
    labs(title = gene)+
    theme_point
  return(p)
}

GeneReductionPlot_size<-function(seuratObj, gene, reductionType = "umap"){
  coordsTable <- rownames_to_column(as.data.frame(seuratObj[[reductionType]]@cell.embeddings, stringsAsFactors = F),"cellname")
  geneTable <- rownames_to_column(as.data.frame((seuratObj[["RNA"]]@counts)[gene,]),"cellname")
  colnames(geneTable) <- c("cellname",gene)
  metaTable <- rownames_to_column(seuratObj@meta.data,"cellname")
  metaTable <- left_join(left_join(metaTable, geneTable),coordsTable)
  if (reductionType == "tsne") {
    dim_1 <- "tSNE_1"
    dim_2 <- "tSNE_2"
  } else if ( reductionType == "umap"){
    dim_1 <- "UMAP_1"
    dim_2 <- "UMAP_2"
  } else if ( reductionType == "wnn.umap"){
    dim_1 <- "wnnUMAP_1"
    dim_2 <- "wnnUMAP_2"
  }
  mean_cluster_positions <- metaTable %>% dplyr::select(seurat_clusters,dim_1,dim_2) %>% group_by(seurat_clusters) %>% summarise(xposition= mean(get(dim_1)),yposition=mean(get(dim_2)))
  p<-ggplot()+
    geom_point(data=metaTable,aes(x=get(dim_1),y=get(dim_2), size=log(get(gene)+1,10), color=seurat_clusters), alpha=0.6)+
    geom_text(data=mean_cluster_positions,aes(x=xposition,y=yposition,label=seurat_clusters), size=6)+
    labs(x="dimension1",y="dimension2",size=gene,color="cluster")+
    guides(color = FALSE) +
    theme_point
  return(p)
}  
  
GeneVlnPlot<-function(seuratObj, gene, reductionType = "umap"){
  coordsTable <- rownames_to_column(as.data.frame(seuratObj[[reductionType]]@cell.embeddings, stringsAsFactors = F),"cellname")
  geneTable <- rownames_to_column(as.data.frame((seuratObj[["RNA"]]@counts)[gene,]),"cellname")
  colnames(geneTable) <- c("cellname",gene)
  metaTable <- rownames_to_column(seuratObj@meta.data,"cellname")
  metaTable <- left_join(left_join(metaTable, geneTable),coordsTable)
  if (reductionType == "tsne") {
    dim_1 <- "tSNE_1"
    dim_2 <- "tSNE_2"
  } else if ( reductionType == "umap"){
    dim_1 <- "UMAP_1"
    dim_2 <- "UMAP_2"
  } else if ( reductionType == "wnn.umap"){
    dim_1 <- "wnnUMAP_1"
    dim_2 <- "wnnUMAP_2"
  }
  mean_cluster_positions <- metaTable %>% dplyr::select(seurat_clusters,dim_1,dim_2) %>% group_by(seurat_clusters) %>% summarise(xposition= mean(get(dim_1)),yposition=mean(get(dim_2)))
  p<- ggplot()+
    geom_jitter(data=metaTable,aes(x=seurat_clusters,y=get(gene), color=seurat_clusters), alpha=0.6, size=0.3, height=0.2)+
    geom_violin(data=metaTable,aes(x=seurat_clusters,y=get(gene), fill=seurat_clusters, color=seurat_clusters), alpha=0.3)+
    labs(title= gene, x="cluster",y="counts",size=gene,color="cluster")+
    guides(color = FALSE, fill=FALSE) +
    theme_boxplot
  return(p)
}

GeneSetReductionPlot<-function(seuratObj, testresults, geneset,reductionType = "umap"){
  coordsTable <- rownames_to_column(as.data.frame(seuratObj[[reductionType]]@cell.embeddings, stringsAsFactors = F),"cellname")
  metaTable <- rownames_to_column(seuratObj@meta.data,"cellname")
  metaTable <- left_join(left_join(metaTable, coordsTable),testresults %>% mutate(seurat_clusters=gsub("cluster","",Testset)) %>% dplyr::select(-Testset))
  if (reductionType == "tsne") {
    dim_1 <- "tSNE_1"
    dim_2 <- "tSNE_2"
  } else if ( reductionType == "umap"){
    dim_1 <- "UMAP_1"
    dim_2 <- "UMAP_2"
  } else if ( reductionType == "wnn.umap"){
    dim_1 <- "wnnUMAP_1"
    dim_2 <- "wnnUMAP_2"
  }
  mean_cluster_positions <- metaTable %>% dplyr::select(seurat_clusters,dim_1,dim_2) %>% group_by(seurat_clusters) %>% summarise(xposition= mean(get(dim_1)),yposition=mean(get(dim_2)))
  p<- ggplot()+
    geom_point(data=metaTable,aes(x=get(dim_1),y=get(dim_2), color=get(geneset)), alpha=0.6)+
    geom_text(data=mean_cluster_positions,aes(x=xposition,y=yposition,label=seurat_clusters), size=6)+
    labs(title=geneset,x="dimension1",y="dimension2",color="padjvalue")+
    theme_point
  return(p)
}

flowplotmarkers <- function(seuratObj, markerx, markery, assay){
  
}
