# This Enhanced DotPlot function take most variable as original DotPlot function 
# with addition ability to 
# - Sort identity and feautres by hclust = cluster_gene = T, cluster_sample =T
# Aug 2022, Simon
DotPlotEnhanced = function(obj, group.by = 'seurat_clusters', features,  title = 'DotPlot', subtitle = '', cluster_gene = T, cluster_sample =T, ...){
    p = DotPlot(st, group.by=group.by, features = features,...) + 
    labs(title = title, subtitle = subtitle) + 
    scale_color_gradientn(colors = RColorBrewer::brewer.pal(n = 9, name = 'YlOrRd')[2:9])+
    RotatedAxis() 
    
    # Get order by hclust
    exp_mtx = p$data %>% 
        pivot_wider(id_cols = id, names_from =features.plot, values_from = avg.exp.scaled) %>% 
        column_to_rownames('id')
    sample_order = exp_mtx %>% dist %>% hclust %>% {.$labels[.$order]}
    gene_order  = exp_mtx %>% t %>% dist %>% hclust %>% {.$labels[.$order]}
    if(cluster_gene) p$data = p$data %>% mutate(features.plot = factor(features.plot, levels = gene_order))
    if(cluster_sample) p$data = p$data %>% mutate(id = factor(id, levels = sample_order))
    
    return(p)
    
}