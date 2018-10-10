## Fast Term Frequency Inverse Document Frequency Calculation
tfidf=function(mat){
  #names = intersect(MRCONSO_Filtered_Semantic_Group$AUI,colnames(mat))
  mat = mat[,names]
  tf = mat/rowSums(mat)
  id=function(col){sum(!col==0)}
  idf = log10(nrow(mat)/apply(mat, 2, id))
  tfidf = mat
  for(word in names(idf)){tfidf[,word] <- tf[,word] * idf[word]}
  return(tfidf)
}