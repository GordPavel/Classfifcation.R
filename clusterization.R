require(magrittr)
require(dplyr)
require(textTinyR)

data <- like.data$question
clust_vec = textTinyR::tokenize_transform_vec_docs(
  data,
  as_token = T,
  to_lower = T,
  remove_punctuation_vector = F,
  remove_numbers = F,
  trim_token = T,
  split_string = T,
  split_separator = " \r\n\t.,;:()?!//",
  remove_stopwords = T,
  language = "russian",
  min_num_char = 3,
  max_num_char = 100,
  stemmer = "porter2_stemmer",
  threads = 4,
  verbose = T
)
init = textTinyR::Doc2Vec$new(
  token_list = clust_vec$token,
  word_vector_FILE = "word_vectors.txt",
  print_every_rows = 5000,
  verbose = TRUE,
  copy_data = FALSE
)
utl = textTinyR::sparse_term_matrix$new(
  vector_data = data,
  file_data = NULL,
  document_term_matrix = TRUE
)
tm = utl$Term_Matrix(
  sort_terms = FALSE,
  to_lower = T,
  remove_punctuation_vector = F,
  remove_numbers = F,
  trim_token = T,
  split_string = T,
  stemmer = "porter2_stemmer",
  split_separator = " \r\n\t.,;:()?!//",
  remove_stopwords = T,
  language = "english",
  min_num_char = 3,
  max_num_char = 100,
  print_every_rows = 100000,
  normalize = NULL,
  tf_idf = T,
  threads = 6,
  verbose = T
)
gl_term_w = utl$global_term_weights()
doc2_idf = init$doc2vec_methods(method = "idf", global_term_weights = gl_term_w)
scal_dat = ClusterR::center_scale(doc2_idf)     # center and scale the data
opt_cl = ClusterR::Optimal_Clusters_KMeans(
  scal_dat,
  max_clusters = 15,
  criterion = "BIC",
  fK_threshold = 0.85,
  num_init = 3,
  max_iters = 50,
  initializer = "kmeans++",
  tol = 1e-04,
  plot_clusters = TRUE,
  verbose = T,
  tol_optimal_init = 0.3,
  seed = 1
)
num_clust = 11
kmed = ClusterR::Cluster_Medoids(
  scal_dat,
  clusters = num_clust,
  distance_metric = "pearson_correlation",
  minkowski_p = 1,
  threads = 6,
  swap_phase = TRUE,
  fuzzy = FALSE,
  verbose = F,
  seed = 1
)
table(kmed$clusters)
freq_clust = textTinyR::cluster_frequency(
  tokenized_list_text = clust_vec$token,
  cluster_vector = kmed$clusters,
  verbose = T
)