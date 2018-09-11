
COL_ESPN <- "name"
RGX_SPLIT_ESPN <- "\\."
.get_cols_max_at <-
  function(data, col, rgx_split) {
    if(missing(col)) {
      col <- COL_ESPN
    }
    if(missing(rgx_split)) {
      rgx_split <- RGX_SPLIT_ESPN
    }
    col_sym <- sym(col)
    data %>%
      pull(!!col_sym) %>% 
      str_split(rgx_split) %>% 
      map_dbl(~length(.)) %>% 
      max()
  }

.separate_cols_at <-
  function(data, col, rgx_split) {
    if(missing(col)) {
      col <- COL_ESPN
    }
    if(missing(rgx_split)) {
      rgx_split <- RGX_SPLIT_ESPN
    }
    col_sym <- sym(col)
    n_cols_max <-
      .get_cols_max_at(data = data, col = col)
    nms_sep <-
      paste0(col, seq(1, n_cols_max, by = 1))
    data %>%
      separate(!!col_sym, into = nms_sep, sep = rgx_split)    
  }

.convert_list_to_tbl <-
  function(x) {
    tibble::enframe(unlist(x))
  }

.convert_list_to_tbl_cleanly_at <-
  function(x, col, rgx_split) {
    if(missing(col)) {
      col <- COL_ESPN
    }
    if(missing(rgx_split)) {
      rgx_split <- RGX_SPLIT_ESPN
    }
    data <- tibble::enframe(unlist(x))
    .separate_cols_at(data = data, col = col, rgx_split = rgx_split)
  }