# vimain からラベル名を取得する関数
get_label_from_vimain <- function(varname, vimain_data) {
  label <- vimain_data %>%
    filter(変数名 == varname) %>%
    pull(ラベル)
  
  if (length(label) > 0) return(label)
  else return(varname)
}

library(tidyverse)
library(forcats)

# PS_CAT_05（インスタント食品の購入頻度）を逆順 factor にして確認
main %>%
  mutate(PS_CAT_05_rev = fct_rev(factor(PS_CAT_05))) %>%
  count(PS_CAT_05_rev)


# 改良版メイン関数
plot_channel_relationship_auto <- function(main_data, target_var_index, vimain_data) {
  
  # 色設定
  palette_name <- .available_colors[.color_env$counter]
  .color_env$counter <- .color_env$counter %% length(.available_colors) + 1
  
  # ターゲット変数名
  target_var <- names(main_data)[target_var_index]
  
  # vimainからラベルを取得
  target_label <- get_label_from_vimain(target_var, vimain_data)
  
  # レベル自動取得
  levels_auto <- main_data[[target_var]] %>%
    na.omit() %>%
    unique() %>%
    as.character() %>%
    factor(levels = unique(.)) %>%
    levels()
  
  # プロット
  main_data %>%
    select(conv_freq = 413, target_freq = all_of(target_var)) %>%
    filter(!is.na(conv_freq), !is.na(target_freq)) %>%
    mutate(
      conv_freq = factor(conv_freq, levels =  c(
        "ほとんど毎日",
        "週に２～３回程度",
        "週に１回程度",
        "月に１～２回程度",
        "半年に１～２回程度",
        "年に１回程度",
        "ほとんど利用していない"
      )),
      target_freq = factor(target_freq, levels = levels_auto)
    ) %>%
    count(conv_freq, target_freq) %>%
    ggplot(aes(x = target_freq, y = fct_rev(conv_freq), fill = n)) +
    geom_tile(color = "white") +
    geom_text(aes(label = n), size = 3) +
    scale_fill_distiller(palette = palette_name, direction = 1) +
    labs(
      title = paste("コンビニ利用頻度 ×", target_label),
      x = target_label,
      y = "コンビニ利用頻度",
      fill = "人数"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}


# 例：インスタント食品の列番号が 615
plot_channel_relationship_auto(main_data = main, target_var_index = 615, vimain_data = vimain)

# 例：ノンアルコール飲料（ビールやカクテル類）が 607
plot_channel_relationship_auto(main_data = main, target_var_index = 607, vimain_data = vimain)

# 例：ノンアルコール飲料（ビールやカクテル類）が 609
plot_channel_relationship_auto(main_data = main, target_var_index = 609, vimain_data = vimain)




library(tidyverse)
library(forcats)
library(RColorBrewer)

# 色用のカウンター環境
.color_env <- new.env()
.color_env$counter <- 1
.available_colors <- c("Blues", "Greens", "Oranges", "Purples", "Reds", "YlGnBu", "PuRd")

# ラベル取得（vimainから）
get_label <- function(var_name, vimain_df) {
  vimain_df %>%
    filter(変数名 == var_name) %>%
    pull(ラベル) %>%
    unique()
}

# プロット関数：ラベル逆順対応
plot_channel_relationship_with_label_reverse <- function(main_data, target_var_index, vimain_data) {
  # ターゲット変数名
  target_var <- names(main_data)[target_var_index]
  

  # 色の切り替え
  palette_name <- .available_colors[.color_env$counter]
  .color_env$counter <- .color_env$counter %% length(.available_colors) + 1
  
  # プロット作成
  tmp <- main_data %>%
    select(conv_freq = 413, target_freq = all_of(target_var)) %>%
    filter(!is.na(conv_freq), !is.na(target_freq))
  rev_label <- tmp$target_freq %>% levels() %>% rev()
  tmp %>% 
  mutate(
      conv_freq = factor(conv_freq, levels = c(
        "ほとんど毎日",
        "週に２～３回程度",
        "週に１回程度",
        "月に１～２回程度",
        "半年に１～２回程度",
        "年に１回程度",
        "ほとんど利用していない"
      )),
      target_freq = fct_relevel(target_freq,rev_label)
    ) %>% 
    count(conv_freq, target_freq) %>%
    ggplot(aes(x = target_freq, y = fct_rev(conv_freq), fill = n)) +
    geom_tile(color = "white") +
    geom_text(aes(label = n), size = 3) +
    scale_fill_distiller(palette = palette_name, direction = 1) +
    labs(
      title = paste("コンビニ利用頻度 ×", target_label),
      x = target_label,
      y = "コンビニ利用頻度",
      fill = "人数"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}


# インスタント食品の列（例: 615）
plot_channel_relationship_with_label_reverse(main, 615, vimain)

# エナジードリンクの列（例: 611）
plot_channel_relationship_with_label_reverse(main, 611, vimain)


# エナジードリンクの列（例: 611）
plot_channel_relationship_with_label_reverse(main, 607, vimain)


