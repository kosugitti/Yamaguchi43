rm(list = ls())
pacman::p_load(summarytools,tidyverse)
source("nri_data_code.R")
dfSummary(main) %>% stview()



main %>%
 as_tibble %>%
 select(4,12,13) %>%
 group_by(MARRIAGE,FAMILY_CD) %>%
 summarise(n = n())  %>%
 arrange(-n)


main %>%
  select(430,493:508) %>%
  mutate(across(2:17,~ as.numeric(.) -1)) %>%
  mutate(radio = rowSums(select(.,2:17), na.rm = TRUE)) %>%
  select(1,radio) %>%
  table()
　# table
  group_by(HOB_05_MA,RADIO_TIME) %>%
  summarise(n =n(),.g) %>%
    
    
    
    
    
#購入意欲の変化

selected_vars <- vimain %>%
  filter(str_detect(ラベル,"\\d{2}/\\d{2}")) %>%
  select(ラベル,変数名) %>%
  mutate(
    ItemID = str_sub(変数名, 7,16),
    date = str_extract(ラベル,"\\d{2}/\\d{2}"),
    type = str_sub(変数名, 1,2),
    product = str_extract(ラベル,"^[^（]+")
  ) %>%
  select(ItemID,date,type,product,変数名)
  
  main %>%
    # 必要な変数を選択（SampleIDと選択された変数群）
    select(SampleID, selected_vars$変数名) %>%
    # SampleID以外の全ての列を数値型に変換
    mutate(across(-SampleID, as.numeric)) %>%
    # ワイド形式からロング形式に変換（SampleID以外の列を縦に並べる）
    pivot_longer(-SampleID) %>%
    # selected_varsとJOIN（変数名をキーに商品情報等を結合）
    right_join(selected_vars, by = c("name" = "変数名")) %>%
    # 不要になった変数名列を削除
    select(-name) %>%
    # 日付列を適切なDate型に変換（2024年を補完）
    mutate(date = lubridate::ymd(paste0("2024/", date))) %>%
    # PS（購入意向）データのみに絞り込み
    filter(type == "PS") %>% 
    # 商品（ItemID）ごとにグループ化
    group_by(ItemID) %>%
    # 各商品の最初の調査をpre、最後の調査をpostとして期間を定義
    mutate(
      period = case_when(
        date == min(date) ~ "pre",
        date == max(date) ~ "post",
      )
    ) %>% 
    # グループ化を解除
    ungroup() %>% 
    # type（PS）とperiod（pre/post）を結合して新しい変数名を作成
    unite("typeperiod", type, period, sep = "") %>%
    # ロング形式からワイド形式に変換（PS_pre, PS_postの列を作成）
    pivot_wider(
      id_cols = c(SampleID, ItemID, product),
      names_from = type_period,
      values_from = value
    ) %>% 
    # pre-post差分を計算（preからpostへの変化量）
    mutate(diff = PS_pre - PS_post) %>% 
    # 商品ごとにグループ化
    group_by(product) %>% 
    # 商品ごとの差分の平均値と標準偏差を計算
    summarise(diff_mean = mean(diff, na.rm = T),
              diff_sd = sd(diff, na.rm = T)) %>% 
    # 差分平均の降順、標準偏差の昇順でソート
    arrange(-diff_mean, diff_sd)