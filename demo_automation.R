
source("https://github.com/karsfri/ILS/raw/master/theme_hms.R")
theme_set_hms()



yfirverd <- function(...){
  df <- yfirverd_get_data()
  
  my_groups <- enquos(...)
  
  df2 <- df %>% 
    sql_clean() %>% 
    mutate(timi = floor_date(Dim_Timi_Utgefid, unit = "months")) %>% 
    group_by(timi, !!!my_groups) %>% 
    summarise_at(
      vars(SeltYfirAuglystuSoluverdi, SeltAAuglystuSoluverdi, SeltUndirAuglystuSoluverdi),
      sum,
      na.rm = TRUE
    ) %>% 
    select(SeltYfirAuglystuSoluverdi, SeltAAuglystuSoluverdi, SeltUndirAuglystuSoluverdi, timi, !!!my_groups) %>% 
    gather(var, val, SeltYfirAuglystuSoluverdi, SeltAAuglystuSoluverdi, SeltUndirAuglystuSoluverdi) %>% 
    mutate(
      var = var %>% fct_inorder() %>% fct_rev
    ) %>% 
    group_by(var) %>% 
    mutate(val = (val + lag(val) + lag(val, 2)) / 3) 
  
  p <- df2 %>% 
    ggplot(aes(x = timi, y = val, fill = var)) +
    geom_area(position = position_fill()) +
    scale_y_continuous(labels = scales::percent_format()) +
    facet_wrap(facets = vars(!!!my_groups))
  
  print(p)
  
  return(df2)
  
}

yfirverd_get_data <- function(){
  require(odbc)
  require(DBI)
  
  con <- dbConnect(odbc(), "HgrDwh_Prod", timeout = 10, encoding = "WINDOWS-1252")
  
  # sql script
  query_yfirverd <- "SELECT 
        [Dim_Timi_Utgefid]
        ,[Dim_Timi_Thinglyst]
        ,[Dim_Fasteignir]
        ,[Faerslunumer]
        ,[Kaupverd]
        ,[Kaupverd_nuvirdi]
        ,[FjoldiFasteigna]
        ,[FjoldiMatseininga]
        ,[OnothaefurSamningur]
        ,[AuglystDags]
        ,[AuglystSoluverd]
        ,[Landshluti]
        ,[Landshlutaflokkun]
        ,[HofudborgLandsbyggd]
        ,[SerbyliFjolbyli]
        ,[FjoldiHerbergja]
        ,[SeltYfirAuglystuSoluverdi]
        ,[SeltAAuglystuSoluverdi]
        ,[SeltUndirAuglystuSoluverdi]
        ,f.[LOAD_DATE]
        ,f.[RECORD_SOURCE]
        ,f.[ETL_ID]
    FROM 
      [HgrDwh].[dwh].[Fact_Kaupsamningar_Stakar] f 
          INNER JOIN dwh.Dim_Timi dtim 
              ON f.Dim_Timi_Utgefid = dim_timi_sk 
          JOIN dwh.Dim_Fasteignir dfst 
  		    ON f.Dim_Fasteignir = dfst.Dim_Fasteignir_sk
   WHERE 
      dtim.ar > 2012
  ORDER BY 
   Dim_Timi_Utgefid ASC"
  
  df <- dbGetQuery(con, query_yfirverd)
  df %>% as_tibble()
}


# df_Landshluti <- yfirverd(Landshluti)
# df_Landshluti



