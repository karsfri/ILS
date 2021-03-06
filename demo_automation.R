

yfirverd <- function(..., print_plot = TRUE, eftir = NULL, filters = NULL){
  
  require(tidyverse)
  require(lubridate)
  require(glue)
  
  df <- yfirverd_get_data()
  if(NROW(df) == 0) stop("0 rows returned from DB")
  
  my_groups <- enquos(...)

  df2 <- df %>% 
    sql_clean() %>% 
    # map(filters, ~. %>% filter(!!!.x))
    filter(!!!filters) %>%
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
      var = var %>% 
        fct_inorder() %>% 
        fct_rev %>% 
        fct_recode(
          `Selt undir auglýstu söluverði` = "SeltUndirAuglystuSoluverdi",
          `Selt á auglýstu söluverði` = "SeltAAuglystuSoluverdi",
          `Selt yfir auglýstu söluverði` = "SeltYfirAuglystuSoluverdi"
        )
    ) %>% 
    group_by(var) %>% 
    mutate(val = (val + lag(val) + lag(val, 2)) / 3) 
  
  
  # Plot
  eftir <- if_else(is.null(eftir), "", paste0("eftir ", eftir, " "))
  title <- glue("Kaupverð íbúða {eftir}í samanburði við ásett verð")
  

  if(print_plot) {
    p <- plot_yfirverd(df2, title = title) +
      facet_wrap(facets = vars(!!!my_groups))
    
    print(p)
  }
  
  return(df2)
  
}

yfirverd_get_data <- function(){
  require(odbc)
  require(DBI)
  
  con <- con()
  
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

con <- function(){
  # creates a connection to HgrDwh based on OS
  require(odbc)
  require(DBI)
  library(RJDBC)
  
  if(Sys.info()['sysname'] == "Windows"){
    return(dbConnect(odbc(), "HgrDwh_Prod", timeout = 10, encoding = "WINDOWS-1252"))
  } else if (Sys.info()['sysname'] == "osx") {
    options(java.parameters = "-Xmx8048m")
    drv <- JDBC(driverClass="com.microsoft.sqlserver.jdbc.SQLServerDriver", classPath="D:/R/Drivers/mssql-jdbc-7.2.1.jre8.jar")
    con <- dbConnect(drv, url="jdbc:sqlserver://localhost;DatabaseName=HgrDwh", user="HgrExtProc", password=Pass)
    return(con)
  }
  stop("OS not detected")
  
}


plot_yfirverd <- function(df, title = NULL){
  df %>% 
    ggplot(aes(x = timi, y = val, fill = var)) +
    geom_area(position = position_fill()) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      y = NULL, 
      x = NULL, 
      title = title,
      subtitle = "-3ja mánaða hlaupandi meðaltal",
      caption = "Heimild: Þjóðskrá Íslands, Fasteignaleit, hagdeild HMS"
    ) 
}

