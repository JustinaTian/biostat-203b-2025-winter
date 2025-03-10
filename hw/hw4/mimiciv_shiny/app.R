## LOAD PACKAGES
library(shiny)
library(tidyverse)
library(dbplyr)
library(bigrquery)

## 0. Helper function: safely cast to POSIXct without tz=
safe_cast_datetime <- function(x) {
  # 如果已经是POSIXt，就直接返回
  if (inherits(x, "POSIXt")) return(x)
  
  # 如果是数字，视为 Unix 时间戳 (秒数)
  if (is.numeric(x)) {
    return(as.POSIXct(x, origin = "1970-01-01"))
  }
  
  # 否则假设是字符串，按常见格式解析
  # 这里假设 "%Y-%m-%d %H:%M:%S" 适用
  as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S")
}

## 1. CONNECT TO BIGQUERY (已给出)
satoken <- "../biostat-203b-2025-winter-4e58ec6e5579.json"
bq_auth(path = satoken)

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter"
)

## 2. 定义对 BigQuery 表的引用 (根据实际情况修改表名)
icustays_tble       <- tbl(con_bq, "icustays")
chartevents_tble    <- tbl(con_bq, "chartevents")
transfers_tble      <- tbl(con_bq, "transfers")
lab_events_tble     <- tbl(con_bq, "labevents")
admissions_tble     <- tbl(con_bq, "admissions")
patients_tble       <- tbl(con_bq, "patients")
diagnoses_icd_tble  <- tbl(con_bq, "diagnoses_icd")
d_icd_diagnoses_tble<- tbl(con_bq, "d_icd_diagnoses")
procedures_icd_tble <- tbl(con_bq, "procedures_icd")
d_icd_procedures_tble <- tbl(con_bq, "d_icd_procedures")

## 3. LOAD MIMIC ICU COHORT (Summary Tab 用的)
mimic_icu_cohort <- readRDS("mimic_icu_cohort.rds") |> 
  rename(
    heart_rate = `heart rate`,
    non_invasive_bp_systolic = `non invasive blood pressure systolic`,
    non_invasive_blood_pressure_diastolic = `non invasive blood pressure diastolic`,
    respiratory_rate = `respiratory rate`,
    temperature_fahrenheit = `temperature fahrenheit`,
  ) |>
  mutate(
    insurance = factor(insurance),
    marital_status = factor(marital_status),
    gender = as.factor(gender)
  )

## 4. 定义分组 (Summary Tab 用的)
varGroups <- list(
  demo = c("insurance", "marital_status", "race", "gender", "ageintime"),
  lab_measure = c("chloride","creatinine","sodium","potassium",
                  "glucose","hematocrit","wbc","bicarbonate"),
  vitals = c("heart_rate","non_invasive_bp_systolic",
             "non_invasive_blood_pressure_diastolic",
             "respiratory_rate","temperature_fahrenheit")
)

## ---- UI ----
ui <- fluidPage(
  titlePanel("ICU Cohort Data"),
  
  tabsetPanel(
    ## ==================== Tab 1: Summary ====================
    tabPanel("Summary",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "varGroup",
                   label = "Variable Group",
                   choices = names(varGroups),
                   selected = "demo"
                 ),
                 uiOutput("varSelect"),
                 uiOutput("xRangeUI")
               ),
               mainPanel(
                 plotOutput("cohortPlot"),
                 verbatimTextOutput("cohortSummary")
               )
             )
    ),
    
    ## ==================== Tab 2: Patient Info ====================
    tabPanel("Patient Info",
             sidebarLayout(
               sidebarPanel(
                 textInput("subjectID", "Subject ID", value = ""),
                 actionButton("submitID", "Submit"),
                 selectInput("plotType", "Select a plot:", 
                             choices = c("ICU", "ADT"))
               ),
               mainPanel(
                 tableOutput("patientTable"),
                 plotOutput("patientPlot")
               )
             )
    )
  )
)

## ---- Server ----
server <- function(input, output, session) {
  
  ###### ========== (A) Summary Tab ========== ######
  # 动态生成 Variable 下拉菜单
  output$varSelect <- renderUI({
    req(input$varGroup)
    selectInput(
      inputId = "variable",
      label = "Variable",
      choices = varGroups[[input$varGroup]],
      selected = varGroups[[input$varGroup]][1]
    )
  })
  
  # 如果变量是数值型，则显示滑动条
  output$xRangeUI <- renderUI({
    req(input$variable)
    if (is.numeric(mimic_icu_cohort[[input$variable]])) {
      rng <- range(mimic_icu_cohort[[input$variable]], na.rm = TRUE)
      sliderInput(
        inputId = "xrange",
        label = "X axis range",
        min = floor(rng[1]),
        max = ceiling(rng[2]),
        value = c(floor(rng[1]), ceiling(rng[2]))
      )
    } else {
      return(NULL)
    }
  })
  
  # 生成图形 & 数值摘要
  observeEvent(list(input$varGroup, input$variable, input$xrange), {
    req(input$variable)
    
    output$cohortSummary <- renderPrint({
      summary(mimic_icu_cohort[[input$variable]])
    })
    
    output$cohortPlot <- renderPlot({
      dataVar <- mimic_icu_cohort[[input$variable]]
      if (is.numeric(dataVar)) {
        p <- ggplot(mimic_icu_cohort, aes_string(x = input$variable)) +
          geom_histogram(fill = "gray", color = "black") +
          theme_minimal()
        if (!is.null(input$xrange)) {
          p <- p + coord_cartesian(xlim = input$xrange)
        }
        p
      } else {
        ggplot(mimic_icu_cohort, aes_string(x = input$variable)) +
          geom_bar(fill = "darkolivegreen3", color = "black") +
          theme_minimal()
      }
    })
  })
  
  ###### ========== (B) Patient Info Tab ========== ######
  
  # 当点击 Submit 按钮时，根据输入的 Subject ID 查询相关信息
  patientData <- eventReactive(input$submitID, {
    pid <- as.numeric(input$subjectID)
    if (is.na(pid) || pid == 0) {
      return(NULL)
    }
    
    ## 1) 查询 ICU stays
    icu_stays <- icustays_tble %>%
      filter(subject_id == pid) %>%
      collect() %>%
      mutate(
        intime  = safe_cast_datetime(intime),
        outtime = safe_cast_datetime(outtime),
        stay_id = as.double(stay_id)
      ) %>%
      select(subject_id, stay_id, intime, outtime)
    
    ## 2) 查询并处理 vitals (chartevents)
    vitals_data <- chartevents_tble %>%
      filter(subject_id == pid, !is.na(valuenum)) %>%
      select(subject_id, stay_id, charttime, itemid, valuenum) %>%
      collect() %>%
      mutate(
        stay_id   = as.double(stay_id),
        charttime = safe_cast_datetime(charttime)
      ) %>%
      inner_join(icu_stays, by = "stay_id") %>%
      filter(charttime >= intime & charttime <= outtime) %>%
      mutate(
        vital_label = case_when(
          itemid %in% c(220045) ~ "HR",
          itemid %in% c(220179) ~ "NBPd",
          itemid %in% c(220181) ~ "NBPs",
          itemid %in% c(220210) ~ "RR",
          itemid %in% c(223761) ~ "Temperature",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(vital_label)) %>%
      group_by(stay_id) %>%
      mutate(relative_charttime = as.numeric(difftime(charttime, min(charttime), units = "hours"))) %>%
      ungroup()
    
    ## 3) 查询 transfers (ADT)
    adt_data <- transfers_tble %>%
      filter(subject_id == pid, !is.na(intime), !is.na(outtime)) %>%
      collect() %>%
      mutate(
        intime  = safe_cast_datetime(intime),
        outtime = safe_cast_datetime(outtime)
      )
    
    ## 4) lab_data
    lab_data <- lab_events_tble %>%
      filter(subject_id == pid) %>%
      distinct(subject_id, charttime, itemid, .keep_all = TRUE) %>%
      collect() %>%
      mutate(charttime = safe_cast_datetime(charttime))
    
    ## 5) patient_info
    patient_info <- admissions_tble %>%
      filter(subject_id == pid) %>%
      select(subject_id, race, admittime) %>%
      distinct() %>%
      left_join(
        patients_tble %>% select(subject_id, gender, anchor_age), 
        by = "subject_id"
      ) %>%
      collect()
    
    ## 6) 诊断 & 手术
    diagnosis_data <- diagnoses_icd_tble %>%
      mutate(icd_code = regexp_replace(icd_code, "^0+", ""), 
             icd_version = as.character(icd_version)) %>%
      filter(subject_id == pid) %>%
      left_join(
        d_icd_diagnoses_tble %>%
          mutate(icd_code = regexp_replace(icd_code, "^0+", ""), 
                 icd_version = as.character(icd_version)),
        by = c("icd_code", "icd_version")
      ) %>%
      left_join(admissions_tble %>% select(subject_id, admittime), by = "subject_id") %>%
      collect() %>%
      arrange(admittime) %>%
      slice(1:3)
    
    procedure_data <- procedures_icd_tble %>%
      mutate(icd_code = regexp_replace(icd_code, "^0+", "")) %>%
      filter(subject_id == pid & !is.na(chartdate)) %>%
      left_join(d_icd_procedures_tble, by = "icd_code") %>%
      collect() %>%
      mutate(
        chartdate = safe_cast_datetime(chartdate),
        procedure_label = ifelse(is.na(long_title), "Unknown Procedure", 
                                 str_trunc(long_title, 30, side="right"))
      )
    
    # 返回列表
    list(
      icu_stays   = icu_stays,
      vitals_data = vitals_data,
      adt_data    = adt_data,
      lab_data    = lab_data,
      patient_info = patient_info,
      diagnosis_data = diagnosis_data,
      procedure_data = procedure_data
    )
  })
  
  # 当选择 ICU 或 ADT 时，绘制对应的图
  observeEvent(list(input$plotType, patientData()), {
    if (is.null(patientData())) {
      output$patientTable <- renderTable(NULL)
      output$patientPlot <- renderPlot({})
      return()
    }
    
    dataList <- patientData()
    
    if (input$plotType == "ICU") {
      # ICU vitals 分面图
      if (nrow(dataList$vitals_data) == 0) {
        output$patientTable <- renderTable(NULL)
        output$patientPlot <- renderPlot({
          plot.new()
          text(0.5, 0.5, "No ICU vitals found for this patient.")
        })
      } else {
        output$patientTable <- renderTable({
          head(dataList$vitals_data, 10)
        })
        
        output$patientPlot <- renderPlot({
          vitals_data <- dataList$vitals_data
          ggplot(vitals_data, aes(x = relative_charttime, y = valuenum, color = vital_label)) +
            geom_line() +
            geom_point(size = 2) +
            facet_grid(vital_label ~ stay_id, scales = "free_y", switch = "y") +
            theme_minimal() +
            labs(
              title = paste("Patient", unique(vitals_data$subject_id), "ICU stays - Vitals"),
              x = "Hours since first measurement",
              y = "Vital Value"
            ) +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              strip.text.y = element_text(angle = 0, hjust = 1), 
              legend.position = "none", 
              panel.grid.major = element_line(color = "grey90")
            )
        })
      }
      
    } else {
      # ADT 甘特图
      if (nrow(dataList$adt_data) == 0) {
        output$patientTable <- renderTable(NULL)
        output$patientPlot <- renderPlot({
          plot.new()
          text(0.5, 0.5, "No ADT data found for this patient.")
        })
      } else {
        output$patientTable <- renderTable({
          head(dataList$adt_data, 10)
        })
        
        output$patientPlot <- renderPlot({
          adt_data      <- dataList$adt_data
          lab_data      <- dataList$lab_data
          patient_info  <- dataList$patient_info
          diagnosis_data <- dataList$diagnosis_data
          procedure_data <- dataList$procedure_data
          
          title_text <- paste0(
            "Patient ", input$subjectID, ", ",
            patient_info$gender[1], ", ",
            patient_info$anchor_age[1], " years old, ",
            patient_info$race[1]
          )
          
          subtitle_text <- diagnosis_data %>%
            filter(!is.na(long_title)) %>%
            pull(long_title) %>%
            paste(collapse = "\n")
          
          ggplot() +
            # Procedure 点
            geom_point(data = procedure_data,
                       aes(x = chartdate, y = "Procedure", 
                           shape = factor(procedure_label)),
                       size = 4) +
            
            # ADT 段
            geom_segment(data = adt_data,
                         aes(x = intime, xend = outtime, 
                             y = "ADT", color = careunit,
                             linewidth = ifelse(str_detect(careunit, "ICU|CCU|SICU"), 5, 2))) +
            scale_linewidth_identity() +
            
            # Lab
            geom_point(data = lab_data,
                       aes(x = charttime, y = "Lab"),
                       shape = 3, size = 3, color = "black") +
            
            scale_x_datetime(date_labels = "%b %d", date_breaks = "7 days") +
            theme_minimal() +
            labs(
              title = title_text,
              subtitle = subtitle_text,
              x = "Calendar Time",
              y = "Event Type",
              color = "Care Unit",
              shape = "Procedure"
            ) +
            scale_color_manual(values = c("red","orange","pink","purple","cyan")) +
            scale_shape_manual(values = c(16,17,15,4,5)) +
            guides(
              color = guide_legend(title = "Care Unit", nrow = 1),
              shape = guide_legend(title = "Procedure", nrow = 1)
            ) +
            theme(
              legend.position = "bottom",
              legend.box = "vertical",
              legend.spacing.y = unit(0.5, "cm"),
              legend.text = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0),
              plot.subtitle = element_text(size = 8, hjust = 0),
              legend.key.size = unit(0.8, "cm")
            )
        })
      }
    }
  })
}

## ---- Run Shiny App ----
shinyApp(ui = ui, server = server)
