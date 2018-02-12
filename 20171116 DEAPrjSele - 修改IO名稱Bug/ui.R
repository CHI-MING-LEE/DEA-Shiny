library(shinydashboard)
library(shiny)
library(DT)



dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Project Selection", titleWidth = 250, tags$li(a(img(src = 'DALab.png',
                  title = "A+ DALab", height = "50px"),
                  style = "padding-top:0px; padding-bottom:0px;"),
                  class = "dropdown")),
  
  dashboardSidebar(
        collapsed =T,
          paste("Version: v1.0 - 2017/10/25"),
            br(),
          HTML(
            paste("Developer: 李騏名","<a href=\"mailto:s105034539@m105.nthu.edu.tw\">s105034539@m105.nthu.edu.tw</a>")),
            br(),
          HTML(
            paste("Maintainer: 李騏名","<a href=\"mailto:momo820526@ie.nthu.edu.tw\">s105034539@m105.nthu.edu.tw</a>")),
          sidebarMenu(
            menuItem("Project Selection", tabName = "dashboard1", icon = icon("line-chart"))
          )
  ),
  
  dashboardBody(
  
          tags$style(HTML('
                 hr{
                      display: block;
                      height: 1px;
                      border: 0;
                      border-top: 2px solid #ABABAB;
                      margin: 1em 0;
                      padding: 0; 
                    }
                    ')),
          
          tags$head(tags$style(HTML(
            '.myClass { 
              font-size: 20px;
              line-height: 50px;
              text-align: left;
              font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
              padding: 0 15px;
              overflow: hidden;
              color: white;
              }
              '))),
          tags$script(HTML('
              $(document).ready(function() {
                             $("header").find("nav").append(\'<span class="myClass">Data Envelopment Analysis</span>\');
                             })
                             ')),
          tabItems(
            tabItem(
              tabName = "dashboard1",
                fluidRow(
                  column(width=3,
                     # 左上方塊，負責資料上傳
                      shinydashboard::box(
                        width = NULL,
                        title = "資料選取", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        fileInput('file1', '上傳欲選專案資料',
                                  accept = c('csv/text',
                                             'comma-separated-values/text',".csv",'.txt'
                                  ),multiple = T, buttonLabel = "瀏覽",placeholder = "尚未選取任何資料"
                        )
                      ),
                     # 左中方塊，負責變數選擇
                     shinydashboard::box(
                       width = NULL,
                       title = "屬性選擇", status = "primary", solidHeader = TRUE,
                       collapsible = TRUE,
                       textInput("X", label = "請輸入投入屬性的欄位，並以空格分隔  Ex.1 3 5 7", value = NA),
                       textInput("Y", label = "請輸入產出屬性的欄位，並以空格分隔  Ex.2 4 6 8", value = NA),
                       #numericInput("threshhold", label = "請輸入篩選的Rsquare門檻",min=0,max=1,step=0.01, value = 0.7),
                       radioButtons(inputId="mode", label = "請選擇DEA模式",
                                    choices = list("CCR" = 1, "AR" = 2), 
                                    selected = NULL, inline = TRUE),
                       actionButton("vGo", "選擇完畢",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                     ),
                     # 左中方塊，負責輸入Y變數規格
                     shinydashboard::box(
                       width = NULL,
                       title = "AR模式請輸入投入/產出屬性比率上下界", status = "primary", solidHeader = TRUE,
                       collapsible = TRUE,
                       
                       # 上下界標示
                         column(width=6, HTML("<b><center> Lower </center></b>")),
                         column(width=6, HTML("<b><center> Upper </center></b>")),
                       
                       # 加一條線
                         h3(),
                         hr(),
                       
                       # 投入屬性
                         column(width=6, HTML("<b> 投入屬性 </b>"), uiOutput("Iratiolower")),
                         column(width=6, br(), uiOutput("Iratioupper")),
                       
                       # 產出屬性
                       #,textInput(inputId="xlimitsmax", label="x-max", value = 0.5)))
                         column(width=6, HTML("<b> 產出屬性 </b>"),uiOutput("Oratiolower")),
                         column(width=6,  br(), uiOutput("Oratioupper")),
                         br(),br(),br(),
                         actionButton("run", "計算相對效率",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                     )
                  ),
                  # 右邊負責圖表輸出
                  column(width=9,
                         shinydashboard::tabBox(id="tabset",width='800', height = "1200"
                           # ,tabPanel("資料欄位", div(column( dataTableOutput("File"),width = 3), style = "font-size:150%"))
                           ,tabPanel("資料檢視", div(DT::dataTableOutput("viewdata"), style = "font-size:100%"))
                           ,tabPanel("分析結果", value = "result",
                                     downloadButton("SaveNum", "儲存結果", class = "butt"),
                                     tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
                                     br(),
                                     br(),
                                     div(DT::dataTableOutput("Efficiency"), style = "font-size:100%"))
                           ,tabPanel("分析圖表", plotOutput("viewplot", width = "750px", height = "550px"))
                           #,tabPanel("預測結果", div(tableOutput("termPlot_Opt"),tableOutput("DOEopt"), style = "font-size:120%"))
                         )
                  )
               )
            ) 
        )
  )
)