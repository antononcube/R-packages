# app.R
# Pure Shiny (non-flexdashboard) version of the SMR/LSA generic recommender interface

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(dplyr)
  library(purrr)
  library(stringr)

  library(SparseMatrixRecommender)
  library(SMRMon)
  library(LSAMon)
})

# ---- Helpers ---------------------------------------------------------------

parse_tag_profile <- function(x, allowed = NULL, drop_prefix = NULL) {
  if (is.null(x) || !nzchar(trimws(x))) return(NULL)

  parts <- str_split(x, ",", simplify = FALSE)[[1]] |> trimws()
  parts <- parts[nzchar(parts)]

  if (!is.null(drop_prefix)) {
    parts <- parts[str_detect(parts, paste0("^", fixed(drop_prefix)))]
    parts <- str_replace(parts, paste0("^", fixed(drop_prefix)), "")
  } else {
    # For tag profile input, ignore Item:* unless explicitly requested with drop_prefix
    parts <- parts[!str_detect(parts, "^Item:")]
  }

  if (length(parts) == 0) return(NULL)

  m <- str_split_fixed(parts, ":", 3)
  tag <- paste0(m[, 1], ":", m[, 2])
  score <- suppressWarnings(as.numeric(m[, 3]))
  score[is.na(score)] <- 1

  if (!is.null(allowed)) {
    keep <- tag %in% allowed
    tag <- tag[keep]
    score <- score[keep]
  }

  if (length(tag) == 0) return(NULL)
  setNames(score, tag)
}

parse_item_history <- function(x, allowed = NULL) {
  if (is.null(x) || !nzchar(trimws(x))) return(NULL)

  parts <- str_split(x, ",", simplify = FALSE)[[1]] |> trimws()
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) return(NULL)

  m <- str_split_fixed(parts, ":", 2)
  item <- m[, 1]
  rating <- suppressWarnings(as.numeric(m[, 2]))
  rating[is.na(rating)] <- 1

  if (!is.null(allowed)) {
    keep <- item %in% allowed
    item <- item[keep]
    rating <- rating[keep]
  }

  if (length(item) == 0) return(NULL)
  setNames(rating, item)
}

make_dt <- function(df, height = 360) {
  DT::datatable(
    df,
    rownames = FALSE,
    options = list(
      scrollX = TRUE,
      scrollY = paste0(height, "px"),
      pageLength = 25,
      deferRender = TRUE
    )
  )
}

# ---- Load artifacts (same contract as your old Rmd) -------------------------

load("smrGoods.RData")  # expects: smrGoods
load("lsaGoods.RData")  # expects: lsaGoods
load("dfGoods.RData")   # expects: dfGoods

lsRecommenders <- list(Goods = smrGoods)
lsLSAObjects   <- list(Goods = lsaGoods)
lsData         <- list(Goods = dfGoods)

# Apply term-weight functions once up-front (keeps previous intent)
lsRecommenders <- purrr::map(
  lsRecommenders,
  ~ .x %>% SMRMonApplyTermWeightFunctions("None", "None", "Cosine")
)

# ---- UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Generic recommender interface (Shiny)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "recommender",
        "Type:",
        choices = names(lsRecommenders),
        selected = names(lsRecommenders)[1]
      ),
      sliderInput(
        "numberOfRecs",
        "Number of recommendations:",
        min = 1, max = 300, value = 10, step = 1
      ),
      tags$hr(),
      h4("Tag type weights"),
      uiOutput("tag_type_weight_sliders"),
      tags$hr(),
      helpText("Input formats:"),
      tags$ul(
        tags$li(tags$code("Good:milk"), ", ", tags$code("Country:denmark")),
        tags$li("Optional weight: ", tags$code("Good:ham:1.1")),
        tags$li("Items inside tag box: ", tags$code("Item:milk:1, Item:ham:0.8")),
        tags$li("Item history box: ", tags$code("milk:1, ham:0.4"))
      ),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Recommendations",
          fluidRow(
            column(
              6,
              h4("Tags"),
              textAreaInput(
                "tagsText",
                "Tag text (metadata)",
                placeholder = "Good:milk, Country:denmark, Good:ham:1.1, Country:china:0.85",
                rows = 7,
                width = "100%"
              ),
              strong("Known tags:"),
              verbatimTextOutput("knownTags")
            ),
            column(
              6,
              h4("Free text"),
              textAreaInput(
                "freeText",
                "Free text (description)",
                placeholder = "Milk unverbalised",
                rows = 7,
                width = "100%"
              ),
              strong("Extracted words/topics:"),
              DTOutput("freeTextExtracted")
            )
          ),
          tags$hr(),
          tabsetPanel(
            tabPanel("Tags + free text", DTOutput("recsTagsAndText")),
            tabPanel("Items", DTOutput("recsItems"))
          )
        ),
        tabPanel(
          "Profiles",
          fluidRow(
            column(
              6,
              h4("Item history"),
              textAreaInput(
                "profileTagsText",
                "Items text (points)",
                placeholder = "milk:1, ham:0.3",
                rows = 7,
                width = "100%"
              ),
              strong("Known items:"),
              verbatimTextOutput("knownItems")
            ),
            column(
              6,
              h4("Profile free text"),
              textAreaInput(
                "profileFreeText",
                "Free text (description)",
                placeholder = "Milk unverbalised",
                rows = 7,
                width = "100%"
              ),
              strong("Extracted words/topics:"),
              DTOutput("profileFreeTextExtracted")
            )
          ),
          tags$hr(),
          tabsetPanel(
            tabPanel("Profile of items", DTOutput("profileByItems")),
            tabPanel("Profile by free text", DTOutput("profileByText")),
            tabPanel("Profile by both", DTOutput("profileByBoth"))
          )
        ),
        tabPanel(
          "Summary",
          h4("Dimensions of the recommender matrix"),
          verbatimTextOutput("dims"),
          tags$hr(),
          h4("Tag types and ranges"),
          DTOutput("tagRanges")
        )
      ),
      width = 8
    )
  )
)

# ---- Server ----------------------------------------------------------------

server <- function(input, output, session) {

  smrObj <- reactive(lsRecommenders[[input$recommender]])
  lsaObj <- reactive(lsLSAObjects[[input$recommender]])
  dfData <- reactive(lsData[[input$recommender]])

  output$tag_type_weight_sliders <- renderUI({
    req(smrObj())
    tts <- smrObj() %>% SMRMonTakeTagTypes
    tagList(lapply(tts, function(tt) {
      sliderInput(
        paste0("ttw__", tt),
        paste0(tt, " weight:"),
        min = 0, max = 1, value = 1, step = 0.01
      )
    }))
  })

  tagTypeWeights <- reactive({
    req(smrObj())
    tts <- smrObj() %>% SMRMonTakeTagTypes
    setNames(
      vapply(tts, function(tt) input[[paste0("ttw__", tt)]], numeric(1)),
      tts
    )
  })

  # ---- Recommendations: tag profile and item list from tagsText -------------

  lsTagsProf <- reactive({
    req(smrObj())
    parse_tag_profile(
      input$tagsText,
      allowed = colnames(smrObj()$M)
    )
  })

  lsNNItems <- reactive({
    req(smrObj())
    parse_tag_profile(
      input$tagsText,
      allowed = rownames(smrObj()$M),
      drop_prefix = "Item:"
    )
  })

  output$knownTags <- renderText({
    p <- lsTagsProf()
    if (is.null(p) || length(p) == 0) "" else paste(names(p), collapse = ", ")
  })

  # Free text -> words + topics (if topics exist in your M)
  lsFreeWordsProf <- reactive({
    req(smrObj(), lsaObj())
    if (!nzchar(trimws(input$freeText))) return(NULL)

    mat <- lsaObj() %>%
      LSAMonRepresentByTerms(query = input$freeText) %>%
      LSAMonTakeValue()

    s <- colSums(mat)
    s <- s[abs(s) > 0]
    if (length(s) == 0) return(NULL)

    names(s) <- paste0("Word:", names(s))
    s <- s[names(s) %in% colnames(smrObj()$M)]
    if (length(s) == 0) return(NULL)

    mx <- max(abs(s))
    if (mx > 0) s <- s / mx
    rev(sort(s))
  })

  lsFreeTopicsProf <- reactive({
    req(smrObj(), lsaObj())
    if (!nzchar(trimws(input$freeText))) return(NULL)

    mat <- lsaObj() %>%
      LSAMonRepresentByTopics(query = input$freeText) %>%
      LSAMonTakeValue()

    mat <- SparseMatrixRecommender::SMRApplyTermWeightFunctions(mat, "None", "None", "Cosine")

    s <- colSums(mat)
    s <- s[abs(s) > 0]
    if (length(s) == 0) return(NULL)

    names(s) <- paste0("Topic:", names(s))
    s <- s[names(s) %in% colnames(smrObj()$M)]
    if (length(s) == 0) return(NULL)

    mx <- max(abs(s))
    if (mx > 0) s <- s / mx
    rev(sort(s))
  })

  lsFreeProf <- reactive({
    c(lsFreeWordsProf(), lsFreeTopicsProf())
  })

  output$freeTextExtracted <- renderDT({
    p <- lsFreeProf()
    if (is.null(p) || length(p) == 0) return(make_dt(data.frame()))
    make_dt(data.frame(Token = names(p), Score = as.numeric(p)))
  })

  output$recsTagsAndText <- renderDT({
    req(smrObj(), dfData())
    prof <- c(lsTagsProf(), lsFreeProf())
    if (is.null(prof) || length(prof) == 0) return(make_dt(data.frame()))

    smrObj() %>%
      SMRMonApplyTagTypeWeights(weights = tagTypeWeights(), default = 1) %>%
      SMRMonRecommendByProfile(profile = prof, nrecs = input$numberOfRecs, normalizeQ = TRUE) %>%
      SMRMonJoinAcross(dfData()) %>%
      SMRMonTakeValue() %>%
      make_dt()
  })

  output$recsItems <- renderDT({
    req(smrObj(), dfData())
    hist <- lsNNItems()
    if (is.null(hist) || length(hist) == 0) return(make_dt(data.frame()))

    smrObj() %>%
      SMRMonApplyTagTypeWeights(weights = tagTypeWeights(), default = 1) %>%
      SMRMonRecommend(history = hist, nrecs = input$numberOfRecs, normalizeQ = TRUE, removeHistoryQ = FALSE) %>%
      SMRMonJoinAcross(dfData()) %>%
      SMRMonTakeValue() %>%
      make_dt()
  })

  # ---- Profiles ------------------------------------------------------------

  lsItemsHist <- reactive({
    req(smrObj())
    parse_item_history(
      input$profileTagsText,
      allowed = rownames(smrObj()$M)
    )
  })

  output$knownItems <- renderText({
    h <- lsItemsHist()
    if (is.null(h) || length(h) == 0) "" else paste(names(h), collapse = ", ")
  })

  lsProfileFreeWordsProf <- reactive({
    req(smrObj(), lsaObj())
    if (!nzchar(trimws(input$profileFreeText))) return(NULL)

    mat <- lsaObj() %>%
      LSAMonRepresentByTerms(query = input$profileFreeText) %>%
      LSAMonTakeValue()

    s <- colSums(mat)
    s <- s[abs(s) > 0]
    if (length(s) == 0) return(NULL)

    names(s) <- paste0("Word:", names(s))
    s <- s[names(s) %in% colnames(smrObj()$M)]
    if (length(s) == 0) return(NULL)

    mx <- max(abs(s))
    if (mx > 0) s <- s / mx
    rev(sort(s))
  })

  lsProfileFreeTopicsProf <- reactive({
    req(smrObj(), lsaObj())
    if (!nzchar(trimws(input$profileFreeText))) return(NULL)

    mat <- lsaObj() %>%
      LSAMonRepresentByTopics(query = input$profileFreeText) %>%
      LSAMonTakeValue()

    mat <- SparseMatrixRecommender::SMRApplyTermWeightFunctions(mat, "None", "None", "Cosine")

    s <- colSums(mat)
    s <- s[abs(s) > 0]
    if (length(s) == 0) return(NULL)

    names(s) <- paste0("Topic:", names(s))
    s <- s[names(s) %in% colnames(smrObj()$M)]
    if (length(s) == 0) return(NULL)

    mx <- max(abs(s))
    if (mx > 0) s <- s / mx
    rev(sort(s))
  })

  lsProfileFreeProf <- reactive({
    c(lsProfileFreeWordsProf(), lsProfileFreeTopicsProf())
  })

  output$profileFreeTextExtracted <- renderDT({
    p <- lsProfileFreeProf()
    if (is.null(p) || length(p) == 0) return(make_dt(data.frame()))
    make_dt(data.frame(Token = names(p), Score = as.numeric(p)))
  })

  dfItemsByFreeText <- reactive({
    req(smrObj())
    prof <- lsProfileFreeProf()
    if (is.null(prof) || length(prof) == 0) return(NULL)

    smrObj() %>%
      SMRMonApplyTagTypeWeights(weights = tagTypeWeights(), default = 1) %>%
      SMRMonRecommendByProfile(profile = prof, nrecs = input$numberOfRecs, normalizeQ = TRUE) %>%
      SMRMonTakeValue()
  })

  dfProfileByFreeText <- reactive({
    req(smrObj())
    items <- dfItemsByFreeText()
    if (is.null(items)) return(NULL)

    smrObj() %>%
      SMRMonProfile(
        history = setNames(items, c("Rating", "Index", "Item")),
        tagTypesQ = TRUE,
        normalizeQ = TRUE
      ) %>%
      SMRMonTakeValue()
  })

  dfProfileByItemsHist <- reactive({
    req(smrObj())
    hist <- lsItemsHist()
    if (is.null(hist) || length(hist) == 0) return(NULL)

    smrObj() %>%
      SMRMonApplyTagTypeWeights(weights = tagTypeWeights(), default = 1) %>%
      SMRMonProfile(history = hist, tagTypesQ = TRUE, normalizeQ = TRUE) %>%
      SMRMonTakeValue()
  })

  output$profileByItems <- renderDT({
    df <- dfProfileByItemsHist()
    if (is.null(df)) return(make_dt(data.frame()))
    make_dt(df)
  })

  output$profileByText <- renderDT({
    df <- dfProfileByFreeText()
    if (is.null(df)) return(make_dt(data.frame()))
    make_dt(df)
  })

  output$profileByBoth <- renderDT({
    a <- dfProfileByItemsHist()
    b <- dfProfileByFreeText()

    if (is.null(a) && is.null(b)) return(make_dt(data.frame()))

    bind_rows(a, b) %>%
      group_by(TagType, Tag) %>%
      summarise(Score = sum(Score), .groups = "drop") %>%
      arrange(desc(Score)) %>%
      select(Score, TagType, Tag) %>%
      make_dt()
  })

  # ---- Summary -------------------------------------------------------------

  output$dims <- renderText({
    req(smrObj())
    paste(dim(smrObj() %>% SMRMonTakeM), collapse = " x ")
  })

  output$tagRanges <- renderDT({
    req(smrObj())
    make_dt(smrObj() %>% SMRMonTakeTagTypeRanges, height = 420)
  })
}

shinyApp(ui, server)
