# --- Shiny Module for an Efficient Frontier Display ---------------------------

library(tidyverse)
library(truncnorm)
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(kableExtra)
library(ggformula)
library(partitions)

set.seed(2805)

discrete_color_scale <- c(
  RColorBrewer::brewer.pal(8, "Set1"), RColorBrewer::brewer.pal(3, "Set3")[1:2]
)
discrete_color_scale[6] <- RColorBrewer::brewer.pal(8, "Dark2")[6]
discrete_color_scale[10] <- RColorBrewer::brewer.pal(10, "Paired")[10]

# Some helper functions 
server_parms <- c(
  "nassets", "correlation", "nportfolios",
  "portfolio_type", "show_eff_frontier", "debug"
)

ui_parms <- c(
  "nassets_input", "resample_input", "correlation_input", 
  "nportfolios_input", "portfolio_type_input"
)

rr_display_ui <- function(rdl) {
  do.call(
   rrDisplayUI,
   rdl[c("id", names(rdl)[names(rdl) %in% c(server_parms[1:4], ui_parms)])]
 )  
}

rr_display_server <- function(rdl) {
  do.call(
    rrDisplayServer,
    rdl[c("id", names(rdl)[names(rdl) %in% server_parms])]
  )  
}


# Generate a set of n random weights, naturally adding up to one.
random_weights <- function(n) {
  w <- runif(n)
  w/sum(w)
}


# Returns a two-element vector containing the expected return and its
# standard deviation for portfolio based on the expected returns of
# the assets, its standard deviation, the covariance matrix of the returns
# and the portfoio weights
portfolio_return <- function(assets_exp_rr, assets_cov_mat, assets_weights) {
  exp_var_ret <- sum((assets_weights %*% t(assets_weights)) * assets_cov_mat)
  exp_var_ret <- ifelse(exp_var_ret < 0, 0, sqrt(exp_var_ret)) 
  c(
    sum(assets_weights * assets_exp_rr$exp_ret),
    exp_var_ret,
    assets_weights
  )
}

# All plots start of with the same set of assets

base_assets_rr <- tibble(
  id = 1:10,
  exp_ret = rtruncnorm(10, 0, 0.15, 0.07, 0.05),
  exp_sd_ret = runif(10, 0.02, 0.1)
)

# To prepare a non-random sample of portofolios this is evenly distributed
# across the multi-dimensional space spanned by the assets, I prepare
# compositions of weights. This being prepared prior to starting the surver
# to save processing time.



if (FALSE) {
  
  nweights <- c(10, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000)
  
  df <- expand.grid(
    nassets = 2:10,
    nweights = nweights,
    steps = NA
  )
  
  for(nassets in 10:3) {
    for(steps in 1:1e5) {
      weights <- as.matrix(t(compositions(n = steps, m = nassets)/steps))
      nw <- nrow(weights)
      if (length(df$steps[df$nassets == nassets & is.na(df$steps) & nw > df$nweights]) > 0) {
        message(sprintf("steps: %d, nassets: %d, nweights: %d", steps, nassets, nw))
        df$steps[df$nassets == nassets & is.na(df$steps) & nw > df$nweights] <- steps - 1
      }
      if(all(!is.na(df$steps[df$nassets == nassets]))) break()
    }
  }
  df$steps[df$nassets == 2] <- df$nweights[df$nassets == 2] - 1
  saveRDS(df, "composition_parms.RDS")
}

composition_parms <- readRDS("composition_parms.RDS")

# The followinng two function simply return a static ggplot of the efficient
# frontier display but take different parameters

plot_rr_display <- function(all_assets, sel_assets, prr) {
  ggplot() +
    geom_point(
      data = prr, aes(x = exp_sd_ret, y = exp_ret),
      color = "gray", alpha = 0.5, size = 0.5
    )  +
    geom_point(
      data= sel_assets, 
      aes(x = exp_sd_ret, y = exp_ret, color = as.factor(id)), 
      size = 4
    ) +
    scale_color_manual(values = discrete_color_scale) +
    scale_x_continuous(
      name = "\u03C3 (Risk assessed by expected standard deviation of returns)",
      limits =  c(0, max(all_assets$exp_sd_ret)),
      breaks = pretty(c(0, max(all_assets$exp_sd_ret)))[-1],
      labels = scales::percent,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = "\u03BC (Return assessed by expected mean of returns)",
      limits = c(0, max(all_assets$exp_ret)),
      breaks = pretty(c(0, max(all_assets$exp_ret)))[-1],
      labels = scales::percent, 
      expand = c(0, 0) 
    ) +
    theme_classic() +
    theme(legend.position = "none")
}


static_rr_display <- function(
  all_assets = base_assets_rr, nassets = 2, correlation = 0.2, 
  nportfolios = 100, portfolio_type = "even"
) {
  sel_assets <- all_assets[1:nassets, ]
  
  correlation <- max(- 1/(nassets -1), correlation)
  
  cor_mat <- matrix(
    nrow = nassets, ncol = nassets, correlation
  )
  diag(cor_mat) <- 1
  cov_mat <- t(t(cor_mat * sel_assets$exp_sd_ret) * sel_assets$exp_sd_ret)
  
  if(portfolio_type == "random") {
    prr <- do.call(
      rbind, replicate(
        nportfolios,
        portfolio_return(
          sel_assets, cov_mat, random_weights(nassets)
        ), 
        simplify = FALSE
      )
    )
  } else {
    steps <- composition_parms$steps[
      composition_parms$nassets == nassets &
        composition_parms$nweights == nportfolios
      ]
    if (nassets > 2) weights <- as.matrix(t(compositions(
      n = steps, m = nassets
    )/steps))
    # too avoid memory stress from the large compositions for
    # two assets and 100,000 portfolios
    else weights <- cbind(
      seq(0, 1, length.out = nportfolios),
      1 - seq(0, 1, length.out = nportfolios)
    )
    prr <- t(apply(
      weights, 1, 
      function(x) portfolio_return(sel_assets, cov_mat, x)
    )) 
  }
  prr <- as_tibble(prr)
  names(prr) <- c(
    "exp_ret", "exp_sd_ret", paste0("weight_asset_", 1:(ncol(prr)-2))
  )

  plot_rr_display(all_assets, sel_assets, prr)
}






# What follows is a Shiny module that generates a risk and return
# graph and the corresponding user interface.
# See https://shiny.rstudio.com if you want to understand the code and learn 
# how to write shiny apps. Give it a try - it can be a lot of fun!


rrDisplayInput <- function(
  id, nassets = 2, correlation = 0.0, nportfolios = 100, 
  portfolio_type = "even", 
  nassets_input = TRUE, resample_input = TRUE, correlation_input = TRUE, 
  nportfolios_input = TRUE, portfolio_type_input = TRUE
) {
  add_tag <- function(tl, ne) {
    tl[[length(tl) + 1]] <- ne
    tl
  }
  ns <- NS(id)
  tl <- tagList()
  if (nassets_input) {
    tl <- add_tag(tl, 
                  sliderInput(
                    ns("nassets"),
                    "Hoy many assets do you want to plot?",
                    min = 2,
                    step = 1,
                    max = 10,
                    value = ifelse(!is.null(nassets), nassets, 2),
                    round = TRUE
                  ))
    tl <- add_tag(tl, hr())
  }
  if (resample_input) {
    tl <- add_tag(
      tl, helpText(
        "Click below when you want to display a new random set of assets"
      )
    )
    tl <- add_tag(tl, actionButton(ns("resample"), "Resample assets"))
    tl <- add_tag(tl, hr())
  }
  if (correlation_input) {
    tl <- add_tag(tl, uiOutput(ns("correlation_slider")))
    tl <- add_tag(tl, helpText(
      "Please note that the minimum possible correlation increases with",
      "the number of assets that you include"
    ))
    tl <- add_tag(tl, hr())
  }
  
  if (nportfolios_input) {
    tl <- add_tag(tl, sliderTextInput(
      ns("nportfolios"),
      "How many portfolios do you want to plot?",
      choices = c(
        "10", "50", "100", "200", "500", "1,000", 
        "2,000", "5,000", "10,000", "20,000", "50,000", "100,000"
      ),
      selected = ifelse(
        is.null(nportfolios), "100", format(nportfolios, big.mark = ",")
      )
    ))
    tl <- add_tag(tl, helpText(
      "Selecting many portfolios will cause plotting to take longer.",
      "Thanks for being patient ;-)"
    ))
  }   
  
  if(portfolio_type_input) tl  <- add_tag(tl, radioButtons(
    ns("portfolio_type"),
    "Do you want your portfolio weights to be random or evenly spaced?",
    c("Random" = "random", "Evenly spaced" = "even"),
    ifelse(is.null(portfolio_type), "even", portfolio_type)
  ))
  tl
}


rrDisplayOutput <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(plotOutput(ns("eff_frontier_plot"), click = ns("plot_clicked"))),
    tableOutput(ns("asset_weights")),
    tableOutput(ns("portfolio_return"))
  )
}


rrDisplayUI <- function(
  id, nassets = 2, correlation = 0.0, nportfolios = 100, 
  portfolio_type = "even", 
  nassets_input = TRUE, resample_input = TRUE, correlation_input = TRUE, 
  nportfolios_input = TRUE, portfolio_type_input = TRUE
) {
  ns <- NS(id)
  tl_input <- rrDisplayInput(
    id, nassets, correlation, 
    nportfolios, portfolio_type,
    nassets_input, resample_input, correlation_input, 
    nportfolios_input, portfolio_type_input
  )
  ifelse(
    length(tl_input) > 0,
    tagList(
      fluidRow(
        column(8, rrDisplayOutput(id)),
        column(4, wellPanel(tl_input)),
      )
    ),
    tagList(
      fluidRow(
        column(8, rrDisplayOutput(id)),
        column(4,p()) 
      )
    )
  )
}

rrDisplayServer <- function(
  id, nassets = 2, correlation = 0.0, nportfolios = 100,
  portfolio_type = "even", show_eff_frontier = FALSE, debug = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
      debug_message <- function(msg, tstamp = TRUE) {
        # Would be nice to include the calling function name automatically
        # but this seems extremely hard to do well given the crazy shiny
        # call stack.
        # See https://stackoverflow.com/questions/7307987/logging-current-function-name
        # for pointers
        if (debug) {
          msg <- sprintf("(%s): %s", id, msg)
          if (tstamp) {
            msg <- sprintf("%s (%s)", msg, format(Sys.time(), "%H:%M:%OS3"))
          }
          message(msg)
        }
      }
      rv <- reactiveValues(
        nassets = nassets,
        correlation = correlation,
        nportfolios = nportfolios,
        portfolio_type = portfolio_type,
        v = 0
      )
      
      observeEvent(
        input$nassets, {
          rv$nassets = input$nassets
        })
      observeEvent(input$resample, {
        rv$v <- rv$v + 1
      })
      observeEvent(
        input$correlation, {
          rv$correlation = input$correlation
        })
      # shinyWidgets::sliderTextInput() allows us to set custom steps but
      # returns a string. The below converts this back to an integer
      observeEvent(
        input$nportfolios, {
          rv$nportfolios = as.integer(gsub(",", "", input$nportfolios))
        })
      observeEvent(
        input$portfolio_type, {
          rv$portfolio_type = input$portfolio_type
        })
      
      sel_weights <- reactiveVal()
      
      assets_sample_rr <- reactive({
        debug_message("randomizing assets")
        if (rv$v == 0) base_assets_rr 
        else tibble(
          id = 1:10,
          exp_ret = rtruncnorm(10, 0, 0.15, 0.07, 0.05),
          exp_sd_ret = runif(10, 0.02, 0.1)
        )
      })
      
      assets_rr <- reactive({
        rep(rv$nassets)
        debug_message(sprintf(
          "returning risk and return for %d assets", rv$nassets
        ))
        assets_sample_rr()[1:rv$nassets, ]
      })
      
      assets_cov_mat <- reactive({
        req(rv$correlation >= - 1/(rv$nassets -1), assets_rr())
        debug_message(sprintf(
          "setting covariante matrix, correlation: %.1f", rv$correlation
        ))
        isolate(sel_weights(NULL))
        cor_mat <- matrix(
          nrow = rv$nassets, ncol = rv$nassets, rv$correlation
        )
        diag(cor_mat) <- 1
        t(t(cor_mat * assets_rr()$exp_sd_ret) * assets_rr()$exp_sd_ret)
      })
      
      portfolios_rr <- reactive({
        req(assets_rr(), assets_cov_mat(), rv$portfolio_type)
        debug_message(sprintf(
          "calculating %d %s portfolio returns", rv$nportfolios,
          ifelse(rv$portfolio_type == "random", "random", "evenly spaced")
        ))
        if(rv$portfolio_type == c("random")) {
          prr <- do.call(
            rbind, replicate(
              rv$nportfolios,
              portfolio_return(
                assets_rr(), assets_cov_mat(), random_weights(rv$nassets)
              ), 
              simplify = FALSE
            )
          )
        } else {
          steps <- composition_parms$steps[
            composition_parms$nassets == rv$nassets &
              composition_parms$nweights == rv$nportfolios
            ]
          if (rv$nassets > 2) weights <- as.matrix(t(compositions(
            n = steps, m = rv$nassets
          )/steps))
          # too avoid memory stress from the large compositions for
          # two assets and 100,000 portfolios
          else weights <- cbind(
            seq(0, 1, length.out = rv$nportfolios),
            1 - seq(0, 1, length.out = rv$nportfolios)
          )
          prr <- t(apply(
            weights, 1, 
            function(x) portfolio_return(isolate(assets_rr()), assets_cov_mat(), x)
          )) 
        }
        df <- as_tibble(prr) 
        names(df) <- c(
          "exp_ret", "exp_sd_ret", paste0("weight_asset_", 1:(ncol(df)-2))
        )
        isolate(sel_weights(NULL))
        debug_message("done!")
        df
      }) 
      
      output$correlation_slider <- renderUI({
        # See https://stats.stackexchange.com/questions/72790/bound-for-the-correlation-of-three-random-variables
        # for a discussion of the minimum common correlation that is possible for
        # a given number of variables
        ns <- session$ns
        
        req(rv$nassets)
        debug_message(sprintf(
          "Adjusting the correlation slider UI for %d assets", rv$nassets
        ))
        min_corr <- - 1/(rv$nassets -1)
        min_corr <- trunc(10 * min_corr)/10
        correl <- isolate(rv$correlation) 
        def_value <- max(ifelse(is.null(correl), 0, correl), min_corr) 
        sliderInput(
          ns("correlation"),
          "Choose the common correlation for your assets",
          min = min_corr,
          step = 0.1,
          max = 1,
          value = def_value,
          round = -1
        )
      })
      
      output$eff_frontier_plot <- renderPlot({
        req(
          assets_rr(), portfolios_rr(),
          rv$correlation >= - 1/(rv$nassets -1)
        )
        debug_message(sprintf(
          "Potting (%d assets, %d portfolios, correlation: %.1f)", 
          rv$nassets, rv$nportfolios, rv$correlation
        ))    
        #  browser()
        if (show_eff_frontier) {
          df <- portfolios_rr() %>%
            mutate(
              exp_sd_ret_rk = cut(exp_sd_ret, 100)
            ) %>%
            group_by(exp_sd_ret_rk) %>%
            summarize(
              exp_ret = max(exp_ret),
              exp_sd_ret = mean(exp_sd_ret),
              .groups = "drop"
            ) %>%
            arrange(exp_sd_ret) %>%
            filter(exp_ret >= lag(exp_ret) | exp_ret >= lead(exp_ret) |
                     exp_ret == min(exp_ret) | exp_ret == max(exp_ret))
        }
        
        p <- plot_rr_display(assets_sample_rr(), assets_rr(), portfolios_rr())
        
        if (show_eff_frontier) {
          p <- p +
            geom_spline(
              data = df, 
              mapping = aes(x = exp_sd_ret, y = exp_ret),
              color = "blue", size = 2
            )
        }
        
        if (!is.null(sel_weights())) {
          p <- p + 
            geom_point(
              data = portfolios_rr()[isolate(sel_weights()),], 
              mapping = aes(x = exp_sd_ret, y = exp_ret),
              color = "orange", size = 2
            )
        }
        
        p
      })
      
      observeEvent(input$plot_clicked, {
        debug_message(sprintf(
          "plot_clicked is NULL: %s", as.character(is.null(input$plot_clicked))
        ))
        if(!is.null(input$plot_clicked)) {
          click <- input$plot_clicked
          dist <- sqrt(
            (click$x - portfolios_rr()$exp_sd_ret)^2 + 
              (click$y - portfolios_rr()$exp_ret)^2
          )
          if (min(dist) < 0.05) sel_weights(which.min(dist))
        }    
      })
      
      output$asset_weights <- function() {
        debug_message(sprintf(
          "sel_weights is NULL: %s", 
          as.character(is.null(sel_weights()))
        ))
        req(rv$nassets, portfolios_rr())
        weights <- rep(NA, rv$nassets)
        if(!is.null(sel_weights())) weights <- unname((as_vector(
          portfolios_rr()[sel_weights(), 3:ncol(portfolios_rr())]
        )))
        tibble(
          Asset = paste0(
            "Asset ", 1:rv$nassets, 
            " (\U00B5 = ", sprintf("%.3f", assets_rr()$exp_ret), 
            ", \U03c3 = ", sprintf("%.3f)", assets_rr()$exp_sd_ret)
          ),
          Weight = ifelse(is.na(weights), "", sprintf("%.3f", weights))
        ) %>% kable(align = "lr") %>% kable_styling(full_width = F) %>%
          column_spec(1, color = discrete_color_scale[1:rv$nassets])
      }
      
      output$portfolio_return <- function() {
        debug_message(sprintf(
          "sel_weights is NULL: %s", 
          as.character(is.null(sel_weights()))
        ))
        if (!is.null(sel_weights())) {
          tibble(
            " " = "Portfolio",
            "Expected Return (\U00B5)" = sprintf(
              "%.3f", portfolios_rr()[sel_weights(), 1]
            ), 
            "Expected Standard Deviation (\U03C3)" = sprintf(
              "%.3f", portfolios_rr()[sel_weights(), 2]
            )
          ) %>% kable(align = "lrr") %>% kable_styling(full_width = F)
        }
      }      
    }
  )
}
