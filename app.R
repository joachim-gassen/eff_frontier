source("shiny_module_rr_display.R")

# --- Define Risk and Return Displays ------------------------------------------

rr_displays <- list(
  list(
    id = "base", nassets_input = FALSE, resample_input = FALSE, 
    correlation_input = FALSE,  nportfolios_input = FALSE, 
    portfolio_type_input = FALSE
  ),
  list(
    id = "correlation", nassets_input = FALSE, resample_input = FALSE, 
    nportfolios_input = FALSE, 
    portfolio_type_input = FALSE
  ),
  list(
    id = "eff_frontier", show_eff_frontier = TRUE, nassets_input = FALSE, 
    resample_input = FALSE, correlation_input = TRUE,  
    nportfolios_input = FALSE, portfolio_type_input = FALSE
  ),
  list(
    id = "three_assets", nassets = 3, nportfolios = 1000,
    show_eff_frontier = TRUE, nassets_input = FALSE, 
    resample_input = FALSE, correlation_input = FALSE,  
    nportfolios_input = FALSE, portfolio_type_input = FALSE
  ),
  list(
    id = "three_assets_flexible", nassets = 3, nportfolios = 1000,
    portfolio_type = "even", portfolio_type_input = FALSE
  ),
  list(
    id = "fully_flexible", nassets = 3, nportfolios = 1000,
    portfolio_type = "random"
  )
)

# --- User Interface -----------------------------------------------------------

ui <- fluidPage(
  titlePanel("Meet the Markowitz Efficient Frontier"),
  HTML("<br><br><br>"),
  fluidRow(
    column(
      8,
      p(
        "The Efficient Frontier not only has won Harry Markowitz the",
        "Nobel memorial prize in Economic Sciences in 1990", 
        "but it might be one of the insights generated",
        "by financial economists that is most relevant for our everyday",
        "lives. It can be summarized by: If you invest, don't put all",
        "your eggs in one nest, but diversify your wealth across as many",
        "assets as possible."
      ),
      p(
        "Please allow me to elaborate. Below, you see a risk/return plot.",
        "It shows two assets, indicated by the larger colored dots.",
        "The vertical axis reports the expected rate of return of the assets",
        "in percent (\U00B5) while the horizontal axis measures",
        "the risk of a given asset, meaning how volatile we expect its",
        "future returns to be (\U03C3)."
      ),
      p(
        "You see that the blue asset is expected to be both, more profitable",
        "and riskier than the red asset. Assuming that you want to invest",
        "some money in these two assets: How could you allocate your money",
        "on these two assets and what risk/return payoff could you expect?"
      ),
      p(
        "The gray dots between the two assets indicate some possible",
        "investment strategies or portfolios as financial economists like",
        "to call them. Click on any gray point to see the assets mix", 
        "(the 'weights') required to construct the portfolio."
      )
    )
  ),
  rr_display_ui(rr_displays[[1]]),
  fluidRow(
    column(
      8,
      p(
        "You will notice that the portfolios do not form a straight line.",
        "Why is that? It is because we assume our two assets not to be",
        "perfectly correlated, meaning that their returns do not perfectly",
        "move together. The starting point depicted below assumes that their",
        "returns are not correlated at all. Below you can change the", 
        "correlation between the two assets. Experiment with it. Which",
        "special cases can you identify?"
      )
    )
  ),
  rr_display_ui(rr_displays[[2]]),
  fluidRow(
    column(
      8,
      p(
        "Think about the portfolio that you would like to invest in.", 
        "Would you like to minimize the risk, or would you like to", 
        "take on some risk for the perspective of higher returns?"
      ),
      p(
        "Whatever you risk preference is, there are some portfolios that",
        "should be not attractive to you in any case. These are the ones",
        "below the 'belly of the curve'. For these portfolios, there exit",
        "other portfolios that have higher returns at the same level of risk.",
        "Even if you really like risk, for a given level of risk you should",
        "always invest into the portfolio that promises a higher rate of",
        "return."
      ),
      p(
        "The visual below high-lights these efficient portfolios.",
        "Change the correlation of the two assets to see how the portfolio",
        "line changes and how the frequency of inefficient portfolios changes.",
        "You will see that the frequency of inefficient portfolio declines",
        "as the assets are becoming more positively correlated."
      )
    )
  ),
  rr_display_ui(rr_displays[[3]]),
  fluidRow(
    column(
      8,
      p(
        "Fine, I hear you say. But in the real world there are many assets",
        "not just two. How does the picture look then? See below.",
        "There, I included another asset and some more portfolios that now",
        "build on three assets."
      )
    )
  ),
  rr_display_ui(rr_displays[[4]]),
  fluidRow(
    column(
      8,
      p(
        "You see that what has been a line has now become an area.", 
        "As you added the third asset, there are now many portfolios that",
        "are not efficient anymore.", 
        "The Efficient Frontier is again highlighted. It is characterized by",
        "those portfolios that delineate the top left curve of the area,",
        "meaning that there are no portfolios available that offer a higher",
        "return for the same level of risk"
      ),
      p(
        "Again, over to you. In the next visual you can change", 
        "the number of assets and portfolios as well as the common", 
        "correlation across the assets.",
        "One remark: When you want to get a",
        "clear visual of the efficient frontier, you need to select",
        "a sufficient number of portfolios to be plotted. This will take",
        "a while. Relax ;-)"
      )
    )
  ),
  rr_display_ui(rr_displays[[5]]),
  
  fluidRow(
    column(
      8,
      p(
        "You might wonder:",
        "'Well, which portfolio on the efficient frontier should I pick?'",
        "Financial Economics has two concepts for you, but I consider both not",
        "to have the same level of general appeal compared to the concept",
        "of the efficient frontier."
      ),
      p(
        "The first answer is: It depends on your risk preference. You",
        "should be able to draw personal indifference lines in the risk-return",
        "space (black curves below), meaning that you can identify risk/return combinations for", 
        "which you are indifferent. Choose the line that just touches the",
        "efficient frontier. This is your portfolio (orange point below)."
      ),
      br(), br(), br(),
      withSpinner(plotOutput("static_risk_pref")),
      br(), br(), br(),
      p(
        "The second answer is: Assume that there is a risk-free asset",
        "like a government bond generating a certain return.",
        "Then you can construct a new portfolio, mixing the risk-free asset (green point below)",
        "with 'the market portfolio' (orange point below). The 'market portfolio' is the point",
        "on the efficient frontier where a line originating from the risk/return",
        "point of the risk-free asset becomes a tangent of the efficient area.",
        "The line (blue below) is also called the capital market line and the Capital Asset",
        "Pricing Model (another corner stone of Financial Economics)",
        "derives conditions under which in equilibrium all market participants",
        "are holding portfolios that are linear combinations of the risk-free",
        "asset and the market portfolio."
      ),
      br(), br(), br(),
      withSpinner(plotOutput("static_capm")),
      br(), br(), br(),
      p(
        "'Why are you not such a big fan of these two concepts compared to the",
        "concept of the efficient frontier?', you might ask.",
        "Well, first they require very rigid assumptions. To draw risk/return",
        "indifference lines, people need to have stable preferences and need",
        "to be aware of these preferences. The Capital Asset Pricing Model is",
        "static in time and builds on homogenous information and expectations",
        "of market participants. Thus, it is an excellent theoretical starting",
        "point for asset pricing but of little use empirically."
      ), 
      p(
        "The reason why I am such a big fan of the Markowitz", 
        "argument regardless lies in its robustness. To make this last point",
        "I prepared a last interactive visual.",  
        "In it, I have included the option to plot randomly distributed", 
        "portfolios. Here, we cannot see the efficient frontier",
        "as we do not depict the entire area of feasible portfolios anymore.",
        "Instead, we see 'monkey portfolios' meaning portfolios that are only",
        "constructed based on the number of assets",
        "and that are thus not dependent on precise expectations of risk and",
        "returns. You see that, at least as long as the assets are modeled to",
        "be only mildly correlated, these random portfolios, by diversifying",
        "across assets, generate returns that are significantly less risky",
        "than the ones you would achieve by betting on one asset,", 
        "also known as putting all eggs in one nest."
      )
    )
  ),
  rr_display_ui(rr_displays[[6]]),
  fluidRow(
    column(
      12,
      hr(),
      HTML(
        "<p><center>Made with <a href=https://www.r-project.org>R</a>",
        "and <a href=https://shiny.rstudio.com>Shiny</a> by",   
        "<a href=https://twitter.com/JoachimGassen> Joachim Gassen</a>,<br>",
        "<a href=https://www.wiwi.hu-berlin.de/rewe>",
        "Humboldt-Universität zu Berlin</a> and",
        "<a href=https://www.accounting-for-transparency.de>",
        "TRR 266 Accounting for Transparency</a>, 2021</center></p>")
    )
  )
)


# --- Shiny Server -------------------------------------------------------------

server <- function(input, output) {
  lapply(rr_displays, rr_display_server)
  
  
  output$static_risk_pref <- renderPlot({
    library(isoband)
    library(sf)
    
    exp_ret<- seq(0, max(base_assets_rr$exp_ret), length.out = 100)
    exp_sd_ret <- seq(0, max(base_assets_rr$exp_sd_ret), length.out = 100)
    
    df <- expand.grid(exp_sd_ret = exp_sd_ret, exp_ret = exp_ret)
    utility <- matrix(
      nrow = 100, ncol = 100, 
      data = 1 - exp(-df$exp_ret +10*df$exp_sd_ret^2), 
      byrow = TRUE
    )
    l <- isolines(exp_sd_ret, exp_ret, utility, seq(0.032, 0.07, length.out = 4))
    
    lines <- iso_to_sfg(l)
    data_lines <- st_sf(
      level = 2:(length(lines)+1),
      geometry = st_sfc(lines)
    )
    
    static_rr_display(nassets = 2, nportfolios = 100) +
      geom_sf(data = data_lines,color = "black") +
      theme_classic() +
      theme(legend.position = "none") +
      geom_point(aes(x = 0.0605, y = 0.069), color = "orange", size = 3)
  })
  
  
  output$static_capm <- renderPlot({
    static_rr_display(nassets = 4, correlation = 0, nportfolios = 100000) +
      geom_segment(
        aes(x = 0, xend = 0.051, y = 0.01, yend = max(base_assets_rr$exp_ret)),
        color = "blue"
      )  +
      geom_point(aes(x = 0.0, y = 0.01), color = "lightgreen", size = 3) +
      geom_point(aes(x = 0.0365, y = 0.0675), color = "orange", size = 3) +
      coord_cartesian(clip = 'off')
  })
}  

# Run the application 
shinyApp(ui = ui, server = server)
