###############
# Autor: Valentin Koob
###############

list.of.packages <- c("colorspace")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
library(colorspace)



######### Ab hier nur noch Berechnungen und Plots #########

plotNormalPops = function(means, sigma.2) {
  means = unlist(strsplit(means, "[ ]"))
  means = as.numeric(gsub(means, pattern =",", replace = "."))
  groups = 1:length(means)
  sigma.2 = as.numeric(sigma.2)
  par(mar = c(5, 4, 1, 2))
  plot(
    c(1, 2) ~ c(1, 1),
    col = "white",
    xlab = "Moegliche Werte in den Populationen x",
    ylab = "Dichte f(x)",
    axes = F,
    ylim = c(0, dnorm(
      means[1], mean = means[1], sd = sqrt(sigma.2)
    )) * 1.4,
    xlim = c(floor(min(means) - 4 * sqrt(sigma.2)), ceiling(max(means) + 4 *
                                                              sqrt(sigma.2)))
  )

  axis(1)
  axis(2)


  x = seq(floor(min(means) - 4 * sqrt(sigma.2)),  ceiling(max(means) + 4 *
                                                            sqrt(sigma.2)), 0.01)
  for (i in 1:length(means)) {
    #segments(means[i], 0, means[i], dnorm(means[i], mean = means[i], sd = sqrt(sigma.2)), col = "gray")
    dfs = dnorm(x, mean = means[i], sd = sqrt(sigma.2))
    points(dfs ~ x,
           ty = "l",
           col = rainbow_hcl(10)[i],
           lwd = 2)
    polygon(
      c(min(x), x, max(x)),
      c(0, dfs, 0),
      col = adjustcolor(rainbow_hcl(10)[i], alpha.f = 0.2),
      border = NA
    )

  }

}






calcncp = function(ns, means, sigma.2) {
  N = ns * length(means)
  GrandMean = 1 / N * sum(ns * means) # funktioniert auch wenn ns nicht gleich ist pro Gruppe
  sgima_alpha = 1 / N * sum(ns * (means - GrandMean) ^ 2)
  f.2 = sgima_alpha / sigma.2
  ncp = N * f.2
  return(ncp)
}



plotDensities = function(ns, means, sigma.2, alpha) {
  means = unlist(strsplit(means, "[ ]"))
  means = as.numeric(means)
  ns = as.numeric(ns)
  sigma.2 = as.numeric(sigma.2)
  alpha = as.numeric(alpha)
  J = length(means)

  par(mfrow = c(1, 2))
  x = seq(0, 15, 0.01)
  dfs_null = df(x, J - 1, (ns * J) - J)
  plot(
    x,
    rep(1, length(x)),
    col = "white",
    ylim = c(0, 1),
    ylab = "Dichte",
    xlab = "F",
    axes = FALSE,
    main = "F-Verteilung unter der H0"
  )
  axis(side = 1,
       at = seq(0, max(x), 2))
  axis(
    side = 2,
    las = 2,
    at = seq(0, 1, 0.1),
    cex.lab = 1.2,
    cex.axis = 1.2
  )



  points(x, dfs_null, ty = "l", pch = 5)
  x_quantil = qf(1 - alpha, J - 1, (ns * J) - J)
  polygon(c(0, x[x < x_quantil][-1], x_quantil),
          c(0, dfs_null[x < x_quantil][-1], 0),
          col = "green3",
          border = NA)
  polygon(c(x_quantil, x[x > x_quantil], max(x)),
          c(0, dfs_null[x > x_quantil], 0),
          col = "red",
          border = NA)

  segments(x_quantil, 0, x_quantil, .8)
  text(x = x_quantil,
       y = 0.85,
       bquote(F[krit] ~ "=" ~ .(round(x_quantil, 2))),
       cex = 1.2)
  text(x_quantil + 0.7, .1, expression(alpha), cex = 1.2)



  plot(
    x,
    rep(1, length(x)),
    col = "white",
    ylim = c(0, 1),
    ylab = "Dichte",
    xlab = "F",
    axes = FALSE,
    main = "F-Verteilung unter der H1 \n mit gestrichelter H0"
  )
  axis(
    side = 1,
    at = seq(0, max(x), 2),
    cex.lab = 1.2,
    cex.axis = 1.2
  )
  axis(
    side = 2,
    las = 2,
    at = seq(0, 1, 0.1),
    cex.lab = 1.2,
    cex.axis = 1.2
  )

  ncp = calcncp(ns, means, sigma.2)
  dfs_h1 = df(x, J - 1, (ns * J) - J, ncp)
  points(x,
         dfs_h1,
         ty = "l",
         pch = 5,
         col = "black")
  polygon(
    c(0, x[x < x_quantil][-1], x_quantil),
    c(0, dfs_h1[x < x_quantil][-1], 0),
    col = "orange",
    border = NA,
    lty = 0
  )
  polygon(
    c(x_quantil, x[x > x_quantil], max(x)),
    c(0, dfs_h1[x > x_quantil], 0),
    col = "dodgerblue",
    border = NA,
    lty = 0
  )
  segments(x_quantil, 0, x_quantil, .8)
  text(x = x_quantil,
       y = 0.85,
       bquote(F[krit] ~ "=" ~ .(round(x_quantil, 2))),
       cex = 1.2)
  points(x,
         dfs_null,
         ty = "l",
         pch = 5,
         lty = 3)


  ##Griechische Buchstaben
  text(x_quantil - 1, .1, expression(beta), cex = 1.2)
  text(x_quantil + 1.3, .1, expression(1 - beta), cex = 1.2)


  ##Power
  power = 1 - pf(x_quantil, J - 1, (ns * J) - J, ncp)
  text(11, 0.85, paste("f = ", round(sqrt(ncp / (
    ns * J
  )), 2), "\nPower = ", round(power, 2), sep = ""), cex = 1.2)
}




################## Shiny Implementierung


ui <- fluidPage(
  titlePanel("Power-Berechnung im Zuge der einfaktoriellen ANOVA"),
  sidebarLayout(
    sidebarPanel(
      textInput(
        "MittelwerteInput",
        h4("Erwartungswerte \\(\\mu_j\\) (Leerzeichen getrennt)"),
        value = "4 5 6"
      ),
      sliderInput(
        "Sigma.2Input",
        h4(withMathJax("Varianz \\(\\sigma^2\\)")),
        min = 1,
        max = 10,
        value = 4,
        step = 1
      ),
      sliderInput(
        "nsInput",
        h4("geplante Stichprobengroesse n (pro Gruppe)"),
        min = 5,
        max = 30,
        value = 10,
        step = 1
      ),
      sliderInput(
        "alphaInput",
        h4("geplantes \\(\\alpha\\)-Niveau"),
        min = .01,
        max = .2,
        value = .05,
        step = .01
      ),
    ),

    mainPanel(fluidPage(
      h4("Veranschaulichung der Populationen"),
      fluidRow(plotOutput(outputId = "means", width = "60%", height = "200px")),
      h4("Veranschaulichung der Dichteverteilungen"),
      fluidRow(plotOutput(outputId = "densities", width = "90%", height = "350px"))
    ))
  )
)




server <- function(input, output) {

  output$means <- renderPlot({
    plotNormalPops(input$MittelwerteInput, input$Sigma.2Input)
  })

  output$densities <- renderPlot({
    plotDensities(input$nsInput,
                  input$MittelwerteInput,
                  input$Sigma.2Input,
                  input$alphaInput)
  })


}

shinyApp(ui = ui, server = server)
