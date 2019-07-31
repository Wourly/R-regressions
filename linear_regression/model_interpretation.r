rm(list=ls())   #clears environment
graphics.off()  #clears plot
dev.new()       #prepares for png output, while preserving RStudio plot

load('model_input.RData')

plants_linear_model <- lm(Diaspore ~ Longevity, data = plants_final)

png(file="plot.png", width=600, height=600) #for html results
dev.set(which = 2)

par(mar = c(5,5,5,5))

with(plants_final,plot(Longevity, Diaspore, pch=16, col="#3cb371", xlab="", ylab=""))

abline(plants_linear_model, cex=1.2, col="#003300")

mtext(side=1, line=3, "Seed bank longevity index", col="#00AA00", font=2,cex=1.5)
mtext(side=2, line=3, "Diaspore mass (mg)", col="#FF8C00", font=2,cex=1.5)

data <- summary(plants_linear_model)

coefficients <- data[["coefficients"]]

get_conf_int <- function (confidence, coefficient, deviation)
{
  confint_lower_border <- (1 - confidence) / 2
  lower_tail_border <- qnorm(confint_lower_border, coefficient, deviation)
  upper_tail_border <- coefficient + coefficient - lower_tail_border
  c(lower_tail_border, upper_tail_border)
}

B0.coefficient <- coefficients[1, 1]
B0.deviation <- coefficients[1, 2]
B0.confidence_interval <- get_conf_int(0.95, B0.coefficient, B0.deviation)

B1.coefficient <- coefficients[2, 1]
B1.deviation <- coefficients[2, 2]
B1.confidence_interval <- get_conf_int(0.95, B1.coefficient, B1.deviation)

abline(a = B0.confidence_interval[1], b = B1.confidence_interval[1], lty = 3, col = "#777777")
abline(a = B0.confidence_interval[2], b = B1.confidence_interval[2], lty = 3, col = "#777777")

dev.copy(which = 4)
dev.off()

file.create('results.html', showWarnings = 1)

html_string <-
'
<!DOCTYPE HTML>
<html>
<head>
  <style>
    table
    {
      border: 0px;
      min-width: 600px;
    }
    td, th
    {
      border: 0;
      margin: 0;
      width: 80px;
      text-align: center;
    }
    td:nth-child(1)
    {
      min-width: 125px;
    }
    td[colspan]
    {
      text-align: left;
      padding-left: 15px;
    }
    a, a:visited
    {
      color: #55F;
      font-weight: 900;
      text-decoration: none;
    }
    a:hover
    {
      color: #00A;
    }
  </style>
</head>
<body>
<h1>
Seed bank longevity
</h1>
<p>
Correlation between seed mass and seed bank longevity index.
</p>
<table cellspacing="0" cellpadding="5">

  <tr>
    <td>
      &nbsp;
    </td>
    <td>
      <b>B<sub>0</sub></b>
    </td>
    <td>
      <b>B<sub>1</sub></b>
    </td>
  </tr>
  
  <tr>
    <td colspan="3">
      <b>Linear model data</b>
    </td>
  </tr>
  
  <tr>
    <td>
      <i>Estimated<br>coefficient</i>
    </td>
    <td>
      __COEFF0
    </td>
    <td>
      __COEFF1
    </td>
  </tr>
  
  <tr>
    <td>
      <i>Standard<br>deviation</i>
    </td>
    <td>
      __DEVI0
    </td>
    <td>
      __DEVI1
    </td>
  </tr>
  
  <tr>
    <td colspan="3">
      <b>Confidence interval (95%) limits</b>
    </td>
  </tr>
  
  <tr>
    <td>
      <i>Lower</i>
    </td>
    <td>
      __LLCONF0
    </td>
    <td>
      __LLCONF1
    </td>
  </tr>
  
  <tr>
    <td>
      <i>Upper</i>
    </td>
    <td>
      __ULCONF0
    </td>
    <td>
      __ULCONF1
    </td>
  </tr>
  
</table>

<img src="plot.png">

<p>
Data were obtained from 
<a target="_blank" href="https://vincentarelbundock.github.io/Rdatasets/datasets.html">vincentarelbundock.github.io</a><br>
from dataset <i><a target="_blank" href="https://vincentarelbundock.github.io/Rdatasets/doc/cluster/plantTraits.html">Plant Species Traits Data</a></i>.
</p>
</body>
</html>
'

#html_string <- gsub("[\r\n]", "", html_string)

html_string <- gsub("__COEFF0", as.character(B0.coefficient), html_string)
html_string <- gsub("__COEFF1", as.character(B1.coefficient), html_string)

html_string <- gsub("__DEVI0", as.character(B0.deviation), html_string)
html_string <- gsub("__DEVI1", as.character(B1.deviation), html_string)

html_string <- gsub("__LLCONF0", as.character(B0.confidence_interval[1]), html_string)
html_string <- gsub("__LLCONF1", as.character(B1.confidence_interval[1]), html_string)

html_string <- gsub("__ULCONF0", as.character(B0.confidence_interval[2]), html_string)
html_string <- gsub("__ULCONF1", as.character(B1.confidence_interval[2]), html_string)

html_file <- file("results.html")
write(html_string, html_file)
close(html_file)

rm(html_file, html_string, get_conf_int)