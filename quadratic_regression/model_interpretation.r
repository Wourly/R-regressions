rm(list=ls())
graphics.off()  #clears plot
dev.new()       #prepares for png output, while preserving RStudio plot

load('model_input.RData')

model <- lm(elements$weight ~ elements$z + I(elements$z^2))

png(file="plot.png", width=600, height=800) #for html results
dev.set(which = 2)

par(mar=c(5,5,1,1))

plot(elements$z, elements$weight, xlab="", ylab="", cex=1 + (elements$weight/250), pch=19, col=c("#CC0000", "#00CC00", "#0000CC"))

mtext(side=1, line=3, "Z", col="#00AA00", font=2,cex=1.5)
mtext(side=2, line=3, "Atomic mass", col="#FF8C00", font=2,cex=1.5)

data <- summary(model)
confidence_intervals <- confint(model, level=0.95)
coefficients <- data[["coefficients"]]

B0.coefficient <- coefficients[1, 1]
B0.deviation <- coefficients[1, 2]
B0.confidence_interval <-confidence_intervals[1,]
  
B1.coefficient <- coefficients[2, 1]
B1.deviation <- coefficients[2, 2]
B1.confidence_interval <-confidence_intervals[2,]

B2.coefficient <- coefficients[3, 1]
B2.deviation <- coefficients[3, 2]
B2.confidence_interval <-confidence_intervals[3,]

create_curve <- function (B0, B1, B2, line_type = 1, color="black", line_width = 1)
{
  min_z <- min(elements$z)
  max_z <- max(elements$z)
  
  line_points_template <- seq(from=min_z, to=max_z, by=max_z/100)
  rm(min_z, max_z)
  
  line_points <- c()
  
  for (point in line_points_template)
  {
    line_points <- append(line_points, B0 + B1 * point + B2 * (point^2))
  }
  
  lines(line_points_template, line_points, lwd=line_width, lty=line_type, col=color)
  
  rm(line_points_template, line_points, point)
}

create_curve(B0.coefficient, B1.coefficient, B2.coefficient, line_width=4, color="#FFCC00")
create_curve(B0.confidence_interval[1], B1.confidence_interval[1], B2.confidence_interval[1], line_type=3, color="#777777", line_width=2)
create_curve(B0.confidence_interval[2], B1.confidence_interval[2], B2.confidence_interval[2], line_type=3, color="#777777", line_width=2)
rm(create_curve)


dev.copy(which = 4)
dev.off()

#presenting results into html file
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
Relative atomic mass
</h1>
<p>
Correlation between atomic mass and atomic number Z.
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
    <td>
      <b>B<sub>2</sub></b>
    </td>
  </tr>
  
  <tr>
    <td colspan="4">
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
    <td>
      __COEFF2
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
    <td>
      __DEVI2
    </td>
  </tr>
  
  <tr>
    <td colspan="4">
      <b>Confidence interval (95%) limits</b>
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
    <td>
      __ULCONF2
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
    <td>
      __LLCONF2
    </td>
  </tr>
  
</table>

<img src="plot.png">

<p>
Data were obtained from 
<a target="_blank" href="https://en.wikipedia.com">Wikipedia</a><br>
exactly from <i><a target="_blank" href="https://en.wikipedia.org/wiki/List_of_chemical_elements">List of chemical elements</a></i>.
</p>
</body>
</html>
'

#html_string <- gsub("[\r\n]", "", html_string)

html_string <- gsub("__COEFF0", as.character(B0.coefficient), html_string)
html_string <- gsub("__COEFF1", as.character(B1.coefficient), html_string)
html_string <- gsub("__COEFF2", as.character(B2.coefficient), html_string)

html_string <- gsub("__DEVI0", as.character(B0.deviation), html_string)
html_string <- gsub("__DEVI1", as.character(B1.deviation), html_string)
html_string <- gsub("__DEVI2", as.character(B2.deviation), html_string)

html_string <- gsub("__ULCONF0", as.character(B0.confidence_interval[2]), html_string)
html_string <- gsub("__ULCONF1", as.character(B1.confidence_interval[2]), html_string)
html_string <- gsub("__ULCONF2", as.character(B2.confidence_interval[2]), html_string)

html_string <- gsub("__LLCONF0", as.character(B0.confidence_interval[1]), html_string)
html_string <- gsub("__LLCONF1", as.character(B1.confidence_interval[1]), html_string)
html_string <- gsub("__LLCONF2", as.character(B2.confidence_interval[1]), html_string)

html_file <- file("results.html")
write(html_string, html_file)
close(html_file)

rm(html_file, html_string)