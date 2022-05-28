plot(data$years, data$adj.vector, main = 'Adjusted Tesla Stock Price from 2015-2021', xlab = 'Years', ylab = 'Adjusted Stock Price in Dollars', col.main = 'dark blue',type = 'o', cex.lab=0.9, col.lab = 'blue', col = 'red',
pch = 2)

barplot(Assignment$High, names.arg = Assignment$Date, main = 'Tesla Stocks Highest Price Overall
History', xlab = 'Date (June 2010 - April 2021)', ylab = 'Price in Dollars', las = 1, col = 'green', col.lab = 'blue')

pie(slices,labels = lblz, col=rainbow(length(lbls)),main = "Average Percent of Volume of Tesla Stock traded each month of 2021")

