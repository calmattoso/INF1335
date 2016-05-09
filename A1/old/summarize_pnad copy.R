# Assignment 1 - Carlos Mattoso - 1210553
# =======================================
#
# The following script was used to lowercase the names of the files
# in the `Input` folders for years 2003 through 2006:
# >> for f in */Input/*; do mv "$f" "`echo $f | tr "[:upper:]" "[:lower:]"`"; done
#

# uncomment the following line if SAScii is not installed.
# install.packages('SAScii')

library(SAScii)

# set this line if on Unix systems as the encoding of some files (e.g. SAS)
# description files is messed up
options(encoding = "windows-1252");

# Handy function to load RData files into object.
load.rdata.into.obj <- function(input) {
  env <- new.env()
  nm <- load(input, env)[1]
  env[[nm]]
}

# Basic setup
#years = 2001:2003 # run separately, due to lack of per capita income variable
 years = 2004:2008

states = c("Rondonia", "Acre", "Amazonas", "Roraima", "Para",
           "Amapa", "Tocantins", "Maranhao", "Piaui", "Ceara",
           "Rio Grande do Norte", "Paraiba", "Pernambuco", "Alagoas",
           "Sergipe", "Bahia", "Minas Gerais", "Espirito Santo",
           "Rio de Janeiro", "Sao Paulo", "Parana", "Santa Catarina",
           "Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso",
           "Goias", "Distrito Federal");
results = data.frame(row.names = states) # data frame with avg income and dengue rate per state

for (year in years) {
  file.header <- paste('./data/', year, '/', sep='')
  file.path   <- paste(file.header, 'Dados/DOM', year, '.txt', sep='')
  
  print(paste("Processing data for the year ", year, "...", sep=''))
  
  if(year >= 2007) {
    # for 2007 and 2008 there's a data.frame that describes the variables
    # in the input file.
    dic <- load.rdata.into.obj(paste(file.header, '/Leitura em R/dicPNAD',
                                     year, '.RData', sep=''))
    pnad.data <- read.fwf(file.path, widths = dic$tamanho)
    # rename the columns to the standard spec
    names(pnad.data) <- dic$cod
  }
  else {
    # For the other years, there's a SAS data file in each year's `Input` folder
    # that contains the variables' descriptions for the dataset. Just read the 
    # dataset directly using the loaded SAS library.
    pnad.data <- read.SAScii(file.path, 
                             paste(file.header, 'Input/input dom', year, '.txt', sep=''),
                             intervals.to.print = 10000, buffersize = 10000)
  }
  
  # Aggregate the data to the results data frame
  # summarize the per capita income grouped by UF, ignoring missing data
  
  # Before 2004 the per capita household income per UF was not reported. Hence, 
  #   the monthly value is used instead.
  income.code = 'V4621'
  if (year < 2004) {
    income.code = 'V4614'
  }
  
  # Some entries have an extremely high per capita or montly income, which
  #   severely skews the mean income per state. I'm hard pressed to believe
  #   a houshold actually had almost 1 trillion reais in income in 2001, so
  #   these outliers are ignored.
  valid.data = pnad.data[income.code] < 900000000000 
  avg.income = tapply(pnad.data[income.code][valid.data], 
                      pnad.data$UF[valid.data], mean, na.rm=TRUE)
  names(avg.income) <- states
  
  results <- cbind(results, avg.income)
  
  # clean up data for current year
  remove(pnad.data);
  gc();
}

# rename the columns to adhere to the format `<year>.income`.
names(results) = lapply(years, paste, '.income', sep='')
View(results)

# !!! IMPORTANT !!!
# Run from this point onward to not preprocess the data again
# results = load.rdata.into.obj("./data/results_2001_2003.RData")
# years.under.analysis = 2001:2003

results = load.rdata.into.obj("./data/results_2004_2008.RData")
years.under.analysis = 2004:2008

names(results) = years.under.analysis

# now, read the dengue incidence rate dataset
dengue = read.csv("./data/dengueUF.csv", dec=",", na.strings = '-')
dengue.cols = sapply('X', paste, years.under.analysis, sep='', USE.NAMES = FALSE)
dengue.subset = dengue[1:27, dengue.cols]

dengue.new.cols = vector(mode="character", length =length(dengue.cols))
for (i in 1:length(names(dengue.subset))) {
  dengue.new.cols[i] = substr(names(dengue.subset)[i], 2, 5)
}
names(dengue.subset) = dengue.new.cols
row.names(dengue.subset) = dengue$UF[1:27]

# Calculate the cross-UF correlation. This tells us whether higher income states
# have a relatively lower dengue incidence rate when compared to others
cross.state.cor = diag(cor(results, dengue.subset))
names(cross.state.cor) = years.under.analysis
  
# Calculate the intra-UF correlation. This tells us whether higher income in a
# state is associated with a lower dengue incidence rate **within** that state.
# In this case, the state is evaluated on its own.
A <- as.matrix(results)
B <- as.matrix(dengue.subset)
intra.state.cor = sapply(seq.int(dim(A)[1]), function(i) cor(A[i,], B[i,]))
names(intra.state.cor) = dengue$UF[1:27]

cross.state.cor
mean(cross.state.cor)

intra.state.cor
mean(intra.state.cor)

cross.title = paste('Cross-state correlation of income and dengue incidence rates for years', 
                    years.under.analysis[1], 'through', tail(years.under.analysis, n=1))
intra.title = paste('Intra-state correlation of income and dengue incidence rates for years', 
                    years.under.analysis[1], 'through', tail(years.under.analysis, n=1))

if (years.under.analysis[1] == 2004) { # 2004..2008
  layout(matrix(c(1,2,3,4,5,5), 3, 2, byrow = TRUE))
  xlab.str = "Mean annual income per capita"
} else { # 2001..2003
  layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
  xlab.str = "Monthly household income"
}

for (i in 1:length(years.under.analysis)) {
  year = as.character(years.under.analysis[i])
  plot(results[,year], dengue.subset[,year], ylab="Dengue incidence rate (per 100,000)", 
       xlab=xlab.str, main = year)
}

par(mfrow=c(1,1))
plot(cross.state.cor, xlab="Year", ylab="Correlation", xaxt = "n", main=cross.title)
#abline(h=mean(cross.state.cor), col="red")
axis(side=1, at=1:length(cross.state.cor), labels=names(cross.state.cor))
boxplot(cross.state.cor, ylab = 'Correlation', main=cross.title)

plot(intra.state.cor, xlab="UF", ylab="Correlation\n", 
     xaxt = "n", main=intra.title)
#abline(h=mean(intra.state.cor), col="red")
axis(side=1, at=1:length(intra.state.cor), labels=names(intra.state.cor), las=2)
boxplot(intra.state.cor, ylab = 'Correlation', main=intra.title)

