Define a defensive function that calculates the Gross Domestic Product
of a nation from the data available in the gapminder dataset. Using that
function, calculate the GDP of Denmark in the following years: 1967,
1977, 1987, 1997, 2007, and 2017.

``` r
# Defining function
gdp_nation_calc <- function(data, year = NULL, country = NULL){
  stopifnot(is.numeric(year)) # error message if year input is not numeric
  stopifnot(is.character(country)) # error message if country input is not character
  stopifnot(is.data.frame(data)) # error message if data input is not data frame
  
  if (!is.null(year)){ # if year is not null (= if any year(s) was/were defined)
    data <- data[data$year %in% year, ] # then subset by the defined year(s)
  }
  if (!is.null(country)){ # if country is not null (= if any country/countries was/were defined)
    data <- data[data$country %in% country, ] # then subset by the defined country/countries
  }
  
  gdp_nation <- data$gdpPercap * data$pop # calculating gdp per nation by multiplying gdp per capita with population
  
  with_country_year <- cbind(data[,c(1,3)], gdp_nation = gdp_nation) # for a better output - put the vector of gdps with the relevant country/year
  return(with_country_year) # and return these
}

# Calculating the GDP of Denmark in the following years: 1967, 1977, 1987, 1997, 2007, and 2017
gdp_nation_calc(data = gapminder, year = c(1967, 1977, 1987, 1997, 2007, 2017), country = "Denmark")
```

    ##   country year   gdp_nation
    ## 1 Denmark 1967  77116977700
    ## 2 Denmark 1977 103920280028
    ## 3 Denmark 1987 128771236166
    ## 4 Denmark 1997 157476118456
    ## 5 Denmark 2007 192906627081

Write a script that loops over each country in the gapminder dataset,
tests whether the country starts with a ‘B’ , and print out whether the
life expectancy is smaller than 50, between 50 and 70, or greater than
70.

``` r
# countries starting with B
b_countries <- grep("^B", unique(gapminder$country), value=TRUE)

for (country in unique(gapminder$country)){
  tmp <- mean(gapminder$lifeExp[gapminder$country == country], na.rm = TRUE) # temporary variable storing mean of life expectancy in the countries
  
  if (country %in% b_countries){ # if the country starts with "B"
    
    if (tmp < 50){
    p <- "Life expectancy is smaller than 50 years here."
  } else if (tmp < 70){
    p <- "Life expectancy is between 50 and 70 years here."
  } else {
    p <- "Life expectancy is greater than 70 years here."
  }
  print(paste("The country", country, "starts with a 'B'.", p)) # Then print "The country XXX starts with a 'B'. Life expectancy is XXX (one of the options above) years here."
  
  } else { # else (if te country does not start with "B")
    print(paste("The country", country, "does not start with a 'B'")) # print "The country xxx does not start with a 'B'.
  }

  rm(tmp) # removing temporary variable storing the mean
}
```

    ## [1] "The country Afghanistan does not start with a 'B'"
    ## [1] "The country Albania does not start with a 'B'"
    ## [1] "The country Algeria does not start with a 'B'"
    ## [1] "The country Angola does not start with a 'B'"
    ## [1] "The country Argentina does not start with a 'B'"
    ## [1] "The country Australia does not start with a 'B'"
    ## [1] "The country Austria does not start with a 'B'"
    ## [1] "The country Bahrain starts with a 'B'. Life expectancy is between 50 and 70 years here."
    ## [1] "The country Bangladesh starts with a 'B'. Life expectancy is smaller than 50 years here."
    ## [1] "The country Belgium starts with a 'B'. Life expectancy is greater than 70 years here."
    ## [1] "The country Benin starts with a 'B'. Life expectancy is smaller than 50 years here."
    ## [1] "The country Bolivia starts with a 'B'. Life expectancy is between 50 and 70 years here."
    ## [1] "The country Bosnia and Herzegovina starts with a 'B'. Life expectancy is between 50 and 70 years here."
    ## [1] "The country Botswana starts with a 'B'. Life expectancy is between 50 and 70 years here."
    ## [1] "The country Brazil starts with a 'B'. Life expectancy is between 50 and 70 years here."
    ## [1] "The country Bulgaria starts with a 'B'. Life expectancy is between 50 and 70 years here."
    ## [1] "The country Burkina Faso starts with a 'B'. Life expectancy is smaller than 50 years here."
    ## [1] "The country Burundi starts with a 'B'. Life expectancy is smaller than 50 years here."
    ## [1] "The country Cambodia does not start with a 'B'"
    ## [1] "The country Cameroon does not start with a 'B'"
    ## [1] "The country Canada does not start with a 'B'"
    ## [1] "The country Central African Republic does not start with a 'B'"
    ## [1] "The country Chad does not start with a 'B'"
    ## [1] "The country Chile does not start with a 'B'"
    ## [1] "The country China does not start with a 'B'"
    ## [1] "The country Colombia does not start with a 'B'"
    ## [1] "The country Comoros does not start with a 'B'"
    ## [1] "The country Congo, Dem. Rep. does not start with a 'B'"
    ## [1] "The country Congo, Rep. does not start with a 'B'"
    ## [1] "The country Costa Rica does not start with a 'B'"
    ## [1] "The country Cote d'Ivoire does not start with a 'B'"
    ## [1] "The country Croatia does not start with a 'B'"
    ## [1] "The country Cuba does not start with a 'B'"
    ## [1] "The country Czech Republic does not start with a 'B'"
    ## [1] "The country Denmark does not start with a 'B'"
    ## [1] "The country Djibouti does not start with a 'B'"
    ## [1] "The country Dominican Republic does not start with a 'B'"
    ## [1] "The country Ecuador does not start with a 'B'"
    ## [1] "The country Egypt does not start with a 'B'"
    ## [1] "The country El Salvador does not start with a 'B'"
    ## [1] "The country Equatorial Guinea does not start with a 'B'"
    ## [1] "The country Eritrea does not start with a 'B'"
    ## [1] "The country Ethiopia does not start with a 'B'"
    ## [1] "The country Finland does not start with a 'B'"
    ## [1] "The country France does not start with a 'B'"
    ## [1] "The country Gabon does not start with a 'B'"
    ## [1] "The country Gambia does not start with a 'B'"
    ## [1] "The country Germany does not start with a 'B'"
    ## [1] "The country Ghana does not start with a 'B'"
    ## [1] "The country Greece does not start with a 'B'"
    ## [1] "The country Guatemala does not start with a 'B'"
    ## [1] "The country Guinea does not start with a 'B'"
    ## [1] "The country Guinea-Bissau does not start with a 'B'"
    ## [1] "The country Haiti does not start with a 'B'"
    ## [1] "The country Honduras does not start with a 'B'"
    ## [1] "The country Hong Kong, China does not start with a 'B'"
    ## [1] "The country Hungary does not start with a 'B'"
    ## [1] "The country Iceland does not start with a 'B'"
    ## [1] "The country India does not start with a 'B'"
    ## [1] "The country Indonesia does not start with a 'B'"
    ## [1] "The country Iran does not start with a 'B'"
    ## [1] "The country Iraq does not start with a 'B'"
    ## [1] "The country Ireland does not start with a 'B'"
    ## [1] "The country Israel does not start with a 'B'"
    ## [1] "The country Italy does not start with a 'B'"
    ## [1] "The country Jamaica does not start with a 'B'"
    ## [1] "The country Japan does not start with a 'B'"
    ## [1] "The country Jordan does not start with a 'B'"
    ## [1] "The country Kenya does not start with a 'B'"
    ## [1] "The country Korea, Dem. Rep. does not start with a 'B'"
    ## [1] "The country Korea, Rep. does not start with a 'B'"
    ## [1] "The country Kuwait does not start with a 'B'"
    ## [1] "The country Lebanon does not start with a 'B'"
    ## [1] "The country Lesotho does not start with a 'B'"
    ## [1] "The country Liberia does not start with a 'B'"
    ## [1] "The country Libya does not start with a 'B'"
    ## [1] "The country Madagascar does not start with a 'B'"
    ## [1] "The country Malawi does not start with a 'B'"
    ## [1] "The country Malaysia does not start with a 'B'"
    ## [1] "The country Mali does not start with a 'B'"
    ## [1] "The country Mauritania does not start with a 'B'"
    ## [1] "The country Mauritius does not start with a 'B'"
    ## [1] "The country Mexico does not start with a 'B'"
    ## [1] "The country Mongolia does not start with a 'B'"
    ## [1] "The country Montenegro does not start with a 'B'"
    ## [1] "The country Morocco does not start with a 'B'"
    ## [1] "The country Mozambique does not start with a 'B'"
    ## [1] "The country Myanmar does not start with a 'B'"
    ## [1] "The country Namibia does not start with a 'B'"
    ## [1] "The country Nepal does not start with a 'B'"
    ## [1] "The country Netherlands does not start with a 'B'"
    ## [1] "The country New Zealand does not start with a 'B'"
    ## [1] "The country Nicaragua does not start with a 'B'"
    ## [1] "The country Niger does not start with a 'B'"
    ## [1] "The country Nigeria does not start with a 'B'"
    ## [1] "The country Norway does not start with a 'B'"
    ## [1] "The country Oman does not start with a 'B'"
    ## [1] "The country Pakistan does not start with a 'B'"
    ## [1] "The country Panama does not start with a 'B'"
    ## [1] "The country Paraguay does not start with a 'B'"
    ## [1] "The country Peru does not start with a 'B'"
    ## [1] "The country Philippines does not start with a 'B'"
    ## [1] "The country Poland does not start with a 'B'"
    ## [1] "The country Portugal does not start with a 'B'"
    ## [1] "The country Puerto Rico does not start with a 'B'"
    ## [1] "The country Reunion does not start with a 'B'"
    ## [1] "The country Romania does not start with a 'B'"
    ## [1] "The country Rwanda does not start with a 'B'"
    ## [1] "The country Sao Tome and Principe does not start with a 'B'"
    ## [1] "The country Saudi Arabia does not start with a 'B'"
    ## [1] "The country Senegal does not start with a 'B'"
    ## [1] "The country Serbia does not start with a 'B'"
    ## [1] "The country Sierra Leone does not start with a 'B'"
    ## [1] "The country Singapore does not start with a 'B'"
    ## [1] "The country Slovak Republic does not start with a 'B'"
    ## [1] "The country Slovenia does not start with a 'B'"
    ## [1] "The country Somalia does not start with a 'B'"
    ## [1] "The country South Africa does not start with a 'B'"
    ## [1] "The country Spain does not start with a 'B'"
    ## [1] "The country Sri Lanka does not start with a 'B'"
    ## [1] "The country Sudan does not start with a 'B'"
    ## [1] "The country Swaziland does not start with a 'B'"
    ## [1] "The country Sweden does not start with a 'B'"
    ## [1] "The country Switzerland does not start with a 'B'"
    ## [1] "The country Syria does not start with a 'B'"
    ## [1] "The country Taiwan does not start with a 'B'"
    ## [1] "The country Tanzania does not start with a 'B'"
    ## [1] "The country Thailand does not start with a 'B'"
    ## [1] "The country Togo does not start with a 'B'"
    ## [1] "The country Trinidad and Tobago does not start with a 'B'"
    ## [1] "The country Tunisia does not start with a 'B'"
    ## [1] "The country Turkey does not start with a 'B'"
    ## [1] "The country Uganda does not start with a 'B'"
    ## [1] "The country United Kingdom does not start with a 'B'"
    ## [1] "The country United States does not start with a 'B'"
    ## [1] "The country Uruguay does not start with a 'B'"
    ## [1] "The country Venezuela does not start with a 'B'"
    ## [1] "The country Vietnam does not start with a 'B'"
    ## [1] "The country West Bank and Gaza does not start with a 'B'"
    ## [1] "The country Yemen, Rep. does not start with a 'B'"
    ## [1] "The country Zambia does not start with a 'B'"
    ## [1] "The country Zimbabwe does not start with a 'B'"

Optional: Write a script that loops over each country in the gapminder
dataset, tests whether the country starts with a ‘M’ and graphs life
expectancy against time (using plot() function) as a line graph if the
mean life expectancy is under 50 years.
