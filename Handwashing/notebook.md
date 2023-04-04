
## 1. Meet Dr. Ignaz Semmelweis
<p><img style="float: left;margin:5px 20px 5px 1px" src="https://assets.datacamp.com/production/project_49/img/ignaz_semmelweis_1860.jpeg"></p>
<!--
<img style="float: left;margin:5px 20px 5px 1px" src="https://assets.datacamp.com/production/project_49/datasets/ignaz_semmelweis_1860.jpeg">
-->
<p>This is Dr. Ignaz Semmelweis, a Hungarian physician born in 1818 and active at the Vienna General Hospital. If Dr. Semmelweis looks troubled it's probably because he's thinking about <em>childbed fever</em>: A deadly disease affecting women that just have given birth. He is thinking about it because in the early 1840s at the Vienna General Hospital as many as 10% of the women giving birth die from it. He is thinking about it because he knows the cause of childbed fever: It's the contaminated hands of the doctors delivering the babies. And they won't listen to him and <em>wash their hands</em>!</p>
<p>In this notebook, we're going to reanalyze the data that made Semmelweis discover the importance of <em>handwashing</em>. Let's start by looking at the data that made Semmelweis realize that something was wrong with the procedures at Vienna General Hospital.</p>


```R
# Load in the tidyverse package
# .... YOUR CODE FOR TASK 1 ....

library(tidyverse)

# Read datasets/yearly_deaths_by_clinic.csv into yearly
yearly <- read_csv("datasets/yearly_deaths_by_clinic.csv")

# Print out yearly
# .... YOUR CODE FOR TASK 1 ....

yearly
```

    Parsed with column specification:
    cols(
      year = [32mcol_double()[39m,
      births = [32mcol_double()[39m,
      deaths = [32mcol_double()[39m,
      clinic = [31mcol_character()[39m
    )



<table>
<caption>A spec_tbl_df: 12 x 4</caption>
<thead>
	<tr><th scope=col>year</th><th scope=col>births</th><th scope=col>deaths</th><th scope=col>clinic</th></tr>
	<tr><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1841</td><td>3036</td><td>237</td><td>clinic 1</td></tr>
	<tr><td>1842</td><td>3287</td><td>518</td><td>clinic 1</td></tr>
	<tr><td>1843</td><td>3060</td><td>274</td><td>clinic 1</td></tr>
	<tr><td>1844</td><td>3157</td><td>260</td><td>clinic 1</td></tr>
	<tr><td>1845</td><td>3492</td><td>241</td><td>clinic 1</td></tr>
	<tr><td>1846</td><td>4010</td><td>459</td><td>clinic 1</td></tr>
	<tr><td>1841</td><td>2442</td><td> 86</td><td>clinic 2</td></tr>
	<tr><td>1842</td><td>2659</td><td>202</td><td>clinic 2</td></tr>
	<tr><td>1843</td><td>2739</td><td>164</td><td>clinic 2</td></tr>
	<tr><td>1844</td><td>2956</td><td> 68</td><td>clinic 2</td></tr>
	<tr><td>1845</td><td>3241</td><td> 66</td><td>clinic 2</td></tr>
	<tr><td>1846</td><td>3754</td><td>105</td><td>clinic 2</td></tr>
</tbody>
</table>




```R
library(testthat) 
library(IRkernel.testthat)
run_tests({
    test_that("Read in data correctly.", {
        expect_is(yearly, "tbl_df", 
            info = 'You should use read_csv (with an underscore) to read "datasets/yearly_deaths_by_clinic.csv" into yearly.')
    })
    
    test_that("Read in data correctly.", {
        yearly_temp <- read_csv('datasets/yearly_deaths_by_clinic.csv')
        expect_equivalent(yearly, yearly_temp, 
            info = 'yearly should contain the data in "datasets/yearly_deaths_by_clinic.csv"')
    })
})
```






    2/2 tests passed


## 2. The alarming number of deaths
<p>The table above shows the number of women giving birth at the two clinics at the Vienna General Hospital for the years 1841 to 1846. You'll notice that giving birth was very dangerous; an <em>alarming</em> number of women died as the result of childbirth, most of them from childbed fever.</p>
<p>We see this more clearly if we look at the <em>proportion of deaths</em> out of the number of women giving birth. </p>


```R
# Adding a new column to yearly with proportion of deaths per no. births
# .... YOUR CODE FOR TASK 1 ....

yearly <- yearly %>%
mutate(proportion_deaths = deaths / births * 100)

# Print out yearly
yearly
```


<table>
<caption>A spec_tbl_df: 12 x 5</caption>
<thead>
	<tr><th scope=col>year</th><th scope=col>births</th><th scope=col>deaths</th><th scope=col>clinic</th><th scope=col>proportion_deaths</th></tr>
	<tr><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1841</td><td>3036</td><td>237</td><td>clinic 1</td><td> 7.806324</td></tr>
	<tr><td>1842</td><td>3287</td><td>518</td><td>clinic 1</td><td>15.759051</td></tr>
	<tr><td>1843</td><td>3060</td><td>274</td><td>clinic 1</td><td> 8.954248</td></tr>
	<tr><td>1844</td><td>3157</td><td>260</td><td>clinic 1</td><td> 8.235667</td></tr>
	<tr><td>1845</td><td>3492</td><td>241</td><td>clinic 1</td><td> 6.901489</td></tr>
	<tr><td>1846</td><td>4010</td><td>459</td><td>clinic 1</td><td>11.446384</td></tr>
	<tr><td>1841</td><td>2442</td><td> 86</td><td>clinic 2</td><td> 3.521704</td></tr>
	<tr><td>1842</td><td>2659</td><td>202</td><td>clinic 2</td><td> 7.596841</td></tr>
	<tr><td>1843</td><td>2739</td><td>164</td><td>clinic 2</td><td> 5.987587</td></tr>
	<tr><td>1844</td><td>2956</td><td> 68</td><td>clinic 2</td><td> 2.300406</td></tr>
	<tr><td>1845</td><td>3241</td><td> 66</td><td>clinic 2</td><td> 2.036409</td></tr>
	<tr><td>1846</td><td>3754</td><td>105</td><td>clinic 2</td><td> 2.797017</td></tr>
</tbody>
</table>




```R
run_tests({
    test_that("A proportion_deaths column exists", {
        expect_true("proportion_deaths" %in% names(yearly), 
            info = 'yearly should have the new column proportion_deaths')
    })
    
    test_that("Read in data correctly.", {
        yearly_temp <- read_csv('datasets/yearly_deaths_by_clinic.csv') %>% 
          mutate(proportion_deaths = deaths / births)
        expect_equivalent(yearly, yearly_temp, 
            info = 'proportion_deaths should be calculated as deaths / births')
    })
})
```






    1/2 tests passed
    > fail :: Read in data correctly.
    `yearly` not equivalent to `yearly_temp`.
    Component "proportion_deaths": Mean relative difference: 0.99
    proportion_deaths should be calculated as deaths / births
    Reason:  
    ---


## 3. Death at the clinics
<p>If we now plot the proportion of deaths at both clinic 1 and clinic 2  we'll see a curious pattern…</p>


```R
# Setting the size of plots in this notebook
options(repr.plot.width=7, repr.plot.height=4)

# Plot yearly proportion of deaths at the two clinics
# .... YOUR CODE FOR TASK 3 ....

ggplot(yearly, aes(x = year, y = proportion_deaths, color = clinic)) + 
geom_line()
```


![png](output_7_0.png)



```R
run_tests({
    test_that("The right columns are plotted", {
        mappings <- str_replace(as.character(last_plot()$mapping), "~", "")
        expect_true(all(c("year", "proportion_deaths", "clinic") %in% mappings),
                    info = "year should be on the x-axis, proportion_deaths should be on the y-axis, and clinic should be mapped to color.")
    })
})

```






    1/1 tests passed


## 4. The handwashing begins
<p>Why is the proportion of deaths constantly so much higher in Clinic 1? Semmelweis saw the same pattern and was puzzled and distressed. The only difference between the clinics was that many medical students served at Clinic 1, while mostly midwife students served at Clinic 2. While the midwives only tended to the women giving birth, the medical students also spent time in the autopsy rooms examining corpses. </p>
<p>Semmelweis started to suspect that something on the corpses, spread from the hands of the medical students, caused childbed fever. So in a desperate attempt to stop the high mortality rates, he decreed: <em>Wash your hands!</em> This was an unorthodox and controversial request, nobody in Vienna knew about bacteria at this point in time. </p>
<p>Let's load in monthly data from Clinic 1 to see if the handwashing had any effect.</p>


```R
# Read datasets/monthly_deaths.csv into monthly
monthly <- read_csv("datasets/monthly_deaths.csv")

# Adding a new column with proportion of deaths per no. births
# .... YOUR CODE FOR TASK 4 ....

monthly <- monthly %>%
mutate(proportion_deaths = deaths / births * 100)

# Print out the first rows in monthly
# .... YOUR CODE FOR TASK 4 ....

head(monthly)
```

    Parsed with column specification:
    cols(
      date = [34mcol_date(format = "")[39m,
      births = [32mcol_double()[39m,
      deaths = [32mcol_double()[39m
    )



<table>
<caption>A tibble: 6 x 4</caption>
<thead>
	<tr><th scope=col>date</th><th scope=col>births</th><th scope=col>deaths</th><th scope=col>proportion_deaths</th></tr>
	<tr><th scope=col>&lt;date&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1841-01-01</td><td>254</td><td>37</td><td>14.5669291</td></tr>
	<tr><td>1841-02-01</td><td>239</td><td>18</td><td> 7.5313808</td></tr>
	<tr><td>1841-03-01</td><td>277</td><td>12</td><td> 4.3321300</td></tr>
	<tr><td>1841-04-01</td><td>255</td><td> 4</td><td> 1.5686275</td></tr>
	<tr><td>1841-05-01</td><td>255</td><td> 2</td><td> 0.7843137</td></tr>
	<tr><td>1841-06-01</td><td>200</td><td>10</td><td> 5.0000000</td></tr>
</tbody>
</table>




```R
run_tests({
    
   test_that("Read in data correctly.", {
        expect_is(monthly, "tbl_df", 
            info = 'You should use read_csv (with an underscore) to read "datasets/monthly_deaths.csv" into monthly.')
    })
    
    test_that("Read in monthly correctly.", {
        monthly_temp <- read_csv("datasets/monthly_deaths.csv")
        expect_true(all(names(monthly_temp) %in% names(monthly)), 
            info = 'monthly should contain the data in "datasets/monthly_deaths.csv"')
    })
    
    test_that("proportion_death is calculated correctly.", {
        monthly_temp <- read_csv("datasets/monthly_deaths.csv")
        monthly_temp <- monthly_temp %>% 
          mutate(proportion_deaths = deaths / births)
        expect_equivalent(monthly, monthly_temp, 
            info = 'proportion_deaths should be calculated as deaths / births')
    })
})
```






    2/3 tests passed
    > fail :: proportion_death is calculated correctly.
    `monthly` not equivalent to `monthly_temp`.
    Component "proportion_deaths": Mean relative difference: 0.99
    proportion_deaths should be calculated as deaths / births
    Reason:  
    ---


## 5. The effect of handwashing
<p>With the data loaded we can now look at the proportion of deaths over time. In the plot below we haven't marked where obligatory handwashing started, but it reduced the proportion of deaths to such a degree that you should be able to spot it!</p>


```R
# Plot monthly proportion of deaths
# ... YOUR CODE FOR TASK 5 ...

ggplot(monthly, aes(x = date, y = proportion_deaths)) + 
geom_line() + 
labs(x = "Month", "Proportion of deaths per number of births")
```


![png](output_13_0.png)



```R
run_tests({
    test_that("The right columns are plotted", {        
        mappings <- str_replace(as.character(last_plot()$mapping), "~", "")
        expect_true(all(c("date", "proportion_deaths") %in% mappings), 
            info = "date should be on the x-axis, proportion_deaths on the y-axis")
    })
})
```






    1/1 tests passed


## 6. The effect of handwashing highlighted
<p>Starting from the summer of 1847 the proportion of deaths is drastically reduced and, yes, this was when Semmelweis made handwashing obligatory. </p>
<p>The effect of handwashing is made even more clear if we highlight this in the graph.</p>


```R
# From this date handwashing was made mandatory
handwashing_start = as.Date('1847-06-01')

# Add a TRUE/FALSE column to monthly called handwashing_started
# .... YOUR CODE FOR TASK 6 ....

monthly <- monthly %>%
mutate(handwashing_started = date >= handwashing_start)

# Plot monthly proportion of deaths before and after handwashing
# .... YOUR CODE FOR TASK 6 ....

ggplot(monthly, aes(x = date, y = proportion_deaths, color = handwashing_started)) + 
geom_line() + 
labs(x = "Month", y = "Proportion of deaths per number of births", color = "Handwashing Started")
```


![png](output_16_0.png)



```R
run_tests({
    test_that("handwashing_started has been defined", {
        expect_true("handwashing_started" %in% names(monthly),
            info = 'monthly should contain the column handwashing_started.')
    })  
    
    test_that("there are 22 rows where handwashing_started is TRUE", {
        expect_equal(22, sum(monthly$handwashing_started),
            info = 'handwashing_started should be a TRUE/FALSE column where the rows where handwashing was enforced are set to TRUE.')
    })
    
    test_that("The right columns are plotted", {        
        mappings <- str_replace(as.character(last_plot()$mapping), "~", "")
        expect_true(all(c("date", "proportion_deaths", "handwashing_started") %in% mappings), 
            info = 'date should be on the x-axis, proportion_deaths on the y-axis, and handwashing_started should be mapped to color.')
    })
})
```






    3/3 tests passed


## 7. More handwashing, fewer deaths?
<p>Again, the graph shows that handwashing had a huge effect. How much did it reduce the monthly proportion of deaths on average?</p>


```R
# Calculating the mean proportion of deaths 
# before and after handwashing.

monthly_summary <- monthly %>%
# .... YOUR CODE FOR TASK 7 HERE ....
group_by(handwashing_started) %>%
summarize(mean_proportion_deaths = mean(proportion_deaths),
         .groups = "keep"
         )

# Printing out the summary.
monthly_summary
```


<table>
<caption>A grouped_df: 2 x 2</caption>
<thead>
	<tr><th scope=col>handwashing_started</th><th scope=col>mean_proportion_deaths</th></tr>
	<tr><th scope=col>&lt;lgl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>FALSE</td><td>10.504998</td></tr>
	<tr><td> TRUE</td><td> 2.109338</td></tr>
</tbody>
</table>




```R
run_tests({
    test_that("mean_proportion_deaths was calculated correctly", {
        flat_summary <- as.numeric(unlist(monthly_summary))
        handwashing_start = as.Date('1847-06-01')
        monthly_temp <- read_csv("datasets/monthly_deaths.csv") %>% 
          mutate(proportion_deaths = deaths / births) %>% 
          mutate(handwashing_started = date >= handwashing_start) %>% 
          group_by(handwashing_started) %>%
          summarise(mean_proportion_deaths = mean(proportion_deaths))
        expect_true(all(monthly_temp$mean_proportion_deaths %in% flat_summary),
            info = 'monthly_summary should contain the mean monthly proportion of deaths before and after handwashing was enforced.')
    })  
})
```






    0/1 tests passed
    > fail :: mean_proportion_deaths was calculated correctly
    all(monthly_temp$mean_proportion_deaths %in% flat_summary) isn't true.
    monthly_summary should contain the mean monthly proportion of deaths before and after handwashing was enforced.
    Reason:  
    ---


## 8. A statistical analysis of Semmelweis handwashing data
<p>It reduced the proportion of deaths by around 8 percentage points! From 10% on average before handwashing to just 2% when handwashing was enforced (which is still a high number by modern standards). 
To get a feeling for the uncertainty around how much handwashing reduces mortalities we could look at a confidence interval (here calculated using a t-test).</p>


```R
# Calculating a 95% Confidence intrerval using t.test 
test_result <- t.test( proportion_deaths ~ handwashing_started, data = monthly)
test_result
```


    
    	Welch Two Sample t-test
    
    data:  proportion_deaths by handwashing_started
    t = 9.6101, df = 92.435, p-value = 1.445e-15
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
      6.660662 10.130659
    sample estimates:
    mean in group FALSE  mean in group TRUE 
              10.504998            2.109338 




```R
run_tests({
    test_that("the confidence intervals match", {
        temp_test_result <- t.test( proportion_deaths ~ handwashing_started, data = monthly)
        expect_equivalent(test_result$conf.int, temp_test_result$conf.int,
            info = 'The t-test should be calculated with proportion_deaths as a function of handwashing_started.')
    })  
})
```






    1/1 tests passed


## 9. The fate of Dr. Semmelweis
<p>That the doctors didn't wash their hands increased the proportion of deaths by between 6.7 and 10 percentage points, according to a 95% confidence interval. All in all, it would seem that Semmelweis had solid evidence that handwashing was a simple but highly effective procedure that could save many lives.</p>
<p>The tragedy is that, despite the evidence, Semmelweis' theory — that childbed fever was caused by some "substance" (what we today know as <em>bacteria</em>) from autopsy room corpses — was ridiculed by contemporary scientists. The medical community largely rejected his discovery and in 1849 he was forced to leave the Vienna General Hospital for good.</p>
<p>One reason for this was that statistics and statistical arguments were uncommon in medical science in the 1800s. Semmelweis only published his data as long tables of raw data, but he didn't show any graphs nor confidence intervals. If he would have had access to the analysis we've just put together he might have been more successful in getting the Viennese doctors to wash their hands.</p>


```R
# The data Semmelweis collected points to that:
doctors_should_wash_their_hands <- TRUE
```


```R
run_tests({
    test_that("The project is finished.", {
        expect_true(doctors_should_wash_their_hands, 
            info = "Semmelweis would argue that doctors_should_wash_their_hands should be TRUE .")
    })  
})
```






    1/1 tests passed

