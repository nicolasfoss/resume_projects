
## 1. Data on tags over time
<p>How can we tell what programming languages and technologies are used by the most people? How about what languages are growing and which are shrinking, so that we can tell which are most worth investing time in?</p>
<p>One excellent source of data is <a href="https://stackoverflow.com/">Stack Overflow</a>, a programming question and answer site with more than 16 million questions on programming topics. By measuring the number of questions about each technology, we can get an approximate sense of how many people are using it. We're going to use open data from the <a href="https://data.stackexchange.com/">Stack Exchange Data Explorer</a> to examine the relative popularity of languages like R, Python, Java and Javascript have changed over time.</p>
<p>Each Stack Overflow question has a <strong>tag</strong>, which marks a question to describe its topic or technology. For instance, there's a tag for languages like <a href="https://stackoverflow.com/tags/r">R</a> or <a href="https://stackoverflow.com/tags/python">Python</a>, and for packages like <a href="https://stackoverflow.com/questions/tagged/ggplot2">ggplot2</a> or <a href="https://stackoverflow.com/questions/tagged/pandas">pandas</a>.</p>
<p><img src="https://assets.datacamp.com/production/project_435/img/tags.png" alt="Stack Overflow tags"></p>
<p>We'll be working with a dataset with one observation for each tag in each year. The dataset includes both the number of questions asked in that tag in that year, and the total number of questions asked in that year.</p>


```R
# Load libraries
library(readr)
library(dplyr)

# Load dataset
by_tag_year <- read_csv("datasets/by_tag_year.csv")

# Inspect the dataset
str(by_tag_year)
```

    Parsed with column specification:
    cols(
      year = [32mcol_double()[39m,
      tag = [31mcol_character()[39m,
      number = [32mcol_double()[39m,
      year_total = [32mcol_double()[39m
    )


    tibble [40,518 x 4] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
     $ year      : num [1:40518] 2008 2008 2008 2008 2008 ...
     $ tag       : chr [1:40518] ".htaccess" ".net" ".net-2.0" ".net-3.5" ...
     $ number    : num [1:40518] 54 5910 289 319 6 3 1 42 19 4 ...
     $ year_total: num [1:40518] 58390 58390 58390 58390 58390 ...
     - attr(*, "spec")=
      .. cols(
      ..   year = [32mcol_double()[39m,
      ..   tag = [31mcol_character()[39m,
      ..   number = [32mcol_double()[39m,
      ..   year_total = [32mcol_double()[39m
      .. )



```R
# These packages need to be loaded in the first `@tests` cell. 
library(testthat) 
library(IRkernel.testthat)

# Then follows one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("the answer is correct", {
    expect_true("readr" %in% .packages(), info = "Did you load the readr package?")
    expect_true("dplyr" %in% .packages(), info = "Did you load the dplyr package?")
    expect_is(by_tag_year, "tbl_df", 
        info = "Did you read in by_tag_year with read_csv (not read.csv)?")
    expect_equal(nrow(by_tag_year), 40518, 
        info = "Did you read in by_tag_year with read_csv?")
    })
})

```






    1/1 tests passed


## 2. Now in fraction format
<p>This data has one observation for each pair of a tag and a year, showing the number of questions asked in that tag in that year and the total number of questions asked in that year. For instance, there were 54 questions asked about the <code>.htaccess</code> tag in 2008, out of a total of 58390 questions in that year.</p>
<p>Rather than just the counts, we're probably interested in a percentage: the fraction of questions that year that have that tag. So let's add that to the table.</p>


```R
# Add fraction column
by_tag_year_fraction <- by_tag_year  %>%
mutate(
fraction = number / year_total
)

# Print the new table
print(by_tag_year_fraction)
```

    [38;5;246m# A tibble: 40,518 x 5[39m
        year tag           number year_total  fraction
       [3m[38;5;246m<dbl>[39m[23m [3m[38;5;246m<chr>[39m[23m          [3m[38;5;246m<dbl>[39m[23m      [3m[38;5;246m<dbl>[39m[23m     [3m[38;5;246m<dbl>[39m[23m
    [38;5;250m 1[39m  [4m2[24m008 .htaccess         54      [4m5[24m[4m8[24m390 0.000[4m9[24m[4m2[24m[4m5[24m 
    [38;5;250m 2[39m  [4m2[24m008 .net            [4m5[24m910      [4m5[24m[4m8[24m390 0.101    
    [38;5;250m 3[39m  [4m2[24m008 .net-2.0         289      [4m5[24m[4m8[24m390 0.004[4m9[24m[4m5[24m  
    [38;5;250m 4[39m  [4m2[24m008 .net-3.5         319      [4m5[24m[4m8[24m390 0.005[4m4[24m[4m6[24m  
    [38;5;250m 5[39m  [4m2[24m008 .net-4.0           6      [4m5[24m[4m8[24m390 0.000[4m1[24m[4m0[24m[4m3[24m 
    [38;5;250m 6[39m  [4m2[24m008 .net-assembly      3      [4m5[24m[4m8[24m390 0.000[4m0[24m[4m5[24m[4m1[24m4
    [38;5;250m 7[39m  [4m2[24m008 .net-core          1      [4m5[24m[4m8[24m390 0.000[4m0[24m[4m1[24m[4m7[24m1
    [38;5;250m 8[39m  [4m2[24m008 2d                42      [4m5[24m[4m8[24m390 0.000[4m7[24m[4m1[24m[4m9[24m 
    [38;5;250m 9[39m  [4m2[24m008 32-bit            19      [4m5[24m[4m8[24m390 0.000[4m3[24m[4m2[24m[4m5[24m 
    [38;5;250m10[39m  [4m2[24m008 32bit-64bit        4      [4m5[24m[4m8[24m390 0.000[4m0[24m[4m6[24m[4m8[24m5
    [38;5;246m# ... with 40,508 more rows[39m



```R
# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("the answer is correct", {
    expect_is(by_tag_year_fraction, "tbl_df", 
        info = "Did you create the by_tag_year_fraction object?")
    expect_true("fraction" %in% colnames(by_tag_year_fraction), 
        info = "Did you use mutate() to add a fraction column?")
    expect_equal(by_tag_year_fraction$fraction,
                 by_tag_year_fraction$number / by_tag_year_fraction$year_total,
        info = "Check how you computed the fraction column: is it the number divided by that year's total?")
    })
    # You can have more than one test
})
```






    1/1 tests passed


## 3. Has R been growing or shrinking?
<p>So far we've been learning and using the R programming language. Wouldn't we like to be sure it's a good investment for the future? Has it been keeping pace with other languages, or have people been switching out of it?</p>
<p>Let's look at whether the fraction of Stack Overflow questions that are about R has been increasing or decreasing over time.</p>


```R
# Filter for R tags
r_over_time <- by_tag_year_fraction %>% 
filter(tag == "r")

# Print the new table
r_over_time
```


<table>
<caption>A spec_tbl_df: 11 x 5</caption>
<thead>
	<tr><th scope=col>year</th><th scope=col>tag</th><th scope=col>number</th><th scope=col>year_total</th><th scope=col>fraction</th></tr>
	<tr><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>2008</td><td>r</td><td>    8</td><td>  58390</td><td>0.0001370098</td></tr>
	<tr><td>2009</td><td>r</td><td>  524</td><td> 343868</td><td>0.0015238405</td></tr>
	<tr><td>2010</td><td>r</td><td> 2270</td><td> 694391</td><td>0.0032690516</td></tr>
	<tr><td>2011</td><td>r</td><td> 5845</td><td>1200551</td><td>0.0048685978</td></tr>
	<tr><td>2012</td><td>r</td><td>12221</td><td>1645404</td><td>0.0074273552</td></tr>
	<tr><td>2013</td><td>r</td><td>22329</td><td>2060473</td><td>0.0108368321</td></tr>
	<tr><td>2014</td><td>r</td><td>31011</td><td>2164701</td><td>0.0143257660</td></tr>
	<tr><td>2015</td><td>r</td><td>40844</td><td>2219527</td><td>0.0184021190</td></tr>
	<tr><td>2016</td><td>r</td><td>44611</td><td>2226072</td><td>0.0200402323</td></tr>
	<tr><td>2017</td><td>r</td><td>54415</td><td>2305207</td><td>0.0236052554</td></tr>
	<tr><td>2018</td><td>r</td><td>28938</td><td>1085170</td><td>0.0266667895</td></tr>
</tbody>
</table>




```R
# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("the answer is correct", {
    expect_is(r_over_time, "tbl_df",
        info = "Did you create an r_over_time object with filter()?")
    expect_equal(nrow(r_over_time), 11,
        info = "Did you filter just for the rows with the 'r' tag?")
    expect_true(all(r_over_time$tag == "r"),
        info = "Did you filter just for the rows with the 'r' tag?")
    })
    # You can have more than one test
})
```






    1/1 tests passed


## 4. Visualizing change over time
<p>Rather than looking at the results in a table, we often want to create a visualization. Change over time is usually visualized with a line plot.</p>


```R
# Load ggplot2
library(ggplot2)

# Create a line plot of fraction over time
ggplot(r_over_time, aes(x = year, y = fraction)) + 
geom_line()
```


![png](output_10_0.png)



```R
# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.

get_aesthetics <- function(p) {
    unlist(c(list(p$mapping), purrr::map(p$layers, "mapping")))
}

run_tests({
    test_that("the answer is correct", {
        expect_true("ggplot2" %in% .packages(), info = "Did you load the ggplot2 package?")
        # expect_true("scales" %in% .packages(), info = "Did you load the scales package?")

        p <- last_plot()
        expect_is(p, "ggplot", info = "Did you create a ggplot figure?")
        expect_equal(length(p$layers), 1, info = "Did you create a plot with geom_line()?")
        expect_is(p$layers[[1]]$geom, "GeomLine", info = "Did you create a plot with geom_line()?")

        aesthetics <- get_aesthetics(p)
        expect_equal(rlang::quo_name(aesthetics$x), "year",
                     info = "Did you put year on the x-axis?")
        expect_equal(rlang::quo_name(aesthetics$y), "fraction",
                     info = "Did you put fraction on the y-axis?")
        
        # expect_equal(length(p$scales$scales), 1, info = "Did you add scale_y_continuous?")    
        # expect_equal(p$scales$scales[[1]]$labels(.03), "3.00%", info = "Did you make the y-axis a percentage?")
    })
})
```






    1/1 tests passed


## 5. How about dplyr and ggplot2?
<p>Based on that graph, it looks like R has been growing pretty fast in the last decade. Good thing we're practicing it now!</p>
<p>Besides R, two other interesting tags are dplyr and ggplot2, which we've already used in this analysis. They both also have Stack Overflow tags!</p>
<p>Instead of just looking at R, let's look at all three tags and their change over time. Are each of those tags increasing as a fraction of overall questions? Are any of them decreasing?</p>


```R
# A vector of selected tags
selected_tags <- c("r", "dplyr", "ggplot2")

# Filter for those tags
selected_tags_over_time <- by_tag_year_fraction %>% 
filter(tag %in% selected_tags)

# Plot tags over time on a line plot using color to represent tag
ggplot(selected_tags_over_time, aes(x = year, y = fraction, color = tag, group = tag)) + 
geom_line()
```


![png](output_13_0.png)



```R
# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.

get_aesthetics <- function(p) {
    unlist(c(list(p$mapping), purrr::map(p$layers, "mapping")))
}

run_tests({
    test_that("the answer is correct", {
        expect_true("ggplot2" %in% .packages(), info = "Did you load the ggplot2 package?")
        
        expect_is(selected_tags_over_time, "tbl_df",
                 info = "Did you create a selected_tags_over_time data frame?")

        expect_equal(nrow(selected_tags_over_time), 28,
                 info = "Did you filter for r, dplyr, and ggplot2 and save it to selected_tags_over_time?")

        expect_equal(sort(unique(selected_tags_over_time$tag)), c("dplyr", "ggplot2", "r"),
                 info = "Did you filter for r, dplyr, and ggplot2 and save it to selected_tags_over_time?")

        p <- last_plot()
        aesthetics <- get_aesthetics(p)
        expect_is(p, "ggplot", info = "Did you create a ggplot figure?")
        expect_equal(p$data, selected_tags_over_time, info = "Did you create your plot out of selected_tags_over_time?")
        
        expect_equal(length(p$layers), 1, info = "Did you create a plot with geom_line()?")
        expect_is(p$layers[[1]]$geom, "GeomLine", info = "Did you create a plot with geom_line()?")

        expect_true(!is.null(aesthetics$x), info = "Did you put year on the x-axis?")
        expect_equal(rlang::quo_name(aesthetics$x), "year",
                     info = "Did you put year on the x-axis?")

        expect_true(!is.null(aesthetics$y), info = "Did you put fraction on the y-axis?")
        expect_equal(rlang::quo_name(aesthetics$y), "fraction",
                     info = "Did you put fraction on the y-axis?")

        expect_true(!is.null(aesthetics$colour), info = "Did you put color on the x-axis?")
        expect_equal(rlang::quo_name(aesthetics$colour), "tag",
                     info = "Did you map the tag to the color?")

        # expect_equal(length(p$scales$scales), 1, info = "Did you add scale_y_continuous?")    
        # expect_equal(p$scales$scales[[1]]$labels(.03), "3.00%", info = "Did you make the y-axis a percentage?")
    })
    # You can have more than one test
})
```






    1/1 tests passed


## 6. What are the most asked-about tags?
<p>It's sure been fun to visualize and compare tags over time. The dplyr and ggplot2 tags may not have as many questions as R, but we can tell they're both growing quickly as well.</p>
<p>We might like to know which tags have the most questions <em>overall</em>, not just within a particular year. Right now, we have several rows for every tag, but we'll be combining them into one. That means we want <code>group_by()</code> and <code>summarize()</code>.</p>
<p>Let's look at tags that have the most questions in history.</p>


```R
# Find total number of questions for each tag
sorted_tags <- by_tag_year %>%
group_by(tag) %>% 
summarize(tag_total = n()) %>% 
arrange(desc(tag_total))

# Print the new table
sorted_tags
```


<table>
<caption>A tibble: 4080 x 2</caption>
<thead>
	<tr><th scope=col>tag</th><th scope=col>tag_total</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>.htaccess           </td><td>11</td></tr>
	<tr><td>.net                </td><td>11</td></tr>
	<tr><td>.net-2.0            </td><td>11</td></tr>
	<tr><td>.net-3.5            </td><td>11</td></tr>
	<tr><td>.net-4.0            </td><td>11</td></tr>
	<tr><td>.net-assembly       </td><td>11</td></tr>
	<tr><td>2d                  </td><td>11</td></tr>
	<tr><td>32-bit              </td><td>11</td></tr>
	<tr><td>32bit-64bit         </td><td>11</td></tr>
	<tr><td>3d                  </td><td>11</td></tr>
	<tr><td>64bit               </td><td>11</td></tr>
	<tr><td>abap                </td><td>11</td></tr>
	<tr><td>absolute            </td><td>11</td></tr>
	<tr><td>abstract            </td><td>11</td></tr>
	<tr><td>abstract-class      </td><td>11</td></tr>
	<tr><td>abstract-syntax-tree</td><td>11</td></tr>
	<tr><td>accelerometer       </td><td>11</td></tr>
	<tr><td>access-control      </td><td>11</td></tr>
	<tr><td>access-vba          </td><td>11</td></tr>
	<tr><td>access-violation    </td><td>11</td></tr>
	<tr><td>accessibility       </td><td>11</td></tr>
	<tr><td>accordion           </td><td>11</td></tr>
	<tr><td>acl                 </td><td>11</td></tr>
	<tr><td>acrobat             </td><td>11</td></tr>
	<tr><td>action              </td><td>11</td></tr>
	<tr><td>actionlistener      </td><td>11</td></tr>
	<tr><td>actionmailer        </td><td>11</td></tr>
	<tr><td>actionscript        </td><td>11</td></tr>
	<tr><td>actionscript-2      </td><td>11</td></tr>
	<tr><td>actionscript-3      </td><td>11</td></tr>
	<tr><td>...</td><td>...</td></tr>
	<tr><td>botframework          </td><td>3</td></tr>
	<tr><td>create-react-app      </td><td>3</td></tr>
	<tr><td>discord               </td><td>3</td></tr>
	<tr><td>enzyme                </td><td>3</td></tr>
	<tr><td>expo                  </td><td>3</td></tr>
	<tr><td>firebase-storage      </td><td>3</td></tr>
	<tr><td>google-cloud-functions</td><td>3</td></tr>
	<tr><td>hyperledger           </td><td>3</td></tr>
	<tr><td>hyperledger-fabric    </td><td>3</td></tr>
	<tr><td>identityserver4       </td><td>3</td></tr>
	<tr><td>ionic                 </td><td>3</td></tr>
	<tr><td>ionic3                </td><td>3</td></tr>
	<tr><td>odoo-10               </td><td>3</td></tr>
	<tr><td>primeng               </td><td>3</td></tr>
	<tr><td>python-3.6            </td><td>3</td></tr>
	<tr><td>raspberry-pi3         </td><td>3</td></tr>
	<tr><td>react-native-ios      </td><td>3</td></tr>
	<tr><td>react-router-v4       </td><td>3</td></tr>
	<tr><td>typescript2.0         </td><td>3</td></tr>
	<tr><td>vue-component         </td><td>3</td></tr>
	<tr><td>vue-router            </td><td>3</td></tr>
	<tr><td>vuejs2                </td><td>3</td></tr>
	<tr><td>vuex                  </td><td>3</td></tr>
	<tr><td>webpack-2             </td><td>3</td></tr>
	<tr><td>android-room          </td><td>2</td></tr>
	<tr><td>android-studio-3.0    </td><td>2</td></tr>
	<tr><td>dialogflow            </td><td>2</td></tr>
	<tr><td>google-cloud-firestore</td><td>2</td></tr>
	<tr><td>hyperledger-composer  </td><td>2</td></tr>
	<tr><td>react-navigation      </td><td>2</td></tr>
</tbody>
</table>




```R
# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("the answer is correct", {
        expect_is(sorted_tags, "tbl_df",
                 info = "Did you create a selected_tags_over_time data frame?")

        expect_equal(colnames(sorted_tags), c("tag", "tag_total"),
                 info = "Did you group by tag and summarize to create a tag_total column?")

        expect_equal(nrow(sorted_tags), length(unique(by_tag_year$tag)),
                 info = "Did you group by tag and summarize to create a tag_total column?")

        expect_equal(sorted_tags$tag_total,
                     sort(sorted_tags$tag_total, decreasing = TRUE),
                     info = "Did you arrange in descending order of tag_total?")
    })
})
```






    1/1 tests passed


## 7. How have large programming languages changed over time?
<p>We've looked at selected tags like R, ggplot2, and dplyr, and seen that they're each growing. What tags might be <em>shrinking</em>? A good place to start is to plot the tags that we just saw that were the most-asked about of all time, including JavaScript, Java and C#.</p>


```R
# Get the six largest tags
highest_tags <- head(sorted_tags$tag)

# Filter for the six largest tags
by_tag_subset <- by_tag_year_fraction %>% 
filter(tag %in% highest_tags)

# Plot tags over time on a line plot using color to represent tag
ggplot(by_tag_subset,
aes(x = year, y = fraction, color = tag)
) + 
geom_line()
```


![png](output_19_0.png)



```R
# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
get_aesthetics <- function(p) {
    unlist(c(list(p$mapping), purrr::map(p$layers, "mapping")))
}

run_tests({
    test_that("the answer is correct", {
        expect_equal(sort(unique(by_tag_subset$tag)), sort(head(sorted_tags$tag, 6)),
                   info = "Did you filter by_tag_year_fraction for only the 6 most asked-about tags, and save it as by_tag_subset?")

        expect_equal(colnames(by_tag_subset), colnames(by_tag_year_fraction),
                   info = "Did you filter by_tag_year_fraction for only the 6 most asked-about tags, and save it as by_tag_subset?")

        p <- last_plot()
        expect_is(p, "ggplot", info = "Did you create a ggplot figure?")
        expect_equal(p$data, by_tag_subset, info = "Did you create your plot out of by_tag_subset?")
        
        expect_equal(length(p$layers), 1, info = "Did you create a plot with geom_line()?")
        expect_is(p$layers[[1]]$geom, "GeomLine", info = "Did you create a plot with geom_line()?")

        aesthetics <- get_aesthetics(p)
        expect_equal(rlang::quo_name(aesthetics$x), "year",
                     info = "Did you put year on the x-axis?")
        expect_equal(rlang::quo_name(aesthetics$y), "fraction",
                     info = "Did you put fraction on the y-axis?")
        expect_equal(rlang::quo_name(aesthetics$colour), "tag",
                     info = "Did you map the tag to the color?")

        # expect_equal(length(p$scales$scales), 1, info = "Did you add scale_y_continuous?")    
        # expect_equal(p$scales$scales[[1]]$labels(.03), "3.00%", info = "Did you make the y-axis a percentage?")
    })
})
```






    1/1 tests passed


## 8. Some more tags!
<p>Wow, based on that graph we've seen a lot of changes in what programming languages are most asked about. C# gets fewer questions than it used to, and Python has grown quite impressively.</p>
<p>This Stack Overflow data is incredibly versatile. We can analyze <em>any</em> programming language, web framework, or tool where we'd like to see their change over time. Combined with the reproducibility of R and its libraries, we have ourselves a powerful method of uncovering insights about technology.</p>
<p>To demonstrate its versatility, let's check out how three big mobile operating systems (Android, iOS, and Windows Phone) have compared in popularity over time. But remember: this code can be modified simply by changing the tag names!</p>


```R
# Get tags of interest
my_tags <- c("android", "ios", "windows-phone")

# Filter for those tags
by_tag_subset <- by_tag_year_fraction %>% 
filter(tag %in% my_tags)

# Plot tags over time on a line plot using color to represent tag
ggplot(by_tag_subset,
      aes(x = year, y = fraction, color = tag)
      ) + 
geom_line()
```


![png](output_22_0.png)



```R
# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
get_aesthetics <- function(p) {
    unlist(c(list(p$mapping), purrr::map(p$layers, "mapping")))
}

run_tests({
    test_that("the answer is correct", {
        expect_equal(sort(my_tags), c("android", "ios", "windows-phone"),
                    info = "Did you create a vector my_tags of just android, ios, and windows-phone?")
        
        expect_equal(sort(unique(by_tag_subset$tag)), c("android", "ios", "windows-phone"),
                   info = "Did you filter by_tag_year_fraction for only ios, android, and windows-phone?")

        expect_equal(colnames(by_tag_subset), colnames(by_tag_year_fraction),
                   info = "Did you filter by_tag_year_fraction for only the three requested tags, and save it as by_tag_subset?")

        p <- last_plot()
        expect_is(p, "ggplot", info = "Did you create a ggplot figure?")
        expect_equal(p$data, by_tag_subset, info = "Did you create your plot out of by_tag_subset?")
        
        expect_equal(length(p$layers), 1, info = "Did you create a plot with geom_line()?")
        expect_is(p$layers[[1]]$geom, "GeomLine", info = "Did you create a plot with geom_line()?")

        aesthetics <- get_aesthetics(p)
        expect_equal(rlang::quo_name(aesthetics$x), "year",
                     info = "Did you put year on the x-axis?")
        expect_equal(rlang::quo_name(aesthetics$y), "fraction",
                     info = "Did you put fraction on the y-axis?")
        expect_equal(rlang::quo_name(aesthetics$colour), "tag",
                     info = "Did you map the tag to the color?")

        # expect_equal(length(p$scales$scales), 1, info = "Did you add scale_y_continuous?")    
        # expect_equal(p$scales$scales[[1]]$labels(.03), "3.00%", info = "Did you make the y-axis a percentage?")
    })
})
```






    1/1 tests passed

