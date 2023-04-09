
# This sets plot images to a nice size
options(repr.plot.width = 6, repr.plot.height = 6)

# Loading packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Loading data
life_expectancy <- read.csv("datasets/UNdata.csv")

# Taking a look at the first few rows
head(life_expectancy)

# These packages need to be loaded in the first `@tests` cell. 
library(testthat) 
library(IRkernel.testthat)

# Then follows one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.

run_tests({
  test_that("Test that life_expectancy exists", {
    expect_true(exists("life_expectancy"), 
                 info = "It seems that the data frame life_expectancy does not exist.")
  })

  test_that("Test that life_expectancy is loaded correctly", {
    expect_true(nrow(life_expectancy)==1571, 
                 info = "The data frame life_expectancy is not correctly loaded.")
  })

  test_that("Test that life_expectancy is loaded correctly", {
    expect_true(ncol(life_expectancy)==7, 
                 info = "The data frame life_expectancy is not correctly loaded.")
  })
})

# Subsetting and reshaping the life expectancy data
subdata <- life_expectancy  %>%
filter(Year == "2000-2005") %>% 
select(Country.or.Area, Subgroup, Value) %>%  
spread(Subgroup, Value)


# Taking a look at the first few rows
head(subdata)
str(subdata)

# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
  test_that("Test that subdata exists", {
    expect_true(exists("subdata"), 
                 info = "It seems that dataset subdata does not exist.")
  })

  test_that("Test that subdata is created correctly", {
    expect_true(nrow(subdata)==195, 
                 info = "It seems that subdata is not correctly created.")
  })

  test_that("Test that subdata is created correctly", {
    expect_true(ncol(subdata)==3, 
                 info = "It seems that subdata is not correctly created.")
  })

  test_that("Test that subdata is contains correct columns", {
    expect_true(sum(is.element(c("Country.or.Area", "Female", "Male"), names(subdata)))==3, 
                 info = "It seems that subdata does not contain the correct columns.")
  })
})

ggplot(subdata, aes(Male, Female)) + 
geom_point()

run_tests({
  test_that("Check that a geom_point plot was plotted.", {
    expect_true( "GeomPoint" %in% class( last_plot()$layers[[1]]$geom ) , 
                 info = "Add geom_point() to produce a scatter plot.")
  })
  
  test_that("Check variables are correctly mapped.", {
    expect_true(deparse(last_plot()$mapping$x)=="~Male" & deparse(last_plot()$mapping$y) == "~Female",
                 info = "Check that the variables are mapped to the correct axes.")
  })
  
})

# Adding an abline and changing the scale of axes of the previous plots
ggplot(subdata, aes(Male, Female)) + 
geom_point() + 
geom_abline(intercept = 0,
           slope = 1,
           linetype = 2) + 
xlim(35,85) +
ylim(35,85)

for (i in 1:length(ggplot_build(last_plot())$data)) 
{
  if ("slope" %in% colnames(ggplot_build(last_plot())$data[[i]])) i1=i
}


run_tests({
    test_that("Intercept of diagonal line is equal to 0.", {
    expect_equal(ggplot_build(last_plot())$data[[i1]]$intercept, 0, 
        info = "Did you add the diagonal line correctly?")
    })
    test_that("Slope of diagonal line is equal to 1.", {
    expect_equal(ggplot_build(last_plot())$data[[i1]]$slope, 1, 
        info = "Did you add the diagonal line correctly?")
    })
    test_that("Limits of x-axis.", {
        expect_equal(length(setdiff(c(39, 79), ggplot_build(last_plot())$layout$panel_scales_x[[1]]$range$range)), 0, 
               info = "The limits of x-axis is not equal to [35, 85].")
})
    test_that("Limits of y-axis.", {
        expect_equal(length(setdiff(c(39, 85), ggplot_build(last_plot())$layout$panel_scales_y[[1]]$range$range)), 0, 
               info = "The limits of y-axis is not equal to [35, 85].")
    })

})

# Adding labels to previous plot
ggplot(subdata, aes(x=Male, y=Female))+
  geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55, size=5)+
  geom_abline(intercept = 0, slope = 1, linetype=2)+
  scale_x_continuous(limits=c(35,85))+
  scale_y_continuous(limits=c(35,85))+
  labs(title= "Life Expectancy at Birth by Country",
       subtitle="Years. Period: 2000-2005. Average.",
       caption= "Source: United Nations Statistics Division",
       x= "Males",
       y= "Females")

run_tests({
    test_that("Title is correct.", {
    expect_equal(toupper(gsub("[[:space:]]", "", last_plot()$labels$title)), "LIFEEXPECTANCYATBIRTHBYCOUNTRY", 
        info = "Did you add the title correctly?")
    })

        test_that("x-axis label is correct.", {
    expect_equal(toupper(gsub("[[:space:]]", "", last_plot()$labels$x)), "MALES", 
        info = "Did you set the x-axis label correctly?")
    })

    
        test_that("y-axis label is correct.", {
    expect_equal(toupper(gsub("[[:space:]]", "", last_plot()$labels$y)), "FEMALES", 
        info = "Did you set the y-axis label correctly?")
    })

          test_that("caption is correct.", {
    expect_equal(toupper(gsub("[[:space:]]", "", last_plot()$labels$caption)), "SOURCE:UNITEDNATIONSSTATISTICSDIVISION", 
        info = "Did you set the caption correctly?")
    })

    
    
})

# Subseting data to obtain countries of interest
top_male <- subdata %>% arrange(Male-Female) %>% head(3)
top_female <- subdata %>% arrange(Female-Male) %>% head(3)

# Adding text to the previous plot to label countries of interest
ggplot(subdata, aes(x=Male, y=Female, label = Country.or.Area))+
  geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55, size=5)+
  geom_abline(intercept = 0, slope = 1, linetype=2)+
  scale_x_continuous(limits=c(35,85))+
  scale_y_continuous(limits=c(35,85))+
  labs(title="Life Expectancy at Birth by Country",
       subtitle="Years. Period: 2000-2005. Average.",
       caption="Source: United Nations Statistics Division",
       x="Males",
       y="Females") +
geom_text(data = top_male,
         aes(x = Male, y = Female)
         ) + 
geom_text(data = top_female,
         aes(x = Male, y = Female, label = Country.or.Area)
         ) + 
theme_bw()

texts=c()
for (i in 1:length(last_plot()$layers)) texts=c(last_plot()$layers[[i]]$data$Country.or.Area %>% as.character, texts)


run_tests({
   
      test_that("Test that countries defined by top_female and top_male are correctly labeled.", {
    expect_true(length(setdiff(texts, c("Russian Federation", "Belarus", "Estonia", "Niger", "Afghanistan", "Maldives")))==0, 
                 info = "It seems that countries defined by top_female and top_male are not labeled correctly.")
  })
    
        test_that("Theme is theme_bw().", {
    expect_equal(last_plot()$theme$panel.background$fill, "white", 
        info = "It seems that your plot does not have theme_bw().")
    })


})

# Subsetting, mutating and reshaping the life expectancy data
subdata2 <- life_expectancy %>% 
  filter(Year %in% c("1985-1990", "2000-2005")) %>% 
  mutate(Sub_Year=paste(Subgroup, Year, sep="_")) %>% 
  mutate(Sub_Year=gsub("-", "_", Sub_Year)) %>% 
  select(-Subgroup, -Year) %>% 
spread(Sub_Year, Value) %>%
mutate(diff_Female = as.numeric(Female_2000_2005 - Female_1985_1990),
      diff_Male = as.numeric(Male_2000_2005 - Male_1985_1990)
      )

# Taking a look at the first few rows
head(subdata2)


run_tests({
  test_that("Test that subdata2 is created correctly.", {
    expect_true(nrow(subdata2)==195, 
                 info = "It seems that dataset subdata2 is not correctly created.")
  })

  test_that("Test that subdata2 is created correctly.", {
    expect_true(ncol(subdata2)==10, 
                 info = "It seems that dataset subdata2 is not correctly created.")
  })

  test_that("Test that subdata2 is created correctly.", {
    expect_true(length(setdiff(c('diff_Female', 'diff_Male'), names(subdata2)))==0, 
                 info = "It seems that subdata2 does not contain columns diff_Female or diff_Male.")
  })

    test_that("Test that subdata2 is created correctly.", {
    expect_true(sum(subdata2$diff_Female)==492, 
                 info = "It seems that the diff_Female column is not correctly created.")
  })  
    
  test_that("Test that subdata2 is created correctly.", {
    expect_true(sum(subdata2$diff_Male)==503, 
                 info = "It seems that the diff_Male column is not correctly created.")
  })
})

# Doing a nice first version of the plot with abline, scaling axis and adding labels
ggplot(subdata2, aes(x=diff_Male, y=diff_Female, label=Country.or.Area))+
  geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55, size=5)+
  geom_abline(intercept = 0, slope = 1, linetype=2)+
scale_x_continuous(-25, 25) + 
scale_y_continuous(-25, 25) +
  labs(title="Life Expectancy at Birth by Country in Years",
       subtitle="Difference between 1985-1990 and 2000-2005. Average.",
       caption="Source: United Nations Statistics Division",
       x="Males",
       y="Females")+
theme_bw()

run_tests({
    
#    test_that("Check that a geom_point plot was plotted.", {
#     expect_true( "GeomPoint" %in% class( last_plot()$layers[[1]]$geom ) , 
#                  info = "Add geom_point() to produce a scatter plot.")
#   })
  
  test_that("Check variables are correctly mapped.", {
    expect_true( deparse(last_plot()$mapping$x)=="~diff_Male" & deparse(last_plot()$mapping$y)=="~diff_Female", 
                 info = "Check that the variables are mapped to the correct axes.")
  })
  
    
#     test_that("Intercept of diagonal line is equal to 0.", {
#     expect_equal(ggplot_build(last_plot())$data[[2]]$intercept, 0, 
#         info = "Did you add the diagonal line correctly?")
#     })
#     test_that("Slope of diagonal line is equal to 1.", {
#     expect_equal(ggplot_build(last_plot())$data[[2]]$slope, 1, 
#         info = "Did you add the diagonal line correctly?")
#     })
    test_that("Limits of x-axis", {
        expect_equal(length(setdiff(c(-20, 15), ggplot_build(last_plot())$layout$panel_scales_x[[1]]$range$range)), 0, 
               info = "Limits of x-axis is not equal to [-25, 25].")
    })
    test_that("Limits of y-axis", {
        expect_equal(length(setdiff(c(-24, 15), ggplot_build(last_plot())$layout$panel_scales_y[[1]]$range$range)), 0, 
               info = "Limits of y-axis is not equal to [-25, 25]")
    })
    
#     test_that("Intercept of diagonal line is equal to 0.", {
#     expect_equal(toupper(gsub("[[:space:]]", "", last_plot()$labels$title)), "LIFEEXPECTANCYATBIRTHBYCOUNTRYINYEARS", 
#         info = "Did you add the title correctly?")
#     })
#     test_that("Slope of diagonal line is equal to 1.", {
#     expect_equal(last_plot()$theme$panel.background$fill, "white", 
#         info = "It seems that your plot does not have theme_bw().")
#     })
})

# Adding an hline and vline to previous plots
ggplot(subdata2, aes(x=diff_Male, y=diff_Female, label=Country.or.Area))+
  geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55, size=5)+
  geom_abline(intercept = 0, slope = 1, linetype=2)+
  scale_x_continuous(limits=c(-25,25))+
  scale_y_continuous(limits=c(-25,25))+
geom_hline(yintercept = 0) + 
geom_vline(xintercept = 0) + 
  labs(title="Life Expectancy at Birth by Country",
       subtitle="Years. Difference between 1985-1990 and 2000-2005. Average.",
       caption="Source: United Nations Statistics Division",
       x="Males",
       y="Females")+
theme_bw()

for (i in 1:length(ggplot_build(last_plot())$data)) 
{
  if ("slope"      %in% colnames(ggplot_build(last_plot())$data[[i]])) i1=i
  if ("yintercept" %in% colnames(ggplot_build(last_plot())$data[[i]])) i2=i
  if ("xintercept" %in% colnames(ggplot_build(last_plot())$data[[i]])) i3=i
}
      

run_tests({
#     test_that("Intercept of diagonal line is equal to 0.", {
#     expect_equal(ggplot_build(last_plot())$data[[i1]]$intercept, 0, 
#         info = "Did you add the diagonal line correctly?")
#     })
#     test_that("Slope of diagonal line is equal to 1.", {
#     expect_equal(ggplot_build(last_plot())$data[[i1]]$slope, 1, 
#         info = "Did you add the diagonal line correctly?")
#     })

        test_that("Horizontal line is well defined.", {
    expect_equal(ggplot_build(last_plot())$data[[i2]]$yintercept, 0, 
        info = "Did you add the horizontal line correctly?")
    })
    test_that("Vertical line is well defined.", {
    expect_equal(ggplot_build(last_plot())$data[[i3]]$xintercept, 0, 
        info = "Did you add the vertical line correctly?")
    })
})

# Subseting data to obtain countries of interest
top <- subdata2 %>% arrange(desc(diff_Male+diff_Female)) %>% head(3)
bottom <- subdata2 %>% arrange(diff_Male+diff_Female) %>% head(3)

# Adding text to the previous plot to label countries of interest
ggplot(subdata2, aes(x=diff_Male, y=diff_Female, label=Country.or.Area), guide=FALSE)+
  geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55, size=5)+
  geom_abline(intercept = 0, slope = 1, linetype=2)+
  scale_x_continuous(limits=c(-25,25))+
  scale_y_continuous(limits=c(-25,25))+
  geom_hline(yintercept=0, linetype=2)+
  geom_vline(xintercept=0, linetype=2)+
  labs(title="Life Expectancy at Birth by Country",
       subtitle="Years. Difference between 1985-1990 and 2000-2005. Average.",
       caption="Source: United Nations Statistics Division",
       x="Males",
       y="Females")+
geom_text(data = top,
         aes(x=diff_Male, y=diff_Female)
         ) +
geom_text(data = bottom,
         aes(x=diff_Male, y=diff_Female) 
         ) +
theme_bw()

texts=c()
for (i in 1:length(last_plot()$layers)) texts=c(last_plot()$layers[[i]]$data$Country.or.Area %>% as.character, texts)

run_tests({
  test_that("Test that dataset bottom exists.", {
    expect_true(exists("bottom"), 
                 info = "It seems that bottom does not exist.")
  })

  test_that("Test that dataset bottom is correctly created.", {
    expect_true(nrow(bottom)==3, 
                 info = "It seems that bottom is not correctly created.")
  })

  test_that("Test that countries defined by top and bottom are correctly labeled.", {
    expect_true(length(setdiff(texts, c("Timor Leste", "Bhutan", "Egypt", "Zimbabwe", "Botswana", "Swaziland")))==0, 
                 info = "It seems that countries defined by top and bottom are not labeled correctly.")
  })


})
