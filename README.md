# Google Data Analytics Professional Certificate Capstone
By Deniz Firat  
July, 2022

# Introduction
In this case study, which is the final project in the [Google Data Analytics Professional Certificate](https://www.coursera.org/professional-certificates/google-data-analytics), I will perform many real-world tasks of a junior data analyst. 

# Scenario
I'm a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, my team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, my team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve my recommendations, so they must be backed up with compelling data insights and professional data visualizations.
The main question I am asked to answer is: How do annual members and casual riders use Cyclistic bikes differently? To answer this question, I will use Cyclistic's historical ride data from the last 12 months to analyze and identify trends. Based on my newfound insights, I am to make recommendations to the company on how to convert casual users into members.

# Process
For data cleaning and analysis, I used RStudio, an integrated development environment for R. The exact procedure can be viewed [here](https://github.com/dfirat/google-data-analytics/blob/main/Google-Data-Analytics-Capstone.md).
I used Tableau to visualize the results, which you can view [here](https://public.tableau.com/app/profile/dfirat/viz/GoogleDataAnalyticsCapstoneProjectCyclistic_16586019303490/RidesDash) and [here](https://public.tableau.com/app/profile/dfirat/viz/GoogleDataAnalyticsCapstoneProjectCyclisticII/LengthDash). Update: I [added](https://github.com/dfirat/google-data-analytics/blob/main/Google-Data-Analytics-Capstone.md) the same visualizations using R.

# Results
First, let's look at the number of rides. It can be seen that members make more rides overall than casual users. Looking at bike type use, the majority of members and occasional users use classic bikes. While casual users sometimes, though rarely, use docked bikes, this is not the case for members. The peak use time for bikes is in the late afternoon at 5 pm. Casual users tend to use bikes later in the day. In contrast, many rides are started by members in the early morning hours and high usage continues into the evening hours. A very big difference can be seen in the different days of the week. While members do most of their riding during the week, the exact opposite is the case for casual users and takes place mainly on weekends. Seasonal differences are small between members and casual users, with the majority of rides taken by both groups predominantly in the summer months.
[![RidesDashboard](RidesDash.png)](https://public.tableau.com/app/profile/dfirat/viz/GoogleDataAnalyticsCapstoneProjectCyclistic_16586019303490/RidesDash)

Let's also take a look at the average ride length. With 28 minutes, these are more than twice as long for occasional users as for members with 13 minutes. With 63 minutes, the longest average rides are made on the docked bikes, which are only used by casual users. In addition, casual users use both the classic and the electric bike significantly longer. During the day, the average ride length for members is very constant. For casual users, there is a slight decrease in the morning hours, but it is still longer compared to members and throughout the day. There are no major differences in the average ride length on the different days. For both groups, this is longest at the weekend. No major differences can be seen over the course of the year either. Both groups have the longest average ride lengths in the summer months.
[![LengthDashboard](LengthDash.png)](https://public.tableau.com/app/profile/dfirat/viz/GoogleDataAnalyticsCapstoneProjectCyclisticII/LengthDash)

# Conclusion
- Members make more rides, but their average ride length is shorter than that of casual users.
- Classic bikes are considerably preferred by both groups.
- Members start using bikes relatively early in the day.
- Casual users use bikes mostly on weekends, while members use it mostly during the week.
- Summer is the main season for both groups.

# Recommendations
1. Since bikes are used primarily in the summer, special offers for casual users could be increased during this time to convert them to members.
2. Discounts could be offered for morning and night hours to motivate casual users to ride more often.
3. Finally, discounts for electric bikes could be offered to increase their appeal, which in turn could make membership more attractive to some casual users.
