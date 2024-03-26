Using RStudio and Shiny, I built an application with variety of widgets user interacts with to explore 7 different couple demographics. 
It is designed to create and display data visualizations presenting survey data for reasons behind title question, and varying extents of couple agreements on specific survey questions, visually optimized for nontechnical and/or young audiences. 


A few of the core concepts and insights into the data that I believe my app creates is that snoring seems to be the most important reason among all regions of the United States, and that no_share_cov, not wanting to share the covers with each other, was deemed least important across the board (bar plot). Additionally, it showed that most people agreed that sleeping apart helped them sleep, and that people roughly equally have no opinion/strongly disagree that sleeping apart helped their sex life (dot plot).

In the entire United States, it looks like many people in couples who are older (45+) and have been together for a long time (20+) marked snoring. Married couples also tended to dominate this specific data set. More men reported not being physically intimate as a reason. For those in couples that reported snoring, many seemed to sleep apart from each other everyday, while many of those in couples that reported one_is_sick (sickness) only sleep apart once a year or less. This may suggest that snoring is an everyday problem, while sickness is a very rare reason in the sense that it does not happen often, even if it is a relatively commonly reported reason.

For my widgets, I had one drop down menu, radio buttons, a check box, and conditional radio buttons. For my stacked bar plot, the drop down menu is for choosing the region of the United States and the radio buttons were for choosing the legend variable. The reason why I chose to let the user filter by U.S. region (All or separate areas) is so that they can compare regions or look at their own region (if they are from the US). The user was also able to choose the legend variable, which contained characteristics of the person in the couple to stack with. These characteristics included options like relationship length, status, age, etc. Together, these widgets allow the user to look at the relationship between multiple categorical variables and the reasons why these couples choose to sleep separately.

For my dot plot, I had a check box and a conditional radio button. If the check box was checked, it would hide the dot plot widget and the dot plot. I opted to do this because my dot graph has survey data about sex lives. I plan on publishing this app and putting it on my resume, and since it is a mature topic, I wanted the user to have the option to hide it. However, the default is left unchecked and showing the dot graph just so that I can display the ability to do so. The default would be to have it checked and hidden if released. The radio buttons are there for the user to pick which statement/opinion to display, whatever they are interested in seeing. I also chose not to filter by region because when I did, the data looked a little incomplete. So, I opted to have them just be from all regions. It helps the user look at the data they are interested in first and overall opinions of the data set.