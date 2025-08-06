# Load necessary libraries
library(shiny)
library(presize)
library(statpsych)
library(pwr)
library(lrstat)

######################################################################################################
## Define UI
######################################################################################################

ui <- fluidPage(
  
  titlePanel("Sample Size Calculation for Pilot Trials"),
  p("This app calculates sample size for external pilot randomized controlled trials designed with different objectives. See the paper by Ying X, et al. (BMJ 2025;390:e083405. doi:10.1136/bmj-2024-083405) for more details. For any issues, contact xiangji.ying@unc.edu."),
  
  tabsetPanel(# Use tabsetPanel to create different sections (tabs)
 
    # Feasibility -------------------------------------------------------------------------------
    tabPanel(strong("Estimate a Feasibility Parameter"),
             fluidRow(
               # Left - Options
               column(6,
                      wellPanel(
                        h3("Instructions"),
                        p("This approach calculates sample size for pilot trials designed to estimate a feasibility parameter with a desired level of precision. The feasibility parameter can be:"),
                        tags$ul(
                          tags$li("Binary (i.e., proportion): e.g., proportion of eligible participants successfully randomized"),
                          tags$li("Count (i.e., Poisson rate): e.g., number of participants enrolled per month")
                        ),
                        p("To calculate the sample size, three values need to be specified:"),
                        tags$ol(
                          tags$li("Expected value of the parameter (e.g., 10% of participants will drop out of the trial)"),
                          tags$li("Desired half-width of the confidence interval (e.g., ±10%) around the parameter"),
                          tags$li("Confidence level (commonly 95%, one-sided or two-sided)")
                        ),
                        radioButtons("option", 
                                     "Select Parameter Type:",
                                     choices = c("Binary", "Count"),
                                     selected = "Binary",
                                     inline = TRUE)
                      ),
                      
                      # Examples based on selected option
                      wellPanel(
                        h3("Example"),
                        # Show Binary example
                        conditionalPanel(
                          condition = "input.option == 'Binary'",
                          p("To understand the extent to which participants might drop out of the definitive trial, researchers design a pilot trial to estimate the probability of dropout."),
                          tags$ol(
                            tags$li("They make an educated guess that dropout will be about 10%."),
                            tags$li("They aim to estimate this parameter with a precision of 10 percentage points (ie, confidence interval (CI) width is 20 percentage points)."),
                            tags$li("They plan to use a one sided 95% CI because they are mostly interested in the upper bound of the CI (ie, whether it exceeds 20%).")
                          ),
                          p("The researchers will need to enroll 26 participants (13 per group) for the pilot trial to have a 95% CI with a maximum width of 20% (0% to 20%).")
                        ),
                        # Show Count example
                        conditionalPanel(
                          condition = "input.option == 'Count'",
                          p("Researchers conduct a traditional sample size calculation for the definitive trial and find that they would need to enroll 180 participants over 18 months (ie, 10 enrollments per month). Because participant recruitment is crucial for trial success, they plan to conduct a pilot trial to estimate the recruitment rate."),
                          tags$ol(
                            tags$li("The recruitment rate needed for successful enrollment in the definitive trial is 10 participants per month."),
                            tags$li("They aim to estimate this parameter with a precision of 3 (ie, CI width is six participants per month)."),
                            tags$li("They plan to use a two sided 95% CI.")
                          ),
                          p("To estimate this recruitment rate with the desired level of precision, researchers calculate that 44 participants (22 per group) will need to be enrolled over 4.4 months during the pilot trial.")
                        )
                      )
               ),
               # Middle & Right - Input and Result Panel
               column(6,
                      wellPanel(
                        h3("Input"),
                        conditionalPanel(
                          condition = "input.option == 'Binary'",
                          numericInput("prop", "Expected proportion", value = 0.1),
                          helpText("Enter the expected value of the parameter to be estimated."),
                          numericInput("ci_width1", "Half-width of confidence interval", value = 0.1),
                          helpText("Enter the desired half-width of the confidence interval around the parameter."),
                          numericInput("ci_level1", "Confidence level", value = 0.95),
                          helpText("Enter the desired confidence level for the confidence interval, usually 95%."),
                          numericInput("side1", "Side of confidence interval", value = 1),
                          helpText("Enter 1 if one-sided or 2 if two-sided."),
                          actionButton("calc_button11", "Calculate")
                        ),
                        conditionalPanel(
                          condition = "input.option == 'Count'",
                          numericInput("rate", "Expected count", value = 10),
                          helpText("Enter the expected value of the parameter to be estimated."),
                          numericInput("ci_width2", "Half-width of confidence interval", value = 3),
                          helpText("Enter the desired half-width of the confidence interval around the parameter."),
                          numericInput("ci_level2", "Confidence level", value = 0.95),
                          helpText("Enter the desired confidence level for the confidence interval, usually 95%."),
                          numericInput("side2", "Side of confidence interval", value = 2),
                          helpText("Enter 1 if one-sided or 2 if two-sided."),
                          actionButton("calc_button12", "Calculate")
                        )
                      ),
                      wellPanel(
                        h3("Results"),
                        conditionalPanel(
                          condition = "input.option == 'Binary'",
                          textOutput("n_output11")
                        ),
                        conditionalPanel(
                          condition = "input.option == 'Count'",
                          textOutput("n_output12")
                        )
                      )
               )
             )),
    
    tabPanel(strong("Test Feasibility Progression Criteria"),
             fluidRow(
               # Top Left - Instructions
               column(6,
                      wellPanel(
                        h3("Instructions"),
                        p("This approach calculates sample size for pilot trials designed to test a progression criteria based on two thresholds for a feasibility parameter:"),
                        tags$ul(
                          tags$li("Goal threshold: If result exceeds this threshold, proceed to the definitive trial with the original design"),
                          tags$li("Minimum threshold: If below this threshold, do not proceed")
                        ),
                        radioButtons("option1", 
                                     "Select Parameter Type:",
                                     choices = c("Binary", "Count"),
                                     selected = "Binary",
                                     inline = TRUE)
                      ),
                      # Examples based on selected option
                      wellPanel(
                        h3("Example"),
                        # Show Binary example
                        conditionalPanel(
                          condition = "input.option1 == 'Binary'",
                          p("Researchers are uncertain about whether eligible participants will consent to participate. They plan to conduct a pilot trial and set the following goal and minimum thresholds:"),
                          tags$ol(
                            tags$li("Goal threshold: if 50% of eligible participants consent to participation, the definitive trial is highly likely to succeed in recruitment"),
                            tags$li("Minimum threshold: if consent is below 20%, a definitive trial is unlikely to succeed.")                          
                            ),
                          p("The researchers decide to use the four tier progression criteria for nuanced decision making. Using a one-sample proportion test with a one sided type I error of 0.05 and power of 0.95, researchers calculate a required pilot sample size of 28 participants (14 per group).")
                        ),
                        # Show Count example
                        conditionalPanel(
                          condition = "input.option1 == 'Count'",
                          p("Investigators anticipate that a definitive trial will require 240 participants and have 24 months to recruit. To assess recruitment feasibility, they plan a pilot trial. Their goal threshold is 10 enrollments per month, and their minimum threshold is six per month. If recruitment falls between six and 10, they expect that they will be able to adjust strategies to reach 10 per month and/or to extend the duration of the trial to achieve the required sample size. However, if it drops below six, they anticipate difficulty enrolling 240 participants within the available time."),
                          p("The researchers decide to use the three tier progression criteria for quick decision making. Because the unit of observation is month rather than individuals, they calculate that an enrollment period of five months is needed for the pilot trial. This calculation is based on a one-sample exact test for Poisson rate with a one sided type I error of 0.05 and power of 0.9.")
                        )
                      )
               ),
               # Top Right - Input
               column(6,
                      wellPanel(
                        h3("Input"),
                        conditionalPanel(
                          condition = "input.option1 == 'Binary'",
                          numericInput("alpha_input", "Type I error (α):", value = 0.05),
                          numericInput("beta_input", "Type II error (β):", value = 0.05),
                          numericInput("p_ll_input", "Minimum threshold:", value = 0.2),
                          numericInput("p_ul_input", "Goal threshold:", value = 0.5),
                          actionButton("calc_button21", "Calculate")
                        ),
                        conditionalPanel(
                          condition = "input.option1 == 'Count'",
                          numericInput("alpha_input0", "Type I error (α):", value = 0.05),
                          numericInput("beta_input0", "Type II error (β):", value = 0.1),
                          numericInput("lambda_input0", "Minimum threshold:", value = 6),
                          numericInput("lambda_input1", "Goal threshold:", value = 10),
                          actionButton("calc_button22", "Calculate")
                        )
                      ),
                      # Bottom right - Results
                      wellPanel(
                        h3("Results"),
                        conditionalPanel(
                          condition = "input.option1 == 'Binary'",
                          textOutput("n_output21"),
                          textOutput("n_exact_output"),
                          textOutput("error_message")
                      ),
                      conditionalPanel(
                        condition = "input.option1 == 'Count'",
                        textOutput("n_exact_output22")
                      )
                      )
               )
             )),
    
    tabPanel(strong("Detect a Feasibility Problem"),
             fluidRow(
               # Top Row - Introduction and Input
               column(6,
                      wellPanel(
                        h3("Instructions"),
                        p("This approach calculates sample size for pilot trials aiming to observe at least one instance of a feasibility problem with a desired confidence level. Researchers need to specify:"),
                        tags$ul(
                          tags$li("Minimum probability of the event occurring in an individual participant that would be problematic"),
                          tags$li("Desired level of confidence or certainty in detecting at least one event. ")                          
                        )
                      ),
                      wellPanel(
                        h3("Example"),
                        p("Researchers plan to conduct a definitive trial with a primary outcome based on the presence or absence of a specific condition. Diagnoses of this condition will be made independently by two clinicians. If they disagree, additional assessments, which are more costly and time consuming, will be required. The researchers are concerned about the likelihood of disagreement and aim to study this probability."),
                        tags$ol(
                          tags$li("The researchers assume that if the clinicians disagree on 10% or more of the diagnoses, the expected cost and duration of the definitive trial will be exceeded by a considerable amount."),
                          tags$li("They plan to use a 95% confidence level.")                          
                        ),
                        p("To ensure 95% confidence in observing at least one disagreement, the researchers calculate a sample size of 30 participants (15 per group for a two-group study, rounded up from 29 for 1:1 equal allocation) for the pilot trial.")
                        )
               ),
               column(6,
                      wellPanel(
                        h3("Input"),
                        numericInput("pi_input", "Probability (π):", value = 0.1),
                        helpText("This is the probability that a problem may occur in one participant."),
                        numericInput("gamma_input", "Confidence level (γ):", value = 0.95),
                        helpText("This is the confidence level that researchers want to have with observing at least one occurrence of the problem. A high level of confidence (at least 95%) is recommended to be used."),
                        actionButton("calc_button3", "Calculate")
                      ),
                      wellPanel(
                        h3("Results"),
                        textOutput("n_output3")
                      )
               )
             )),
    
    # Inform sample size -------------------------------------------------------------------------------
    tabPanel(strong("Minimize Combined Sample Size"),
             fluidRow(
               # Top Row - Introduction and Input
               column(6,
                      wellPanel(
                        h3("Instructions"),
                        p("This approach calculates sample size for pilot trials aiming to minimze the combined sample size of pilot and definitive trials, when the definitive trial sample size is calculated based on pilot-estimated variance and then adjusted upward. The pilot trial sample size is calculated via an iterative process and depends on which of following methods is used for definitive trial sample size adjustment:"),
                        tags$ul(
                          tags$li("Upper confidence limit (UCL) method"),
                          tags$li("Non-central t distribution (NCT) method")
                        ),
                        radioButtons("option2", 
                                     "Select Method:",
                                     choices = c("UCL", "NCT"),
                                     selected = "UCL",
                                     inline = TRUE)
                      ),
                      wellPanel(
                        h3("Example"),
                        conditionalPanel(
                          condition = "input.option2 == 'UCL'",
                          p("Researchers plan to test an intervention with a continuous primary outcome in a definitive trial and a standardized mean difference of 0.4, which is the minimum effect of clinical importance. To calculate the pilot sample size that minimizes the overall sample size:"),
                          tags$ol(
                            tags$li("The researchers start with a small pilot of 10 per group."),
                            tags$li("They calculate that the definitive trial needs 100 participants per group, assuming a standardized mean difference of 0.4, a two sided type I error rate of 0.05, and a type II error rate of 0.2. They then inflate this size to 139 per group using the 80% confidence limit method.")        ,
                            tags$li("The combined sample size of both the pilot and definitive trials is 149 per arm."),
                            tags$li("They increase the pilot size by one participant per group and repeat the above three steps."),
                            tags$li("The researchers find that to minimize the combined sample size, the pilot trial should enroll 36 participants (18 per group).")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.option2 == 'NCT'",
                          p("Researchers plan to test an intervention with a continuous primary outcome in a definitive trial and a standardized mean difference of 0.4, which is the minimum effect of clinical importance. To calculate the pilot sample size that minimizes the overall sample size:"),
                          tags$ol(
                            tags$li("The researchers start with a small pilot of 10 per group."),
                            tags$li("They calculate that the definitive trial needs 100 participants per group, assuming a standardized mean difference of 0.4, a two sided type I error rate of 0.05, and a type II error rate of 0.2. They then inflate this size to 109 per group using the non-central t distribution method."),
                            tags$li("The combined sample size of both the pilot and definitive trials is 119 per arm."),
                            tags$li("They increase the pilot size by one participant per group and repeat the above three steps."),
                            tags$li("The researchers find that to minimize the combined sample size, the pilot trial should enroll 22 participants (11 per group).")
                          )
                        )
                      )
               ),
               column(6,
                      wellPanel(
                        h3("Input"),
                        conditionalPanel(
                          condition = "input.option2 == 'UCL'",
                          numericInput("d_input1", "Expected standardized mean difference (e.g., Cohen's d):", value = 0.4, min = 0, max = 1),
                          numericInput("sig_level_input1", "Type I error (α):", value = 0.05, min = 0, max = 1),
                          helpText("This is the type I error rate used for the definitive trial sample size calculation."),
                          numericInput("beta_input1", "Type II error (β):", value = 0.2, min = 0, max = 1),
                          helpText("This is the type II error rate used for the definitive trial sample size calculation."),
                          numericInput("ci_width_input", "Confidence interval width:", value = 0.8, min = 0, max = 1),
                          helpText("This represents the prespecified confidence interval width for the standard deviation estimated by the pilot trial. Commonly used values are 0.8 and 0.95."),
                          actionButton("calc_button41", "Calculate")
                        ),
                        conditionalPanel(
                          condition = "input.option2 == 'NCT'",
                          numericInput("d_input2", "Expected standardized mean difference (e.g., Cohen's d):", value = 0.4, min = 0, max = 1),
                          numericInput("sig_level_input2", "Type I error (α):", value = 0.05, min = 0, max = 1),
                          helpText("This is the type I error rate used for the definitive trial sample size calculation."),
                          numericInput("beta_input2", "Type II error (β):", value = 0.2, min = 0, max = 1),
                          helpText("This is the type II error rate used for the definitive trial sample size calculation."),
                          actionButton("calc_button42", "Calculate")
                        )
                      ),
                      wellPanel(
                        h3("Results"),
                        conditionalPanel(
                          condition = "input.option2 == 'UCL'",
                          textOutput("ucl_output"), 
                          textOutput("error_message41"),
                          textOutput("ucl_n_m")
                        ),
                        conditionalPanel(
                          condition = "input.option2 == 'NCT'",
                          textOutput("nct_output"), 
                          textOutput("error_message42"),
                          textOutput("nct_n_m")
                        )
                      )
               )
             )),
    
     # Efficacy -------------------------------------------------------------------------------
    tabPanel(strong("Rule Out Interventions"),
             fluidRow(
               # Top Row - Introduction and Input
               column(6,
                      wellPanel(
                        h3("Instructions"),
                        p("This approach calculates sample size for pilot trials designed to rule out interventions that are unlikely to produce clinically important effects. To calculate the sample size, researchers would: "),
                        tags$ul(
                          tags$li("Assume a difference of zero between the treatment and control groups"),
                          tags$li("Decide on a minimum clinically important between-group standardized difference (e.g., a standardized mean difference of 0.3)"),
                          tags$li("Set the minimum clinically important difference as the maximum half-width of the 80% or 90% one-sided confidence interval around the zero effect")
                        ),
                        radioButtons("option3", 
                                     "Select Outcome Type:",
                                     choices = c("Continuous", "Binary"),
                                     selected = "Continuous",
                                     inline = TRUE)
                      ),
                      wellPanel(
                        h3("Example"),
                        # Show continuous example
                        conditionalPanel(
                          condition = "input.option3 == 'Continuous'",
                          p("Researchers plan to assess a weight loss program in a definitive trial. They first want to conduct a pilot trial to evaluate the feasibility of the trial protocol. The investigators identify a minimum clinically important effect based on literature and expert opinions. Because the intervention is novel, and because a definitive trial would be expensive, they also want to evaluate whether the intervention is unlikely to produce the minimum clinically important effect. To calculate the sample size, they follow these steps:"),
                          tags$ol(
                            tags$li("They assume no effect of the intervention, setting a mean difference of zero between the weight loss group and control group."),
                            tags$li("They decide on a standardized effect size of 0.3 for the primary outcome. Based on previous studies and expert opinion, this is the smallest effect size they consider meaningful for a definitive trial."),
                            tags$li("They choose to use an 80% one sided confidence interval (CI) for their calculations.")                          
                          ),
                          p("The researchers calculate that the pilot trial needs 32 participants (16 per group) to yield a one sided 80% CI of the effect size with an upper limit below 0.3 when centered around zero.")
                        ),
                        # Show binary example
                        conditionalPanel(
                          condition = "input.option3 == 'Binary'",
                          p("Researchers aim to assess a vaccine uptake intervention and want to exclude it from further testing if it is unlikely to have a minimum clinically important effect:"),
                          tags$ol(
                            tags$li("They assume an equal proportion of vaccinated people in both groups, starting at 50%."),
                            tags$li("They expect the intervention to increase vaccination rates from 50% to 60% (or more)."),
                            tags$li("They use this 10% difference as the maximum half width of a one sided 80% CI.")                          
                          ),
                          p("Based on these parameters, the researchers calculate that the pilot trial needs to include 70 participants.")
                        )
                      )
               ),
               column(6,
                      wellPanel(
                        h3("Input"),
                        conditionalPanel(
                          condition = "input.option3 == 'Continuous'",
                          numericInput("ci_level51", "Confidence level:", value = 0.8),
                          helpText("Commonly used values are 0.8 and 0.9."),
                          numericInput("w_input", "Clinically important effect:", value = 0.3),
                          numericInput("var_input", "Variance:", value = 1),
                          helpText("Enter 1 if standardized mean difference is used as the clinically important effect."),
                          numericInput("R_input", "R (n2/n1 ratio):", value = 1),
                          helpText("e.g., Enter 1 for 1:1 allocation."),
                          actionButton("calc_button51", "Calculate")
                        ),
                        conditionalPanel(
                          condition = "input.option3 == 'Binary'",
                          numericInput("ci_level52", "Confidence level:", value = 0.8),
                          helpText("Commonly used values are 0.8 and 0.9."),
                          numericInput("p_ctl", "Control group proportion", value = 0.5),
                          numericInput("p_diff", "Clinically important effect:", value = 0.1),
                          actionButton("calc_button52", "Calculate")
                        )
                      ),
                      wellPanel(
                        h3("Results"),
                        conditionalPanel(
                          condition = "input.option3 == 'Continuous'",
                          textOutput("n_output51"),
                          textOutput("error_message51")
                      ),
                      conditionalPanel(
                        condition = "input.option3 == 'Binary'",
                        uiOutput("n_output52"), 
                        textOutput("error_message52")
                      )
               )
             ))
    )
))

######################################################################################################
## Define server logic
######################################################################################################
server <- function(input, output) {
  
  # Estimate a Feasibility Parameter -----------------------------------------------------------------
  # Parameter type: Binary
  observeEvent(input$calc_button11, {
    prop <- input$prop
    ci_width1 <- input$ci_width1
    ci_level1 <- input$ci_level1
    side1 <- input$side1
    
    # Check the correctness of input values and provide an error message
    if (prop < 0 || prop > 1 || ci_width1 <= 0 || ci_width1 >= 1 || ci_level1 <= 0 || ci_level1 >= 1 || (side1 != 1 & side1 != 2)) {
      output$n_output11 <- renderText("Error: Please ensure that 0 <= prop <= 1, 0 < ci_width < 1, 0 < ci_level < 1, and side = 1 or 2.")
    } else {
      n1 <- if(side1 == 2) {
        ceiling(prec_prop(p = prop, conf.width = ci_width1*2, conf.level = ci_level1)$n)
      } else if(side1 == 1) {
        ceiling(prec_prop(p = prop, conf.width = ci_width1*2, conf.level = (1-(1-ci_level1)*2))$n)
      }
      output$n_output11 <- renderText(paste("Pilot trial sample size:", n1))
    }
  })
  # Parameter type: Count
  observeEvent(input$calc_button12, {
    rate <- input$rate
    ci_width2 <- input$ci_width2
    ci_level2 <- input$ci_level2
    side2 <- input$side2
    
    # Check the correctness of input values and provide an error message
    if (rate < 0 || ci_width2 <= 0 || ci_level2 <= 0 || ci_level2 >= 1 || (side2 != 1 & side2 != 2)) {
      output$n_output12 <- renderText("Error: Please ensure that rate >= 0, ci_width > 0, 0 < ci_level < 1, and side2 = 1 or 2.")
    } else {
      n2 <- if(side2 == 2) {
        prec_rate(r = rate, conf.width = ci_width2*2, conf.level = ci_level2)$time
      } else if(side2 == 1) {
        prec_rate(r = rate, conf.width = ci_width2*2, conf.level = (1-(1-ci_level2)*2))$time
      }      
      output$n_output12 <- renderText(
        paste(
          "Number of observation units:", n2
        ))
    }
  })
  
  # Test feasibility progression criteria ---------------------------------------------------------------
  # Parameter type: Binary
  observeEvent(input$calc_button21, {
    alpha <- input$alpha_input
    beta <- input$beta_input
    p_ll <- input$p_ll_input
    p_ul <- input$p_ul_input
    
    # Error checking for inputs
    if(alpha <= 0 | alpha >= 1 | beta <= 0 | beta >= 1 | p_ll <= 0 | p_ll >= 1 | p_ul <= 0 | p_ul >= 1 | p_ul <= p_ll) {
      output$error_message <- renderText({
        "Error: Please ensure 0 < alpha, beta, p_ll, p_ul < 1 and p_ul > p_ll."
      })
      output$n_output21 <- renderText({ "" })
      output$n_exact_output <- renderText({ "" })
      
    } else {
      output$error_message <- renderText({ "" })
      
      # Proportion test sample size calculation
      n <- ((qnorm(1-alpha)*sqrt(p_ll*(1-p_ll)) + qnorm(1-beta)*sqrt(p_ul*(1-p_ul)))/(p_ul-p_ll))^2
      n <- 0.25*n*(1 + sqrt(1 + 2/(n*abs(p_ul-p_ll))))^2
      
      output$n_output21 <- renderText({
        paste("Pilot trial sample size based on Proportion test with continuity correction:", n)
      })
      
      # Binomial exact test sample size calculation
      N <- 1:10000 
      CritVal <- qbinom(p = 1 - alpha, size = N, prob = p_ll)
      Beta <- pbinom(CritVal, N, p_ul)
      Power <- 1 - Beta
      SampSize <- min(which(Power > 1-beta))
      n_exact <- N[SampSize]
      
      output$n_exact_output <- renderText({
        paste("Pilot trial sample size based on Binomial exact test:", n_exact)
      })
    }
  })
  
  # Parameter type: Count
  observeEvent(input$calc_button22, {
    alpha <- input$alpha_input0
    beta <- input$beta_input0
    lambda0 <- input$lambda_input0
    lambda1 <- input$lambda_input1
    
    # Error checking for inputs
    if(alpha <= 0 | alpha >= 1 | beta <= 0 | beta >= 1 | lambda0 == lambda1) {
      output$error_message <- renderText({
        "Error: Please ensure 0 < alpha, beta < 1 and lambda0 != lambda1."
      })
      output$n_exact_output22 <- renderText({ "" })
      
    } else {
      output$error_message <- renderText({ "" })
      
      # Poisson exact test sample size calculation
      n_exact=getDesignOneRateExact(beta = beta, lambdaH0 = lambda0, lambda = lambda1, D = 1, alpha = alpha)$n
      
      output$n_exact_output22 <- renderText({
        paste("Number of observation units based on Poisson exact test:", n_exact)
      })
    }
  })
  
  # Detect a feasibility problem ---------------------------------------------------------------
  observeEvent(input$calc_button3, {
    gamma_input <- input$gamma_input
    pi_input <- input$pi_input
    
    # Check the correctness of input values and provide an error message
    if (gamma_input < 0 || gamma_input > 1 || pi_input <= 0 || pi_input >= 1 ) {
      output$n_output3 <- renderText("Error: Please ensure that 0<π<1 and 0<γ<1.")
    } else {
      n <- log(1 - gamma_input) / log(1 - pi_input)
      output$n_output3 <- renderText(paste("Pilot trial sample size:", ceiling(n)))
    }
  })
  
  # Minimize total sample size ---------------------------------------------------------------
  observeEvent(input$calc_button41, {
    d1 <- input$d_input1
    sig.level1 <- input$sig_level_input1
    beta1 <- input$beta_input1
    n_p1 <- 1:1000
    ci.width <- input$ci_width_input
    n_m1 <- pwr.t.test(d = d1, sig.level = sig.level1, power = 1 - beta1, alternative = "two.sided")$n
    
    
    # Error checking for inputs
    if(sig.level1 <= 0 | sig.level1 >= 1 | beta1 <= 0 | beta1 >= 1 | ci.width <= 0 | ci.width >= 1 ) {
      output$error_message41 <- renderText({
        "Error: Please ensure 0 < alpha, beta, ci.width < 1."
      })
      output$ucl_output <- renderText({ "" })
    } else {
      output$error_message41 <- renderText({ "" })
      
      # UCL sample size calculation
      N1 <- c()
      for (i in 1:length(n_p1)) {
        N1[i] <- 2 * n_p1[i] + 2 * n_m1 * (2 * n_p1[i] - 2) / qchisq(1 - ci.width, 2 * n_p1[i] - 2)
      }
      n_ucl <- n_p1[which.min(N1)] * 2
      
      ucl_n_m <- ceiling(N1[which.min(N1)])  - n_ucl
      
      output$ucl_output <- renderText({
        paste("Pilot trial sample size based on UCL:", n_ucl)
      })
      
      output$ucl_n_m <- renderText({
        paste("Definitive trial sample size adjusted by UCL:", ucl_n_m)
      })
    }
  })
  
  observeEvent(input$calc_button42, {
    d2 <- input$d_input2
    sig.level2 <- input$sig_level_input2
    beta2 <- input$beta_input2
    n_p2 <- 1:1000
    n_m2 <- pwr.t.test(d = d2, sig.level = sig.level2, power = 1 - beta2, alternative = "two.sided")$n
    
    # Error checking for inputs
    if(sig.level2 <= 0 | sig.level2 >= 1 | beta2 <= 0 | beta2 >= 1 ) {
      output$error_message42 <- renderText({
        "Error: Please ensure 0 < alpha, beta < 1."
      })
      output$nct_output <- renderText({ "" })
      
    } else {
      output$error_message42 <- renderText({ "" })
      
      # NCT sample size calculation
      N2 <- c()
      for (i in 1:length(n_p2)) {
        N2[i] <- n_p2[i] + 2 * (qt(1 - beta2, df = 2 * n_p2[i] - 2, ncp = qt(1-sig.level2/2, 2 * n_m2 - 2), lower.tail = T))^2 / d2^2
      }
      n_nct <- n_p2[which.min(N2)] * 2
      
      nct_n_m <- ceiling(N2[which.min(N2)])*2  - n_nct

      
      output$nct_output <- renderText({
        paste("Pilot trial sample size based on NCT:", n_nct)
      })
      
      output$nct_n_m <- renderText({
        paste("Definitive trial sample size adjusted by NCT:", nct_n_m)
      })
    }
  })
  
  # Rule out interventions ---------------------------------------------------------------
  observeEvent(input$calc_button51, {
    
    # Check for input validity
    valid_input51 <- TRUE
    error_msg51 <- ""
    
    if(input$ci_level51 < 0 || input$ci_level51 > 1) {
      valid_input51 <- FALSE
      error_msg51 <- "Alpha should be between 0 and 1."
    } else if(input$w_input < 0) {
      valid_input51 <- FALSE
      error_msg51 <- "w should be non-negative."
    } else if(input$var_input < 0) {
      valid_input51 <- FALSE
      error_msg51 <- "Variance should be non-negative."
    } else if(input$R_input < 0) {
      valid_input51 <- FALSE
      error_msg51 <- "R should be non-negative."
    }
    
    output$error_message51 <- renderText({
      error_msg51
    })
    
    # If input is valid, perform calculations
    if(valid_input51) {
      tryCatch({
        n51 <- size.ci.mean2(
          alpha = (1-input$ci_level51)*2,
          var = input$var_input,
          w = input$w_input * 2, 
          R = input$R_input
        )[1]
        
        # Display results
        output$n_output51 <- renderText({
          paste("Pilot trial sample size:", ceiling(n51)*2)
        })
      }, error = function(e) {
        # Error handling during calculation
        output$error_message51 <- renderText({
          paste("Calculation Error: ", e$message)
        })
        output$n_output51 <- renderText({ "" })
      })
    } else {
      # Clear result output when input is invalid
      output$n_output51 <- renderText({ "" })
    }
  }) 

  observeEvent(input$calc_button52, {
    
    # Check for input validity
    valid_input52 <- TRUE
    error_msg52 <- ""
    
    if(input$ci_level52 < 0 || input$ci_level52 > 1) {
      valid_input52 <- FALSE
      error_msg52 <- "Confidence level should be between 0 and 1."
    } else if(input$p_ctl < 0 || input$p_ctl > 1) {
      valid_input52 <- FALSE
      error_msg52 <- "Control group proportion should be between 0 and 1."
    } 
    
    # Display any input validation error
    output$error_message52 <- renderText({ error_msg52 })
    
    # If input is valid, perform calculations
    if(valid_input52) {
      tryCatch({
        # Perform Newcombe method calculation
        n52 <- prec_riskdiff(
          p1 = input$p_ctl, 
          p2 = input$p_ctl, 
          conf.width = input$p_diff * 2, 
          conf.level = 1 - (1 - input$ci_level52) * 2, 
          method = "newcombe"
        )$n1
        
        # Perform Wald method calculation
        n52_e <- prec_riskdiff(
          p1 = input$p_ctl, 
          p2 = input$p_ctl, 
          conf.width = input$p_diff * 2, 
          conf.level = 1 - (1 - input$ci_level52) * 2, 
          method = "wald"
        )$n1
        
        # Display both results in one output
        output$n_output52 <- renderUI({
          HTML(
            paste(
              "Pilot trial sample size (Newcombe):", ceiling(n52) * 2, "<br>",
              "Pilot trial sample size (Wald):", ceiling(n52_e) * 2
            )
          )
        })
        
        # Clear any previous error message
        output$error_message52 <- renderText({ "" })
        
      }, error = function(e) {
        # Error handling during calculation
        output$error_message52 <- renderText({
          paste("Calculation Error:", e$message)
        })
        output$n_output52 <- renderText({ "" })
      })
    } else {
      # Clear result output when input is invalid
      output$n_output52 <- renderText({ "" })
    }
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
