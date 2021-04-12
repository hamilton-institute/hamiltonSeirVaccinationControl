#' control_ui Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
control_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    bs4Dash::bs4DashPage(
      sidebar_collapsed = TRUE,
      sidebar_mini = FALSE,
      body = bs4Dash::bs4DashBody(
        hamiltonThemes::use_bs4Dash_distill_theme(),
        shinyjs::useShinyjs(),
        br(),
        fluidRow(
          column(
            width = 3,
            dateInput(ns("start_vac"), "Starting date of vaccination", "2021-01-01" ),
            sliderInput(ns("vacc_control"), "Percentage of population to vaccinate (%)", 0, 100, 60, step = 1),
            sliderInput(ns("vacc_ef"), "Vaccine effectiveness (%)", 50, 100, 90, step = 1),
            sliderInput(ns("vacc_re"), "Vaccine refusal rate (%)", 0, 100, 30, step = 1),
            # https://www.irishpost.com/news/almost-a-third-of-irish-people-would-refuse-covid-19-vaccine-survey-says-194257
            
            sliderInput(ns("R0_Y"), "Average number of infections from each infected person (R number) for under 65s", 0, 10, 0.8, step=0.1),
            
            sliderInput(ns("R0_O"), "Average number of infections from each infected person (R number) for over 65s", 0, 10, 0.8, step=0.1),
            
            sliderInput(inputId = ns("R0_O_Y"),
                        label = "Average number of infections passed between under and over 65s per infected person (Cross R number)",
                        0, 10, 0.3, step=0.1),
            
            actionButton(inputId = ns("button"), label = "Show/hide extra options"),
            
            numericInput(inputId = ns("num_days"),
                         label = "Total number of days that vaccine can be administered",
                         min = 0,
                         value = 1000),
            
            numericInput(inputId = ns("exp_Y"),
                         label = "Number of asymptomatic spreaders under 65 at start date",
                         min = 0,
                         value = 2000),
            
            numericInput(inputId = ns("inf_Y"),
                         label = "Number of symptomatic spreaders under 65 at start date",
                         value = 2000),
            
            numericInput(inputId = ns("exp_O"),
                         label = "Number of asymptomatic spreaders over 65 at start date",
                         value = 200),
            
            numericInput(inputId = ns("inf_O"),
                         label = "Number of symptomatic spreaders over 65 at start date",
                         value = 200),
            
            numericInput(inputId = ns("rec_Y"),
                         label = "Number of recovered (i.e. immune) people under 65 at start date",
                         value = 200000),
            
            numericInput(inputId = ns("rec_O"),
                         label = "Number of recovered (i.e. immune) people over 65 at start date",
                         value = 100000),
            
            numericInput(inputId = ns("pop_Y"),
                         label = "Population of Ireland under 65",
                         value = 4000000),
            
            numericInput(inputId = ns("pop_O"),
                         label = "Population of Ireland over 65",
                         value = 900000)
          
          ),
          bs4Dash::bs4TabCard(
            width = 9,
            title = "COVID-19 Optimal Vaccination Policy",
            id = 'tabcard',
            closable = FALSE,
            collapsible = FALSE,
            bs4Dash::bs4TabPanel(
              tabName = "Spread",
              plotly::plotlyOutput(ns("plot"), height = 500) %>% hamiltonThemes::distill_load_spinner(),
              checkboxInput(ns("log_scale"), "Log scale?", value = FALSE)
            ),
            bs4Dash::bs4TabPanel(
              tabName = "Assumptions",
              get_assumptions_text2()
            )
          )
        )
      ),
      footer = hamiltonThemes::bs4dash_distill_footer()
    )
  )
}
    
#' original_v2 Server Function
#'
#' @noRd 
control_server <- function(input, output, session){
  ns <- session$ns
 
  observeEvent(input$button, {
    shinyjs::toggle("num_days")
    shinyjs::toggle("exp_Y")
    shinyjs::toggle("exp_O")
    shinyjs::toggle("inf_Y")
    shinyjs::toggle("inf_O")
    shinyjs::toggle("rec_Y")
    shinyjs::toggle("rec_O")
    shinyjs::toggle("pop_Y")
    shinyjs::toggle("pop_O")
  }, ignoreNULL = FALSE)
  
  output$plot <- plotly::renderPlotly({
    ##### General setup
    # Inputs are YSU, YSNV, YE, YI, YR, OSU, OSNV, OE, OI, OR, YR0Y, YR0O, OR0Y, OR0O, Yvac, Ovac, Veff
    # where YSU = Young Susceptible Not Yet Vaccinated
    # YSNV = Young Susceptible Refused Vaccine
    # etc
    
    # Outputs are Time YSU YSV YSVNE YSNV YE YI YR YRV OSU OSV OSVNE OSNV OE OI OR ORV
    # These mean
    # YSV = Young Susceptible And Vaccinated - waiting for it to become effective
    # YSVNE = Young Susceptible but Vaccine Not Effective
    # YSNV = Young Susceptible But Refused Vaccine
    # YR = Young Recovered due to having the disease
    # YVR = Young Recovered due to having vaccination
    Y_Pop <- input$pop_Y #Total young susceptible population
    O_Pop <- input$pop_O #Total old susceptible population
    young_nv <- input$vacc_re/100 #Percentage of young people unwilling to get vaccinated
    old_nv <- input$vacc_re/100 #Percentage of old people unwilling to get vaccinated
    young_exp <- input$exp_Y #Number of young exposed
    old_exp <- input$exp_O #Number of old exposed
    young_inf <- input$inf_Y #Number of young infected
    old_inf <- input$inf_O #Number of old infected
    time_horizon <- input$num_days #Number of days
    
    # Parameters
    mean_h.t_inf <- 7.4 #Mean holding time in the infected state
    mean_h.t_exp <- 6.6 #Mean holding time in the exposed state
    R0_oo <- input$R0_O #Average number of old people an old person can infect before recovering
    R0_oy <- input$R0_O_Y #Average number of young people an old person can infect before recovering
    R0_yo <- input$R0_O_Y #Average number of old people an young person can infect before recovering
    R0_yy <- input$R0_Y #Average number of young people an young person can infect before recovering
    mean_h.t_vacc <- 14 #Mean holding time before the vaccine becomes effective
    effectiveness <- input$vacc_ef/100 #Vaccine effectiveness
    
    d <- 0.00005 #Threshold for convergence to optimal trajectory
    error_term <- 1 #Random initialisation to enter the while loop
    
    upp.control <- input$vacc_control #Upper bound for control
    
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- - - - - - - - - - - - - - - - - - Creating state and parameter vectors- - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    
    State <- c(OSU = (1-old_nv)*(O_Pop-(old_exp+old_inf+old_rec)), OSV =0, OSVNE =0, 
           OSNV =old_nv*(O_Pop-(old_exp+old_inf+old_rec)), OE =old_exp, OI =old_inf, OR =old_rec, ORV =0, 
           YSU = (1-young_nv)*(Y_Pop-(young_exp+young_inf+young_rec)), YSV =0, YSVNE =0, 
           YSNV =young_nv*(Y_Pop-(young_exp+young_inf+young_rec)), YE =young_exp, YI =young_inf, YR =young_rec, YRV =0,
           LOSU = 0, LOSV = 0, LOSVNE = 0, LOSNV = 0, LOE = 0, LOI = 0,
           LYSU = 0, LYSV = 0, LYSVNE = 0, LYSNV = 0, LYE = 0, LYI = 0)

    Parameters <- c(beta00 = R0_oo/(mean_h.t_exp+mean_h.t_inf+mean_h.t_vacc), beta01 = R0_yo/(mean_h.t_exp+mean_h.t_inf+mean_h.t_vacc), 
                beta10 = R0_oy/(mean_h.t_exp+mean_h.t_inf+mean_h.t_vacc), beta11 = R0_yy/(mean_h.t_exp+mean_h.t_inf+mean_h.t_vacc),
                vacc = 1/mean_h.t_vacc, veff = effectiveness, gammaE = 1/mean_h.t_exp, gammaI = 1/mean_h.t_inf, WO = 10^11, 
                WY = 10^11, r_O=old_nv, r_Y=young_nv)

    
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- - - - - - - - - - - - - - - - - - Time sequence and intial control- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    
    times_state <- seq(from=0,to=time_horizon,by=1)
    times_adjoint <- seq(from=time_horizon,to=0,by=-1)
    
    signal_uO = as.data.frame(list(times = times_state, import = rep(0,length(times_state))))
    signal_uY = signal_uO
    
    uY = stats::approxfun(signal_uY, rule = 2)
    uO = stats::approxfun(signal_uO, rule = 2)
    
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- - - - - - - - - - - - - - - - - - -Define the state ODEs- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    
    state_system <- function(t, state, parms){
      #Extract state variables
      OSU = state[1]
      OSV = state[2]
      OSVNE = state[3]
      OSNV = state[4]
      OE = state[5]
      OI = state[6]
      OR = state[7]
      ORV = state[8]
      YSU = state[9]
      YSV = state[10]
      YSVNE = state[11]
      YSNV = state[12]
      YE = state[13]
      YI = state[14]
      YR = state[15]
      YRV = state[16]
      LOSU = state [17]
      LOSV = state[18]
      LOSVNE = state[19]
      LOSNV = state[20]
      LOE = state[21]
      LOI = state[22]
      LYSU = state[23]
      LYSV = state[24]
      LYSVNE = state[25]
      LYSNV = state[26]
      LYE = state[27]
      LYI = state[28]
      
      #Extract parameter values
      beta00 = parms[1]
      beta01 = parms[2]
      beta10 = parms[3]
      beta11 = parms[4]
      vacc = parms[5]
      veff = parms[6]
      gammaE = parms[7]
      gammaI = parms[8]
      WO = parms[9]
      WY = parms[10]
      r_O = parms[11]
      r_Y = parms[12]
      
      #Total populations of young and old people
      ON <- OSU + OSV + OSVNE + OSNV + OE + OI + OR + ORV
      YN <- YSU + YSV + YSVNE + YSNV + YE + YI + YR + YRV
      
      #Read in the control function
      uOt <- uO(t)
      uYt <- uY(t)
      
      #State equations
  dOSU <- - (beta00*(OE+OI)/ON + beta10*(YE+YI)/YN)*OSU-uOt*OSU 
  dOSV <- uOt*OSU - (beta00*(OE+OI)/ON + beta10*(YE+YI)/YN)*OSV - (1-veff)*vacc*OSV - vacc*veff*OSV
  dOSVNE <- (1-veff)*vacc*OSV - (beta00*(OE+OI)/ON + beta10*(YE+YI)/YN)*OSVNE
  dOSNV <- -(beta00*(OE+OI)/ON + beta10*(YE+YI)/YN)*OSNV
  dOE <- (beta00*(OE+OI)/ON + beta10*(YE+YI)/YN)*(OSU + OSV + OSVNE + OSNV) - gammaE*OE
  dOI <- gammaE*OE - gammaI*OI
  dOR <- gammaI*OI
  dORV <- vacc*veff*OSV
  dYSU <- - (beta01*(OE+OI)/ON + beta11*(YE+YI)/YN)*YSU-uYt*YSU
  dYSV <- uYt*YSU - (beta01*(OE+OI)/ON + beta11*(YE+YI)/YN)*YSV -(1-veff)*vacc*YSV -vacc* veff*YSV
  dYSVNE <- (1-veff)*vacc*YSV - (beta01*(OE+OI)/ON + beta11*(YE+YI)/YN)*YSVNE
  dYSNV <- -(beta01*(OE+OI)/ON + beta11*(YE+YI)/YN)*YSNV
  dYE <- (beta01*(OE+OI)/ON + beta11*(YE+YI)/YN)*(YSU + YSV + YSVNE + YSNV) - gammaE*YE
  dYI <- gammaE*YE - gammaI*YI
  dYR <- gammaI*YI
  dYRV <- vacc*veff*YSV
  dLOSU <- 0
  dLYSU <- 0
  dLOSV <- 0
  dLYSV <- 0
  dLOSVNE <- 0
  dLYSVNE <-0
  dLOSNV <- 0
  dLYSNV <- 0
  dLOE <- 0
  dLYE <- 0
  dLOI <- 0
  dLYI <- 0
  
  dxdt <- c(dOSU, dOSV, dOSVNE, dOSNV, dOE, dOI, dOR, dORV, dYSU, dYSV, dYSVNE, dYSNV, dYE, dYI, dYR, dYRV,
            dLOSU, dLOSV, dLOSVNE, dLOSNV, dLOE, dLOI, dLYSU, dLYSV, dLYSVNE, dLYSNV, dLYE, dLYI )
  list(dxdt)
}   
    
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- - - - - - - - - - - - - - - - - - -Solve the state ODEs forwards in time- - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    
    deSolve::ode(
      func=state_system,
      y=State,
      times=times_state,
      parms=Parameters,
      rtol = 1e-2,
      atol = 1e-2
    ) %>%
      as.data.frame() -> out_state
    
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- - - - - - - - - - - - - - - - - - -Define the adjoint ODEs- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    
    adjoint_system <- function(t, state, parms){
      #Extract state variables
      OSU = state[1]
      OSV = state[2]
      OSVNE = state[3]
      OSNV = state[4]
      OE = state[5]
      OI = state[6]
      OR = state[7]
      ORV = state[8]
      YSU = state[9]
      YSV = state[10]
      YSVNE = state[11]
      YSNV = state[12]
      YE = state[13]
      YI = state[14]
      YR = state[15]
      YRV = state[16]
      LOSU = state [17]
      LOSV = state[18]
      LOSVNE = state[19]
      LOSNV = state[20]
      LOE = state[21]
      LOI = state[22]
      LYSU = state[23]
      LYSV = state[24]
      LYSVNE = state[25]
      LYSNV = state[26]
      LYE = state[27]
      LYI = state[28]
      
      #Extract parameter values
      beta00 = parms[1]
      beta01 = parms[2]
      beta10 = parms[3]
      beta11 = parms[4]
      vacc = parms[5]
      veff = parms[6]
      gammaE = parms[7]
      gammaI = parms[8]
      WO = parms[9]
      WY = parms[10]
      r_O = parms[11]
      r_Y = parms[12]
      
      ON <- OSU + OSV + OSVNE + OSNV + OE + OI + OR + ORV
      YN <- YSU + YSV + YSVNE + YSNV + YE + YI + YR + YRV
      
      uOt <- uO(t)
      uYt <- uY(t)
      
      #Adjoint equations
      dOSU <- 0
      dOSV <- 0
      dOSVNE <- 0
      dOSNV <- 0
      dOE <- 0
      dOI <- 0
      dOR <- 0
      dORV <- 0
      dYSU <- 0
      dYSV <- 0
      dYSVNE <- 0
      dYSNV <- 0
      dYE <- 0
      dYI <- 0
      dYR <- 0
      dYRV <- 0
      dLOSU <- (beta00*(OE+OI)/ON + beta10*(YE+YI)/YN + uOt)*LOSU - uOt*LOSV 
      - (beta00*(OE+OI)/ON + beta10*(YE+YI)/YN)*LOE 
      dLYSU <- (beta01*(OE+OI)/ON + beta11*(YE+YI)/YN + uYt)*LYSU - uYt*LYSV 
      - (beta01*(OE+OI)/ON + beta11*(YE+YI)/YN)*LYE 
      dLOSV <- (beta00*(OE+OI)/ON + beta10*(YE+YI)/YN + (1-veff)*vacc + veff*vacc)*LOSV - (1-veff)*vacc*LOSVNE - (beta00*(OE+OI)/ON + beta10*(YE+YI)/YN)*LOE
      dLYSV <- (beta01*(OE+OI)/ON + beta11*(YE+YI)/YN + (1-veff)*vacc + veff*vacc)*LYSV - (1-veff)*vacc*LYSVNE - (beta01*(OE+OI)/ON + beta11*(YE+YI)/YN)*LYE
      dLOSVNE <- (beta00*(OE+OI)/ON + beta10*(YE+YI)/YN)*(LOSVNE-LOE)
      dLYSVNE <- (beta01*(OE+OI)/ON + beta11*(YE+YI)/YN)*(LYSVNE-LYE)
      dLOSNV <- (beta00*(OE+OI)/ON + beta10*(YE+YI)/YN)*(LOSNV-LOE)
      dLYSNV <- (beta01*(OE+OI)/ON + beta11*(YE+YI)/YN)*(LYSNV-LYE)
      dLOE <- beta00*OSU/ON*(LOSU-LOE) + beta00*OSV/ON*(LOSV-LOE) + beta00*OSVNE/ON*(LOSVNE-LOE) + beta00*OSNV/ON*(LOSNV-LOE) + 
        beta01*YSU/ON*(LYSU-LYE) + beta01*YSV/ON*(LYSV-LYE) + beta01*YSVNE/ON*(LYSVNE-LYE) + beta01*YSNV/ON*(LYSNV-LYE) + 
        gammaE*LOE - gammaE*LOI
      dLYE <- beta10*OSU/YN*(LOSU-LOE) + beta10*OSV/YN*(LOSV-LOE) + beta10*OSVNE/YN*(LOSVNE-LOE) + beta10*OSNV/YN*(LOSNV-LOE) + 
        beta11*YSU/YN*(LYSU-LYE) + beta11*YSV/YN*(LYSV-LYE) + beta11*YSVNE/YN*(LYSVNE-LYE) + beta11*YSNV/YN*(LYSNV-LYE) + 
        gammaE*LYE - gammaE*LYI
      dLOI <- beta00*OSU/ON*(LOSU-LOE) + beta00*OSV/ON*(LOSV-LOE) + beta00*OSVNE/ON*(LOSVNE-LOE) + beta00*OSNV/ON*(LOSNV-LOE) + 
        beta01*YSU/ON*(LYSU-LYE) + beta01*YSV/ON*(LYSV-LYE) + beta01*YSVNE/ON*(LYSVNE-LYE) + beta01*YSNV/ON*(LYSNV-LYE) + 
        gammaI*LOI -1
      dLYI <- beta10*OSU/YN*(LOSU-LOE) + beta10*OSV/YN*(LOSV-LOE) + beta10*OSVNE/YN*(LOSVNE-LOE) + beta10*OSNV/YN*(LOSNV-LOE) + 
        beta11*YSU/YN*(LYSU-LYE) + beta11*YSV/YN*(LYSV-LYE) + beta11*YSVNE/YN*(LYSVNE-LYE) + beta11*YSNV/YN*(LYSNV-LYE) +
        gammaI*LYI - 1

      dxdt <- c(dOSU, dOSV, dOSVNE, dOSNV, dOE, dOI, dOR, dORV, dYSU, dYSV, dYSVNE, dYSNV, dYE, dYI, dYR, dYRV,
                dLOSU, dLOSV, dLOSVNE, dLOSNV, dLOE, dLOI, dLYSU, dLYSV, dLYSVNE, dLYSNV, dLYE, dLYI)
      list(dxdt)
    }
    
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- - - - - - - - - - - - - - - - - - -Solve the adjoint ODEs backwards in time - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    
    deSolve::ode(
      func=adjoint_system,
      y=as.numeric(out_state[length(out_state),-1]), #The input is the output from the state ODEs
      times=times_adjoint,
      parms=Parameters,
      rtol = 1e-2,
      atol = 1e-2
    ) %>%
      as.data.frame() -> out_adjoint
    
    
    iteration <- 1 #Counter
    
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- - - - - - - - - - - - - - - - - - -Repeat process until convergence - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    
    while (error_term > d) {

       # New control 
    new_uO <- out_state[,"OSU"]/Parameters["WO"]*(out_adjoint[,17]- out_adjoint[,18])
    new_uY <- out_state[,"YSU"]/Parameters["WY"]*(out_adjoint[,23]- out_adjoint[,24])
      
      if(any(new_uO>upp.control)){
        new_uO <- rep(upp.control,length(times_state))
      }else if(any(new_uO<0)){
        new_uO <- 0
      }
      
      if(any(new_uY>upp.control)){
        new_uY <- rep(upp.control,length(times_state))
      }else if(any(new_uY<0)){
        new_uY <- 0
      }
      
      signal_uO = as.data.frame(list(times = times_state, import = new_uO))
      signal_uY = as.data.frame(list(times = times_state, import = new_uY))
      uY = stats::approxfun(signal_uY, rule = 2)
      uO = stats::approxfun(signal_uO, rule = 2)
      
      state_previous <- as.matrix(out_state[,-1]) # Needed for the error term
      dimnames(state_previous)<- NULL
      
      deSolve::ode(
        func=state_system,
        y=State,
        times=times_state,
        parms=Parameters,
        rtol = 1e-2,
        atol = 1e-2
      ) %>%
        as.data.frame() -> out_state
      
      deSolve::ode(
        func=adjoint_system,
        y=as.numeric(out_state[length(out_state),-1]),
        times=times_adjoint,
        parms=Parameters,
        rtol = 1e-2,
        atol = 1e-2
      ) %>%
        as.data.frame() -> out_adjoint
      
      state_current <- as.matrix(out_state[,-1]) # Needed for the error term
      dimnames(state_current)<- NULL
      
      error_term <- norm(state_previous-state_current,type = "1")/norm(state_previous,type = "1")
      
      iteration <- iteration+1 # Counter of iterations
    }
    
    # Extract out the infections and quantiles for each group
    final_raw = out_state[,c('YR', 'OR', 'YRV', 'ORV')]
    colnames(final_raw) = c("Under 65s recovered from disease",
                            "Over 65s recovered from disease",
                            "Under 65s successfully vaccinated",
                            "Over 65s successfully vaccinated")
    final_raw$Date = seq.Date(from = as.Date(input$start_vac), to = as.Date(input$start_vac) + nrow(final_raw) - 1,
                    by = 1)
    
    # Tidy up into one data frame
    final = final_raw %>% 
      tidyr::pivot_longer(names_to = 'Type', values_to = 'Count', -Date) %>% 
      dplyr::mutate(Count = round(Count))
    
    # This caused a load of pain but replaced three of the above lines  
    #   tidyr::separate(Type, c("Age group", "Type"), sep = "XXX") %>% 
    plt1 = ggplot2::ggplot(final,
                           ggplot2::aes(x = Date, colour = Type)) +
      ggplot2::geom_line(ggplot2::aes(y = `Count`)) +
      ggplot2::labs(x = "Date", title = "Virus progression with vaccination effectiveness and refusal", y = NULL) +
      ggplot2::scale_x_date(date_labels = "%d-%b-%y") + 
      ggplot2::scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
      ggplot2::theme_bw()
    # theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0))
    if(input$log_scale) plt1 = plt1 + ggplot2::scale_y_log10(expand = c(0, 0), labels = scales::comma)
    
    plotly::ggplotly(plt1)
    
  })
}
    
## To be copied in the UI
# mod_original_v2_ui("original_v2_ui_1")
    
## To be copied in the server
# callModule(mod_original_v2_server, "original_v2_ui_1")
