# The contents of this file will be run for all testthat tests

library(data.table)

# Setup function for creating test data
setup_test_data <- function() {
  # Sample analytic data ----
    set.seed(98104)

    test_analytic <- data.table(
      chi_year = rep(2015:2024, each = 100),
      chi_sex = sample(c("Female", "Male"),
                       size = 1000,  # 100 rows * 10 years
                       replace = TRUE),
      chi_geo_region = sample(c("East", "North", "Seattle", "South"),
                               size = 1000,
                               replace = TRUE)
    )

    test_analytic[, `:=`(
      indicator1 = sample(0:1, .N, replace = TRUE),
      indicator2 = sample(0:1, .N, replace = TRUE)
    )]

    test_analytic[, chi_geo_kc := 'King County']

    setorder(test_analytic, chi_year)

  # Sample analysis set ----
    test_analysis_set <- data.table(
      cat1 = c('Regions', 'Gender'),
      cat1_varname = c('chi_geo_region', 'chi_sex'),
      `_kingcounty` = c('x'),
      `_wastate` = NA_character_,
      demgroups = NA_character_,
      crosstabs = NA_character_,
      trends = NA_character_,
      set = 1,
      set_indicator_keys = 'indicator1, indicator2'
    )


    ################################ migrate this out  ###########################################################


    data_modeller <- function(ph.data, number_of_observations, return_code = TRUE, comments = TRUE) {
      ### receives a data table of public health data, number of observations and user decision if they want code (or a DT) and, if code, if it should be commented
      ### "number_of_observations" may be a number or a string
      ### returns code or a DT of identical structure and similar, but non-correlated, values for each variable provided that can be modeled. If comments are enabled, will return comment for non modeled variables.
      ### warning: r has a run-instruction character limit of 4094. If code is requested, and the resulting instruction is longer, you must break this into seperate instructions, such making several smaller DTs and binding them together. (test if sourcing as a script is an exception to the limit)
      ### warning: (not implemented) currently will create multiple years, but reads the received data set as if it were one year, and models multiple years by repeating the model process with shifted seed
      ### warning: (not implemented) number.of.observations is of the final dataset. If the requested number does not divide evenly across the number of years, the result will be rounded up and the user should remove observations if necessary
      ### warning: the data returned is modelled on the data given but correlations between variables are not. This effectively anonymizes results as long as the underlying populations are diverse or large enough. A small enough population may provide sufficient certainty of the results. (note to self for vignette: build an example showing seperate runs. One of a DT built from multiple mono-race reuslts bound together, versus building results from a table with observations from multiple races. Show how the results in the former more closely resembles results by race from actual data.)

      if(inherits(number_of_observations, "character") & return_code == FALSE) {
        number_of_observations <- as.integer(number_of_observations)
        if(is.na(number_of_observations)) {
          stop("user has requested data, but 'number_of_observations' could not be coerced to an integer. 'number_of_observations' must be an integer.")
        }
        if(!(number_of_observations > 0)) {
          stop("number_of_observations must be an integer greater than 0")
        }
      }
      #if(!return_code & comments) {
      #  message("user has requested data, 'comments' set to FALSE.")
      #  comments <- FALSE
      #}


      variable_modeller <- function(oneVariable, number_of_observations, varName = NA, comments = TRUE) {
        if(any(class(oneVariable) %in% "data.table")) {
          if(ncol(oneVariable) == 1) {
            message(class(oneVariable))
            oneVariable <- oneVariable[,1][[1]]
            message(class(oneVariable))
            message("caught DT")
          } else {
            stop("more than 1 column passed. Only pass a vector or one column")
          }
        }

        instructions <- NA

        if(is.na(varName)){
          variableName <- sub(".*\\$.*?", "\\1", deparse(substitute(oneVariable)))
        } else {
          variableName <- varName
        }

        oneVariableClass <- class(oneVariable)

        #factor
        if(is.na(instructions) & inherits(oneVariable, "factor")) {
          orderTF <- is.ordered(oneVariable)
          detectedLevels <- levels(oneVariable)
          instructions <- paste0(variableName," = factor(sample(c('",paste0(unlist(unique(oneVariable)),collapse = "', '"),"'), ", number_of_observations,", replace = TRUE, prob = c(",paste0(prop.table(table(oneVariable, useNA = 'ifany')), collapse = ", "),")), levels = c('",paste0(detectedLevels, collapse = "', '"),"'), ordered = ", orderTF,")", collapse = "")
          instructions <- gsub("'NA'", "NA", instructions)
          if(comments){
            instructions <- paste0(instructions, " # as a factor")
          }
        }

        #integer: categorical
        if(is.na(instructions) & inherits(oneVariable, "integer") & (length(unique(oneVariable)) <= 61 & length(oneVariable) > 61)) {
          instructions <- paste0(variableName," = as.integer(sample(c('",paste0(unlist(unique(oneVariable)),collapse = "', '"),"'), ", number_of_observations,", replace = TRUE, prob = c(",paste0(prop.table(table(oneVariable, useNA = 'ifany')), collapse = ", "),")))", collapse = "")
          instructions <- gsub("'NA'", "NA", instructions)
          if(comments){
            instructions <- paste0(instructions, " # as a categorical non factor")
          }
        }

        #character: categorical
        if(is.na(instructions) & inherits(oneVariable, "character") & (length(unique(oneVariable)) <= 61 & length(oneVariable) > 61)) {
          instructions <- paste0(variableName," = sample(c('",paste0(unlist(unique(oneVariable)),collapse = "', '"),"'), ", number_of_observations,", replace = TRUE, prob = c(",paste0(prop.table(table(oneVariable, useNA = 'ifany')), collapse = ", "),"))", collapse = "")
          instructions <- gsub("'NA'", "NA", instructions)
          if(comments){

            instructions <- paste0(instructions, " # as a categorical non factor")
          }
        }

        #continuous integer
        if(is.na(instructions) & inherits(oneVariable, "integer") & (length(unique(oneVariable)) > 61 & length(oneVariable) > 61)) {
          #uniform distribution
          instructions <- paste0(variableName, " = as.integer(runif(", number_of_observations,", ", min(oneVariable, na.rm = TRUE), ", ", max(oneVariable, na.rm = TRUE),"))")
          if(comments){
            instructions <- paste0(instructions, " # continuous integer with uniform distribution")
          }
        }

        #continuous double
        if(is.na(instructions) & inherits(oneVariable, "double") & (length(unique(oneVariable)) > 61 & length(oneVariable) > 61)) {
          count_decimal_places <- function(x) {
            if (!is.numeric(x)) return(NA)
            sapply(x, function(num) {
              if (is.na(num)) return(NA)
              str_num <- as.character(num)
              if (grepl("\\.", str_num)) {
                return(nchar(strsplit(str_num, "\\.")[[1]][2]))
              } else {
                return(0)
              }
            })
          }
          oneVariable[,RH := count_decimal_places(oneVariable[[1]])]
          numberOfDecimals <- max(oneVariable$RH, na.rm = T)
          #uniform distribution
          instructions <- paste0(variableName, " = as.double(round(runif(", number_of_observations,", ", min(oneVariable, na.rm = TRUE), ", ", max(oneVariable, na.rm = TRUE),"),", numberOfDecimals , "))")
          if(comments){
            instructions <- paste0(instructions, " # continuous double with uniform distribution")
          }
        }

        #if unmatched
        if(is.na(instructions)) {

          instructions <- paste0("`",variableName,"`", " = NA")
          if(comments){
            instructions <- paste0(instructions, " # data type not modeled")
          }
        }

        if(is.na(instructions)) {

        } else{
          return(instructions)
        }
      }

      batch_variable_modeller <- function(x) {
        variable_modeller(ph.data[,..x][[1]], number_of_observations, names(ph.data)[x], comments = comments)
      }

      codeList <- lapply(seq_along(ph.data), batch_variable_modeller)

      if(comments) {

        codeListParsed <- c(list("DT <- data.table("),gsub(" #", ", #", codeList[1:(length(codeList)-1)]), gsub(" #",") #",codeList[length(codeList)]))

        codeText <- paste(unlist(codeListParsed), collapse =" \n" )

      } else {
        codeListParsed <- c(list("DT <- data.table("),paste0(codeList[1:(length(codeList)-1)], ","), paste0(codeList[length(codeList)], ")"))

        codeText <- paste(unlist(codeListParsed), collapse =" \n" )
      }

      if(return_code) {
        #codeText <- paste(unlist(codeList), collapse =" \n" )

        cat(codeText)
        return(codeList)

      } else {

        cat(codeText)
        eval( parse(text = paste0(codeText)))
      #  eval( parse(text = paste0("DT <- data.table(", codeText,collapse =  "")))
        return(DT)
      }

    }

#
#     ph.data <- get_data_birth(cols = c('race4', 'chi_age', 'hra20_name', 'sex', 'birth_weight_grams'))
#     ph.data <- get_data_birth(cols = c('race4', 'chi_age', 'hra20_name', 'sex', 'birth_weight_grams', "time_of_birth", "mother_birthplace_country" ))
#     #ph.data <- get_data_birth()
#     todo <- data_modeller(ph.data = ph.data, number_of_observations =  10000, return_code = F, comments = TRUE)
#
#     tada <- data_modeller(ph.data = ph.data, number_of_observations =  10000, return_code = T, comments = TRUE)
#
#     codeListParsed <- c(list("DT <- data.table("),gsub(" #", ", #", tada[1:(length(tada)-1)]), gsub(" #",") #",tada[length(tada)]))
#
#     codeText <- paste(unlist(codeListParsed), collapse =" \n" )
#
#     tada <- eval( parse(text = paste0(codeText)))
#
#
#
#     str(ph.data)
#     str(todo)
#     str(tada)
#

    ################################ end migrate this out ###########################################################

    generate_test_data <- function(dataset = "generic", observations = 100, seed = 1000, years = 2023){
      ### generates a synthetic data set appropriate for testing functions relying on APDE data structures and where you do not want to use real data
      ### receives description of data set to emulate, number of observations to include, a seed and number of years.
      ### returns a data.table of synthetic data. If dataset is "generic" the returned structure will have idealized chi values and generic indicators

      # input validation
      datasetOptions <- c("generic", "brfss", "death")
      dataset <- tolower(dataset)
      if(!(dataset %in% datasetOptions)) {
        stop(paste0("dataset must be one of: '", paste(datasetOptions, collapse = "', '"),"'"))
      }

      year_iterator <- function(observations, seed, years) {

      }

      if(dataset == "generic") {
        for(year in years) {
          seed <- seed*year
          DTIteration <- data.table(
            id = 1:observations,
            chi_geo_kc = sample(c(0,1), observations, replace = T),
            chi_race_7 = factor(sample(c("Asian", "AIAN", "Black", "Hispanic", "NHPI", "White", "Other", "Multiple", NA), observations, replace = T, prob = c(.19,.01,.07,.11,.01,.35,.07,.14,.02)), levels = c("Asian", "AIAN", "Black", "Hispanic", "NHPI", "White", "Other", "Multiple", NA)),
            chi_sex = as.factor(sample(c("Male","Female"), observations, replace = T)),
            chi_geo_region = factor(sample(c("South", "North", "Seattle", "East"), observations, replace = T), levels = c("South","North","Seattle","East")),
            indicator1 = as.factor(sample(c("never","sometimes", "always", NA), observations, replace = T)),
            indicator2 = as.factor(sample(c(1,2,3,4, NA), observations, replace = T)),
            indicator3 = as.factor(sample(c("<20","21-40","41-60","61<"),  observations, replace = T)),
            chi_year = year)
          if(exists("returnDT")) {
            returnDT <- rbind(returnDT, DTIteration)
          } else {
            returnDT <- DTIteration
          }
        }
      } else if(dataset == "death") {
        for(year in years) {
          seed <- seed*year
          DTIteration <- data.table(
            `state_file_number` = NA, # data type not modelled,
            `underlying_cod_code` = NA, # data type not modelled,
            age6 = sample(c('65-74', '75+', '25-44', '45-64', '<18', '18-24'), observations, replace = TRUE, prob = c(0.0107097718352957, 0.0114414953768376, 0.0644581919776492, 0.184660413756403, 0.184194771502694, 0.544535355551121)), # as a categorical non factor,
            bigcities = sample(c(NA, 'Seattle city', 'Auburn city', 'Kent city', 'Federal Way city', 'Bellevue city', 'Renton city', 'Kirkland city', 'Redmond city'), observations, replace = TRUE, prob = c(0.0491585179272268, 0.0566753143085213, 0.0538814607862702, 0.0679837690414422, 0.0367192177210138, 0.0190248120800905, 0.052351493381228, 0.249850329275594, 0.414355085478614)), # as a categorical non factor,
            `hra20_name` = NA, # data type not modelled,
            chi_sex = factor(sample(c('Female', 'Male', NA), observations, replace = TRUE, prob = c(0.468036985299009, 0.531763453735116, 0.000199560965875075)), levels = c('Female', 'Male'), ordered = FALSE), # as a factor,
            chi_geo_kc = sample(c('King County'), observations, replace = TRUE, prob = c(1)), # as a categorical non factor,
            pov200grp = sample(c(NA, 'Very high poverty areas', 'High poverty areas', 'Medium poverty areas', 'Low poverty areas'), observations, replace = TRUE, prob = c(0.287234750216191, 0.174283243530899, 0.24998336991951, 0.285771303133107, 0.00272733320029269)), # as a categorical non factor,
            race3 = factor(sample(c('White', 'Black', NA, 'Asian', 'Multiple', 'AIAN', 'NHPI'), observations, replace = TRUE, prob = c(0.0782278986230293, 0.763054613184328, 0.0178939666067984, 0.111355018958292, 0.0109758531231291, 0.0108428124792124, 0.0076498370252112)), levels = c('Black', 'White', 'Multiple', 'Asian', 'AIAN', 'NHPI'), ordered = FALSE), # as a factor,
            race4 = factor(sample(c('White', 'Hispanic', 'Black', NA, 'Asian', 'Multiple', 'AIAN', 'NHPI'), observations, replace = TRUE, prob = c(0.0771635734716956, 0.727200159648773, 0.0169626820993814, 0.110889376704583, 0.0108428124792124, 0.0391139493115147, 0.0103106499035455, 0.00751679638129449)), levels = c('Black', 'White', 'Multiple', 'Asian', 'NHPI', 'Hispanic', 'AIAN'), ordered = FALSE), # as a factor,
            chi_geo_region = sample(c(NA, 'Seattle', 'South', 'East', 'North'), observations, replace = TRUE, prob = c(0.204549990021952, 0.077230093793654, 0.287367790860108, 0.428124792123994, 0.00272733320029269)), # as a categorical non factor,
            wastate = sample(c('Washington State'), observations, replace = TRUE, prob = c(1)), # as a categorical non factor,
            `chi_age` = NA, # data type not modelled,
            chi_year = sample(c('2021'), observations, replace = TRUE, prob = c(1)), # as a categorical non factor,
            race3_hispanic = sample(c(NA, 'Hispanic'), observations, replace = TRUE, prob = c(0.0392469899554314, 0.960753010044569)) # as a categorical non factor
          )

          if(exists("returnDT")) {
            returnDT <- rbind(returnDT, DTIteration)
          } else {
            returnDT <- DTIteration
          }
        }
      } else if(dataset == "brfss") {
        for(year in years) {
          seed <- seed*year
          DTIteration <- data.table(
            chi_year = sample(c('2023'), observations, replace = TRUE, prob = c(1)), # as a categorical non factor,
            age = runif(observations, 18, 99), # continuous with uniform distribution,
            age5_v2 = factor(sample(c('25-44', '45-64', '18-24', '75+', '65-74'), observations, replace = TRUE, prob = c(0.0619025944469731, 0.366257017144591, 0.320285237445001, 0.149294492489759, 0.102260658473676)), levels = c('18-24', '25-44', '45-64', '65-74', '75+'), ordered = FALSE), # as a factor,
            chi_sex = factor(sample(c('Female', 'Male'), observations, replace = TRUE, prob = c(0.504779244424215, 0.495220755575785)), levels = c('Male', 'Female'), ordered = FALSE), # as a factor,
            race3 = factor(sample(c('White', NA, 'Asian', 'Black', 'Multiple', 'NHPI', 'AIAN'), observations, replace = TRUE, prob = c(0.715217721134881, 0.0567440449097254, 0.0115308754362009, 0.135639508420574, 0.00971021089364285, 0.0380822333485055, 0.0330754058564709)), levels = c('White', 'Black', 'AIAN', 'Asian', 'NHPI', 'Multiple', NA), ordered = FALSE), # as a factor,
            race4 = factor(sample(c('White', 'Hispanic', 'Asian', 'Multiple', 'Black', 'AIAN', NA, 'NHPI'), observations, replace = TRUE, prob = c(0.00652404794416629, 0.0511303292368381, 0.133667121832802, 0.00804126839629798, 0.0923987255348202, 0.662570171445911, 0.0364132908511607, 0.00925504475800334)), levels = c('AIAN', 'Black', 'Asian', 'NHPI', 'Hispanic', 'White', 'Multiple', NA), ordered = FALSE), # as a factor,
            hispanic = sample(c('0', '1', NA), observations, replace = TRUE, prob = c(0.896829009255045, 0.0923987255348202, 0.010772265210135)), # as a categorical non factor,
            income6b = factor(sample(c('$50-74,999', NA, '$100,000+', '$20-34,999', '$75-99,999', '<$20,000', '$35-49,999'), observations, replace = TRUE, prob = c(0.0399028978910636, 0.0696404187528448, 0.062661204673039, 0.101046882111971, 0.101805492338037, 0.441662873615536, 0.183280230617509)), levels = c('<$20,000', '$20-34,999', '$35-49,999', '$50-74,999', '$75-99,999', '$100,000+'), ordered = FALSE), # as a factor,
            sexorien = factor(sample(c('Something else', 'Straight', 'Lesbian/Gay', 'Bisexual'), observations, replace = TRUE, prob = c(0.887725686542255, 0.0342891822181763, 0.0523441055985435, 0.0256410256410256)), levels = c('Straight', 'Lesbian/Gay', 'Bisexual', 'Something else'), ordered = FALSE), # as a factor,
            trnsgndr = sample(c('0', '1'), observations, replace = TRUE, prob = c(0.990593233196783, 0.00940676680321651)), # as a categorical non factor,
            veteran3 = sample(c('0', '1', NA), observations, replace = TRUE, prob = c(0.922166590805644, 0.072523137611895, 0.00531027158246093)), # as a categorical non factor,
            asthnow = sample(c('0', '1', NA), observations, replace = TRUE, prob = c(0.896222121074192, 0.0951297223486573, 0.00864815657715066)), # as a categorical non factor,
            bphigh = sample(c('0', '1', NA), observations, replace = TRUE, prob = c(0.699135184342285, 0.296768320436959, 0.00409649522075558)), # as a categorical non factor,
            cholchk5 = sample(c('1', '0', NA), observations, replace = TRUE, prob = c(0.832802306175087, 0.0989227734789865, 0.0682749203459263)), # as a categorical non factor,
            x_crcrec = sample(c(NA), observations, replace = TRUE, prob = c(1)), # as a categorical non factor,
            x_crcrec2 = sample(c(NA), observations, replace = TRUE, prob = c(1)), # as a categorical non factor,
            cvdheart = sample(c('0', NA, '1'), observations, replace = TRUE, prob = c(0.948869670763162, 0.0432407828857533, 0.00788954635108481)), # as a categorical non factor,
            cvdstrk3 = sample(c('0', '1', NA), observations, replace = TRUE, prob = c(0.977393415263238, 0.0203307540585647, 0.00227583067819754)), # as a categorical non factor,
            denvst1 = sample(c(NA), observations, replace = TRUE, prob = c(1)), # as a categorical non factor,
            diab2 = sample(c('0', '1', NA), observations, replace = TRUE, prob = c(0.924594143529055, 0.0731300257927477, 0.00227583067819754)), # as a categorical non factor,
            exerany = sample(c('0', '1', NA), observations, replace = TRUE, prob = c(0.870581095433166, 0.128053406159915, 0.00136549840691853)), # as a categorical non factor,
            disab2 = sample(c('0', NA, '1'), observations, replace = TRUE, prob = c(0.777120315581854, 0.199969655590957, 0.0229100288271886)), # as a categorical non factor,
            ecignow1 = sample(c('3', NA, '2', '1'), observations, replace = TRUE, prob = c(0.0166894249734486, 0.0256410256410256, 0.920194204217873, 0.0374753451676529)), # as a categorical non factor,
            firearm4 = sample(c(NA), observations, replace = TRUE, prob = c(1)), # as a categorical non factor,
            flushot7 = sample(c('0', NA, '1'), observations, replace = TRUE, prob = c(0.567440449097254, 0.384615384615385, 0.0479441662873616)), # as a categorical non factor,
            fnotlast = sample(c('0', NA, '1'), observations, replace = TRUE, prob = c(0.846002124108633, 0.0726748596571082, 0.0813230162342588)), # as a categorical non factor,
            sdhfood1 = sample(c('5', NA, '1', '3', '2', '4'), observations, replace = TRUE, prob = c(0.00819299044151115, 0.00955848884842968, 0.0549233803671673, 0.0588681535427098, 0.787133970565923, 0.0813230162342588)), # as a categorical non factor,
            genhlth2 = sample(c('0', '1', NA), observations, replace = TRUE, prob = c(0.881201638598088, 0.116977696859354, 0.00182066454255803)), # as a categorical non factor,
            mam2yrs = sample(c(NA), observations, replace = TRUE, prob = c(1)), # as a categorical non factor,
            medcost1 = sample(c('0', '1', NA), observations, replace = TRUE, prob = c(0.914580488544986, 0.0819299044151115, 0.0034896070399029)), # as a categorical non factor,
            x_pastaer = sample(c('0', '1', NA), observations, replace = TRUE, prob = c(0.560309512972235, 0.298892429069944, 0.140798057957821)), # as a categorical non factor,
            fmd = sample(c('0', '1', NA), observations, replace = TRUE, prob = c(0.855257168866636, 0.130480958883326, 0.0142618722500379)), # as a categorical non factor,
            mjnow = sample(c('0', NA, '1'), observations, replace = TRUE, prob = c(0.767410104688211, 0.144135942952511, 0.0884539523592778)), # as a categorical non factor,
            obese = sample(c('0', NA, '1'), observations, replace = TRUE, prob = c(0.69458352298589, 0.207555757851616, 0.0978607191624943)), # as a categorical non factor,
            x_bmi5cat = factor(sample(c('Overweight', NA, 'Obese', 'Normal', 'Underweight'), observations, replace = TRUE, prob = c(0.018813533606433, 0.343043544226976, 0.332726445152481, 0.207555757851616, 0.0978607191624943)), levels = c('Underweight', 'Normal', 'Overweight', 'Obese'), ordered = FALSE), # as a factor,
            x_veglt1a = sample(c(NA), observations, replace = TRUE, prob = c(1)), # as a categorical non factor,
            crvscrnx = sample(c(NA), observations, replace = TRUE, prob = c(1)), # as a categorical non factor,
            persdoc3 = sample(c('1', '0', NA), observations, replace = TRUE, prob = c(0.861629494765589, 0.126687907752997, 0.011682597481414)), # as a categorical non factor,
            x_pneumo3 = sample(c(NA, '0', '1'), observations, replace = TRUE, prob = c(0.18236989834623, 0.0499165528751328, 0.767713548778638)), # as a categorical non factor,
            smoker1 = sample(c('0', NA, '1'), observations, replace = TRUE, prob = c(0.907449552419967, 0.0553785465028069, 0.0371719010772265)), # as a categorical non factor,
            finalwt1 = runif(observations, 9.1, 3698.3), # continuous with uniform distribution,
            x_ststr = sample(c('2023532011', '2023532012', '2023532082', '2023532021', '2023532022', '2023532031', '2023532032', '2023532131', '2023532121', '2023532161', '2023532151', '2023531141', '2023532061', '2023531142', '2023532091', '2023532112', '2023532081', '2023532071', '2023532042', '2023532122', '2023532051', '2023532072', '2023532062', '2023532101', '2023532102', '2023531271', '2023531231', '2023532052', '2023531241', '2023532111', '2023532092', '2023532041', '2023532141', '2023532132', '2023531161', '2023531301', '2023531211', '2023531242', '2023532142', '2023531202', '2023532019'), observations, replace = TRUE, prob = c(0.0581095433166439, 0.00364132908511607, 0.000151722045213169, 0.000151722045213169, 0.000151722045213169, 0.000151722045213169, 0.000151722045213169, 0.000151722045213169, 0.00121377636170536, 0.000151722045213169, 0.587922925201032, 0.210286754665453, 0.000151722045213169, 0.0136549840691853, 0.00804126839629798, 0.0127446517979062, 0.0101653770292824, 0.00106205431649219, 0.000606888180852678, 0.00136549840691853, 0.000910332271279017, 0.00242755272341071, 0.0019723865877712, 0.00151722045213169, 0.000910332271279017, 0.00182066454255803, 0.00652404794416629, 0.000606888180852678, 0.000758610226065847, 0.00166894249734486, 0.000606888180852678, 0.000455166135639508, 0.000758610226065847, 0.000606888180852678, 0.0019723865877712, 0.00242755272341071, 0.000606888180852678, 0.000606888180852678, 0.000303444090426339, 0.0453648915187377, 0.0171445911090881)), # as a categorical non factor,
            hra20_id_1 = runif(observations, 1, 61), # continuous with uniform distribution,
            hra20_id_2 = runif(observations, 1, 61), # continuous with uniform distribution,
            hra20_id_3 = runif(observations, 1, 61), # continuous with uniform distribution,
            hra20_id_4 = runif(observations, 1, 61), # continuous with uniform distribution,
            hra20_id_5 = runif(observations, 1, 61), # continuous with uniform distribution,
            hra20_id_6 = runif(observations, 1, 61), # continuous with uniform distribution,
            hra20_id_7 = runif(observations, 1, 61), # continuous with uniform distribution,
            hra20_id_8 = runif(observations, 1, 61), # continuous with uniform distribution,
            hra20_id_9 = runif(observations, 1, 61), # continuous with uniform distribution,
            hra20_id_10 = runif(observations, 1, 61), # continuous with uniform distribution,
            default_wt = runif(observations, 9.1, 3698.3), # continuous with uniform distribution,
            `_id` = NA, # data type not modelled,
            chi_geo_region = sample(c(NA, 'South', 'East', 'North', 'Seattle'), observations, replace = TRUE, prob = c(0.230010620543165, 0.0650887573964497, 0.330602336519496, 0.284175390684266, 0.0901228948566227)) # as a categorical non factor
          )

          if(exists("returnDT")) {
            returnDT <- rbind(returnDT, DTIteration)
          } else {
            returnDT <- DTIteration
          }
        }
      } else if(dataset == "skeleton") {
        for(year in years) {
          seed <- seed*year
          DTIteration <- data.table(
            #paste data modelling code here
          )

          if(exists("returnDT")) {
            returnDT <- rbind(returnDT, DTIteration)
          } else {
            returnDT <- DTIteration
          }
        }
      }
      return(returnDT)
    }

    test_data_generic <- generate_test_data("generic", 100, 1000, c(2016:2023))
    test_data_brfss <- generate_test_data("brfss", 100, 1000, c(2016:2023))
    test_data_death <- generate_test_data("death", 100, 1000, c(2016:2023))

    test_analysis_set_twosets <- data.table(
      #this should work with the generic data set
      cat1 = rep(c('Regions', 'Gender', 'Race/ethnicity'),2),
      cat1_varname = rep(c('chi_geo_region', 'chi_sex', 'chi_race_7'),2),
      `_kingcounty` = c('x'),
      `_wastate` = NA_character_,
      demgroups = c(rep(NA_character_,3),rep("x", 3)),
      crosstabs = c(rep(NA_character_,3),rep("x", 3)),
      trends = c(rep(NA_character_,3),rep("x", 3)),
      set = c(rep(1,3), rep(2,3)),
      set_indicator_keys = c(rep(c('indicator1, indicator2'),3), rep("indicator3",3))
    )

    # create twoset analysis set
    #remove("test_twoset_estimates")
    for(indicator in c("indicator1","indicator2")) {
      partialDT <- data.table(
        indicator = indicator,
        tab = c(rep('demgroups', 4), '_kingcounty'),
        year = c('2023'),
        cat1 = c('Region', 'Region', 'Region', 'Region', 'King County'),
        cat1_group = c("East", "North", "Seattle", "South", 'King County'),
        cat1_varname = c('chi_geo_region', 'chi_geo_region', 'chi_geo_region', 'chi_geo_region', 'chi_geo_kc'),
        cat2 = NA_character_,
        cat2_group = NA_character_,
        cat2_varname = NA_character_,
        data_source = 'JustTesting',
        caution = NA_character_,
        suppression = NA_character_,
        chi = 1,
        source_date = Sys.Date(),
        run_date = Sys.Date(),
        numerator = c(111, 175, 210, 600, 430000),
        denominator = c(1000, 1500, 2000, 2500, 2200000)
      )
      if(exists("test_twoset_estimates")) {
        test_twoset_estimates <- rbind(test_twoset_estimates, partialDT)
    } else {
      test_twoset_estimates <- partialDT
    }
    }
    test_twoset_estimates[, result := numerator / denominator]
    test_twoset_estimates[, se := sqrt((result * (1-result)) / denominator)]
    test_twoset_estimates[, rse := 100 * se / result]
    test_twoset_estimates[, lower_bound := result - 1.96 * se]
    test_twoset_estimates[, upper_bound := result + 1.96 * se]



  # Sample instructions ----
    test_instructions <- data.table(
      indicator_key = c("indicator1", "indicator2", "indicator1", "indicator2"),
      tab = c("demgroups", "demgroups", "_kingcounty", "_kingcounty"),
      cat1 = c("Regions", "Gender", "King County", "King County"),
      cat1_varname = c("chi_geo_region", "chi_sex", "chi_geo_kc", "chi_geo_kc"),
      cat2 = NA_character_,
      cat2_varname = NA_character_,
      start = c(2019),
      end = c(2024)
    )

  # Sample estimates ----
    test_estimates <- data.table(
      indicator_key = c("indicator1"),
      tab = c(rep('demgroups', 4), '_kingcounty'),
      year = c('2023'),
      cat1 = c('Region', 'Region', 'Region', 'Region', 'King County'),
      cat1_group = c("East", "North", "Seattle", "South", 'King County'),
      cat1_varname = c('chi_geo_region', 'chi_geo_region', 'chi_geo_region', 'chi_geo_region', 'chi_geo_kc'),
      cat2 = NA_character_,
      cat2_group = NA_character_,
      cat2_varname = NA_character_,
      data_source = 'JustTesting',
      caution = NA_character_,
      suppression = NA_character_,
      chi = 1,
      source_date = Sys.Date(),
      run_date = Sys.Date(),
      numerator = c(111, 175, 210, 600, 430000),
      denominator = c(1000, 1500, 2000, 2500, 2200000)
    )
    test_estimates[, result := numerator / denominator]
    test_estimates[, se := sqrt((result * (1-result)) / denominator)]
    test_estimates[, rse := 100 * se / result]
    test_estimates[, lower_bound := result - 1.96 * se]
    test_estimates[, upper_bound := result + 1.96 * se]


    test_estimates_old <- data.table(
      indicator_key = c("indicatorX"),
      tab = c(rep('demgroups', 4), '_kingcounty'),
      year = c('2022'),
      cat1 = c('Region', 'Region', 'Region', 'Region', 'King County'),
      cat1_group = c("East", "North", "Seattle", "South", 'King County'),
      cat1_varname = c('chi_geo_region', 'chi_geo_region', 'chi_geo_region', 'chi_geo_region', 'chi_geo_kc'),
      cat2 = NA_character_,
      cat2_group = NA_character_,
      cat2_varname = NA_character_,
      data_source = 'JustTesting',
      caution = NA_character_,
      suppression = NA_character_,
      chi = 1,
      source_date = Sys.Date(),
      run_date = Sys.Date(),
      numerator = c(113, 177, 209, 400, 460000),
      denominator = c(1000, 1500, 2000, 2500, 2300000)
    )
    test_estimates_old[, result := numerator / denominator]
    test_estimates_old[, se := sqrt((result * (1-result)) / denominator)]
    test_estimates_old[, rse := 100 * se / result]
    test_estimates_old[, lower_bound := result - 1.96 * se]
    test_estimates_old[, upper_bound := result + 1.96 * se]

  # Sample metadata ----
    test_metadata <- data.table(
      indicator_key = c("indicatorX"),
      result_type = c("proportion"),
      valid_years = c("2020 2021 2022 2022"),
      latest_year = c(2022),
      data_source = 'test',
      valence = 'positive',
      latest_year_result = 0.20,
      latest_year_kc_pop = 2300000,
      latest_year_count = 460000,
      map_type = 'hra',
      unit = 'person',
      chi = 1,
      run_date = Sys.Date()
      )

  # Return ----
  list(my.analytic = test_analytic,
       my.analysis_set = test_analysis_set,
       my.analysis_set_twosets = test_analysis_set_twosets,
       my.generic_data = test_data_generic,
       my.brfss_data = test_data_brfss,
       my.death_data = test_data_death,
       my.estimate= test_estimates,
       my.estimate_old= test_estimates_old,
       my.metadata = test_metadata,
       my.instructions = test_instructions)
}
