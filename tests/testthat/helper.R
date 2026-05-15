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

      if(dataset == "generic") {
        for(year in years) {
          seed <- seed*year
          DTIteration <- data.table(
            id = 1:observations,
            chi_geo_kc = sample(c('King County',NA_character_), observations, replace = T),
            race4 = factor(sample(c("Asian", "AIAN", "Black", "Hispanic", "NHPI", "White", "Multiple", NA), observations, replace = T, prob = c(.19,.01,.07,.11,.01,.42,.14,.02)), levels = c("Asian", "AIAN", "Black", "Hispanic", "NHPI", "White", "Other", "Multiple", NA)),
            chi_sex = as.factor(sample(c("Male","Female"), observations, replace = T)),
            chi_geo_region = factor(sample(c("South", "North", "Seattle", "East"), observations, replace = T), levels = c("South","North","Seattle","East")),
            indicator1 = as.factor(sample(c("never","sometimes", "always", NA), observations, replace = T)),
            indicator2 = as.factor(sample(c(1,2,3,4, NA), observations, replace = T)),
            indicator3 = as.factor(sample(c("<20","21-40","41-60","61<"),  observations, replace = T)),
            chi_year = year,
            wastate = 'Washington State')
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
            `death_zip_code` = sample(c(NA, "98104", "98034", "98112", "98105", "98030", "98108", "98058", "98004", "98002", "98166", "98075", "98006", "98198", "98372", "98033", "98029", "98031", "98106", "98024", "98055", "98122", "98072", "98023", "98109", "98177", "98195", "98144", "98133", "98134", "98119", "98070", "98107", "98936", "98026", "98020", "98037", "98043", "98021", "98208", "98036", "98363", "98087", "99362", "98807", "98201", "98012", "98942", "98922", "98272", "98926", "98203", "98252", "98848", "99216", "98275", "98271", "98382", "98204", "98816", "98801", "99005", "98405", "98118", "98008", "98115", "98042", "98014", "98146", "98168", "98116", "98032", "98028", "98007", "98057", "98155", "98003", "98188", "98056", "98047", "98052", "98125", "98059", "98178", "98005", "98126", "98101", "98102", "98065", "98103", "98053", "98424", "98117", "98038", "98011", "98148", "98408", "98401", "98199", "98027", "98045", "98121", "98001", "98019", "98409", "98466", "98664", "98074", "98193", "98225", "98040", "98022", "98383", "98360", "98239", "98114", "98092", "98354", "98498", "98513", "98584", "98391", "98051", "98209", "98374", "98136", "98404", "98370", "98957", "98039", "98229", "98569", "98506", "98371", "98221", "98604", "98366", "98499", "98502", "98010", "98367", "98077", "98274", "98531", "98444", "98642", "98467", "98050", "98226", "98422", "98095", "99403", "98493", "98273", "99109", "98158", "99350", "98224", "98332", "89058", "98304", "98528", "99999", "98083", "98406", "98312", "98215", "98682", "98248", "9001", "98233", "98256", "98110", "98164", "98025", "98321", "98940", "98368", "99202", "98223", "98686", "99352", "98390", "98310", "98192", "98373", "98431", "98230", "98329", "98375", "98251", "98035", "98407", "98249", "98270", "99204", "99207", "98387", "98335", "98520", "98532", "98333", "98111", "98260", "98503", "98013", "98465", "98683", "98737", "98258", "98267", "98597", "98068", "99217", "98296", "99205", "98113", "98250", "98257", "98181", "98277", "98847", "98433", "98826", "98418", "98902", "98328", "98579", "98901", "98586", "98320"),
                                      observations , replace = TRUE, prob = c(6.66888962987663e-05, 6.66888962987663e-05, 0.0130043347782594, 0.0298099366455485, 0.0391463821273758, 0.042214071357119, 0.0057352450816939, 0.00786928976325442, 0.00726908969656552, 0.00746915638546182, 0.00146715571857286, 0.0102034011337112, 0.000800266755585195, 6.66888962987663e-05, 0.00140046682227409, 0.00206735578526175, 0.0053351117039013, 0.00106702234078026, 0.0130710236745582, 0.0136045348449483, 0.00153384461487162, 6.66888962987663e-05, 0.00180060020006669, 0.00906968989663221, 0.00486828942980994, 0.0204068022674225, 0.0113371123707903, 0.0101367122374125, 0.00906968989663221, 0.0226742247415805, 0.0382794264754918, 0.000133377792597533, 0.000800266755585195, 0.00113371123707903, 0.00580193397799266, 0.000466822274091364, 0.00753584528176059, 0.0132710903634545, 0.000666888962987663, 0.00446815605201734, 0.000866955651883961, 6.66888962987663e-05, 0.00100033344448149, 0.0148716238746249, 0.00440146715571857, 0.0660220073357786, 0.0112037345781927, 0.0059353117705902, 0.0120040013337779, 0.00706902300766922, 0.00140046682227409, 6.66888962987663e-05, 0.00400133377792598, 0.00426808936312104, 0.00260086695565188, 0.00293431143714572, 0.00106702234078026, 6.66888962987663e-05, 0.000600200066688896, 0.00926975658552851, 6.66888962987663e-05, 0.0178726242080694, 0.00406802267422474, 0.0106702234078026, 0.0441480493497833, 0.00866955651883961, 0.00526842280760253, 0.00666888962987663, 0.00893631210403468, 0.00786928976325442, 0.000266755585195065, 6.66888962987663e-05, 0.0302767589196399, 6.66888962987663e-05, 6.66888962987663e-05, 0.00933644548182728, 0.00806935645215072, 0.00440146715571857, 0.0215405135045015, 0.00526842280760253, 0.005135045015005, 0.0264088029343114, 0.0112704234744915, 0.00780260086695565, 0.0436145381793931, 0.00126708902967656, 0.00380126708902968, 0.0105368456152051, 0.00806935645215072, 0.00413471157052351, 0.0156052017339113, 0.000133377792597533, 6.66888962987663e-05, 0.0198066022007336, 0.0105368456152051, 0.00693564521507169, 0.00706902300766922, 0.000133377792597533, 0.00793597865955318, 6.66888962987663e-05, 6.66888962987663e-05, 0.0208736245415138, 0.016538846282094, 0.00446815605201734, 0.00280093364454818,
                                                                              0.000333444481493831, 0.000666888962987663, 0.000400133377792598, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 0.000133377792597533, 6.66888962987663e-05, 0.000400133377792598, 0.000266755585195065, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 0.000200066688896299, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 0.000133377792597533, 0.000200066688896299, 6.66888962987663e-05, 0.000133377792597533, 6.66888962987663e-05, 6.66888962987663e-05, 0.000133377792597533, 0.000133377792597533, 0.000466822274091364, 0.000200066688896299, 0.000133377792597533, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 0.000133377792597533, 0.000200066688896299, 6.66888962987663e-05, 0.000133377792597533, 0.000133377792597533, 6.66888962987663e-05, 0.000466822274091364, 6.66888962987663e-05, 0.000200066688896299, 0.00100033344448149, 0.000400133377792598, 6.66888962987663e-05, 0.000266755585195065, 0.000133377792597533, 6.66888962987663e-05, 0.000333444481493831, 0.000333444481493831, 0.00453484494831611, 6.66888962987663e-05, 0.00053351117039013, 6.66888962987663e-05, 6.66888962987663e-05, 0.000266755585195065, 6.66888962987663e-05, 6.66888962987663e-05, 0.000666888962987663, 0.00526842280760253, 0.000333444481493831, 0.00760253417805935, 0.000133377792597533, 0.000333444481493831, 0.000600200066688896, 0.000466822274091364, 6.66888962987663e-05, 0.000133377792597533, 0.000400133377792598, 0.000200066688896299, 6.66888962987663e-05, 0.000200066688896299, 6.66888962987663e-05, 0.00406802267422474, 0.000200066688896299, 0.000400133377792598, 0.000200066688896299, 0.000400133377792598, 0.000266755585195065, 6.66888962987663e-05, 0.000400133377792598, 6.66888962987663e-05, 6.66888962987663e-05,
                                                                              6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 0.000133377792597533, 6.66888962987663e-05, 0.000400133377792598, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 0.000200066688896299, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 0.000333444481493831, 0.000200066688896299, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 0.000133377792597533, 6.66888962987663e-05, 0.000133377792597533, 6.66888962987663e-05, 0.000133377792597533, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 6.66888962987663e-05, 0.000133377792597533, 6.66888962987663e-05, 0.000600200066688896, 0.014271423807936)), # as categorical character (non factor)
            `underlying_cod_code` = NA, # data type not modelled,
            age6 = sample(c('65-74', '75+', '25-44', '45-64', '<18', '18-24'), observations, replace = TRUE, prob = c(0.0107097718352957, 0.0114414953768376, 0.0644581919776492, 0.184660413756403, 0.184194771502694, 0.544535355551121)), # as a categorical non factor,
            bigcities = sample(c(NA, 'Seattle city', 'Auburn city', 'Kent city', 'Federal Way city', 'Bellevue city', 'Renton city', 'Kirkland city', 'Redmond city'), observations, replace = TRUE, prob = c(0.0491585179272268, 0.0566753143085213, 0.0538814607862702, 0.0679837690414422, 0.0367192177210138, 0.0190248120800905, 0.052351493381228, 0.249850329275594, 0.414355085478614)), # as a categorical non factor,
            `hra20_name` = NA, # data type not modelled,
            chi_sex = factor(sample(c('Female', 'Male', NA), observations, replace = TRUE, prob = c(0.468036985299009, 0.531763453735116, 0.000199560965875075)), levels = c('Female', 'Male'), ordered = FALSE), # as a factor,

            chi_geo_kc = sample(c('King County'), observations, replace = TRUE, prob = c(1)), # as a categorical non factor,
            pov200grp = sample(c(NA, 'Very high poverty areas', 'High poverty areas', 'Medium poverty areas', 'Low poverty areas'), observations, replace = TRUE, prob = c(0.287234750216191, 0.174283243530899, 0.24998336991951, 0.285771303133107, 0.00272733320029269)), # as a categorical non factor,
            race3 = factor(sample(c('White', 'Black', NA, 'Asian', 'Multiple', 'AIAN', 'NHPI'), observations, replace = TRUE, prob = c(0.0782278986230293, 0.763054613184328, 0.0178939666067984, 0.111355018958292, 0.0109758531231291, 0.0108428124792124, 0.0076498370252112)), levels = c('Black', 'White', 'Multiple', 'Asian', 'AIAN', 'NHPI'), ordered = FALSE), # as a factor,
            race4 = factor(sample(c('White', 'Hispanic', 'Black', NA, 'Asian', 'Multiple', 'AIAN', 'NHPI'), observations, replace = TRUE, prob = c(0.0771635734716956, 0.727200159648773, 0.0169626820993814, 0.110889376704583, 0.0108428124792124, 0.0391139493115147, 0.0103106499035455, 0.00751679638129449)), levels = c('Black', 'White', 'Multiple', 'Asian', 'NHPI', 'Hispanic', 'AIAN'), ordered = FALSE), # as a factor,
            malignant_neoplasm = sample(c(0,1), observations, replace = TRUE, prob = c(.65, .35)),
            chi_geo_region = sample(c(NA, 'Seattle', 'South', 'East', 'North'), observations, replace = TRUE, prob = c(0.204549990021952, 0.077230093793654, 0.287367790860108, 0.428124792123994, 0.00272733320029269)), # as a categorical non factor,
            wastate = sample(c('Washington State'), observations, replace = TRUE, prob = c(1)), # as a categorical non factor,
            `chi_age` = pmax(0,pmin(100,round(rnorm(observations, mean = 50, sd = 25)))), # integer with normal distribution
            chi_year = year,
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
            chi_year = year,
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

    test_data_generic <- generate_test_data("generic", 10000, 1000, c(2016:2023))
    test_data_brfss <- generate_test_data("brfss", 10000, 1000, c(2016:2023))
    test_data_death <- generate_test_data("death", 10000, 1000, c(2016:2023))

    test_analysis_set_twosets <- data.table(
      #this should work with the generic data set
      cat1 = c(rep(c('Regions', 'Gender', 'Race/ethnicity'),2), 'Washington State'),
      cat1_varname = c(rep(c('chi_geo_region', 'chi_sex', 'race4'),2), 'wastate'),
      `_kingcounty` = c(rep('x', 6), NA_character_),
      `_wastate` = c(rep(NA_character_, 6), 'x'),
      demgroups = c(rep(NA_character_,3),rep("x", 3), NA_character_),
      crosstabs = c(rep(NA_character_,3),rep("x", 3), NA_character_),
      trends = c(rep(NA_character_,3),rep("x", 3), NA_character_),
      set = c(rep(1,3), rep(2,3), 2),
      set_indicator_keys = c(rep(c('indicator1, indicator2'),3), rep("indicator3",4))
    )

    # create twoset analysis set
    #not currently exported, may not be needed
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
        source_date = as.Date("2025-01-01"),
        run_date = as.Date("2025-01-01"),
        numerator = c(111, 175, 210, 600, 430000),
        denominator = c(1000, 1500, 2000, 2500, 2200000)
      )
      if(exists("test_twoset_estimates")) {
        test_twoset_estimates <- rbind(test_twoset_estimates, partialDT)
      } else {
        test_twoset_estimates <- partialDT
      }
    }
    partialDT <- data.table(
      indicator = "indicator3",
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
      source_date = as.Date("2025-01-01"),
      run_date = as.Date("2025-01-01"),
      numerator = c(111, 175, 210, 600, 430000),
      denominator = c(1000, 1500, 2000, 2500, 2200000)
    )
    test_twoset_estimates <- rbind(test_twoset_estimates, partialDT)

    test_twoset_estimates[, result := numerator / denominator]
    test_twoset_estimates[, se := sqrt((result * (1-result)) / denominator)]
    test_twoset_estimates[, rse := 100 * se / result]
    test_twoset_estimates[, lower_bound := result - 1.96 * se]
    test_twoset_estimates[, upper_bound := result + 1.96 * se]


    #twoset metadata should work with with the "generic" dataset
    test_twoset_metadata <- data.table(
      indicator_key = c("indicator1", "indicator2","indicator3"),
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
      run_date = as.Date("2025-01-01")
    )


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
      indicator_key = c("indicatorX"),
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
      source_date = as.Date("2025-01-01"),
      run_date = as.Date("2025-01-01"),
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
      run_date = as.Date("2024-01-01")
      )

    validate_hhsaw_connection <- function(hhsaw_key = 'hhsaw'){
      # Key should be a character string that can be used to generate a database connection
      # Also have to allow for the option of interactive authentication
      # TODO: Allow hhsaw_key to be a database connection itself
      is.db = function(x){
        r = try(dbIsValid(hhsaw_key))
        if(inherits(r, 'try-error')){
          r = FALSE
        }
        r
      }
      status <- 0
      closeserver = TRUE
      if(is.character(hhsaw_key)){
        server <- grepl('server', tolower(Sys.info()['release']))
        trykey <- try(keyring::key_get(hhsaw_key, keyring::key_list(hhsaw_key)[['username']]), silent = T)
        if (inherits(trykey, "try-error")) warning(paste0("Your hhsaw keyring is not properly configured or you are not connected to the VPN. \n",
                                                       "Please check your VPN connection and or set your keyring and run the function again. \n",
                                                       paste0("e.g., keyring::key_set('hhsaw', username = 'ALastname@kingcounty.gov') \n"),
                                                       "When prompted, be sure to enter the same password that you use to log into to your laptop. \n",
                                                       "If you already have an hhsaw key on your keyring with a different name, you can specify it with the 'mykey = ...' or 'hhsaw_key = ...' argument \n"))
        rm(trykey)

        if(server == FALSE){
          con <- try(con <- DBI::dbConnect(odbc::odbc(),
                                           driver = getOption('rads.odbc_version'),
                                           server = 'kcitazrhpasqlprp16.azds.kingcounty.gov',
                                           database = 'hhs_analytics_workspace',
                                           uid = keyring::key_list(hhsaw_key)[["username"]],
                                           pwd = keyring::key_get(hhsaw_key, keyring::key_list(hhsaw_key)[["username"]]),
                                           Encrypt = 'yes',
                                           TrustServerCertificate = 'yes',
                                           Authentication = 'ActiveDirectoryPassword'), silent = T)
          if (inherits(con, "try-error")) warning(paste("Either your computer is not connected to KC systems (e.g. VPN is not connected), your hhsaw key is not properly configured, and/or your key value is outdated.",
                                                     "To (re)set your hhsaw key use keyring::key_set('", hhsaw_key, "', username = 'ALastname@kingcounty.gov')"),
                                               "When prompted, be sure to enter the same password that you use to log into to your laptop.")
        }else{
          message(paste0('Please enter the password you use for your laptop into the pop-up window. \n',
                         'Note that the pop-up may be behind your Rstudio session. \n',
                         'You will need to use your two factor authentication app to confirm your KC identity.'))
          con <- DBI::dbConnect(odbc::odbc(),
                                driver = getOption('rads.odbc_version'),
                                server = "kcitazrhpasqlprp16.azds.kingcounty.gov",
                                database = "hhs_analytics_workspace",
                                uid = keyring::key_list(hhsaw_key)[["username"]],
                                Encrypt = "yes",
                                TrustServerCertificate = "yes",
                                Authentication = "ActiveDirectoryInteractive")
          status <- 1
        }

        # on.exit(DBI::dbDisconnect(con))

      }else if(is.db(hhsaw_key)){
        closeserver = FALSE
        con = hhsaw_key
        status <- 1
      }else{
        warning('`hhsaw_key` is not a reference to database connection or keyring')
      }

      return(status)

    }

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
       my.instructions = test_instructions,
       my.hhsaw_status_test = validate_hhsaw_connection)
}
