context("Checking the BODextract output length",{
        expect_equal(length(BODextract(sample(0:10,340,replace=TRUE))),2)
        expect_equal(length(BODextract(sample(0:10,340,replace=TRUE))$RV),11)
        expect_equal(length(BODextract(sample(0:10,340,replace=TRUE))$Freq),11)
        })

context("Alcohol Data",{
        expect_equal(Alcohol_data$Days,seq(0,7))
        expect_equal(Alcohol_data$week1,c(47,54,43,40,40,41,39,95))
        expect_equal(Alcohol_data$week2,c(42,47,54,40,49,40,43,84))
        })

context("Plant Disease Data",{
        expect_equal(Plant_DiseaseData$Dis.plant,seq(0,9))
        expect_equal(Plant_DiseaseData$fre,c(36,48,38,23,10,3,1,1,0,0))
        })

context("Course Data",{
        expect_equal(Course_data$sub.pass,seq(0,8))
        expect_equal(Course_data$fre,c(1,4,4,8,9,6,8,12,13))
        })

context("Chromosome data",{
        expect_equal(Chromosome_data$No.of.Asso,seq(0,3))
        expect_equal(Chromosome_data$fre,c(32,103,122,80))
        })

context("Exam Data",{
        expect_equal(Exam_data$No.of.alpha,seq(0,9),seq(0,9))
        expect_equal(Exam_data$fre,c(63,67,34,18,11,8,4,3,1,0))
        })

context("Terror Data Argentina",{
        expect_equal(Terror_data_ARG$Incidents,seq(0,6))
        expect_equal(Terror_data_ARG$fre,c(46,15,5,3,5,1,1))
        })

context("Terror Dara USA",{
        expect_equal(Terror_data_USA$Incidents,seq(0,5))
        expect_equal(Terror_data_USA$fre,c(38,26,8,2,1,1))
        })
