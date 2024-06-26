#' @title Diabetes Data
#'
#' @description These data are courtesy of Dr John Schorling,
#' Department of Medicine, University of Virginia School of Medicine.
#'
#'     The data contains information on 403 subjects from 1046
#' subjects who were interviewed in a study to understand the
#' prevalence of obesity, diabetes, and other cardiovascular risk
#' factors in central Virginia for African Americans. According to
#' Dr John Hong, Diabetes Mellitus Type II (adult onset diabetes) is
#' associated most strongly with obesity. The waist/hip ratio may be a
#' predictor in diabetes and heart disease. DM II is also associated
#' with hypertension - they may both be part of "Syndrome X". The 403
#' subjects were the ones who were actually screened for diabetes.
#' Glycosylated hemoglobin > 7.0 is usually taken as a positive diagnosis
#' of diabetes.
#'
#'
#'
#' @format A data frame with 403 rows and 22 variables (See "Note"):
#' \describe{
#'   \item{id}{Subject id}
#'   \item{chol}{Total cholesterol}
#'   \item{stab.glu}{Stabilized glucose}
#'   \item{hdl}{High density lipoprotein}
#'   \item{ratio}{Cholesterol/hdl ratio}
#'   \item{glyhb}{Glycosylated hemoglobin}
#'   \item{location}{A factor with levels \code{Buckingham} and \code{Louisa}}
#'   \item{age}{Age (years)}
#'   \item{gender}{Gender, \code{male} or \code{female}}
#'   \item{height}{Height (inches)}
#'   \item{weight}{Weight (pounds)}
#'   \item{frame}{A factor with levels \code{small},
#'   \code{medium} and \code{large}}
#'   \item{bp.1s}{First systolic blood pressure}
#'   \item{bp.1d}{First diastolic blood pressure}
#'   \item{bp.2s}{Second systolic blood pressure}
#'   \item{bp.2d}{Second diastolic blood pressure}
#'   \item{waist}{Waist (inches)}
#'   \item{hip}{Hip (inches)}
#'   \item{time.ppn}{Postprandial time when labs were
#'   drawn in minutes}
#'   \item{bmi}{Body mass index}
#'   \item{dtest}{An indicator whether \code{glyhb} is greater than 7 or not}
#'   \item{whr}{Waist to hip ratio}
#'  }
#'
#' @note The last three variables (\code{bmi}, \code{dtest}, \code{whr})
#' were created. For \code{bmi}, following formula was used:
#' \deqn{bmi = 703 * (weight_lbs) / (height_inches)^2}
#'
#'
#' @references Willems, James P., J. Terry Saunders, Dawn E. Hunt, and
#' John B. Schorling. "Prevalence of coronary heart disease risk factors
#' among rural blacks: a community-based study." \emph{Southern medical journal}
#' 90, no. 8 (1997): 814-820.
#'
#' @references Schorling, John B., Julienne Roach, Marjorie Siegel,
#' Natalie Baturka, Dawn E. Hunt, Thomas M. Guterbock, and Herbert
#' L. Stewart. "A trial of church-based smoking cessation interventions
#' for rural African Americans." \emph{Preventive Medicine} 26, no. 1
#' (1997): 92-101.
#'
#'
#' @examples
#' data("Diabetes")
#' plot(Diabetes$hdl~Diabetes$weight, pch = 16,
#'        col =ifelse(Diabetes$gender=="male",1,2))
#' #------------------------------------------
#' ## density plot
#' femaleBMI <- density(subset(Diabetes, gender == "female")$bmi, na.rm = TRUE)
#' maleBMI <- density(subset(Diabetes, gender == "male")$bmi, na.rm = TRUE)
#' ## -------
#' plot(NULL, ylim = c(0,0.08), xlim = c(10,60),
#'      xlab = "BMI", ylab = "Density", main = "")
#' grid(col = 1)
#' polygon(maleBMI, col = rgb(0,0,1,0.2), border = 4)
#' polygon(femaleBMI, col = rgb(1,0,0,0.2), border = 2)
#' abline(h = 0)
#' legend("topright", c("Male", "Female"), pch = 15,
#'        col = c(rgb(0,0,1,0.2), rgb(1,0,0,0.2)), bty = "n")
#' #------------------------------------------
#' logistic.model <- glm(as.factor(dtest)~chol+age+bmi,
#'                       data = Diabetes,family = "binomial")
#' summary(logistic.model)
#' #------------------------------------------
#' class <- logistic.model$y
#' score <- logistic.model$fitted.values
#' rocit_object <- rocit(score = score, class = class)
#' summary(rocit_object)
#' plot(rocit_object)


#' @source staff.pubhealth.ku.dk/~tag/Teaching/share/data/Diabetes.html#sec-2
#'
"Diabetes"
