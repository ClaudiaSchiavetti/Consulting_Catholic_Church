# ---- Load Required Libraries ----
# This section installs and loads all necessary packages for the Shiny app.
# It checks if each package is installed and installs it if not, then loads them.
required_packages <- c(
  "shiny", "leaflet", "dplyr", "readr", "sf", "DT", "shinythemes", 
  "rnaturalearth", "rnaturalearthdata", "RColorBrewer", "mapview", 
  "webshot", "writexl", "plotly", "shinyjs"
)
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Ensure PhantomJS is installed for webshot functionality (used for map downloads).
if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs()
}

# Load the libraries after ensuring they are installed.
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(sf)
library(DT)
library(shinythemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridisLite)
library(ggplot2)
library(RColorBrewer)
library(webshot)
library(writexl)
library(plotly)
library(shinyjs)
useShinyjs()

# ---- Docker Instructions ----
# Set Shiny app to listen on all interfaces and a specific port for Docker compatibility.
options(shiny.host = "0.0.0.0")
options(shiny.port = 3838) # Also 8180 is a valid option

# ---- Set the Working Directory ----
# Define the path to the data and set it as the working directory.
path_outputs <- "C:/Users/schia/Documents/LMU/Consulting/App"
# path_outputs <- "C:\\Users\\soffi\\Desktop\\CONSULTING"
setwd(path_outputs)

# ---- Load the Data ----
# Read the CSV file containing the geographic data, preserving original column names.
data <- read.csv("final_geo_table.csv", check.names = FALSE)

# ---- Define Variable Abbreviations ----
# Create a named vector for abbreviated variable names used in displays like histograms and legends.
variable_abbreviations <- c(
  "Inhabitants in thousands" = "Pop. (thousands)",
  "Catholics in thousands" = "Cath. (thousands)",
  "Area in km^2" = "Area (km²)",
  "Inhabitants per km^2" = "Pop. per km²",
  "Catholics per 100 inhabitants" = "Cath. per 100 Pop.",
  "Ecclesiastical territories (total)" = "Eccl. Terr. (total)",
  "Average area of ecclesiastical territories in km^2" = "Avg. Eccl. Area (km²)",
  "Ecclesiastical territories of Latin rite" = "Eccl. Terr. (Latin)",
  "Ecclesiastical territories of Eastern rites" = "Eccl. Terr. (Eastern)",
  "Patriarchal Sees" = "Patr. Sees",
  "Metropolitan Sees" = "Metr. Sees",
  "Archiepiscopal Sees" = "Archiep. Sees",
  "Episcopal Sees" = "Episc. Sees",
  "Territorial Prelatures" = "Terr. Prel.",
  "Territorial Abbacies" = "Terr. Abb.",
  "Exarchates and Ordinariates" = "Exarch. & Ordin.",
  "Military Ordinariates" = "Mil. Ordin.",
  "Apostolic Vicariates" = "Apost. Vic.",
  "Apostolic Prefectures" = "Apost. Pref.",
  "Apostolic Administrations" = "Apost. Admin.",
  "Independent Missions" = "Ind. Missions",
  "Patriarchal Exarchate" = "Patr. Exarch.",
  "Parishes (total)" = "Parishes (total)",
  "Mission stations with resident priest" = "Miss. Stn. w/ Priest",
  "Mission stations without resident priest" = "Miss. Stn. w/o Priest",
  "Other pastoral centres" = "Other Past. Ctrs.",
  "Pastoral centres (total)" = "Past. Ctrs. (total)",
  "Inhabitants per pastoral centre" = "Pop. per Past. Ctr.",
  "Catholics per pastoral centre" = "Cath. per Past. Ctr.",
  "Pastoral centres per diocese" = "Past. Ctrs. per Diocese",
  "Parishes as share of total pastoral centres" = "Parishes % Past. Ctrs.",
  "Mission stations" = "Miss. Stn. (total)",
  "Mission stations as share of total pastoral centres" = "Miss. Stn. % Past. Ctrs.",
  "Number of other pastoral centres as share of total pastoral centres" = "Other Past. Ctrs. %",
  "Pastoral centres as share of total pastoral centres" = "Past. Ctrs. % Total",
  "Parishes with diocesan pastor" = "Parishes w/ Dioc. Pastor",
  "Parishes with religious pastor" = "Parishes w/ Rel. Pastor",
  "Parishes without pastor administered by another priest" = "Parishes Adm. by Priest",
  "Parishes without pastor entrusted to permanent deacons" = "Parishes w/ Perm. Deacons",
  "Parishes without pastor entrusted to non-priest religious men" = "Parishes w/ Non-Pr. Men",
  "Parishes without pastor entrusted to religious women" = "Parishes w/ Rel. Women",
  "Parishes without pastor entrusted to laypeople" = "Parishes w/ Laypeople",
  "Parishes entirely vacant" = "Vacant Parishes",
  "Parishes administered by a priest" = "Parishes w/ Priest Admin.",
  "Parishes entrusted to non-priests" = "Parishes w/ Non-Pr. Admin.",
  "Bishops (total)" = "Bishops (total)",
  "Diocesan bishops" = "Dioc. Bishops",
  "Titular bishops" = "Tit. Bishops",
  "Secular cardinals" = "Sec. Cardinals",
  "Secular patriarchs" = "Sec. Patriarchs",
  "Secular archbishops" = "Sec. Archbishops",
  "Secular bishops" = "Sec. Bishops",
  "Secular episcopate (total)" = "Sec. Episc. (total)",
  "Religious cardinals" = "Rel. Cardinals",
  "Religious patriarchs" = "Rel. Patriarchs",
  "Religious archbishops" = "Rel. Archbishops",
  "Religious bishops" = "Rel. Bishops",
  "Religious episcopate (total)" = "Rel. Episc. (total)",
  "Diocesan bishops (as main office)" = "Dioc. Bp. (Main)",
  "Coadjutor or auxhiliary bishops (as main office)" = "Coadj./Aux. Bp. (Main)",
  "Bishops with office in Roman Curia (as main office)" = "Bp. in Roman Curia",
  "Bishops with other main office" = "Bp. Other Office",
  "Bishops without office" = "Bp. w/o Office",
  "Bishops native to country of residence" = "Native Bishops",
  "Bishops foreign to country of residence" = "Foreign Bishops",
  "Diocesan priests (total)" = "Dioc. Priests (total)",
  "Religious priests" = "Rel. Priests",
  "Incardinated diocesan priests" = "Inc. Dioc. Priests",
  "Incardinated diocesan priests (resident abroad)" = "Inc. Dioc. Priests Abroad",
  "Incardinated diocesan priests (incardinated in another country)" = "Inc. Dioc. Priests Other Ctry.",
  "Difference between incardinated and present diocesan priests" = "Inc. vs. Pres. Dioc. Priests",
  "Incardinated diocesan priests on January 1" = "Inc. Dioc. Priests Jan 1",
  "Yearly ordinations of diocesan priests" = "Dioc. Priest Ord. (Yr.)",
  "Yearly deaths of diocesan priests" = "Dioc. Priest Deaths (Yr.)",
  "Yearly defections of diocesan priests" = "Dioc. Priest Def. (Yr.)",
  "Yearly variation in number of diocesan priests for other reasons" = "Dioc. Priest Var. Other (Yr.)",
  "Yearly variation in number of diocesan priests (overall balance)" = "Dioc. Priest Var. (Yr.)",
  "Yearly ordinations of diocesan priests as share of those incardinated on January 1" = "Dioc. Ord. % Inc. Jan 1",
  "Yearly deaths of diocesan priests as share of those incardinated on January 1" = "Dioc. Deaths % Inc. Jan 1",
  "Yearly defections of diocesan priests as share of those incardinated at January 1" = "Dioc. Def. % Inc. Jan 1",
  "Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1" = "Dioc. Net Ord. % Inc. Jan 1",
  "Priests and bishops as share of apostolic workforce" = "Priests & Bp. % Workforce",
  "Diocesan permanent deacons" = "Dioc. Perm. Deacons",
  "Religious permanent deacons" = "Rel. Perm. Deacons",
  "Permanent deacons (diocesan and religious)" = "Perm. Deacons (total)",
  "Non-priest religious men (with temporary or perpetual vows)" = "Non-Pr. Rel. Men",
  "Religious women (with temporary or perpetual vows)" = "Rel. Women",
  "Lay members of secular institutes for men" = "Lay Sec. Inst. Men",
  "Members of secular institutes for women" = "Sec. Inst. Women",
  "Lay missionaries" = "Lay Missionaries",
  "Catechists" = "Catechists",
  "Inhabitants per priest" = "Pop. per Priest",
  "Catholics per priest" = "Cath. per Priest",
  "Secondary schools for diocesan clergy - seminaries" = "Dioc. Sec. Sem.",
  "Secondary schools for diocesan clergy - residences" = "Dioc. Sec. Res.",
  "Philosophy+theology centres for diocesan clergy - seminaries" = "Dioc. Phil+Theo Sem.",
  "Philosophy+theology centres for diocesan clergy - residences" = "Dioc. Phil+Theo Res.",
  "Secondary schools for religious clergy - seminaries" = "Rel. Sec. Sem.",
  "Secondary schools for religious clergy - residences" = "Rel. Sec. Res.",
  "Philosophy+theology centres for religious clergy - seminaries" = "Rel. Phil+Theo Sem.",
  "Philosophy+theology centres for religious clergy - residences" = "Rel. Phil+Theo Res.",
  "Secondary schools and philosophy+theology centres (for diocesan and religious clergy)" = "Sec. & Phil+Theo (total)",
  "Secondary schools and philosophy+theology centres (for diocesan clergy)" = "Dioc. Sec. & Phil+Theo",
  "Secondary schools and philosophy+theology centres (for religious clergy)" = "Rel. Sec. & Phil+Theo",
  "Philosophy+theology centres for diocesan and religious clergy - seminaries and residences" = "Phil+Theo Sem. & Res.",
  "Philosophy+theology centres for diocesan and religious clergy - seminaries" = "Phil+Theo Sem. (total)",
  "Philosophy+theology centres for diocesan and religious clergy - residences" = "Phil+Theo Res. (total)",
  "Secondary schools for diocesan and religious clergy - seminaries and residences" = "Sec. Sem. & Res. (total)",
  "Secondary schools for diocesan and religious clergy - seminaries" = "Sec. Sem. (total)",
  "Secondary schools for diocesan and religious clergy - residences" = "Sec. Res. (total)",
  "Secondary schools and philosophy+theology centres for diocesan clergy - seminaries" = "Dioc. Sec. & Sem.",
  "Secondary schools and philosophy+theology centres for diocesan clergy - residences" = "Dioc. Sec. & Res.",
  "Secondary schools and philosophy+theology centres for religious clergy - seminaries" = "Rel. Sec. & Sem.",
  "Secondary schools and philosophy+theology centres for religious clergy - residences" = "Rel. Sec. & Res.",
  "Students for diocesan clergy in secondary schools" = "Dioc. Sec. Students",
  "Students for religious clergy in secondary schools" = "Rel. Sec. Students",
  "Students for diocesan and religious clergy in secondary schools" = "Sec. Students (total)",
  "Candidates for diocesan clergy in philosophy centres" = "Dioc. Phil. Cand.",
  "Candidates for religious clergy in philosophy centres" = "Rel. Phil. Cand.",
  "Candidates for diocesan and religious clergy in philosophy centres" = "Phil. Cand. (total)",
  "Candidates for diocesan clergy in theology centres" = "Dioc. Theo. Cand.",
  "Candidates for religious clergy in theology centres" = "Rel. Theo. Cand.",
  "Candidates for diocesan and religious clergy in theology centres" = "Theo. Cand. (total)",
  "Candidates for diocesan clergy in philosophy+theology centres" = "Dioc. Phil+Theo Cand.",
  "Candidates for religious clergy in philosophy+theology centres" = "Rel. Phil+Theo Cand.",
  "Candidates for diocesan and religious clergy in philosophy+theology centres" = "Phil+Theo Cand. (total)",
  "Students in philosophy centres for diocesan clergy who left seminary" = "Dioc. Phil. Dropouts",
  "Students in theology centres for diocesan clergy who left seminary" = "Dioc. Theo. Dropouts",
  "Students in philosophy+theology centres for diocesan clergy who left seminary" = "Dioc. Phil+Theo Dropouts",
  "Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants" = "Voc. Rate per 100k Pop.",
  "Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics" = "Voc. Rate per 100k Cath.",
  "Philosophy+theology candidates for diocesan and religious clergy per 100 priests" = "Cand. per 100 Priests",
  "Vocation rate - philosophy+theology and secondary school students (for diocesan and religious clergy) per 100 thousand Catholics" = "Voc. Rate Stud. per 100k Cath.",
  "Vocation rate - philosophy+theology and secondary school students (for diocesan and religious clergy) per 100 thousand inhabitants" = "Voc. Rate Stud. per 100k Pop.",
  "Ordination rate - Yearly ordinations per 100 philosophy+theology students for diocesan priesthood" = "Ord. Rate per 100 Dioc. Cand.",
  "Departures per 100 enrolled students in philosophy+theology centres for diocesan clergy" = "Dropout Rate per 100 Dioc.",
  "Kindergartens" = "Kindergartens",
  "Students in kindergartens" = "Kindergarten Students",
  "Primary or elementary schools" = "Prim. Schools",
  "Students in primary or elementary schools" = "Prim. School Students",
  "Secondary schools" = "Sec. Schools",
  "Students in secondary schools" = "Sec. School Students",
  "Students in higher institutes" = "Higher Inst. Students",
  "Students in universities for ecclesiastical studies" = "Eccl. Univ. Students",
  "Students in other universities" = "Other Univ. Students",
  "Infant baptisms (people up to 7 years old)" = "Infant Baptisms",
  "Adult baptisms (people over 7 years old)" = "Adult Baptisms",
  "Baptisms" = "Baptisms (total)",
  "Infant baptisms (people up to 7 years old) per 1000 Catholics" = "Infant Bapt. per 1000 Cath.",
  "Marriages between Catholics" = "Cath. Marriages",
  "Mixed marriages" = "Mixed Marriages",
  "Marriages" = "Marriages (total)",
  "Marriages per 1000 Catholics" = "Marriages per 1000 Cath.",
  "Confirmations" = "Confirmations",
  "Confirmations per 1000 Catholics" = "Conf. per 1000 Cath.",
  "First Communions" = "First Communions",
  "First Communions per 1000 Catholics" = "First Comm. per 1000 Cath.",
  "Share of adult baptisms (people over 7 years old)" = "Adult Bapt. %",
  "Share of mixed marriages (among those celebrated with ecclesiastical rite)" = "Mixed Marr. %",
  "Hospitals" = "Hospitals",
  "Dispensaries" = "Dispensaries",
  "Leprosaria" = "Leprosaria",
  "Homes for old, chronically ill, and disabled people" = "Homes for Elderly/Disabled",
  "Orphanages" = "Orphanages",
  "Nurseries" = "Nurseries",
  "Matrimonial advice centres" = "Marr. Advice Ctrs.",
  "Special centres for social (re-)education" = "Soc. Re-ed. Ctrs.",
  "Other welfare institutions" = "Other Welfare Inst.",
  "Welfare institutions (total)" = "Welfare Inst. (total)",
  "ISPR houses for men" = "ISPR Men Houses",
  "Bishop members of ISPRs for men" = "Bp. ISPR Men",
  "Priest members of ISPRs for men" = "Priest ISPR Men",
  "Permanent deacon members of ISPRs for men" = "Perm. Deacon ISPR Men",
  "Seminarian members of ISPRs for men" = "Sem. ISPR Men",
  "Non-priest religious members of ISPRs for men" = "Non-Pr. ISPR Men",
  "Novice seminarian members of ISPRs for men" = "Novice Sem. ISPR Men",
  "Novice non-seminarian members of ISPRs for men" = "Novice Non-Sem. ISPR Men",
  "Postulants in ISPRs for men" = "Postulants ISPR Men",
  "ISPR autonomous houses for women" = "ISPR Women Houses",
  "Postulants in ISPR autonomous houses for women" = "Postulants ISPR Women",
  "Novices in ISPR autonomous houses for women" = "Novices ISPR Women",
  "Professed religious women in ISPR autonomous houses (with temporary vows)" = "Temp. Vow ISPR Women",
  "Professed religious women in ISPR autonomous houses (with perpetual vows)" = "Perp. Vow ISPR Women",
  "ISPR centralized institutes houses for women" = "ISPR Cent. Women Houses",
  "Postulants in ISPR centralized institutes houses for women" = "Postulants Cent. ISPR Women",
  "Novices in ISPR centralized institutes houses for women" = "Novices Cent. ISPR Women",
  "Professed religious women in ISPR centralized institutes houses (with temporary vows)" = "Temp. Vow Cent. ISPR Women",
  "Professed religious women in ISPR centralized institutes houses (with perpetual vows)" = "Perp. Vow Cent. ISPR Women",
  "Candidates admitted to probation period in ISPR secular institutes for women" = "Prob. Cand. ISPR Women",
  "Members temporarily incorporated into ISPR secular institutes for women" = "Temp. Inc. ISPR Women",
  "Members definitively incorporated into ISPR secular institutes for women" = "Def. Inc. ISPR Women",
  "Laypeople associated with ISPR secular institutes for women" = "Lay Assoc. ISPR Women",
  "First instance cases for declaration of nullity of marriage pending on January 1 (ordinary processes)" = "1st Inst. Nullity Pend. Jan 1 (Ord.)",
  "First instance cases for declaration of nullity of marriage introduced during year (ordinary processes)" = "1st Inst. Nullity Intro. (Ord.)",
  "First instance cases for declaration of nullity of marriage completed during year (ordinary processes)" = "1st Inst. Nullity Compl. (Ord.)",
  "First instance cases for declaration of nullity of marriage pending on December 31 (ordinary processes)" = "1st Inst. Nullity Pend. Dec 31 (Ord.)",
  "Second instance cases for declaration of nullity of marriage pending on January 1 (ordinary processes)" = "2nd Inst. Nullity Pend. Jan 1 (Ord.)",
  "Second instance cases for declaration of nullity of marriage introduced during year (ordinary processes)" = "2nd Inst. Nullity Intro. (Ord.)",
  "Second instance cases for declaration of nullity of marriage completed during year (ordinary processes)" = "2nd Inst. Nullity Compl. (Ord.)",
  "Second instance cases for declaration of nullity of marriage pending on December 31 (ordinary processes)" = "2nd Inst. Nullity Pend. Dec 31 (Ord.)",
  "First instance cases for declaration of nullity of marriage pending on January 1 (documentary processes)" = "1st Inst. Nullity Pend. Jan 1 (Doc.)",
  "First instance cases for declaration of nullity of marriage introduced during year (documentary processes)" = "1st Inst. Nullity Intro. (Doc.)",
  "First instance cases for declaration of nullity of marriage completed during year (documentary processes)" = "1st Inst. Nullity Compl. (Doc.)",
  "First instance cases for declaration of nullity of marriage pending on December 31 (documentary processes)" = "1st Inst. Nullity Pend. Dec 31 (Doc.)",
  "Second instance cases for declaration of nullity of marriage pending on January 1 (documentary processes)" = "2nd Inst. Nullity Pend. Jan 1 (Doc.)",
  "Second instance cases for declaration of nullity of marriage introduced during year (documentary processes)" = "2nd Inst. Nullity Intro. (Doc.)",
  "Second instance cases for declaration of nullity of marriage completed during year (documentary processes)" = "2nd Inst. Nullity Compl. (Doc.)",
  "Second instance cases for declaration of nullity of marriage pending on December 31 (documentary processes)" = "2nd Inst. Nullity Pend. Dec 31 (Doc.)",
  "First instance cases for declaration of nullity of marriage pending on January 1 (briefer processes before bishop)" = "1st Inst. Nullity Pend. Jan 1 (Brief)",
  "First instance cases for declaration of nullity of marriage introduced during year (briefer processes before bishop)" = "1st Inst. Nullity Intro. (Brief)",
  "First instance cases for declaration of nullity of marriage completed during year (briefer processes before bishop)" = "1st Inst. Nullity Compl. (Brief)",
  "First instance cases for declaration of nullity of marriage pending on December 31 (briefer processes before bishop)" = "1st Inst. Nullity Pend. Dec 31 (Brief)",
  "Second instance cases for declaration of nullity of marriage pending on January 1 (briefer processes before bishop)" = "2nd Inst. Nullity Pend. Jan 1 (Brief)",
  "Second instance cases for declaration of nullity of marriage introduced during year (briefer processes before bishop)" = "2nd Inst. Nullity Intro. (Brief)",
  "Second instance cases for declaration of nullity of marriage completed during year (briefer processes before bishop)" = "2nd Inst. Nullity Compl. (Brief)",
  "Second instance cases for declaration of nullity of marriage pending on December 31 (briefer processes before bishop)" = "2nd Inst. Nullity Pend. Dec 31 (Brief)",
  "First instance sentences pro nullity of marriage (ordinary processes)" = "1st Inst. Sent. Pro Nullity (Ord.)",
  "First instance sentences contra nullity of marriage (ordinary processes)" = "1st Inst. Sent. Contra Nullity (Ord.)",
  "First instance peremptions of cases for declaration of nullity of marriage (ordinary processes)" = "1st Inst. Peremp. Nullity (Ord.)",
  "First instance renunciations of cases for declaration of nullity of marriage (ordinary processes)" = "1st Inst. Renunc. Nullity (Ord.)",
  "Total first instance closures of cases for declaration of nullity of marriage (ordinary processes)" = "1st Inst. Closures Nullity (Ord.)",
  "Second instance decree of first sentence confirmation for declaration of nullity of marriage (ordinary processes)" = "2nd Inst. Conf. Nullity (Ord.)",
  "Second instance sentence pro nullity of marriage (ordinary processes)" = "2nd Inst. Sent. Pro Nullity (Ord.)",
  "Second instance sentence contra nullity of marriage (ordinary processes)" = "2nd Inst. Sent. Contra Nullity (Ord.)",
  "Second instance peremptions of cases for declaration of nullity of marriage (ordinary processes)" = "2nd Inst. Peremp. Nullity (Ord.)",
  "Second instance renunciations of cases for declaration of nullity of marriage (ordinary processes)" = "2nd Inst. Renunc. Nullity (Ord.)",
  "Total second instance closures of cases (ordinary processes)" = "2nd Inst. Closures Nullity (Ord.)",
  "First instance sentences pro nullity of marriage (documentary processes)" = "1st Inst. Sent. Pro Nullity (Doc.)",
  "First instance cases for declaration of nullity of marriage referred to ordinary process (documentary processes)" = "1st Inst. Ref. to Ord. (Doc.)",
  "First instance peremptions of cases for declaration of nullity of marriage (documentary processes)" = "1st Inst. Peremp. Nullity (Doc.)",
  "First instance renunciations of cases for declaration of nullity of marriage (documentary processes)" = "1st Inst. Renunc. Nullity (Doc.)",
  "Total first instance closures of cases for declaration of nullity of marriage (documentary processes)" = "1st Inst. Closures Nullity (Doc.)",
  "Second instance sentences pro nullity of marriage (documentary processes)" = "2nd Inst. Sent. Pro Nullity (Doc.)",
  "Second instance cases for declaration of nullity of marriage referred to ordinary process (documentary processes)" = "2nd Inst. Ref. to Ord. (Doc.)",
  "Second instance peremptions of cases for declaration of nullity of marriage (documentary processes)" = "2nd Inst. Peremp. Nullity (Doc.)",
  "Second instance renunciations of cases for declaration of nullity of marriage (documentary processes)" = "2nd Inst. Renunc. Nullity (Doc.)",
  "Total second instance closures of cases for declaration of nullity of marriage (documentary processes)" = "2nd Inst. Closures Nullity (Doc.)",
  "First instance sentences pro nullity of marriage (briefer processes before bishop)" = "1st Inst. Sent. Pro Nullity (Brief)",
  "First instance cases for declaration of nullity of marriage referred to ordinary process (briefer processes before bishop)" = "1st Inst. Ref. to Ord. (Brief)",
  "First instance peremptions of cases for declaration of nullity of marriage (briefer processes before bishop)" = "1st Inst. Peremp. Nullity (Brief)",
  "First instance renunciations of cases for declaration of nullity of marriage (briefer processes before bishop)" = "1st Inst. Renunc. Nullity (Brief)",
  "Total first instance closures of cases for declaration of nullity of marriage (briefer processes before bishop)" = "1st Inst. Closures Nullity (Brief)",
  "Second instance decree of first sentence confirmation for declaration of nullity of marriage (briefer processes before bishop)" = "2nd Inst. Conf. Nullity (Brief)",
  "Second instance cases for declaration of nullity of marriage referred to ordinary process (briefer processes before bishop)" = "2nd Inst. Ref. to Ord. (Brief)",
  "Second instance peremptions of cases for declaration of nullity of marriage (briefer processes before bishop)" = "2nd Inst. Peremp. Nullity (Brief)",
  "Second instance renunciations of cases for declaration of nullity of marriage (briefer processes before bishop)" = "2nd Inst. Renunc. Nullity (Brief)",
  "Total second instance closures of cases for declaration of nullity of marriage (briefer processes before bishop)" = "2nd Inst. Closures Nullity (Brief)",
  "Processes for dispensation from unconsummated marriage pending on January 1" = "Disp. Uncons. Pend. Jan 1",
  "Processes for dispensation from unconsummated marriage introduced during year" = "Disp. Uncons. Intro. (Yr.)",
  "Acts of dispensation from unconsummated marriage transmitted to Apostolic See with favorable vote" = "Disp. Uncons. Fav. Vote",
  "Acts of dispensation from unconsummated marriage transmitted to Apostolic See with unfavorable vote" = "Disp. Uncons. Unfav. Vote",
  "Processes for dispensation from unconsummated marriage closed for other reasons" = "Disp. Uncons. Closed Other",
  "Processes for dispensation from unconsummated marriage pending on December 31" = "Disp. Uncons. Pend. Dec 31",
  "Processes for declaration of presumed death of spouse pending on January 1" = "Pres. Death Pend. Jan 1",
  "Processes for declaration of presumed death of spouse introduced during year" = "Pres. Death Intro. (Yr.)",
  "Processes for declaration of presumed death of spouse closed during year" = "Pres. Death Closed (Yr.)",
  "Processes for declaration of presumed death of spouse pending on December 31" = "Pres. Death Pend. Dec 31",
  "Cases for declaration of nullity of marriage - not paid by parties" = "Nullity Cases Not Paid",
  "Cases for declaration of nullity of marriage - partially paid by parties" = "Nullity Cases Part. Paid",
  "Cases for declaration of nullity of marriage - entirely paid by parties" = "Nullity Cases Fully Paid",
  "Processes for dispensation from unconsummated marriage - not paid by parties" = "Disp. Uncons. Not Paid",
  "Processes for dispensation from unconsummated marriage - partially paid by parties" = "Disp. Uncons. Part. Paid",
  "Processes for dispensation from unconsummated marriage - entirely paid by parties" = "Disp. Uncons. Fully Paid",
  "Processes for presumed death of spouse - not paid by parties" = "Pres. Death Not Paid",
  "Processes for presumed death of spouse - partially paid by parties" = "Pres. Death Part. Paid",
  "Processes for presumed death of spouse - entirely paid by parties" = "Pres. Death Fully Paid",
  "Ecclesiastical territories (total) - index numbers (base 2013 = 100)" = "Eccl. Terr. Index (2013=100)",
  "Pastoral centres (total) - index numbers (base 2013 = 100)" = "Past. Ctrs. Index (2013=100)",
  "Parishes (total) - index numbers (base 2013 = 100)" = "Parishes Index (2013=100)",
  "Priests (diocesan and religious)" = "Priests (total)",
  "Priests (diocesan and religious) - index numbers (base 2013 = 100)" = "Priests Index (2013=100)",
  "Diocesan priests (total) - index numbers (base 2013 = 100)" = "Dioc. Priests Index (2013=100)",
  "Incardinated diocesan priests - index numbers (base 2013 = 100)" = "Inc. Dioc. Priests Index (2013=100)",
  "Religious priests - index numbers (base 2013 = 100)" = "Rel. Priests Index (2013=100)",
  "Permanent deacons (diocesan and religious) - index numbers (base 2013 = 100)" = "Perm. Deacons Index (2013=100)",
  "Non-priest religious men (with temporary or perpetual vows) - index numbers (base 2013 = 100)" = "Non-Pr. Rel. Men Index (2013=100)",
  "Religious women (with temporary or perpetual vows) - index numbers (base 2013 = 100)" = "Rel. Women Index (2013=100)",
  "Candidates for diocesan and religious clergy in philosophy+theology centres - index numbers (base 2013 = 100)" = "Phil+Theo Cand. Index (2013=100)",
  "Students for diocesan and religious clergy in secondary schools - index numbers (base 2013 = 100)" = "Sec. Students Index (2013=100)"
)

# ---- Data Filtering Functions ----
# Function to check if a column has non-NA data at the country level.
has_country_data <- function(col_data, region_type) {
  country_data <- col_data[region_type == "Country"]
  return(sum(!is.na(country_data)) > 0)
}

# ---- Identify and Filter Columns with Country Data ----
# Apply the function to identify columns with country-level data.
cols_with_country_data <- sapply(names(data), function(col_name) {
  if (is.numeric(data[[col_name]])) {
    has_country_data(data[[col_name]], data$`Region type`)
  } else {
    TRUE
  }
})

# Filter the dataset to retain only columns with country data.
data_filtered <- data[, cols_with_country_data]

# ---- Separate Data into Macroregions and Countries ----
# Extract macroregion data and rename the region column for consistency.
data_macroregions <- data_filtered %>%
  filter(`Region type` == "Macroregion") %>%
  rename(macroregion = Region)

# Extract country data.
data_countries <- data_filtered %>%
  filter(`Region type` == "Country")

# ---- Load and Prepare World Map ----
# Load world map data from Natural Earth as an sf object.
world <- ne_countries(scale = "medium", returnclass = "sf")

# Unify Somalia by merging Somalia and Somaliland geometries.
somalia_unified <- world %>%
  filter(admin %in% c("Somalia", "Somaliland")) %>%
  summarise(admin = "Somalia", name = "Somalia", geometry = st_union(geometry))

# Update the world map by removing original entries and adding the unified Somalia.
world <- world %>%
  filter(!admin %in% c("Somalia", "Somaliland")) %>%
  bind_rows(somalia_unified)

# ---- Initial Map Data Merge ----
# Merge country data with the world map using country names.
map_data <- left_join(world, data_countries, by = c("name" = "Region"))

# ---- Analyze Unmatched Country Names ----
# Identify countries in data that do not match the world map.
unmatched_in_data <- anti_join(data_countries, world, by = c("Region" = "name"))
print(unmatched_in_data$Region)

# ---- Manual Country Name Corrections ----
# Define a vector of corrections for mismatched country names.
name_corrections <- c(
  "Antigua and Barbuda" = "Antigua and Barb.",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Bosnia and Herzegovina" = "Bosnia and Herz.",
  "Brunei Darussalam" = "Brunei",
  "Cayman Islands" = "Cayman Is.",
  "Central African Republic" = "Central African Rep.",
  "China (Mainland)" = "China",
  "China (Taiwan)" = "Taiwan",
  "Cook Islands" = "Cook Is.",
  "Cote d'Ivoire" = "Côte d'Ivoire",
  "Dem. Peoples Rep. Of Korea" = "North Korea",
  "Dem. Rep. of the Congo" = "Dem. Rep. Congo",
  "Dominican Republic" = "Dominican Rep.",
  "East Timor" = "Timor-Leste",
  "Equatorial Guinea" = "Eq. Guinea",
  "Eswatini" = "eSwatini",
  "Faeroe Islands" = "Faeroe Is.",
  "Falkland Islands (Malvinas)" = "Falkland Is.",
  "Fed. S. Micronesia" = "Micronesia",
  "French Guiana" = "France",
  "French Polynesia" = "Fr. Polynesia",
  "Gibraltar" = "United Kingdom",
  "Great Britain" = "United Kingdom",
  "Guadeloupe" = "France",
  "Hong Kong SAR" = "Hong Kong",
  "Iran (Islamic Rep. of)" = "Iran",
  "Kazakchstan" = "Kazakhstan",
  "Macao SAR" = "Macao",
  "Macedonia (Rep. of North)" = "North Macedonia",
  "Marshall Islands" = "Marshall Is.",
  "Martinique" = "France",
  "Montenegro (Rep. of)" = "Montenegro",
  "N. Mariana Islands" = "United States of America",
  "Netherlands Antilles" = "Curaçao",
  "Peoples Dem. Rep. Lao" = "Laos",
  "Republic of Korea" = "South Korea",
  "Republic of Moldova" = "Moldova",
  "Reunion" = "France",
  "Russian Fed. (in Europe)" = "Russia",
  "Russian Federation (in Asia)" = "Russia",
  "Saint Kitts and Nevis" = "St. Kitts and Nevis",
  "Sao Tome and Principe" = "São Tomé and Principe",
  "Solomon Islands" = "Solomon Is.",
  "South Sudan" = "S. Sudan",
  "St. Vincent and the Grenadines" = "St. Vin. and Gren.",
  "Svalbard and Jan Mayen Is." = "Norway",
  "Syrian Arab Republic" = "Syria",
  "Tokelau" = "New Zealand",
  "Turkiye" = "Turkey",
  "Turks and Caicos Islands" = "Turks and Caicos Is.",
  "United Republic of Tanzania" = "Tanzania",
  "United States" = "United States of America",
  "Venezuela (The Bolivar Rep. of)" = "Venezuela",
  "Viet Nam" = "Vietnam",
  "Virgin Islands (Great. Brit.)" = "British Virgin Is.",
  "Virgin Islands (U.S.A.)" = "United States of America",
  "Wallis and Futuna Islands" = "Wallis and Futuna Is.",
  "Western Sahara" = "W. Sahara"
)

# Apply corrections by creating a new 'country' column with standardized names.
data_countries$country <- ifelse(
  data_countries$Region %in% names(name_corrections),
  name_corrections[data_countries$Region],
  data_countries$Region
)

# ---- Aggregate Numeric Values by Country and Year ----
# Ensure 'Year' column exists and is converted to integer.
stopifnot("Year" %in% names(data_countries))
data_countries <- data_countries %>%
  mutate(Year = suppressWarnings(as.integer(Year)))

# Identify numeric columns excluding 'Year'.
num_cols <- names(data_countries)[sapply(data_countries, is.numeric)]
num_cols <- setdiff(num_cols, "Year")

# Aggregate by summing numeric values for each country-year pair.
data_countries <- data_countries %>%
  group_by(country, Year) %>%
  summarise(
    across(
      all_of(num_cols),
      ~ if (all(is.na(.))) NA_real_ else sum(., na.rm = TRUE)
    ),
    .groups = "drop"
  )

# ---- Safe Division Helper Function ----
# Helper function to perform division safely, avoiding NA or division by zero.
safe_div <- function(num, den, scale = 1) {
  ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, (num / den) * scale)
}

# ---- Recompute Derived Variables ----
# Shorthand for data_countries to simplify recomputations.
dc <- data_countries

# Recompute densities and rates, rounding to 2 decimal places.
if (all(c("Inhabitants per km^2", "Inhabitants in thousands", "Area in km^2") %in% names(dc))) {
  dc[["Inhabitants per km^2"]] <- round(safe_div(dc[["Inhabitants in thousands"]] * 1000, dc[["Area in km^2"]]), 2)
}
if (all(c("Catholics per 100 inhabitants", "Catholics in thousands", "Inhabitants in thousands") %in% names(dc))) {
  dc[["Catholics per 100 inhabitants"]] <- round(safe_div(dc[["Catholics in thousands"]], dc[["Inhabitants in thousands"]], 100), 2)
}
if (all(c("Inhabitants per pastoral centre", "Inhabitants in thousands", "Pastoral centres (total)") %in% names(dc))) {
  dc[["Inhabitants per pastoral centre"]] <- round(safe_div(dc[["Inhabitants in thousands"]] * 1000, dc[["Pastoral centres (total)"]]), 2)
}
if (all(c("Catholics per pastoral centre", "Catholics in thousands", "Pastoral centres (total)") %in% names(dc))) {
  dc[["Catholics per pastoral centre"]] <- round(safe_div(dc[["Catholics in thousands"]] * 1000, dc[["Pastoral centres (total)"]]), 2)
}
if (all(c("Pastoral centres per diocese", "Pastoral centres (total)", "Ecclesiastical territories (total)") %in% names(dc))) {
  dc[["Pastoral centres per diocese"]] <- round(safe_div(dc[["Pastoral centres (total)"]], dc[["Ecclesiastical territories (total)"]]), 2)
}
if (all(c("Parishes as share of total pastoral centres", "Parishes (total)", "Pastoral centres (total)") %in% names(dc))) {
  dc[["Parishes as share of total pastoral centres"]] <- round(safe_div(dc[["Parishes (total)"]], dc[["Pastoral centres (total)"]]), 2)
}

# Recompute mission stations share.
if (all(c("Mission stations with resident priest",
          "Mission stations without resident priest",
          "Pastoral centres (total)") %in% names(dc))) {
  dc[["Mission stations as share of total pastoral centres"]] <-
    round(safe_div(dc[["Mission stations with resident priest"]] +
                     dc[["Mission stations without resident priest"]],
                   dc[["Pastoral centres (total)"]]), 2)
}
if (all(c("Number of other pastoral centres as share of total pastoral centres", "Other pastoral centres", "Pastoral centres (total)") %in% names(dc))) {
  dc[["Number of other pastoral centres as share of total pastoral centres"]] <- round(safe_div(dc[["Other pastoral centres"]], dc[["Pastoral centres (total)"]]), 2)
}

# Compute total priests for burdens.
priests_total <- if ("Priests (diocesan and religious)" %in% names(dc)) {
  dc[["Priests (diocesan and religious)"]]
} else if (all(c("Diocesan priests (total)", "Religious priests") %in% names(dc))) {
  dc[["Diocesan priests (total)"]] + dc[["Religious priests"]]
} else {
  NA_real_
}
if ("Inhabitants per priest" %in% names(dc) && !all(is.na(priests_total))) {
  dc[["Inhabitants per priest"]] <- round(safe_div(dc[["Inhabitants in thousands"]] * 1000, priests_total), 2)
}
if ("Catholics per priest" %in% names(dc) && !all(is.na(priests_total))) {
  dc[["Catholics per priest"]] <- round(safe_div(dc[["Catholics in thousands"]] * 1000, priests_total), 2)
}

# Recompute sacraments per 1000 Catholics.
if (all(c("Infant baptisms (people up to 7 years old) per 1000 Catholics",
          "Infant baptisms (people up to 7 years old)", "Catholics in thousands") %in% names(dc))) {
  dc[["Infant baptisms (people up to 7 years old) per 1000 Catholics"]] <-
    round(safe_div(dc[["Infant baptisms (people up to 7 years old)"]], dc[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("Marriages per 1000 Catholics", "Marriages", "Catholics in thousands") %in% names(dc))) {
  dc[["Marriages per 1000 Catholics"]] <- round(safe_div(dc[["Marriages"]], dc[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("Confirmations per 1000 Catholics", "Confirmations", "Catholics in thousands") %in% names(dc))) {
  dc[["Confirmations per 1000 Catholics"]] <- round(safe_div(dc[["Confirmations"]], dc[["Catholics in thousands"]] * 1000, 1000), 2)
}
if (all(c("First Communions per 1000 Catholics", "First Communions", "Catholics in thousands") %in% names(dc))) {
  dc[["First Communions per 1000 Catholics"]] <- round(safe_div(dc[["First Communions"]], dc[["Catholics in thousands"]] * 1000, 1000), 2)
}

# Recompute shares.
if (all(c("Share of adult baptisms (people over 7 years old)", "Adult baptisms (people over 7 years old)", "Baptisms") %in% names(dc))) {
  dc[["Share of adult baptisms (people over 7 years old)"]] <- round(safe_div(dc[["Adult baptisms (people over 7 years old)"]], dc[["Baptisms"]]), 2)
}
if (all(c("Share of mixed marriages (among those celebrated with ecclesiastical rite)", "Mixed marriages", "Marriages") %in% names(dc))) {
  dc[["Share of mixed marriages (among those celebrated with ecclesiastical rite)"]] <- round(safe_div(dc[["Mixed marriages"]], dc[["Marriages"]]), 2)
}

# Recompute vocation/ordination/departure rates.
if (all(c("Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants",
          "Candidates for diocesan and religious clergy in philosophy+theology centres", "Inhabitants in thousands") %in% names(dc))) {
  dc[["Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand inhabitants"]] <-
    round(safe_div(dc[["Candidates for diocesan and religious clergy in philosophy+theology centres"]],
                   dc[["Inhabitants in thousands"]] * 1000, 100000), 2)
}
if (all(c("Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics",
          "Candidates for diocesan and religious clergy in philosophy+theology centres", "Catholics in thousands") %in% names(dc))) {
  dc[["Vocation rate - philosophy+theology candidates for diocesan and religious clergy per 100 thousand Catholics"]] <-
    round(safe_div(dc[["Candidates for diocesan and religious clergy in philosophy+theology centres"]],
                   dc[["Catholics in thousands"]] * 1000, 100000), 2)
}
if (all(c("Ordination rate - Yearly ordinations per 100 philosophy+theology students for diocesan priesthood",
          "Yearly ordinations of diocesan priests", "Candidates for diocesan clergy in philosophy+theology centres") %in% names(dc))) {
  dc[["Ordination rate - Yearly ordinations per 100 philosophy+theology students for diocesan priesthood"]] <-
    round(safe_div(dc[["Yearly ordinations of diocesan priests"]],
                   dc[["Candidates for diocesan clergy in philosophy+theology centres"]], 100), 2)
}
if (all(c("Departures per 100 enrolled students in philosophy+theology centres for diocesan clergy",
          "Students in philosophy+theology centres for diocesan clergy who left seminary",
          "Candidates for diocesan clergy in philosophy+theology centres") %in% names(dc))) {
  dc[["Departures per 100 enrolled students in philosophy+theology centres for diocesan clergy"]] <-
    round(safe_div(dc[["Students in philosophy+theology centres for diocesan clergy who left seminary"]],
                   dc[["Candidates for diocesan clergy in philosophy+theology centres"]], 100), 2)
}

# Recompute philosophy+theology candidates per 100 priests.
if (all(c("Philosophy+theology candidates for diocesan and religious clergy per 100 priests",
          "Candidates for diocesan and religious clergy in philosophy+theology centres") %in% names(dc)) && !all(is.na(priests_total))) {
  dc[["Philosophy+theology candidates for diocesan and religious clergy per 100 priests"]] <-
    round(safe_div(dc[["Candidates for diocesan and religious clergy in philosophy+theology centres"]], priests_total, 100), 2)
}

# Compute lagged values for diocesan priest shares.
dc <- dc %>%
  arrange(country, Year) %>%
  group_by(country) %>%
  mutate(
    prev_inc_priests = lag(`Incardinated diocesan priests on January 1`, n = 1)
  ) %>%
  ungroup()
if (all(c("Yearly ordinations of diocesan priests as share of those incardinated on January 1",
          "Yearly ordinations of diocesan priests") %in% names(dc))) {
  dc[["Yearly ordinations of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(dc[["Yearly ordinations of diocesan priests"]], dc[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly deaths of diocesan priests as share of those incardinated on January 1",
          "Yearly deaths of diocesan priests") %in% names(dc))) {
  dc[["Yearly deaths of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(dc[["Yearly deaths of diocesan priests"]], dc[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly defections of diocesan priests as share of those incardinated at January 1",
          "Yearly defections of diocesan priests") %in% names(dc))) {
  dc[["Yearly defections of diocesan priests as share of those incardinated at January 1"]] <-
    round(safe_div(dc[["Yearly defections of diocesan priests"]], dc[["prev_inc_priests"]], 100), 2)
}
if (all(c("Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1",
          "Yearly ordinations of diocesan priests", "Yearly deaths of diocesan priests", "Yearly defections of diocesan priests") %in% names(dc))) {
  net_ord <- dc[["Yearly ordinations of diocesan priests"]] - dc[["Yearly deaths of diocesan priests"]] - dc[["Yearly defections of diocesan priests"]]
  dc[["Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1"]] <-
    round(safe_div(net_ord, dc[["prev_inc_priests"]], 100), 2)
}

# Recompute weighted averages.
if (all(c("Average area of ecclesiastical territories in km^2", "Area in km^2", "Ecclesiastical territories (total)") %in% names(dc))) {
  dc[["Average area of ecclesiastical territories in km^2"]] <-
    round(safe_div(dc[["Area in km^2"]], dc[["Ecclesiastical territories (total)"]]), 2)
}
if (all(c("Average diocesan area (in km^2)", "Area in km^2", "Ecclesiastical territories (total)") %in% names(dc))) {
  dc[["Average diocesan area (in km^2)"]] <-
    round(safe_div(dc[["Area in km^2"]], dc[["Ecclesiastical territories (total)"]]), 2)
}

# Recompute apostolic workforce share.
if (all(c("Priests and bishops as share of apostolic workforce",
          "Bishops (total)", "Catechists", "Lay missionaries") %in% names(dc)) &&
    ("Priests (diocesan and religious)" %in% names(dc) ||
     all(c("Diocesan priests (total)", "Religious priests") %in% names(dc)))) {
  
  priests_total <- if ("Priests (diocesan and religious)" %in% names(dc)) {
    dc[["Priests (diocesan and religious)"]]
  } else {
    dc[["Diocesan priests (total)"]] + dc[["Religious priests"]]
  }
  
  workforce <- priests_total + dc[["Bishops (total)"]] + dc[["Catechists"]] + dc[["Lay missionaries"]]
  dc[["Priests and bishops as share of apostolic workforce"]] <-
    round(safe_div(priests_total + dc[["Bishops (total)"]], workforce), 2)
}

# Commit all recomputed values back to data_countries.
data_countries <- dc

# ---- Define Excluded Variables for Map Tab ----
# List variables to exclude from the "Map" tab's variable selection menu.
excluded_vars <- c(
  "Area in km^2",
  "prev_inc_priests",
  "Yearly ordinations of diocesan priests as share of those incardinated on January 1",
  "Yearly deaths of diocesan priests as share of those incardinated on January 1",
  "Yearly defections of diocesan priests as share of those incardinated at January 1",
  "Yearly ordinations minus deaths and defections of diocesan priests as share of those incardinated on January 1",
  "Ecclesiastical territories (total) - index numbers (base 2013 = 100)",
  "Pastoral centres (total) - index numbers (base 2013 = 100)",
  "Parishes (total) - index numbers (base 2013 = 100)",
  "Priests (diocesan and religious) - index numbers (base 2013 = 100)",
  "Diocesan priests (total) - index numbers (base 2013 = 100)",
  "Incardinated diocesan priests - index numbers (base 2013 = 100)",
  "Religious priests - index numbers (base 2013 = 100)",
  "Permanent deacons (diocesan and religious) - index numbers (base 2013 = 100)",
  "Non-priest religious men (with temporary or perpetual vows) - index numbers (base 2013 = 100)",
  "Religious women (with temporary or perpetual vows) - index numbers (base 2013 = 100)",
  "Candidates for diocesan and religious clergy in philosophy+theology centres - index numbers (base 2013 = 100)",
  "Students for diocesan and religious clergy in secondary schools - index numbers (base 2013 = 100)"
)

# ---- Define Allowed Variables for Map Tab ----
# Select numeric variables excluding 'Year' and those in excluded_vars.
allowed_variables <- setdiff(
  names(data_countries)[sapply(data_countries, is.numeric) & names(data_countries) != "Year"],
  excluded_vars
)

# ---- Final Map Data Rematching ----
# Re-merge the world map with corrected country data.
map_data <- left_join(world, data_countries, by = c("name" = "country"))

# ---- Verify Unmatched Countries After Corrections ----
# Check for any remaining unmatched countries and print them.
unmatched_after_fix <- anti_join(data_countries, world, by = c("country" = "name"))
print(unique(unmatched_after_fix$country))

# ---- Identify Time Series Variables ----
# Filter variables that have data for more than one year for time series use.
time_series_vars <- allowed_variables[
  sapply(allowed_variables, function(var) {
    years <- data_countries %>%
      filter(!is.na(.data[[var]])) %>%
      pull(Year) %>%
      unique()
    length(years) > 1
  })
]

# ---- UI Layout ----
# Prepare country choices for the search input.
all_countries <- sort(unique(data_countries$country))
all_countries <- all_countries[!is.na(all_countries) & all_countries != ""]
country_choices_list <- as.list(all_countries)
names(country_choices_list) <- all_countries
final_country_dropdown_choices <- c("Type to search..." = "", country_choices_list)

# Define the Shiny UI with custom styles and layout.
ui <- tagList(
  tags$head(
    tags$style(HTML("
  html, body {
    height: 100%;
    margin: 0;
    padding: 0;
    overflow: hidden;
  }
  #map {
    height: 100vh !important;
    width: 100vw !important;
    position: absolute;
    top: 0;
    left: 0;
    z-index: 1;
  }
  .leaflet-container {
    background: #ececec !important;
    height: 100% !important;
    width: 100% !important;
  }
  .leaflet-tile-pane { filter: grayscale(10%) brightness(1.15); }
  .leaflet-control { font-size: 14px; }
  .panel-default {
    box-shadow: 0 2px 6px rgba(0,0,0,0.25);
    border-radius: 10px;
    z-index: 1000 !important;
  }
  .panel-default > .panel-heading {
    background-color: #4e73df;
    color: white;
    font-weight: bold;
  }
  .navbar {
    z-index: 1001 !important;
  }
  body {
    font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
  }
  .plotly, .js-plotly-plot, .plotly text {
  font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
  }
  .shiny-plot-output {
    margin-top: 10px;
  }
   .panel-default .form-group {
    margin-bottom: 15px;
  }
  div.tab-pane[data-value='Data Explorer'] .data-explorer-main {
    overflow-y: auto !important;
    max-height: 80vh !important;
    padding: 15px;
  }
  .leaflet-top {
    margin-top: 70px !important;
  }
  .panel-default hr {
    margin-top: 5px;
    margin-bottom: 5px;
  }
  .ts-wrap { height: calc(100vh - 150px); }
 .ts-sidebar {
  max-height: calc(100vh - 190px);
  overflow-y: auto;
  background-color: #f8f9fa;
  border-radius: 8px;
  border: 1px solid #dee2e6;
  padding: 15px;
}
")), useShinyjs()
  ),
  
  navbarPage("Annuarium Statisticum Ecclesiae", id = "navbar", theme = shinytheme("flatly"),
             
             tabPanel("Map",
                      div(
                        leafletOutput("map", height = "100vh", width = "100%"),
                        absolutePanel(
                          id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",
                          width = 300, height = "auto",
                          style = "background-color: rgba(255,255,255,0.8); padding: 10px; border-radius: 10px; overflow-y: auto; max-height: 90vh;",
                          selectInput("variable", "Select variable to display:",
                                      choices = allowed_variables),
                          selectInput("year", "Select year:",
                                      choices = sort(unique(data_countries$Year)), selected = max(data_countries$Year)),
                          radioButtons("display_mode", "Display mode:",
                                       choices = list("Absolute values" = "absolute",
                                                      "Per thousand inhabitants" = "per_capita",
                                                      "Per thousand Catholics" = "per_catholic"),
                                       selected = "absolute"),
                          selectizeInput("country_search", "Search for a country:",
                                         choices = final_country_dropdown_choices,
                                         selected = "", multiple = FALSE,
                                         options = list(placeholder = 'Type to search...')),
                          plotOutput("varPlot", height = 150),
                          hr(),
                          div(style = "margin-bottom: 15px;", htmlOutput("country_info")),
                          actionButton("reset_map", "Reset View", icon = icon("undo")),
                          div(style = "margin-top: 15px;",
                              downloadButton("download_map", "Download Map", class = "btn btn-primary")
                          )
                        )
                      )
             ),
             
             # DATA TABLE TAB
             tabPanel("Data Explorer",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          tags$div(
                            style = "background-color: #f8f9fa; border-radius: 8px; padding: 15px; border: 1px solid #dee2e6; font-size: 14px;",
                            selectInput("explorer_variable", "Select variable:", choices = c("Select a variable..." = "", allowed_variables)), # Use full variable names
                            selectInput("explorer_year", "Select year:", choices = sort(unique(data_countries$Year))),
                            div(style = "margin-top: 10px;",
                                downloadButton("download_csv", "CSV", class = "btn btn-sm btn-success"),
                                downloadButton("download_excel", "Excel", class = "btn btn-sm btn-info")
                            ),
                            br(),
                            actionButton("reset_table", "Reset Filters", icon = icon("redo"), class = "btn btn-sm btn-secondary")
                          )
                        ),
                        mainPanel(
                          class = "data-explorer-main",
                          width = 9,
                          DTOutput("table"),
                          br()
                        )
                      )
             ),
             # Time Series TAB
             tabPanel("Time Series",
                      fluidRow(
                        column(
                          width = 3,
                          class = "ts-sidebar",
                          style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;",
                          selectInput("ts_variable", "Select variable:",
                                      choices = time_series_vars,
                                      selected = time_series_vars[1]),
                          radioButtons("ts_level", "Region level:",
                                       choices = c("Continent" = "Macroregion", "Country" = "Country"),
                                       selected = "Macroregion"),
                          uiOutput("ts_region_selector"),
                          div(style = "margin-top: 10px;",
                              downloadButton("download_ts_plot", "Download Plot", class = "btn btn-sm btn-primary"),
                              actionButton("reset_ts", "Reset", icon = icon("undo"), class = "btn btn-sm btn-secondary")
                          )
                        ),
                        column(
                          width = 9,
                          div(class = "ts-wrap",
                              plotlyOutput("ts_plot", height = "100%")
                          )
                        )
                      )
             )
  )
)

# ---- Server Logic ----
# Helper function to standardize macroregion names for consistency in plotting.
TARGET_REGIONS <- c(
  "South and Far East Asia",
  "Africa",
  "Europe",
  "South America",
  "North America",
  "Middle East Asia",
  "Central America",
  "Oceania"
)
standardize_macro <- function(df, col = "macroregion") {
  df %>%
    filter(!.data[[col]] %in% c("World", "America", "Asia")) %>% # drop aggregates
    mutate(
      macro_simplified = dplyr::case_when(
        .data[[col]] %in% c("Central America (Mainland)", "Central America (Antilles)") ~ "Central America",
        .data[[col]] %in% c("South East and Far East Asia", "South & Far East Asia") ~ "South and Far East Asia",
        .data[[col]] == "Middle East" ~ "Middle East Asia",
        TRUE ~ .data[[col]]
      )
    ) %>%
    filter(macro_simplified %in% TARGET_REGIONS) %>%
    mutate(macro_simplified = factor(macro_simplified, levels = TARGET_REGIONS))
}

# Define the server function for the Shiny app.
server <- function(input, output, session) {
  
  # ---- Handle Map Download ----
  # Generate filename and content for downloading the map as PNG.
  output$download_map <- downloadHandler(
    filename = function() {
      ml <- mode_label()
      paste0("map_export_", input$variable, "_", input$year,
             switch(ml,
                    "absolute" = "",
                    "per_capita" = "_per_capita",
                    "per_catholic" = "_per_catholic"), ".png")
    },
    content = function(file) {
      ml <- mode_label()
      library(htmlwidgets)
      # Filter data for the selected year and apply display mode.
      filtered_data <- map_data %>% filter(Year == input$year)
      if (as.integer(input$year) == 2022 && input$display_mode == "per_capita") {
        filtered_data[[input$variable]] <- ifelse(
          !is.na(filtered_data[["Inhabitants in thousands"]]) & filtered_data[["Inhabitants in thousands"]] > 0,
          filtered_data[[input$variable]] / filtered_data[["Inhabitants in thousands"]],
          NA_real_
        )
      } else if (as.integer(input$year) == 2022 && input$display_mode == "per_catholic") {
        filtered_data[[input$variable]] <- ifelse(
          !is.na(filtered_data[["Catholics in thousands"]]) & filtered_data[["Catholics in thousands"]] > 0,
          filtered_data[[input$variable]] / filtered_data[["Catholics in thousands"]],
          NA_real_
        )
      }
      
      # Ensure valid values for color palette to avoid domain errors.
      valid_values <- filtered_data[[input$variable]][!is.na(filtered_data[[input$variable]])]
      pal <- if (length(valid_values) > 0) {
        colorNumeric(palette = viridisLite::plasma(256), domain = valid_values, na.color = "transparent")
      } else {
        colorNumeric(palette = viridisLite::plasma(256), domain = c(0, 1), na.color = "transparent")
      }
      
      leaflet_obj <- leaflet(filtered_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        fitBounds(-110, -40, 120, 65) %>%
        addPolygons(
          fillColor = ~pal(filtered_data[[input$variable]]),
          color = "white", weight = 1, opacity = 0.45, fillOpacity = 0.6,
          label = ~name
        ) %>%
        addLegend(pal = pal, values = filtered_data[[input$variable]],
                  title = paste(switch(ml,
                                       "absolute" = variable_abbreviations[input$variable], # Use abbreviated name
                                       "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."), # Adjusted for brevity
                                       "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath.")),
                                "in", input$year),
                  position = "bottomright")
      
      # Add title directly inside the map using leaflet::addControl().
      leaflet_obj <- leaflet_obj %>%
        addControl(
          html = paste0("<div style='font-size:20px; font-weight:bold; background-color:rgba(255,255,255,0.7);
                  padding:6px 12px; border-radius:6px;'>",
                        switch(ml,
                               "absolute" = input$variable, # Use full variable name
                               "per_capita" = paste(input$variable, "per thousand inhabitants"),
                               "per_catholic" = paste(input$variable, "per thousand Catholics")),
                        " - ", input$year, "</div>"),
          position = "topright"
        ) %>%
        addControl(
          html = "<div style='font-size:13px; background-color:rgba(255,255,255,0.6); padding:4px 10px;
            border-radius:5px;'>Source: Annuarium Statisticum Ecclesiae</div>",
          position = "bottomleft"
        )
      temp_html <- tempfile(fileext = ".html")
      saveWidget(leaflet_obj, temp_html, selfcontained = TRUE)
      # Take screenshot
      webshot::webshot(temp_html, file = file, vwidth = 1600, vheight = 1000)
    }
  )
  
  # ---- Initialize Reactive Values ----
  # Reactive value for selected country.
  selected_country <- reactiveVal(NULL)
  # Reactive values for synchronizing selections across tabs.
  selections <- reactiveValues(
    variable = NULL,
    year = NULL,
    from_tab = NULL # Track which tab triggered the change
  )
  
  # Reactive for display mode label.
  mode_label <- reactive({
    if (isTRUE(as.integer(input$year) == 2022)) input$display_mode else "absolute"
  })
  
  
  # Helper function to update time series for a selected country.
  update_time_series_for_country <- function(country) {
    req(country)
    if (country %in% data_countries$country) {
      updateRadioButtons(session, "ts_level", selected = "Country")
      shinyjs::delay(500, {
        updateSelectInput(session, "ts_regions", selected = country)
      })
    }
  }
  
  # ---- Synchronize Variable and Year Selections ----
  # Update from Map tab.
  observeEvent(input$variable, {
    if (is.null(selections$from_tab) || selections$from_tab != "explorer") {
      selections$variable <- input$variable
      selections$from_tab <- "map"
      updateSelectInput(session, "explorer_variable", selected = input$variable)
      if (input$variable %in% time_series_vars) {
        updateSelectInput(session, "ts_variable", selected = input$variable)
      } else {
        updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
      }
    }
  })
  
  # Update from Data Explorer tab.
  observeEvent(input$explorer_variable, {
    req(input$explorer_variable)
    if (input$explorer_variable != "" && (is.null(selections$from_tab) || selections$from_tab != "map")) {
      selections$variable <- input$explorer_variable
      selections$from_tab <- "explorer"
      updateSelectInput(session, "variable", selected = input$explorer_variable)
      if (input$explorer_variable %in% time_series_vars) {
        updateSelectInput(session, "ts_variable", selected = input$explorer_variable)
      } else {
        updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
      }
    }
  })
  
  # Update from Time Series tab.
  observeEvent(input$ts_variable, {
    if (is.null(selections$from_tab) || selections$from_tab != "map") {
      selections$variable <- input$ts_variable
      selections$from_tab <- "time_series"
      updateSelectInput(session, "variable", selected = input$ts_variable)
      updateSelectInput(session, "explorer_variable", selected = input$ts_variable)
    }
  })
  
  # Synchronize years from Map tab.
  observeEvent(input$year, {
    if (is.null(selections$from_tab) || selections$from_tab != "explorer") {
      selections$year <- input$year
      updateSelectInput(session, "explorer_year", selected = input$year)
    }
  })
  
  # Synchronize years from Data Explorer tab.
  observeEvent(input$explorer_year, {
    if (is.null(selections$from_tab) || selections$from_tab != "map") {
      selections$year <- input$explorer_year
      updateSelectInput(session, "year", selected = input$explorer_year)
    }
  })
  
  # ---- Update Available Years Based on Variable ----
  # Dynamically update year choices based on data availability for the selected variable.
  observeEvent(input$variable, {
    available_years <- sort(unique(data_countries %>%
                                     filter(!is.na(.data[[input$variable]])) %>%
                                     pull(Year)))
    updateSelectInput(session, "year", choices = available_years, selected = max(available_years))
  })
  
  # ---- Limit Display Modes to 2022 ----
  # Restrict per capita/per Catholic modes to 2022 data only.
  observeEvent(input$year, {
    if (isTRUE(as.integer(input$year) == 2022)) {
      updateRadioButtons(
        session, "display_mode",
        choices = list("Absolute values" = "absolute",
                       "Per thousand inhabitants" = "per_capita",
                       "Per thousand Catholics" = "per_catholic"),
        selected = if (input$display_mode %in% c("absolute", "per_capita", "per_catholic")) input$display_mode else "absolute"
      )
    } else {
      updateRadioButtons(
        session, "display_mode",
        choices = list("Absolute values" = "absolute"),
        selected = "absolute"
      )
    }
  })
  
  
  # ---- Render Interactive World Map ----
  # Create the Leaflet map with selected variable data.
  output$map <- renderLeaflet({
    ml <- mode_label()
    req(input$variable, input$year)
    filtered_data <- map_data %>% filter(Year == input$year)
    if (as.integer(input$year) == 2022 && input$display_mode == "per_capita"){
      filtered_data[[input$variable]] <- ifelse(
        !is.na(filtered_data[["Inhabitants in thousands"]]) & filtered_data[["Inhabitants in thousands"]] > 0,
        filtered_data[[input$variable]] / filtered_data[["Inhabitants in thousands"]],
        NA_real_
      )
    } else if (as.integer(input$year) == 2022 && input$display_mode == "per_catholic") {
      filtered_data[[input$variable]] <- ifelse(
        !is.na(filtered_data[["Catholics in thousands"]]) & filtered_data[["Catholics in thousands"]] > 0,
        filtered_data[[input$variable]] / filtered_data[["Catholics in thousands"]],
        NA_real_
      )
    }
    # Ensure valid values for color palette.
    valid_values <- filtered_data[[input$variable]][!is.na(filtered_data[[input$variable]])]
    pal <- if (length(valid_values) > 0) {
      colorNumeric(palette = viridisLite::plasma(256), domain = valid_values, na.color = "transparent")
    } else {
      colorNumeric(palette = viridisLite::plasma(256), domain = c(0, 1), na.color = "transparent")
    }
    
    leaflet(filtered_data, options = leafletOptions(
      maxBounds = list(c(-120, -240), c(120, 240)),
      maxBoundsViscosity = 1,
      zoomControl = FALSE # disable default (top-left)
    )) %>%
      addProviderTiles("CartoDB.Voyager", options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = 0, lat = 30, zoom = 3) %>%
      htmlwidgets::onRender("
    function(el, x) {
      var map = this;
      L.control.zoom({ position: 'topright' }).addTo(map);
    }
  ") %>%
      
      addPolygons(
        fillColor = ~pal(filtered_data[[input$variable]]),
        weight = 1,
        opacity = 0.45,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.6,
        layerId = ~name,
        label = ~lapply(paste0("<strong>", name, "</strong><br/>",
                               switch(ml,
                                      "absolute" = variable_abbreviations[input$variable], # Use abbreviated name
                                      "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."), # Adjusted for brevity
                                      "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath.")),
                               ": ",
                               ifelse(ml != "absolute" & filtered_data[[input$variable]] == 0,
                                      "<0.01",
                                      formatC(filtered_data[[input$variable]], format = "f", digits = 2, big.mark = ","))), htmltools::HTML),
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.8, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = filtered_data[[input$variable]],
                title = paste(switch(ml,
                                     "absolute" = variable_abbreviations[input$variable], # Use abbreviated name
                                     "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."), # Adjusted for brevity
                                     "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath.")),
                              "in", input$year),
                position = "bottomright")
  })
  
  # ---- Time Series Region Selector UI ----
  # Dynamically render region selector based on level (Country or Macroregion).
  output$ts_region_selector <- renderUI({
    if (input$ts_level == "Country") {
      selectInput("ts_regions", "Select country/countries:",
                  choices = sort(unique(data_countries$country)),
                  multiple = TRUE)
    } else {
      selectInput("ts_regions", "Select continent(s):",
                  choices = TARGET_REGIONS,
                  selected = TARGET_REGIONS,
                  multiple = TRUE)
    }
  })
  
  # ---- Handle Map Click Events ----
  # Highlight clicked country and update selections.
  observeEvent(input$map_shape_click, {
    req(input$map_shape_click$id, input$year)
    clicked_country <- input$map_shape_click$id
    filtered_data <- map_data %>% filter(name == clicked_country, Year == input$year)
    
    if (nrow(filtered_data) == 0 || is.na(filtered_data$geometry[1])) {
      showNotification("No data available for this country in the selected year.", type = "warning")
      return()
    }
    
    selected_country(clicked_country)
    updateSelectInput(session, "country_search", selected = clicked_country)
    
    # Update time series only if variable is valid.
    if (input$variable %in% time_series_vars) {
      update_time_series_for_country(clicked_country)
      showNotification(
        paste("Time series updated to show", clicked_country),
        type = "message",
        duration = 3
      )
    }
    
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = filtered_data,
        fill = FALSE, color = "red", weight = 3, opacity = 1, group = "highlight"
      ) %>%
      setView(
        lng = tryCatch(
          st_coordinates(st_centroid(st_union(filtered_data$geometry)))[1],
          error = function(e) 0
        ),
        lat = tryCatch(
          st_coordinates(st_centroid(st_union(filtered_data$geometry)))[2],
          error = function(e) 30
        ),
        zoom = 4
      )
  })
  
  # ---- Handle Country Search Selection ----
  # Highlight selected country from search and update view.
  observeEvent(input$country_search, {
    req(input$country_search, input$year)
    selected_country(input$country_search)
    
    # Update time series only if variable is valid.
    if (input$variable %in% time_series_vars) {
      update_time_series_for_country(input$country_search)
    }
    
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = map_data %>% filter(name == input$country_search, Year == input$year),
        fill = FALSE, color = "red", weight = 3, opacity = 1, group = "highlight") %>%
      setView(
        lng = tryCatch(
          st_coordinates(st_centroid(st_union(map_data %>% filter(name == input$country_search))))[1],
          error = function(e) 0
        ),
        lat = tryCatch(
          st_coordinates(st_centroid(st_union(map_data %>% filter(name == input$country_search))))[2],
          error = function(e) 30
        ),
        zoom = 4
      )
  })
  
  # ---- Sync Selections on Tab Switch to Time Series ----
  # Ensure time series tab reflects current selections when activated.
  observeEvent(input$navbar, {
    if (input$navbar == "Time Series") {
      # Ensure variable is valid for time series.
      valid_variable <- if (!is.null(input$variable) && input$variable %in% time_series_vars) {
        input$variable
      } else {
        time_series_vars[1] # Fallback to first valid time series variable
      }
      
      # Update variable and level.
      updateSelectInput(session, "ts_variable", selected = valid_variable)
      if (!is.null(selected_country()) && selected_country() %in% data_countries$country) {
        updateRadioButtons(session, "ts_level", selected = "Country")
        # Use a longer delay to ensure UI is ready.
        shinyjs::delay(500, {
          updateSelectInput(session, "ts_regions", selected = selected_country())
        })
      } else {
        updateRadioButtons(session, "ts_level", selected = "Macroregion")
        shinyjs::delay(500, {
          updateSelectInput(session, "ts_regions", selected = TARGET_REGIONS)
        })
      }
    }
  })
  
  # ---- Reset Map View and Selections ----
  # Clear highlights and reset view on reset button click.
  observeEvent(input$reset_map, {
    selected_country(NULL)
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      setView(lng = 0, lat = 30, zoom = 3)
    updateSelectInput(session, "country_search", selected = "")
    updateRadioButtons(session, "display_mode", selected = "absolute")
    
    # Reset time series to default.
    updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
    updateRadioButtons(session, "ts_level", selected = "Macroregion")
    updateSelectInput(session, "ts_regions", selected = TARGET_REGIONS)
    
    # Reset data explorer.
    updateSelectInput(session, "explorer_variable", selected = "")
    updateSelectInput(session, "explorer_year", selected = max(data_countries$Year))
  })
  
  # ---- Display Country-Specific Information ----
  # Render HTML with country info based on selection.
  output$country_info <- renderUI({
    ml <- mode_label()
    req(selected_country())
    info <- map_data %>% filter(name == selected_country(), Year == input$year)
    if (as.integer(input$year) == 2022 && input$display_mode == "per_capita") {
      info[[input$variable]] <- ifelse(
        !is.na(info[["Inhabitants in thousands"]]) & info[["Inhabitants in thousands"]] > 0,
        info[[input$variable]] / info[["Inhabitants in thousands"]],
        NA_real_
      )
    } else if (as.integer(input$year) == 2022 && input$display_mode == "per_catholic") {
      info[[input$variable]] <- ifelse(
        !is.na(info[["Catholics in thousands"]]) & info[["Catholics in thousands"]] > 0,
        info[[input$variable]] / info[["Catholics in thousands"]],
        NA_real_
      )
    }
    if (nrow(info) == 0 || is.na(info[[input$variable]][1])) {
      HTML(paste0("<strong>", selected_country(), "</strong><br/>No data available"))
    } else {
      value <- info[[input$variable]][1]
      formatted <- if (ml != "absolute" && value == 0) {
        "<0.01"
      } else {
        format(round(as.numeric(value), ifelse(ml == "absolute", 0, 2)), big.mark = ",", scientific = FALSE)
      }
      HTML(paste0("<strong>", selected_country(), "</strong><br/>",
                  switch(ml,
                         "absolute" = input$variable, # Use full variable name
                         "per_capita" = paste(input$variable, "per thousand inhabitants"),
                         "per_catholic" = paste(input$variable, "per thousand Catholics")),
                  " in ", input$year, ": ", formatted))
    }
  })
  
  # ---- Render Macroregion Histogram ----
  # Generate bar plot for continent-level distribution.
  output$varPlot <- renderPlot({
    
    ml <- mode_label()
    req(input$variable, input$year)
    
    filtered_macro <- data_macroregions %>%
      filter(Year == input$year) %>%
      mutate(macroregion = case_when(
        macroregion %in% c("Central America (Mainland)", "Central America (Antilles)") ~ "Central America",
        macroregion == "South East and Far East Asia" ~ "South & Far East Asia",
        TRUE ~ macroregion
      )) %>%
      filter(!macroregion %in% c("World", "America", "Asia")) %>%
      group_by(macroregion) %>%
      summarise(
        value = switch(ml,
                       "absolute" = sum(.data[[input$variable]], na.rm = TRUE),
                       "per_capita" = ifelse(
                         sum(.data[["Inhabitants in thousands"]], na.rm = TRUE) > 0,
                         sum(.data[[input$variable]], na.rm = TRUE) / sum(.data[["Inhabitants in thousands"]], na.rm = TRUE),
                         NA_real_
                       ),
                       "per_catholic" = ifelse(
                         sum(.data[["Catholics in thousands"]], na.rm = TRUE) > 0,
                         sum(.data[[input$variable]], na.rm = TRUE) / sum(.data[["Catholics in thousands"]], na.rm = TRUE),
                         NA_real_
                       )),
        .groups = "drop"
      )
    
    if (all(is.na(filtered_macro$value))) {
      plot.new()
      text(0.5, 0.5, "No data available at macroregion level", cex = 1.2)
      return()
    }
    
    ggplot(filtered_macro, aes(x = reorder(macroregion, value), y = value)) +
      geom_col(fill = "#f7f7f7", color = "gray80", linewidth = 0.3, alpha = 1) +
      geom_text(
        aes(label = ifelse(ml != "absolute" & value == 0, "<0.01",
                           scales::comma(value, accuracy = ifelse(ml == "absolute", 1, 0.01)))),
        hjust = -0.05, size = 2.9
      ) +
      coord_flip(clip = "off") +
      labs(
        x = "Continents",
        y = NULL,
        title = paste("Continent-level distribution", "in", input$year),
        caption = switch(ml,
                         "absolute" = variable_abbreviations[input$variable], # Use abbreviated name
                         "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."), # Adjusted for brevity
                         "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath."))
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold", margin = margin(b = 10)),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#CCCCCC1A"), # More transparent (~0.25 alpha)
        panel.grid.minor = element_line(colour = "#CCCCCC1A"), # More transparent (~0.1 alpha)
        axis.text.y = element_text(size = 8)
      )
  })
  
  # ---- Render Time Series Plot ----
  # Generate Plotly line chart for time series data.
  output$ts_plot <- renderPlotly({
    req(input$ts_variable, input$ts_regions, input$ts_level)
    
    data_source <- if (input$ts_level == "Country") data_countries else data_macroregions
    region_col <- if (input$ts_level == "Country") "country" else "macroregion"
    
    plot_data <- data_source %>%
      filter(.data[[region_col]] %in% input$ts_regions) %>%
      select(Year, !!sym(region_col), !!sym(input$ts_variable)) %>%
      rename(region = !!sym(region_col), value = !!sym(input$ts_variable)) %>%
      filter(!is.na(value)) # Remove NA values
    
    if (input$ts_level == "Macroregion") {
      plot_data <- data_macroregions %>%
        standardize_macro("macroregion") %>%
        select(Year, macro_simplified, !!sym(input$ts_variable)) %>%
        rename(region = macro_simplified, value = !!sym(input$ts_variable)) %>%
        filter(region %in% input$ts_regions) %>%
        group_by(Year, region) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        filter(!is.na(value))
    }
    
    if (nrow(plot_data) == 0) {
      return(
        plot_ly() %>%
          add_trace(x = numeric(0), y = numeric(0),
                    type = "scatter", mode = "lines+markers",
                    showlegend = FALSE, hoverinfo = "skip") %>%
          layout(title = "No data available for the selected variable and region(s)")
      )
    }
    
    plot_ly(
      data = plot_data,
      x = ~Year,
      y = ~value,
      color = ~region,
      colors = RColorBrewer::brewer.pal(max(3, length(unique(plot_data$region))), "Set2"),
      type = "scatter", # Explicitly specify trace type
      mode = "lines+markers", # Explicitly specify mode
      hoverinfo = "text",
      text = ~paste0(
        "<b>", region, "</b><br>",
        "Year: ", Year, "<br>",
        "Value: ", round(value, 2)
      ),
      line = list(width = 2), # Corrected typo: removed 'list10:'
      marker = list(size = 6, opacity = 0.8)
    ) %>%
      layout(
        title = paste("Time Series of", variable_abbreviations[input$ts_variable]),
        hovermode = "closest",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Absolute Value"),
        legend = list(title = list(text = ifelse(input$ts_level == "Country", "Countries", "Continents")))
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  # ---- Time Series Reset Button ----
  # Reset time series selections to defaults.
  observeEvent(input$reset_ts, {
    updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
    updateRadioButtons(session, "ts_level", selected = "Macroregion")
    updateSelectInput(session, "ts_regions", selected = TARGET_REGIONS)
    
    # Clear country selection.
    selected_country(NULL)
    leafletProxy("map") %>% clearGroup("highlight")
    updateSelectInput(session, "country_search", selected = "")
    
    # Reset data explorer.
    updateSelectInput(session, "explorer_variable", selected = "")
    updateSelectInput(session, "explorer_year", selected = max(data_countries$Year))
  })
  
  # ---- Time Series Download Button ----
  # Create a static ggplot for downloading the time series plot.
  plot_ts_static <- reactive({
    req(input$ts_variable, input$ts_regions, input$ts_level)
    
    data_source <- if (input$ts_level == "Country") data_countries else data_macroregions
    region_col <- if (input$ts_level == "Country") "country" else "macroregion"
    
    plot_data <- data_source %>%
      filter(.data[[region_col]] %in% input$ts_regions) %>%
      select(Year, !!sym(region_col), !!sym(input$ts_variable)) %>%
      rename(region = !!sym(region_col), value = !!sym(input$ts_variable))
    
    if (input$ts_level == "Macroregion") {
      plot_data <- data_macroregions %>%
        standardize_macro("macroregion") %>%
        select(Year, macro_simplified, !!sym(input$ts_variable)) %>%
        rename(region = macro_simplified, value = !!sym(input$ts_variable)) %>%
        filter(region %in% input$ts_regions) %>%
        group_by(Year, region) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    }
    
    
    ggplot(plot_data, aes(x = Year, y = value, color = region)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(title = paste("Time Series of", input$ts_variable),
           x = "Year", y = "Absolute Value", color = ifelse(input$ts_level == "Country", "Countries", "Continents")) +
      theme_minimal(base_size = 13) +
      theme(plot.background = element_rect(fill = "white", colour = "white"),
            panel.background = element_rect(fill = "white", colour = "white")) +
      theme(legend.position = "bottom")
  })
  
  output$download_ts_plot <- downloadHandler(
    filename = function() {
      paste0("time_series_", input$ts_variable, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = plot_ts_static(), width = 10, height = 6, dpi = 300)
    }
  )
  
  
  # ---- Render Data Table for Explorer Tab ----
  # Display data table with optional per capita calculations for 2022.
  output$table <- renderDT({
    if (is.null(input$explorer_variable) || input$explorer_variable == "") {
      return(datatable(data.frame(Message = "Please select a variable to explore.")))
    }
    req(input$explorer_year)
    
    base_cols <- data_countries %>%
      filter(Year == input$explorer_year) %>%
      select(country, Year, !!input$explorer_variable,
             `Inhabitants in thousands`, `Catholics in thousands`)
    
    if (as.integer(input$explorer_year) == 2022) {
      filtered <- base_cols %>%
        mutate(
          `Per 1000 Inhabitants` = ifelse(
            !is.na(`Inhabitants in thousands`) & `Inhabitants in thousands` > 0,
            round(.data[[input$explorer_variable]] / `Inhabitants in thousands`, 3),
            NA_real_
          ),
          `Per 1000 Catholics` = ifelse(
            !is.na(`Catholics in thousands`) & `Catholics in thousands` > 0,
            round(.data[[input$explorer_variable]] / `Catholics in thousands`, 3),
            NA_real_
          )
        ) %>%
        select(-`Inhabitants in thousands`, -`Catholics in thousands`) # drop context cols
    } else {
      filtered <- base_cols %>%
        select(-`Inhabitants in thousands`, -`Catholics in thousands`) # drop context cols
    }
    
    if (!is.null(selected_country()) && selected_country() %in% filtered$country) {
      filtered <- filtered %>% filter(country == selected_country())
    }
    datatable(filtered, options = list(pageLength = 20))
  })
  
  # ---- Update Available Years for Explorer Tab ----
  # Dynamically update years based on variable data availability.
  observeEvent(input$explorer_variable, {
    req(input$explorer_variable)
    available_years <- sort(unique(data_countries %>%
                                     filter(!is.na(.data[[input$explorer_variable]])) %>%
                                     pull(Year)))
    updateSelectInput(session, "explorer_year", choices = available_years, selected = max(available_years))
  })
  
  # ---- Reset Table Filters ----
  # Clear selections in data explorer tab.
  observeEvent(input$reset_table, {
    updateSelectInput(session, "explorer_variable", selected = "")
    updateSelectInput(session, "explorer_year", selected = max(data_countries$Year))
    selected_country(NULL)
    
    # Clear map highlight.
    leafletProxy("map") %>% clearGroup("highlight")
    updateSelectInput(session, "country_search", selected = "")
  })
  
  # ---- Download Data as CSV ----
  # Handler for CSV download from data explorer.
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("data_explorer_", input$explorer_variable, "_", input$explorer_year, ".csv")
    },
    content = function(file) {
      req(input$explorer_variable, input$explorer_year)
      filtered <- data_countries %>%
        filter(Year == input$explorer_year) %>%
        select(country, Year, all_of(input$explorer_variable))
      if (!is.null(selected_country()) && selected_country() %in% filtered$country) {
        filtered <- filtered %>% filter(country == selected_country())
      }
      write.csv(filtered, file, row.names = FALSE)
    }
  )
  
  # ---- Download Data as Excel ----
  # Handler for Excel download from data explorer.
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("data_explorer_", input$explorer_variable, "_", input$explorer_year, ".xlsx")
    },
    content = function(file) {
      req(input$explorer_variable, input$explorer_year)
      filtered <- data_countries %>%
        filter(Year == input$explorer_year) %>%
        select(country, Year, all_of(input$explorer_variable))
      if (!is.null(selected_country()) && selected_country() %in% filtered$country) {
        filtered <- filtered %>% filter(country == selected_country())
      }
      writexl::write_xlsx(filtered, path = file)
    }
  )
}

# ---- Launch the Shiny App ----
shinyApp(ui, server)