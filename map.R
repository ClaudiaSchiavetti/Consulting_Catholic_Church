# ---- Load the required libraries ---- 

required_packages <- c("shiny", "leaflet", "dplyr", "readr", "sf", "DT", "shinythemes", "rnaturalearth", "rnaturalearthdata", "RColorBrewer", "mapview", "webshot", "writexl", "plotly",
                       "shinyjs")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Ensure PhantomJS is installed
if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs()
}
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


# ---- Docker instructions ---- 

options(shiny.host = "0.0.0.0") 
options(shiny.port = 3838) # Also 8180 is a valid option 

# ---- Set the Working Directory ---- 
path_outputs <- "C:/Users/schia/Documents/LMU/Consulting/App"
#path_outputs <- "C:\\Users\\soffi\\Desktop\\CONSULTING"
setwd(path_outputs)

# ---- Load the data ---- 

data <- read.csv("final_geo_table.csv", check.names = FALSE)

# ---- Define variable abbreviations for display in histogram and map legend ----
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

# Function to check if a column has data only in macroregions
has_country_data <- function(col_data, region_type) {
  # Check if there's any non-NA data for countries
  country_data <- col_data[region_type == "Country"]
  return(sum(!is.na(country_data)) > 0)
}

# Identify columns that have country data
cols_with_country_data <- sapply(names(data), function(col_name) {
  if(is.numeric(data[[col_name]])) {
    has_country_data(data[[col_name]], data$`Region type`)
  } else {
    TRUE
  }
})

# Filter dataset to keep only columns with country data
data_filtered <- data[, cols_with_country_data]

# Select only Macroregions
data_macroregions <- data_filtered %>%
  filter(`Region type` == "Macroregion") %>%
  rename(macroregion = Region)

# Select only Countries
data_countries <- data_filtered %>%
  filter(`Region type` == "Country")

# Load world map from Natural Earth (returns an 'sf' object for mapping)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Fix Somalia shape by merging Somalia and Somaliland
somalia_unified <- world %>%
  filter(admin %in% c("Somalia", "Somaliland")) %>%
  summarise(admin = "Somalia", name = "Somalia", geometry = st_union(geometry))

# Remove original entries and add unified Somalia
world <- world %>%
  filter(!admin %in% c("Somalia", "Somaliland")) %>%
  bind_rows(somalia_unified)

# Merge the data with the world map using country names
map_data <- left_join(world, data_countries, by = c("name" = "Region"))

# ---- Analysis of unmatched names ---- 

unmatched_in_data <- anti_join(data_countries, world, by = c("Region" = "name"))
print(unmatched_in_data$Region)

# Manual corrections 
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

#Create a new column with the corrected country names 
data_countries$country <- ifelse(
  data_countries$Region %in% names(name_corrections),
  name_corrections[data_countries$Region],
  data_countries$Region
)

# Aggregate numeric values for countries that have been merged but keeping NA when necessary
data_countries <- data_countries %>%
  group_by(country, Year) %>%
  summarise(across(
    all_of(setdiff(names(.)[sapply(., is.numeric)], "Year")),
    ~ if (all(is.na(.))) NA_real_ else sum(., na.rm = TRUE)
  ), .groups = "drop")

# Define variables to exclude
excluded_vars <- c("Area in km^2")

# Only include numeric variables with country-level data and not excluded
allowed_variables <- setdiff(
  names(data_countries)[sapply(data_countries, is.numeric) & names(data_countries) != "Year"],
  excluded_vars
)

#Rematching
map_data <- left_join(world, data_countries, by = c("name" = "country"))

#Verification step 
unmatched_after_fix <- anti_join(data_countries, world, by = c("country" = "name"))
print(unique(unmatched_after_fix$country))

# Keep only variables with data for more than one year
time_series_vars <- allowed_variables[
  sapply(allowed_variables, function(var) {
    years <- data_countries %>%
      filter(!is.na(.data[[var]])) %>%
      pull(Year) %>%
      unique()
    length(years) > 1
  })
]


# ---- UI layout ----
# 1. Get the list of unique, non-missing, non-empty country names from the data
all_countries <- sort(unique(data_countries$country))
# Remove any NA values or purely empty strings that might be in the country list
all_countries <- all_countries[!is.na(all_countries) & all_countries != ""]
# 2. Create a named list for selectizeInput choices
# The 'names' of this list are what the user sees in the dropdown.
# The 'values' of this list are what Shiny passes to the server logic.
# For countries, it's common for them to be the same.
country_choices_list <- as.list(all_countries)
names(country_choices_list) <- all_countries # Assign each country name as its own list element's name
final_country_dropdown_choices <- c("Type to search..." = "", country_choices_list)

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
"))
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
             #Time Serie TAB
             tabPanel("Time Series",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("ts_variable", "Select variable:",
                                      choices = time_series_vars,
                                      selected = time_series_vars[1]),
                          radioButtons("ts_level", "Region level:",
                                       choices = c("Macroregion", "Country"),
                                       selected = "Macroregion"),
                          uiOutput("ts_region_selector"),
                        mainPanel(
                          plotlyOutput("ts_plot")
                        )
                      )
             )
             
             
  )
) 

# ---- Server logic ---- 
server <- function(input, output, session) {
  
  # ---- Handle map download with full variable names ----
  output$download_map <- downloadHandler(
    filename = function() {
      paste0("map_export_", input$variable, "_", input$year, 
             switch(input$display_mode, 
                    "absolute" = "", 
                    "per_capita" = "_per_capita", 
                    "per_catholic" = "_per_catholic"), ".png")
    },
    content = function(file) {
      library(htmlwidgets)
      # Filter data for the selected year and apply display mode
      filtered_data <- map_data %>% filter(Year == input$year)
      if (input$display_mode == "per_capita") {
        filtered_data[[input$variable]] <- ifelse(
          !is.na(filtered_data[["Inhabitants in thousands"]]) & filtered_data[["Inhabitants in thousands"]] > 0,
          filtered_data[[input$variable]] / filtered_data[["Inhabitants in thousands"]],
          NA_real_
        )
      } else if (input$display_mode == "per_catholic") {
        filtered_data[[input$variable]] <- ifelse(
          !is.na(filtered_data[["Catholics in thousands"]]) & filtered_data[["Catholics in thousands"]] > 0,
          filtered_data[[input$variable]] / filtered_data[["Catholics in thousands"]],
          NA_real_
        )
      }
      # Ensure valid values for color palette to avoid domain errors
      valid_values <- filtered_data[[input$variable]][!is.na(filtered_data[[input$variable]])]
      pal <- if (length(valid_values) > 0) {
        colorNumeric(palette = viridisLite::plasma(256), domain = valid_values, na.color = "transparent")
      } else {
        colorNumeric(palette = viridisLite::plasma(256), domain = c(0, 1), na.color = "transparent")
      }
      
      leaflet_obj <-leaflet(filtered_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        fitBounds(-110, -40, 120, 65) %>%
        addPolygons(
          fillColor = ~pal(filtered_data[[input$variable]]),
          color = "white", weight = 1, opacity = 0.45, fillOpacity = 0.6,
          label = ~name
        ) %>%
        addLegend(pal = pal, values = filtered_data[[input$variable]],
                  title = paste(switch(input$display_mode,
                                       "absolute" = variable_abbreviations[input$variable], # Use abbreviated name
                                       "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."), # Adjusted for brevity
                                       "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath.")),
                                "in", input$year),
                  position = "bottomright")
      
      # Create a title overlay using prependContent
      # Add title directly inside the map using leaflet::addControl()
      leaflet_obj <- leaflet_obj %>%
        addControl(
          html = paste0("<div style='font-size:20px; font-weight:bold; background-color:rgba(255,255,255,0.7); 
                  padding:6px 12px; border-radius:6px;'>", 
                        switch(input$display_mode,
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
  
  # ---- Initialize reactive values for selections ----
  selected_country <- reactiveVal(NULL)
  ts_selected_regions <- reactive({
    if (!is.null(selected_country()) && selected_country() %in% data_countries$country) {
      return(list(level = "Country", regions = selected_country()))
    } else {
      return(list(level = "Macroregion", regions = unique(data_macroregions$macroregion)))
    }
  })
  selections <- reactiveValues(variable = NULL, year = NULL)
  
  # ---- Synchronize variable and year selections ----
  observe({
    selections$variable <- input$variable
    selections$year <- input$year
  })
  observeEvent(input$variable, {
    updateSelectInput(session, "explorer_variable", selected = input$variable)
    
    if (input$variable %in% time_series_vars) {
      updateSelectInput(session, "ts_variable", selected = input$variable)
      
      isolate({
        selection <- ts_selected_regions()
        updateRadioButtons(session, "ts_level", selected = selection$level)
        updateSelectInput(session, "ts_regions", selected = selection$regions)
      })
    }
  })
  
  observeEvent(input$year, {
    updateSelectInput(session, "explorer_year", selected = input$year)
  })
  
  # ---- Update available years based on variable selection ----
  observeEvent(input$variable, {
    available_years <- sort(unique(data_countries %>%
                                     filter(!is.na(.data[[input$variable]])) %>%
                                     pull(Year)))
    updateSelectInput(session, "year", choices = available_years, selected = max(available_years))
  })
  
  # ---- Render the interactive world map with abbreviated names in legend ----
  output$map <- renderLeaflet({
    req(input$variable, input$year)
    filtered_data <- map_data %>% filter(Year == input$year)
    if (input$display_mode == "per_capita") {
      filtered_data[[input$variable]] <- ifelse(
        !is.na(filtered_data[["Inhabitants in thousands"]]) & filtered_data[["Inhabitants in thousands"]] > 0,
        filtered_data[[input$variable]] / filtered_data[["Inhabitants in thousands"]],
        NA_real_
      )
    } else if (input$display_mode == "per_catholic") {
      filtered_data[[input$variable]] <- ifelse(
        !is.na(filtered_data[["Catholics in thousands"]]) & filtered_data[["Catholics in thousands"]] > 0,
        filtered_data[[input$variable]] / filtered_data[["Catholics in thousands"]],
        NA_real_
      )
    }
    # Ensure valid values for color palette
    valid_values <- filtered_data[[input$variable]][!is.na(filtered_data[[input$variable]])]
    pal <- if (length(valid_values) > 0) {
      colorNumeric(palette = viridisLite::plasma(256), domain = valid_values, na.color = "transparent")
    } else {
      colorNumeric(palette = viridisLite::plasma(256), domain = c(0, 1), na.color = "transparent")
    }
    
    leaflet(filtered_data, options = leafletOptions(
      maxBounds = list(c(-120, -240), c(120, 240)), 
      maxBoundsViscosity = 1,
      zoomControl = FALSE  # disable default (top-left)
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
                               switch(input$display_mode,
                                      "absolute" = variable_abbreviations[input$variable], # Use abbreviated name
                                      "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."), # Adjusted for brevity
                                      "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath.")), 
                               ": ", 
                               ifelse(input$display_mode != "absolute" & filtered_data[[input$variable]] == 0,
                                      "<0.01",
                                      formatC(filtered_data[[input$variable]], format = "f", digits = 2, big.mark = ","))), htmltools::HTML),
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.8, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = filtered_data[[input$variable]], 
                title = paste(switch(input$display_mode,
                                     "absolute" = variable_abbreviations[input$variable], # Use abbreviated name
                                     "per_capita" = paste(variable_abbreviations[input$variable], "per 1000 Pop."), # Adjusted for brevity
                                     "per_catholic" = paste(variable_abbreviations[input$variable], "per 1000 Cath.")),
                              "in", input$year), 
                position = "bottomright")
  })
  #---- Time serie output ----
  output$ts_region_selector <- renderUI({
    if (input$ts_level == "Country") {
      selectInput("ts_regions", "Select country/countries:",
                  choices = sort(unique(data_countries$country)),
                  multiple = TRUE)
    } else {
      selectInput("ts_regions", "Select macroregion(s):",
                  choices = sort(unique(data_macroregions$macroregion)),
                  selected = sort(unique(data_macroregions$macroregion)),
                  multiple = TRUE)
    }
  })
  
  
  
  # ---- Handle map click events to highlight countries ----
  observeEvent(input$map_shape_click, {
    req(input$map_shape_click$id, input$year) # Ensure click ID and year are valid
    clicked_country <- input$map_shape_click$id
    filtered_data <- map_data %>% filter(name == clicked_country, Year == input$year)
    
    # Check if data exists for the clicked country and year
    if (nrow(filtered_data) == 0 || is.na(filtered_data$geometry[1])) {
      showNotification("No data available for this country in the selected year.", type = "warning")
      return()
    }
    
    # Update reactive value and dropdown
    selected_country(clicked_country)
    updateSelectInput(session, "country_search", selected = clicked_country)
    
    # Highlight and center map
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
  
  observeEvent(selected_country(), {
    req(selections$variable)
    
    if (selections$variable %in% time_series_vars) {
      updateSelectInput(session, "ts_variable", selected = selections$variable)
      
      # Add delay here to fix country selection
      shinyjs::delay(100, {
        updateRadioButtons(session, "ts_level", selected = "Country")
        updateSelectInput(session, "ts_regions", selected = selected_country())
      })
    }
  })
  
  
  
  # ---- Handle country search selection ----
  observeEvent(input$country_search, {
    req(input$country_search, input$year)
    selected_country(input$country_search)
    
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = map_data %>% filter(name == input$country_search, Year == input$year),
        fill = FALSE, color = "red", weight = 3, opacity = 1, group = "highlight") %>%
      setView(
        lng = st_coordinates(st_centroid(st_union(map_data %>% filter(name == input$country_search))))[1],
        lat = st_coordinates(st_centroid(st_union(map_data %>% filter(name == input$country_search))))[2],
        zoom = 4)
  })
  
  # ---- Reset map view and clear selections ----
  observeEvent(input$reset_map, {
    selected_country(NULL)
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      setView(lng = 0, lat = 30, zoom = 3)
    updateSelectInput(session, "country_search", selected = "")
    updateRadioButtons(session, "display_mode", selected = "absolute")
  })
  
  # ---- Display country-specific information with full variable names ----
  output$country_info <- renderUI({
    req(selected_country())
    info <- map_data %>% filter(name == selected_country(), Year == input$year)
    if (input$display_mode == "per_capita") {
      info[[input$variable]] <- ifelse(
        !is.na(info[["Inhabitants in thousands"]]) & info[["Inhabitants in thousands"]] > 0,
        info[[input$variable]] / info[["Inhabitants in thousands"]],
        NA_real_
      )
    } else if (input$display_mode == "per_catholic") {
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
      formatted <- if (input$display_mode != "absolute" && value == 0) {
        "<0.01"
      } else {
        format(round(as.numeric(value), ifelse(input$display_mode == "absolute", 0, 2)), big.mark = ",", scientific = FALSE)
      }
      HTML(paste0("<strong>", selected_country(), "</strong><br/>", 
                  switch(input$display_mode,
                         "absolute" = input$variable, # Use full variable name
                         "per_capita" = paste(input$variable, "per thousand inhabitants"),
                         "per_catholic" = paste(input$variable, "per thousand Catholics")), 
                  " in ", input$year, ": ", formatted))
    }
  })
  
  # ---- Render macroregion histogram with abbreviated name in caption ----
  output$varPlot <- renderPlot({
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
        value = switch(input$display_mode,
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
      geom_col(fill = "#f7f7f7", color = "gray80", linewidth = 0.3, alpha = 1)+
      geom_text(
        aes(label = ifelse(input$display_mode != "absolute" & value == 0, "<0.01", 
                           scales::comma(value, accuracy = ifelse(input$display_mode == "absolute", 1, 0.01)))),
        hjust = -0.05, size = 2.9
      ) +
      coord_flip(clip = "off") +
      labs(
        x = "Continents",
        y = NULL,
        title = paste("Continent-level distribution", "in", input$year),
        caption = switch(input$display_mode,
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
        panel.grid.major.y = element_line(colour = "#CCCCCC1A"),  # More transparent (~0.25 alpha)
        panel.grid.minor = element_line(colour = "#CCCCCC1A"),    # More transparent (~0.1 alpha)
        axis.text.y = element_text(size = 8)
      )
  })
  
  #---- Time Serie plot ---- 
  output$ts_plot <- renderPlotly({
    req(input$ts_variable, input$ts_regions, input$ts_level)
    
    data_source <- if (input$ts_level == "Country") data_countries else data_macroregions
    region_col <- if (input$ts_level == "Country") "country" else "macroregion"
    
    plot_data <- data_source %>%
      filter(.data[[region_col]] %in% input$ts_regions) %>%
      select(Year, !!sym(region_col), !!sym(input$ts_variable)) %>%
      rename(region = !!sym(region_col),
             value = !!sym(input$ts_variable))
    
    plot_ly(plot_data, x = ~Year, y = ~value, color = ~region,
            type = 'scatter', mode = 'lines+markers+text',
            text = ~region,
            textposition = 'top right',
            hoverinfo = 'text',
            hovertext = ~paste0("<b>", region, "</b><br>Year: ", Year, "<br>Value: ", round(value, 2))
    ) %>%
      layout(title = paste("Time Series of", input$ts_variable),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Absolute Value"),
             legend = list(title = list(text = "Continents")))
  })
  
  # ---- Render data table for explorer tab ----
  output$table <- renderDT({
    if (is.null(input$explorer_variable) || input$explorer_variable == "") {
      return(datatable(data.frame(Message = "Please select a variable to explore.")))
    }
    req(input$explorer_year)
    filtered <- data_countries %>% filter(Year == input$explorer_year) %>% select(country, Year, 
                                                                                  !!input$explorer_variable, 
                                                                                  `Inhabitants in thousands`, 
                                                                                  `Catholics in thousands`) %>%
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
      )
    
    
    if (!is.null(selected_country()) && selected_country() %in% filtered$country) {
      filtered <- filtered %>% filter(country == selected_country())
    }
    datatable(filtered, options = list(pageLength = 20))
  })
  
  # ---- Update available years for explorer tab ----
  observeEvent(input$explorer_variable, {
    req(input$explorer_variable)
    available_years <- sort(unique(data_countries %>%
                                     filter(!is.na(.data[[input$explorer_variable]])) %>%
                                     pull(Year)))
    updateSelectInput(session, "explorer_year", choices = available_years, selected = max(available_years))
  })
  
  observeEvent(input$explorer_variable, {
    if (input$explorer_variable %in% time_series_vars) {
      updateSelectInput(session, "ts_variable", selected = input$explorer_variable)
      
      isolate({
        selection <- ts_selected_regions()
        updateRadioButtons(session, "ts_level", selected = selection$level)
        updateSelectInput(session, "ts_regions", selected = selection$regions)
      })
    }
  })
  
  # ---- Reset table filters ----
  observeEvent(input$reset_table, {
    updateSelectInput(session, "explorer_variable", selected = "")
    updateSelectInput(session, "explorer_year", selected = max(data_countries$Year))
    selected_country(NULL)
  })
  
  # ---- Download data as CSV ----
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
  
  # ---- Download data as Excel ----
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

# ---- Launch the app ----
shinyApp(ui, server)
