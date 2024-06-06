#' Example patient data
#'
#' @description
#' A data frame of patient lesion data, formatted for input to `bodymap_df`.
#'
#' @format ## `example_patient_data`
#' A data frame with 14 rows and 5 columns:
#' \describe{
#'   \item{lesion_date}{Date of lesion diagnosis}
#'   \item{lookup_name}{Value to match against `lesion_lookup` site list.
#'   May be the same value as `site_description`.}
#'   \item{site_description}{Descriptive text for lesion site used in plot labels}
#'   \item{laterality}{Side of body on which lesion occurred (Bilateral, Left, Midline, Right,
#'   Not a paired site, Unknown)}
#'   \item{lesion_type}{Identifies lesion as primary or metastatic (Primary, Metastatic)}
#' }
#' @name example_patient_data
NULL

#' Lookup list for lesion plot locations
#'
#' @description
#'
#' A list of lesion sites for both primary and metastatic lesion types. Used for matching against
#'  input data to determine plot location for input lesions.
#'
#' @format ## `lesion_lookup`
#' A data frame with 1,361 rows and 7 columns:
#' \describe{
#'   \item{Type}{Identifies lesion as primary or metastatic (Primary, Metastatic)}
#'   \item{Site}{Value to match against `lookup_name` input column. Primary sites use ICD-O-3
#'   morphology terms. Metastatic sites use custom site terms.}
#'   \item{ICD Code}{ICD-O-3 morphology code. Not currently used for matching; provided
#'   as reference.}
#'   \item{y}{Y plot location value.}
#'   \item{x, x_left,x_right}{X plot location. Lesions with lateralities of Midline,
#'   Not a paired site, and Unknown use x. Otherwise, left or right location used.}
#' }
#' @source Lesion site development source: https://github.com/TheMillerLab/BodyMapR Plot locations
#' reviewed for alignment with modified body map. Manual adjustments performed as required.
"lesion_lookup"
