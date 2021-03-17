
create_report <- function(pub_date, latest_fy){
  
  `%>%` <- magrittr::`%>%`
  
  # Create Report and Add Table of Contents
  rmarkdown::render(
    here::here("publication", "markdown", "report.Rmd"), 
    output_file = here::here("publication", "output", pub_date,
                             paste0(pub_date, "_temp-report.docx")),
    envir = new.env()
  )
  
  officer::read_docx(here::here("publication", "output", pub_date,
                                paste0(pub_date, "_temp-report.docx"))) %>%
    officer::cursor_reach(keyword = "Introduction") %>%
    officer::body_add_toc(pos = "before") %>%
    officer::body_add_par("Contents", pos = "before", 
                          style = "Contents Header") %>%
    officer::cursor_reach(keyword = "Introduction") %>%
    officer::body_add_break(pos = "before") %>%
    print(here::here("publication", "output", pub_date,
                     paste0(pub_date, "_temp-report-2.docx")))
  
  # Cover Page
  cover_page <- officer::read_docx(
    here::here("publication", "markdown", "templates", 
               "report-cover-page.docx")) %>%
    officer::body_replace_all_text("Insert publication title here",
                                   "Dementia Post-Diagnostic Support") %>%
    officer::body_replace_all_text(
      "Subtitle",
      paste("Local Delivery Plan Standard - Figures to", latest_fy)) %>%
    officer::body_replace_all_text(
      "DD Month YYYY", 
      stringr::str_trim(format(pub_date, "%e %B %Y"))) %>%
    officer::body_replace_all_text("DATE HERE", 
                                   format(pub_date, "%d/%m/%Y"))
  
  # Combine Cover and Report
  cover_page %>%
    officer::cursor_end() %>%
    officer::body_add_break() %>%
    officer::body_add_docx(
      here::here("publication", "output", pub_date,
                 paste0(pub_date, "_temp-report-2.docx"))) %>%
    officer::headers_replace_all_text("DATE HERE",
                                      format(pub_date, "%d/%m/%Y")) %>%
    print(here::here("publication", "output", pub_date, 
                     paste0(pub_date, "_dementia-pds_report.docx")))
  
  # Remove Temporary Files
  unlink(here::here("publication", "output", pub_date,
                    paste0(pub_date, 
                           c("_temp-report.docx",
                             "_temp-report-2.docx"))))
  
}


### END OF SCRIPT ###