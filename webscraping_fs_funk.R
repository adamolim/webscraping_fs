# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# Webscraping de HRM2 - Funktionen der Finanzstatistik
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Goal: download qualitative descriptions of governement functions (Funktionen der Finanzstatistik)
# according to the harmonized accounting manual (HRM2) in an automated way.

# Michele Adamoli 
# Many thanks to Brandon Qorri
# FSO, Neuchâtel

# Some instructions under:
# http://joshuamccrain.com/tutorials/web_scraping_R_selenium.html
# https://cran.r-project.org/web/packages/RSelenium/RSelenium.pdf

# Time start

start <- Sys.time()

# Load packages

library(pacman)

p_load(RSelenium, rvest, tidyverse, beepr) 

# Open the browser

rD <- rsDriver(port= sample(7600)[1], browser=c("firefox"), chromever = NULL)
remDr <- rD[["client"]]

# More information https://stackoverflow.com/questions/33451107/failed-to-connect-to-www-googleapis-com-port-443-network-is-unreachable

# Access to the website for the first time

website <- paste0("https://www.srs-cspcp.ch/de/index-mch2")
remDr$navigate(website)

# Accept cookies

Sys.sleep(0.5) # give the page time to fully load

remDr$findElement(value = '//*[@id="popup-buttons"]/button[2]')$clickElement()

# Function to be looped

web_scrap <- function(fs_funk, langue){
    
# Acces to the page

website <- paste0("https://www.srs-cspcp.ch/", langue, "/index-mch2")
remDr$navigate(website)

# Typ of account: select and click

remDr$findElement(value = '//*[@id="edit-field-ham2-type-target-id"]/option[3]')$clickElement()

# Number of the account
    
remDr$findElement(using = "id", 
                  value = "edit-field-ham2-account-2-value")$sendKeysToElement(list(fs_funk))

# Launch the research

remDr$findElements(using = "id", 
                   value = "edit-submit-index-mch2")[[1]]$clickElement()

# Open the result

remDr$findElement(value = paste0("//*[text()=", fs_funk, "]"))$clickElement()

# Load the page

Sys.sleep(1) # give the page time to fully load

# Save as raw html

html <- remDr$getPageSource()[[1]]

# Parse

parsed_html <- read_html(html)
    
# Extract infos
    
txt_label <- html_nodes(parsed_html, xpath = '//*[@id="dialog-indexmch2"]/div/div/div[2]/article/table/tbody/tr[4]/td/div') %>% 
        html_text()

txt_content <- html_nodes(parsed_html, xpath = '//*[@id="dialog-indexmch2"]/div/div/div[2]/article/table/tbody/tr[5]/td') %>% 
    html_text()

txt_keywords <- html_nodes(parsed_html, xpath = '//*[@id="dialog-indexmch2"]/div/div/div[2]/article/table/tbody/tr[6]/td') %>% 
    html_text()

i <- paste0(fs_funk, "_", langue)

fs_table_partial <- data.frame(Label = c("Libellé", "Contenu", "Mots-clés"), Konto = fs_funk, Langue = langue, Info = c(txt_label, txt_content, txt_keywords))

fs_table <<- bind_rows(fs_table, fs_table_partial)

}

# Dummy

fs_table <- NULL

# Variables à looper

list_lang <- c("de", "fr")
list_funk <- c("411", "412", "413", "421", "422", "431", "432",
               "433", "434", "480", "490", "511", "512", "513", 
               "514", "521", "522", "523", "524", "531", "532",
               "533", "534", "535", "541", "542", "543", "544",
               "545", "551", "552", "559", "560", "571", "572", 
               "573", "579")

# Execute the loop

for(j in list_lang)
    for (i in list_funk)
        web_scrap(fs_funk = i, langue = j)

# Untidy

fs_table_02 <- fs_table %>%
    as_tibble() %>% 
    mutate(Info = case_when(Label == "Mots-clés" ~ str_replace_all(Info, "\\s+", "; "),
                            TRUE ~ Info)) %>% 
    mutate(Info =  str_replace(Info, "; ", "")) %>% 
    mutate(Info =  str_replace(Info, "\n      ", "")) %>%
    mutate(Info = case_when(Label == "Contenu" ~ str_replace_all(Info, "\\s+", " "),
                            TRUE ~ Info)) %>%
    mutate(Info = case_when(Label == "Contenu" ~ str_replace_all(Info, "\n", " "),
                            TRUE ~ Info)) %>%
    spread(key = Langue, value = Info) %>% 
    arrange(Konto)

# Save results

write.table(fs_table_02, row.names = FALSE,
            file = paste0("output/table_fs_funk_", str_sub(start, 1, 10), ".csv"),
            sep = ";",
            fileEncoding = "utf-8")

# Time end and duration

end <- Sys.time()

duration <- end - start
print(duration)

# Acoustic signal

beep(3)

