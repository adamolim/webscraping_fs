# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# Webscraping de HRM2 - Ausgabenart in der Finanzstatistik
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Goal: download qualitative descriptions of governement type of expensees (fs_art)
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

rD <- rsDriver(
    port = sample(7600)[1],
    browser = c("firefox"),
    chromever = NULL
)

remDr <- rD[["client"]]

# Access to the website for the first time

website <- paste0("https://www.srs-cspcp.ch/de/index-mch2")
remDr$navigate(website)

# Accept cookies

Sys.sleep(1) # give the page time to fully load

remDr$findElement(value = '//*[@id="popup-buttons"]/button[2]')$clickElement()

# Define a function to be looped

web_scrap <- function(account_type, langue, page) {
    # Access the website
    
    # Identify the pages
    
    if (account_type == "invest") {
        website <-
            paste0(
                "https://www.srs-cspcp.ch/",
                langue ,
                "/index-mch2?field_ham2_type_target_id=3676&field_ham2_category_target_id=All&field_ham2_keywords_fr_target_id&field_ham2_keywords_it_target_id&field_ham2_keywords_en_target_id&field_ham2_keywords_de_target_id&field_ham2_title_value=&field_ham2_account_2_value=&page=",
                page
            )
    } else if (account_type == "erfolg") {
        website <-
            paste0(
                "https://www.srs-cspcp.ch/",
                langue ,
                "/index-mch2?field_ham2_type_target_id=3653&field_ham2_category_target_id=All&field_ham2_keywords_fr_target_id&field_ham2_keywords_it_target_id&field_ham2_keywords_en_target_id&field_ham2_keywords_de_target_id&field_ham2_title_value=&field_ham2_account_2_value=&page=",
                page
            )
    }
    
    # Browse
    
    remDr$navigate(website)
    
    # https://www.srs-cspcp.ch/fr/index-mch2?field_ham2_type_target_id=3653&field_ham2_category_target_id=All&field_ham2_keywords_fr_target_id=&field_ham2_keywords_it_target_id=&field_ham2_keywords_en_target_id=&field_ham2_keywords_de_target_id=&field_ham2_title_value=&field_ham2_account_2_value=
    # https://www.srs-cspcp.ch/fr/index-mch2?field_ham2_type_target_id=3653&field_ham2_category_target_id=All&field_ham2_keywords_fr_target_id&field_ham2_keywords_it_target_id&field_ham2_keywords_en_target_id&field_ham2_keywords_de_target_id&field_ham2_title_value=&field_ham2_account_2_value=&page=0
    #
    # Save the page
    
    Sys.sleep(1) # give the page time to fully load
    html <- remDr$getPageSource()[[1]]
    
    # Extract informations
    
    fs_table_00 <- read_html(html) %>% # parse HTML
        html_element(xpath = '/html/body/div[1]/div/div/div/div[2]/div[2]/main/section/div/div/div/div[1]/div[2]/div') %>%
        html_table()
    
    # Clean it
    
    fs_table_01 <- set_names(fs_table_00, "title", "item", "level") %>%
        mutate(langue = langue) %>%
        mutate(account_type = account_type) %>%
        mutate(page = page)
    
    # Bind
    
    fs_table <<- bind_rows(fs_table, fs_table_01)
    
}

# Dummy

fs_table <- NULL


# Exécuter la loop

for (l in c("invest", "erfolg"))
    for (i in c("de", "fr"))
        for (j in 0:if (l == "invest") {
            5
        } else if (l == "erfolg") {
            16
        })
            web_scrap(account_type = l,
                      langue = i,
                      page = j)

# Save results

write.table(
    fs_table,
    row.names = FALSE,
    file = paste0("output/table_fs_art_", str_sub(start, 1, 10), ".csv"),
    sep = ";",
    fileEncoding = "utf-8"
)

# Time end and duration

end <- Sys.time()

duration <- end - start
print(duration)

# Acoustic signal

beep(3)
