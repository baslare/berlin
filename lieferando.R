require(rvest)
require(tidyverse)
require(RSelenium)


plz_html <- html_session("https://www.dasoertliche.de/Themen/Postleitzahlen/Berlin.html")

tbl <- plz_html %>% html_node("#tl-plz") %>% html_table()

tbl <- tbl %>% filter(Ortsname == "Berlin")

driver <- rsDriver(port=4571L,browser = "firefox")
maind <- driver$client


maind$navigate("https://www.lieferando.de/lieferservice/essen/berlin-10115")

button_jetzt <- maind$findElement("class","_20KxW")
button_jetzt$clickElement()

list_elements <- maind$findElements("class","_2ro375")
#list_elements[[length(list_elements)]]$
  
maind$sendKeysToActiveElement(sendKeys = "page_down")

list_elements[[1]]$sendKeysToElement(sendKeys = "page_down")


maind$deleteAllCookies()
maind$refresh()
