download_from_Gorilla <-function(output, url, myGorillaemail){
  if (!require(downloader)) {
    stop("downloader not installed")
  } else if (!require(keyring)){
    stop("keyring not installed")
  } else if (!require(httr)){
    stop("httr not installed") 
  } else {
    print("------ start download -------")
  };
  
  #---------------------- get credentials and login ----------------------------#
  login <- list(
    email = myGorillaemail, 
    password = keyring::key_get("Gorilla", myGorillaemail))
  login_res <- POST("https://gorilla.sc/api/login", body = login, encode = "form")
  
  #---------------------- cycle over the list of urls --------------------------#
  for (i in 1:length(url)){
    file_end <- ""
    experiment_download_url <- GET(url[i])
    content_type_you_need_to_save <- experiment_download_url$headers$`content-type`
    
    #------------------------- check content type -------------------------------# 
    if (content_type_you_need_to_save == "text/csv") {
      file_end <- ".csv"
    } else if (content_type_you_need_to_save == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
      file_end <- ".xlsx"
    } else if (content_type_you_need_to_save == "audio/wav") {
      file_end <- ".weba"
    } else if (content_type_you_need_to_save == "video/webm;codecs=vp8") {
      file_end <- ".webm"
    }
    
    file_download_url <- experiment_download_url$url
    file_name_saver <- paste0(substring(as.character(url[i]),54,nchar(as.character(url[i]))))
    
    #download the file
    downloader::download(file_download_url, paste0(output, file_name_saver), mode = "wb", quiet = T)
    print(paste0("file ",i))
  }
  print("------ download done -------")
}