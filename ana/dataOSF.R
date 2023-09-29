#' Download data from OSF and unzip into folder structure in \code{data/} folder.
#' 
#' @param repository OSF repository to download from (string of 5 characters)
#' @param foldername repository data folder (data)
#' @param overwrite (Boolean) Whether files should be overwritten if already there.
#' @param folder Folder in the current working directory to store files in
#' @param unzip (Boolean) Whether or not to unzip zip files.
#' @param removezips (Boolean) Whether or not to remove zip files after unzipping.
#' @return empty
#' @import osfr
#' @export
downloadOSFdata <- function(repository,foldername,folder,overwrite=TRUE,unzip=TRUE,removezips=TRUE) {
  
  # get the 5-character repository name:
  slash_idx <- as.numeric(gregexpr(pattern ='/',repository)[[1]])
  if (rev(slash_idx)[1] == nchar(repository)) {
    repository <- substr(repository,1,rev(slash_idx)[1]-1)
    slash_idx <- as.numeric(gregexpr(pattern ='/',repository)[[1]])
    if (length(slash_idx)>0) {
      repository <- substr(repository,rev(slash_idx)[1]+1,nchar(repository))
    }
  }
  # connect to the repository:
  mainOSFnode <- osfr::osf_retrieve_node(repository)
  
  
  if (overwrite) {
    conflict <- 'overwrite'
  } else {
    conflict <- 'skip'
  }
  
  # foldername needs to have a trailing back slash:
  if (substr(foldername,nchar(foldername),nchar(foldername)) != "\\" ) {
    foldername <- sprintf('%s\\',foldername)
  }
  
  # list files in the OSF folder:
  files <- osfr::osf_ls_files(mainOSFnode, path=sprintf('%s',foldername), n_max=500)
  
  cat('files on OSF:\n')
  print(files)
  
  cat(sprintf('downloading %s: please be patient\n',foldername))
  osfr::osf_download(x = files, path = sprintf('%s/%s', folder, foldername), conflicts = conflict)
  
  if (unzip) {
    # check if it is a zip file:
    for(filename in files$name){
      ext <- tools::file_ext(filename)
      if (ext == 'zip' & file.exists(sprintf('%s/%s/%s',folder,foldername,filename))) {
        utils::unzip(zipfile=sprintf('%s/%s/%s',folder,foldername,filename),
                     exdir=sprintf('%s/%s',folder,foldername))
        if (removezips & file.exists(sprintf('%s/%s/%s',folder,foldername,filename))) {
          file.remove(sprintf('%s/%s/%s',folder,foldername,filename))
        }
        
      }
    }
    
  }
  
}