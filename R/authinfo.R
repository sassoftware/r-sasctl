# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

#' query authinfo file
#' 
#' authinfo copied from SWAT, not sourced because SWAT is not available on CRAN
#' https://github.com/sassoftware/R-swat/blob/e6ea7f2e88e1a2855f91e4af20ef5111cb4f2ea6/R/authinfo.R
#' 
#' @param hostname select the hostname among all inside an authinfo
#' @param username select the username among all inside an authinfo, default to first
#' @param protocol protocol
#' @param authinfo A `character` string that specifies an alternative path to a .authinfo file that is used for authentication. By default, ~/.authinfo is used on Linux and %HOMEDRIVE% \%HOMEPATH%\_authinfo is used on Windows.
#' @return list with authinfo
#' @noRd
#' 

query_authinfo <- function(hostname, username=NULL, protocol=NULL, filepath=NULL )
{
  hosts <- list()
  host <- NULL
  skipnext <- FALSE
  path_sep <- .Platform$path.sep
  file_sep <- .Platform$file.sep
  
  if ( !is.null(username) && username == '' ) {
    username <- NULL
  }
  
  if ( !is.null(protocol) && protocol == '' ) {
    protocol <- NULL
  }
  
  # Construct possible list of authinfo/netrc files
  authinfo_paths <- c('_authinfo.gpg', '.authinfo.gpg', '_netrc.gpg', '.netrc.gpg',
                      '_authinfo', '.authinfo', '_netrc', '.netrc')
  
  ### when using HOMEDRIVE it get path with file separated with \\ instead of /
  ### leading to error
  
  # if ( .Platform$OS.type == 'windows' )
  # {
  #   homedir <- paste(Sys.getenv('HOMEDRIVE'), Sys.getenv('HOMEPATH'), sep='')
  # }
  # else
  # {
    homedir <- Sys.getenv('HOME')
  # }
  
  if ( homedir == '' )
  {
    homedir <- path.expand('~')
  } 
  
  if ( is.null(filepath) )
  {
    if ( nchar(Sys.getenv('AUTHINFO')) > 0 )
    {
      authinfo_paths <- strsplit(Sys.getenv('AUTHINFO'), path_sep)[[1]]
      for ( i in 1:length(authinfo_paths) )
      {
        authinfo_paths[[i]] <- path.expand(authinfo_paths[[i]])
      } 
    }
    else if ( nchar(Sys.getenv('NETRC')) > 0 )
    {
      authinfo_paths <- strsplit(Sys.getenv('NETRC'), path_sep)[[1]]
      for ( i in 1:length(authinfo_paths) )
      {
        authinfo_paths[[i]] <- path.expand(authinfo_paths[[i]])
      } 
    }
    else
    {
      for ( i in 1:length(authinfo_paths) )
      {
        authinfo_paths[[i]] <- paste(homedir, authinfo_paths[[i]], sep=file_sep)
      } 
    }
  }
  else
  {
    authinfo_paths <- filepath
    for ( i in 1:length(filepath) )
    {
      authinfo_paths[[i]] <- path.expand(authinfo_paths[[i]]) 
    }
  }
  
  map_protocol <- function( p )
  {
    if ( is.null(p) ) {
      return( NULL )
    }
    if ( is.numeric(p) ) {
      return( p )
    }
    if ( tolower(p) == 'http' ) {
      p <- 80
    }
    else if ( tolower(p) == 'https' ) {
      p <- 443
    } 
    else {
      p <- as.integer(p)
    }
    return( p )
  }
  
  protocol <- map_protocol(protocol)
  
  for ( i in 1:length(authinfo_paths) )
  {
    if ( !file.exists(authinfo_paths[[i]]) )
    {
      next
    }
    
    info <- scan(authinfo_paths[[i]], character(0), comment.char='#',
                 blank.lines.skip=TRUE, quiet=TRUE)
    
    for ( i in 1:length(info) ) 
    {
      token <- info[[i]]
      
      if ( skipnext )
      {
        skipnext <- FALSE
        next
      }
      
      if ( token == 'host' || token == 'machine' ) 
      {
        if ( !is.null(host) && length(names(host)) )
        {
          hosts[[length(hosts) + 1]] <- host
        }
        host <- list()
        host[['hostname']] <- tolower(info[[i + 1]])
        skipnext <- TRUE
      }
      else if ( token == 'default' )
      {
        if ( !is.null(host) && length(names(host)) )
        {
          hosts[[length(hosts) + 1]] <- host
        }
        host <- list()
        host[['hostname']] <- '*'
      }
      else if ( token == 'password' )
      {
        host[['password']] <- info[[i + 1]]
        skipnext <- TRUE
      }
      else if ( token == 'login' || token == 'user' || token == 'account' ) 
      {
        host[['username']] <- info[[i + 1]]
        skipnext <- TRUE
      }
      else if ( token == 'port' || token == 'protocol' )
      {
        host[['port']] <- map_protocol(info[[i + 1]])
        skipnext <- TRUE
      }
      else
      {
        skipnext <- TRUE
      }
    }
    
    if ( !is.null(host) && length(names(host)) )
    {
      hosts[[length(hosts) + 1]] <- host
    }
    
    for ( i in 1:length(hosts) )
    {
      host <- hosts[[i]]
      
      if ( (host$hostname == hostname || host$hostname == '*') &&
           (is.null(host$username) || is.null(username) || host$username == username) && 
           (is.null(host$port) || is.null(protocol) || host$port == protocol) ) 
      {
        return( host )
      }
    }
  }
  
  return( NULL )
}
