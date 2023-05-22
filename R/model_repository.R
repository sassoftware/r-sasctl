# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

#' Register a zip file inside model manager
#' 
#' Registers a zip formatted model in SAS Model Manager.
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param file path to file
#' @param name model name that will be used when registering
#' @param project `MMproject` object, project ID or project name. If name, will try to find a single project with exact name match. See `exact` parameter 
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @param type string, pmml, spk, zip, astore or CAS,
#' @param force Boolean, force the creation of project if unavailable
#' @param version This parameter indicates to create a new project version, use the latest version, or use an existing version to import the model into. Valid values are 'NEW', 'LATEST', or a number.
#' @param force_pmml_translation default is TRUE, set to false will upload pmml as is, but may not work properly. Only if `type = "pmml"`
#' @param model_function [sasctl::create_project()] parameter of model function of the created project if `force = TRUE`. Valid values: analytical, classification, cluster, forecasting, prediction, Text categorization, Text extraction, Text sentiment, Text topics, transformation
#' @param additional_project_parameters list of additional parameters to be passed to  [sasctl::create_project()] `additional_parameters` parameter
#' @param project_description description string of additional parameters to be passed to [sasctl::create_project()] `description` parameter
#' @param ... pass to `sasctl::vPOST()` function
#' 
#' @return a `MMmodel` class list
#' 
#' @examples 
#' 
#' \dontrun{
#' ### Building and registering a pmml model
#' 
#' library("pmml")
#' 
#' hmeq <- read.csv("https://support.sas.com/documentation/onlinedoc/viya/exampledatasets/hmeq.csv",
#'                  stringsAsFactors = TRUE)
#' 
#' hmeq <- na.omit(hmeq)
#' 
#' model1 <- lm(BAD ~ ., hmeq)
#' 
#' 
#' saveXML(pmml(model1, model.name="General_Regression_Model",
#'              app.name="Rattle/PMML",
#'              description="Linear Regression Model"),
#'              "my_model.pmml")
#'         
#' output <- register_model(session = sess,
#'                         file = "my_model.pmml",
#'                         name = "R_LinearModel",
#'                         type = "pmml", 
#'                          ## Project UUID example
#'                         projectId = "2322da44-9b24-43f6-96f4-456456231")
#' 
#' output
#' 
#' ### Bulding and registering an astore model with SWAT
#' 
#' library("swat")
#' 
#' conn <- swat::CAS(hostname = "https://my.sas.server", ## change if needed
#'             port = 8777,
#'             username = "sasuser",
#'             password = "!s3cr3t")
#' 
#' swat::loadActionSet(conn, "astore")
#' swat::loadActionSet(conn, "decisionTree")
#' 
#' 
#' hmeq <- read.csv("https://support.sas.com/documentation/onlinedoc/viya/exampledatasets/hmeq.csv")
#' castbl <- cas.upload.frame(conn, hmeq)
#' 
#' colinfo <- cas.table.columnInfo(conn, table = castbl)$ColumnInfo
#' target <- colinfo$Column[1]
#' inputs <- colinfo$Column[-1]
#' nominals <- c(target, subset(colinfo, Type == 'varchar')$Column)
#' 
#' dt <- cas.decisionTree.dtreeTrain(conn,
#'                                   table = castbl,
#'                                   target = target,
#'                                   inputs = inputs,
#'                                   nominals = nominals,
#'                                   varImp = TRUE,
#'                                   ## save astore
#'                                   saveState = list(name = "dt_model_astore",
#'                                                    replace = TRUE), 
#'                                   casOut = list(name = 'dt_model', 
#'                                                 replace = TRUE)
#' )
#' dt
#' 
#' ## downloading astore
#' astore_blob <- cas.astore.download(conn,
#'                                    rstore =  list(name = "dt_model_astore")
#' )
#' 
#' ## saving astore as binary file
#' astore_path <- "./rf_model.astore"
#' con <- file(astore_path, "wb")
#' ### file is downloaded as base64 encoded
#' writeBin(object = jsonlite::base64_dec(astore_blob$blob$data), 
#'          con = con, useBytes = T)
#'          
#' close(con)
#'
#' ### sasctl connecting
#' sess <- session(hostname = "https://my.sas.server",
#'                 username = "sasuser",
#'                 password = "!s3cr3t")
#' 
#' output <- register_model(session = sess,
#'                           file = astore_path,
#'                           name = "R_swatModel",
#'                           type = "astore",
#'                           projectId = "a0c2923b-67e9-4e7f-b5d0-549a04103523") 
#' 
#' ### Registering a Zip model
#' 
#' output <- register_model(session = sess,
#'                         file = "model.zip",
#'                         name = "R_LinearModel", 
#'                         type = "zip",
#'                         projectId = "2322da44-9b24-43f6-96f4-456456231") 
#' 
#' output
#' }
#' 
#' @export

register_model <- function(session, file, name, project, type,
                           force_pmml_translation = TRUE, exact = TRUE, 
                           force = FALSE, model_function = NULL,
                           additional_project_parameters = NULL,
                           version = 'latest',
                           project_description = "R SASctl automatic project",
                           ...) {
  
  
  ### TODO
  ### Additional_parameters will need to be handled in a different call
  ### trying to define them while uploading with the model are just ignored
  ### automatically create project and model version
  
  if (missing(file)) {
    stop("A path to pmml file must be set")
  }
  
  if (missing(name)) {
    stop("A name must be given to the model")
  }
  
  if (missing(project)) {
    stop("Automatic project creation not implemented")
  }
  
  if (missing(type)) {
    stop("Type must be defined as SPK, ZIP, ASTORE, PMML or CAS")
  }


  type <- tolower(type)
  
  ### astore treatment 
  
  if (type == "astore") {
      
    if ( !(tools::file_ext(file) %in% c("sasast", "ast", "astore")) ) {
      stop("For type = astore, file must be .sasast, .ast or .astore")
    }
    
      if (is.raw(file)) {
        if (session$platform$release == "V04") {
          sfile <- file 
        } else {
          stop("Raw object astore is not supported in Viya 3.5 or lower.")
        }
      } else {
        sfile <- httr::upload_file(file)
      }
  }
  
  ### PMML treatment 
  
  if (type == "pmml") {
    
    if ( !(tools::file_ext(file) %in% c("pmml", "xml")) ) {
      stop("For type = pmml, file must be .pmml, .xml")
    }
    
    if (force_pmml_translation) {
      if (!any(grepl("4-2|4\\.2|4_2", readLines(file, 3)))) {
        
        warning("The pmml file version is being converted to 4.2, newer models may be not compatible or not be properly registered")
        
        new_file_path <- paste0(tools::file_path_sans_ext(file), 
                                "_converted42.pmml")
        
        sasctl::convert_to_pmml42(file_in = file,
                                  file_out = new_file_path)
        
        sfile <- httr::upload_file(new_file_path)
        
      }
    } else {
      sfile <- httr::upload_file(file)
    }
  }
  
  ### ZIP file treatment 
  
  if (type == "zip") {
    
    if ( !(tools::file_ext(file) %in% c("zip")) ) {
      stop("For type = zip, file must be .zip")
    }
    if (tools::file_ext(file) != "zip" ) {
      stop("The file is not .zip")
    }
    
    if (session$platform$release == "V03" | is.null(session$platform$minor)) {
      sfile <- list(file = httr::upload_file(file))
    }
    
    else if (session$platform$major >= 2023 & 
             session$platform$minor >= 3 ) {

      sfile <- httr::upload_file(file)} 
    
    else {
      ### releases older than 2023.3 (and 3.5) requires this format
      sfile <- list(file = httr::upload_file(file))
    }
    
  }
  
  
  ### spk file treatment 
  
  if (type == "spk") {
    
    if (tools::file_ext(file) != "spk" ) {
      stop("The file is not .spk")
    }
    
    sfile <- httr::upload_file(file)
  }
  
  ### force create project
  
  if (force) {
    
    if (!project_exists(session, project)) {
      
      if (!is.character(project)) {
        stop("New project name is not a character string")
      }
      
      if (uuid::UUIDvalidate(project)) {
        stop("Project with current uuid doesn't exist. uuid string is not a valid name for a new project")  
      } 
      
      
      project <- create_project(session, 
                                name = project, 
                                description = project_description, 
                                model_function = model_function,
                                additional_parameters = additional_project_parameters, 
                                ...)
      
      message(paste("The project with the name", project$name, "has been successfully created"))
      
      forced_created_project <- TRUE
    } else {
      
      forced_created_project <- FALSE
    }
    
  } else {
    
    forced_created_project <- FALSE
  }
  
  project_id <- single_id_from_object(session, project, class = "MMproject", type = "project", exact = exact)
  
  ### payload and query
  
    query <- list(name = name, 
                  type = type, 
                  projectId = project_id,
                  'function' = model_function,
                  versionOption = version)
    
    payload <- sfile
  
  ### Model upload
  
  model <- vPOST(session, 
                 path = "modelRepository/models",
                 query = query,
                 fragment = "octetStream",
                 payload = payload,
                 encode = "multipart",
                 ...
  )
  

  model <- create_sasctl_obj(model$items, "MMmodel")
  
  ### this will transform lists to DF, make similar to MMmodel
  model$links <- model$links[[1]]
  model$inputVariables <- model$inputVariables[[1]]
  model$outputVariables <- model$outputVariables[[1]]
  model$modelVersions <- model$modelVersions[[1]]
  model$tags <- model$tags[[1]]
  model$globalTags <- model$globalTags[[1]]
  
  ### additional project updates
  
  if (forced_created_project) {
    
    variable_columns <- c("name", "length", "type", "level", "role")
    
    projectVars <- update_project_variables(session,
                                            project,
                                            sasctl_vars = rbind(  
                                              model$inputVariables[variable_columns],
                                              model$outputVariables[variable_columns]
                                            ),
                                            ...
    )
    
  }
  
  return(model)
}

#' List Projects
#' 
#' Returns a list of projects from Model Manager
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param filters list of of names vectors for filter parameters (createdBy, modifiedBy, name). By default it will use the `contains` operation. For more info visit `https://developer.sas.com/apis/rest/Topics/#filters`.
#' @param limit maximum number of projects to return
#' @param start the index of the first project to return
#' @param exact boolean, If the filter query should use "contains" for partial match or "eq" for exact match
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @return A `data.frame` with the list of projects
#' @examples
#' 
#' \dontrun{
#' projects <- list_projects(sess, filter = list(createdBy = "creatorUser", name = "projectName"))
#' projects
#' }
#' 
#' @export

list_projects <-  function(session, start = 0, limit = 10, filters = list(), exact = FALSE, ...) {

  ## creating filter query eg: "or(or(contains(createdBy, '123')),or(contains(name, '123')))"
  query <- build_filter_query(filters, exact = exact)
  
  ## adding query for limits and start index
  query <- append(query, list(start = start, limit = limit))  
  
  projects <- vGET(session,
              path = "modelRepository/projects",
              query = query,
                ...
              )
  if (length(projects$items) == 0) {
    return(list())
  }
  
  projects <- create_sasctl_obj(projects$items, "MMprojectList", append = TRUE)
  
  return(projects)
}

#' List Repositories
#' 
#' Returns a list of model repositories. This is required to be able to create model projects
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param filters list of of names vectors for filter parameters (createdBy, modifiedBy, name). By default it will use the `contains` operation. For more info visit `https://developer.sas.com/apis/rest/Topics/#filters`.
#' @param limit maximum number of repositories to return
#' @param start the index of the first project to return
#' @param exact boolean, If the filter query should use "contains" for partial match or "eq" for exact match
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @return A `data.frame` with the list of repositories
#' @examples
#' 
#' \dontrun{
#' repositories <- list_repositories(sess, filter = 
#'                                             list(createdBy = "creatorUser",
#'                                                  name = "projectName")
#'                                                  )
#' repositories
#' }
#' 
#' @export

list_repositories <-  function(session, start = 0, limit = 10, filters = list(), exact = FALSE, ...) {
  
  ## creating filter query eg: "or(or(contains(createdBy, '123')),or(contains(name, '123')))"
  query <- build_filter_query(filters, exact = exact)
  
  ## adding query for limits and start index
  query <- append(query, list(start = start, limit = limit))  
  
  repositories <- vGET(session,
                   path = "modelRepository/repositories",
                   query = query,
                   ...
  )
  if (length(repositories$items) == 0) {
    return(list())
  }
  
  repositories <- create_sasctl_obj(repositories$items, "MMrepository", append = TRUE)
  
  return(repositories)
}

#' List Models
#' 
#' Returns a list of models from Model Manager
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param filters list of of names vectors for filter parameters (createdBy, modifiedBy, name). By default it will use the `contains` operation. For more info visit `https://developer.sas.com/apis/rest/Topics/#filters`.
#' @param limit maximum number of models to return
#' @param start the index of the first model to return
#' @param exact boolean, If the filter query should use "contains" for partial match or "eq" for exact match
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @return A `data.frame` with the list of models
#' @examples
#' 
#' \dontrun{
#' models <- list_models(sess, filter = list(createdBy = "creatorUser", name = "modelName"))
#' models
#' }
#' 
#' @export

list_models <-  function(session, start = 0, limit = 10, filters = list(), exact = FALSE, ...) {
  
  ## creating filter query eg: "or(or(contains(createdBy, '123')),or(contains(name, '123')))"
  query <- build_filter_query(filters, exact = exact)
  
  ## adding query for limits and start index
  query <- append(query, list(start = start, limit = limit))  
  
  models <- vGET(session,
                   path = "modelRepository/models",
                   query = query,
                   ...
  )
  if (length(models$items) == 0) {
    return(list())
  }
  
  models <- create_sasctl_obj(models$items, "MMmodelList", append = TRUE)
  
  return(models)
}

#' Create a project
#' 
#' Returns a sasctl MMproject object from Model Manager
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param name The name of the project
#' @param model_function The model function of the project. Valid values: analytical, classification, cluster, forecasting, prediction, Text categorization, Text extraction, Text sentiment, Text topics, transformation
#' @param description The description of the project.
#' @param input_vars `data.frame` with the input data sample to configure the project variables.
#' @param output_vars `data.frame` with the output data sample to configure the project variables.
#' @param additional_parameters `list` of parameters and lists to be added to the payload
#' @param image Image URI to be used as project cover
#' @param ... additional parameters to be passed to `httr::POST` such as `httr::add_headers`
#' @return A `data.frame` with the list of projects
#' @examples
#' 
#' \dontrun{
#' new_project <- create_project(sess, name = "ModelProj", 
#'                                     description = "My fancy project",
#'                                     model_function = "classification")
#' new_project
#' }
#' 
#' @export

create_project <-  function(session, name, description = NULL, 
                            model_function = NULL, 
                            input_vars = NULL, output_vars = NULL,
                            image = NULL, additional_parameters = NULL,
                            ...) {
  
  ## TODO add the following parameters to have more control of the project 
  ## variables
  ## eventProbabilityVariable
  ## targetLevel
  ## classTargetValues
  ## predictionVariable
  ## targetEventValue
  ## trainTable
  ## properties 
  
  ### getting SAS Viya model default model repository and folderId
  repositories <- sasctl::list_repositories(session)
  default_repository <- repositories[repositories$defaultRepository == TRUE & !is.na(repositories$defaultRepository),][c("id","folderId")][1,]
  
  ### defining the payload to configure the function
  
  ## default payload
  payload <- list(name = name,
       "function" = model_function,
       description = description,
       folderId = default_repository$folderId,
       repositoryId = default_repository$id,
       imageURI = image)
  
  ## appending additional properties ## TODO
  
  #### Variables,
  if (!is.null(input_vars)) {
    if (!is.data.frame(input_vars)) {
      stop('input_vars must be a data.frame, if you sliced a single column try data[,col_number, drop = FALSE] to keep as data.frame')
    }
    
    input_vars <- sasctl::write_in_out_json(input_vars[,,drop = FALSE],
                                noFile = TRUE )
  }
  
  if (!is.null(output_vars)) {
    if (!is.data.frame(output_vars)) {
      stop('input_vars must be a data.frame, if you sliced a single column try data[,col_number, drop = FALSE] to keep as data.frame')
    }
    
    output_vars <- sasctl::write_in_out_json(output_vars[,,drop = FALSE],
                                             input = FALSE, noFile = TRUE )
  }
  
  if (!is.null(input_vars) | !is.null(output_vars)) {
    vars <- rbind(input_vars, output_vars)
    
    payload <- append(payload, list(variables = vars))
    
  }
  
  #### additional generic parameters
  
  if (!is.null(additional_parameters)) {
    
    if (!is.list(additional_parameters)) {
      stop("additional_paramenters must be a list or a list of lists")  
    }
    payload <- append(payload, additional_parameters)
  }
    

  project <- vPOST(session,
                   path = "modelRepository/projects",
                   ### the following content type must be used or the API will reject it
                   httr::content_type("application/vnd.sas.models.project+json"),
                   payload = payload,
                   ...
  )
  
  project <- create_sasctl_obj(project, "MMproject")
  
  return(project)
}

#' Update a project
#' 
#' Returns a sasctl MMproject object from Model Manager
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param name The name of the project
#' @param project `MMproject` object, project ID or project name. If name, will try to find a single project with exact name match. See `exact` parameter 
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @param input_vars `data.frame` with the input data sample to configure the project variables.
#' @param output_vars `data.frame` with the output data sample to configure the project variables.
#' @param additional_parameters `list` of parameters and lists to be added to the payload
#' @param update_variables logical, TRUE, will make additional rest call to update variables using [sasctl::update_project_variables()], 
#' variables can't be changed in the same endpoint as other resources
#' @param ... additional parameters to be passed to `httr::POST` such as `httr::add_headers`
#' @return A `data.frame` with the list of projects
#' @examples
#' 
#' \dontrun{
#' new_project <- create_project(sess, name = "ModelProj", 
#'                               description = "My fancy project",
#'                               model_function = "classification")
#'                               
#' updated_project <- update_project(sess, project = new_project, 
#'                                   additional_parameters = 
#'                                   list(description = "Updated fancy description",
#'                                        scoreInputTable = "myTable",
#'                                        predictionVariable = "BAD"))
#' updated_project
#' }
#' 
#' @export

update_project <-  function(session, name, project,
                            input_vars = NULL, output_vars = NULL,
                            additional_parameters = NULL,
                            update_variables = TRUE,
                            exact = TRUE,
                            ...) {

  
  project_id <- single_id_from_object(session, project, class = "MMproject", type = "project", exact = exact)
  
   ## Variables must be changes in their own endpoint /variables
  
    if (is.null(input_vars) & is.null(output_vars)) {
      update_variables <- FALSE
    }
  
    if (update_variables) {

      update_project_variables(session, 
                              project = project, 
                              input_vars = input_vars,
                              output_vars = output_vars,
                              
                              ...)  
      
      #### get new project with new etag
      project <- sasctl::get_project(session, project, ...)
      
    }
    
  
  ### this is required to get the etag if missing and get latest object update
  if (!any(names(project) == "etag")) {
    project <- sasctl::get_project(session, project, ...)
    
  }
  
  #### additional generic parameters
  
  if (!is.null(additional_parameters)) {
    
    if (!is.list(additional_parameters)) {
      stop("additional_paramenters must be a list or a list of lists")  
    }
    
    for (parameter in names(additional_parameters)) {
      project[parameter] <- additional_parameters[parameter] 
    }
    
    payload <- project
  }
  

  project <- vPUT(session,
                  path = paste0("modelRepository/projects/", project_id),
                  httr::content_type("application/vnd.sas.models.project+json"),
                  httr::add_headers("If-Match" = project$etag),
                  payload = payload,
                  ...
  )
  
  project <- create_sasctl_obj(project, "MMproject")
  
  return(project)
}

#' Update a model
#' 
#' Returns a sasctl MMmodel object from Model Manager
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param name The name of the model
#' @param model `MMmodel` object, model ID or model name. If name, will try to find a single model with exact name match. See `exact` parameter
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @param input_vars `data.frame` with the input data sample to configure the model variables.
#' @param output_vars `data.frame` with the output data sample to configure the model variables.
#' @param additional_parameters `list` of parameters and lists to be added to the payload
#' @param update_variables logical, TRUE, will make additional rest call to update variables using [sasctl::update_model_variables()], 
#' variables can't be changed in the same endpoint as other resources
#' @param ... additional parameters to be passed to `httr::POST` such as `httr::add_headers`
#' @return A `MMmodel` object with the model information
#' @examples
#' 
#' \dontrun{
#' updated_model <- update_model(sess, model = new_model, 
#'                               additional_parameters = 
#'                                list(description = "Updated fancy description",
#'                               modeler = "BAD"))
#' updated_model
#' }
#' 
#' @export

update_model <-  function(session, name, model,
                          input_vars = NULL, output_vars = NULL,
                          additional_parameters = NULL,
                          update_variables = TRUE,
                          exact = TRUE,
                          ...) {
  
  model_id <- single_id_from_object(session, model, class = "MMmodel", type = "model", exact = exact)
  
  ## Variables must be changes in their own endpoint, additional request must be done
  
  if (is.null(input_vars) & is.null(output_vars)) {
    update_variables <- FALSE
  }
  
  if (update_variables) {
    
    update_model_variables(session, 
                           model = model, 
                           input_vars = input_vars,
                           output_vars = output_vars,
                           ...)  
    
    #### if you change the model with new variables
    #### it requires a new etag that doesn't come with the header, it requires a new tag
    model <- sasctl::get_model(session, model, ...)
    
  }
  
  ### this is required to get the etag if missing
  
  if (!any(names(model) == "etag")) {
    model <- sasctl::get_model(session, model, ...)
  }  
  
  #### additional generic parameters
  
  if (!is.null(additional_parameters)) {
    
    if (!is.list(additional_parameters)) {
      stop("additional_paramenters must be a list or a list of lists")  
    }
    
    for (parameter in names(additional_parameters)) {
      model[parameter] <- additional_parameters[parameter] 
    }
    
    payload <- model
  }
  

  
  model <- vPUT(session,
                path = paste0("modelRepository/models/", model_id),
                ### the following content type must be used or the API will reject it
                httr::content_type("application/vnd.sas.models.model+json"), ##vnd.sas.models.model
                httr::add_headers("If-Match" = model$etag),
                payload = payload,
                ...
  )
  
  model <- create_sasctl_obj(model, "MMmodel")
  
  return(model)
}

#' Update project variables
#' 
#' Returns a sasctl MMproject object from Model Manager
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param project `MMproject` object, project ID or project name. If name, will try to find a single project with exact name match. See `exact` parameter
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @param input_vars `data.frame` with the input data sample to configure the project variables.
#' @param output_vars `data.frame` with the output data sample to configure the project variables.
#' @param sasctl_vars `data.frame` from `MMmodel` object `inputVariables`, `outputVariables`. 
#' @param ... additional parameters to be passed to `httr::POST` such as `httr::add_headers`
#' @return A `data.frame` with the list of projects
#' @examples
#' 
#' \dontrun{
#' new_project <- create_project(sess, name = "ModelProj",  
#'                                     description = "My fancy project", 
#'                                     model_function = "classification")
#'                                     
#' new_variables <- update_project_variables(sess, project = new_project, 
#'                                           input_vars = iris[,1:4], 
#'                                           output_vars = iris[,5, drop = F])
#'                                           
#' new_variables
#' }
#' 
#' @export

update_project_variables <-  function(session, project,
                            input_vars = NULL, output_vars = NULL,
                            exact = TRUE, sasctl_vars,
                            ...) {
  
  project_id <- single_id_from_object(session, project, class = "MMproject", type = "project", exact = exact)
  
  ## appending additional properties ## TODO
  if (missing(sasctl_vars)) {
  
    #### Variables
    if (!is.null(input_vars)) {
      if (!is.data.frame(input_vars)) {
        stop('input_vars must be a data.frame, if you sliced a single column try data[,col_number, drop = FALSE] to keep as data.frame')
      }
      
      input_vars <- sasctl::write_in_out_json(input_vars[,,drop = FALSE],
                                              noFile = TRUE)
    }
    
    if (!is.null(output_vars)) {
      if (!is.data.frame(output_vars)) {
        stop('input_vars must be a data.frame, if you sliced a single column try data[,col_number, drop = FALSE] to keep as data.frame')
      }
      
      output_vars <- sasctl::write_in_out_json(output_vars[,,drop = FALSE],
                                               input = FALSE, noFile = TRUE)
    }
    
    if (!is.null(input_vars) | !is.null(output_vars)) {
      vars <- rbind(input_vars, output_vars)
      
      payload <- list(items = vars)
      
    } else {
      stop("No new variables defined")
    }
  } else {
    payload <- list(items = sasctl_vars)  
  }
  
  projectVariables <- vPOST(session,
                  path = paste0("modelRepository/projects/", project_id, "/variables"),
                  ### the following content type must be used or the API will reject it
                  httr::content_type("application/vnd.sas.collection+json"),
                  payload = payload,
                  ...
  )
  
  
  projectVariables <- create_sasctl_obj(projectVariables, "MMprojectVariables")


  return(projectVariables)
}


#' Update model variables
#' 
#' Update a SAS Model Manager model variables
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param model `MMmodel` object, model ID or model name. If name, will try to find a single model with exact name match. See `exact` parameter
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @param input_vars `data.frame` with the input data sample to configure the model variables.
#' @param output_vars `data.frame` with the output data sample to configure the model variables.
#' @param ... additional parameters to be passed to `httr::POST` such as `httr::add_headers`
#' @return A `MMmodelVariables` variables
#' @examples
#' 
#' \dontrun{
#' new_variables <- update_model_variables(sess, model = new_model, 
#'                                             input_vars = iris[,1:4], 
#'                                             output_vars = iris[,5, drop = F]
#'                              )
#' new_variables
#' }
#' 
#' @export

update_model_variables <-  function(session, model,
                                    input_vars = NULL, output_vars = NULL,
                                    exact = TRUE,
                                    ...) {
  
  model_id <- single_id_from_object(session, model, class = "MMmodel", type = "model", exact = exact)
  
  ## appending additional properties ## TODO
  
  #### Variables
  if (!is.null(input_vars)) {
    if (!is.data.frame(input_vars)) {
      stop('input_vars must be a data.frame, if you sliced a single column try data[,col_number, drop = FALSE] to keep as data.frame')
    }
    
    input_vars <- sasctl::write_in_out_json(input_vars[,,drop = FALSE],
                                            noFile = TRUE)
  }
  
  if (!is.null(output_vars)) {
    if (!is.data.frame(output_vars)) {
      stop('input_vars must be a data.frame, if you sliced a single column try data[,col_number, drop = FALSE] to keep as data.frame')
    }
    
    output_vars <- sasctl::write_in_out_json(output_vars[,,drop = FALSE],
                                             input = FALSE, noFile = TRUE)
  }
  
  if (!is.null(input_vars) | !is.null(output_vars)) {
    vars <- rbind(input_vars, output_vars)
    
    payload <- list(items = vars)
    
  } else {
    stop("No new variables defined")
  }
  
  modelVariables <- vPOST(session,
                          path = paste0("modelRepository/models/", model_id, "/variables"),
                          ### the following content type must be used or the API will reject it
                          httr::content_type("application/vnd.sas.collection+json"),
                          payload = payload,
                          ...
  )
  
  
  modelVariables <- create_sasctl_obj(modelVariables, "MMmodelVariables")
  
  
  return(modelVariables)
}

#' Delete a project
#' 
#' Delete a project and all associated models and resources
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param project `MMproject` object, project ID or project name. If name, will try to find a single project with exact name match. See `exact` parameter
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @return A [`httr::response`] object.
#' 
#' @examples
#' 
#' \dontrun{
#' new_project <- create_project(sess, name = "ModelProj", 
#'                                     description = "My fancy project", 
#'                                     model_function = "classification")
#'                                     
#' delete_project(sess, new_project)
#' }
#' 
#' @export

delete_project <- function(session, project, exact = TRUE){
  
  project_id <- single_id_from_object(session, project, class = "MMproject", type = "project", exact = exact)
  
  del_proj <- vDELETE(session,
                      paste0("modelRepository/projects/", project_id))
  
  return(del_proj)
}


#' Delete a model
#' 
#' delete a model from Model Manager
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param model `MMmodel` or `MMmodelVersion` object, model ID or model name. If name, will try to find a single model with exact name match. See `exact` parameter
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @return A [`httr::response`] object.
#' 
#' @examples
#' 
#' \dontrun{
#' my_model <- get_model(sess, "MyModel")
#' 
#' delete_model(sess, my_model)
#' }
#' 
#' @export

delete_model <- function(session, model, exact = TRUE){
  
  model_id <- single_id_from_object(session, model, class = c("MMmodel", "MMmodelVersion"), type = "model", exact = exact)
  
  del_model <- vDELETE(session,
                      paste0("modelRepository/models/", model_id))
  
  return(del_model)
}

#' Delete a model content
#' 
#' delete model from Manager
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param model `MMmodel` object, model ID or model name. If name, will try to find a single model with exact name match. See `exact` parameter
#' @param content `MMmodelContentList` obtained from [`list_model_contents()`], if missing will delete all files will be deleted. 
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @return A [`httr::response`] object.
#' 
#' @examples
#' 
#' \dontrun{
#' my_model <- get_model(sess, "MyModel")
#' 
#' delete_model_contents(sess, my_model)
#' }
#' 
#' @export

delete_model_contents <- function(session, model, content, exact = TRUE){
  
  if (missing(model)) {
    model_id <- list_model_contents(session, content$modelId)
  }
    
  model_id <- single_id_from_object(session, model, class = c("MMmodel", "MMmodelVersion"), type = "model", exact = exact)
  
  if (missing(content)) {
    content <- list_model_contents(session, model_id)
  }
  
  if (!is_sasctl_class(content, c('MMmodelContentList'))) {
    stop("content is not a MMmodelContentList list from list_model_contents() function")
  }
  
  for (content_id in content$id) {
  
    del_content <- vDELETE(session,
                       paste0("modelRepository/models/", model_id, "/contents/",content_id))
  
  }
  
  return(del_content)
  
}

#' List Models
#' 
#' Returns a list of models from Model Manager
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param model `MMmodel` or `MMmodelVersion` object, model ID or model name. If name, will try to find a single model with exact name match. See `exact` parameter
#' @param limit maximum number of models to return
#' @param start the index of the first content to return
#' @param exact boolean, If the filter query should use "contains" for partial match or "eq" for exact match
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @return A `MMmodelContentList` list with the list of contents
#' @examples
#' 
#' \dontrun{
#' models <- list_models(sess, filter = list(createdBy = "creatorUser", name = "modelName"))
#' 
#' models
#' }
#' 
#' @export

list_model_contents <-  function(session, model, start = 0, limit = 20, exact = FALSE, ...) {
  
  model_id <- single_id_from_object(session, model, class = c("MMmodel", "MMmodelVersion"), type = "model", exact = exact)
  
  ## adding query for limits and start index
  query <- list(start = start, limit = limit)  
  
  contents <- vGET(session,
                 path = paste0("modelRepository/models/", model_id, "/contents"),
                 query = query,
                 ...
  )
  if (length(contents$items) == 0) {
    return(list())
  }
  
  contents <- create_sasctl_obj(contents$items, "MMmodelContentList", append = TRUE)
  
  return(contents)
}

#' Get a project
#' 
#' Returns a single sasctl `MMProject` from SAS Model Manager
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param project `MMproject` object, project ID or project name. If name, will try to find a single project with exact name match. See `exact` parameter
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @return `list` with sasctl attribute
#' @examples
#' 
#' \dontrun{
#' my_project <- get_project(sess, project = ModelProj)
#' 
#' my_project
#' }
#' 
#' @export

get_project <- function(session, project, exact = TRUE, ...){
  
  project_id <- single_id_from_object(session, project, class = "MMproject", type = "project", exact = exact)
  
  project <- vGET(session,
               path = paste0("modelRepository/projects/", project_id),
               httr::content_type("application/vnd.sas.models.project+json"),
               ...
               )
  
  project <- create_sasctl_obj(project, "MMproject")
  
  return(project) 
  
}

#' Test if a project exists
#' 
#' Test if the project exists inside SAS Model Manager
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param project `MMproject` object, project ID or project name.
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @return boolean
#' @examples
#' 
#' \dontrun{
#' project_exists(sess, project = ModelProj)
#' }
#' 
#' @export

project_exists <- function(session, project, ...){
  
  project_id <- tryCatch(
                  single_id_from_object(session, project, class = "MMproject", type = "project", exact = TRUE),
                  error = function(e) e
  )
  
  proj_head <- tryCatch(
    
    vHEAD(session,
          path = paste0("modelRepository/projects/", project_id),
          ...),
    
    error = function(e) e
  )
  
  if (any(class(proj_head) == "error")) {
    exists <- FALSE
  } else {
    exists <- TRUE
  }
  
  return(exists) 
}

#' Test if a model exists
#' 
#' Test if the model exists inside SAS Model Manager
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param model `MMmodel` object, model ID or model name.
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @return boolean
#' @examples
#' 
#' \dontrun{
#' model_exists(sess, model = ModelProj)
#' }
#' 
#' @export

model_exists <- function(session, model, ...){
  
  model_id <- tryCatch(
    single_id_from_object(session, model, class = "MMmodel", type = "model", exact = TRUE),
    error = function(e) e
  )
  
  proj_head <- tryCatch(
    
    vHEAD(session,
          path = paste0("modelRepository/models/", model_id),
          ...),
    
    error = function(e) e
  )
  
  if (any(class(proj_head) == "error")) {
    exists <- FALSE
  } else {
    exists <- TRUE
  }
  
  return(exists) 
}


#' Get a model
#' 
#' Returns a single sasctl `MMmodel` from SAS Model Manager
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param model `MMmodel` or `MMmodelVersion` object, project ID or model name. If name, will try to find a single model with exact name match. See `exact` parameter
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @return `list` with sasctl attribute
#' @examples
#' 
#' \dontrun{
#' my_model <- get_model(sess, model = MMmodel)
#' 
#' my_model
#' }
#' 
#' @export

get_model <- function(session, model, exact = TRUE, ...){
  
  model_id <- single_id_from_object(session, model, class = c("MMmodel", 'MMmodelVersion'), type = "model", exact = exact)
  
  model <- vGET(session,
                  path = paste0("modelRepository/models/", model_id),
                  httr::content_type("application/vnd.sas.models.model+json"),
                  ...
  )
  
  model <- create_sasctl_obj(model, "MMmodel")
  
  return(model) 
  
}

#' Add model version
#' 
#' Add model version
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param model `MMmodel` object, project ID or model name. If name, will try to find a single model with exact name match. See `exact` parameter
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @param ... additional parameters to be passed to `httr::POST` such as `httr::add_headers`
#' @param minor if `TRUE`, new minor version is created, by default a major version is created.
#' @return A `data.frame` with the list of projects
#' @examples
#' 
#' \dontrun{
#' my_model <- get_model(sess, model = "MyModel")
#' 
#' nvmodel <- add_model_version(sess,  my_model)
#' 
#' nvmodel
#' }
#' 
#' @export

add_model_version <-  function(session, model,
                                  exact = TRUE,
                                  minor = FALSE,
                                      ...) {
  
  model_id <- single_id_from_object(session, model, class = "MMmodel", type = "model", exact = exact)
  
  if (!is_sasctl_class(model, "MMmodel")) {
    model <- sasctl::get_model(session, model, ...)
  }  
  
  payload <- list()
  
  if (minor) {
  payload$option = 'minor'
  } else {
  payload$option = 'major'
  }
  
  model <- vPOST(session,
                            path = paste0("modelRepository/models/", model_id, "/modelVersions"),
                            ### the following content type must be used or the API will reject it
                            httr::content_type("application/json"),
                            payload = payload,
                            ...
  )
  
  
  model <- create_sasctl_obj(model, "MMmodelVersion")
  
  
  return(model)
}

#' Add model content
#' 
#' Add model content
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param file path to file
#' @param model `MMmodel` object, model ID or model name. If name, will try to find a single model with exact name match. See `exact` parameter
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @param role file role, such as "scoreResource" and "score" 
#' @param ... pass to `sasctl::vPOST()` function
#' @return a `MMcontent` class list
#' @examples 
#' 
#' \dontrun{
#' my_model <- get_model(sess, "MyModel")
#' 
#' myContent <- add_model_content(sess, file = "my_fancy_file.R", model = my_model)
#' myContent
#' }
#' 
#' @export

add_model_content <-  function(session, file,
                               model,
                               role = NULL,
                               exact = TRUE,
                               ...) {
  
  model_id <- single_id_from_object(session, model, class = "MMmodel", type = "model", exact = exact)
  
  if (!any(names(model) == "etag")) {
    model <- sasctl::get_model(session, model, ...)
  }  
  
  
  # query <- list(role = role)
  
  payload <- list(file = httr::upload_file(file), role = role)
  
  
  ### requesting
  
  model <- vPOST(session, 
                 path = paste0("modelRepository/models/", model_id, '/contents'),
                 # query = query,
                 fragment = "octetStream",
                 payload = payload,
                 encode = "multipart",
                 ...
  )
  
  model <- create_sasctl_obj(model$items, "MMmodelContentList")
  
  return(model)
}

#' Create Score Code template
#' 
#' Creates an R file in the path with an example.  The file structure are as follows:
#' For official documentation go to [Scoring R models documentation](https://go.documentation.sas.com/doc/en/mdlmgrcdc/v_025/mdlmgrug/n04i7s6bdu7ilgn1e350am3byuxx.htm#p0m6jg3tgih1agn1cgdcexpwgp0g) 
#' 
#' - The file should start with a function with all the input variables which SAS Viya will use to insert data
#' 
#' - Then it is followed by a comment line `#output: outvar1, outvar2` which is case sensitive and must follow that
#' structure so SAS can receive the function output properly.
#' 
#' - If you are using a previously created model, it should be read, we recommend `.rda` format, but could be
#' a `pmml` file or other format that suits you, just make sure that it is properly classified as scoring resource
#' when using inside SAS Model Manager.
#' 
#' - You then can use any logic to score the model or just an arbitrary R code.
#' 
#' - To pass the information back to SAS it must return a list of the variables defined at the beginning of the
#' script.
#' 
#' @param path path to create file, default current working directory
#' @param openFile automatically open file for editing
#' @return nothing
#' @examples
#' 
#' \dontrun{
#' create_scoreSample()
#' }
#' 
#' @export

create_scoreSample <- function(path = ".", openFile = TRUE){
  
  ### files inside /inst are the top level

  orFile <- system.file("scoreCode.R",
                       package = "sasctl")
  
  file.copy(orFile, path)
  
  
  if (openFile) {
    if (.Platform$GUI == "RStudio") {
      rstudioapi::documentOpen(paste0(path, "/scoreCode.R"))
    } else {
      utils::file.edit(paste0(path, "/scoreCode.R"))
      }
  }
  
  path <- ifelse(grepl("\\/$", path), path, paste0(path, "/"))
  
  message(paste0("Example file copied to ", path, "scoreCode.R"))
  
}

#' Convert pmml 4.x to 4.2 
#' 
#' Converts a pmml header text file from 4.x version to 4.2
#' 
#' @param file_in path to a .pmml file
#' @param file_out path to write the converted .pmml file
#' @return nothing
#' @examples
#'  
#' \dontrun{
#' hmeq <- read.csv("https://support.sas.com/documentation/onlinedoc/viya/exampledatasets/hmeq.csv",
#'                  stringsAsFactors = TRUE)
#' 
#' hmeq[hmeq == ""] <- NA
#' hmeq <- na.omit(hmeq)
#' hmeq$BAD <- as.factor(hmeq$BAD)
#' 
#' model1 <- glm(BAD ~ ., hmeq, family = binomial("logit"))
#' summary(model1)
#' 
#' XML::saveXML(pmml::pmml(model1, model.name = "General_Regression_Model",
#'                         app.name = "Rattle/PMML",
#'                         description = "Linear Regression Model"),
#'              "dev/my_model44.pmml")
#'              
#' convert_to_pmml42("my_model.pmml", "my_model_conv.pmml")
#' }
#' 
#' @export

convert_to_pmml42 <- function(file_in, file_out){
  
  lines <- readLines(file_in)
  
  #8 lines = header
  
  lines[1:8] <- gsub("4-\\d", "4-2", lines[1:8])
  lines[1:8] <- gsub("4_\\d", "4_2", lines[1:8])
  lines[1:8] <- gsub("4\\.\\d\\.\\d", "4.2", lines[1:8]) ## some files have 4.4.1 version
  lines[1:8] <- gsub("Version : 4\\.\\d", "Version : 4.2", lines[1:8]) 
  
  writeLines(lines, file_out)
  
}

