#' Build Event Handlers
#'
#' @param target
#'    Target. See details.
#' @param rule
#'    Rule.
#' @param action_list
#'    Action list.
#' @param name
#'    Name of the handler.
#' @param status
#'    Handler status.
#' @param description
#'    Description.
#' @param type
#'    Configuration type.
#' @param limit_traffic
#'    Limit traffic in megabytes.
#' @param timing
#'    Timing when the handler is fired.
#' @param speed_class
#'    Maximum speed class.
#' @param to
#'    Destination of the notification mail.
#' @param title
#'    Title of the notification mail.
#' @param message
#'    Message body of the notification mail.
#' @param endpoint
#'    AWS Lambda's endpoint.
#' @param function_name
#'    AWS Lambda's function name.
#' @param access_key
#'    AWS access key.
#' @param secret_access_key
#'    AWS secret key.
#' @param parameter_1
#'    The first parameter of Lambda.
#' @param parameter_2
#'    The second parameter of Lambda.
#' @param parameter_3
#'    The third parameter of Lambda.
#'
#' @rdname build_event_handlers
#' @export
build_event_handler <- function(target, rule, action_list, name, status = c("inactive", "active"), description = NA) {
   target <- if (inherits(target, "soracom_subscriber")) {
      list("targetImsi" = get_segment(target))
   } else if (inherits(target, "soracom_operator") || inherits(target, "soracom_token")) {
      list("targetOperatorId" = get_segment(target))
   } else if (inherits(target, "soracom_group")) {
      list("targetGroupId" = get_segment(target))
   } else if (inherits(target, "soracom_tags")) {
      list("targetTag" = as.list(target))
   } else if (is.character(target) && !is.null(names(target))) {
      list("targetTag" = as.list(target))
   } else {
      stop("Unsupported target.")
   }
   status <- match.arg(status)

   # When the action list is passed as single object
   if (inherits(action_list, "soracom_event_handler_action")) {
      action_list <- list(action_list)
   }

   c(
      target,
      list(
         "name" = name,
         "description" = description,
         "ruleConfig" = rule,
         "actionConfigList" = action_list,
         "status" = status
      )
   )
}

#' @rdname build_event_handlers
#' @export
rule_config <- function(type = c("DailyTrafficRule", "DailyTotalTrafficRule", "MonthlyTrafficRule", "MonthlyTotalTrafficRule", "CumulativeTrafficRule"), limit_traffic, timing = c("NEVER", "IMMEDIATELY", "BEGINNING_OF_NEXT_MONTH", "BEGINNING_OF_NEXT_DAY", "AFTER_ONE_DAY")) {
   type <- match.arg(type)
   limit_traffic <- as.numeric(limit_traffic)
   timing <- match.arg(timing)

   structure(
      list(
         "type" = type,
         "properties" = list(
            "limitTotalTrafficMegaByte" = limit_traffic,
            "inactiveTimeoutDateConst" = timing
         )
      ),
      class = c("soracom_event_handler_rule", "list")
   )
}

#' @rdname build_event_handlers
#' @export
change_speed_class_action <- function(speed_class = c("s1.minimum", "s1.slow", "s1.standard", "s1.fast"), timing = c("NEVER", "IMMEDIATELY", "BEGINNING_OF_NEXT_MONTH", "BEGINNING_OF_NEXT_DAY", "AFTER_ONE_DAY")) {
   speed_class <- match.arg(speed_class)
   timing <- match.arg(timing)

   structure(
      list(
         "type" = "ChangeSpeedClassAction",
         properties = list(
            "speedClass" = speed_class,
            "executionDateTimeConst" = timing
         )
      ),
      class = c("soracom_event_handler_action", "list")
   )
}

#' @rdname build_event_handlers
#' @export
send_mail_action <- function(to, message, title, timing = c("NEVER", "IMMEDIATELY", "BEGINNING_OF_NEXT_MONTH", "BEGINNING_OF_NEXT_DAY", "AFTER_ONE_DAY")) {
   timing <- match.arg(timing)

   structure(
      list(
         "type" = "SendMailAction",
         properties = list(
            "to" = to,
            "message" = message,
            "title" = title,
            "executionDateTimeConst" = timing
         )
      ),
      class = c("soracom_event_handler_action", "list")
   )
}

#' @rdname build_event_handlers
#' @export
send_mail_to_operator_action <- function(message, title, timing = c("NEVER", "IMMEDIATELY", "BEGINNING_OF_NEXT_MONTH", "BEGINNING_OF_NEXT_DAY", "AFTER_ONE_DAY")) {
   timing <- match.arg(timing)

   structure(
      list(
         "type" = "SendMailToOperatorAction",
         properties = list(
            "message" = message,
            "title" = title,
            "executionDateTimeConst" = timing
         )
      ),
      class = c("soracom_event_handler_action", "list")
   )
}

#' @rdname build_event_handlers
#' @export
invoke_aws_lambda_action <- function(endpoint, function_name, access_key, secret_access_key, parameter_1, parameter_2, parameter_3, timing = c("NEVER", "IMMEDIATELY", "BEGINNING_OF_NEXT_MONTH", "BEGINNING_OF_NEXT_DAY", "AFTER_ONE_DAY")) {
   timing <- match.arg(timing)

   structure(
      list(
         "type" = "InvokeAWSLambdaAction",
         properties = list(
            "endpoint" = endpoint,
            "functionName" = function_name,
            "accessKey" = access_key,
            "secretAccessKey" = secret_access_key,
            "parameter1" = parameter_1,
            "parameter2" = parameter_2,
            "parameter3" = parameter_3,
            "executionDateTimeConst" = timing
         )
      ),
      class = c("soracom_event_handler_action", "list")
   )
}

reshape_for_post <- function(handler) {
   if (inherits(handler, "soracom_event_handler")) {
      handler <- flatten_data_frame(handler)
      # removes duplicated target properties:
      # should have only one of targetImsi, targetOperatorId, targetTag, or targetGroupId.
      Filter(function(x) !is.na(x) && (!is.data.frame(x) || nrow(x) > 0), handler)
   } else {
      handler
   }
}

flatten_data_frame <- function(x) {
   if (is.list(x)) {
      x <- as.list(x)  # for data.frame
      c(lapply(x, flatten_data_frame))
   } else {
      x
   }
}
