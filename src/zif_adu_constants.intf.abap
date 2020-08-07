"! <p class="shorttext synchronized" lang="en">Constants</p>
INTERFACE zif_adu_constants
  PUBLIC.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">C (Create) - R (Read) - U (Update) - D (Delete)</p>
    BEGIN OF crud,
      create TYPE zadu_crud_ind VALUE 'C' ##NO_TEXT,
      read   TYPE zadu_crud_ind VALUE 'R' ##NO_TEXT,
      update TYPE zadu_crud_ind VALUE 'U' ##NO_TEXT,
      delete TYPE zadu_crud_ind VALUE 'D' ##NO_TEXT,
    END OF crud.

  CONSTANTS:
    BEGIN OF check_cross_reference_status,
      only_in_source            TYPE /sdf/teap_envi_status VALUE '1' ##NO_TEXT,
      only_in_target            TYPE /sdf/teap_envi_status VALUE '2' ##NO_TEXT,
      different_version         TYPE /sdf/teap_envi_status VALUE '3' ##NO_TEXT,
      same_version              TYPE /sdf/teap_envi_status VALUE '4' ##NO_TEXT,
      not_supported             TYPE /sdf/teap_envi_status VALUE '5' ##NO_TEXT,
      not_in_both_systems       TYPE /sdf/teap_envi_status VALUE '6' ##NO_TEXT,
      inconsistent_source       TYPE /sdf/teap_envi_status VALUE '7' ##NO_TEXT,
      locked_target             TYPE /sdf/teap_envi_status VALUE '8' ##NO_TEXT,
      function_not_fit_in_group TYPE /sdf/teap_envi_status VALUE '9' ##NO_TEXT,
      user_inconsistent_source  TYPE /sdf/teap_envi_status VALUE 'A' ##NO_TEXT,
    END OF check_cross_reference_status.

  CONSTANTS:
    BEGIN OF severity,
      error   TYPE zif_adu_check_transport_reader=>ty_severity VALUE '1' ##NO_TEXT,
      warning TYPE zif_adu_check_transport_reader=>ty_severity VALUE '2' ##NO_TEXT,
      info    TYPE zif_adu_check_transport_reader=>ty_severity VALUE '3' ##NO_TEXT,
    END OF severity.

ENDINTERFACE.
