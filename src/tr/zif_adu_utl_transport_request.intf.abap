INTERFACE zif_adu_utl_transport_request
  PUBLIC.

  TYPES:
    BEGIN OF ty_content,
      transport_request TYPE trkorr,
      cofile            TYPE xstring,
      data              TYPE xstring,
    END OF ty_content,
    ty_contents TYPE HASHED TABLE OF ty_content
      WITH UNIQUE KEY transport_request.

  TYPES:
    BEGIN OF ty_import_result,
      transport_request TYPE trkorr,
      tms_alert         TYPE stmscalert,
    END OF ty_import_result,
    ty_import_results TYPE STANDARD TABLE OF ty_import_result
      WITH EMPTY KEY.

  METHODS get_path_cofiles
    RETURNING
      VALUE(rv_result) TYPE string.

  METHODS get_path_data
    RETURNING
      VALUE(rv_result) TYPE string.

  METHODS get_path_trans
    RETURNING
      VALUE(rv_result) TYPE string.

  METHODS build_filename_cofiles
    IMPORTING
      iv_transport_request TYPE trkorr
    RETURNING
      VALUE(rv_result)     TYPE string.

  METHODS build_filename_data
    IMPORTING
      iv_transport_request TYPE trkorr
    RETURNING
      VALUE(rv_result)     TYPE string.

  METHODS build_path_cofiles
    IMPORTING
      iv_transport_request TYPE trkorr
    RETURNING
      VALUE(rv_result)     TYPE string.

  METHODS build_path_data
    IMPORTING
      iv_transport_request TYPE trkorr
    RETURNING
      VALUE(rv_result)     TYPE string.

  METHODS is_filename_valid
    IMPORTING
      iv_filename      TYPE string
    RETURNING
      VALUE(rv_result) TYPE abap_bool.

  METHODS convert_filename_to_tr
    IMPORTING
      iv_filename      TYPE string
    RETURNING
      VALUE(rv_result) TYPE trkorr
    RAISING
      zcx_adu_transport_request.

  METHODS read_header
    IMPORTING
      iv_transport_request TYPE trkorr
    RETURNING
      VALUE(rs_result)     TYPE trwbo_request_header
    RAISING
      zcx_adu_transport_request.

  METHODS add_to_queue
    IMPORTING
      iv_transport_request TYPE trkorr
    RETURNING
      VALUE(rs_result)     TYPE stmscalert
    RAISING
      zcx_adu_transport_request.

  METHODS import
    IMPORTING
      it_content       TYPE ty_contents
      iv_add_queue     TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(rt_result) TYPE ty_import_results
    RAISING
      zcx_adu_transport_request.

  METHODS zip_import
    IMPORTING
      iv_zip           TYPE xstring
      iv_add_queue     TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(rt_result) TYPE ty_import_results
    RAISING
      zcx_adu_transport_request.

  METHODS zip_to_content
    IMPORTING
      iv_zip           TYPE xstring
    RETURNING
      VALUE(rt_result) TYPE ty_contents
    RAISING
      zcx_adu_transport_request.

ENDINTERFACE.
