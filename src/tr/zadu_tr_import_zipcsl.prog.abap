*&---------------------------------------------------------------------*
*& Include zadu_tr_import_zipcsl
*&---------------------------------------------------------------------*

CLASS lcl_process DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS help_path.

    METHODS run.

ENDCLASS.


CLASS lcl_process IMPLEMENTATION.


  METHOD help_path.

    DATA:
      lt_files        TYPE filetable,
      lv_initial_path TYPE string,
      lv_return_code  TYPE i.

    DATA(lt_dynpfields) = VALUE dynpread_tabtype( ( fieldname = `P_PATH` ) ).

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = sy-repid
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = lt_dynpfields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.
    IF sy-subrc = 0.
      lv_initial_path = lt_dynpfields[ fieldname = `P_PATH` ]-fieldvalue.
    ENDIF.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = |{ 'Select ZIP file'(001) }|
        default_extension       = `zip`
        file_filter             = '*.zip'
        initial_directory       = lv_initial_path
      CHANGING
        file_table              = lt_files
        rc                      = lv_return_code
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5 ).

    p_path = COND #( WHEN sy-subrc = 0 AND lt_files IS NOT INITIAL THEN lt_files[ 1 ]-filename
                                                                   ELSE lv_initial_path ).

  ENDMETHOD.


  METHOD run.

    TYPES ty_hex TYPE x LENGTH 255.

    DATA lt_data TYPE TABLE OF ty_hex WITH DEFAULT KEY.
    DATA lv_data TYPE xstring.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = p_path
        filetype                = 'BIN'
      CHANGING
        data_tab                = lt_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno DISPLAY LIKE sy-msgty
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CONCATENATE LINES OF lt_data INTO lv_data IN BYTE MODE.

    TRY.
        DATA(lt_result) = zcl_adu_utl_transport_request=>get( )->zip_import( lv_data ).
      CATCH zcx_adu_transport_request INTO DATA(lx_transport_request).
        DATA(lo_messages) = NEW zcl_adu_messages( ).
        lo_messages->add_exception( lx_transport_request ).
        lo_messages->display_messages( ).
        RETURN.
    ENDTRY.

    IF lt_result IS INITIAL.
      RETURN.
    ENDIF.

    TRY.

        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(lo_salv_table)
          CHANGING
            t_table        = lt_result ).

        lo_salv_table->get_functions( )->set_all( ).

        lo_salv_table->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv).
        lo_messages = NEW zcl_adu_messages( ).
        lo_messages->add_exception( lx_salv ).
        lo_messages->display_messages( ).
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
