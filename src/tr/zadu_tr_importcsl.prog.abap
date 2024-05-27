*&---------------------------------------------------------------------*
*&  Include  zadu_tr_importcsl
*&---------------------------------------------------------------------*

CLASS lcl_process DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS help_path.

    METHODS run.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_file,
        transport_request TYPE trkorr,
        path              TYPE string,
        zip_filename      TYPE string,
        cofile_filename   TYPE string,
        data_filename     TYPE string,
        status            TYPE string,
        message           TYPE string,
      END OF ty_file,
      ty_files TYPE STANDARD TABLE OF ty_file WITH KEY transport_request.

    DATA mt_files TYPE ty_files.
    DATA mo_salv  TYPE REF TO cl_salv_table.

    METHODS read_from_folder
      IMPORTING iv_path          TYPE string
                iv_recursively   TYPE abap_bool
                iv_include_zip   TYPE abap_bool
      RETURNING VALUE(rt_result) TYPE ty_files
      RAISING   zcx_adu_transport_request.

    METHODS read_from_zip
      IMPORTING iv_path          TYPE string
                iv_filename      TYPE string
      RETURNING VALUE(rt_result) TYPE ty_files
      RAISING   zcx_adu_transport_request.

    METHODS import_selected_files.

    METHODS on_user_command
      FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

ENDCLASS.



CLASS lcl_process IMPLEMENTATION.


  METHOD help_path.

    DATA lv_initial_path TYPE string.
    DATA lt_files        TYPE filetable.

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

    cl_gui_frontend_services=>directory_browse( EXPORTING  initial_folder       = lv_initial_path
                                                CHANGING   selected_folder      = lv_initial_path
                                                EXCEPTIONS cntl_error           = 1
                                                           error_no_gui         = 2
                                                           not_supported_by_gui = 3
                                                           OTHERS               = 4 ).

    p_path = COND #( WHEN sy-subrc = 0 AND lt_files IS NOT INITIAL
                     THEN lt_files[ 1 ]-filename
                     ELSE lv_initial_path ).

  ENDMETHOD.


  METHOD run.

    TRY.
        mt_files = read_from_folder( iv_path        = p_path
                                     iv_recursively = p_recurs
                                     iv_include_zip = p_zip ).
      CATCH zcx_adu_transport_request INTO DATA(lx_transport_request).
        DATA(lo_messages) = NEW zcl_adu_messages( ).
        lo_messages->add_exception( lx_transport_request ).
        lo_messages->display_messages( ).
        RETURN.
    ENDTRY.

    IF mt_files IS INITIAL.
      RETURN.
    ENDIF.

    TRY.

        cl_salv_table=>factory( IMPORTING r_salv_table = mo_salv
                                CHANGING  t_table      = mt_files ).

        mo_salv->set_screen_status( report   = sy-repid
                                    pfstatus = 'SALV_TABLE_STANDARD' ).

        mo_salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        mo_salv->get_functions( )->set_all( ).

        LOOP AT mo_salv->get_columns( )->get( ) ASSIGNING FIELD-SYMBOL(<ls_column>).

          CASE <ls_column>-columnname.
            WHEN 'PATH'.
              <ls_column>-r_column->set_short_text( 'Path' ).
              <ls_column>-r_column->set_medium_text( 'Path' ).
              <ls_column>-r_column->set_long_text( 'Path' ).

              <ls_column>-r_column->set_output_length( 50 ).

            WHEN 'ZIP_FILENAME'.

              IF p_zip = abap_true.

                <ls_column>-r_column->set_short_text( 'ZIP' ).
                <ls_column>-r_column->set_medium_text( 'ZIP' ).
                <ls_column>-r_column->set_long_text( 'ZIP' ).

                <ls_column>-r_column->set_output_length( 12 ).

              ELSE.

                <ls_column>-r_column->set_technical( ).

              ENDIF.

            WHEN 'COFILE_FILENAME'.
              <ls_column>-r_column->set_short_text( 'cofile' ).
              <ls_column>-r_column->set_medium_text( 'cofile' ).
              <ls_column>-r_column->set_long_text( 'cofile' ).

              <ls_column>-r_column->set_output_length( 12 ).

            WHEN 'DATA_FILENAME'.
              <ls_column>-r_column->set_short_text( 'data' ).
              <ls_column>-r_column->set_medium_text( 'data' ).
              <ls_column>-r_column->set_long_text( 'data' ).

              <ls_column>-r_column->set_output_length( 12 ).

            WHEN 'STATUS'.
              <ls_column>-r_column->set_short_text( 'Status' ).
              <ls_column>-r_column->set_medium_text( 'Status' ).
              <ls_column>-r_column->set_long_text( 'Status' ).

              <ls_column>-r_column->set_output_length( 10 ).

            WHEN 'MESSAGE'.
              <ls_column>-r_column->set_short_text( 'Message' ).
              <ls_column>-r_column->set_medium_text( 'Message' ).
              <ls_column>-r_column->set_long_text( 'Message' ).

              <ls_column>-r_column->set_output_length( 100 ).

          ENDCASE.

        ENDLOOP.

        SET HANDLER on_user_command FOR mo_salv->get_event( ) ACTIVATION abap_true.

        mo_salv->display( ).

        SET HANDLER on_user_command FOR mo_salv->get_event( ) ACTIVATION abap_false.

        mo_salv->close_screen( ).

      CATCH cx_salv_msg INTO DATA(lx_salv).
        lo_messages = NEW zcl_adu_messages( ).
        lo_messages->add_exception( lx_salv ).
        lo_messages->display_messages( ).
    ENDTRY.

  ENDMETHOD.


  METHOD read_from_folder.

    DATA lt_files              TYPE STANDARD TABLE OF file_info WITH EMPTY KEY.
    DATA lv_count              TYPE i.
    DATA lt_folders            TYPE STANDARD TABLE OF file_info WITH EMPTY KEY.
    DATA lt_transport_requests LIKE rt_result.

    cl_gui_frontend_services=>directory_list_files( EXPORTING  directory                   = iv_path
                                                               files_only                  = abap_true
                                                    CHANGING   file_table                  = lt_files
                                                               count                       = lv_count
                                                    EXCEPTIONS cntl_error                  = 1
                                                               directory_list_files_failed = 2
                                                               wrong_parameter             = 3
                                                               error_no_gui                = 4
                                                               not_supported_by_gui        = 5
                                                               OTHERS                      = 6 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adu_transport_request
        EXPORTING
          textid = VALUE #( msgid = sy-msgid
                            msgno = sy-msgno
                            attr1 = 'TEXT1'
                            attr2 = 'TEXT2'
                            attr3 = 'TEXT3'
                            attr4 = 'TEXT4' )
          text1  = CONV #( sy-msgv1 )
          text2  = CONV #( sy-msgv2 )
          text3  = CONV #( sy-msgv3 )
          text4  = CONV #( sy-msgv4 ).
    ENDIF.

    DATA(li_utility) = zcl_adu_utl_transport_request=>get( ).

    LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<ls_file>).

      DATA(lv_extension) =
        to_upper( zcl_adu_general=>get( )->split_filename_extension( |{ <ls_file>-filename }| )-extension ).

      IF     iv_include_zip = abap_true
         AND lv_extension   = 'ZIP'.

        INSERT LINES OF read_from_zip( iv_path     = iv_path
                                       iv_filename = |{ <ls_file>-filename }| )
               INTO TABLE lt_transport_requests.

        CONTINUE.

      ELSEIF li_utility->is_filename_valid( |{ <ls_file>-filename }| ) = abap_false.
        CONTINUE.
      ENDIF.

      DATA(lv_transport_request) = li_utility->convert_filename_to_tr( |{ <ls_file>-filename }| ).

      ASSIGN lt_transport_requests[ transport_request = lv_transport_request ] TO FIELD-SYMBOL(<ls_transport_request>).
      IF sy-subrc <> 0 OR <ls_transport_request> IS NOT ASSIGNED.
        INSERT VALUE #( transport_request = lv_transport_request
                        path              = iv_path )
               INTO TABLE lt_transport_requests ASSIGNING <ls_transport_request>.
      ENDIF.

      CASE <ls_file>-filename(1).
        WHEN 'K'.
          <ls_transport_request>-cofile_filename = <ls_file>-filename.

        WHEN 'R'.
          <ls_transport_request>-data_filename = <ls_file>-filename.

        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_adu_transport_request
            EXPORTING
              textid = zcx_adu_transport_request=>filename_not_supported
              text1  = |{ <ls_file>-filename }|.

      ENDCASE.

      UNASSIGN <ls_transport_request>.

    ENDLOOP.

    LOOP AT lt_transport_requests ASSIGNING <ls_transport_request> WHERE cofile_filename IS INITIAL
                                                                      OR data_filename   IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adu_transport_request
        EXPORTING
          textid = zcx_adu_transport_request=>incomplete
          text1  = |{ <ls_transport_request>-transport_request }|.

    ENDLOOP.

    rt_result = lt_transport_requests.

    IF iv_recursively = abap_false.
      RETURN.
    ENDIF.

    cl_gui_frontend_services=>directory_list_files( EXPORTING  directory                   = iv_path
                                                               directories_only            = abap_true
                                                    CHANGING   file_table                  = lt_folders
                                                               count                       = lv_count
                                                    EXCEPTIONS cntl_error                  = 1
                                                               directory_list_files_failed = 2
                                                               wrong_parameter             = 3
                                                               error_no_gui                = 4
                                                               not_supported_by_gui        = 5
                                                               OTHERS                      = 6 ).

    rt_result = VALUE #( BASE rt_result FOR <folder> IN lt_folders
                         ( LINES OF
                           read_from_folder( iv_path        = |{ iv_path }\\{ <folder>-filename }|
                                             iv_recursively = iv_recursively
                                             iv_include_zip = iv_include_zip ) ) ).

  ENDMETHOD.


  METHOD read_from_zip.

    DATA lt_file_data          TYPE TABLE OF raw255 WITH EMPTY KEY.
    DATA lv_content            TYPE xstring.
    DATA lt_transport_requests LIKE rt_result.

    cl_gui_frontend_services=>gui_upload( EXPORTING  filename                = |{ iv_path }\\{ iv_filename }|
                                                     filetype                = 'BIN'
                                          CHANGING   data_tab                = lt_file_data
                                          EXCEPTIONS file_open_error         = 1
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
      RAISE EXCEPTION TYPE zcx_adu_transport_request
        EXPORTING
          textid = VALUE #( msgid = sy-msgid
                            msgno = sy-msgno
                            attr1 = 'TEXT1'
                            attr2 = 'TEXT2'
                            attr3 = 'TEXT3'
                            attr4 = 'TEXT4' )
          text1  = CONV #( sy-msgv1 )
          text2  = CONV #( sy-msgv2 )
          text3  = CONV #( sy-msgv3 )
          text4  = CONV #( sy-msgv4 ).
    ENDIF.

    CONCATENATE LINES OF lt_file_data INTO lv_content IN BYTE MODE.

    DATA(lo_zip) = NEW cl_abap_zip( ).

    lo_zip->load( lv_content ).

    DATA(li_utility) = zcl_adu_utl_transport_request=>get( ).

    LOOP AT lo_zip->files ASSIGNING FIELD-SYMBOL(<ls_file>).

      IF li_utility->is_filename_valid( <ls_file>-name ) = abap_false.
        CONTINUE.
      ENDIF.

      DATA(lv_transport_request) = li_utility->convert_filename_to_tr( <ls_file>-name ).

      ASSIGN lt_transport_requests[ transport_request = lv_transport_request
                                    zip_filename      = iv_filename ]
             TO FIELD-SYMBOL(<ls_transport_request>).
      IF sy-subrc <> 0 OR <ls_transport_request> IS NOT ASSIGNED.
        INSERT VALUE #( transport_request = lv_transport_request
                        path              = iv_path
                        zip_filename      = iv_filename )
               INTO TABLE lt_transport_requests ASSIGNING <ls_transport_request>.
      ENDIF.

      CASE <ls_file>-name(1).
        WHEN 'K'.
          <ls_transport_request>-cofile_filename = <ls_file>-name.

        WHEN 'R'.
          <ls_transport_request>-data_filename = <ls_file>-name.

        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_adu_transport_request
            EXPORTING
              textid = zcx_adu_transport_request=>filename_not_supported
              text1  = |{ <ls_file>-name } ({ iv_filename })|.

      ENDCASE.

      UNASSIGN <ls_transport_request>.

    ENDLOOP.

    rt_result = lt_transport_requests.

  ENDMETHOD.


  METHOD on_user_command.

    CASE e_salv_function.
      WHEN 'STMSIMPORT'.
        CALL TRANSACTION 'STMS_IMPORT' WITH AUTHORITY-CHECK.

      WHEN 'IMPORTFILE'.
        import_selected_files( ).

    ENDCASE.

  ENDMETHOD.


  METHOD import_selected_files.

    DATA lt_result    TYPE zif_adu_utl_transport_request=>ty_import_results.
    DATA lt_file_data TYPE TABLE OF raw255 WITH EMPTY KEY.
    DATA lv_content   TYPE xstring.

    DATA(lt_rows) = mo_salv->get_selections( )->get_selected_rows( ).

    DATA(li_utility) = zcl_adu_utl_transport_request=>get( ).

    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<lv_row>).
      CLEAR lt_result.

      DATA(lr_file) = REF #( mt_files[ <lv_row> ] ).

      cl_progress_indicator=>progress_indicate(
          i_text               = |Importing: { lr_file->transport_request } ({ <lv_row> }/{ lines( lt_rows ) })|
          i_output_immediately = abap_true ).

      IF lr_file->zip_filename IS NOT INITIAL.

        CLEAR lt_file_data.

        cl_gui_frontend_services=>gui_upload(
          EXPORTING  filename                = |{ lr_file->path }\\{ lr_file->zip_filename }|
                     filetype                = 'BIN'
          CHANGING   data_tab                = lt_file_data
          EXCEPTIONS file_open_error         = 1
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
          lr_file->status = 'ERROR'.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                  INTO lr_file->message.
          CONTINUE.
        ENDIF.

        CONCATENATE LINES OF lt_file_data INTO lv_content IN BYTE MODE.

        TRY.
            lt_result = li_utility->zip_import( lv_content ).
          CATCH zcx_adu_transport_request INTO DATA(lx_transport_request).
            lr_file->status  = 'ERROR'.
            lr_file->message = lx_transport_request->get_text( ).
            CONTINUE.
        ENDTRY.

      ELSE.

        DATA(ls_content) =
          VALUE zif_adu_utl_transport_request=>ty_content( transport_request = lr_file->transport_request ).

        CLEAR lt_file_data.

        cl_gui_frontend_services=>gui_upload(
          EXPORTING  filename                = |{ lr_file->path }\\{ lr_file->cofile_filename }|
                     filetype                = 'BIN'
          IMPORTING  filelength              = ls_content-cofile_filesize
          CHANGING   data_tab                = ls_content-cofile
          EXCEPTIONS file_open_error         = 1
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
          lr_file->status = 'ERROR'.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                  INTO lr_file->message.
          CONTINUE.
        ENDIF.

        CLEAR lt_file_data.

        cl_gui_frontend_services=>gui_upload(
          EXPORTING  filename                = |{ lr_file->path }\\{ lr_file->data_filename }|
                     filetype                = 'BIN'
          IMPORTING  filelength              = ls_content-data_filesize
          CHANGING   data_tab                = ls_content-data
          EXCEPTIONS file_open_error         = 1
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
          lr_file->status = 'ERROR'.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                  INTO lr_file->message.
          CONTINUE.
        ENDIF.

        TRY.
            lt_result = zcl_adu_utl_transport_request=>get( )->import( VALUE #( ( ls_content ) ) ).
          CATCH zcx_adu_transport_request INTO lx_transport_request.
            lr_file->status  = 'ERROR'.
            lr_file->message = lx_transport_request->get_text( ).
            CONTINUE.
        ENDTRY.

      ENDIF.

      IF lt_result IS NOT INITIAL.
        lr_file->status  = lt_result[ 1 ]-tms_alert-error.
        lr_file->message = lt_result[ 1 ]-tms_alert-text.
      ELSE.
        lr_file->status  = 'OK'.
      ENDIF.

      IF lr_file->message IS INITIAL.
        lr_file->message = 'Imported'.
      ENDIF.

    ENDLOOP.

    mo_salv->refresh( ).

  ENDMETHOD.


ENDCLASS.
