CLASS zcl_adu_gos DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adu_gos ALL METHODS FINAL.

    CLASS-METHODS create_for_object
      IMPORTING
        is_object     TYPE gos_s_obj
      RETURNING
        VALUE(result) TYPE REF TO zif_adu_gos
      RAISING
        cx_gos_api.

    METHODS constructor
      IMPORTING
        is_object TYPE gos_s_obj
      RAISING
        cx_gos_api.

  PROTECTED SECTION.
    METHODS get_api FINAL
      RETURNING
        VALUE(result) TYPE REF TO cl_gos_api
      RAISING
        cx_gos_api.

    METHODS get_mimetype_from_extension
      IMPORTING
        iv_extension  TYPE c
      RETURNING
        VALUE(result) TYPE w3conttype.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_file_name_extension,
        filename  TYPE string,
        extension TYPE string,
      END OF ty_file_name_extension.

    DATA:
      ms_object TYPE gos_s_obj.

    METHODS split_file_name_extension
      IMPORTING
        iv_filename   TYPE string
      RETURNING
        VALUE(result) TYPE ty_file_name_extension.

ENDCLASS.



CLASS zcl_adu_gos IMPLEMENTATION.


  METHOD create_for_object.

    result = NEW zcl_adu_gos( is_object ).

  ENDMETHOD.


  METHOD constructor.

    ms_object = is_object.

  ENDMETHOD.


  METHOD zif_adu_gos~attach.

    IF it_files IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_api) = get_api( ).

    DATA(lv_commit) = VALUE abap_bool( ).

    LOOP AT it_files INTO DATA(ls_file).

      DATA(lv_extension) = VALUE sdba_funct( ).

      DATA(ls_attachment) =
        VALUE gos_s_attcont(
            atta_cat  = cl_gos_api=>c_msg
            filename  = ls_file-filename
            descr     = ls_file-description
            tech_type = split_file_name_extension( ls_file-filename )-extension
            content   = ls_file-content
            content_x = ls_file-content_x ).

      TRY.
          DATA(lv_ok) = lo_api->insert_al_item( is_attcont = ls_attachment iv_roltype = cl_gos_api=>c_attachment ).
        CLEANUP.
          ROLLBACK WORK.
      ENDTRY.

      lv_commit = xsdbool( lv_commit = abap_true OR lv_ok = abap_true ).

    ENDLOOP.

    IF lv_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD zif_adu_gos~get_attachment_list.

    result = get_api( )->get_atta_list( ).

  ENDMETHOD.


  METHOD zif_adu_gos~get_attachment.

    DATA(ls_attachment) = get_api( )->get_al_item( is_key ).

    result =
        VALUE #(
            attachment = ls_attachment
            tech_type  = to_lower( ls_attachment-tech_type )
            mimetype   = get_mimetype_from_extension( ls_attachment-tech_type ) ).

  ENDMETHOD.


  METHOD zif_adu_gos~get_attachments.

    result =
        VALUE #(
            FOR ls_attachment IN zif_adu_gos~get_attachment_list( )
            ( zif_adu_gos~get_attachment( CORRESPONDING #( ls_attachment ) ) ) ).

  ENDMETHOD.


  METHOD get_api.

    result = cl_gos_api=>create_instance( ms_object ).

  ENDMETHOD.


  METHOD get_mimetype_from_extension.

    " Use table SDOKMIME_C to specify custom MIME Types for extensions
    CALL FUNCTION 'SDOK_MIMETYPE_GET'
      EXPORTING
        extension = iv_extension
      IMPORTING
        mimetype  = result.

  ENDMETHOD.


  METHOD split_file_name_extension.

    DATA:
      lv_file_name_c    TYPE c LENGTH 255,
      lv_file_extension TYPE c LENGTH 10.

    IF iv_filename IS INITIAL.
      RETURN.
    ENDIF.

    lv_file_name_c = iv_filename.

    CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
      EXPORTING
        filename  = lv_file_name_c
        uppercase = abap_false
      IMPORTING
        extension = lv_file_extension.

    result-extension = lv_file_extension.

    IF lv_file_extension IS INITIAL.
      result-filename = iv_filename.
    ELSE.

      DATA(lv_filename_length) = ( strlen( iv_filename ) - strlen( lv_file_extension ) ) - 1.

      result-filename = iv_filename(lv_filename_length).

    ENDIF.

  ENDMETHOD.


ENDCLASS.
