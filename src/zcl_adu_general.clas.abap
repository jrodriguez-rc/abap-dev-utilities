CLASS zcl_adu_general DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_adu_general.

    ALIASES get          FOR zif_adu_general~get.
    ALIASES get_instance FOR zif_adu_general~get.

    METHODS constructor.

  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO zif_adu_general.

ENDCLASS.



CLASS zcl_adu_general IMPLEMENTATION.


  METHOD constructor.

    instance = me.

  ENDMETHOD.


  METHOD zif_adu_general~get.

    result = COND #( WHEN instance IS BOUND THEN instance ELSE NEW zcl_adu_general( ) ).

  ENDMETHOD.


  METHOD zif_adu_general~get_structure_fields.

    TRY.
        DATA(structure) = CAST cl_abap_structdescr(
                COND #( WHEN structure_description IS BOUND
                          THEN structure_description
                        WHEN structure_name IS NOT INITIAL
                          THEN CAST #( cl_abap_structdescr=>describe_by_name( structure_name ) ) ) ).
      CATCH cx_sy_move_cast_error.
        CLEAR structure.
    ENDTRY.

    IF structure IS NOT BOUND.
      RETURN.
    ENDIF.

    result =
        REDUCE #( INIT all_components TYPE cl_abap_structdescr=>component_table
            FOR component IN structure->get_components( )
                NEXT all_components =
                    COND #(
                        WHEN component-as_include = abap_false
                          THEN VALUE #( BASE all_components
                                        ( component ) )
                        WHEN recursive = abap_true
                          THEN VALUE #( BASE all_components
                                        FOR field IN zif_adu_general~get_structure_fields(
                                                         structure_description = component-type
                                                         recursive             = abap_true )
                                        ( field ) )
                        ELSE
                          all_components ) ).

  ENDMETHOD.


  METHOD zif_adu_general~split_filename_extension.

    DATA lv_file_name_c    TYPE c LENGTH 255.
    DATA lv_file_extension TYPE c LENGTH 20.

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


  METHOD zif_adu_general~get_timestamp.

    GET TIME STAMP FIELD result.

  ENDMETHOD.


  METHOD zif_adu_general~get_date.

    DATA(lv_timestamp) = zif_adu_general~get_timestamp( ).

    CONVERT TIME STAMP lv_timestamp
            TIME ZONE zif_adu_general~get_system_time_zone( )
            INTO DATE result.

  ENDMETHOD.


  METHOD zif_adu_general~get_time.

    DATA(lv_timestamp) = zif_adu_general~get_timestamp( ).

    CONVERT TIME STAMP lv_timestamp
            TIME ZONE zif_adu_general~get_system_time_zone( )
            INTO TIME result.

  ENDMETHOD.


  METHOD zif_adu_general~get_system_time_zone.

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone = result.

  ENDMETHOD.


ENDCLASS.
