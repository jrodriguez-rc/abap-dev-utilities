CLASS zcl_adu_transform DEFINITION
  PUBLIC ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_adu_transform.

    METHODS constructor
      IMPORTING io_datadescr TYPE REF TO cl_abap_datadescr.

  PROTECTED SECTION.

    METHODS get_type_result FINAL
      RETURNING VALUE(ro_result) TYPE REF TO cl_abap_datadescr.

    METHODS set_unix_timestamp
      IMPORTING iv_enable TYPE abap_bool DEFAULT abap_true.

    METHODS is_type_boolean
      IMPORTING io_type       TYPE REF TO cl_abap_datadescr
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_type_timestamp
      IMPORTING io_type       TYPE REF TO cl_abap_datadescr
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS build_structure
      IMPORTING io_structure  TYPE REF TO cl_abap_structdescr
      RETURNING VALUE(result) TYPE REF TO cl_abap_structdescr.

    METHODS build_table
      IMPORTING io_table      TYPE REF TO cl_abap_tabledescr
      RETURNING VALUE(result) TYPE REF TO cl_abap_tabledescr.

    METHODS build_element
      IMPORTING io_element    TYPE REF TO cl_abap_elemdescr
                is_component  TYPE abap_componentdescr OPTIONAL ##NEEDED
      RETURNING VALUE(result) TYPE REF TO cl_abap_elemdescr.

    METHODS process_table
      IMPORTING io_table  TYPE REF TO cl_abap_tabledescr
                it_table  TYPE ANY TABLE
      EXPORTING et_result TYPE ANY TABLE.

    METHODS process_structure
      IMPORTING io_structure TYPE REF TO cl_abap_structdescr
                is_data      TYPE any
      EXPORTING es_result    TYPE any.

    METHODS process_element
      IMPORTING io_element   TYPE REF TO cl_abap_elemdescr
                is_component TYPE abap_componentdescr OPTIONAL ##NEEDED
                iv_data      TYPE any
      EXPORTING ev_result    TYPE any.

  PRIVATE SECTION.

    DATA mo_type_original  TYPE REF TO cl_abap_datadescr.
    DATA mo_type_result    TYPE REF TO cl_abap_datadescr.
    DATA mv_unix_timestamp TYPE abap_bool.

ENDCLASS.



CLASS ZCL_ADU_TRANSFORM IMPLEMENTATION.


  METHOD build_element.

    CASE io_element->type_kind.
      WHEN cl_abap_typedescr=>typekind_packed.

        IF mv_unix_timestamp = abap_true AND is_type_timestamp( io_element ).
          result = cl_abap_elemdescr=>get_p( p_length   = 13
                                             p_decimals = 0 ).
        ELSE.
          result = io_element.
        ENDIF.

      WHEN OTHERS.
        result = io_element.

    ENDCASE.

  ENDMETHOD.


  METHOD build_structure.

    DATA(lt_components) = io_structure->get_components( ).

    LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_component>).

      CASE <ls_component>-type->kind.
        WHEN cl_abap_typedescr=>kind_table.
          <ls_component>-type = build_table( CAST #( <ls_component>-type ) ).

        WHEN cl_abap_typedescr=>kind_struct.
          <ls_component>-type = build_structure( CAST #( <ls_component>-type ) ).

        WHEN cl_abap_typedescr=>kind_elem.
          <ls_component>-type = build_element( io_element   = CAST #( <ls_component>-type )
                                               is_component = <ls_component> ).

      ENDCASE.

    ENDLOOP.

    result = cl_abap_structdescr=>get( lt_components ).

  ENDMETHOD.


  METHOD build_table.

    DATA lo_line_new_type TYPE REF TO cl_abap_datadescr.

    DATA(lo_line) = io_table->get_table_line_type( ).

    lo_line_new_type =
        SWITCH #( lo_line->kind
                  WHEN cl_abap_typedescr=>kind_table  THEN build_table( CAST #( lo_line ) )
                  WHEN cl_abap_typedescr=>kind_struct THEN build_structure( CAST #( lo_line ) )
                  WHEN cl_abap_typedescr=>kind_elem   THEN build_element( CAST #( lo_line ) )
                  ELSE                                     lo_line ).

    result = cl_abap_tabledescr=>create( lo_line_new_type ).

  ENDMETHOD.


  METHOD constructor.

    mo_type_original = io_datadescr.

  ENDMETHOD.


  METHOD get_type_result.

    IF mo_type_result IS NOT BOUND.

      mo_type_result =
          SWITCH #( mo_type_original->kind
                    WHEN cl_abap_typedescr=>kind_table  THEN build_table( CAST #( mo_type_original ) )
                    WHEN cl_abap_typedescr=>kind_struct THEN build_structure( CAST #( mo_type_original ) )
                    WHEN cl_abap_typedescr=>kind_elem   THEN build_element( CAST #( mo_type_original ) ) ).

    ENDIF.

    ro_result = mo_type_result.

  ENDMETHOD.


  METHOD is_type_boolean.

    TRY.
        DATA(lo_element) = CAST cl_abap_elemdescr( io_type ).
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    result = xsdbool(    lo_element->absolute_name = '\TYPE=XSDBOOLEAN'
                      OR lo_element->absolute_name = '\TYPE-POOL=ABAP\TYPE=ABAP_BOOL'
                      OR lo_element->absolute_name = '\TYPE=DDBOOL_D' ).
    IF result = abap_true OR lo_element->is_ddic_type( ) = abap_false.
      RETURN.
    ENDIF.

    lo_element->get_ddic_field( RECEIVING  p_flddescr   = DATA(field_description)
                                EXCEPTIONS not_found    = 1                " Type could not be found
                                           no_ddic_type = 2                " Typ is not a dictionary type
                                           OTHERS       = 3 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = xsdbool(    field_description-domname = 'XSDBOOLEAN'
                      OR field_description-domname = 'DDBOOL'
                      OR field_description-domname = 'ABAP_BOOLEAN' ).

  ENDMETHOD.


  METHOD is_type_timestamp.

    result = xsdbool(    io_type->get_relative_name( ) = 'TIMESTAMP'
                      OR io_type->get_relative_name( ) = 'TIMESTAMPL' ).
    IF result = abap_true.
      RETURN.
    ENDIF.

    io_type->get_ddic_header( RECEIVING  p_header     = DATA(ls_ddic_header)
                              EXCEPTIONS not_found    = 1                " Type could not be found
                                         no_ddic_type = 2                " Typ is not a dictionary type
                                         OTHERS       = 3 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = xsdbool(    ls_ddic_header-refname = 'TZNTSTMPS'
                      OR ls_ddic_header-refname = 'TZNTSTMPL' ).

  ENDMETHOD.


  METHOD process_element.

    CLEAR ev_result.

    CASE io_element->type_kind.
      WHEN cl_abap_typedescr=>typekind_packed.

        IF mv_unix_timestamp = abap_true AND is_type_timestamp( io_element ).
          ev_result = zcl_adu_convert=>time->timestamp_java_to_abap( CONV #( iv_data ) ).
        ELSE.
          ev_result = iv_data.
        ENDIF.

      WHEN OTHERS.
        ev_result = iv_data.

    ENDCASE.

  ENDMETHOD.


  METHOD process_structure.

    CLEAR es_result.

    DATA(lt_components) = io_structure->get_components( ).

    LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_component>).

      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE is_data TO FIELD-SYMBOL(<lg_from>).
      IF sy-subrc <> 0 OR <lg_from> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE es_result TO FIELD-SYMBOL(<lg_to>).
      IF sy-subrc <> 0 OR <lg_to> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      CASE <ls_component>-type->kind.
        WHEN cl_abap_typedescr=>kind_table.

          process_table( EXPORTING io_table  = CAST #( <ls_component>-type )
                                   it_table  = <lg_from>
                         IMPORTING et_result = <lg_to> ).

        WHEN cl_abap_typedescr=>kind_struct.

          process_structure( EXPORTING io_structure = CAST #( <ls_component>-type )
                                       is_data      = <lg_from>
                             IMPORTING es_result    = <lg_to> ).

        WHEN cl_abap_typedescr=>kind_elem.

          process_element( EXPORTING io_element   = CAST #( <ls_component>-type )
                                     is_component = <ls_component>
                                     iv_data      = <lg_from>
                           IMPORTING ev_result    = <lg_to> ).

        WHEN OTHERS.
          <lg_to> = <lg_from>.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD process_table.

    DATA lr_line TYPE REF TO data.

    CLEAR et_result.

    DATA(lo_line) = io_table->get_table_line_type( ).

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<ls_table>).
      CLEAR lr_line.

      CREATE DATA lr_line LIKE LINE OF et_result.
      ASSIGN lr_line->* TO FIELD-SYMBOL(<lg_line>).

      CASE lo_line->kind.
        WHEN cl_abap_typedescr=>kind_table.

          process_table( EXPORTING io_table  = CAST #( lo_line )
                                   it_table  = <ls_table>
                         IMPORTING et_result = <lg_line> ).

        WHEN cl_abap_typedescr=>kind_struct.

          process_structure( EXPORTING io_structure = CAST #( lo_line )
                                       is_data      = <ls_table>
                             IMPORTING es_result    = <lg_line> ).

        WHEN cl_abap_typedescr=>kind_elem.

          process_element( EXPORTING io_element = CAST #( lo_line )
                                     iv_data    = <ls_table>
                           IMPORTING ev_result  = <lg_line> ).

      ENDCASE.

      INSERT <lg_line> INTO TABLE et_result.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_unix_timestamp.

    IF mv_unix_timestamp = iv_enable.
      RETURN.
    ENDIF.

    mv_unix_timestamp = iv_enable.
    CLEAR mo_type_result.

  ENDMETHOD.


  METHOD zif_adu_transform~create_result_type.

    DATA(lo_type_result) = get_type_result( ).

    CREATE DATA result TYPE HANDLE lo_type_result.

  ENDMETHOD.


  METHOD zif_adu_transform~move_results.

    CLEAR output.

    CASE mo_type_original->kind.
      WHEN cl_abap_typedescr=>kind_table.

        process_table( EXPORTING io_table  = CAST #( mo_type_original )
                                 it_table  = input
                       IMPORTING et_result = output ).

      WHEN cl_abap_typedescr=>kind_struct.

        process_structure( EXPORTING io_structure = CAST #( mo_type_original )
                                     is_data      = input
                           IMPORTING es_result    = output ).

      WHEN cl_abap_typedescr=>kind_elem.

        process_element( EXPORTING io_element = CAST #( mo_type_original )
                                   iv_data    = input
                         IMPORTING ev_result  = output ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
