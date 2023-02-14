CLASS zcl_adu_table_pack DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_adu_table_pack.

    CLASS-METHODS create
      IMPORTING
        iv_packet_size TYPE i
        it_table       TYPE ANY TABLE
      RETURNING
        VALUE(result)  TYPE REF TO zif_adu_table_pack
      RAISING
        zcx_adu_messages.

    METHODS constructor
      IMPORTING
        iv_packet_size TYPE i
        it_table       TYPE ANY TABLE
      RAISING
        zcx_adu_messages.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_line,
        index TYPE i,
        data  TYPE REF TO data,
      END OF ty_line,
      ty_table TYPE SORTED TABLE OF ty_line
        WITH UNIQUE KEY index.

    DATA:
      mv_packet_size TYPE i,
      mv_iterator    TYPE i,
      mt_table       TYPE ty_table,
      mo_type        TYPE REF TO cl_abap_tabledescr.

    METHODS get_next_internal
      RETURNING
        VALUE(result) TYPE REF TO data.

ENDCLASS.



CLASS zcl_adu_table_pack IMPLEMENTATION.


  METHOD create.

    result = NEW zcl_adu_table_pack( iv_packet_size = iv_packet_size it_table = it_table ).

  ENDMETHOD.


  METHOD constructor.

    DATA:
      lr_line TYPE REF TO data.

    FIELD-SYMBOLS:
      <lt_table> TYPE ANY TABLE.

    mv_packet_size = iv_packet_size.

    TRY.
        mo_type = CAST #( cl_abap_tabledescr=>describe_by_data( it_table ) ).
      CATCH cx_sy_move_cast_error.
        CLEAR mo_type.
    ENDTRY.

    IF mo_type IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_adu_messages
        EXPORTING
          textid = zcx_adu_messages=>data_type_not_compatible.
    ENDIF.

    DATA(lv_index) = 0.

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<ls_line>).

      lv_index = lv_index + 1.

      DATA(ls_line) = VALUE ty_line( index = lv_index ).

      CREATE DATA ls_line-data LIKE <ls_line>.
      ASSIGN ls_line-data->* TO FIELD-SYMBOL(<ls_line_current>).
      <ls_line_current> = <ls_line>.

      INSERT ls_line INTO TABLE mt_table.

    ENDLOOP.

    IF mv_packet_size = 0.
      mv_packet_size = lines( mt_table ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_adu_table_pack~get_next.

    FIELD-SYMBOLS:
      <lt_result> TYPE ANY TABLE.

    CLEAR:
      et_table.

    DATA(lr_result) = get_next_internal( ).

    IF lr_result IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN lr_result->* TO <lt_result>.

    et_table = <lt_result>.

  ENDMETHOD.


  METHOD get_next_internal.

    DATA:
      lr_result TYPE REF TO data.

    FIELD-SYMBOLS:
      <lt_result> TYPE ANY TABLE.

    IF mv_iterator >= lines( mt_table ).
      RETURN.
    ENDIF.

    CREATE DATA lr_result TYPE HANDLE mo_type.

    ASSIGN lr_result->* TO <lt_result>.

    LOOP AT mt_table ASSIGNING FIELD-SYMBOL(<ls_line>)
        WHERE index BETWEEN ( mv_iterator + 1 ) AND ( mv_iterator + mv_packet_size ).

      ASSIGN <ls_line>-data->* TO FIELD-SYMBOL(<ls_data>).

      INSERT <ls_data> INTO TABLE <lt_result>.

    ENDLOOP.

    result = lr_result.

    mv_iterator = mv_iterator + lines( <lt_result> ).

  ENDMETHOD.


ENDCLASS.
