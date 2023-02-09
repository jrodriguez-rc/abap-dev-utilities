CLASS ltcl_pack DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    METHODS no_size FOR TESTING RAISING zcx_adu_messages.
    METHODS with_size_3_pack FOR TESTING RAISING zcx_adu_messages.
    METHODS with_size_2_pack FOR TESTING RAISING zcx_adu_messages.

ENDCLASS.


CLASS ltcl_pack IMPLEMENTATION.


  METHOD no_size.

    DATA(lt_int) = VALUE int4_table( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ( 6 ) ( 7 ) ( 8 ) ( 9 ) ).
    DATA(lt_int_) = VALUE int4_table( ).
    DATA(lt_int_result) = VALUE int4_table( ).

    DATA(li_pack) = zcl_adu_table_pack=>create( iv_packet_size = 0 it_table = lt_int ).

    li_pack->get_next( IMPORTING et_table = lt_int_ ).

    WHILE lt_int_ IS NOT INITIAL.

      INSERT LINES OF lt_int_ INTO TABLE lt_int_result.

      li_pack->get_next( IMPORTING et_table = lt_int_ ).

    ENDWHILE.

    cl_abap_unit_assert=>assert_equals(
      act = lt_int
      exp = lt_int_result ).

  ENDMETHOD.


  METHOD with_size_3_pack.

    DATA(lt_int) = VALUE int4_table( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ( 6 ) ( 7 ) ( 8 ) ( 9 ) ).
    DATA(lt_int_) = VALUE int4_table( ).
    DATA(lt_int_result) = VALUE int4_table( ).

    DATA(li_pack) = zcl_adu_table_pack=>create( iv_packet_size = 3 it_table = lt_int ).

    li_pack->get_next( IMPORTING et_table = lt_int_ ).

    WHILE lt_int_ IS NOT INITIAL.

      INSERT LINES OF lt_int_ INTO TABLE lt_int_result.

      li_pack->get_next( IMPORTING et_table = lt_int_ ).

    ENDWHILE.

    cl_abap_unit_assert=>assert_equals(
      act = lt_int
      exp = lt_int_result ).

  ENDMETHOD.


  METHOD with_size_2_pack.

    DATA(lt_int) = VALUE int4_table( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ( 6 ) ( 7 ) ( 8 ) ( 9 ) ).
    DATA(lt_int_) = VALUE int4_table( ).
    DATA(lt_int_result) = VALUE int4_table( ).

    DATA(li_pack) = zcl_adu_table_pack=>create( iv_packet_size = 2 it_table = lt_int ).

    li_pack->get_next( IMPORTING et_table = lt_int_ ).

    WHILE lt_int_ IS NOT INITIAL.

      INSERT LINES OF lt_int_ INTO TABLE lt_int_result.

      li_pack->get_next( IMPORTING et_table = lt_int_ ).

    ENDWHILE.

    cl_abap_unit_assert=>assert_equals(
      act = lt_int
      exp = lt_int_result ).

  ENDMETHOD.


ENDCLASS.
