CLASS ltcl_itf DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    METHODS itf_to_string FOR TESTING.

ENDCLASS.


CLASS ltcl_itf IMPLEMENTATION.


  METHOD itf_to_string.

    DATA(lv_expected) =
        `Line 1                                                                                              ` &&
        `                                ` &&
        `Still on line 1                                                                                     ` &&
        `                                ` &&
        `Still on line 1` &&
        cl_abap_char_utilities=>newline &&
        `Line 2                                                                                              ` &&
        `                                ` &&
        `Still on line 2` &&
        cl_abap_char_utilities=>newline &&
        `Line 3`.

    DATA(lv_string) =
        zcl_adu_texts=>get( )->itf_to_string( VALUE #( ( tdformat = '*' tdline = 'Line 1' )
                                                       ( tdformat = ' ' tdline = 'Still on line 1' )
                                                       ( tdformat = ' ' tdline = 'Still on line 1' )
                                                       ( tdformat = '/' tdline = 'Line 2' )
                                                       ( tdformat = ' ' tdline = 'Still on line 2' )
                                                       ( tdformat = '*' tdline = 'Line 3' ) ) ).

    cl_abap_unit_assert=>assert_equals( act = lv_string exp = lv_expected ).

  ENDMETHOD.


ENDCLASS.
