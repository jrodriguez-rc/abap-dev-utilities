CLASS ltcl_structure_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.

  PRIVATE SECTION.
    METHODS components_from_simple_struc FOR TESTING
      RAISING zcx_adu_messages.

    METHODS components_from_complex_struc FOR TESTING
      RAISING zcx_adu_messages.

ENDCLASS.

CLASS ltcl_structure_test IMPLEMENTATION.


  METHOD components_from_simple_struc.

    DATA(utility) = zcl_adu_general=>get_instance( ).

    DATA(components) = utility->get_structure_fields( structure_name = 'BAPIRET2' ).

    cl_abap_unit_assert=>assert_equals( act = lines( components )
                                        exp = 14 ).

  ENDMETHOD.


  METHOD components_from_complex_struc.

    DATA:
      BEGIN OF ls_complex_struc.
        INCLUDE TYPE bapiret2.
    DATA:
        structure  TYPE bapiret2,
        field_test TYPE string,
      END OF ls_complex_struc.

    DATA(utility) = zcl_adu_general=>get_instance( ).

    DATA(components) =
        utility->get_structure_fields(
            structure_description = CAST #( cl_abap_datadescr=>describe_by_data( ls_complex_struc ) ) ).

    cl_abap_unit_assert=>assert_equals( act = lines( components )
                                        exp = 16 ).

  ENDMETHOD.


ENDCLASS.
