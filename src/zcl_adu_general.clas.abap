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


  METHOD get_instance.

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


ENDCLASS.
