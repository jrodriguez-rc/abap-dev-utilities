CLASS zcl_adu_convert DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-DATA time TYPE REF TO zif_adu_convert_time READ-ONLY.

    CLASS-METHODS class_constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_adu_convert IMPLEMENTATION.


  METHOD class_constructor.

    time = NEW lcl_time( ).

  ENDMETHOD.


ENDCLASS.
