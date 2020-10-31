"! <p class="shorttext synchronized" lang="en">Serialize: Check Transport</p>
CLASS zcl_adu_oo_serialize DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS deserialize
      IMPORTING
        serialized_object TYPE string
      RETURNING
        VALUE(result)     TYPE REF TO if_serializable_object.

    METHODS serialize
      IMPORTING
        serializable_object TYPE REF TO if_serializable_object
      RETURNING
        VALUE(result)       TYPE string.

    METHODS save
      IMPORTING
        type                TYPE zadu_oo_serializ-type
        code                TYPE zadu_oo_serializ-code
        serializable_object TYPE REF TO if_serializable_object
        timestamp           TYPE timestampl OPTIONAL.

    METHODS save_serialized_object
      IMPORTING
        type      TYPE zadu_oo_serializ-type
        code      TYPE zadu_oo_serializ-code
        object    TYPE string
        timestamp TYPE timestampl OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_adu_oo_serialize IMPLEMENTATION.


  METHOD deserialize.

    CALL TRANSFORMATION id
      SOURCE XML serialized_object
      RESULT input = result.

  ENDMETHOD.


  METHOD serialize.

    CALL TRANSFORMATION id
      SOURCE input = serializable_object
      RESULT XML result.

  ENDMETHOD.


  METHOD save.

    save_serialized_object( type   = type
                            code   = code
                            object = serialize( serializable_object ) ).

  ENDMETHOD.


  METHOD save_serialized_object.

    DATA(serialization) = VALUE zadu_oo_serializ( client = sy-mandt
                                                  type   = type
                                                  code   = code
                                                  object = object ).

    IF timestamp IS NOT INITIAL.
      serialization-timestamp = timestamp.
    ELSE.
      GET TIME STAMP FIELD serialization-timestamp.
    ENDIF.

    INSERT zadu_oo_serializ FROM serialization.

  ENDMETHOD.


ENDCLASS.
