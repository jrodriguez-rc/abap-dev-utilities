"! <p class="shorttext synchronized">Serialize: Check Transport</p>
CLASS zcl_adu_oo_serialize DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS deserialize
      IMPORTING serialized_object TYPE string
      RETURNING VALUE(result)     TYPE REF TO if_serializable_object.

    METHODS serialize
      IMPORTING serializable_object TYPE REF TO if_serializable_object
      RETURNING VALUE(result)       TYPE string.

    METHODS serialize_json
      IMPORTING serializable_object TYPE REF TO if_serializable_object
      RETURNING VALUE(result)       TYPE string
      RAISING   zcx_adu_oo_serialize.

    METHODS save
      IMPORTING !type               TYPE zadu_oo_serializ-type
                !code               TYPE zadu_oo_serializ-code
                serializable_object TYPE REF TO if_serializable_object
                !timestamp          TYPE timestampl OPTIONAL
                json                TYPE abap_bool  DEFAULT abap_true
      RAISING   zcx_adu_oo_serialize.

    METHODS save_serialized_object
      IMPORTING !type      TYPE zadu_oo_serializ-type
                !code      TYPE zadu_oo_serializ-code
                !object    TYPE string
                !timestamp TYPE timestampl OPTIONAL
      RAISING   zcx_adu_oo_serialize.

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


  METHOD serialize_json.

    TRY.

        DATA(writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
        CALL TRANSFORMATION id
             SOURCE input = serializable_object
             RESULT XML writer.

        cl_abap_conv_in_ce=>create( input = writer->get_output( ) )->read( IMPORTING data = result ).

      CATCH cx_root INTO DATA(conversion_exception).
        RAISE EXCEPTION TYPE zcx_adu_oo_serialize
          EXPORTING
            previous = conversion_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD save.
    " TODO: parameter TIMESTAMP is never used (ABAP cleaner)

    save_serialized_object( type   = type
                            code   = code
                            object = COND #( WHEN json = abap_false
                                             THEN serialize( serializable_object )
                                             ELSE serialize_json( serializable_object ) ) ).

  ENDMETHOD.


  METHOD save_serialized_object.

    IF type IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adu_oo_serialize
        EXPORTING
          textid = zcx_adu_oo_serialize=>type_is_empty.
    ENDIF.

    IF code IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adu_oo_serialize
        EXPORTING
          textid = zcx_adu_oo_serialize=>code_is_empty.
    ENDIF.

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
