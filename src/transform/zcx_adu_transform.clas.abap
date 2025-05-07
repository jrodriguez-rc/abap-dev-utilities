CLASS zcx_adu_transform DEFINITION
  PUBLIC
  INHERITING FROM zcx_adu_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING textid    LIKE if_t100_message=>t100key OPTIONAL
                text1     TYPE csequence                OPTIONAL
                text2     TYPE csequence                OPTIONAL
                text3     TYPE csequence                OPTIONAL
                text4     TYPE csequence                OPTIONAL
                !previous LIKE previous                 OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_adu_transform IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( textid   = textid
                        text1    = text1
                        text2    = text2
                        text3    = text3
                        text4    = text4
                        previous = previous ).

  ENDMETHOD.


ENDCLASS.
