INTERFACE zif_adu_log
  PUBLIC.

  CONSTANTS:
    BEGIN OF gc_parameter,
      content_type TYPE balpar VALUE 'CTYPE',
      content      TYPE balpar VALUE 'DATA',
    END OF gc_parameter.

  CONSTANTS:
    BEGIN OF gc_message_class,
      very_important        TYPE balprobcl VALUE '1',
      important             TYPE balprobcl VALUE '2',
      medium                TYPE balprobcl VALUE '3',
      additinal_information TYPE balprobcl VALUE '4',
      other                 TYPE balprobcl VALUE '',
    END OF gc_message_class.

  METHODS add_exception
    IMPORTING
      ix_exception TYPE REF TO cx_root.

  METHODS add_message
    IMPORTING
      is_message TYPE bal_s_msg.

  METHODS add_content_json
    IMPORTING
      iv_json TYPE string.

  METHODS add_content_xml
    IMPORTING
      iv_xml TYPE string.

  METHODS save.

ENDINTERFACE.
