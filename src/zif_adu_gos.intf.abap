INTERFACE zif_adu_gos
  PUBLIC.

  TYPES:
    BEGIN OF ty_file_content,
      filename    TYPE string,
      description TYPE string,
      content     TYPE string,
      content_x   TYPE xstring,
    END OF ty_file_content,
    ty_files_content TYPE STANDARD TABLE OF ty_file_content
      WITH KEY filename.

  METHODS attach
    IMPORTING
      it_files TYPE ty_files_content
    RAISING
      cx_gos_api.

  METHODS get_attachment_list
    RETURNING
      VALUE(result) TYPE gos_t_atta
    RAISING
      cx_gos_api.

  METHODS get_attachment
    IMPORTING
      is_key        TYPE gos_s_attkey
    RETURNING
      VALUE(result) TYPE zadu_s_gos_attachment
    RAISING
      cx_gos_api.

  METHODS get_attachments
    RETURNING
      VALUE(result) TYPE zadu_t_gos_attachment
    RAISING
      cx_gos_api.

ENDINTERFACE.
