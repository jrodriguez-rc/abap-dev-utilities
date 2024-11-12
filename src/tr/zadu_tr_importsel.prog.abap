*&---------------------------------------------------------------------*
*&  Include  zadu_tr_importsel
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK opt WITH FRAME TITLE TEXT-opt.

  PARAMETERS p_zip TYPE abap_bool AS CHECKBOX.
  PARAMETERS p_recurs TYPE abap_bool AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK opt.

SELECTION-SCREEN BEGIN OF BLOCK path WITH FRAME TITLE TEXT-pat.

  PARAMETERS p_path TYPE string LOWER CASE OBLIGATORY.

SELECTION-SCREEN END OF BLOCK path.
