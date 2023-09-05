*&---------------------------------------------------------------------*
*& Report zadu_tr_import_zip
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zadu_tr_import_zip.

INCLUDE zadu_tr_import_zipsel.
INCLUDE zadu_tr_import_zipcsl.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  lcl_process=>help_path( ).

START-OF-SELECTION.
  NEW lcl_process( )->run( ).
