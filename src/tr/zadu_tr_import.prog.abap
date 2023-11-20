*&---------------------------------------------------------------------*
*& Report zadu_tr_import
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zadu_tr_import.

INCLUDE zadu_tr_importsel.
INCLUDE zadu_tr_importcsl.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  lcl_process=>help_path( ).

START-OF-SELECTION.
  NEW lcl_process( )->run( ).
