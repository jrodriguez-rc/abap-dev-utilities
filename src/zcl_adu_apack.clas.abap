"! <p class="shorttext synchronized" lang="en">ABAP Development Utilities APACK</p>
CLASS zcl_adu_apack DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_apack_manifest.

    CONSTANTS:
      gc_group       TYPE string VALUE 'github.com/jrodriguez-rc' ##NO_TEXT,
      gc_artifact_id TYPE string VALUE 'abap-dev-utilities' ##NO_TEXT,
      gc_version     TYPE string VALUE '1.0.0' ##NO_TEXT,
      gc_repository  TYPE string VALUE 'https://github.com/jrodriguez-rc/abap-dev-utilities.git' ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_adu_apack IMPLEMENTATION.


  METHOD constructor.

    if_apack_manifest~descriptor = VALUE #( group_id    = gc_group
                                             artifact_id = gc_artifact_id
                                             version     = gc_version
                                             git_url     = gc_repository ).

  ENDMETHOD.


ENDCLASS.
