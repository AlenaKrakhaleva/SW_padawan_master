*&---------------------------------------------------------------------*
*& Include          ZSW_DATA_SELECTION
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: r_but1 RADIOBUTTON GROUP r1 TYPE seu_radiob DEFAULT 'X',
              r_but2 RADIOBUTTON GROUP r1 TYPE seu_radiob,
              r_but3 RADIOBUTTON GROUP r1 TYPE seu_radiob.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: r_but4 RADIOBUTTON GROUP r2 TYPE SEU_RADIOB DEFAULT 'X',
              r_but5 RADIOBUTTON GROUP r2 TYPE SEU_RADIOB.
SELECTION-SCREEN END OF BLOCK b2.
