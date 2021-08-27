*&---------------------------------------------------------------------*
*& Report ZSW_PRINTPROGRAM1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsw_printprogram1.

DATA gv_fmname TYPE rs38l_fnam.
DATA gt_data TYPE ztt_swmoviedata.

START-OF-SELECTION.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM zsw_movie.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'YSF_TEST_SW01'
    IMPORTING
      fm_name            = gv_fmname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE 'SmartForm has not been found' TYPE 'Í'.
  ELSE.
    CALL FUNCTION gv_fmname
      EXPORTING
        it_data          = gt_data
      EXCEPTIONS
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        OTHERS           = 5.
  ENDIF.
