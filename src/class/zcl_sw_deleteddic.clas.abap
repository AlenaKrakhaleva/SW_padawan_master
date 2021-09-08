class ZCL_SW_DELETEDDIC definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IR_INPUT type ref to ZCL_SW_UPLOADDATA_INPUT .
  methods START_APP2 .
protected section.
private section.

  constants MCO_CONFIRM_ANSWER_J type CHAR1 value 'J' ##NO_TEXT.
  data MV_DEL1_INDICATOR_MOVIE type RS_XFIELD .
  data MV_DEL2_INDICATOR_ORDER type RS_XFIELD .
  data MV_DEL3_INDICATOR_CHAR type RS_XFIELD .

  methods DELETE_MOVIEDATA .
  methods DELETE_MOVIEORDER .
ENDCLASS.



CLASS ZCL_SW_DELETEDDIC IMPLEMENTATION.


  METHOD constructor.
    me->mv_del1_indicator_movie = ir_input->mv_cb2_del_movie.
    me->mv_del2_indicator_order = ir_input->mv_cb3_del_movieorder.
  ENDMETHOD.


  METHOD delete_moviedata.

    DATA lv_answer TYPE CHAR1.

    CALL FUNCTION 'SWO_POPUP_TO_CONFIRM'
      EXPORTING
        text   = 'ARE YOU SURE, DUDE?'
        title  = 'HOLD ON FOR A SEC!'
      IMPORTING
        answer = lv_answer.

    IF lv_answer EQ me->mco_confirm_answer_j.
            DELETE FROM zsw_movie.
    ELSE.
      " deletion is cancelled
    ENDIF.

  ENDMETHOD.


  method DELETE_MOVIEORDER.
    DATA lv_answer TYPE CHAR1.

    CALL FUNCTION 'SWO_POPUP_TO_CONFIRM'
      EXPORTING
        text   = 'ARE YOU SURE, DUDE?'
        title  = 'HOLD ON FOR A SEC!'
      IMPORTING
        answer = lv_answer.

    IF lv_answer EQ me->mco_confirm_answer_j.
            DELETE FROM zsw_movieorder.
    ELSE.
      " deletion is cancelled
    ENDIF.
  endmethod.


  METHOD start_app2.

    IF me->mv_del1_indicator_movie EQ abap_true.
      me->delete_moviedata( ).
    ELSEIF me->mv_del2_indicator_order EQ abap_true.
      me->delete_movieorder( ).
    ELSE.

* TO DO FOR THE 3D TABLE
    ENDIF.

  ENDMETHOD.
ENDCLASS.
