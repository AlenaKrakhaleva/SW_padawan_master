*&---------------------------------------------------------------------*
*& Report ZSW_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsw_upload_data.

INCLUDE zsw_data_selection.

DATA go_input TYPE REF TO zcl_sw_uploaddata_input.
DATA go_app TYPE REF TO zcl_fetch_moviedata.
DATA go_app_delete Type ref to zcl_sw_deleteddic.

Start-OF-SELECTION.

go_input = NEW zcl_sw_uploaddata_input(
  i_radiobut1 = r_but1
  i_radiobut2 = r_but2
  i_radiobut3 = r_but3
  i_radiobut4 = r_but4
  i_radiobut5 = r_but5
  i_checkbox1 = pa_alv
  i_checkbox2 = pa_del1
).


go_app_delete = NEW zcl_sw_deleteddic( ir_input = go_input ).
go_app = NEW zcl_fetch_moviedata( ir_input = go_input ).

go_app_delete->start_app2( ).
go_app->start_app( ).
