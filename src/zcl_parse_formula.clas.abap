CLASS zcl_parse_formula DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_parse_formula .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zif_parse_formula .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS gc_sum TYPE char4 VALUE 'SUM(' ##NO_TEXT.
    CONSTANTS gc_separator TYPE c VALUE ',' ##NO_TEXT.
    CONSTANTS gc_asterisk TYPE c VALUE '*' ##NO_TEXT.
    CONSTANTS gc_open_brace TYPE c VALUE '(' ##NO_TEXT.
    CONSTANTS gc_close_brace TYPE c VALUE ')' ##NO_TEXT.
    CONSTANTS gc_zero TYPE c VALUE '0' ##NO_TEXT.
    CONSTANTS gc_plus TYPE c VALUE '+' ##NO_TEXT.
    CONSTANTS gc_minus TYPE c VALUE '-' ##NO_TEXT.
    CONSTANTS gc_mult TYPE c VALUE '*' ##NO_TEXT.
    CONSTANTS gc_div TYPE c VALUE '/' ##NO_TEXT.
    CLASS-DATA so_instance TYPE REF TO zif_parse_formula.

    METHODS get_operand
      IMPORTING
        !iv_formula      TYPE zparser_e_formula
        !io_consumer     TYPE REF TO zif_parse_formula_consumer
      RETURNING
        VALUE(rv_result) TYPE wrbtr
      RAISING
        zcx_parse_formula .
ENDCLASS.



CLASS ZCL_PARSE_FORMULA IMPLEMENTATION.


  METHOD get_instance.
    IF so_instance IS NOT BOUND.
      so_instance = NEW zcl_parse_formula( ).
    ENDIF.

    ro_instance = so_instance.
  ENDMETHOD.


  METHOD get_operand.
    DATA(lv_formula) = iv_formula.
    IF iv_formula(1) = gc_minus.
      lv_formula = iv_formula+1.
    ENDIF.

    rv_result = io_consumer->get_operand( lv_formula ).
    IF iv_formula(1) = gc_minus.
      rv_result = 0 - rv_result.
    ENDIF.
  ENDMETHOD.


  METHOD zif_parse_formula~calculate.
    IF iv_formula IS INITIAL.
      RETURN.
    ENDIF.

    DATA: lv_beg         TYPE i VALUE 0,
          lv_end         TYPE i VALUE 0,
          lv_len         TYPE i,
          lv_operand_len TYPE i,
          lv_formula     TYPE zparser_e_formula.
    lv_len = strlen( iv_formula ).

    WHILE lv_end < lv_len.
      IF iv_formula+lv_end(1) CA '+-'.
        IF lv_end <> lv_beg.
          lv_operand_len = lv_end - lv_beg.
          lv_formula = iv_formula+lv_beg(lv_operand_len).
          rv_result = rv_result + zif_parse_formula~calculate( iv_formula = lv_formula io_consumer = io_consumer ).
        ENDIF.
        lv_beg = lv_end.
      ENDIF.
      lv_end = lv_end + 1.
    ENDWHILE.

    IF lv_beg <> lv_end.
      lv_operand_len = lv_end - lv_beg.
      lv_formula = iv_formula+lv_beg(lv_operand_len).
      rv_result = rv_result + get_operand( iv_formula  = lv_formula
                                           io_consumer = io_consumer ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
