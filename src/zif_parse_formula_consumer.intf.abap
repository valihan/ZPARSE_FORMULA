interface ZIF_PARSE_FORMULA_CONSUMER
  public .


  methods GET_OPERAND
    importing
      !IV_FORMULA type ZPARSER_E_FORMULA
    returning
      value(RV_RESULT) type WRBTR
    raising
      ZCX_PARSE_FORMULA .
endinterface.
