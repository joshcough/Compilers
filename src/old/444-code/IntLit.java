class IntLit extends Literal{

  Var var;
  Cls type;

  IntLit( String s ){ intValue = Integer.parseInt( s ); }

  public String toString(){ return String.valueOf( intValue ); }
  void setValue( String s ){ intValue = Integer.parseInt( s ); }

  void typeCheck() throws TypeCheckException {
  }

}
