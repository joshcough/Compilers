class CharLit extends Literal{

  Cls type = Cls.CHAR;

  CharLit( String s ){ strValue = s; }
  
  public String toString(){ return strValue; }
  void setValue( String s ){ strValue = s; }
}
