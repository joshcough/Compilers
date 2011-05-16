public class TerminalExpression extends Expression{
  
  String stringValue;
  int intValue;

  public TerminalExpression(String val) { stringValue = val; }

  public TerminalExpression(int val) { intValue = val; }

  public TerminalExpression(){}

  public String toString() { 
    if(stringValue != null)
      return stringValue;
    else
      return String.valueOf(intValue);
  }

  void typeCheck() throws TypeCheckException {
    if( stringValue == null ) type=Cls.INT;     
    else if( stringValue.equals("true") ) { type= Cls.BOOL;}
    else if( stringValue.equals("false") ) { type=Cls.BOOL;}
    else if( stringValue.equals("null") ) { type=new Cls("null");}
    else type=Cls.CHAR;
  }

  void genCode(){
    if( stringValue == null ){
      if( intValue < 256 ) System.out.println( "bipush " + intValue );
      else System.out.println("sipush " + intValue );
    }
    else if( stringValue.equals( "true" ) )
      System.out.println( "bipush 1" );
    else if( stringValue.equals( "false" ) )
      System.out.println( "bipush 0" );
    else if ( stringValue.equals( "null" ) )
      System.out.println( "aconst_null" );
    else{ 
      System.out.print( "char? ");
      System.out.println( toString() );
    }
  }

  void genLoad(){
    genCode();
  }
  
}
