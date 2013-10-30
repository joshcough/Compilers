public class Literal extends TerminalExpression {

  int intValue;
  String strValue;

  boolean isString() {
    if(strValue != null) 
      return true;
    else
      return false;
  }

  String getString() { return strValue; }
  int getInt() { return intValue; }
  
}

