import java.util.Iterator;

public class LessThen extends BinaryExpression{

  LessThen(){ el = new ExpressionList(); }
  
  void add( Expression e ){ el.add( e ); }

  public String toString() { 
    String toString = "LessThenStatement:\n";
    for( Iterator it = el.iterator(); it.hasNext(); ){
      toString += ((Expression)it.next()).toString();
      if( it.hasNext() ) toString += " <lessthen> ";
    }
    return toString + "\n";
  }

  void typeCheck() throws TypeCheckException {
    el.typeCheck(); 
    type = Cls.BOOL;
    ensureSameTypes( el, "<" );
    ensureInt( el, "<" );
    cmpType = Cls.INT;
  }

  void genCode(){
    if( Cls.isPrimitive( cmpType ) ) genCmpCode( "if_icmplt" );
    else genCmpCode( "if_acmplt" );
  }

  void genLoad(){
    genCode();
  }
    
}
