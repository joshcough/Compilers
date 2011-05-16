import java.util.Iterator;

public class GreaterThen extends BinaryExpression{
  
  GreaterThen(){ el = new ExpressionList(); }
  
  void add( Expression e ){ el.add( e ); }

  public String toString() { 
    String toString = "GreaterThenStatement:\n";
    for( Iterator it = el.iterator(); it.hasNext(); ){
      toString += ((Expression)it.next()).toString();
      if( it.hasNext() ) toString += " <greaterthen> ";
    }
    return toString + "\n";
  }
 
  void typeCheck() throws TypeCheckException {
    el.typeCheck(); 
    type = Cls.BOOL;
    ensureSameTypes( el, ">" );
    ensureInt( el, ">" );
  }

  void genCode(){
    if( Cls.isPrimitive( cmpType ) ) genCmpCode( "if_icmpgt" );
    else genCmpCode( "if_acmpgt" );
  }

}

