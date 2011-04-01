import java.util.Iterator;

public class GreaterThenEqual extends BinaryExpression{

  GreaterThenEqual(){ el = new ExpressionList(); }
  
  void add( Expression e ){ el.add( e ); }

  public String toString() { 
    String toString = "GreaterThenEqualStatement:\n";
    for( Iterator it = el.iterator(); it.hasNext(); ){
      toString += ((Expression)it.next()).toString();
      if( it.hasNext() ) toString += " <GreaterThenEqual> ";
    }
    return toString + "\n";
  }

  void typeCheck() throws TypeCheckException {
    el.typeCheck(); 
    type = Cls.BOOL;
    ensureSameTypes( el, ">=" );
    ensureInt( el, ">=" );
  }
 
  void genCode(){
    if( Cls.isPrimitive( cmpType ) ) genCmpCode( "if_icmpge" );
    else genCmpCode( "if_acmpge" );
  }
 
}

 
