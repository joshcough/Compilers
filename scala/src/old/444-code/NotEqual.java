import java.util.Iterator;

public class NotEqual extends BinaryExpression{

  NotEqual(){ el = new ExpressionList(); }
  
  void add( Expression e ){ el.add( e ); }

  public String toString() { 
    String toString = "NotEqualStatement:\n";
    for( Iterator it = el.iterator(); it.hasNext(); ){
      toString += ((Expression)it.next()).toString();
      if( it.hasNext() ) toString += " <notequal> ";
    }
    return toString + "\n";
  }

  void typeCheck() throws TypeCheckException {
    el.typeCheck(); 
    type = Cls.BOOL;
    ensureSameTypes( el, "!=" );
  }

  void genCode(){
    if( Cls.isPrimitive( cmpType ) ) genCmpCode( "if_icmpne" );
    else genCmpCode( "if_acmpne" );
  }

}
