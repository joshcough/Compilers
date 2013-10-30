import java.util.Iterator;

public class Equals extends BinaryExpression{

  Equals(){ el = new ExpressionList(); }
 
  void add( Expression e ){ el.add( e ); }
  
  public String toString(){ 
    String toString = "EqualsStatement:\n";
    for( Iterator it = el.iterator(); it.hasNext(); ){
      toString += ((Expression)it.next()).toString();
      if( it.hasNext() ) toString += " <equals> ";
    }
    return toString + "\n";
  }

  void typeCheck() throws TypeCheckException {
    el.typeCheck(); 
    type = Cls.BOOL;
    Cls c = ensureSameTypes( el, "==" );
    cmpType = c;
  }


  void genCode(){
    if( Cls.isPrimitive( cmpType ) ) genCmpCode( "if_icmpeq" );
    else genCmpCode( "if_acmpeq" );
  }

  
}
