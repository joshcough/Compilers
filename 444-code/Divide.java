/* Generated by Together */
import java.util.Iterator;

public class Divide extends BinaryExpression{

  Divide(){ el = new ExpressionList(); }
  
  void add( Expression e ){ el.add( e ); }
  
  public String toString() { 
    String toString = "DivideStatement:\n";
    for( Iterator it = el.iterator(); it.hasNext(); ){
      toString += ((Expression)it.next()).toString();
      if( it.hasNext() ) toString += " <divide> ";
    }
    return toString + "\n";
  }

  void typeCheck() throws TypeCheckException {
    el.typeCheck(); 
    type = Cls.INT;
    ensureSameTypes( el, "/" );
    ensureInt( el, "/" );
  }

  void genCode(){
    genLoads();
    genOperators( "idiv" );
  }
}
