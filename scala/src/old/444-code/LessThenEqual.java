import java.util.Iterator;

public class LessThenEqual extends BinaryExpression{

  LessThenEqual(){ el = new ExpressionList(); }
  
  void add( Expression e ){ el.add( e ); }
  
  public String toString() { 
    String toString = "LessThenEqualStatement:\n";
    for( Iterator it = el.iterator(); it.hasNext(); ){
      toString += ((Expression)it.next()).toString();
      if( it.hasNext() ) toString += " <LessTheEqual> ";
    }
    return toString + "\n";
  }
 
  void typeCheck() throws TypeCheckException {
    el.typeCheck(); 
    type = Cls.BOOL;
    ensureSameTypes( el, "<=" );
    ensureInt( el, "<=" );
  }
 
  void genCode(){
    if( Cls.isPrimitive( cmpType ) ) genCmpCode( "if_icmple" );
    else genCmpCode( "if_acmple" );
  }

}
