import java.util.Iterator;

public class Or extends BinaryExpression{

  Or(){ el = new ExpressionList(); }

  void add( Expression e ){ el.add( e ); }
  
  public String toString() { 
    String toString = "OrStatement:\n";
    for( Iterator it = el.iterator(); it.hasNext(); ){
      toString += ((Expression)it.next()).toString();
      if( it.hasNext() ) toString += " <or> ";
    }
    return toString + "\n";
  }

  void typeCheck() throws TypeCheckException {
    el.typeCheck(); 
    type = Cls.BOOL;
    ensureSameTypes( el, "|" );
    ensureBool( el, "|" );
  }
 
  void genCode(){
    genLoads();
    genOperators( "ior" );
  }

 }
