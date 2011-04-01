import java.util.Iterator;

public class And extends BinaryExpression{

  And(){ el = new ExpressionList(); }

  void add( Expression e ){ el.add( e ); }
  
  public String toString() { 
    String toString = "AndStatement:\n";
    for( Iterator it = el.iterator(); it.hasNext(); ){
      toString += ((Expression)it.next()).toString();
      if( it.hasNext() ) toString += " <and> ";
    }
    return toString + "\n";
  }
  
  void typeCheck() throws TypeCheckException {
    el.typeCheck(); 
    type = Cls.BOOL;
    ensureSameTypes( el, "&" );
    ensureBool( el, "&" );
  }

   void genCode(){
    genLoads();
    genOperators( "iand" );
  }
   
 }
