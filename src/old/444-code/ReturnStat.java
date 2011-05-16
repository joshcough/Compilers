/* Generated by Together */

public class ReturnStat extends Statement{

  Expression exp;
  Method scope;
  void setExp( Expression e ){ exp=e; }
  Expression getExp(){ return exp; }
  
  void setScope( Method m ){ scope = m; }
  Method getScope(){ return scope; }

  void typeCheck() throws TypeCheckException {
    if( exp == null ) {
      if( scope.getType() != Cls.VOID ){
        System.out.println( "Method " + scope.getName() + 
                            " requires a return type of " +
                            scope.getType().getName() );
        throw new TypeCheckException();
      }
    }
    else{
      exp.typeCheck();
      if( scope.getType() == Cls.VOID ){
        System.out.println( "cannot return a value from method whose result type is void" );
        throw new TypeCheckException();
      }
      ensureCompatibleTypes( exp.getType(), scope.getType() );
    }
  }

  void genCode(){
    if( exp != null ) exp.genLoad();
    if( exp == null ) System.out.println("return");
    else if( ! Cls.isPrimitive( exp.getType() ) ) System.out.println( "areturn" );
    else System.out.println( "ireturn" );
  }


}
