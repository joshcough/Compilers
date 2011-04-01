import java.util.Iterator;
public class VarDecStat extends Statement {

  Var var;
  Expression exp;
  Cls type;
  
  VarDecStat( Var v, Expression e ){ 
    var=v; 
    exp=e; 
    if(v instanceof Local) {
      Local cl = (Local)v;
      if( cl.getScope().getLocals().get( cl.getName() ) != null ){
        System.out.println("Multiple declaration of "+cl.getName() + "\n");
      }
    }
    else if(v instanceof Field){
      Field cf = (Field)v;
      if( cf.getScope().getFields().get( cf.getName() ) != null ){
        System.out.println("Multiple declaration of "+cf.getName() + "\n");
      }
    }
  }
  void setVar( Var v ){ var=v; }
  void setExp( Expression e ){ exp=e; }
  Var getVar(){ return var; }
  Expression getExp(){ return exp; }
  
  public String toString(){
    String toString = "VarDec " + 
      ((Local)var).toString();
    if( exp!=null ) toString+= " = " + exp.toString();
        return toString + "\n";
  }
  
  void typeCheck() throws TypeCheckException{
    if( exp == null ) return;
    type = var.getType();
    exp.typeCheck();
    ensureNotVoid( var.getType() );
    ensureNotPrimitiveNull( var, exp );
    ensureCompatibleTypes( var, exp );
  }

  void ensureNotVoid( Cls c )throws TypeCheckException{
    if( c == Cls.VOID ){
      System.out.println( "Cannot declare a variable of type: void" );
      throw new TypeCheckException();
    }
  }

  void genCode(){
    exp.genCode();
    var.genStore();
  }
}

