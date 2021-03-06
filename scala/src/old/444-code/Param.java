/* Generated by Together */

public class Param implements Var{
  String name=null;
  Cls type=null;
  Method scope=null;
  int pointer;

  Param(){}
  Param(String name){ setName( name ); }
 
  public String toString(){
    String toString="Param:";
    toString += " name " + name;
    toString += ", type " + type.getName();
    toString += ", scope " + scope.getName();
    toString += ", stack pointer " + pointer;
    return toString;
  }

  public String getName(){ return name; }
  public Cls getType(){ return type; }
  public void setName(String s){ name=s; }
  public void setType(String s){setType( new Cls(s) );}
  public void setType(Cls c){ type = c; }
  public void setScope( Method m ){ scope = m; }
  public Method getScope(){ return scope; } 
  public void setExpression( Expression e ){}
  public Expression getExpression(){ return null;}
  public void addQualifier(String q){}
  public boolean isFinal(){ return false; }
  boolean isStatic(){ return false; }  
  public void setPointer( int i ){ pointer = i; }
  public int getPointer(){ return pointer; }   
  
  void genCode() {
    String s = getType().genSignature();
    System.out.print( s );
    if( ! s.equals( "I" ) &&  ! s.equals( "C" ) &&  ! s.equals( "Z" ) )
      System.out.print(";");
  }

  public void genLoad(){
    System.out.println( "aload " + getPointer() );
  }

  public void genStore(){
    System.out.println( "astore " + getPointer() );
  }
}
