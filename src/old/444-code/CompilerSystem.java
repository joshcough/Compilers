/* Generated by Together */
import java.util.Hashtable;
import java.util.Iterator;

public class CompilerSystem extends Hashtable{

  static int typeCheckErrors=0;

  public CompilerSystem(){
    addClass( Cls.INT ); 
    addClass( Cls.BOOL );
    addClass( Cls.CHAR );
    addClass( Cls.VOID );
    addClass( Cls.ANY );
  }
  
  void addClass( Cls cls ){ put( cls.getName(),cls); }

  public String toString(){
    if( isEmpty() ) return "No Classes";
    String toString="";
    System.out.println("\nSystem of " + (this.keySet().size()-5) + " Class(es)\n");
    for( Iterator it = this.keySet().iterator(); it.hasNext(); ){
      Cls c = (Cls)this.get(it.next());
      toString+=c.toString() + "\n\n";
    }
    return toString;
  }
    
  Cls resolveClass( String name ){ 
    return (Cls)get( name );
  }

  static void addTypeCheckError(){ typeCheckErrors++; }
  static int getTypeCheckErrors(){ return typeCheckErrors; }

  void typeCheck() throws TypeCheckException{
     for( Iterator it = keySet().iterator(); it.hasNext(); ){
          Cls c = (Cls)get( it.next() );
          c.typeCheck();
      } 
  }
 
  void genCode(){ 
    for( Iterator it = this.keySet().iterator(); it.hasNext(); ){
      Cls c = (Cls)this.get(it.next());
      c.genCode();
    }
  }
  

}