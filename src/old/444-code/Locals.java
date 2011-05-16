import java.util.Hashtable;
import java.util.Iterator;

class Locals extends Hashtable{

  public String toString(){
    if( this==null ) return "No Fields";
    String toString="\t\tLocals:\n";
    for( Iterator it = this.keySet().iterator(); it.hasNext(); ){
      Local l = (Local)this.get(it.next());
      toString+= "\t\t\t" + l.toString() + "\n";
    }
    return toString;
  }

  void add( Local l ){ put( l.getName(),l ); }
  
  void typeCheck() throws TypeCheckException {
    for( Iterator it = keySet().iterator(); it.hasNext(); ){
      Local l = (Local)get( it.next() );
      l.typeCheck();
    }
  }
}
