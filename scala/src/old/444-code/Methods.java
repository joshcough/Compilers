/* Generated by Together */
import java.util.Hashtable;
import java.util.Iterator;
public class Methods extends Hashtable{

  public String toString(){
    if( isEmpty() ) return "No Methods";
    String toString="Methods\n";
    for( Iterator it = this.keySet().iterator(); it.hasNext(); ){
      Method m = (Method)this.get(it.next());
      toString+="\t" + m.toString() + "\n";
    }
    return toString;
  }
  
   void typeCheck() throws TypeCheckException {
      for( Iterator it = keySet().iterator(); it.hasNext(); ){
          Method m = (Method)get( it.next() );
          m.typeCheck();
      }
  }

  void genCode(){
    if( isEmpty() ) return;
    for( Iterator it = this.keySet().iterator(); it.hasNext(); ){
      Method m = (Method)this.get(it.next());
      m.genCode();
    }
  }
}
