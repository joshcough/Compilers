/* Generated by Together */
import java.util.Hashtable;
import java.util.Iterator;
public class Fields extends Hashtable{   

  public String toString(){
    if( isEmpty() ) return "No Fields";
    String toString="Fields\n";
    for( Iterator it = this.keySet().iterator(); it.hasNext(); ){
      Field f = (Field)this.get(it.next());
      toString+= "\t" + f.toString() + "\n";
    }
    return toString;
  }

  void add( Field f ){ put( f.getName(),f ); }
  
  void typeCheck() throws TypeCheckException {
      for( Iterator it = keySet().iterator(); it.hasNext(); ){
          Field f = (Field)get( it.next() );
          f.typeCheck();
      }
  }

    void genCode(){
        if( isEmpty() ) return;
        for( Iterator it = this.keySet().iterator(); it.hasNext(); ){
            Field f = (Field)this.get(it.next());
            f.genCode();
        }
    }


}
