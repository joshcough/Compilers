import java.util.ArrayList;
import java.util.Iterator;

public class ElseIfStats extends ArrayList{

  public String toString(){
    String toString = "";
    for( Iterator it = this.iterator(); it.hasNext(); ){
      ElseIfStat e = (ElseIfStat)it.next();
      toString += e.toString() + "\n";
    }
    return toString;
  }
  
  
 void typeCheck() throws TypeCheckException{
   for( Iterator it = this.iterator(); it.hasNext(); ){
     ElseIfStat e = (ElseIfStat)it.next();
     e.typeCheck();
    }
  }
 
  void genCode( String label2 ){
    for( Iterator it = this.iterator(); it.hasNext(); ){
     ElseIfStat e = (ElseIfStat)it.next();
     e.genCode( label2 );
    }
  }

}
