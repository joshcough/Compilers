/**
 *   File Name: CompileException.java
 *   Author: Joshua D. Cough
 *   Description: 
 */

/**
 *   Revision History
 *   -----------------------------------------------------
 *   Date               Author            Changes
 *   -----------------------------------------------------
 *   Fri Mar  9, 2001	Joshua D. Cough	  Created
 */


/** package **/


/** imports **/


/**
 *
 */
class CompileException extends Exception{

  Exception e;
  String s;
  /**
   *
   */
  static public void main(String args[]){
    CompileException ce = 
      new CompileException
        ( new Exception("ParseException: Encountered \"end\" at line 18, column 1." ), "Google");
    System.out.println( ce.toString() );
                                              
  }

  /**
   *
   */
  CompileException(Exception ex, String s){
    this.e=ex;
    this.s = e.toString().substring
      ( e.toString().indexOf(":")+2, e.toString().indexOf( "." )+1 );
    this.s = s + "\n" + this.s;
  }

  public String toString(){ return s; }

}
