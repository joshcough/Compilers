/*
 * Author: John Witkowski
 * Date : 02/26/2001
 * 
 * Description: A ErrorHandle Object will do most of the repetitive work found 
 *              within the catch statements while handling errors.
 */

public class ErrorHandle {
  static String parseExceptions[] = new String[100];
  static int errorCount = 0; // Total errors found during a parse
  
  public ErrorHandle(ParseException e, int DELIM) {
    CompilerSystem.addTypeCheckError();
    try {
      parseExceptions[errorCount++] = e.toString();
    }
    catch(ArrayIndexOutOfBoundsException ex) {
      System.out.println("Too many errors found to continue");
      System.exit(1);
    }
 
    Token t;
    do {
      t = MODLParser.getNextToken();
      if(t.kind == 0){ //This is an <EOF>
        ErrorHandle.printExceptions();    
        System.out.println("Encountered "+errorCount+" error(s) during parse");
        System.out.println("Model Parser Version 1.0.1: file parsed successfully.");
        System.exit(1);
      }
    } while (t.kind != DELIM);
   
    
    // Usually this is where a recover is made
    // however since different for most cases
    // it is needed to put this into catch block
    // in .jj file
  }
  
  static public void printExceptions() {
    for(int i = 0; i < errorCount; i++)
      System.out.println(parseExceptions[i]);
  }    
  
  // A helper method for debugging error handling issues
  static public void printLocation(String method) {
    System.out.println("Error caught in "+method);
  }
}
