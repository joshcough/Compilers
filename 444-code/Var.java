
interface Var {

  String getName();
  Cls getType();
  void setName(String s);
  void setType(String s);
  void setType(Cls c);
  void setExpression(Expression e);
  Expression getExpression();
  void addQualifier(String q);
  boolean isFinal();
  void setPointer( int i );
  int getPointer();
  void genStore();
  void genLoad();

}
