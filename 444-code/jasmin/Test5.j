

.class public foo
.super java/lang/Object

.field public i I
.field public static i2 I

; constructor
.method public <init>()V
aload_0
invokenonvirtual java/lang/Object/<init>()V
bipush 6
aload_0
putfield foo/i I
return
.end method

.method public foo2(I)V
.limit stack 99
.limit locals 99
getstatic java/lang/System/out Ljava/io/PrintStream;
aload_0
getfield foo/i I
invokestatic java/lang/String/valueOf(I)Ljava/lang/String;
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
return
.end method

; class init
.method public <clinit>()V
.limit stack 99
.limit locals 99
bipush 6
putstatic foo/i2 I
return
.end method

