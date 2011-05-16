

.class public Foo
.super java/lang/Object

.field public static i I
.field public static i2 I

; constructor
.method public <init>()V
;.limit stack 99
;.limit locals 99
aload_0
invokenonvirtual java/lang/Object/<init>()V
return
.end method

.method public static main([Ljava/lang/String;)V
.limit stack 99
.limit locals 99
return
bipush 10
newarray int
astore 2
bipush 4
;----array store--------
bipush 0
aload 2
iastore
;----array store end----
bipush 7
;----array store--------
bipush 9
aload 2
iastore
;----array store end----
getstatic java/lang/System/out Ljava/io/PrintStream;
;----array load---------
aload 2
sipush 1000
iaload
;----array load end----
invokestatic java/lang/String/valueOf(I)Ljava/lang/String;
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
.end method

; class init
.method public <clinit>()V
.limit stack 99
.limit locals 99
bipush 6
putstatic Foo/i I
bipush 7
putstatic Foo/i2 I
return
.end method

