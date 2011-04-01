.class public NewArray
.super java/lang/Object

.method public <init>()V
   .limit stack 99
.limit locals 99
   aload_0
   invokenonvirtual java/lang/Object/<init>()V
   return
.end method

.method public static main([Ljava/lang/String;)V
   .limit stack 4
   .limit locals 2

   iconst_2
   newarray boolean
   astore_1

   aload_1
   iconst_0
   iconst_1
   bastore

   return
.end method