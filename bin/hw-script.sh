cd ..
rm -rf compilers-hw1
cp -r Interpreters compilers-hw1
cd compilers-hw1
sbt package
cp target/scala_2.8.1/compilers_2.8.1-0.1.jar ./lib/compiler.jar
cp project/boot/scala-2.8.1/lib/scala-library.jar ./lib
rm -rf .git project target out lib_managed 444-code src/main/scheme Test.class Test.j compilers.iml runtime.o test.S	test2.S
# just for now:
rm liveness spill spill-tests.txt  spilltests.txt 
mv bin/L1 .
tar cfvz compilers-hw1.tar.gz *
mv compilers-hw1.tar.gz ..
echo "done. now run:"
echo "scp ./compilers-hw1.tar.gz jdc974@some.machine.edu:./"
