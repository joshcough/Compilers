cd ..
rm -rf Compilers
cp -r Interpreters Compilers
cd Compilers
mkdir lib
cp target/scala_2.8.1/compilers_2.8.1-0.1.jar ./lib/compiler.jar
cp project/boot/scala-2.8.1/lib/scala-library.jar ./lib
rm -rf .git project target out lib_managed 444-code src/main/scheme Test.class Test.j compilers.iml runtime.o test.S	test2.S
# just for now:
rm liveness spill spill-tests.txt  spilltests.txt 
tar cfvz Compilers.tar.gz *
mv Compilers.tar.gz ..
echo "done. now run:"
echo "scp ./Compilers.tar.gz joshcough@partsims.ccl.northwestern.edu:./"
