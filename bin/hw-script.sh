# make temp dir to do work in
rm -rf build
mkdir build
cd build

# clone the project
git clone git://github.com/joshcough/Interpreters.git
cd Interpreters

#build the code
./bin/sbt update
./bin/sbt test
./bin/sbt package

# move a few things into place
cp target/scala_2.8.1/compilers_2.8.1-0.1.jar ./lib/compiler.jar
cp project/boot/scala-2.8.1/lib/scala-library.jar ./lib
mv bin/L1 bin/spill bin/liveness .

# get rid of stuff i dont need to hand in
rm -rf .git project target out lib_managed test 444-code src/main/scheme Test.class Test.j compilers.iml runtime.o test.S test2.S a.out bin Test.o Test.S

# create the tar to hand in
tar cfvz ../compilers.tar.gz *

# done creating compilers.tar.gz
echo "done creating compilers.tar.gz. now run:"
echo "scp ./build/compilers.tar.gz jdc974@tlab-18.cs.northwestern.edu:./"

tar cfvz ../spill-test.tar.gz spill-test
echo "done creating ./build/spill-test.tar.gz."
