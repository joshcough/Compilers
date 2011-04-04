# make temp dir to do work in
rm -rf tmp
mkdir tmp
cd tmp

# clone the project
git clone git@github.com:joshcough/Interpreters.git
cd Interpreters

#build the code
sbt package
#sbt test

# move a few things into place
cp target/scala_2.8.1/compilers_2.8.1-0.1.jar ./lib/compiler.jar
cp project/boot/scala-2.8.1/lib/scala-library.jar ./lib
mv bin/L1 .

# get rid of stuff i dont need to hand in
rm -rf .git project target out lib_managed test 444-code src/main/scheme Test.class Test.j compilers.iml runtime.o test.S test2.S a.out bin Test.o Test.S

# create the tar to hand in
tar cfvz ../compilers-hw1.tar.gz *

# done
echo "done. now run:"
echo "scp ./tmp/compilers-hw1.tar.gz jdc974@tlab-18.cs.northwestern.edu:./spring2011/"
