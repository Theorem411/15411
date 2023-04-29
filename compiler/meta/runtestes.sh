# print system info:
echo "GIT COMMIT HASH:"
git log -1 --pretty=format:%h 

# make the grader
cd /home/useitkal/dist/harness
make clean
make 

# make compiler
cd /home/useitkal/dist/compiler
git pull
make clean
make

# cleanup
rm l6results.txt 2> /dev/null
touch l6results.txt

# run tests
../harness/runHarness "gradeCompiler" -e llvm  ../tests/l1-basic/ -qqq -j 16 >> l6results.txt 2>&1
../harness/runHarness "gradeCompiler" -e llvm  ../tests/l2-basic/ -qqq -j 16 >> l6results.txt 2>&1
../harness/runHarness "gradeCompiler" -e llvm  ../tests/l3-basic/ -qqq -j 16 >> l6results.txt 2>&1
../harness/runHarness "gradeCompiler" -e llvm  ../tests/l4-basic/ -qqq -j 16 >> l6results.txt 2>&1
../harness/runHarness "gradeCompiler" -e llvm  ../tests/l1-large/ -qqq -j 16 >> l6results.txt 2>&1
../harness/runHarness "gradeCompiler" -e llvm  ../tests/l2-large/ -qqq -j 16 >> l6results.txt 2>&1
../harness/runHarness "gradeCompiler" -e llvm  ../tests/l3-large/ -qqq -j 16 >> l6results.txt 2>&1
../harness/runHarness "gradeCompiler" -e llvm  ../tests/l4-large/ -qqq -j 16 >> l6results.txt 2>&1