DIR=$(dirname $0)

	$DIR/../../tools/assembler --vs $DIR/asm/vs_mov.txt       --fs $DIR/asm/fs_vs_tests.txt --lnk $DIR/asm/linker_vs_tests.txt --expected 0xFF01FF00 --testonly \
&&	$DIR/../../tools/assembler --vs $DIR/asm/vs_constant.txt  --fs $DIR/asm/fs_vs_tests.txt --lnk $DIR/asm/linker_vs_tests.txt --expected 0x664D331A --testonly \
&&	$DIR/../../tools/assembler --vs $DIR/asm/vs_branching.txt --fs $DIR/asm/fs_vs_tests.txt --lnk $DIR/asm/linker_vs_tests.txt --expected 0xFF01FF00 --testonly \
&&	$DIR/../../tools/assembler --vs $DIR/asm/vs_function.txt  --fs $DIR/asm/fs_vs_tests.txt --lnk $DIR/asm/linker_vs_tests.txt --expected 0xFF01FF00 --testonly \
&&	$DIR/../../tools/assembler --vs $DIR/asm/vs_stack.txt     --fs $DIR/asm/fs_vs_tests.txt --lnk $DIR/asm/linker_vs_tests.txt --expected 0xFF01FF00 --testonly \
&&	$DIR/../../tools/assembler --vs $DIR/asm/vs_predicate.txt --fs $DIR/asm/fs_vs_tests.txt --lnk $DIR/asm/linker_vs_tests.txt --expected 0x667F007F --testonly \
&&	$DIR/../../tools/assembler --vs $DIR/asm/vs_constant_relative_addressing.txt  --fs $DIR/asm/fs_vs_tests.txt --lnk $DIR/asm/linker_vs_tests.txt --expected 0x664D331A --testonly \
&&	$DIR/../../tools/assembler --vs $DIR/asm/vs_attribute_relative_addressing.txt --fs $DIR/asm/fs_vs_tests.txt --lnk $DIR/asm/linker_vs_tests.txt --expected 0xFF01FF00 --testonly \
&&	$DIR/../../tools/assembler --vs $DIR/asm/vs_export_relative_addressing.txt    --fs $DIR/asm/fs_vs_tests.txt --lnk $DIR/asm/linker_vs_tests.txt --expected 0x6601FF1A --testonly \
&&	$DIR/../../tools/assembler --vs $DIR/asm/vs_uniform.txt  --fs $DIR/asm/fs_vs_tests.txt --lnk $DIR/asm/linker_vs_tests.txt --expected 0x664D331A --vs_uniform '["uniform_100"]=(0.1,0.2,0.0,0.0)' --vs_uniform '["uniform_135"]=(0.0,0.0,0.3,0.4)' --testonly \
&&	echo "All tests passed"
