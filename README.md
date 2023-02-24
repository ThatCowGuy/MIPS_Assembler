
===================================================================================
REGISTERS
===================================================================================
You can call all 32 general purpose registers by their INDEX (0 prefixed or not),
or by their 2-Letter Name (AT == 01, T0 == 08, RA == 31 etc). Additionally allowed:

	ZERO	==	R0		==  00
	STACK	==	SP		==  29
	R		==  RA		==  31

For COP1 / FP Registers, you need to prefix them with 'F' (or 'f'). The numeral
can be 0 prefixed or not, so F02 == F2 for example. You probably want to avoid
using odd FP-Regs, because some N64 Games don't do well with those on default.

You can also prefix the registers with '$'. These prefixes wont affect parsing.
===================================================================================





===================================================================================
INJECTION POINTS
===================================================================================
Use .ORG, .ROMORG, .ROM_ORG, .FILEORG or .FILE_ORG in combination with a NUMERAL to
determine where in the editted file the following assembly code will be injected to.
The virtual PC will also move to the new Injection Point.

Use .RAMORG, .RAM_ORG, .RAMSTART or .RAM_START in combination with a NUMERAL to
determine where the file will exist in RAM during runtime. For JUMP instructions,
the set RAM_START value will be added to the targetted location, to turn it into
an absolute target.
===================================================================================
LIMITER // TODO, unimplemented but kinda cool
===================================================================================
Use .LIMIT, .STOP or .STOP_HERE in combination with a NUMERAL to define a point
after which no further code injection may happen. This is meant to avoid overwriting
parts of the editted File that you dont want to overwrite (always make backups).
When the current PC reaches the point defined through a LIMITER, an error message
will pop up and the assembly will stop.

Use .SIZE or .MAX_SIZE in combination with a NUMERAL to determine how far the current
PC is allowed to proceed from the last INJECTION POINT. When the difference between
the current PC and the last INJECTION POINT reaches the determined SIZE, an error
message will pop up and the assembly will stop.
===================================================================================





===================================================================================
CONSTANTS
===================================================================================
CONSTANTS can be used to access a preset value all over the .asm file. They can be
defined anywhere within the file (because we do a pre-parsing run for this reason).
Whenever you want to use a constant, prefix it with '@':

	DEFINITION:
		[CONST_NAME]: VALUE
	USAGE:
		LUI T0, @CONST_NAME			==> will use the upper 16 bits of VALUE
		ADDIU T0, T0, @CONST_NAME	==> will use the lower 16 bits of VALUE

You can also use a CONSTANT as a BRANCH of JUMP target, or as an address offset:

	USAGE:
		JAL @CONSTANT			==> constant has to represent the absolute target !
		LW T0, @CONSTANT(T0)	==> loads WORD from *(T0 + lower16(VALUE)) into T0
===================================================================================





===================================================================================
LABELS
===================================================================================
LABELS are basically identical to CONSTANTS here, except their VALUE is set to
the current PC of where they were defined. They are intended to be used as GOTOs
in BRANCH targets, and don't need special prefixing when used as such: 

	DEFINITION:
		LABEL_NAME:				==> LABEL value (LABEL_PC) is set to current PC
	USAGE:
		BEQ R0, R0, LABEL_NAME	==> BRANCH to LABEL_PC

However, LABELs can also be used as JUMP targets (if RAM_START is defined), or even
as plain CONSTANTS. Whenever you use a LABEL in this context, prefix it with ':':

	DEFINITION:
		// ENTRY FUNCTION_A		==> Comment
		LABEL_NAME:				==> LABEL value (LABEL_PC) is set to current PC
	USAGE:
		JAL :LABEL_NAME			==> JUMP to (LABEL_PC + RAM_START)
		NOP
	USAGE:
		LI T0, :LABEL_NAME		==> Load LABEL_PC into T0 (like a Function-Pointer)
===================================================================================





===================================================================================
ALIGNMENT
===================================================================================
All instructions will automatically get WORD aligned. The File is padded with 0x00.

All DATA inserts will automatically get aligned to their own SIZE. STRING DATA will
get aligned as if it was BYTE DATA. The File is also padded with 0x00.
===================================================================================





===================================================================================
DATA
===================================================================================
Use .BYTE, .HALF, .HALFWORD or .WORD to insert one or more DATA pieces of the
specified size to the current PC.

Use .STRING "CONTENT" to put a string literal (0-terminated) at the current PC.
===================================================================================





===================================================================================
PSEUDOS
===================================================================================
LI == LOAD IMMEDIATE, and LA == LOAD ADDRESS will load a FULL WORD into the dst-REG.
Both instructions are virtually the same because of how CONSTANTs and LABELs work.
Note that both LI & LA are unrolled into 2 INSTRUCTIONS, and therefore move PC by 8.

	USAGE:
		LI dst, imm
	TRANSLATION:
		LUI dst, imm
		ADDIU dst, dst, imm

B == BRANCH is an unconditional BRANCH to the specified target.

	USAGE:
		B label
	TRANSLATION:
		BEQ R0, R0, label

MOVE will move the WORD in the src-REG to the dst-REG.

	USAGE:
		MOVE dst, src
	TRANSLATION:
		ADDU dst, R0, src

SUBI == SUBTRACT IMMEDIATE is not a real instruction (surprise!). However, it is easily
substituted by translating the PSEUDO into an ADDI with negative immediate:

	USAGE:
		SUBI dst, src, imm
	TRANSLATION:
		ADDI dst, src, 2complement(imm)

Next we have 4 conditional BRANCH PSEUDOS: 
BLT == BRANCH (if) LESS THAN		==> if (A < B) GOTO target;
BGT == BRANCH (if) GREATER THAN		==> if (A > B) GOTO target;
BLE == BRANCH (if) LESS OR EQUAL	==> if (A <= B) GOTO target;
BGE == BRANCH (if) GREATER OR EQUAL ==> if (A >= B) GOTO target;
Note that all of these are unrolled into 2 INSTRUCTIONS, and therefore move PC by 8.

	GENERAL_USAGE:
		PSEUDO_B src1, src2, label

	USAGE:
		BLT src1, src2, label
	TRANSLATION:
		SLT AT, src1, src2
		BNE AT, R0, label
		
	USAGE:
		BGT src1, src2, label
	TRANSLATION:
		SLT AT, src2, src1
		BNE AT, R0, label
		
	USAGE:
		BLE src1, src2, label
	TRANSLATION:
		SLT AT, src2, src1
		BEQ AT, R0, label
		
	USAGE:
		BGE src1, src2, label
	TRANSLATION:
		SLT AT, src1, src2
		BEQ AT, R0, label
===================================================================================





===================================================================================
CAPITALIZATION
===================================================================================
You can use whatever capitalization you want. Everything will be transformed into
uppercase to make parsing easier. This means r0 == R0, addiu == ADDIU, etc., but it
also counts for CONSTANTS and LABELS, So @MyConstant == @MYCONSTANT. Remember this
to avoid duplicates (though you will recieve an error message in that case anyways).
===================================================================================


===================================================================================
COMMENTS
===================================================================================
If you want to comment your .asm File, you can start a comment by prefixing it with
"//", "#" or ";". Everything behind those symbols is completely ignored in parsing.
===================================================================================


===================================================================================
INDENTATION + WHITESPACES
===================================================================================
All indentations, as well as all preceeding and trailing whitespaces, and all excess
whitespaces are filtered out before parsing starts. So format your code as you wish:

	INPUT:
		[	BEQ      R0,   T0, :SOME_LABEL   // Comment ]
	TRANSLATION:
		[BEQ R0 T0 :SOME_LABEL]

Of course the content of STRING DATA is not affected by this:

	INPUT:
		[.STRING "\tA    B  " // Comment ]
	TRANSLATION:
		[.STRING "\tA    B  "]
===================================================================================





===================================================================================
LUI
===================================================================================
This instruction will ALWAYS parse the upper 16 bits of a given CONSTANT or LABEL.

	USAGE:
		[CONSTANT]: 0x1234  ==> CONST VALUE is set to 0x00001234
		LUI T0, @CONSTANT	==> loads upper16(VALUE) = 0x0000 into T0
	TRANSLATION:
		LUI T0, 0x0000		

If a Hex Literal with 4 or less Digits is given, it is assumed that the Literal
already represents the upper 16 bits, and it is translated as such:

	USAGE:
		LUI T0, 0x1234		==> loads 0x1234 into T0, not upper16(0x00001234)
	TRANSLATION:
		LUI T0, 0x1234

If the Hex Literal has more Digits, or if the Literal has any other Base, the
actual upper 16 bits of it are used instead:

	USAGE:
		LUI T0, 0x123456	==> loads upper16(0x123456) = 0x0012 into T0
		LUI T1, 250000		==> loads upper16(250000 = 0x3D090) = 0x0003 into T0
	TRANSLATION:
		LUI T0, 0x0012
		LUI T1, 0x0003
===================================================================================





===================================================================================
MULT, MULTU, DIV and DIVU.
===================================================================================
These 4 instructions are treated as PSEUDOS. 2 Formats are allowed and translated:

	FORMAT-A:
		User knows that these instructions are followed by an MFLO. However, these
		instructions don't fit any normal instruction pattern, so we insert R0 as
		destination to turn it into a SPECIAL_DSS instruction. Because R0 == 0b00000,
		this translates to the exact same output.
	USAGE:
		INSTR s, S
	TRANSLATION:
		INSTR R0, s, S

	FORMAT-B:
		User doesn't know that these instructions have to be followed by a MFLO. We
		do the same as above, but we also insert the MFLO in the User-intended way.
	USAGE:
		INSTR d, s, S
	TRANSLATION:
		INSTR R0, s, S
		MFLO d
===================================================================================