
# Documentation of what my Assembler sees

<!--============================================================================-->
## NUMERALS
<!--============================================================================-->
I use the term "**Numeral**" for immediatly available numeric values (called Immediates or Literals). Examples are 120, -4, 0xFF, 0x8038261C, or 0b11001.

Be aware that some MIPS instructions specifically only take the upper or lower halfowrd of any given Numeral; For example **LUI T0, 0x8038261C** will only load the upper 0x80380000 into T0. 
<!--============================================================================-->

## INJECTION POINTS (and PC)
<!--============================================================================-->
MIPS assembly code "lives" somewhere within a File (**ROM Location**) that is being read and written to somewhere in RAM for execution (**RAM Location**).

At runtime, a processor linearly runs through the instructions in RAM. To determine where the processor is currently at, MIPS keeps track of an **Instruction Pointer** that points to the instruction that is currently being executed. 
I call this pointer **PC** (Program Counter), and Im carrying a virtual PC through the assembly, to correctly calculate JUMP-Distances. This virtual PC has nothing to do with execution; it's only there so that the assembler knows where the next instruction lives. Why do I tell you that ? Because you need to understand the PC to get what the next Directives even mean:

Use **.ORG, .ROMORG, .ROM_ORG, .FILEORG** or **.FILE_ORG** in combination with a NUMERAL to
determine where in the editted file the following assembly code will be injected to.
The virtual PC will also move to the new Injection Point.

Use **.RAMORG, .RAM_ORG, .RAMSTART** or **.RAM_START** in combination with a NUMERAL to
determine where the file will exist in RAM during runtime (I call this RAM_START).
For JUMP instructions, the RAM_START value will be added to the targetted location, to turn it into an absolute target. Now, this is a bit shoddy, but you have 2 options here: Either, you dont set RAM_START and specify all JUMP targets as RAM-Addresses, or you set RAM_START and specify all JUMP targets as ROM-Addresses. Don't mix this up, or your JUMPs will land in garbage.
<!--============================================================================-->

<!--============================================================================-->
## DATA
<!--============================================================================-->
Use **.BYTE, .HALF, .HALFWORD** or **.WORD** in addition with 1 or more NUMERALS to insert one or more DATA pieces of the specified size to the current PC. Note that my assembler will do the memory-alignment for you automatically, so there is no need to insert any padding manually:

    WORD               ==    4 Bytes
    HALF / HALFWORD    ==    2 Bytes
    BYTE               ==    1 Byte



Use **.STRING "CONTENT"** to put a string literal (0-terminated) at the current PC.
<!--============================================================================-->

<!--============================================================================-->
## REGISTERS
<!--============================================================================-->
You can call all 32 general purpose registers by their INDEX (0 prefixed or not),
or by their 2-Letter Name (AT == 01, T0 == 08, RA == 31 etc). Additionally allowed:

    ZERO	==	R0		==  00
	STACK	==	SP		==  29
	R	==      RA		==  31

For COP1 / FP Registers, you need to prefix them with 'F' (or 'f'). The numeral
can be 0 prefixed or not, so F02 == F2 for example. You probably want to avoid
using odd FP-Regs, because some N64 Games don't do well with those on default.

You can also prefix the registers with '$' (like $2 == 02 == V0).
<!--============================================================================-->





<!--============================================================================-->
## CONSTANTS
<!--============================================================================-->
**CONSTANTS** can be used to access a preset value all over the file. They can be defined anywhere within the file (because I do a pre-parsing run for a couple of reasons anyways).

You create a new constant by enclosing a name within []-brackets (Note that this is NOT Data, so the constant wont be injected into the file on its own). Whenever you want to use the constant, prefix it with '@'; This assembler will automatically use the correct lower/upper half:

	DEFINITION:
		[CONST_NAME]: VALUE
	USAGE:
		LUI T0, @CONST_NAME		==> will use the upper 16 bits of VALUE
		ADDIU T0, T0, @CONST_NAME	==> will use the lower 16 bits of VALUE

You can also use a CONSTANT as a BRANCH of JUMP target, or as an address offset:

	USAGE:
		JAL @CONSTANT			==> constant has to represent the absolute target !
		LW T0, @CONSTANT(T0)	        ==> loads WORD from *(T0 + lower16(VALUE)) into T0
<!--============================================================================-->





<!--============================================================================-->
## LABELS
<!--============================================================================-->
**LABELS** are basically identical to CONSTANTS here, except their VALUE is automatically set to
the current PC of where they were defined. They are intended to be used as **GOTOs**
in **BRANCH** targets, and don't need special prefixing when used as such: 

	DEFINITION:
		LABEL_NAME:			==> LABEL value (LABEL_PC) is set to current PC
	USAGE:
		BEQ R0, R0, LABEL_NAME	        ==> unconditionally BRANCH to LABEL_PC
            NOP

However, LABELs can also be used as JUMP targets (if RAM_START is defined), or even
as plain CONSTANTS. Whenever you use a LABEL in this context, prefix it with ':':

	USAGE:
		JAL :LABEL_NAME			==> JUMP to (LABEL_PC + RAM_START)
		NOP
	USAGE:
		LI T0, :LABEL_NAME		==> Load LABEL_PC into T0 (like a Function-Pointer)
<!--============================================================================-->




<!--============================================================================-->
## ALIGNMENT
<!--============================================================================-->
All instructions will automatically get WORD aligned. The File is padded with 0x00.

All DATA inserts will automatically get aligned to their own SIZE. STRING DATA will
get aligned as if it was BYTE DATA.
<!--============================================================================-->










<!--============================================================================-->
## PSEUDOS
<!--============================================================================-->
There are a couple of instructions that are not officially part of the set of MIPS instructions, but appear here and there as socalled Pseudo-Instructions, that can easily be translated to real MIPS instructions. Note that some of these translations need 2 instructions.

Quick primer for the abbreviations I use here:

    dst    ==    Destination
    srcN   ==    Source #N
    imm    ==    Immediate

### LI (Load Immediate) and LA (Load Address)
LI and LA load a FULL WORD into the dst-REG. Both instructions are virtually the same because of how I implemented CONSTANTs and LABELs.

	USAGE:
		LI dst, imm
	TRANSLATION:
		LUI dst, imm
		ADDIU dst, dst, imm

### B (Branch unconditionally)
B is an unconditional BRANCH to the specified target. Essentially a GOTO.

	USAGE:
		B label
	TRANSLATION:
		BEQ R0, R0, label

### MOVE
MOVE will copy the WORD from the src-REG to the dst-REG.

	USAGE:
		MOVE dst, src
	TRANSLATION:
		ADDU dst, R0, src

### SUBI (Substract Immediate)
SUBI is in fact not a real instruction (surprise!). However, it is easily substituted by translating this PSEUDO into an ADDI with negative immediate:

	USAGE:
		SUBI dst, src, imm
	TRANSLATION:
		ADDI dst, src, 2complement(imm)

### BLT, BGT, BLE, BGE (conditional Branch Pseudos)
Next we have 4 conditional BRANCH PSEUDOS that are also surprisingly non-standard:

    BLT == BRANCH (if) LESS THAN		==> if (A < B) GOTO target;
    BGT == BRANCH (if) GREATER THAN		==> if (A > B) GOTO target;
    BLE == BRANCH (if) LESS OR EQUAL	==> if (A <= B) GOTO target;
    BGE == BRANCH (if) GREATER OR EQUAL	==> if (A >= B) GOTO target;

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
<!--============================================================================-->


<!--============================================================================-->
## STYLE AND SYNTAX
<!--============================================================================-->
Basically, you can format your assembly however you want, because I do a pre-parsing run to standardize the assembly anyways. If you find something that *doesn't* work, let me know.

#### CAPITALIZATION
You can use whatever capitalization you want. Everything will be transformed into
uppercase to make parsing easier. This means r0 == R0, addiu == ADDIU, etc., but it
also counts for CONSTANTS and LABELS, So @MyConstant == @MYCONSTANT. Remember this
to avoid duplicates (though you will recieve an error message in that case anyways).

#### COMMENTS
If you want to comment your .asm File, you can start a comment by prefixing it with
"//", "#" or ";". Everything behind those symbols is completely ignored in parsing.

#### INDENTATION + WHITESPACES
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




<!--============================================================================-->
## Additional Notes on LUI
<!--============================================================================-->
LUI will ALWAYS parse the upper 16 bits of a given CONSTANT or LABEL.

	USAGE:
		[CONSTANT]: 0x1234	==> CONST VALUE is set to 0x00001234
		LUI T0, @CONSTANT	==> loads upper16(VALUE) = 0x00000000 into T0
	TRANSLATION:
		LUI T0, 0x0000		

If a Hex Literal with 4 or less Digits is given, it is assumed that the Literal
already represents the upper 16 bits, and it is translated as such:

	USAGE:
		LUI T0, 0x1234		==> loads 0x12340000 into T0, not upper16(0x00001234)
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
<!--============================================================================-->





<!--============================================================================-->
## Additional Notes on MULT, MULTU, DIV and DIVU
<!--============================================================================-->
These 4 instructions are treated as PSEUDOS. 2 Formats are allowed and translated:

#### FORMAT-A
User knows that these instructions are followed by an MFLO (correctly omitted dst-Reg from the instruction in Question). Because these instructions don't fit any normal instruction pattern, I insert R0 == 0b00000 as the dst-Reg to turn it into a SPECIAL_DSS instruction for easier parsing:

	USAGE:
		INSTR s, S
            MFLO d
	TRANSLATION:
		INSTR R0, s, S
            MFLO d

#### FORMAT-B
User doesn't know that these instructions have to be followed by a MFLO (usage of a dst-Reg in the first instruction implies this). I do the same as above, but also insert the MFLO as intended:

	USAGE:
		INSTR d, s, S
	TRANSLATION:
		INSTR R0, s, S
		MFLO d
<!--============================================================================-->

<!--============================================================================-->
## LIMITER // TODO, unimplemented but kinda cool

<!--============================================================================-->
Use .LIMIT, .STOP or .STOP_HERE in combination with a NUMERAL to define a point
after which no further code injection may happen. This is meant to avoid overwriting
parts of the editted File that you dont want to overwrite (always make backups).
When the current PC reaches the point defined through a LIMITER, an error message
will pop up and the assembly will stop.

Use .SIZE or .MAX_SIZE in combination with a NUMERAL to determine how far the current
PC is allowed to proceed from the last INJECTION POINT. When the difference between
the current PC and the last INJECTION POINT reaches the determined SIZE, an error
message will pop up and the assembly will stop.
<!--============================================================================-->

<!--============================================================================-->
## .FLOAT // TODO, unimplemented but kinda cool

Another DATA insert that can read floating point literals and convert them to the correct bytes for insertion. This might end up not being a DATA directive, but just part of the allowed NUMERALS.

