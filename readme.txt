--------------------------------------------------------------
ASM6f (v1.6)
A 6502 assembler by loopy (loopy at mm.st)
With modifications by freem, nicklausw, Sour, SpiderDave

ASM6f (this version): https://github.com/SpiderDave/asm6f
ASM6f: https://github.com/freem/asm6f
ASM6: http://3dscapture.com/NES/asm6.zip
--------------------------------------------------------------

ASM6f is a 6502 assembler primarily targeted at NES/Famicom development.

This is free software.  You may use, modify, and / or redistribute any part
of this software in any fashion.

--------------------------------------------------------------
Command line
--------------------------------------------------------------

Usage:

        asm6 [-options] sourcefile [outputfile] [listfile]

Options:

        -?         Show some help
        -l         Create listing
        -L         Create verbose listing (expand REPT, MACRO)
        -d<name>:  Define a symbol and make it equal to 1
        -q         Quiet mode (suppress all output unless there's an error)
        -n         export FCEUX-compatible .nl files
        -f         export Lua symbol file
        -c         export .cdl for use with FCEUX/Mesen
        -m         export Mesen-compatible label file (.mlb)
        -r         raw output. Removes the first 16 bytes of the file.
        Default output is <sourcefile>.bin
        Default listing is <sourcefile>.lst

--------------------------------------------------------------
Syntax
--------------------------------------------------------------

Comments begin with a semicolon (;).  A colon (:) following a label is
optional.

    examples:

        lda #$00             ;hi there
        label1: jmp label2
        label2  beq label1

--------------------------------------------------------------
Numbers and expressions
--------------------------------------------------------------

Hexadecimal numbers begin with '$' or end with 'h'.  Binary numbers begin
with '%' or end with 'b'.  Characters and strings are surrounded by
single or double quotes.  The characters (' " \) within quotes must be
preceded by a backslash (\).

    examples:

        12345
        '12345'
        $ABCD
        0ABCDh
        %01010101
        01010101b

Supported operators (listed by precedence):

          ( )
 (unary)  + - ~ ! < >
          * / %
          + -
          << >>
          < > <= >=
          = == != <> 
          &
          ^
          |
          &&
          ||

'=' and '<>' are equivalent to C's '==' and '!=' operators.  The unary '<'
and '>' operators give the lower and upper byte of a 16-bit word (respectively).
All other operators function like their C equivalents.

--------------------------------------------------------------
Labels
--------------------------------------------------------------

Labels are case sensitive.  The special '$' label holds the current program
address.  Labels beginning with '@' are local labels. They have limited scope,
visible only between non-local labels.  Names of local labels may be reused.

        label1:
          @tmp1:
          @tmp2:
        label2:
          @tmp1:
          @tmp2:

Labels beginning with one or more '+' or '-' characters are nameless labels,
especially useful for forward and reverse branches.

    example:

      --  ldx #0
       -  lda $2002 ;loop (wait for vblank)
          bne -
       -  lda $2002 ;nameless labels are easy to reuse..
          bne -

          cpx #69
          beq +     ;forward branch..
          cpx #96
          beq +here ;use more characters to make more unique

          jmp --    ;multiple --'s handy for nested loops
       +  ldx #0
   +here  nop

--------------------------------------------------------------
Assembler directives (in no particular order)
--------------------------------------------------------------

All directives are case insensitive and can also be preceded by a period (.)


EQU

        For literal string replacement, similar to #define in C.

                one EQU 1
                plus EQU +
                DB one plus one ;DB 1 + 1

=

        Unlike EQU, statements with '=' are evaluated to a number first.
        Also unlike EQU, symbols created with '=' can be reused.

                i=1
                j EQU i+1
                k=i+1   ;k=1+1
                i=j+1   ;i=i+1+1
                i=k+1   ;i=2+1

INCLUDE (also INCSRC)

        Assemble another source file as if it were part of the current
        source.

                INCLUDE whatever.asm

INCBIN (also BIN)

        Add the contents of a file to the assembly output.

                moredata: INCBIN whatever.bin

    An optional file offset and size can be specified.

        INCBIN foo.bin, $400        ;read foo.bin from $400 to EOF
        INCBIN foo.bin, $200, $2000 ;read $2000 bytes, starting from $200

DB, DW (also BYTE/WORD, DCB/DCW, DC.B/DC.W)

        Emit byte(s) or word(s).  Multiple arguments are separated by
        commas.  Strings can be "shifted" by adding a value to them (see
        example).

                DB $01,$02,$04,$08
                DB "ABCDE"+1          ;equivalent to DB "BCDEF"
                DB "ABCDE"-"A"+32     ;equivalent to DB 32,33,34,35,36

DL, DH

        Similar to DB, outputting only the LSB or MSB of a value.

                DL a,b,c,d            ;equivalent to DB <a, <b, <c, <d
                DH a,b,c,d            ;equivalent to DB >a, >b, >c, >d

HEX

        Compact way of laying out a table of hex values.  Only raw hex values
        are allowed, no expressions.  Spaces can be used to separate numbers.

                HEX 456789ABCDEF  ;equivalent to DB $45,$67,$89,$AB,$CD,$EF
                HEX 0 1 23 4567   ;equivalent to DB $00,$01,$23,$45,$67

DSB, DSW (also DS.B/DS.W)

        Define storage (bytes or words).  The size argument may be followed
        by a fill value (default filler is 0).

                DSB 4         ;equivalent to DB 0,0,0,0
                DSB 8,1       ;equivalent to DB 1,1,1,1,1,1,1,1
                DSW 4,$ABCD   ;equivalent to DW $ABCD,$ABCD,$ABCD,$ABCD

PAD

        Fill memory from the current address to a specified address.  A fill
        value may also be specified.

                PAD $FFFA     ;equivalent to DSB $FFFA-$
                PAD $FFFA,$EA ;equivalent to DSB $FFFA-$,$EA

ORG

        Set the starting address if it hasn't been assigned yet, otherwise
        ORG functions like PAD.

                ORG $E000     ;start assembling at $E000
                .
                .
                .
                ORG $FFFA,$80 ;equivalent to PAD $FFFA,$80

ALIGN

        Fill memory from the current address to an N byte boundary.  A fill
        value may also be specified.

                ALIGN 256,$EA

FILLVALUE

        Change the default filler for PAD, ALIGN, etc.

                FILLVALUE $FF

BASE

        Set the program address.  This is useful for relocatable code,
        multiple code banks, etc.  The same can also be accomplished by
        assigning the '$' symbol directly (i.e. '$=9999').

                oldaddr=$
                BASE $6000
                stuff:
                    .
                    .
                    .
                BASE oldaddr+$-stuff

IF / ELSEIF / ELSE / ENDIF

        Process a block of code if an expression is true (nonzero).

                IF j>0
                    DB i/j
                ELSE
                    DB 0
                ENDIF

IFDEF / IFNDEF

        Process a block of code if a symbol has been defined / not defined.

                IFDEF _DEBUG_
                    .
                    .
                    .
                ENDIF

MACRO / ENDM

        MACRO name args...

        Define a macro.  Macro arguments are comma separated.
        Labels defined inside macros are local (visible only to that macro).

                MACRO setAXY x,y,z
                    LDA #x
                    LDX #y
                    LDY #z
                ENDM

                setAXY $12,$34,$56
                        ;expands to LDA #$12
                        ;           LDX #$34
                        ;           LDY #$56

REPT / ENDR

        Repeat a block of code a specified number of times.
        Labels defined inside REPT are local.

                i=0
                REPT 256
                    DB i
                    i=i+1
                ENDR

ENUM / ENDE

        Reassign PC and suppress assembly output.  Useful for defining
        variables in RAM.

                ENUM $200
                foo:    db 0
                foo2:   db 0
                ENDE

ERROR

        Stop assembly and display a message.

                IF x>100
                        ERROR "X is out of range :("
                ENDIF


IGNORENL/ENDINL

        Suppresses output of any labels when exporting FCEUX .nl files.
        Useful for defining labels that may conflict with zero page addresses.

                ; don't show these button masks in the .nl file
                IGNORENL
                PAD_A      = %10000000
                PAD_B      = %01000000
                PAD_SELECT = %00100000
                PAD_START  = %00010000
                PAD_UP     = %00001000
                PAD_DOWN   = %00000100
                PAD_LEFT   = %00000010
                PAD_RIGHT  = %00000001
                ENDINL


UNSTABLE

        Enables use of somewhat unstable 6502 opcodes.

                ; use some unstable undocumented instructions
                ldy #0
                ldx #1
                ahx example,y
                shy example,x
                shx example,y
                tas example,y


HUNSTABLE

        Enables use of highly unstable 6502 opcodes.

                ; throw caution to the wind and use xaa
                HUNSTABLE
                xaa #7

PRINT

        Print text to console.
        Useful for displaying extra information.

                PRINT Hello World!


TEXT
TEXTMAPFROM
TEXTMAPTO

        Define and use textmaps.  The TEXT directive is similar to "DB", but it
        uses a textmap defined with TEXTMAPFROM and TEXTMAPTO.
        
                TEXTMAPFROM 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ x-!.c
                TEXTMAPTO 000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232428292bafcf

                ; outputs 1D 0E 1C 1D
                TEXT "TEST"


IFFILEEXIST/IFFILE

        Process a block of code if a file exists.

                IFFILEEXIST "file.ext"
                    .
                    .
                    .
                ENDIF


--------------------------------------------------------------
iNES directives
--------------------------------------------------------------

Note that using an iNES header is optional; it's only inserted
if at least one of these following directives is used.

INESPRG x
        Number of PRG ROM banks in a NES ROM.

INESCHR x
        Number of CHR ROM banks in a NES ROM.

INESMAP x
        Mapper number of NES ROM.

INESMIR x
        Mirroring mode of a NES ROM.

NES2CHRRAM x
        Amount of CHR RAM used by a NES ROM.

NES2PRGRAM x
        Amount of PRG RAM used by a NES ROM.

NES2SUB x
        Submapper number of NES ROM.

NES2TV x
        TV mode of NES ROM: NTSC, PAL, or both (0, 1 or 2) ('N', 'P' or 'B')

NES2VS
        Sets ROM to use the Vs. Unisystem.

NES2BRAM x
        Amount of battery-packed PRG RAM in NES ROM.

NES2CHRBRAM x
        Amount of battery-packed CHR RAM in NES ROM.

--------------------------------------------------------------
Supported Undocumented Opcodes
--------------------------------------------------------------
asm6f supports the use of a number of undocumented/"illegal" opcodes.
Unstable opcodes require the use of the "UNSTABLE" and/or "HUNSTABLE"
directives, or an error will be thrown.

Information about these opcodes was sourced from Graham's 6502 Opcode document:
http://www.oxyron.de/html/opcodes02.html

[Normal]
These opcodes appear to be "safe" to use.
The alternate $EB opcode for sbc #{immediate} is not supported.

slo (Shift Left, Or)

        slo {addrmode} = asl {addrmode} + ora {addrmode}
        Known as "aso" in other undocumented opcode sources.

rla (Rol Left, And)

        rla {addrmode} = rol {addrmode} + and {addrmode}

sre (Shift Right, Exclusive or)

        sre {addrmode} = lsr {addrmode} + eor {addrmode}
        Known as "lse" in other undocumented opcode sources.

rra (Ror Right, Add with carry)

        rra {addrmode} = ror {addrmode} + adc {addrmode}

sax (Store A&X)

        sax {addrmode} = store A&X into {addrmode}
        "The A&X operation is a result of A and X put onto the bus
        at the same time."

lax (Load A&X)

        lax {addrmode} = lda {addrmode} + ldx {addrmode}

dcp (Decrement, ComPare)

        dcp {addrmode} = dec {addrmode} + cmp {addrmode}

isc (Increment, Subtract with Carry)

        isc {addrmode} = inc {addrmode} + sbc {addrmode}
        Known as "isb" in other undocumented opcode sources.

anc

        anc #{immediate} = and #{immediate} + (asl)
        "this command performs an AND operation only, but bit 7 is put into
        the carry, as if the ASL/ROL would have been executed."


alr (And Left Rotate)

        alr #{immediate} = and #{immediate} + (rol)
        Known as "asr" in other undocumented opcode sources.

arr

        arr #{immediate} = and #{immediate} + lsr
        "Part of this command are some ADC mechanisms. Following effects appear
        after AND but before ROR: the V-Flag is set according to (A and #{imm})+#{imm},
        bit 0 does NOT go into carry, but bit 7 is exchanged with the carry."

axs (A&X, Subtract immediate)

        axs #{immediate} = A&X minus #{immediate} into X
        Known as "sbx" in other undocumented opcode sources. (Groepaz's doc)
        "Performs CMP and DEX at the same time, so that the MINUS sets the flag
        like CMP, not SBC."

las

        las {addrmode} = Stores {addrmode}&S into A,X,S.
        Known as "lar" in other undocumented opcode sources.
        One source (which?) calls las "probably unreliable".

[Unstable]
"unstable in certain matters"; requires UNSTABLE directive.

ahx (A&H&X)

        ahx {addrmode} = stores A&X&H into {addrmode}
        "Sometimes the &H drops off. Page boundary crossing will not work as
        expected; the bank where the value is stored may not be equal to the
        value stored."

shy (Store H&Y)

        shy {addrmode} = stores Y&H into {addrmode}
        "Sometimes the &H drops off. Page boundary crossing will not work as
        expected; the bank where the value is stored may not be equal to the
        value stored."

shx (Store H&X)

        shx {addrmode} = stores X&H into {addrmode}
        "Sometimes the &H drops off. Page boundary crossing will not work as
        expected; the bank where the value is stored may not be equal to the
        value stored."

tas

        tas {addrmode} = Stores A&X into S, A&X&H into {addrmode}

[Highly Unstable]
"DO NOT USE!!!", "results are not predictable on some machines".
YOU HAVE BEEN WARNED.

requires HUNSTABLE directive.

xaa (X And A) HIGHLY UNSTABLE!!!

        xaa #{immediate} = X&#{immediate}
        Known as "ane" in other undocumented opcode sources. (Groepaz's doc)

lax is currently not supported.



--------------------------------------------------------------