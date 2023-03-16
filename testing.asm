//#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//# Labels and Constants
[PLAYER_VELOC]:         0x8037C4B8
[PLAYER_POS_1]:         0x8037C5A0 //# remember 0x10 offset
[PLAYER_POS_2]:         0x8037C5B0
[LAST_GROUNDED_POS]:    0x8037C2D8 //# 3 floats
[PLAYER_HEALTH]:        0x80385F80
[DATA]:                 0x8046B8D0
[TIMER]:                0x8037C5AC
[BUTTON_ARR_1]:         0x802812B8 //# 1B (ABZS and DPad-UDLR)
[BUTTON_ARR_2]:         0x802812B9 //# 1B (??LR and C-UDLR)
[JOYSTICK_X]:           0x802812BA //# 1B int
[JOYSTICK_Y]:           0x802812BB //# 1B int
[A_BUTTON_TIMER]:       0x80281138 //# uint
[B_BUTTON_TIMER]:       0x8028113C //# uint
[Z_BUTTON_TIMER]:       0x80281150 //# uint
[L_BUTTON_TIMER]:       0x80281154 //# uint
[R_BUTTON_TIMER]:       0x80281158 //# uint
[Cr_BUTTON_TIMER]:      0x8028114C //# uint
[CAM_Y_ROTATION]:       0x8037D96C //# float
[SLOPE_TIMER]:          0x8037C2E4 //# float 0.0 to 1.0
[SLOPE_DIRECTION]:      0x8037C0C8 //# float (2 floats actually)
[MAP_ID]:               0x8037E8F5 //# 1B
[SPECIAL_MAP_ID]:       0x803835D4 //# int
[OBJ_ARR]:              0x8036E560
[PARTICLE_ARR]:         0x80384490
[CHARGING_SSPRING]:     0x8037D4C2 //# 1B
[RAREWARE_BOX_BROKEN]:  0x803831A6 //# 1B, the last 2 Bits
[GAME_SPEED_MODIF]:     0x80384480 //# float

[COLLECTABLES_BASE]:    0x80385F30
[JINJO_OFFSET]:         0x48
[CUR_HP_OFFSET]:        0x50
[MAX_HP_OFFSET]:        0x54
[EXTRA_LIVES_OFFSET]:   0x58
[TOTAL_JIGGYS_OFFSET]:  0x98

[PLAYER_RGB]:           0x8037C6F4 //# 3 floats RGB
[GLOBAL_TIMER]:         0x8027DCC8
[MAP_MODEL_POINTER]:    0x80382368
[TMAP_MODEL_POINTER]:   0x8038236C
[FLOOR_OBJ_POINTER]:    0x8037C200
[sFLOOR_OBJ_POINTER]:   0x8038C200
[WALL_COLLISIONS]:      0x8037C27D
[COLLIDING_NORMAL]:     0x8037C258
[PLAYER_FACING_ANG]:    0x8037C690
[PLAYER_MOVING_ANG]:    0x8037C694
[PLAYER_GROUNDED]:      0x8037BF60 //# 1B
[PLAYER_WET]:           0x8037BF61
[PREV_MOV_STATE]:       0x8037D160
[CUR_MOV_STATE]:        0x8037D164
[CUR_ANIM_STATE]:       0x8037D2BA //# 1B
[TTRAINERS_VISIBLE]:    0x8037D237 //# 1B
[TOP_SPEED]:            0x8037C4F0 //# float
[USED_MOVES_BF]:        0x8037C3A7 //# 1B (here's BF atleast)
[JIGGY_BF]:             0x803832C0
[HONEYCOMB_BF]:         0x803832E0

[TEXTPOINTER]:          0x80382F2C

[DUMMY_DATA]:           0x804759B0
[EXTRA_SIZE]:           0x00010000
[HIJACK_JAL_LOC]:       0x8024E7D4
[sHIJACK_JAL_LOC]:      0x8025E7D4
[HIJACK_RA]:            0x8023E000

[fSQRT]:                0x80265350
[fSET_JIG_BIT]:         0x80321120 //# A0 = bit, A1 = boolean
[fSET_JIG_COLLECTED]:   0x8032108C //# A0 = bit, A1 = boolean
[fXYZ_DISTANCE]:        0x80256064
[fXZ_DISTANCE]:         0x80258640
[fXYZ_DISTANCE_VEC]:    0x80258BC0
[fSET_MOVSTATE]:        0x8029A72C
[fNORM_VECTOR]:         0x80256450
[fSET_MISC_FLAG_T]:     0x802933E8 //# A0 = ID
[fSET_MISC_FLAG_F]:     0x802933FC //# A0 = ID
[fSPAWN_ACTOR]:         0x803056FC //# A0 = ID, A1 = &pos, A2 = frot
[fPRINT_TEXT]:          0x802F7870
[fITOA]:                0x8033D884 //# A0 dst-p, A1 src int

//# to create a JAL manually, take the address, replace 0x80 by 0x0C
//# and shift the address right twice (or divide by 4)
// 0x03F42930 = 0x80442930 in ROM - space: 0x670 (1648 Bytes)
// 0x03F6B8D0 = 0x8046B8D0 in ROM - space: 0x1D0 (464 B Bytes)
//#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

//# Usage of Pos1+0x0C:
//# Byte 1 - Control Bitfield
//#   Bit 1 - Flight Troll
//#   Bit 2 - Control Loss
//#   Bit 3 - Smth moving Up
//#   Bit 4 - Smth moving Down
//#   Bit 5 - Playing Spring SFX
//#   Bit 6 - Hatch Broken
//#   Bit 7 - Mumbo Spell Started
//#   Bit 8 - Termite Spell Done
//# Byte 2 - Wall Timer
//# Byte 3 - Ice Timer
//# Byte 4 - Sand Timer

//# Usage of Pos2+0x0C:
//# Byte 1 - Death Timer
//# Byte 2 - Death Count
//# Byte 3 - Jiggy-10 collected
//# Byte 4 - Jiggy-10 Timer

//#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.org 0x00
.word 0x80442930
//#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.org 0x20 //# Runtime JAL-Hijack by ThatCowGuy
                //# ROM: 0x80442930
NOP
NOP
ADDIU SP, SP, 0xFFE0
SW RA, 0x1C(SP)
SW T0, 0x18(SP)
SW T1, 0x14(SP)
SW A0, 0x10(SP)
SW A1, 0x0C(SP)
SW A2, 0x08(SP)

//# injecting the Mainloop-Hook
LUI T1, 0x0C11 //# HEX version of JAL 0x804754B0
ORI T1, T1, 0xD52C
LUI T0, @HIJACK_JAL_LOC
ORI T0, T0, @HIJACK_JAL_LOC
SW T1, 0x00(T0)

//# performing the DMA
LUI A0, 0x8047
ORI A0, A0, 0x54B0
LUI A1, 0x03F7
ORI A1, A1, 0x54B0
LUI A2, @EXTRA_SIZE
JAL 0x802405F0
ORI A2, A2, @EXTRA_SIZE

//# disabling THIS function
LUI T0, 0x03E0
ORI T0, T0, 0x0008 //# JR RA
LUI T1, 0x8044
SW T0, 0x2930(T1)
LW A2, 0x08(SP)
LW A1, 0x0C(SP)
LW A0, 0x10(SP)
LW T1, 0x14(SP)
LW T0, 0x18(SP)
LW RA, 0x1C(SP)
JR RA
ADDIU SP, SP, 0x20
//#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.org 0xC0 //# Custom ASM by ThatCowGuy
               //# Code is @0x804754B0 (0x7C500000 offset)
MAIN:
//# store some regs for later restoring
ADDIU SP, SP, 0xFFC0 //# -0x40
SW RA, 0x14(SP)

//# Pre-Fill the entire used moves BF to avoid the BGS JiggySpawn Glitch
LUI T0, @USED_MOVES_BF
ORI T0, T0, @USED_MOVES_BF
ORI T1, R0, 0xFF
SB T1, 0x00(T0)
//# Force 9 Lifes always
LUI T0, @COLLECTABLES_BASE
ORI T0, T0, @COLLECTABLES_BASE
ORI T1, R0, 0x09
SW T1, @EXTRA_LIVES_OFFSET(T0)
//# Force 3 Max HP always
LUI T0, @COLLECTABLES_BASE
ORI T0, T0, @COLLECTABLES_BASE
ORI T1, R0, 0x03
SW T1, @MAX_HP_OFFSET(T0)
LW T2, @CUR_HP_OFFSET(T0)
BLT T2, T1, DONT_FIX_HP
NOP
    SW T1, @CUR_HP_OFFSET(T0)
DONT_FIX_HP:

//# Take away BGS Switch Jiggy Flag and apply 10th MM Jiggy Flag
LUI T0, @JIGGY_BF
ORI T0, T0, @JIGGY_BF
LBU T1, 0x04(T0) //# BGS Switch Jiggy ID = 0x34
ANDI T1, T1, 0x20
BEQ T1, R0, NO_BGS_JIGGY_COLLECTED
NOP
    ORI A0, R0, 0xA //# Final MM Jiggy ID = 0xD
    JAL @fSET_JIG_BIT
    ORI A1, R0, 0x01

    ORI A0, R0, 0xA //# Final MM Jiggy ID = 0xD
    JAL @fSET_JIG_COLLECTED
    ORI A1, R0, 0x01
NO_BGS_JIGGY_COLLECTED:


LUI T0, @TEXTPOINTER
ORI T0, T0, @TEXTPOINTER
LW T0, 0x00(T0)
BEQ T0, R0, NO_MUMBO_EFFECT //# NULL Check...
NOP
    LUI T3, @PLAYER_POS_1
    ORI T3, T3, @PLAYER_POS_1
    LBU T1, 0x0C(T3) //# Custom Bitfield
    ANDI T2, T1, 0x02 //# Bit-7 = Mumbo Effect Started
    BNE T2, R0, TERMITE_TIME
    NOP
        //# Checking for Mumbo's Line
        LBU T0, 0x00(T0)
        ORI T4, R0, 0x59 //# 'Y' - Mumbo's Line
        BNE T0, T4, NO_MUMBO_EFFECT
        NOP
            ORI T2, T1, 0x02
            SB T2, 0x0C(T3)
            
            ORI A0, R0, 0x1D
            JAL 0x8025A6EC //# Play CoMusic
            ORI A1, R0, 0x7FFF

            BEQ R0, R0, NO_MUMBO_EFFECT
            NOP
    TERMITE_TIME:
    LUI T3, @PLAYER_POS_1
    ORI T3, T3, @PLAYER_POS_1
    LBU T1, 0x0C(T3) //# Custom Bitfield
    ANDI T2, T1, 0x01 //# Bit-8 = Termite Spell Done
    BNE T2, R0, NO_MUMBO_EFFECT
    NOP
        //# Checking for Tickers's Line
        LBU T0, 0x00(T0)
        ORI T4, R0, 0x4E //# 'N' - Tickers's Line
        BNE T0, T4, NO_MUMBO_EFFECT
        NOP
            ORI T2, T1, 0x01
            SB T2, 0x0C(T3)

            ADDI A0, R0, 0x01
            LUI A1, 0x3F80 //# SFX Speed = 1.0f
            JAL 0x8030E760 //# SFX func
            ORI A2, R0, 0x37FF
NO_MUMBO_EFFECT:

LUI T0, @PLAYER_POS_2
ORI T0, T0, @PLAYER_POS_2
LBU T1, 0x0C(T0) //# Timer so they dont stack
BEQ T1, R0, DONT_DECREMENT_DEATH_TIMER
NOP
    SUBI T1, T1, 0x01
    BEQ R0, R0, DEATH_COUNTER_END
    SB T1, 0x0C(T0)
DONT_DECREMENT_DEATH_TIMER:
LUI T2, @CUR_MOV_STATE
ORI T2, T2, @CUR_MOV_STATE
LW T2, 0x00(T2)
ORI T3, R0, 0x41 //# Death
BEQ T2, T3, NEW_DEATH
NOP
    LUI T2, @PLAYER_POS_1
    ORI T2, T2, @PLAYER_POS_1
    LWC1 F0, 0x04(T2)
    CVT.W.S F0, F0
    MFC1 T2, F0
    LUI T3, 0xFFFF
    ORI T3, T3, 0xE0C0 //# -8000
    BLT T3, T2, DEATH_COUNTER_END
    NOP
        NEW_DEATH:
        ORI T1, R0, 0x78 //# 4s
        SB T1, 0x0C(T0)
        LBU T1, 0x0D(T0) //# Actual Death Coutner
        ADDIU T1, T1, 0x01
        SB T1, 0x0D(T0)
DEATH_COUNTER_END:

LUI A1, @CUR_MOV_STATE
ORI A1, A1, @CUR_MOV_STATE
LW A1, 0x00(A1)
ORI A2, R0, 0x01
BNE A1, A2, NO_BITFIELD_RESET
NOP
    LUI A1, @PLAYER_POS_1
    ORI A1, A1, @PLAYER_POS_1
    LBU T3, 0x0C(A1) //# Custom Bitfield
    BEQ T3, R0, NO_BITFIELD_RESET
    NOP
        LUI A0, @dRESPAWN_LOC
        JAL @fXZ_DISTANCE //# A1 = &player_pos already
        ORI A0, A0, @dRESPAWN_LOC
        CVT.W.S F0, F0
        MFC1 T2, F0
        ORI T3, R0, 0x3E8 //# 1000
        BLT T3, T2, NO_BITFIELD_RESET
        NOP
            //# Bitfield Clear
            SB R0, 0x0C(A1)

            ORI A0, R0, 0x55
            JAL 0x8025A6EC //# Play CoMusic
            ORI A1, R0, 0x00 
NO_BITFIELD_RESET:

//# Killing the random vile theme after a Jiggy Grab...
LUI A0, @CUR_MOV_STATE
ORI A0, A0, @CUR_MOV_STATE
LW A0, 0x00(A0)
ORI A1, R0, 0x01 //# Idle
BEQ A0, A1, STOP_VILE_THEME
ORI A1, R0, 0x73 //# Locked
BEQ A0, A1, STOP_VILE_THEME
NOP
BEQ R0, R0, DONT_STOP_VILE_THEME
NOP
    STOP_VILE_THEME:
    ORI A0, R0, 0x55
    JAL 0x8025A6EC //# Play CoMusic
    ORI A1, R0, 0x00
DONT_STOP_VILE_THEME:

LUI T0, @PLAYER_POS_1
ORI T0, T0, @PLAYER_POS_1
LBU T1, 0x0C(T0) //# Custom Bitfield
ANDI T1, T1, 0x40 //# Check Bit-1
BEQ T1, R0, NO_CONTROL_LOSS
NOP
    LUI T0, @BUTTON_ARR_1
    ORI T0, T0, @BUTTON_ARR_1
    SH R0, 0x00(T0)
NO_CONTROL_LOSS:

LUI T0, @TTRAINERS_VISIBLE
ORI T0, T0, @TTRAINERS_VISIBLE
LBU T0, 0x00(T0)
BEQ T0, R0, NO_TURBO_TRAINER_TROLL
NOP
    LUI T0, @CUR_MOV_STATE
    ORI T0, T0, @CUR_MOV_STATE
    LW T0, 0x00(T0)
    ORI T1, R0, 0x08 //# TT-Jumping
    BEQ T0, T1, TURBO_TRAINER_TROLL
    LUI T1, 0x3F98 //# T1 = 1.1875
    ORI T1, R0, 0x71 //# TT-Falling
    BEQ T0, T1, TURBO_TRAINER_TROLL
    LUI T1, 0x3F98 //# T1 = 1.1875
    ORI T1, R0, 0x16 //# TT-Walking
    BEQ T0, T1, TURBO_TRAINER_TROLL
    LUI T1, 0x3FB0 //# T1 = 1.375
    BEQ R0, R0, NO_TURBO_TRAINER_TROLL
    NOP
        TURBO_TRAINER_TROLL:
        MTC1 T1, F20
        LUI T0, @PLAYER_VELOC
        ORI T0, T0, @PLAYER_VELOC
        LWC1 F2, 0x00(T0)
        LWC1 F4, 0x08(T0)

        LUI T0, @PLAYER_POS_2
        ORI T0, T0, @PLAYER_POS_2
        LBU T0, 0x0C(T0) //# Death Timer
        BNE T0, R0, NO_TURBO_TRAINER_DEATH
        NOP
            MUL.S F12, F2, F2
            MUL.S F14, F4, F4
            JAL @fSQRT
            ADD.S F12, F12, F14
            CVT.W.S F0, F0
            MFC1 T1, F0
            ORI T2, R0, 0x07D0 //# 2000
            BLT T1, T2, NO_TURBO_TRAINER_DEATH
                //# add a death to the count
                LUI T0, @PLAYER_POS_2
                ORI T0, T0, @PLAYER_POS_2
                ORI T1, R0, 0x78 //# 4s
                SB T1, 0x0C(T0)
                LBU T1, 0x0D(T0) //# Actual Death Coutner
                ADDIU T1, T1, 0x01
                SB T1, 0x0D(T0)
        NO_TURBO_TRAINER_DEATH:

        LUI T0, @PLAYER_VELOC
        ORI T0, T0, @PLAYER_VELOC
        MUL.S F2, F2, F20
        MUL.S F4, F4, F20
        LUI T1, @BUTTON_ARR_1
        SWC1 F2, 0x00(T0)
        SWC1 F4, 0x08(T0)
        ORI T1, T1, @BUTTON_ARR_1
        LBU T0, 0x00(T1)
        ANDI T0, T0, 0xBF //# 1011 1111
        SB T0, 0x00(T1) //# Disable Ratatat-Cheese
NO_TURBO_TRAINER_TROLL:

LUI T0, @PLAYER_GROUNDED
ORI T0, T0, @PLAYER_GROUNDED
LBU T0, 0x00(T0)
BEQ T0, R0, NO_SLOPE_TIMER_TROLL
LUI T0, @SLOPE_TIMER
    ORI T0, T0, @SLOPE_TIMER
    LW T1, 0x00(T0)
    BEQ T1, R0, NO_SLOPE_TIMER_TROLL
    MTC1 T1, F0
        LUI T1, 0x467A //# 16k.0
        MTC1 T1, F2
        MUL.S F0, F0, F2
        NOP
        ORI T2, R0, 0x3840 //# 14400 = 90% of normal slope timer
        CVT.W.S F0, F0
        MFC1 T1, F0
        BGT T1, T2, NO_SLOPE_TIMER_TROLL
        LUI T3, 0x3F80 //# 1.0
            ADDI A0, R0, 0xC7 
            LUI A1, 0x3F80 //# SFX Speed = 1.0f
            ORI A2, R0, 0x37FF
            JAL 0x8030E760 //# SFX func
            SW T3, 0x00(T0)
NO_SLOPE_TIMER_TROLL:

//# Unset Bit-6 (RBox exists)
LUI T0, @PLAYER_POS_1
ORI T0, T0, @PLAYER_POS_1
LBU T1, 0x0C(T0)
ANDI T1, T1, 0xFB //# Kill Bit-6
SB T1, 0x0C(T0)

//# set up obj arr start
LUI T0, @OBJ_ARR
ORI T0, T0, @OBJ_ARR
LW T0, 0x00(T0) //# obj arr header
LW T9, 0x00(T0) //# obj count
ADDIU T0, T0, 0x08 //# obj arr start
OBJ_LOOP:
    BEQ T9, R0, OBJ_LOOP_BREAK
    LW T1, 0x44(T0)
    SLL T1, T1, 0x08
    SRL T1, T1, 0x16 //# = (val & 0x00FFFFFF) >> 14 = Obj-ID

    ORI T2, R0, 0x3C0 //# Gruntling 3
    BNE T1, T2, NOT_GRUNTLING_THREE
    NOP
        LBU T1, 0x166(T0) //# Gruntling HP
        ORI T2, R0, 0x63 //# Gruntling Max-HP = 99
        BEQ T1, T2, GRUNTLING_THREE_AT_FULL_HP
        ORI A0, R0, 0x14
            SUB A1, T1, T2 //# MaxHP - CurHP
            ORI T1, R0, 0x19
            DIV A1, T1
            MFLO A1
            JAL 0x803463D4
            SB T2, 0x166(T0)
            BNE V0, R0, GRUNTLING_THREE_AT_FULL_HP //# V0 = Banjo HP
            NOP
                JAL @fKILL_BANJO
                NOP
        GRUNTLING_THREE_AT_FULL_HP:
        BEQ R0, R0, OBJ_LOOP_CONTINUE
        NOP
    NOT_GRUNTLING_THREE:

    ORI T2, R0, 0x2A4 //# TNT Box Part 1
    BNE T1, T2, NOT_TNT_BOX
    NOP
        LUI T2, @CUR_MOV_STATE
        ORI T2, T2, @CUR_MOV_STATE
        LW T2, 0x00(T2)
        ORI T3, R0, 0x73 //# Locked
        BNE T2, T3, DONT_MOVE_TNT
        NOP
            LUI A1, @PLAYER_POS_1
            ORI A1, A1, @PLAYER_POS_1
            JAL @fXZ_DISTANCE //# A1 = &player_pos already
            ADDI A0, T0, 0x04 //# A0 = &obj_pos
            CVT.W.S F0, F0
            MFC1 T2, F0
            ORI T3, R0, 0x12C //# 300
            BLT T3, T2, DONT_MOVE_TNT
            NOP
                //# [BUTTON_ARR_1]: 0x802812B8 //# 1B (ABZS and DPad-UDLR)
                //# [BUTTON_ARR_2]: 0x802812B9 //# 1B (??LR and C-UDLR)
                LUI T3, @BUTTON_ARR_1
                ORI T3, T3, @BUTTON_ARR_1
                LHU T3, 0x00(T3)
                ORI T4, R0, 0x4030 //# 0b 01000000 00110000
                AND T3, T3, T4
                BNE T3, T4, DONT_MOVE_TNT
                NOP
                    LUI T2, @PLAYER_HEALTH
                    ORI T2, T2, @PLAYER_HEALTH
                    SW R0, 0x00(T2)
                    LW T3, 0x00(A1)
                    SW T3, 0x00(A0)
                    LW T3, 0x04(A1)
                    SW T3, 0x04(A0)
                    //# 5 units extra, to prevent 0-distance
                    LWC1 F0, 0x08(A1)
                    LUI T3, 0x4220 //# 40.0
                    MTC1 T3, F2
                    ADD.S F0, F0, F2
                    SWC1 F0, 0x08(A0)
        DONT_MOVE_TNT:
        BEQ R0, R0, OBJ_LOOP_CONTINUE
        NOP
    NOT_TNT_BOX:

    ORI T2, R0, 0x225 //# Rareware Box
    BNE T1, T2, NOT_RAREWARE_BOX
    NOP
        LUI T2, @PLAYER_POS_1
        ORI T2, T2, @PLAYER_POS_1
        LBU T3, 0x0C(T2)
        ORI T3, T3, 0x04
        SB T3, 0x0C(T2)
        BEQ R0, R0, OBJ_LOOP_CONTINUE
        NOP
    NOT_RAREWARE_BOX:

    ORI T2, R0, 0x2E0 //# TTC Stairs
    BNE T1, T2, NOT_TTC_STAIRS
    NOP
        LUI T2, @PLAYER_POS_1
        ORI T2, T2, @PLAYER_POS_1
        LBU T2, 0x0C(T2)
        ANDI T2, T2, 0x04
        BNE T2, R0, DONT_MOVE_TTC_STAIRS
            LUI A1, @PLAYER_POS_1
            ORI A1, A1, @PLAYER_POS_1
            LWC1 F0, 0x04(A1)
            CVT.W.S F0, F0
            MFC1 T2, F0
            LUI T3, 0xFFFF
            ORI T3, T3, 0xE0C0 //# -8000
            BLT T3, T2, DONT_MOVE_TTC_STAIRS
            NOP
                LUI T2, 0x4422
                ORI T2, T2, 0x8000 //# 600.0
                SW T2, 0x08(T0)
        DONT_MOVE_TTC_STAIRS:
        BEQ R0, R0, OBJ_LOOP_CONTINUE
        NOP
    NOT_TTC_STAIRS:

    ORI T2, R0, 0x20F //# RBB Door
    BNE T1, T2, NOT_RBB_DOOR
    NOP
        LUI T2, @COLLECTABLES_BASE
        ORI T2, T2, @COLLECTABLES_BASE
        LW T2, @TOTAL_JIGGYS_OFFSET(T2)
        ORI T3, R0, 0x0A
        BLT T2, T3, DONT_MOVE_RBB_DOOR
        NOP
            LUI T2, 0x453B //# 2992.0
            SW T2, 0x08(T0)
        DONT_MOVE_RBB_DOOR:
        BEQ R0, R0, OBJ_LOOP_CONTINUE
        NOP
    NOT_RBB_DOOR:

    ORI T2, R0, 0x46 //# Jiggy
    BNE T1, T2, NOT_JIGGY
    NOP
        LUI A1, @PLAYER_POS_1
        ORI A1, A1, @PLAYER_POS_1
        LWC1 F0, 0x00(A1)
        CVT.W.S F0, F0
        MFC1 T2, F0
        ORI T3, R0, 0x1194 //# 4500
        BLT T2, T3, OBJ_LOOP_CONTINUE //# not the CORRECT Jiggy
        NOP
            JAL @fXZ_DISTANCE //# A1 = &player_pos already
            ADDI A0, T0, 0x04 //# A0 = &obj_pos
            CVT.W.S F0, F0
            MFC1 T2, F0
            ORI T3, R0, 0xB5 //# 180
            BLT T3, T2, DONT_PLAY_JIGGY_SFX
            NOP
                ADDI A0, T0, 0x04 //# A0 = &obj_pos
                LWC1 F0, 0x04(A0)
                CVT.W.S F2, F0
                MFC1 T2, F2
                ORI T3, R0, 0x640 //# 1600
                BLT T3, T2, DONT_PLAY_JIGGY_SFX
                NOP
                    LUI T2, 0x42A0 //# 80.0
                    MTC1 T2, F2
                    ADD.S F0, F0, F2
                    MFC1 T2, F0
                    SW T2, 0x04(A0)

                    LUI A0, @PLAYER_POS_1
                    ORI A0, A0, @PLAYER_POS_1
                    LBU T1, 0x0C(A0) //# Custom Bitfield
                    ANDI T2, T1, 0x20
                    BNE T2, R0, DONT_PLAY_JIGGY_SFX
                    NOP
                        ORI T2, T1, 0x20
                        SB T2, 0x0C(A0)
                        
                        SW T0, 0x20(SP)
                        SW T9, 0x24(SP)
                        ADDI A0, R0, 0xC7
                        LUI A1, 0x3F80 //# SFX Speed = 1.0f
                        JAL 0x8030E760 //# SFX func
                        ORI A2, R0, 0x37FF
                        LW T0, 0x20(SP)
                        LW T9, 0x24(SP)
            DONT_PLAY_JIGGY_SFX:
            BEQ R0, R0, OBJ_LOOP_CONTINUE
            NOP
    NOT_JIGGY:

    ORI T2, R0, 0x37A //# Bottles Mound
    BNE T1, T2, NOT_BOTTLES_MOUND
    NOP
        LUI T2, @MAP_ID
        ORI T2, T2, @MAP_ID
        LBU T2, 0x00(T2)
        ORI T3, R0, 0x0C //# Tickers Tower / Church
        BEQ T2, T3, DONT_MOVE_BOTTLES_MOUND
        NOP
            LUI T2, @PLAYER_POS_2
            ORI T2, T2, @PLAYER_POS_2
            LBU T2, 0x0D(T2)
            BEQ T2, R0, DONT_MOVE_BOTTLES_MOUND
                LUI T2, 0x42CC //# 102.0
                SW T2, 0x08(T0)
        DONT_MOVE_BOTTLES_MOUND:
        BEQ R0, R0, OBJ_LOOP_CONTINUE
        NOP
    NOT_BOTTLES_MOUND:
    
    ORI T2, R0, 0x05 //# Ticker
    BNE T1, T2, NOT_TICKER
    NOP
        LUI T2, @PLAYER_POS_1
        ORI T2, T2, @PLAYER_POS_1
        LBU T3, 0x0C(T2) //# Custom Bitfield
        ANDI T4, T3, 0x01 //# Ticker Spell Done
        BEQ T4, R0, OBJ_LOOP_CONTINUE
        NOP
            //# Size Change
            LUI A0, 0x3F00 //# 0.5
            SW A0, 0x128(T0)

            ADDI A0, T0, 0x04 //# A0 = &obj_pos
            LUI A1, @PLAYER_POS_1
            ORI A1, A1, @PLAYER_POS_1
            LWC1 F0, 0x00(A1)
            LUI T3, 0x4348 //# 200.0
            MTC1 T3, F2
            ADD.S F0, F0, F2
            SWC1 F0, 0x00(A0)

            LW T3, 0x04(A1)
            SW T3, 0x04(A0)
            LW T3, 0x08(A1)
            SW T3, 0x08(A0)
            BEQ R0, R0, OBJ_LOOP_CONTINUE
            NOP
    NOT_TICKER:

    ORI T2, R0, 0x229 //# FP House
    BNE T1, T2, NOT_HOUSE
    NOP
        ADDI A0, T0, 0x04 //# A0 = &obj_pos
        LUI A1, @PLAYER_POS_1
        JAL @fXZ_DISTANCE
        ORI A1, A1, @PLAYER_POS_1
        CVT.W.S F0, F0
        MFC1 T2, F0
        ORI T3, R0, 0xFA //# 250
        BLT T3, T2, DONT_PLAY_HOUSE_SFX
        NOP
            ADDI A0, T0, 0x04 //# A0 = &obj_pos
            LWC1 F0, 0x08(A0)
            LUI T2, 0x42A0 //# 80.0
            MTC1 T2, F2
            ADD.S F0, F0, F2
            MFC1 T2, F0
            SW T2, 0x08(A0)

            LUI A0, @PLAYER_POS_1
            ORI A0, A0, @PLAYER_POS_1
            LBU T1, 0x0C(A0) //# Custom Bitfield
            ANDI T2, T1, 0x20
            BNE T2, R0, DONT_PLAY_HOUSE_SFX
            NOP
                ORI T2, T1, 0x20
                SB T2, 0x0C(A0)
                
                SW T0, 0x20(SP)
                SW T9, 0x24(SP)
                ADDI A0, R0, 0xC7
                LUI A1, 0x3F80 //# SFX Speed = 1.0f
                JAL 0x8030E760 //# SFX func
                ORI A2, R0, 0x37FF
                LW T0, 0x20(SP)
                LW T9, 0x24(SP)
            DONT_PLAY_HOUSE_SFX:
        BEQ R0, R0, OBJ_LOOP_CONTINUE
        NOP
    NOT_HOUSE:

    ORI T2, R0, 0x1F9 //# GV Cactus
    BNE T1, T2, NOT_CACTUS
    NOP
        ADDI A0, T0, 0x04 //# A0 = &obj_pos
        LUI A1, @PLAYER_POS_1
        JAL @fXYZ_DISTANCE
        ORI A1, A1, @PLAYER_POS_1
        CVT.W.S F0, F0
        MFC1 T2, F0
        ORI T3, R0, 0x15E //# 350
        BLT T3, T2, DONT_PLAY_CACTUS_SFX
        NOP
            ADDI A0, T0, 0x04 //# A0 = &obj_pos
            LWC1 F0, 0x04(A0)
            LUI T2, 0x42A0 //# 80.0
            MTC1 T2, F2
            ADD.S F0, F0, F2
            MFC1 T2, F0
            SW T2, 0x04(A0)
            
            LUI A0, @PLAYER_POS_1
            ORI A0, A0, @PLAYER_POS_1
            LBU T1, 0x0C(A0) //# Custom Bitfield
            ANDI T2, T1, 0x20
            BNE T2, R0, DONT_PLAY_CACTUS_SFX
            NOP
                ORI T2, T1, 0x20
                SB T2, 0x0C(A0)
                
                SW T0, 0x20(SP)
                SW T9, 0x24(SP)
                ADDI A0, R0, 0xC7
                LUI A1, 0x3F80 //# SFX Speed = 1.0f
                JAL 0x8030E760 //# SFX func
                ORI A2, R0, 0x37FF
                LW T0, 0x20(SP)
                LW T9, 0x24(SP)
            DONT_PLAY_CACTUS_SFX:
        BEQ R0, R0, OBJ_LOOP_CONTINUE
        NOP
    NOT_CACTUS:

    ORI T2, R0, 0x22C //# FP Present Stack
    BNE T1, T2, NOT_PRESENT_STACK
    NOP
        ADDI A0, T0, 0x04 //# A0 = &obj_pos
        LUI A1, @PLAYER_POS_1
        JAL @fXZ_DISTANCE
        ORI A1, A1, @PLAYER_POS_1
        CVT.W.S F0, F0
        MFC1 T2, F0
        ORI T3, R0, 0xFA //# 250
        BLT T3, T2, DONT_PLAY_PRESENT_STACK_SFX
        NOP
            ADDI A0, T0, 0x04 //# A0 = &obj_pos
            LWC1 F0, 0x04(A0)
            LUI T2, 0x4220 //# 40.0
            MTC1 T2, F2
            SUB.S F0, F0, F2
            MFC1 T2, F0
            SW T2, 0x04(A0)
            
            LUI A0, @PLAYER_POS_1
            ORI A0, A0, @PLAYER_POS_1
            LBU T1, 0x0C(A0) //# Custom Bitfield
            ANDI T2, T1, 0x10
            BNE T2, R0, DONT_PLAY_PRESENT_STACK_SFX
            NOP
                ORI T2, T1, 0x10
                SB T2, 0x0C(A0)
                
                SW T0, 0x20(SP)
                SW T9, 0x24(SP)
                ADDI A0, R0, 0x61
                LUI A1, 0x3F80 //# SFX Speed = 1.0f
                JAL 0x8030E760 //# SFX func
                ORI A2, R0, 0x37FF
                LW T0, 0x20(SP)
                LW T9, 0x24(SP)
            DONT_PLAY_PRESENT_STACK_SFX:
        BEQ R0, R0, OBJ_LOOP_CONTINUE
        NOP
    NOT_PRESENT_STACK:

    ORI T2, R0, 0x3C7 //# Grunty Doll
    BNE T1, T2, NOT_GRUNTY_DOLL
    NOP
        //# F0 = Distance
        LUI A0, @PLAYER_POS_1
        ORI A0, A0, @PLAYER_POS_1
        JAL @fXYZ_DISTANCE
        ADDI A1, T0, 0x04 //# A0 = &obj_pos
        CVT.W.S F0, F0
        MFC1 T2, F0
        ORI T3, R0, 0xC8 //# 200
        BLT T3, T2, DONT_PLAY_GRUNTY_DOLL_SFX
        NOP
            //# Doll-Mem = Doll_Pos - Player_Pos
            LUI A0, @dDOLL_MEMORY
            ORI A0, A0, @dDOLL_MEMORY
            LUI A1, @PLAYER_POS_1
            ORI A1, A1, @PLAYER_POS_1
            JAL @fXYZ_DISTANCE_VEC
            ADDI A2, T0, 0x04 //# A0 = &obj_pos
            JAL @fNORM_VECTOR
            NOP
            LUI T2, 0x44E1 //# 1800.0
            MTC1 T2, F0
            LWC1 F2, 0x00(A0)
            LWC1 F4, 0x04(A0)
            LWC1 F6, 0x08(A0)
            MUL.S F2, F2, F0
            MUL.S F4, F4, F0
            MUL.S F6, F6, F0
            LUI T2, @PLAYER_VELOC
            ORI T2, T2, @PLAYER_VELOC
            SWC1 F2, 0x00(A0)
            SWC1 F4, 0x04(A0)
            SWC1 F6, 0x08(A0)

            SW T0, 0x20(SP)
            SW T9, 0x24(SP)

            JAL @fSET_MISC_FLAG_F
            ORI A0, R0, 0x05
            JAL @fSET_MISC_FLAG_F
            ORI A0, R0, 0x12
            JAL @fSET_MOVSTATE
            ORI A0, R0, 0x05
            
            LUI A0, @dDOLL_MEMORY
            ORI A0, A0, @dDOLL_MEMORY
            LWC1 F2, 0x00(A0)
            LWC1 F4, 0x04(A0)
            LWC1 F6, 0x08(A0)
            LUI T2, @PLAYER_VELOC
            ORI T2, T2, @PLAYER_VELOC
            SWC1 F2, 0x00(T2)
            SWC1 F4, 0x04(T2)
            SWC1 F6, 0x08(T2)

            ADDI A0, R0, 0x2D
            LUI A1, 0x3F80 //# SFX Speed = 1.0f
            JAL 0x8030E760 //# SFX func
            ORI A2, R0, 0x37FF
            LW T0, 0x20(SP)
            LW T9, 0x24(SP)
            DONT_PLAY_GRUNTY_DOLL_SFX:
        BEQ R0, R0, OBJ_LOOP_CONTINUE
        NOP
    NOT_GRUNTY_DOLL:
    NOP

    OBJ_LOOP_CONTINUE:
    ADDIU T0, T0, 0x180 //# obj size = 0x180
    BEQ R0, R0, OBJ_LOOP
    SUBI T9, T9, 0x1
OBJ_LOOP_BREAK:

LUI T0, @CUR_MOV_STATE
ORI T0, T0, @CUR_MOV_STATE
LW T0, 0x00(T0)
ORI T1, R0, 0x23 //# Taking Off
BNE T0, T1, NO_FLIGHT_TROLL
LUI T0, @PLAYER_GROUNDED
    ORI T0, T0, @PLAYER_GROUNDED
    LBU T0, 0x00(T0)
    BNE T0, R0, NO_FLIGHT_TROLL
    LUI T0, @PLAYER_VELOC
        ORI T0, T0, @PLAYER_VELOC
        LUI T1, 0x4348 //# 200.0f
        SW T1, 0x04(T0)

        LUI T0, @PLAYER_FACING_ANG
        ORI T0, T0, @PLAYER_FACING_ANG
        LWC1 F0, 0x00(T0)
        LUI T1, 0x4120 //# 10.0f
        MTC1 T1, F2
        ADD.S F0, F0, F2
        SWC1 F0, 0x00(T0)
        LUI T0, @PLAYER_MOVING_ANG
        ORI T0, T0, @PLAYER_MOVING_ANG
        SWC1 F0, 0x00(T0)

        LUI T0, @PLAYER_POS_1
        ORI T0, T0, @PLAYER_POS_1
        LBU T1, 0x0C(T0) //# Custom Bitfield
        ANDI T2, T1, 0x80 //# Bit-0
        BNE T2, R0, BIT_ZERO_IS_SET
        NOP
            ORI T1, T1, 0x80 //# Set Bit-0
            SB T1, 0x0C(T0)

            //# Stop Current Music     
            ORI A0, R0, 0x00
            JAL 0x8025A388
            ORI A1, R0, 0x4E2
            JAL 0x8025AB00
            NOP
            JAL 0x8024BD08
            ORI A0, R0, 0x00 

            ORI A0, R0, 0x55
            JAL 0x8025A6EC //# Play CoMusic
            ORI A1, R0, 0x7FFF
        BIT_ZERO_IS_SET:
        NOP
NO_FLIGHT_TROLL:
NOP

//# Fall Damage Amplification
LUI T0, @CUR_MOV_STATE
ORI T0, T0, @CUR_MOV_STATE
LW T0, 0x00(T0)
ORI T1, R0, 0x72 //# Fall Damage Recovery
BNE T0, T1, NO_FALL_DEATH
NOP
    JAL @fKILL_BANJO
    NOP
NO_FALL_DEATH:

LUI T0, @PLAYER_POS_1
ORI T0, T0, @PLAYER_POS_1
LBU T0, 0x0E(T0)
BNE T0, R0, DONT_INIT_ICE_TROLL
NOP
    LUI T0, @LAST_GROUNDED_POS
    ORI T0, T0, @LAST_GROUNDED_POS
    LWC1 F0, 0x04(T0)
    CVT.W.S F0, F0
    MFC1 T0, F0
    ORI T1, R0, 0x07D0 //# 2000
    BGT T1, T0, DONT_INIT_ICE_TROLL
    NOP
        LUI T0, @PLAYER_VELOC
        ORI T0, T0, @PLAYER_VELOC
        LWC1 F0, 0x04(T0)
        CVT.W.S F0, F0
        MFC1 T0, F0
        LUI T1, 0xFFFF
        ORI T1, T1, 0xF63C //# -2500
        BGT T0, T1, DONT_INIT_ICE_TROLL
        NOP
            LUI T0, @PLAYER_POS_1
            ORI T0, T0, @PLAYER_POS_1
            ORI T1, R0, 0x58
            SB T1, 0x0E(T0) //# set ice timer
            ADDI A0, R0, 0x112 //# Chinker Alertion
            LUI A1, 0x3F80 //# SFX Speed = 1.0f
            JAL 0x8030E760 //# SFX func
            ORI A2, R0, 0x37FF
DONT_INIT_ICE_TROLL:

LUI T0, @PLAYER_POS_1
ORI T0, T0, @PLAYER_POS_1
LBU T8, 0x0E(T0) //# ice timer
BEQ T8, R0, NO_ICE_TROLL
NOP
    SUBI T8, T8, 0x01
    SB T8, 0x0E(T0)

    ORI T5, R0, 0x9B //# 155 normal r
    ORI T6, R0, 0xCD //# 205 normal a
    BEQ T8, R0, UNFREEZE_MODE
    NOP
        FREEZE_MODE:
        ORI T5, R0, 0xFF //# ice r
        ORI T6, R0, 0xFF //# ice a
    UNFREEZE_MODE:
    LUI T0, @TMAP_MODEL_POINTER
    ORI T0, T0, @TMAP_MODEL_POINTER
    LW T0, 0x00(T0) //# T0 = TMapModel
    LW T1, 0x10(T0) //# T1 = VertexStorageSetup Offset
    LHU T2, 0x32(T0) //# T2 = VertexCount
    ADD T1, T1, T0 //# T1 = VertexStorageSetup
    ADDI T1, T1, 0x18 //# T1 = VertexStorageList Offset
        VERTEX_LOOP_A:
        LBU T3, 0x0D(T1) //# T3 = Green Vertex Shade Byte
        ORI T4, R0, 0xFA //# 250
        BNE T3, T4, NON_WATER_VERTEX
        NOP
            SB T5, 0x0C(T1)
            SB T6, 0x0F(T1)
        NON_WATER_VERTEX:
        ADDI T1, T1, 0x10
        SUBI T2, T2, 0x01
        BEQ T2, R0, VERTEX_LOOP_A_END
        NOP
        BEQ R0, R0, VERTEX_LOOP_A
        NOP
    VERTEX_LOOP_A_END:

    LUI T0, @TMAP_MODEL_POINTER
    ORI T0, T0, @TMAP_MODEL_POINTER
    LW T0, 0x00(T0) //# T0 = TMapModel
    LW T1, 0x1C(T0) //# T1 = CollisionSetup Offset
    ADD T0, T0, T1
    LHU T1, 0x10(T0) //# Read Special-Offset Val
    SLL T1, T1, 0x2  //# Special-Offset *= 4
    ADDU T0, T0, T1
    ADDIU T0, T0, 0x18 //# const 0x18 offset
    LUI T2, 0x0003 //# T2 = water collision
    LUI T3, 0x8800
    ORI T3, T3, 0x0001 //# T3 = ground coll with an extra +1
        TRI_LOOP_A:
        LW T4, 0x00(T0)
        BEQ T4, R0, TRI_LOOP_A_END
        NOP
        BEQ T8, R0, UNFREEZING_THE_ICE
        NOP
            FREEZING_THE_ICE:
            BNE T4, T2, TRI_LOOP_A_INCREMENT
            NOP
                BEQ R0, R0, TRI_LOOP_A_INCREMENT
                SW T3, 0x00(T0)
            UNFREEZING_THE_ICE:
            BNE T4, T3, TRI_LOOP_A_INCREMENT
            NOP
                SW T2, 0x00(T0)
        TRI_LOOP_A_INCREMENT:
        ADDI T0, T0, 0x04
        BEQ R0, R0, TRI_LOOP_A
        NOP
    TRI_LOOP_A_END:
    ORI T0, R0, 0x01
    BNE T1, T0, NO_ICE_BREAKING_SFX
    NOP
        ADDI A0, R0, 0x7B
        LUI A1, 0x3F80 //# SFX Speed = 1.0f
        JAL 0x8030E760 //# SFX func
        ORI A2, R0, 0x37FF
    NO_ICE_BREAKING_SFX:
    NOP
NO_ICE_TROLL:
NOP

//# load &player.pos
LUI A0, @PLAYER_POS_1
ORI A0, A0, @PLAYER_POS_1
//# load &min_bound
LUI A1, @dWIND
ORI A1, A1, @dWIND
//# load &max_bound
//# perform box-check: V0 = *A0 in [*A1:*A2]
JAL 0x802584FC
ADDI A2, A1, 0x0C
BEQ V0, R0, NO_WIND_EFFECT
NOP
    LUI T0, @PLAYER_VELOC
    ORI T0, T0, @PLAYER_VELOC
    LWC1 F0, 0x00(T0)
    LUI T1, 0x45A8 //# 180.0f * 30 ~= 5376.0
    MTC1 T1, F2
    LUI T1, @GAME_SPEED_MODIF
    ORI T1, T1, @GAME_SPEED_MODIF
    LWC1 F4, 0x00(T1)
    MUL.S F2, F2, F4
    ADD.S F0, F0, F2
    SWC1 F0, 0x00(T0)
NO_WIND_EFFECT:

LUI T0, @FLOOR_OBJ_POINTER
ORI T0, T0, @FLOOR_OBJ_POINTER
LW T0, 0x00(T0)
LW T0, 0x0C(T0)
LUI T5, 0x8800
ORI T5, T5, 0x0800
BNE T0, T5, DONT_INIT_QUICKSAND_TROLL
LUI T0, @PLAYER_GROUNDED
    ORI T0, T0, @PLAYER_GROUNDED
    LBU T0, 0x00(T0)
    BEQ T0, R0, DONT_INIT_QUICKSAND_TROLL
    NOP
        ADDI A0, R0, 0xEA
        LUI A1, 0x3F80 //# SFX Speed = 1.0f
        JAL 0x8030E760 //# SFX func
        ORI A2, R0, 0x37FF
        
        //# add a death to the count
        LUI T0, @PLAYER_POS_2
        ORI T0, T0, @PLAYER_POS_2
        ORI T1, R0, 0x78 //# 4s
        SB T1, 0x0C(T0)
        LBU T1, 0x0D(T0) //# Actual Death Coutner
        ADDIU T1, T1, 0x01
        SB T1, 0x0D(T0)
        
        LUI T0, @PLAYER_POS_1
        ORI T0, T0, @PLAYER_POS_1
        LBU T1, 0x0C(T0) //# Custom Bitfield
        ORI T1, T1, 0x40 //# Set Bit-1
        SB T1, 0x0C(T0)

        ORI T1, R0, 0x70 //# the higher the longer it takes to sink
        SB T1, 0x0F(T0) //# sand timer
        LUI T0, @FLOOR_OBJ_POINTER
        ORI T0, T0, @FLOOR_OBJ_POINTER
        LW T0, 0x00(T0)
        //# comparison values
        LW T5, 0x04(T0)
        LW T6, 0x08(T0)
        LW T7, 0x0C(T0)
        LUI T0, @MAP_MODEL_POINTER
        ORI T0, T0, @MAP_MODEL_POINTER
        LW T0, 0x00(T0) //# T0 = MapModel
        LW T1, 0x1C(T0) //# T1 = CollisionSetup Offset
        ADDU T0, T0, T1
        LHU T1, 0x10(T0) //# Read Special-Offset Val
        SLL T1, T1, 0x2  //# Special-Offset *= 4
        ADDU T0, T0, T1
        ADDIU T0, T0, 0x18 //# const 0x18 offset
                TRI_LOOP_B:
                LW T4, 0x08(T0)
                BEQ T4, R0, TRI_LOOP_B_END
                LW T4, 0x00(T0)
                BNE T4, T5, TRI_LOOP_B_INCREMENT
                LW T4, 0x04(T0)
                BNE T4, T6, TRI_LOOP_B_INCREMENT
                LW T4, 0x08(T0)
                BNE T4, T7, TRI_LOOP_B_INCREMENT
                NOP
                    SW R0, 0x00(T0)
                    SW R0, 0x04(T0)
                    SW R0, 0x08(T0)
                TRI_LOOP_B_INCREMENT:
                BEQ R0, R0, TRI_LOOP_B
                ADDIU T0, T0, 0x0C
        TRI_LOOP_B_END:
        NOP
DONT_INIT_QUICKSAND_TROLL:

LUI T0, @PLAYER_POS_1
ORI T0, T0, @PLAYER_POS_1
LBU T1, 0x0F(T0) //# sand timer
BEQ T1, R0, NO_QUICKSAND_TROLL
NOP
    SUBI T1, T1, 0x01
    SB T1, 0x0F(T0)
    ORI T2, R0, 0x1B //# time to complete Deathwarp
    BNE T1, T2, QUICKSAND_DEATH_SKIP
    NOP
        ADDI A0, R0, 0x85
        LUI A1, 0x3F80 //# SFX Speed = 1.0f
        JAL 0x8030E760 //# SFX func
        ORI A2, R0, 0x37FF

        JAL 0x8029B62C
        NOP
    QUICKSAND_DEATH_SKIP:
    JAL @fSET_MOVSTATE
    ORI A0, R0, 0x07 //# Crouching
    LUI T0, @PLAYER_POS_1
    ORI T0, T0, @PLAYER_POS_1
    LWC1 F0, 0x14(T0)
    LUI T1, 0x3FE0 //# +2.0 (gravity is -3)
    MTC1 T1, F2
    ADD.S F0, F0, F2
    SWC1 F0, 0x04(T0)
    SWC1 F0, 0x14(T0)
    LUI T0, @PLAYER_VELOC
    ORI T0, T0, @PLAYER_VELOC
    SW R0, 0x00(T0)
    SW R0, 0x04(T0)
    SW R0, 0x08(T0)
    LUI T0, @JOYSTICK_X
    ORI T0, T0, @JOYSTICK_X
    SH R0, 0x00(T0) //# also affecting joy_y
NO_QUICKSAND_TROLL:

//# load &player.pos
LUI A0, @PLAYER_POS_1
ORI A0, A0, @PLAYER_POS_1
//# load &min_bound
LUI A1, @dTROTLESS_ZONE
ORI A1, A1, @dTROTLESS_ZONE
//# load &max_bound
//# perform box-check: V0 = *A0 in [*A1:*A2]
JAL 0x802584FC
ADDI A2, A1, 0x0C
BEQ V0, R0, NO_TROTLESS_ZONE_EFFECT
NOP
    JAL @fIS_ABOVE_FLOOR
    NOP
    BEQ V0, R0, NO_TROTLESS_ZONE_EFFECT
        LUI T0, @CUR_MOV_STATE
        ORI T0, T0, @CUR_MOV_STATE
        LW T0, 0x00(T0)
        ORI T1, R0, 0x08 //# TT Jumping
        BEQ T0, T1, TROTLESS_ZONE_EFFECT
        ORI T1, R0, 0x15 //# TT Idle
        BEQ T0, T1, TROTLESS_ZONE_EFFECT
        ORI T1, R0, 0x16 //# TT Walking
        BEQ T0, T1, TROTLESS_ZONE_EFFECT
        ORI T1, R0, 0x45 //# TT Sliding
        BEQ T0, T1, TROTLESS_ZONE_EFFECT
        NOP
        BEQ R0, R0, NO_TROTLESS_ZONE_EFFECT
        NOP
            TROTLESS_ZONE_EFFECT:
            JAL @fSET_MOVSTATE
            ORI A0, R0, 0x17 //# TT Exit
NO_TROTLESS_ZONE_EFFECT:

LUI T0, @CUR_MOV_STATE
ORI T0, T0, @CUR_MOV_STATE
LW T0, 0x00(T0)
ORI T1, R0, 0x21 //# Shockspring Charge
BEQ T0, T1, SHOCKSPRING_FORCE
    LUI T0, @CHARGING_SSPRING
    ORI T0, T0, @CHARGING_SSPRING
    LBU T0, 0x00(T0)
    BEQ T0, R0, NO_SHOCKSPRING_FORCE
    NOP
        SHOCKSPRING_FORCE:
        LUI T0, @BUTTON_ARR_1
        ORI T0, T0, @BUTTON_ARR_1
        LBU T1, 0x00(T0)
        ORI T1, T1, 0x80 //# Force A-Held
        SB T1, 0x00(T0)
NO_SHOCKSPRING_FORCE:

LUI T0, @PLAYER_POS_1
ORI T0, T0, @PLAYER_POS_1
LBU T0, 0x0D(T0)
BNE T0, R0, NO_TOLL_CHECKS
NOP
    LUI T0, @PARTICLE_ARR
    ORI T0, T0, @PARTICLE_ARR
    LUI T3, @dTOLL_OBJ
    ORI T3, T3, @dTOLL_OBJ
    ORI T5, R0, 0x48 //# Particle Size
    PARTICLE_LOOP:
    LBU T1, 0x44(T0) //# Is_Alive ?
    BEQ T1, R0, PARTICLE_LOOP_END
    NOP
        //# Pointer DeRef #1
        LW T1, 0x34(T0)
        //# Grab the 1st Pointer
        LW T1, 0x00(T1)
        //# Compare with Egg Pointer
        LUI T2, 0x8037
        ORI T2, T2, 0x26A0
        BNE T1, T2, PARTICLE_NOT_EGG
        NOP
            //# Double DeRef this time
            LW T1, 0x30(T0)
            LW T1, 0x00(T1)

            LH T2, 0x04(T1) //# egg X
            MTC1 T2, F0
            CVT.S.W F0, F0
            SWC1 F0, 0x0C(T3)
            LH T2, 0x06(T1) //# egg Y
            MTC1 T2, F0
            CVT.S.W F0, F0
            SWC1 F0, 0x10(T3)
            LH T2, 0x08(T1) //# egg Z
            MTC1 T2, F0
            CVT.S.W F0, F0
            SWC1 F0, 0x14(T3)

            OR A0, R0, T3
            JAL @fXYZ_DISTANCE
            ADDIU A1, A0, 0x0C

            CVT.W.S F0, F0
            MFC1 T1, F0
            ORI T2, R0, 0x64
            BLT T2, T1, EGG_TOO_FAR_AWAY
            NOP
                LUI T1, 0x4100 //# High enough to End an Egg
                SW T1, 0x04(T0)

                LUI T0, @PLAYER_POS_1
                ORI T0, T0, @PLAYER_POS_1
                ORI T1, R0, 0x3C //# 2s Hopefully
                SB T1, 0x0D(T0)

                JAL @fSET_MOVSTATE
                ORI A0, R0, 0x73

                ORI A0, R0, 0x42
                JAL 0x8025A6EC //# Play CoMusic
                ORI A1, R0, 0x5FFF

                BEQ R0, R0, PARTICLE_LOOP_END
                NOP
            EGG_TOO_FAR_AWAY:
            NOP
        PARTICLE_NOT_EGG:
        ADDU T0, T0, T5
        BEQ R0, R0, PARTICLE_LOOP
        NOP
    PARTICLE_LOOP_END:
    NOP
NO_TOLL_CHECKS:

LUI T0, @PLAYER_POS_1
ORI T0, T0, @PLAYER_POS_1
LBU T1, 0x0D(T0)
BEQ T1, R0, NO_WALL_MOVEMENT
NOP
    SUBI T1, T1, 0x01
    SB T1, 0x0D(T0)

    ORI T2, R0, 0x1E //# 1s
    BNE T1, T2, DONT_LOCK_PLAYER_AGAIN
    NOP
        JAL @fSET_MOVSTATE
        ORI A0, R0, 0x73
    DONT_LOCK_PLAYER_AGAIN:
    BLT T2, T1, NO_WALL_MOVEMENT
    NOP
        MTC1 T1, F0
        MTC1 T2, F2
        CVT.S.W F0, F0
        CVT.S.W F2, F2
        DIV.S F0, F0, F2

        LUI T0, @dWALL_RANGE
        ORI T0, T0, @dWALL_RANGE
        LH T1, 0x00(T0) //# start
        LH T2, 0x02(T0) //# end
        SUB T3, T2, T1  //# diff = (end - start)

        MTC1 T3, F2
        CVT.S.W F2, F2
        MUL.S F0, F0, F2
        NOP
        CVT.W.S F0, F0
        MFC1 T1, F0

        SUB T9, T2, T1 //# new X = end - diff*ratio
        //#SH T9, 0x04(T0)

        LUI T0, @MAP_MODEL_POINTER
        ORI T0, T0, @MAP_MODEL_POINTER
        LW T0, 0x00(T0) //# T0 = TMapModel
        LW T1, 0x10(T0) //# T1 = VertexStorageSetup Offset
        LHU T2, 0x32(T0) //# T2 = VertexCount
        ADD T1, T1, T0 //# T1 = VertexStorageSetup
        ADDI T1, T1, 0x18 //# T1 = VertexStorageList Offset
            VERTEX_LOOP_B:
            LBU T3, 0x0E(T1) //# T3 = Blue Vertex Shade Byte
            ORI T4, R0, 0xFD
            BNE T3, T4, NON_MOVWALL_VERTEX_B
            NOP
                //# Set new X
                SH T9, 0x00(T1)
            NON_MOVWALL_VERTEX_B:
            ADDI T1, T1, 0x10
            SUBI T2, T2, 0x01
            BEQ T2, R0, VERTEX_LOOP_B_END
            NOP
            BEQ R0, R0, VERTEX_LOOP_B
            NOP
        VERTEX_LOOP_B_END:
        NOP
NO_WALL_MOVEMENT:

LUI T0, @MAP_ID
ORI T0, T0, @MAP_ID
LBU T0, 0x00(T0)
ORI T1, R0, 0x0C //# Tickers Tower / Church
BNE T0, T1, NO_FINAL_MESSAGE
NOP
    LUI T0, @TEXTPOINTER
    ORI T0, T0, @TEXTPOINTER
    LW T0, 0x00(T0)
    BEQ T0, R0, NO_FINAL_MESSAGE
    NOP
        LBU T1, 0x00(T0)
        ORI T2, R0, 0x4F //# 'O'
        BNE T1, T2, NO_FINAL_MESSAGE
        NOP
            LUI T1, @PLAYER_POS_2
            ORI T1, T1, @PLAYER_POS_2
            LBU A0, 0x0D(T1)

            ORI A1, R0, 0x64
            DIV A0, A1
            MFLO T1
            ADDIU T1, T1, 0x30
            SB T1, 0x38(T0)
            
            JAL @fMODULO
            NOP
            OR A0, A2, R0
            ORI A1, R0, 0x0A
            DIV A0, A1
            MFLO T1
            ADDIU T1, T1, 0x30
            SB T1, 0x39(T0)
            
            JAL @fMODULO
            NOP
            OR A0, A2, R0
            ADDIU T1, A0, 0x30
            SB T1, 0x3A(T0)
NO_FINAL_MESSAGE:
NOP

RETURN:
//# restore regs
LW RA, 0x14(SP)
J @HIJACK_RA
ADDIU SP, SP, 0x40
//#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.org 0x11B0 //# Lookup Table for Note-Keys
                //# RAM Loc: 0x80478000
[tQUICK]: 0x80478000
.byte 0x51, 0x55, 0x49, 0x43, 0x4b, 0x20, 0x21, 0x00
[tBUTTONS]: 0x80478008
.byte 0x50, 0x20, 0x20, 0x45, 0x20, 0x20, 0x53, 0x00
[tTIMER]: 0x80478010
.byte 0x30, 0x00, 0x00, 0x00
[dWIND]: 0x80478014
.word 0xC43E0000
.word 0x42B40000
.word 0x44A10000 //# 1288.0
.word 0x00000000
.word 0x439B0000
.word 0x44C30000 //# 1560.0
[dTROTLESS_ZONE]: 0x8047802C
.word 0xC528C000 //# -2700.0
.word 0x43C80000 //# +400.0
.word 0xC5480000 //# -3200.0
.word 0xC4610000 //# -900.0
.word 0x44BB8000 //# +1500.0
.word 0xC4D48000 //# -1700.0
[dTOLL_OBJ]: 0x80478044
.word 0x4403C000
.word 0x43C00000
.word 0x4584B000 //# 4246
.word 0x00000000
.word 0x00000000
.word 0x00000000
[dWALL_RANGE]: 0x8047805C
.halfword 0x020E //# 526
.halfword 0x029E //# 670
[dDOLL_MEMORY]: 0x80478060
.word 0x00000000
.word 0x00000000
.word 0x00000000
[dDEATH_COUNTER]: 0x8047806C
.word 0x00000000
[tMESSAGE_1]: 0x80478070
.byte 0x42, 0x41, 0x4e, 0x4a, 0x4f, 0x2d, 0x4b, 0x41
.byte 0x5a, 0x4f, 0x4f, 0x49, 0x45, 0x00, 0x00, 0x00
[tMESSAGE_2]: 0x80478080
.byte 0x46, 0x4f, 0x52, 0x54, 0x20, 0x46, 0x55, 0x4e
.byte 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
[dRESPAWN_LOC]: 0x80478090
.word 0xC484A000
.word 0x42CA0000
.word 0x00000000


//#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.org 0x01270 //# Custom Funcs
                //# RAM Loc: 0x80479000
//#=============================================================
//# calling this will kill Banjo
//#=============================================================
[fKILL_BANJO]: 0x80479000
ADDIU SP, SP, 0xFFC0
SW RA, 0x04(SP)
SW T0, 0x08(SP)
SW T1, 0x0c(SP)
SW T2, 0x10(SP)
SW T3, 0x14(SP)
SW T4, 0x18(SP)
SW T5, 0x1c(SP)
SW T6, 0x20(SP)
SW T7, 0x24(SP)
SW T8, 0x28(SP)
SW T9, 0x2c(SP)
    LUI T0, @PLAYER_HEALTH
    ORI T0, T0, @PLAYER_HEALTH
    SW R0, 0x00(T0)
    JAL @fSET_MOVSTATE
    ORI A0, R0, 0x41
LW RA, 0x04(SP)
LW T0, 0x08(SP)
LW T1, 0x0c(SP)
LW T2, 0x10(SP)
LW T3, 0x14(SP)
LW T4, 0x18(SP)
LW T5, 0x1c(SP)
LW T6, 0x20(SP)
LW T7, 0x24(SP)
LW T8, 0x28(SP)
LW T9, 0x2c(SP)
JR RA
ADDIU SP, SP, 0x40
//#=============================================================
//# V0 = random char out of A B Z L R
//#=============================================================
[fGET_RANDOM_BUTTON]: 0x80479078
ADDIU SP, SP, 0xFFC0
SW RA, 0x04(SP)
    JAL 0x8034A390
    NOP
    ORI T1, R0, 0x05
    DIV V0, T1
    MFHI T1
    ORI T2, R0, 0x00
    BEQ T1, T2, GET_RANDOM_BUTTON_END
    ORI V0, R0, 0x41 //# 'A'
    ORI T2, R0, 0x01
    BEQ T1, T2, GET_RANDOM_BUTTON_END
    ORI V0, R0, 0x42 //# 'B'
    ORI T2, R0, 0x02
    BEQ T1, T2, GET_RANDOM_BUTTON_END
    ORI V0, R0, 0x5A //# 'Z'
    ORI T2, R0, 0x03
    BEQ T1, T2, GET_RANDOM_BUTTON_END
    ORI V0, R0, 0x4C //# 'L'
    ORI V0, R0, 0x52 //# 'R'
    GET_RANDOM_BUTTON_END:
LW RA, 0x04(SP)
JR RA
ADDIU SP, SP, 0x40
//#==========================================
//# V0 = VShade of current floor tri
//# A0 = Which Shade ?
//# C = Red, D = Green, E = Blue, F = Alpha
//#==========================================
[fCHECK_V1_SHADE_TRI]: 0x804790D4

LUI T0, @MAP_MODEL_POINTER
LW T0, @MAP_MODEL_POINTER(T0) //# T0 = MapModel
LW T1, 0x10(T0) //# T1 = VertexStorageSetup Offset
ADDU T0, T0, T1 //# T0 = VertexStorageSetup
ADDIU T0, T0, 0x18 //# T0 = VertexStorageList

LUI T1, @FLOOR_OBJ_POINTER
ORI T1, T1, @FLOOR_OBJ_POINTER
LW T1, 0x00(T1) //# T1 = FloorObj

LHU T2, 0x04(T1) //# holds Tri-Vertex ID-1 now
SLL T2, T2, 0x04 //# Tri-Vertex ID-1 * 0x10
ADDU T2, T0, T2 //# THE vertex address
ADDU T2, T2, A0
LBU V0, 0x00(T2) //# VShade of Vertex

LUI T3, @PLAYER_POS_1
ORI T3, T3, @PLAYER_POS_1
SW T2, 0x0C(T3)

LHU T2, 0x06(T1) //# holds Tri-Vertex ID-2 now
SLL T2, T2, 0x04 //# Tri-Vertex ID-2 * 0x10
ADDU T2, T0, T2 //# THE vertex address
ADDU T2, T2, A0
LBU T2, 0x00(T2) //# Red VShade of Vertex
BNE T2, V0, RETURN_FROM_VSHADE_CHECK
NOP

LHU T2, 0x08(T1) //# holds Tri-Vertex ID-3 now
SLL T2, T2, 0x04 //# Tri-Vertex ID-3 * 0x10
ADDU T2, T0, T2 //# THE vertex address
ADDU T2, T2, A0
LBU T2, 0x00(T2) //# Red VShade of Vertex
BNE T2, V0, RETURN_FROM_VSHADE_CHECK
NOP

JR RA
NOP

RETURN_FROM_VSHADE_CHECK:
JR RA
ORI V0, R0, 0x00
//#==========================================
//# V0 = Is Banjo above a Floor ?
//#==========================================
[fIS_ABOVE_FLOOR]: 0x8047915C

LUI V0, @sFLOOR_OBJ_POINTER
LW V0, @sFLOOR_OBJ_POINTER(V0)
JR RA
LW V0, 0x00(V0)
//#==========================================
//# A0 = X, A1 = Y
//# A2 = X mod Y
//#==========================================
[fMODULO]: 0x8047916C

SUBI A3, A1, 0x01
BLT A0, A1, MODULO_END
OR A2, A0, R0
MODULO:
    SUBU A2, A2, A1
BLT A3, A2, MODULO
NOP
MODULO_END:
JR RA
NOP
//#==========================================

//#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%