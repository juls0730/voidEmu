; kitchen sink for instructions
_start:
    exit 0x00 ; 001N
    cls ; 00E0
    ret ; 00EE
    sys 0x00 ; 0NNN
    jp 0x123 ; 1NNN
    jp v0, 0x123 ; BNNN
    call foo ; 2NNN
    se V1, 0xAA ; 3xkk
    sne V2, 0xAA ; 4xkk
    se V1, V2 ; 5xy0
    sne V1, V2 ; 9xy0
    add v1, 0x12 ; 7xkk
    add v1, v2 ; 8xy4
    add I, V8 ; Fx1E
    or V2, v3 ; 8xy1
    and VA, vb ; 8xy2
    xor VA, vb ; 8xy3
    sub v1, v2 ; 8xy5
    shr VA, vb ; 8xy6 shifts VA right by vb
    shr VA ; 8xx6
    subn VA, vb ; 8xy7
    shl VA, vb ; 8xyE
    shl VA ; 8xxE
    rnd VD, 0xFF ; Cxkk
    drw VE, VF, 0x4 ; Dxxy
    skp VE ; Ex9E
    sknp VA ; ExA1
    ld V1, 0xAA ; 6xkk
    ld V2, v3 ; 8xy0
    ld I, 0xAA ; ANNN
    ld v2, DT ; Fx07
    ld v2, K ; Fx0A
    ld DT, V5 ; Fx15
    ld ST, V5 ; Fx18
    ld F, V5 ; Fx29
    ld B, V5 ; Fx33
    ld [I], VA ; Fx55
    ld VA, [I] ; Fx65

foo:
    add v8, v9
    ret