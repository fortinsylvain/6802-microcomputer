;***********************************************************
; Programme de gestion de micro board
;
; Titre: SYLS01
;
; Date creation originale : 23 janvier 1992
; Retape                  : 13 mars 2022
;***********************************************************
; Adress Map
; ----------
; 0000-007F SRAM in the 6802
; 1000-17FF SRAM External
; A000-A003 PIA for 7 segments display
; B000-B003 PIA for keyboard
; C000-C001 ASCIA
; F000-FFFF EEPROM

; PIA for seven segment display
PIAAFFDA = $a000
PIAAFFCA = $a001
; PA7 <9>
; PA6 <8>
; PA5 <7>
; PA4 <6>
; PA3 <5>
; PA2 <4>
; PA1 <3>
; PA0 <2>
PIAAFFDB = $a002
PIAAFFCB = $a003 
; PB7 <17>
; PB6 <16>
; PB5 <15>
; PB4 <14>
; PB3 <13>
; PB2 <12>
; PB1 <11>
; PB0 <10>

; PIA for keyboard
PIAKEYDA = $b000
PIAKEYCA = $b001
; PA7 <9>
; PA6 <8>
; PA5 <7>
; PA4 <6>
; PA3 <5>
; PA2 <4>
; PA1 <3>
; PA0 <2>
PIAKEYDB = $b002
PIAKEYCB = $b003
; PB7 <17>  N/C
; PB6 <16>  N/C
; PB5 <15>  N/C
; PB4 <14>  N/C
; PB3 <13>  N/C
; PB2 <12>  N/C
; PB1 <11>  N/C
; PB0 <10>  BEEPER 1:ON, 0:OFF

; ASCIA
ACIA     = $c000  ; base address
ACIAC    = ACIA   ; control register
ACIAS    = ACIA   ; status register
ACIAD    = ACIA+1 ; data register

ACIA_TDRE = $02 
ACIA_RDRF = $01 

C_CR = $0d
C_LF = $0a
; 


            dsect    ; Starts an offset-section which does not generate any code in the output file
                     ; Variables stored in RAM
            org     $1700         
STACKTOP    db 0                 ; top of the stack pointer
HEXDISP     db 0, 0, 0, 0, 0, 0  ; Hex values for the six display, HEXDISP+0 (right most), HEXDISP+5 (left most)
SEGDISP     db 0, 0, 0, 0, 0, 0  ; Segments values for the six display, SEGDISP+0 (right most), SEGDISP+5 (left most)
ADHEXAFF    dw 0
ADSEGAFF    dw 0
PNTABCNV    dw 0
ADDAFF      dw 0
NBKEY       db 0
RUNADD      dw 0

;SERBUF      
;SEBUFWRA    db 0                 ; serial buffer write adress
;SEBUFRDA    db 0                 ; serial buffer read adress
;SEADDDIF    db 0

            dend              ; end an offset-section

            ;section code
            ;.text .section ".text","acrx"
            org   $f000
RSTVECT:    ldaa  #$00        ; Reg. direction
            staa  PIAKEYCA
            staa  PIAKEYCB
            staa  PIAAFFCA
            staa  PIAAFFCB
            staa  PIAKEYDA    ; P.A Key en entree
            ldaa  #$ff        ; 1=sortie  0=entree
            staa  PIAAFFDA
            staa  PIAAFFDB    ; A&B AFF. EN SORTIE
            staa  PIAKEYDB    ; B en sortie
            ldaa  #$04        ; reg, donne
            staa  PIAKEYCA
            staa  PIAKEYCB
            staa  PIAAFFCA
            staa  PIAAFFCB

            lds   #STACKTOP   ; Initialize stack pointer

            ; -------------------
            ; Programme principal
            ; -------------------
            ldx   #VERSCOD    ; Affiche la version
            jsr   MESSAGE     ; du SYLS.
            jsr   BEEP

            jsr   ASCIAINI    ; Init of serial link
            
            cli               ; Clear Interrupt Mask to enable interrupt

            ldx   #S_CRLF
            jsr   PRINTSTR
            ldx   #S_SYLS01
            jsr   PRNTSTR2
            ldx   #S_VERS
            jsr   PRNTSTR2

            jsr   WKEYON      ; attend un touche
            jsr   BEEP
NEWADD:     clr   NBKEY       ; key enfoncees = 0

            ldaa  #$11        ; remplis les buffers
            ldx   #HEXDISP    ; affich. de " - ".
BOUCLEF:    staa  $0,X
            inx 
            cpx   #SEGDISP    ; dernier afficheur?
            bne   BOUCLEF
            jsr   CONVTOUT    ; conversion buffers
            jsr   AFFICHE
            jsr   WKEYOFF

NEXTKEY:    jsr   WKEYON      ; KEY HEX dans reg A

            cmpa  #$10        ; touche fonction ?
            bge   FCTKEY      ; si oui on saute
            ; Traitement des touches (0-F)
            ; ---------------------------
            anda  #$0f        ; (0-F) hex key only
            inc   NBKEY       ; KEY=KEY+1

            ldab  NBKEY       ; check key pressed
            cmpb  #$06
            ble   SAUTA       ; Plus que 6 key ?
            jsr   ALERTE      ; Averti l<usager
            jsr   WKEYOFF
            bra   NEXTKEY
                                ; Key (HEX) REG. A
SAUTA:      ldx   #SEGDISP    ; calcul ou stocker
            stx   ADHEXAFF    ; la key (HEX)
            ldab  ADHEXAFF+1
            subb  NBKEY       ; SEGDISP-(Nombre de KEY)
            stab  ADHEXAFF+1  ; store LSB destination
            ldx   ADHEXAFF
            staa  $0,X        ; Stock HEX value Key
            jsr   CONVERTI    ; trouve eq. code AFF. resultat dans reg. B
            ldx   #ADHEXAFF   ; calcul ou stocker
            stx   ADSEGAFF    ; le code d'aff
            ldaa  ADSEGAFF+1
            suba  NBKEY       ; ADHEXAFF-(Nombre de key)
            staa  ADSEGAFF+1  ; LSB destination
            ldx   ADSEGAFF
            stab  $0,X        ; Stock code aff
            ldaa  NBKEY       ; Check si 4 ieme KEY?
            cmpa  #$04
            bne   SAUTB
            jsr   FORMEADD    ; Forme l'adresse ADDAFF(16 bits)
            jsr   LIS         ; Lis l'adresse
SAUTB:      jsr   AFFICHE     ; Envoie aux afficheux
            jsr   WKEYOFF
            bra   NEXTKEY     ; retourne en haut

            ; Traitement des touches de fonction
FCTKEY:     nop 
            cmpa  #$10
            beq   FCTKEYA 
            cmpa  #$11
            beq   FCTKEYB
            cmpa  #$12
            beq   FCTKEYC 
            cmpa  #$13
            beq   FCTKEYD 
            cmpa  #$15 
            beq   FCTKEYE
            CMPA  #$16
            beq   FCTKEYF 
            cmpa  #$17 
            beq   FCTKEYG 

            jsr   ALERTE      ; erreur de touche inconnue
            jsr   WKEYOFF
            jmp   NEXTKEY

FCTKEYA:    jsr   WKEYOFF     ; New Address key
            jmp   NEWADD      ; ---------------

FCTKEYB:    ldx   ADDAFF
            inx
            stx   ADDAFF
            bra   SAUTC       ; Decomp, lis, conv et affiche

FCTKEYC:    ldx   ADDAFF
            dex 
            stx   ADDAFF
            bra   SAUTC       ; Decomp, lis, conv et affiche

FCTKEYD:    ldx   #LOADCOD            
            jsr   MESSAGE
            jsr   BEEP
            jsr   DELAYA 
            jsr   BEEP
            jmp   LOAD
            jmp   FCTEND

FCTKEYE:    ldx   #RUNCOD     ; Touche EXEC
            jsr   MESSAGE     ; Affiche (RUN.)
            ldx   RUNADD
            jmp   $0,X        ; Saute a l'adresse indique par RUNADD(16bits)

FCTKEYF:    nop                 ; Touche ENTER
            ldaa  HEXDISP+1
            clc 
            rola
            rola
            rola
            rola
            adda  HEXDISP
            ldx   ADDAFF
            staa  $0,X        ; Store donee to memory
            inx                 ; ADD=ADD+1
            stx   ADDAFF
            jsr   BEEP 
SAUTC:      jsr   DECOMPOS 
            jsr   LIS
            jsr   CONVTOUT
            jsr   AFFICHE
            ldaa  #$04
            staa  NBKEY       ; Key enfonce = 4
            jmp   FCTEND

FCTKEYG:    ldx   #NULCOD
            jsr   MESSAGE
            jmp   FCTEND

FCTEND:     jsr   WKEYOFF
            jmp   NEXTKEY

ROND:       jmp   ROND        ; Sleep for ever

            ; ---------
            ; load
            ; ---------
            ;org   $f700
LOAD:       ldaa  #$03
            staa  $c000
            ldaa  #$19
            staa  $c000
            ldx   #$1000
            stx   $17f9
LOAD1:      jsr   LOAD1

            ; ----------
            ; subroutine
            ; ----------
            ;org   $f800
            ; sous-programme affiche
            ; Transfert les codes segments stocke dans les six SEGDISP
            ; aux afficheurs respectifs.
AFFICHE:    ldx   #SEGDISP+5  ; source donne
            ldab  #$04
            clc
BOUCLEA:    ldaa  $0,X        ; prend donne
            staa  PIAAFFDA    ; a l'affichage
            stab  PIAAFFDB    ; select afficheur
            clr   PIAAFFDB    ; impulsion chargement
            dex
            rolb
            bcc   BOUCLEA     ; termine ?
            clr   PIAAFFDA
            clr   PIAAFFDB
            rts 
            ; SOUS-PROG. de mise en forme d<une address decomposee.
            ; le present ramasse les 4 codes hex stocke pour les afficheurs
            ; et forme l<addresse correspondante et ira la porter
            ; en ADDAFF(16bits)

FORMEADD:   clc
            ldaa  HEXDISP+5   ; FORME MSB
            rola
            rola
            rola
            rola
            adda  HEXDISP+4
            staa  ADDAFF      ; store MSB
            ldab  HEXDISP+3   ; forme lsb
            rolb
            rolb
            rolb
            rolb
            addb  HEXDISP+2
            stab  ADDAFF+1    ; store lsb
            rts
            ; sous routine qui decpmpose l'adress ADDAFF(16 bits)
            ; retourne les 4 bases en hex l'addresse dans
            ; les buffers respectifs des afficheurs
DECOMPOS:   ldaa  ADDAFF      ; MSB addresse
            tab
            anda  #$f0
            clc
            rora
            rora
            rora
            rora
            staa  HEXDISP+5
            andb  #$0f
            stab  HEXDISP+4
            ldaa  ADDAFF+1   ; LSB addresse
            tab
            anda  #$f0
            clc 
            rora
            rora
            rora
            rora
            staa  HEXDISP+3
            andb  #$0f
            stab  HEXDISP+2
            rts
            ; sous-programme de lecture d<une cellule (converti+stocke)
            ; prend le contenu addresse par ADDAFF(16bits) et le scinde
            ; pourstker sa valeur hex et equivalente (code aff.)
            ; su buffer ram respectif des afficheurs.
LIS:        ldx   ADDAFF      ; lis adresse
            ldaa  $0,X        ; donne pointee
            anda  #$f0        ; mask
            clc
            rora
            rora
            rora
            rora
            staa  HEXDISP+1   ; stocke hex data
            jsr   CONVERTI    ; trouve cod aff eq
                                ; resultat en reg. b
            stab  SEGDISP+1   ; stocke cod aff
            ldx   ADDAFF      ; relis l'addresse
            ldaa  $0,X
            anda  #$0f        ; mask
            staa  HEXDISP     ; store hex data
            jsr   CONVERTI    ; trouve cod add eq
                                ; resultat en reg. b
            stab  SEGDISP     ; store cod aff
            rts

            ; sous programme pour generer un court delay (entre bip).
DELAYA:     ldx   #$600
PAUSEA:     dex     
            bne   PAUSEA
            rts

            ; sous programme pour generer un court bip de 14.34ms
BEEP:       ldaa  #$01
            staa  PIAKEYDB
            ldx   #$700
BOUCLEB:    dex 
            bne   BOUCLEB
            clr   PIAKEYDB 
            rts 

            ; sous programme qui genere une serie de 3 court bips.
ALERTE:     jsr   BEEP
            jsr   DELAYA
            jsr   BEEP
            jsr   DELAYA
            jsr   BEEP
            rts 

            ; sous programme qui attend qu'une touche soit enfoncee
WKEYON:     ldaa  PIAKEYDA
            rola
            rola 
            bcc   WKEYON
            clc 
            rora
            rora
            anda  #$1f 
            rts 

            ; sous programme qui attend que la touche soit relache
WKEYOFF:    ldaa  PIAKEYDA
            rola
            rola 
            bcs   WKEYOFF
            ldx   #$1000      ; debounce
BOUCLEI:    dex
            bne   BOUCLEI
            rts 

            ; sous-programme qui affiche message
            ; ce sous-programme affiche le message (6 bytes)
            ; contenu dans une table dont l'adresse de depart
            ; est prealablement stockee dans le registre X.
            ; table de messages

; '0' : segment ON     this is at hardware level
; '1' : segment OFF
; segment bit number correspondance
;      0
;    -----
; 5 |     | 1
;   |  6  |
;    -----
; 4 |     | 2
;   |     |
;    ----- . 7
;      3

CHAR_BLK = $00  ; ' '        '0' : segment OFF
CHAR_0 = $3f    ; '0'        '1' : segment ON
CHAR_1 = $06    ; '1'
CHAR_2 = $5b    ; '2'
CHAR_3 = $4f    ; '3'
CHAR_4 = $66    ; '4'
CHAR_5 = $6d    ; '5'
CHAR_6 = $7d    ; '6'
CHAR_7 = $07    ; '7'
CHAR_8 = $7f    ; '8'
CHAR_9 = $6f    ; '9'
CHAR_A = $77    ; 'A'
CHAR_B = $7c    ; 'B'
CHAR_C = $39    ; 'C'
CHAR_D = $5e    ; 'd'
CHAR_E = $79    ; 'E'
CHAR_F = $71    ; 'F'
CHAR_I = $06    ; 'I'
CHAR_L = $38    ; 'L'
CHAR_N = $37    ; 'n'
CHAR_O = $3f    ; 'O'
CHAR_R = $33    ; 'P'
CHAR_S = $6d    ; 'S'
CHAR_U = $3e    ; 'U'
CHAR_Y = $6e    ; 'y'
CHARdash = $40  ;'-'

VERSCOD:    db CHAR_S, CHAR_Y, CHAR_L, CHAR_S + $80, CHAR_0, CHAR_1         ; SYLS.01
NULCOD:     db CHAR_N, CHAR_U, CHAR_L + $80, CHAR_BLK, CHAR_BLK, CHAR_BLK   ; NUL.
RUNCOD:     db CHAR_R, CHAR_U, CHAR_N + $80, CHAR_BLK, CHAR_BLK, CHAR_BLK   ; RUN.
LOADCOD:    db CHAR_L, CHAR_O, CHAR_A, CHAR_D + $80, CHAR_BLK, CHAR_BLK     ; LOAD.
FINCOD:     db CHAR_F, CHAR_I, CHAR_N + $80, CHAR_BLK, CHAR_BLK, CHAR_BLK   ; FIN.
MESSAGE:    ldab  #$04 
            clr   PIAAFFDB 
BOUCLEC:    ldaa  $0,X        ; Prend code
            coma                ; 1's complement, '0' lit the segment, more convenient to reverse
            staa  PIAAFFDA    ; stocke
            stab  PIAAFFDB    ; charge
            clr   PIAAFFDB 
            inx                 ; prochaine data
            clc 
            rolb                ; afich suivant
            bcc   BOUCLEC     ; dernier caractere ?
            clc 
            rts 

            ; sous-programme de convertion (HEX-AFF) applique a tous les affich.
            ; Ceci va prendre chaque buffer hex et trouver son equiv.
            ; en code d'aff, et le stocke ds le buff. associe a l'aff.
CONVTOUT:   ldx   #HEXDISP    ; source hex
            stx   ADHEXAFF
            ldx   #SEGDISP    ; destination cod add.
            stx   ADSEGAFF 
BOUCLEZ:    ldx   ADHEXAFF    ; prend hex
            ldaa  $0,X
            jsr   CONVERTI    ; conversion
            ldx   ADSEGAFF
            stab  $0,X        ; recoit cod aff. eq.
            inc   ADHEXAFF+1  ; LSB=LSB+1
            inc   ADSEGAFF+1  ; LSB=LSB+1
            cpx   #ADHEXAFF   ; a t'on fini les 6 ?
            bne   BOUCLEZ     ; sinon on boucle
            rts 

            ; sous-programme de convertion HEX-AFF.
            ; sous routine de convertion hex. a code d'affichage led.
            ; avant de lancer la routine on doit charger le reg. A
            ; du code hex a cnv. le code affiche et retourne dans B.
CONVERTI:   tab
            ldx   #CODTAB 
            stx   PNTABCNV    ; MSB+LSB ADD. table
            addb  PNTABCNV+1  ; add code to LSB add. table
            stab  PNTABCNV+1  
            clrb  
            adcb  PNTABCNV    ; add carry to MSB add. table
            stab  PNTABCNV
            ldx   PNTABCNV    ; add code equiv.
            ldab  $0,X        ; b code aff.
            comb    
            rts 

; Init ACIA

;  bits definition
;##################################################
;#
;# BIT # 7 6 5 4 3 2 1 0    ACIA CTL register (write only)
;#       | | | | | | | |
;#       | | | | | | |_|_______00     clock / 1
;#       | | | | | |           01     clock / 16
;#       | | | | | |           10     clock / 64
;#       | | | | | |           11     master reset
;#       | | | | | |
;#       | | | |_|_|___________000    7 data bits, even parity, 2 stops
;#       | | |                 001    7 data bits, odd  parity, 2 stops
;#       | | |                 010    7 data bits, even parity, 1 stop
;#       | | |                 011    7 data bits, odd  parity, 1 stop
;#       | | |                 100    8 data bits, even parity, 2 stops
;#       | | |                 101    8 data bits, odd  parity, 2 stops
;#       | | |                 110    8 data bits, even parity, 1 stop
;#       | | |                 111    8 data bits, odd  parity, 2 stop
;#       | | |
;#       | |_|_________________00     RTS low,  disable TXMIT interrupt
;#       |                     01     RTS low,  enable  TXMIT interrupt
;#       |                     10     RTS high, disable TXMIT interrupt
;#       |                     11     RTS high, enable  TXMIT interrupt output level
;#       |
;#       |_____________________0      disable RXMIT interrupt
;#                             1      enable  RXMIT interrupt
;##################################################

;##################################################
;#
;# BIT # 7 6 5 4 3 2 1 0    ACIA status register (read only)
;#       | | | | | | | |
;#       | | | | | | | |______ RDRF receive data register full
;#       | | | | | | |________ TDRE transmit data register empty
;#       | | | | | |__________ DCD  data carrier detect status
;#       | | | | |____________ CTS  clear to send status
;#       | | | |______________ FE   framing error
;#       | | |________________ OVRN receive data overrun error
;#       | |__________________ PE   parity error
;#       |____________________ IRQ  interrupt request
;#
;##################################################

ASCIAINI:   
            ldaa  #$03        ; master reset
            staa  ACIAC
            nop
            ;ldaa  #$00
            ;staa  ACIAC
            ;ldaa  #$15        ; 8 data bits, odd  parity, 2 stops, clock / 16
            ;ldaa  #$14        ; 8 data bits, odd  parity, 2 stops, clock / 1
            ;ldaa  #$94        ; 8 data bits, odd  parity, 2 stops, clock / 1, ,enable  RXMIT interrupt 
            ldaa  #$95         ; 8 data bits, odd  parity, 2 stops, clock / 16, ,enable  RXMIT interrupt 
            staa  ACIAC
            ldaa  ACIAD       ; Read ACIA Receive Data register to clear if any IRQ pending...
            rts 

; send one character byte from A to serial
SENDCHAR:   psha 
            pshb 
SENDCHAL:            
            ldab  ACIAS 
            andb  #ACIA_TDRE 
            beq   SENDCHAL
            staa  ACIAD
            pulb 
            pula
            rts
            
; send a string. Need $0 at the end
PRINTSTR:   ldaa  0,X
            beq   PRINTEND    ; 0 mean end of string
            jsr   SENDCHAR
            inx               ; next character
            bra   PRINTSTR    
PRINTEND:   rts

; send a string plus CRLF. Need $0 at the end
PRNTSTR2:   ldaa  0,X
            beq   PRNTST2E    ; 0 mean end of string
            jsr   SENDCHAR
            inx               ; next character
            bra   PRNTSTR2
PRNTST2E:   ldaa  #C_CR       ; send carriage return
            jsr   SENDCHAR
            ldaa  #C_LF       ; send line feed
            jsr   SENDCHAR
            rts

S_CRLF      DB $0d, $0a, 0
S_SYLS01    DB 'Syls01', 0
S_VERS      DB 'Firmware version: August 21, 2022 18H49', 0

            ; table de donnees pour s.p de convertion HEX-AFF.
            ;org   $F9C0       ; !!!; C0-FF Attention
CODTAB:     DB CHAR_0, CHAR_1, CHAR_2, CHAR_3, CHAR_4, CHAR_5       ; 0-5
            DB CHAR_6, CHAR_7, CHAR_8, CHAR_9, CHAR_A, CHAR_B       ; 6-B
            DB CHAR_C, CHAR_D, CHAR_E, CHAR_F, CHAR_BLK, CHARdash   ; C-F,' ','-'

 
IRQVECT:                      
            ; Determine the source of the IRQ
            ldaa  ACIAS       ; Read ACIA Status Register
            bita  #$80        ; Anytime the ~IRQ is low the ~IRQ bit will be high to indicate the service request
            beq   IRQNOSER    ; IRQ from ACIA?
            bra   IRQACIA     ; Yes it's the ACIA
IRQNOSER:                     ; No it's not the ACIA
                              ; We could not find the source of the IRQ!
            rti               ; return from interrupt anyway

IRQACIA:                      ; Determine what happen in ACIA to cause an interrupt
            bita  #$01        ; Check the Receive Data Register Full
            beq   IRQNORX     ; Did we received a character?
            ldaa  ACIAD       ; Yes, then Read ACIA Receive Data register to clear the IRQ
            bra   PROCHAR     ; Process the character
IRQNORX:                      ; No it's not a received character
            rti

PROCHAR:                      ; process character in A
            jsr SENDCHAR      ; simply retransmit the character
            rti

; Serial buffer write
;SERBUFWR:



            org   $fff8       ; interrupt request (IRQ)
            word  IRQVECT
            org   $fffe       ; reset vector
            word  RSTVECT

