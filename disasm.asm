;Parašykite programą, kuri realizuoja mini-disasemblerį, 
;kurio aprašas pateikiamas žemiau.
;
;Programa turi skaityti COM failą ir disasembliuoti tokias instrukcijas:
;1) Visas MOV instrukcijas;
  
.model small
       ASSUME CS:kodas, DS:duomenys, SS:stekas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
stekas segment word stack 'STACK'
       dw 400h dup (00)               ; stekas -> 2 Kb
stekas ends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
duomenys segment para public 'DATA'
	instruction_table:
		db 10001110b 
				dw offset regMemToSeg
		db 8Ch 
				dw offset segToRegMem
		db 10100000b
				dw offset memToAccumulator
		db 10100001b
				dw offset memToAccumulator
		db 10100010b
				dw offset accumulatorToMem
		db 10100011b
				dw offset accumulatorToMem
		db 11000110b
				dw offset immediateToRegMem
		db 11000111b
				dw offset immediateToRegMem
		db 88h 
				dw offset regMemToFromReg
		db 89h 
				dw offset regMemToFromReg
		db 8Ah 
				dw offset regMemToFromReg
		db 8Bh 
				dw offset regMemToFromReg	
		db 10110000b 
				dw offset immediateToReg
		db 10110001b 
				dw offset immediateToReg
		db 10110010b 
				dw offset immediateToReg
		db 10110011b 
				dw offset immediateToReg
		db 10110100b 
				dw offset immediateToReg
		db 10110101b 
				dw offset immediateToReg
		db 10110110b 
				dw offset immediateToReg
		db 10110111b 
				dw offset immediateToReg
		db 10111000b 
				dw offset immediateToReg
		db 10111001b 
				dw offset immediateToReg
		db 10111010b 
				dw offset immediateToReg
		db 10111011b 
				dw offset immediateToReg
		db 10111100b 
				dw offset immediateToReg
		db 10111101b 
				dw offset immediateToReg
		db 10111110b 
				dw offset immediateToReg
		db 10111111b 
				dw offset immediateToReg						
	klaidaskaityme:
		db 00
    komEilIlgis:
        db 00
    komEilutesArgumentas:
        db 100h dup (00)
    skaitomasFailas:
        dw 0FFFh 
    eiluteIsvedimui:
       db '' 
    nuskaitytas:
       db 00, 00, '$'     
    klaidosPranesimas:
        db 'Klaida skaitant argumenta $'
    klaidosApieFailoAtidarymaPranesimas:
        db 'Klaida atidarant faila $'
    klaidosApieFailoSkaitymaPranesimas:
        db 'Klaida skaitant faila $' 
    newLine:   
        db 0Dh, 0Ah, '$'  ; tekstas ant ekran
;-----------------------------------------------------
	w_is:
		db 00
	d_is:
		db 00
	mod_is:
		db 00
	reg_is:
		db 00
	rm_is:
		db 00
	mov_string:
		db 'mov $'
	low_adr:
		db 00
	high_adr:
		db 00
;---------------------Registrai-----------------------
	segmentRegisters:
		db 'es$'
		db 'cs$'
		db 'ss$'
		db 'ds$'

 	mod11w0Registers:
		db 'al$'
		db 'cl$'
		db 'dl$'
		db 'bl$'
		db 'ah$'
		db 'ch$'
		db 'dh$'
		db 'bh$'
		 
	mod11w1Registers:
		db 'ax$'
		db 'cx$'
		db 'dx$'
		db 'bx$'
		db 'bp$'
		db 'sp$'
		db 'si$'
		db 'di$'
		
	effectiveAddressCalculationRegisters:	
		db '[bx+si$' ;0
		db '[bx+di$' ;7
		db '[bp+si$' ;14
		db '[bp+di$' ;21
		db '[si   $' ;28
		db '[di   $' ;35
		db '[bp   $' ;42
		db '[bx   $' ;49

duomenys ends
;-----------------------------------------------------
PrintSymbol macro symbol
	push ax
	push dx
	mov	ah, 2
	mov	dl, symbol
	int	21h
	pop	dx
	pop	ax
endm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
writeln macro line
	push ax
	push dx

	mov ah, 09
	mov dx, offset line
	int 21h

	mov ah, 09
	mov dx, offset newLine
	int 21h

	pop dx
	pop ax          
endm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
write macro line
	push ax
	push dx

	mov ah, 09
	mov dx, offset line
	int 21h

	pop dx
	pop ax      
endm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LOCALS @@

kodas segment para public 'CODE'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    atverkFaila proc near
        ; dx - failo vardo adresas
        ; CF yra 1 jeigu klaida 
        push ax
        push dx

        mov ah, 3Dh
        mov al, 00h       ; skaitymui
        int 21h

        jc @@pab
        mov word ptr skaitomasFailas, ax

        @@pab:  
        pop dx
        pop ax
        ret   
    atverkFaila endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    uzdarykFaila proc near
        ; dx - failo vardo adresas
        ; CF yra 1 jeigu klaida 
        push ax
        push bx

        mov ah, 3Eh
        int 21h

        @@pab:  
        pop dx
        pop ax
        ret     
    uzdarykFaila endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    rasykSimboli proc near
        ; al - simbolis 
        push ax
        push dx
        mov dl, al
        mov ah, 02h
        int 21h
        pop dx
        pop ax
        ret   
    rasykSimboli endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   konvertuokI16taine proc near
        ; al - baitas
        ; ax - rezultatas
        mov ah, al
        and ah, 0F0h
        shr ah, 1
        shr ah, 1
        shr ah, 1
        shr ah, 1
        and al, 0Fh

        cmp al, 09
        jle @@plius0
        sub al, 10
        add al, 'A'
        jmp @@AH
        @@plius0:
        add al, '0'
        @@AH:
             
        cmp ah, 09
        jle @@darplius0
        sub ah, 10
        add ah, 'A'
        jmp @@pab
        @@darplius0:
        add ah, '0'
        @@pab:
        xchg ah, al 
        ret     
    konvertuokI16taine endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    skaitykArgumenta proc near
		; nuskaito ir paruosia argumenta
		; jeigu jo nerasta, tai CF <- 1, prisingu atveju - 0
		push bx
		push di
		push si 
		push ax

		xor bx, bx
		xor si, si
		xor di, di

		mov bl, es:byte ptr[80h]         
		mov byte ptr komEilIlgis, bl
		mov si, 0081h  
		mov di, offset komEilutesArgumentas
		push cx
		mov cx, bx
		mov ah,00
		cmp cx, 0000
		jne @@pagalVisus
		stc 
		jmp @@pab

		@@pagalVisus:
		mov al, byte ptr es:[si]
		cmp al, ' '
		je @@toliau
		cmp al, 0Dh
		je @@toliau
		cmp al, 0Ah
		je @@toliau
		mov byte ptr [di],al
		; call rasykSimboli  
		mov ah, 01                  ; ah - pozymis, kad buvo bent vienas "netarpas"
		inc di     
		jmp @@kitasZingsnis
		@@toliau:
		cmp ah, 01                  ; gal jau buvo "netarpu"?  
		je @@isejimas 
		@@kitasZingsnis:
		inc si

		loop @@pagalVisus
		@@isejimas: 
		cmp ah, 01                  ; ar buvo "netarpu"?  
		je @@pridetCOM
		stc                         ; klaida!   
		jmp @@pab 
		@@pridetCOM:
		mov byte ptr [di],'.'
		mov byte ptr [di+1], 'C'
		mov byte ptr [di+2], 'O'
		mov byte ptr [di+3], 'M'
		clc                         ; klaidos nerasta
		@@pab:
		pop cx
		pop ax
		pop si
		pop di 
		pop dx
		ret
    skaitykArgumenta endp 	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    writeASCIIZ proc near
		; spausdina eilute su nuline pabaiga, dx - jos adresas
		push si
		push ax
		push dx

		mov  si, dx

		@@pagalVisus:
		mov dl, byte ptr [si]  ; krauname simboli
		cmp dl, 00             ; gal jau eilutes pabaiga?
		je @@pab

		mov ah, 02
		int 21h
		inc si
		jmp @@pagalVisus
		@@pab:

		writeln newLine

		pop dx
		pop ax
		pop si
		ret
    writeASCIIZ endp 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    skaitomeFaila proc near
        ; skaitome faila po viena baita 
        ; bx - failo deskriptorius
        ; dx - buferis 
        push ax
        push dx
        push bx
        push cx
        push si
                
        mov si, dx 
        @@kartok:
        mov cx, 01
        mov ah, 3Fh 
        int 21h
        jc @@isejimas           ; skaitymo klaida
        cmp ax, 00
        je @@isejimas           ; skaitymo pabaiga

        mov al, byte ptr [si]   ; is buferi

		mov cx, 00FFh
		mov bp, 00h
	   
		@@loop_ci:				   
				cmp al, byte ptr [instruction_table + bp]
				je @@call_function
				inc bp
				inc bp
				inc bp
		loop @@loop_ci
	   
		@@call_function:
		
		call word ptr [instruction_table + 1 + bp]

        ;call konvertuokI16taine
        mov word ptr [si], ax  
        jmp @@kartok
        
        @@isejimas:
        pop si
        pop cx
        pop bx
        pop dx
        pop ax
        ret   
    skaitomeFaila endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	skaitykKita proc near
		mov cx, 01
		mov ah, 3Fh 
		int 21h
		jc @@isejimas1           ; skaitymo klaida
		cmp ax, 00
		je @@isejimas1           ; skaitymo pabaiga
		mov al, byte ptr [si]  
		
		mov byte ptr klaidaskaityme, 00000000b
		@@isejimas1:
		mov byte ptr klaidaskaityme, 11111111b

		ret
	skaitykKita endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	findMOD proc near
		push ax
		push bx
		xor ah, ah
		xor bx, bx
		and al, 11000000b ; randu mod
		shr al, 6
		mov byte ptr [mod_is], al
		pop bx
		pop ax
		ret
	findMOD endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	findRM proc near
		push ax
		push bx
		xor ah, ah
		xor bx, bx
		and al, 00000111b ; randu r/m		
		mov byte ptr [rm_is], al
		pop bx
		pop ax
		ret
	findRM endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	findREG proc near
		push ax
		push bx
		xor ah, ah
		xor bx, bx
		and al, 00111000b ; randu reg
		shr al, 3
		mov byte ptr [reg_is], al
		
		pop bx
		pop ax
		ret
	findREG endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	findW proc near
		push ax
		push bx
		xor ah, ah
		xor bx, bx
		and al, 00000001b ; randu W
		mov byte ptr [w_is], al
		
		pop bx
		pop ax
		ret
	findW endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	findD proc near
		push ax
		push bx
		xor ah, ah
		xor bx, bx
		and al, 00000010b ; randu D
		mov byte ptr [d_is], al
		
		pop bx
		pop ax
		ret
	findD endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	printSEG proc near
		push ax
		push bx
		push si
		push dx
		mov al, byte ptr [reg_is]
		mov bl, 3
		mul bl        
		mov si, ax		
		lea dx, [segmentRegisters + si] ;  00
		mov ah, 09 
		int 21h	
		pop dx
		pop si
		pop bx
		pop ax
		ret
	printSEG endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	printREG proc near
		push bx
        push dx
        push bx
        push cx
        push si
		push ax
		
		;mov al, byte ptr [rm_is]
		
		cmp byte ptr [mod_is], 11b
			jne no_11
			mov al, byte ptr [rm_is]
			mov bl, 3
			mul bl        
			mov si, ax	;RANDU IS KURIOS rm VIETOS IMT
			;;DAR TIKRINT KOKS W YRA
			cmp byte ptr [w_is], 1b
				jne w_is_0
				lea dx, [mod11w1Registers + si]
				jmp @@spausdinkmod11
				w_is_0:
				lea dx, [mod11w0Registers + si]
			@@spausdinkmod11:
			mov ah, 09 
			int 21h	
			jmp @@igala	
			
		no_11:
			cmp byte ptr [mod_is], 01b
			jne no_01
			push ax
			push bx
			push dx
			push si
			
			mov al, byte ptr [rm_is]
			mov bl, 7
			mul bl        
			mov si, ax	;RANDU IS KURIOS rm VIETOS IMT
			
			lea dx, [effectiveAddressCalculationRegisters + si]
			mov ah, 09 
			int 21h	
			PrintSymbol '+'
			pop si
			pop dx
			pop bx
			pop ax

				call readone

			PrintSymbol ']'
			jmp @@igala	
		no_01:	
			cmp byte ptr [mod_is], 00b
			jne no_00
			
				cmp byte ptr [rm_is], 110b
				jne ne_110b
					PrintSymbol '['
					;-----------------------------------neberandu klaidos
						
						call getimmediate
					
					PrintSymbol ']'
					jmp @@igala
				ne_110b:
				mov al, byte ptr [rm_is]
					mov bl, 7
					mul bl        
					mov si, ax	;RANDU IS KURIOS rm VIETOS IMT
					lea dx, [effectiveAddressCalculationRegisters + si]
					mov ah, 09 
					int 21h	
					PrintSymbol ']'
				jmp @@igala	
		no_00:	
			;cia jei mod = 10
			push ax
			push bx
			push dx
			push si
			
			mov al, byte ptr [rm_is]
			mov bl, 7
			mul bl        
			mov si, ax	;RANDU IS KURIOS rm VIETOS IMT
			lea dx, [effectiveAddressCalculationRegisters + si]
			mov ah, 09 
			int 21h	
			
			pop si
			pop dx
			pop bx
			pop ax
			PrintSymbol '+'
					mov byte ptr [w_is], 1b ;CIA KLAUSTUKAS JEI W0
					call getimmediate
			PrintSymbol ']'

		@@igala:
		pop ax	
		pop si
        pop cx
        pop bx
        pop dx
        pop bx	
		ret
	printREG endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    regMemToSeg proc near
		call skaitykKita

		push ax
        push dx
        push bx
        push cx
        push si
		
		call findMOD
		call findREG	
		call findRM 	

		mov byte ptr [w_is], 1b
		
		write mov_string  ;turim "mov "
		call printSEG
		PrintSymbol ','
		call printREG
		
		write newLine
		
		pop si
        pop cx
        pop bx
        pop dx
        pop ax
		ret     
    regMemToSeg endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    segToRegMem proc near
		call skaitykKita

		push ax
        push dx
        push bx
        push cx
        push si
		
		call findMOD
		call findREG	
		call findRM 	
		
		mov byte ptr [w_is], 1b
		
		write mov_string  ;turim "mov "
		call printREG
		PrintSymbol ','
		call printSEG
		write newLine
		
		pop si
        pop cx
        pop bx
        pop dx
        pop ax
		ret     
    segToRegMem endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	accumulatorToMem proc near
		call findW		
		mov byte ptr [rm_is], 000b
		mov byte ptr [mod_is], 11b
		write mov_string
		PrintSymbol '['
		call getimmediate
		PrintSymbol ']'
		PrintSymbol ','
		call printREG ;spausdinu AX/AL 
		write newLine
		ret
	accumulatorToMem endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	memToAccumulator proc near
		call findW
		mov byte ptr [rm_is], 000b
		mov byte ptr [mod_is], 11b
		write mov_string
		
		call printREG ;spausdinu AX/AL
		PrintSymbol ','
		PrintSymbol '['
		mov byte ptr [w_is], 1b
		call getimmediate
		PrintSymbol ']'
		write newLine		
		ret
	memToAccumulator endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	immediateToRegMem proc near
		
		call findW
		call skaitykKita
		
		call findMOD
		call findRM
		
		write mov_string
		
		call printREG
		PrintSymbol ','
		
		cmp byte ptr [w_is], 1b
			jne nop_w1
			call getimmediate
			jmp @@ciaipabaiga
		 nop_w1:
			call readone
		@@ciaipabaiga: 
		
		write newLine		
		ret
	immediateToRegMem endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	immediateToReg proc near
		;1011 1011
		push ax
		and al, 00001000b
		shr al, 3
		mov byte ptr [w_is], al
		pop ax
		
		mov byte ptr [mod_is], 11b
		push ax
		and al, 00000111b
		mov byte ptr [rm_is], al
		pop ax
		write mov_string
		
		call printREG
		PrintSymbol ','
		cmp byte ptr [w_is], 1b
			jne nera_w1
			call getimmediate
			jmp @@cia
		 nera_w1:
			call readone
		@@cia: 
		write newLine		
		ret
	immediateToReg endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
getimmediate proc near
		mov byte ptr [low_adr], 00b
		mov byte ptr [high_adr], 00b
		call skaitykKita
				
		mov byte ptr [low_adr], al
		
		cmp byte ptr [w_is], 1b
			jne ne_w1
			call skaitykKita
			mov byte ptr [high_adr], al
			;call getimmediate
			;cia  konstanta kokia kelt
			push ax
			push bx
			push dx
			
			mov bl, byte ptr [high_adr]
			mov bh, 00
			mov ax, bx
			call konvertuokI16taine
			mov word ptr [si], ax  
			write eiluteIsvedimui
		
		ne_w1:
		mov bl, byte ptr [low_adr]
		mov bh, 00
		mov ax, bx
		call konvertuokI16taine
		mov word ptr [si], ax  
		write eiluteIsvedimui
		
		pop dx
		pop bx
		pop ax
		ret
	getimmediate endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
readone proc near
		mov byte ptr [low_adr], 00b
		mov byte ptr [high_adr], 00b
		call skaitykKita
		push ax
		push bx
		push dx		
		push si
		mov byte ptr [low_adr], al
			xor ax, ax
			mov bl, byte ptr [low_adr]
			mov bh, 00h
			mov ax, bx
			call konvertuokI16taine
			mov word ptr [si], ax  
			write eiluteIsvedimui
		pop si
		pop dx
		pop bx
		pop ax
		ret
	readone endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	regMemToFromReg proc near
		
		write mov_string
	
		call findW
		call findD
		
		call skaitykKita

		push ax
        push dx
        push bx
        push cx
        push si
		call findMOD
		call findREG	
		call findRM 
		
		cmp byte ptr [d_is], 0b
			jne disno0
			call printREG
			PrintSymbol ','
			
			push ax
			push bx
			MOV AL, byte ptr [reg_is]
			MOV BL, byte ptr [rm_is]
			XCHG AL, byte ptr [rm_is]
			XCHG BL, byte ptr [reg_is] 
			pop bx
			pop ax
			mov byte ptr [mod_is], 11b
			call printREG
			jmp @@regmemtofromregend
		PrintSymbol ','
		
		disno0:
			
			xor cx,cx
			mov cl, byte ptr [mod_is] ;CIA AR REIK
			mov byte ptr [mod_is], 11b
			
			
			push ax
			push bx
			MOV AL, byte ptr [reg_is]
			MOV BL, byte ptr [rm_is]
			XCHG AL, byte ptr [rm_is]
			XCHG BL, byte ptr [reg_is] 
			pop bx
			pop ax
		
			call printREG
			PrintSymbol ','
			mov byte ptr [mod_is], cl
			push ax
			push bx
			MOV AL, byte ptr [reg_is]
			MOV BL, byte ptr [rm_is]
			XCHG AL, byte ptr [rm_is]
			XCHG BL, byte ptr [reg_is] 
			pop bx
			pop ax
			
			call printREG

		@@regmemtofromregend:
		write newLine
		pop si
        pop cx
        pop bx
        pop dx
        pop ax
		ret
	regMemToFromReg endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    pradzia:
		; pradzioje ds ir es rodo i PSP;
		; PSP+80h -> kiek baitu uzima komandine eilute (be programos pavadinimo)
		; 
		mov ax,     seg duomenys                   ; "krauname" duomenu segmenta
		mov ds,     ax

		call skaitykArgumenta
		jnc @@rasykArgumenta
		writeln klaidosPranesimas
		jmp @@Ok

		@@rasykArgumenta: 
		mov dx, offset  komEilutesArgumentas      
		call writeASCIIZ 

		;Atidarome faila
		mov dx, offset  komEilutesArgumentas      
		call atverkFaila
		jnc @@skaitomeFaila
		writeln klaidosApieFailoAtidarymaPranesimas
		jmp @@Ok

		@@skaitomeFaila:
		mov bx, word ptr skaitomasFailas          
		mov dx, offset nuskaitytas          
		call skaitomeFaila
		jnc @@failoUzdarymas
		writeln klaidosApieFailoSkaitymaPranesimas
		;jmp @@Ok

		@@failoUzdarymas:
		mov bx, word ptr skaitomasFailas          
		call uzdarykFaila

		@@Ok:
		mov ah,     4ch                            ; baigimo funkcijos numeris
		int 21h
kodas  ends
    end pradzia 
