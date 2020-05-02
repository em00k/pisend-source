
; check for pi 

	macro nextreg_a reg
		dw $92ed
		db reg
	endm
	
	macro nextreg_nn reg, value
		dw $91ed
		db reg
		db value
	endm	
	
	device zxspectrumnext 
	org $2000
	
begin:

	; start 
	
hardclearuart:
			nextreg_nn $7f,0
			; we store the speed in nreg $7f , 2 = 115,200 8 = 2,000,000
			
			; try 115200 first 
			ld hl,trylowboaud : call print_at
			call openuart : call clearuart
			call waitforsup				 
			ld a,(supbaudflag) : cp 2 : nextreg_nn $7f,2 : ld hl,trysuccess : jr z,correctbaud
			
			; try 2MBit 
			ld hl,tryhighbaud: call print_at
			ld a,1 : ld (curbaud),a : call setbaudrate : call clearuart
			call waitforsup
			ld a,(supbaudflag) : cp 2 : nextreg_nn $7f,8 : ld hl,trysuccess : jr z,correctbaud
			
			nextreg_nn $7f,0			; set to 0 if failed 
			ld hl,failedsup
printfailed call print_at : jp finish 


correctbaud:
			;call print_at
			call flushuart
			ld hl,nextpiversion : call streamuart : call flushuart
			ld a,$0a : call senduart
loopy
			; we need a big delay in z80 terms before we try to read the uart 
			call delay : call getnextpiversion : ld hl,uartrxbuffer+(nextpiversionend-nextpiversion)+6 : call print_at

finish:		ret 

delay:			
			push bc : ld b,200			; we need a bit of a wait 
.wtl		call RasterWait : djnz .wtl : pop bc : ret 

RasterWait:
			push bc 
			ld e,190 : ld a,$1f : ld bc,$243b : out (c),a : inc b
		
.waitforlinea:	
			in a,(c) : cp e : jr nz,.waitforlinea		
			pop bc 
			ret 

waitforsup:
			;clear the buffer 

			xor a : ld (supbaudflag),a : ld (supcounter), a : ld hl,0 : ld (suploops), hl 		; reset stuffs 
			call flushuart : ld a,4 : call senduart : ld de,65535			
			nextreg_nn $7,0				; slow down or we end the loop too fast 
			
readuart:   ld bc,RX : ld hl,uartstring
.uartlp		in a,(c) : cp (hl) : jr nz,notachar
			
			push bc : ld b,200			; this is purely to slow stuff down 
.wtl		ld (temp),ix : djnz .wtl : pop bc  
			
			inc hl : ld a,(hl) : or a 
			jp z,readone
			jp .uartlp
uartstring	
			db "DietPi for the SpecNext", 0 
			
notachar:	
			dec de : ld	bc,TX : in	a,(c) : and 1 : cp 1 : jp z,readuart
			ld a,d : or e : jr z,notfound1 
			jp readuart
keeplooking: 			
			ld a,(supcounter) : cp 20 : jp c,readone
			jr readuart
			
notfound1: 	ld hl,(suploops) : inc hl : ld (suploops),hl : ld a,h : or l : jr z,keeplooking	
notfound2:  ld a,1 : out (254),a : jp timedout

readone:	ld a, 2: ld (supbaudflag),a
			ld hl,foundsup : call print_at		
timedout:	
			nextreg_nn $7,3

			ret 

supcounter:  db 0
suploops: 	 dw 0 		
supbaudflag: db 0 	
temp 		 dw 0
TX 	EQU $133B
RX 	EQU $143B

getnextpiversion:

			

		; Read data from UART.
			
			;call flushuart 
			;nextreg_nn $7,0
			ld hl,uartrxbuffer
			ld de,0
			ld (hl),255
		; > HL = Destination
		; > DE = Length

in232a:	
			ld	bc,$133B				; TX 
			in	a,(c)					; read 
			and 1 						; bit 0 = 0 fifo is empty or bit 0 = 1 data to get 
			or a 
			jr z,nomoredata 			; no more data we're done 
			
			ld	bc,$143B
			in	a,(c)
			ld b,a 
			cp 10 : jr nz,inrange
			ld a,b 
			cp 31 : jr c,outofrange
			
			cp 164 : jr nc,outofrange
inrange:			
			ld	(hl),a   				; Store to memory
			inc	hl
			ld (hl),255
outofrange:
			inc de 
			ld a,d
			;cp 2						; 512bytes
			;jr z,nomoredata
			jr nz,in232a
nomoredata:		
			;ld (bufferin),a
			dec hl 
			ld (hl),255
			ret 

flushuart:
			nextreg_nn $7,3 : ld e,0
fifo:		ld	bc,$143B : 
			in	d,(c) : in	d,(c)  					; : dec	hl : jr	nz,fifo
			ld	bc,$133B							; TX 
			in	a,(c)								; read 
			and 1 									; bit 0 = 0 fifo is empty or bit 0 = 1 data to get 
			or a   	
			jr nz,fifo 								; no more data we're done 	;;
			dec e 
			jr nz,fifo 
			nextreg_nn $7,3
			ret 

senduart:
			ld bc,$133b 						; write to uart 
			ld d,a 
.toutb:		; if busy do a little loop 
			in a,(c) : and 2 : jr nz,.toutb
			ld a,d : out (c),a
			ret 	

streamuart	ld a,(hl):inc hl:or a:ret z
			ld bc,$133b 						; write to uart 
			ld d,a 
.koutb:		; if busy do a little loop 
			in a,(c) : and 2 : jr nz,.koutb : ld a,d : out (c),a
			jr streamuart

openuart: 
			ld bc,5435 : ld a,64 : out (c),a 
			call setbaudrate
			nextreg_nn $a0, $30
			nextreg_nn $a2, $d2			
			call flushuart
			ret 
	
clearuart: 
			; sends a clear command 
			ld a, 13 : call senduart
			ld a, $03 : call senduart
			ld a, $03 : call senduart
			
			ret 

setbaudrate:		
			LD A, (curbaud)
			push af : pop af							
			ld d,0 : sla a : rl d : sla a		
			rl d : sla a : rl d : sla a		
			rl d : ld e,a : ld hl,BaudPrescale	
			add hl,de : ld bc,9275 : ld a,17 : out (c),a
			ld bc,9531 : in a,(c) : ld e,a : rlc e 
			ld d,0 : add hl,de

			ld e,(hl) : inc hl : ld d,(hl) : ex de,hl

			push hl		; this is prescaler		
			push af		; and value
						
			ld bc,rxsetbaud	: ld a,l : and %01111111
			out (c),a : ld a,h : rl l : rla	: or %10000000
			out (c),a : pop af : ld l,a : ld h,0 : pop hl
			ret

BaudPrescale:

			DEFW 243,248,256,260,269,278,286,234 			; Was 0 - 115200 adjust for 0-7
			DEFW 14,14,15,15,16,16,17,14 					;2000000 -14
		
curbaud:	
			DEFB 0			;start at 115200
			DEFB 0			;Zero for easy load at 16bits

print_at	ld a,(hl):inc hl:cp $ff:ret z
			rst 16:jr print_at

trylowboaud		db 		22, 0, 0,"Trying 115,200...",255
tryhighbaud		db 		22, 0, 0,"Trying 2000000...",255
trysuccess		db		22, 0, 0,"COMMS@ ",255
failedsup		db 		22, 1, 0,"Unable to talk to pi0",13,"Is the Pi ready?",255
foundsup		db 		22, 1, 0,"Found Pi at above speed",13,255
;nextpiversion	db		"nextpi-admin_version",0
nextpiversion	db		"uname",0
nextpiversionend
rxsetbaud EQU 5179
uartrxbuffer	defs	512,0
	
endbegin:

	savebin "H:\dot\checkpi",begin,endbegin-begin 
	