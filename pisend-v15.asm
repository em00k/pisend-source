
; pisend 2.20 for zx spectrum next 
; emook2020 / david saphier 2020
;
; sends file or commands to pizero 
; 32bitdisplay by alwin henseler
; div32_16 by fgsgh 
; aweasome base64encoder by KevB 
; auto uart speed by Tim Gilberts 

; ".pisend 2.20"
;stty -F /dev/ttyAMA0 57600

; issues 
; files size of 8k arriving as only 4kb
;
; usage 
;	.pisend [file]
;		base63 encodes a file a send to the pi0 uart 
;	.pisend -q 
;		MUST BE CALLED to discover pi0 
;	.pisend -c [cmd]
;		Isses a bash command to NextPi 
;	.pisend -r 
;		Reads the pi0 uart buffer to $c000 
;	.pisend -s 
;		Sends a soft command (no uart clear etc)
;	.pisend -U
;		Swaps the baud of the pi0 and reboots 
debug = 1

m_drv_api	 	equ $92
m_getsetdrv  	equ $89
f_open       	equ $9a
f_close      	equ $9b
f_read       	equ $9d
f_write      	equ $9e
f_seek       	equ $9f
f_get_dir    	equ $a8
f_set_dir    	equ $a9
fa_read      	equ $01
fa_append    	equ $06
fa_overwrite 	equ $0c
writetx			equ $133b 
readrx			equ $143b
ctrlc			equ $03	
loadbuff		equ $6000				; this is where the load buffer is 
ba64buff		equ $a000				; this is where we encode the load buffer to 
buffersize		equ	$4000				; this is how much to load 

	device zxspectrum48

	macro nextreg_nn reg, value
		dw $91ed
		db reg
		db value
	endm
	
	macro nextreg_a reg
		dw $92ed
		db reg
	endm
	
	org 2000h
	
start1		di 
			;DW $01DD 
			nextreg_nn 7, 3						;		 28 mhz
			ld (commandline),hl
			push iy,ix,hl,de,bc,af : exx 		; we need to check we return to basic cleanly
			push hl,de,bc : ex af,af' 			; so i reserve everything 
			push af
			ld (fixstack+1),sp
			
			ld sp,$5BBF
			ld hl,(commandline)
	; check for args 
			ld a,h:or l:jr nz,argsset 
	; show help & end 
			ei
			ld hl,emptyline:call print_rst16:jp finish	
			
			
argsset:
			;ifdef debug = 1 
				push hl : 
				ld hl,version : call print_at
				pop hl 
			;endif 
	; copy arg to buffer (must be filespec)
			ld de,textbuff
copytext:	ld a,(hl)
			cp $0d
			jp z,foundazero 
			cp ':'
			jp z,foundazero 
			ld (de),a 
			ldi 
			xor a : ld (de),a
			jp copytext
			
foundazero: 			
			; search for -c for a command line 
			ld hl,textbuff
qloop:		ld a,(hl)
			inc hl 
			cp '-'
			jr z,firstbit			; is there a - 
			;or a 
			jp nz,uploadmode
firstbit:	ld a,(hl)
			cp 'c'					
			jr z,configline			; -c then configline 
			
secondbit:	cp 'q'

			jp z,hardclearuart		; -q hard clear / query 
			
			cp 's'
			jp z,silentkey			; -s soft send (no clear or baud checks)
			
			cp 'U'					
			jp z,swappibaud 		; -U swap uart speed 
			
			cp 'r'			; 
			jp z,readtheuart		; -r read uart buffer exepcts the baud to already be set and open 
				
			jp uploadmode 			; must be an upload 
			
readtheuart:
			ld a,$7f : call getreg : or a : jp z,finish		; reg $7f isnt set
			
			; dont need to flush, just a read loop 

			; Read data from UART.

			ld hl,$c000 : ld de,$c001 : ld (hl),0 : ld bc,512 : ldir 		; make sure ram in empty 
			ld hl,$c000 : ld de,512
			; > HL = Destination
			; > DE = Length

in232a:	
			ld	bc,$133B : in a,(c) : and 1 : cp 1 : jr nz,nodata 
			ld	bc,$143B : in a,(c) : ld (hl),a   ; Store to memory
			inc	hl
			; tx = 133b
			dec de : ld a,d : or e  : jr nz,in232a
nodata:		jp finish
			
			
			
			
silentkey:
			ld a,1 : ld (silent_prog),a 		; this is where we just send the output direct to the uart 
configline: 
			inc hl 
			ld de,textbuff
copytext2:	ld a,(hl)
			cp $0d
			jp z,configdone 
			cp ':'
			jp z,configdone 
			cp 0
			jp z,configdone 
			ld (de),a 
			ldi 
			xor a : ld (de),a
			jp copytext2
failedbaudtest
			db "Failed to detect baud rate,   ",13
			db "Did you run '.pisend -q'?     ",13
			db 0 
swappibaud: 	
			ld a,$7f : call getreg : cp 8 : jp z,.set2mbit
			cp 2 : jp z,.dotnset	; 115200 
			ld hl,failedbaudtest : jp printfailed
		
.set2mbit:
			;ld a,1 : ld (CURBAUD),a 
			ld hl,_2mbto115 : call print_rst16
			ld hl,update115
			jr .overdotnset
.dotnset:			
			ld hl,_115to2mb : call print_rst16
			ld hl,update2mb
.overdotnset:			
			ld de,textbuff : ld bc,end115-update115: ldir 
			ld hl,updatetext : call print_rst16
		
configdone:
			ld a,(silent_prog) : cp 1 : jr z,justsend 
	; this bit just sends a command  to nextpi and quits 
			ld a,$7f : call getreg : cp 8 : jr z,.set2mbit
			cp 2 : jr z,.dotnset	; 115200 
			ld hl,failedbaudtest : jp printfailed
.set2mbit:
			ld a,1 : ld (CURBAUD),a 
.dotnset:			
			
			call openuart			
			call clearuart   ; ctrl+c				
			ld a,$0d : call senduart
justsend:			
			ld hl,textbuff+1 : call streamuart
			ld a,$0a : call senduart
			jp finish

hardclearuart:
			
			call openuart
			call clearuart
			ld a,3 : ld bc,writetx : out (c),a 
			call setbaud115200
			nextreg_nn $7f,0
			ld hl,trylowboaud : call print_at
			ld a,13 : call senduart
			ld a,4 : call senduart
			; try 115200 first 
			call waitforsup				; sup flag will = 2 if a sup was found. 
			ld a,(supbaudflag) : cp 2 : nextreg_nn $7f,2 : ld hl,trysuccess : jr z,correctbaud
			; try 2MBit 
			ld hl,tryhighbaud: call print_at
			ld a,1 : ld (CURBAUD),a 
			call setbaudrate
			;ld a,3 : call senduart
			;ld a,17 : call senduart			
			call clearuart
			ld a,13 : call senduart
			;ld a,3 : call senduart
			;ld a,3 : call senduart
			ld a,4 : call senduart
			;ld a,19 : call senduart
			call waitforsup
			
			ld a,(supbaudflag) : cp 2 : nextreg_nn $7f,8 : ld hl,trysuccess : jr z,correctbaud
			nextreg_nn $7f,0			; set to 0 if failed 
			ld hl,failedsup
printfailed call print_rst16 : jp finish 
correctbaud:
			call print_at
			jp finish

waitforsup:

			;clear the buffer 

			xor a : ld (supbaudflag),a : ld (supcounter), a : ld hl,0 : ld (suploops), hl 		; reset stuffs 
			call flushuart
			ld a,4 : call senduart
			ld de,65535
			
			nextreg_nn $7,1
			call delay 
readuart:   ld bc,RX : ld hl,uartstring
.uartlp		in a,(c) : cp (hl) : jr nz,notachar

			call delay 

			inc hl : ld a,(hl) : or a 
			jp z,readone
			jp .uartlp
uartstring	
			db "DietPi for the SpecNext", 0 
tempsup		dw 0 			
notachar:	and 7 
			out	(254),a   ; Set border color
			dec de 
			ld	bc,TX : in	a,(c) : and 1 : cp 1
			jp z,readuart
			ld a,d : or e : jr z,notfound1
			jp readuart
keeplooking: 			
			ld a,(supcounter) : cp 255 : jp c,readone
			jr readuart
			
notfound1: 	ld hl,(suploops) : inc hl : ld (suploops),hl : ld a,h : or l : jr z,keeplooking	
notfound: 	;ld hl,failedsup : call print_rst16
notfound2:  ld a,1 : out (254),a : jp timedout

readone:	ld a, 2: ld (supbaudflag),a
			;ld hl,foundsup : call print_rst16		
			nextreg_nn $7,3
			ret 
timedout:	
			nextreg_nn $7,3
			;pop hl : jp finish
			ret 
delay:			
			push bc : push de : ld b,4			; we need a bit of a wait 
.wtl		call RasterWait : djnz .wtl : pop de : pop bc : ret 

RasterWait:
			push bc 
			ld e,190 : ld a,$1f : ld bc,$243b : out (c),a : inc b
		
.waitforlinea:	
			in a,(c) : cp e : jr nz,.waitforlinea		
			pop bc 
			ret 

supcounter:  db 0
supbuffer:   db "SUP>"
suploops: 	 dw 0 		
supbaudflag: db 0 	
silent_prog: db 0 
uploadmode:			

			;call reservebank
			; this bit sends the filename on command line to nextpi 
			; textbuff now has our filename 
			;call reservebank	
			;break 
			ld a,$7f : call getreg : cp 8 : jr z,.set2mbit
			cp 2 : jr z,.set115 : jp finish
.set2mbit:
			ld a,1 : ld (CURBAUD),a 
.set115:	
			;ld a,6 : out (254),a 
			; 
			call openuart			; opens uart port 
			
			call waitforsup
			; stores all current memory banks and requests new ones 
			; this is split up for testing puposes 
			ld a, $53 : call getreg : ld (bank3orig),a	          
			ld a, $54 : call getreg : ld (bank4orig),a	          
			ld a, $55 : call getreg : ld (bank5orig),a	          
			ld a, $56 : call getreg : ld (bank6orig),a	          
			ld a, $57 : call getreg : ld (bank7orig),a
			call getbank : ld (bank3),a 
			call getbank : ld (bank4),a 
			call getbank : ld (bank5),a 
			call getbank : ld (bank6),a 
			call getbank : ld (bank7),a 
			ld a,(bank3) : nextreg_a $53
			ld a,(bank4) : nextreg_a $54
			ld a,(bank5) : nextreg_a $55
			ld a,(bank6) : nextreg_a $56
			ld a,(bank7) : nextreg_a $57

			call setdrv
			ld ix,textbuff			; filename 
			call openfile			; open 

			call getfilesize		; get size of file into bufferfs 
			
			; print the file name top right 
			
			ld hl,textbuff : call print_fname
			ld a,13 : rst 16 
			
			; txtsize label 
			ld hl,txtsize : call print_rst16

			; print file size 
			ld de,(bufferfs+9) : ld hl,(bufferfs+7)
			call b2d32 : ld hl,b2dend-10 : call print_rst16
			ld a,13 : rst 16
			
			; file size is in bufferfs as a 32bit long 
			ld hl,(bufferfs+9)
			ld ix,(bufferfs+7)
		
			; ixhl = 32bit size in bytes 
			; we need to divide the size by <8192> buffersize to work out how many chunks to send 
			; and get the remainder in de for the last chunk 
			; 
			ld bc,buffersize : call div32_16
			
			; de remainder 
			ld (overrun),de 		; save the offset (push to stack?)
			; ix * buffersize + de  
			push ix 				; get ix into hl 
			
			ld de,(bufferfs+9)		; get back 32bize to check it isnt 0 
			ld hl,(bufferfs+7)
			; check for zero bytes 
			ld a,l : or h : jr z,yesitwaszero
			jr wasnt0
yesitwaszero:
			ld a,e : or d : jr z,weatzeroman 
			jr wasnt0
weatzeroman:
			pop hl					; get this off stack before quitting
			jp finish			
wasnt0: 
			pop hl
			; we only want l otherwise the file is crazy 
			; now we want h as well 
			; lets save hl on to bc 
			push hl
			
			ld a,l 					
			push af 
			
			ld hl,enablecatcher : call streamuart		; this gets nextpi ready for to recieve  
			ld hl,textbuff : call streamuart			; and the output file 
			
			ld a,'"' : call senduart					; send closing quote 
			ld a,13 : call senduart						; send a LF 
;

			; quick check to see if anything way echoed back 

;
			pop af
			push af 
			ld b,a
			; print textchunk label 
			ld hl,txtchunks : call print_rst16
			
			ld a,13 : rst 16
			; prints number of chunk
			ld a,b 
			call b2d8 : ld hl,b2dend-3 : call print_rst16
			
			;ld hl,(overrun) : call b2d16 : ld hl,b2dend-5 : call print_rst16
			ld a,13 : rst 16
			
			;call b2d8 : ld hl,b2dend-3 : call print_rst16
			
			; sends the header of the base64 
			push bc 
			ld hl,headerend-header : ld (filesize),hl : ld hl,header : call senduartmemory
			pop bc 
			
			pop af 
	
			; now we pop bc back we puch from hl up above 

			pop bc 
			ld a,b 
			or c 					; check if bc = 0 
			jr z,noloopneeded		; if so no loop needed 
			
			ld a,2 : out (254),a 
			
			; write header 
			
			
outermain:	; how many times we need to loop to upload the data 

			push bc								; push so we can reuse bc  
						
			ld bc, buffersize
			ld (filesize),bc 
			
			;load section 
			call loadfile	  					; loads a <8192> buffersize to loadbuff

			ld hl,loadbuff : ld de,ba64buff
			; hl source de dest bc length of bytes (8192 normally)
			ld a,6: out ($fe),a 
			call encodebase64
			; now bc = number of bytes to send, 
			ld hl,ba64buff : ld (filesize),bc  
			nextreg_nn $7,2
			call senduartmemory 				; need to write; sends it 
			nextreg_nn $7,3
			pop bc 								; pop back 
			;ld a,b
			dec bc 
			push bc 
			; print it 
			; position of chunks 
			ld a,22 : rst 16 : ld a,5  : rst 16 : ld a, 10 : rst 16
			ld h,b : ld l,c : call b2d16 : ld hl,b2dend-5 : call print_rst16 
			
			pop bc 
			ld a,b 
			or c
			jr nz,outermain 

			ld hl,txtfinal : call print_rst16
noloopneeded:
			ld bc,(overrun)						; final bit 
			ld (filesize),bc 
			
			ld hl,bc : call b2d16 : ld hl,b2dend-5 : call print_rst16
			
			ld bc,(overrun)	
			call loadfile
			
			ld hl,loadbuff : ld de,ba64buff
			call encodebase64
			ld hl,ba64buff : ld (filesize),bc  
			nextreg_nn $7,2
			call senduartmemory 				; need to write ; sends it 
			nextreg_nn $7,3
			ld a,$0A : call senduart			; make sure a return was sent 
			ld a,$0A : call senduart			; doubly make sure 

			call setdrv 
						
			call fclose

			ld a,$0d : call senduart			; ctrl + c 
			ld a,$0d : call senduart			; ctrl + c 
			
			; put all the ram back man 
			di 
						
			ld a,(bank3orig) : nextreg_a $53
			ld a,(bank4orig) : nextreg_a $54
			ld a,(bank5orig) : nextreg_a $55
			ld a,(bank6orig) : nextreg_a $56
			ld a,(bank7orig) : nextreg_a $57					
			
finish:		di 	

fixstack:	ld sp,00000		
			pop af : ex af,af' 
			;call freebank						; now safe to freebanks
			pop bc,de,hl : 	exx 
			pop af,bc,de,hl,ix,iy
			ei 
			ret 
			
			db "pisend written by David Saphier / em00k 2020 for SpecNext / NextPi - one love"

			ret

flip		db 0

debugbanks:
			
			ld a,(bank3) : call b2d8 : ld a, 13 : rst 16 
			ld a,(bank4) : call b2d8 : ld a, 13 : rst 16 
			ld a,(bank5) : call b2d8 : ld a, 13 : rst 16 
			ld a,(bank6) : call b2d8 : ld a, 13 : rst 16 
			ld a,(bank7) : call b2d8 : ld a, 13 : rst 16 
			ret 

senduart:
			ld bc,$133b 						; write to uart 
			ld d,a 
.toutb:		; if busy do a little loop 
			in a,(c) : and 2 : jr nz,.toutb
			ld a,d : out (c),a
			ret 

senduartmemory:
			di 
			push bc : push hl : push de 
			xor a : ld (dolinefeed),a 
			ld de,(filesize)					; chunk size 
			ld bc,$133b
outb:	
			in a,(c) : and 2 : jr nz,outb
			ld a,(hl) : inc hl : out (c),a

			and 7 : out (254),a   
			ld a,(dolinefeed)
			inc a
			
			or a 
			jr z,linefeed 
			
			ld (dolinefeed),a 
			jr nolinefeed
			
dolinefeed: db 0 

linefeed: 	in a,(c) : and 2 : jr nz,linefeed
			ld a,$0a : out (c),a
			xor a : ld (dolinefeed),a 			

nolinefeed:			
			dec de : ld a,e : or d : jr nz,outb
			ld a,$0a : out (c),a
			pop de : pop hl : pop bc 
			ret 
			
print_rst16	ld a,(hl):inc hl:or a:ret z:rst 16:

			jr print_rst16

streamuart	ld a,(hl):inc hl:or a:ret z:  

			ld bc,$133b 						; write to uart 
			ld d,a 
.koutb:		; if busy do a little loop 
			in a,(c) : and 2 : jr nz,.koutb : ld a,d : out (c),a
			jr streamuart
					

print_fname	ld a,(hl):inc hl:cp 0:ret z:rst 16:jr print_fname
print_at	ld a,(hl):inc hl:cp $ff:ret z:rst 16:jr print_at
			
print_title	
			ld b,32
printloop:
			ld a,(hl):inc hl : rst 16 : djnz printloop : ld a,13 : rst 16 : ret 
			
openfile:
			push de:call fopen:pop de:ret c:
			ld a,(handle):or a:ret z
			ld bc, 0 : ld de, 0: ld ixl,0
			call fseek
			ret 
loadfile: 	
			ld ix,loadbuff
			ld a,(handle)
			call fread		
			ret 

openuart: 
			ld bc,5435 : ld a,64 : out (c),a 
			;call setbaud115200
			nextreg_nn $a0, $30
			nextreg_nn $a2, $d2			
			call flushuart
			ret 
	
clearuart: 
			; sends a clear command 
			push bc : ld b,200			; this is purely to slow stuff down 
.wtl		ld (tempsup),ix : djnz .wtl : pop bc 
			ld a, 13 : call senduart
			ld a, $03 : call senduart
			ld a, $03 : call senduart
			ret 

TX 	EQU $133B
RX 	EQU $143B

flushuart:
			; KevB https://www.specnext.com/forum/search.php?author_id=1315&sr=posts
			; Empty the 512 BYTE FIFO.
			nextreg_nn $7,0
			ld e,255
flush:		; changed to just keep looping to empty 
			;ld	bc,$143B : ld hl,512				;     ; FIFO (Read 512 BYTES)
fifo:		ld	bc,$143B
			in	d,(c) : in	d,(c)  					; : dec	hl : jr	nz,fifo
			ld	bc,$133B							; TX 
			in	a,(c)								; read 
			and 1 									; bit 0 = 0 fifo is empty or bit 0 = 1 data to get 
			or a   	
			jr nz,fifo 								; no more data we're done 	;;
			dec e 
			or a 
			jr nz,fifo 
			nextreg_nn $7,3
			ret 
waituart:
			ld h,0
waituartlp:
			ld a, $0d : ld bc,$133B : out (c),a 
			in a,(c) : and 1 : cp 1 : jr z,founduart
			
			ld b,25	
ml2:	    halt 
			djnz ml2 
			inc h : ld a,h : cp 10 : jr z,giveup
			out ($fe),a 
			jp waituartlp			

giveup:		ld a,2 : out ($fe),a : ld hl,faileduart : call print_rst16
			jp finish
			
founduart:	ret 		
	
getfilesize: 
			; ix = filespec hl in dotland 
	
nsetdrive:
			ld hl,bufferfs
			ld a,(handle)
			rst $08
			db $a1
			jr nc,successfs
			jr c,failopen
			;a = error code 
			jr donefsizefs
; data

bufferfs:
			defs 11,0
failopen: 
			ld a,1
			out ($fe),a
			jr donefsizefs
successfs: 		
			ld a,4
			out ($fe),a
			jr donefsizefs
donefsizefs:
			ret 

; ------- MEMORY STUFF 
reservebank:
			di
			call getbank : nextreg_a $53  
			call getbank : nextreg_a $54  
			call getbank : nextreg_a $55  
			call getbank : nextreg_a $56  
			call getbank : nextreg_a $57  
			ret 
storebanks:
			di 

			ld a, $53 : call getreg : ld (bank3orig),a	          
			ld a, $54 : call getreg : ld (bank4orig),a	          
			ld a, $55 : call getreg : ld (bank5orig),a	          
			ld a, $56 : call getreg : ld (bank6orig),a	          
			ld a, $57 : call getreg : ld (bank7orig),a
			ret 

.waitkey:	xor a : in a,(0xfe) : cpl : and 15 : jr z,.waitkey	
			
setbanks:	di 
			 
			ld a,(bank3) : nextreg_a $53
			ld a,(bank4) : nextreg_a $54
			ld a,(bank5) : nextreg_a $55
			ld a,(bank6) : nextreg_a $56
			ld a,(bank7) : nextreg_a $57

			ret 
fixbanks:
			di 
			ld a,(bank3) : nextreg_a $53 
			nextreg_nn $54,4
			nextreg_nn $55,5
			nextreg_nn $56,0
			nextreg_nn $57,1
			ret 
getbank:
			; NextZXOSAPI to ask the OS for a free bank 
			;
			ld hl,$0001  	; H=banktype (ZX=0, 1=MMC); L=reason (1=allocate)
			exx
			ld c,7 			; RAM 7 required for most IDEDOS calls
			ld de,$01bd 	; IDE_BANK
			rst $8:defb $94 ; M_P3DOS
			jp nc,failed
			ld a,e 
			jr notfailed
bank:       
			db 223
failed:		; added this for when working in CSpect in
			ld a,34
			;ld hl,bank
			;dec (hl)
notfailed:	ret 			

freebank:
			
			ld a,(bank3) : call free 
			ld a,(bank4) : call free 
			ld a,(bank5) : call free 
			ld a,(bank6) : call free 
			ld a,(bank7) : call free 
			ret 

free:		;ld (freeend+1),sp 
			ld hl,$0003  	; H=banktype (ZX=0, 1=MMC); L=reason (1=allocate)
			ld e,a
			exx
			ld c,7 			; RAM 7 required for most IDEDOS calls
			ld de,$01bd 	; IDE_BANK
			rst $8 : defb $94 ; M_P3DOS
freeend: 	;ld sp,00000
			ret

getreg:		; in register in a, out value in a 
			ld bc,$243B			; Register Select 
			out(c),a			; 
			ld bc,$253B			; reg access 
			in a,(c)
			ret
			
div32_16:
			; https://www.omnimaga.org/asm-language/(z80)-32-bit-by-16-bits-division-and-32-bit-square-root/msg406903/#msg406903
			;this divides hlix by bc
			;the result is stored in hlix, the remainder in de
			;bc is unmodified
			;a is 0
			;it doesnt use any other registers or ram
			ld de,0  ; 10
			ld a,32  ; 7
div32_16loop:
			add ix,ix  ; 15
			adc hl,hl  ; 15
			ex de,hl  ; 4
			adc hl,hl  ; 15
			or a   ; 4
			sbc hl,bc  ; 15
			inc ix   ; 10
			jr nc,cansub  ; 12/7
			add hl,bc  ; 11
			dec ix  ; 10
cansub:
			ex de,hl  ; 4
			dec a   ; 4
			jr nz,div32_16loop ; 12/7
			ret   ; 1
	
border		db 0  
toggle		db 0
 
setdrv		ld a,'*':rst $08:db $89:xor a:ld (handle),a:ret
fopen		ld	b,$01:db 33 ;: ret 
     		;ld	b,$0c:push ix:pop hl:ld a,42:rst $08:db $9a:ld (handle),a:ret
fcreate		ld	b,$0e:push ix:pop hl:ld a,42:rst $08:db $9a:ld (handle),a:ret
fread		push ix:pop hl:db 62
handle		db	0:or a:ret z:rst $08:db $9d:ret
fwrite		push ix:pop hl:ld a,(handle):or a:ret z:rst $08:db $9e:ret
fclose		ld a,(handle):or a:ret z:rst $08:db $9b:ret
fseek		ld a,(handle):rst $08:db $9f:ret;


error_opening 	db 		0 
stackb			dw 		0
filename		ds		32,0
filesize		dw		0000
filesizebytes	ds		32,32
textbuff    	ds 		32,0
version			db		22,21,0,16,3,"2.20",22,0,0,255
emptyline		db		".pisend 2.20 - David Saphier",13,13,"usage :",13,13,"pisend [-q][-c][-s][-r][file]",13,13
				db		"pisend is used for interacting  "
				db		"with NextPi. pisend can base64  "
				db		"encode files and transmit to the"
				db		"pi0, issue commands, read the pi"
				db		"console output etc.             ",13,13
				

linetwo			db 		"use:",13,13
				
				db 		".pisend -c {cmd}",13
				db		" sends a command to the pi0, eg:",13,13
				
				db 		".pisend -c nextpi-tzx_load rbc.tzx ",13
				db 		".pisend -c nextpi-play_sid sing*",13
				
				db 		13,13 

				db 		".pisend -c nextpi-tzx_load rbc.tzx ",13
				db 		".pisend -c nextpi-play_sid sing*",13,13
				
				db 		".pisend -q",13,13
				db		" discovers pi0 and sets baud    ",13
				db 		13,13 
				
				db 		"You must send this -q first to  discover the pi0. Reg $7f",13
				db 		"is set on successful exit",13
				db 		13,13

				db 		".pisend -r",13,13
				db		"flush what is in the uart buffer",13
				db		" to $c000 / 49152 with 512 bytes",13
				db 		13,13 

				db 		"pisend {filename}",13,13
				db		"pisend will base64 encode and   ",13
				db		"trasnmit to the pi0             ",13
				db 		13,13 

				
				db 		"Thanks to kevb, TimG, Big D and all the rest...",13,0
header 			db 		"begin-base64 644 data.uue",$0A
headerend 
trylowboaud		db 		22, 0, 0,"Trying 115,200...",255
tryhighbaud		db 		22, 0, 0,"Trying 2000000...",255
trysuccess		db		22, 0, 0,"COMMS@ "
				db		22, 0, 17,"Connected OK!",255
enablecatcher 	db 		'nextpi-file_stream > "/ram/',0
;;enablecatcher 	db 		"cat > /mnt/dongle/",0
sucess	 		db 		13,13,"success!",13,255
failedsave 	 	db 		13,13,"failed save :(",13,255
stradd			db 		"-q",0
findme			db 		"c:rbc.uue",0,0,0
faileduart		db 		"failed to find uart?",0
txtsize			db		22,4,1,"size   : ",0
txtchunks		db		22,5,1,"chunks :   ",0
txtfinal		db		22,6,1,"final  :     ",0
txtfname		db		22,1,1,"file   :",255
tempatts		db 		0
quietflag		db 		0 		
modaddress 	 	equ $c000
modebuff   	 	db 0,0 	
size 			dw 0
splitsize		dw 0 
copysize		dw 0 
copyaddress		dw 0 
copylength		dw 0 
copyoffset		dw 0 
overrun			dw 0000
				defs 255,0
tempstack		dw $3fff
bank3			db 	35			; $6000
bank4			db 	36			; $8000
bank5			db 	37			; $A000
bank6			db 	38			; $C000
bank7			db 	39			; $E000
bank3orig		db 	35			; $6000
bank4orig		db 	36			; $8000
bank5orig		db 	37			; $A000
bank6orig		db 	38			; $C000
bank7orig		db 	39			; $E000
commandline		dw 0000
failedsup		db 22,10,1,"Unable to read SUP> from Pi!",13,"Is the Pi ready? try .pisend -q"," to clear pi job.",22,1,1," ",0
foundsup		db 22,1,1,"- Connected to NextPi",0
update2mb		db "nextpi-admin_enable",$0d,$0a
				db "cp /boot/cmdline.txt /boot/cmdline.bak",$0d,$0a
				db "echo dwc_otg.lpm_enable=0 console=serial0,2000000 console=tty1 root=PARTUUID=b8ef7a27-02 "
				db "rootfstype=ext4 elevator=deadline fsck.repair=yes rootwait > /boot/cmdline.txt"
				db $0d,$0a
				db "nextpi-admin_disable",$0d,$0a
				db "reboot",$0d,$0a
end2mb
update115		db "nextpi-admin_enable",$0d,$0a
				db "cp /boot/cmdline.txt /boot/cmdline.bak",$0d,$0a
				db "echo dwc_otg.lpm_enable=0 console=serial0,115200 console=tty1 "
				db "root=PARTUUID=b8ef7a27-02 rootfstype=ext4 elevator=deadline fsck.repair=yes rootwait > /boot/cmdline.txt"
				db $0d,$0a
				db "nextpi-admin_disable",$0d,$0a
				db "reboot",$0d,$0a
updatetext		db 13,"Baud update + rebooting pi"
				db 13,"Use .term to confirm",0
end115
_2mbto115:		db 13,"Swapping 2Mbit to 115,200",0
_115to2mb: 		db 13,"Swapping 115,200 to 2Mbit",0
RXSETBAUD EQU 5179

setbaud115200:	
			;XOR A
			
setbaudrate:		
			LD A, (CURBAUD)

;NOW WE CALCULATE THE PRESCALER VALUE TO SET FOR OUR VGA TIMING.

			PUSH AF
   
			POP AF						; A BOARD = 0 			
   
			LD D,0
			SLA A		; *2
			RL D
			SLA A		; *4
			RL D
			SLA A		; *8
			RL D
			SLA A		; *16
			RL D	
			LD E,A		
			LD HL,BaudPrescale	; HL NOW POINTS AT THE BAUD TO USE.
			ADD HL,DE						; VECTOR FOR BAUD VALUES 
			LD BC,9275	;NOW ADJUST FOR THE SET VIDEO TIMING.
			LD A,17			; REG $11 
			OUT (C),A
			LD BC,9531	
			IN A,(C)	;GET TIMING ADJUSTMENT
			LD E,A
			RLC E		;*2 GUARANTEED AS <127
			LD D,0
			ADD HL,DE

			LD E,(HL)
			INC HL
			
			  
			LD D,(HL)
			EX DE,HL

			PUSH HL		; THIS IS PRESCALER		
			PUSH AF		; AND VALUE
						
			LD BC,RXSETBAUD					; 
			LD A,L
			AND %01111111	; RES BIT 7 TO REQUEST WRITE TO LOWER 7 BITS
			OUT (C),A
			LD A,H
			RL L		; BIT 7 IN CARRY
			RLA		; NOW IN BIT 0
			OR %10000000	; SET MSB TO REQUEST WRITE TO UPPER 7 BITS
			OUT (C),A

			POP AF
			LD L,A
			LD H,0

			pop hl
			jp endstuff

BaudPrescale:

			DEFW 243,248,256,260,269,278,286,234 			; Was 0 - 115200 adjust for 0-7
			DEFW 14,14,15,15,16,16,17,14 					;2000000 -14
		
CURBAUD:	
			DEFB 0			;start at 115200
			DEFB 0			;Zero for easy load at 16bits
endstuff:
	ret 
; Encode BASE64.


; > HL = Source
; > DE = Destination
; > BC = Original data length (0 = 65536)

; < HL = Updated
; < DE = Updated
; < BC = Encoded length
	
encodebase64:	
			;dw $01dd
			di
			ld	(labstack+1),sp

			ld	sp,hl			; SP = source
			add	hl,bc
			ld	(labhl+1),hl		; PTR+LEN
			ld	(labde+1),de		; LEN

			dec	bc

labpacket:	
			pop	hl			; Input (WORD)
			ld	a,b
			or	c			; >=1 ?
			jr	nz,labok
			ld	h,a			; NULL

labok:	
			ld	a,l
			ex	af,af'
			ld	a,h
			ld	h,encode256tab/256
			ldi				; Translate>Output
			inc	bc

			ld	l,a
			ex	af,af'
			rra
			rr	l
			rra
			rr	l
			ld	h,encode256tab/256
			ldi				; Translate>Output

		;	------------

			pop	hl			; Input (BYTE)
			dec	sp
			ld	a,b
			or	c
			jp	z,lab2bytes		; Zero?
			bit	7,b
			jp	z,lab3bytes		; Negative?

labpad2:	
			ld	a,'='			; Pad ==
			ld	(de),a
			inc	de
labpad1:	
			ld	a,'='			; Pad =
			ld	(de),a
			inc	de
			jr	labdone

		;	------------

lab2bytes:	
			ld	l,a			; NULL

lab3bytes:	
			ex	af,af'
			ld	h,a
			ex	af,af'
			add	hl,hl
			add	hl,hl
			ld	a,l
			res	7,h
			res	6,h
			ld	l,h
			ld	h,encode64tab/256
			ldi				; Translate>Output

			bit	7,b
			jr	nz,labpad1		; Negative?

			ld	l,a
			ld	h,encode256tab/256

			ld	a,b
			or	c
			ldi				; Translate>Output
			jp	nz,labpacket		; Zero?

		;	------------

labdone:	
			ld	h,d			; Calculate LEN
			ld	l,e
labde:	
			ld	bc,0
			xor	a			; CF=0
			sbc	hl,bc
			ld	b,h
			ld	c,l
labhl:	
			ld	hl,0			; Return PTR

labstack:	
			ld	sp,0
			ei
			ret 

;### clcn32 -> converts 32bit-value in ascii-string (terminated by 0)
;### input      de,ix=32bit value, iy=destination address
;### output     iy=last char in destination string
;### destroyed af,bc,de,hl,ix
; combined routine for conversion of different sized binary numbers into
; directly printable ascii(z)-string
; input value in registers, number size and -related to that- registers to fill
; is selected by calling the correct entry:
;
;  entry  inputregister(s)  decimal value 0 to:
;   b2d8             a                    255  (3 digits)
;   b2d16           hl                  65535   5   "
;   b2d24         e:hl               16777215   8   "
;   b2d32        de:hl             4294967295  10   "
;   b2d48     bc:de:hl        281474976710655  15   "
;   b2d64  ix:bc:de:hl   18446744073709551615  20   "
;
; the resulting string is placed into a small buffer attached to this routine,
; this buffer needs no initialization and can be modified as desired.
; the number is aligned to the right, and leading 0's are replaced with spaces.
; on exit hl points to the first digit, (b)c = number of decimals
; this way any re-alignment / postprocessing is made easy.
; changes: af,bc,de,hl,ix
; p.s. some examples below

; by alwin henseler


b2d8:    	ld h,0
			ld l,a
b2d16:   	ld e,0
b2d24:   	ld d,0
b2d32:   	ld bc,0
b2d48:   	ld ix,0          ; zero all non-used bits
b2d64:   	ld (b2dinv),hl
			ld (b2dinv+2),de
			ld (b2dinv+4),bc
			ld (b2dinv+6),ix ; place full 64-bit input value in buffer
			ld hl,b2dbuf
			ld de,b2dbuf+1
			ld (hl)," "
b2dfilc: equ $-1         ; address of fill-character
			ld bc,18
			ldir            ; fill 1st 19 bytes of buffer with spaces
			ld (b2dend-1),bc ;set bcd value to "0" & place terminating 0
			ld e,1          ; no. of bytes in bcd value
			ld hl,b2dinv+8  ; (address msb input)+1
			ld bc,#0909
			xor a
b2dskp0:	dec b
			jr z,b2dsiz     ; all 0: continue with postprocessing
			dec hl
			or (hl)         ; find first byte <>0
			jr z,b2dskp0
b2dfnd1:	dec c
			rla
			jr nc,b2dfnd1   ; determine no. of most significant 1-bit
			rra
			ld d,a          ; byte from binary input value
b2dlus2:	push hl
			push bc
b2dlus1: 	ld hl,b2dend-1  ; address lsb of bcd value
			ld b,e          ; current length of bcd value in bytes
			rl d            ; highest bit from input value -> carry
b2dlus0: 	ld a,(hl)
			adc a,a
			daa
			ld (hl),a       ; double 1 bcd byte from intermediate result
			dec hl
			djnz b2dlus0    ; and go on to double entire bcd value (+carry!)
			jr nc,b2dnxt
			inc e           ; carry at msb -> bcd value grew 1 byte larger
			ld (hl),1       ; initialize new msb of bcd value
b2dnxt:  	dec c
			jr nz,b2dlus1   ; repeat for remaining bits from 1 input byte
			pop bc          ; no. of remaining bytes in input value
			ld c,8          ; reset bit-counter
			pop hl          ; pointer to byte from input value
			dec hl
			ld d,(hl)       ; get next group of 8 bits
			djnz b2dlus2    ; and repeat until last byte from input value
b2dsiz:  	ld hl,b2dend    ; address of terminating 0
			ld c,e          ; size of bcd value in bytes
			or a
			sbc hl,bc       ; calculate address of msb bcd
			ld d,h
			ld e,l
			sbc hl,bc
			ex de,hl        ; hl=address bcd value, de=start of decimal value
			ld b,c          ; no. of bytes bcd
			sla c           ; no. of bytes decimal (possibly 1 too high)
			ld a,"0"
			rld             ; shift bits 4-7 of (hl) into bit 0-3 of a
			cp "0"          ; (hl) was > 9h?
			jr nz,b2dexph   ; if yes, start with recording high digit
			dec c           ; correct number of decimals
			inc de          ; correct start address
			jr b2dexpl      ; continue with converting low digit
b2dexp:  	rld             ; shift high digit (hl) into low digit of a
b2dexph: 	ld (de),a       ; record resulting ascii-code
			inc de
b2dexpl: 	rld
			ld (de),a
			inc de
			inc hl          ; next bcd-byte
			djnz b2dexp     ; and go on to convert each bcd-byte into 2 ascii
			sbc hl,bc       ; return with hl pointing to 1st decimal
			ret

b2dinv:  	ds 8            ; space for 64-bit input value (lsb first)
b2dbuf:  	ds 20           ; space for 20 decimal digits
b2dend:  	ds 1            ; space for terminating 0

	align 256


; **MUST BE 256 BYTE ALIGNED**


decode256_tab:
	db	0		;     0
	db	0		;     1
	db	0		;     2
	db	0		;     3
	db	0		;     4
	db	0		;     5
	db	0		;     6
	db	0		;     7
	db	0		;     8
	db	0		;     9
	db	0		;     10
	db	0		;     11
	db	0		;     12
	db	0		;     13
	db	0		;     14
	db	0		;     15
	db	0		;     16
	db	0		;     17
	db	0		;     18
	db	0		;     19
	db	0		;     20
	db	0		;     21
	db	0		;     22
	db	0		;     23
	db	0		;     24
	db	0		;     25
	db	0		;     26
	db	0		;     27
	db	0		;     28
	db	0		;     29
	db	0		;     30
	db	0		;     31
	db	0		; " " 32
	db	0		; "!" 33
	db	0		;     34
	db	0		; "#" 35
	db	0		; "$" 36
	db	0		; "%" 37
	db	0		; "" 38
	db	0		; "'" 39
	db	0		; "(" 40
	db	0		; ")" 41
	db	0		; "*" 42
	db	62		; "+" 43
	db	0		; "," 44
	db	0		; "-" 45
	db	0		; "." 46
	db	63		; "/" 47
	db	52		; "0" 48
	db	53		; "1" 49
	db	54		; "2" 50
	db	55		; "3" 51
	db	56		; "4" 52
	db	57		; "5" 53
	db	58		; "6" 54
	db	59		; "7" 55
	db	60		; "8" 56
	db	61		; "9" 57
	db	0		; ":" 58
	db	0		; ";" 59
	db	0		; "<" 60
	db	0		; "=" 61
	db	0		; ">" 62
	db	0		; "?" 63
	db	0		; "@" 64
	db	0		; "A" 65
	db	1		; "B"
	db	2		; "C"
	db	3		; "D"
	db	4		; "E"
	db	5		; "F"
	db	6		; "G"
	db	7		; "H"
	db	8		; "I"
	db	9		; "J"
	db	10		; "K"
	db	11		; "L"
	db	12		; "M"
	db	13		; "N"
	db	14		; "O"
	db	15		; "P"
	db	16		; "Q"
	db	17		; "R"
	db	18		; "S"
	db	19		; "T"
	db	20		; "U"
	db	21		; "V"
	db	22		; "W"
	db	23		; "X"
	db	24		; "Y"
	db	25		; "Z" 90
	db	0		; "(" 91
	db	0		; "\" 92
	db	0		; ")" 93
	db	0		; "^" 94
	db	0		; "_" 95
	db	0		; "`" 96
	db	26		; "a" 97
	db	27		; "b"
	db	28		; "c"
	db	29		; "d"
	db	30		; "e"
	db	31		; "f"
	db	32		; "g"
	db	33		; "h"
	db	34		; "i"
	db	35		; "j"
	db	36		; "k"
	db	37		; "l"
	db	38		; "m"
	db	39		; "n"
	db	40		; "o"
	db	41		; "p"
	db	42		; "q"
	db	43		; "r"
	db	44		; "s"
	db	45		; "t"
	db	46		; "u"
	db	47		; "v"
	db	48		; "w"
	db	49		; "x"
	db	50		; "y"
	db	51		; "z" 122
	db	0		; "{" 123
	db	0		; "|" 124
	db	0		; "}" 125
	db	0		; "~" 126
	db	0		;     127
	ds	128,0		; 128-255


; --------------------------------------------------------------------------


; **MUST BE 256 BYTE ALIGNED**
	ALIGN 256

encode256tab:

 db "AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHHIIIIJJJJKKKKLLLLMMMM"
 db "NNNNOOOOPPPPQQQQRRRRSSSSTTTTUUUUVVVVWWWWXXXXYYYYZZZZ"
 db "aaaabbbbccccddddeeeeffffgggghhhhiiiijjjjkkkkllllmmmm"
 db "nnnnooooppppqqqqrrrrssssttttuuuuvvvvwwwwxxxxyyyyzzzz"
 db "0000111122223333444455556666777788889999++++////"

encode64tab:

 db "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"


; --------------------------------------------------------------------------


			
.endmarker

	savebin "pisend",start1,.endmarker-start1
	savebin "h:/dot/pisend",start1,.endmarker-start1


;-------------------------------

