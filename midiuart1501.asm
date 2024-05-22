;;; 80 characters wide please ;;;;;;;;;;;;;;;;;;;;;;;;;; 8-space tabs please ;;;


;
;;;
;;;;;  Converter between MIDI and 9600 baud UART
;;;
;


;;; Connections ;;;

;;;                                                          ;;;
;                          .--------.                          ;
;                  Supply -|01 \/ 08|- Ground                  ;
;        MIDI Tx <--- RA5 -|02    07|- RA0 <--- UART Rx        ;
;    MIDI Rx LED <--- RA4 -|03    06|- RA1 ---> UART Rx LED    ;
;        MIDI Rx ---> RA3 -|04    05|- RA2 ---> UART Tx        ;
;                          '--------'                          ;
;                                                              ;
;    LEDs are active low.                                      ;
;                                                              ;
;;;                                                          ;;;


;;; Assembler Directives ;;;

	list		P=PIC12F1501, F=INHX32, ST=OFF, MM=OFF, R=DEC, X=ON
	#include	P12F1501.inc
	errorlevel	-302	;Suppress "register not in bank 0" messages
	errorlevel	-224	;Suppress TRIS instruction not recommended msgs
	__config	_CONFIG1, _FOSC_INTOSC & _WDTE_OFF & _PWRTE_ON & _MCLRE_OFF & _CP_OFF & _BOREN_OFF & _CLKOUTEN_OFF
			;_FOSC_INTOSC	Internal oscillator, I/O on RA5
			;_WDTE_OFF	Watchdog timer disabled
			;_PWRTE_ON	Keep in reset for 64 ms on start
			;_MCLRE_OFF	RA3/!MCLR is RA3
			;_CP_OFF	Code protection off
			;_BOREN_OFF	Brownout reset off
			;_CLKOUTEN_OFF	CLKOUT disabled, I/O on RA4
	__config	_CONFIG2, _WRT_OFF & _STVREN_OFF & _BORV_LO & _LPBOR_OFF &_LVP_OFF
			;_WRT_OFF	Write protection off
			;_STVREN_OFF	Stack over/underflow DOES NOT reset
			;_BORV_LO	Brownout reset voltage low trip point
			;_LPBOR_OFF	Low power brownout reset disabled
			;_LVP_OFF	High-voltage on Vpp to program


;;; Macros ;;;

DELAY	macro	value		;Delay 3*W cycles, set W to 0
	movlw	value
	decfsz	WREG,F
	bra	$-1
	endm

DNOP	macro
	bra	$+1
	endm


;;; Constants ;;;

;Pin Assignments
UAR_PIN	equ	RA0	;UART receiver pin
UAR_LED	equ	RA1	;UART receiver LED pin
MIR_PIN	equ	RA3	;MIDI receiver pin
MIR_LED	equ	RA4	;MIDI receiver LED pin

;FLAGS:
UARFULL	equ	7	;UART receiver buffer is full


;;; Variable Storage ;;;

	cblock	0x70	;Bank-common registers
	
	FLAGS	;You've got to have flags
	UARFSAP	;UART receiver FSA pointer
	UATFSAP	;UART transmitter FSA pointer
	MIRFSAP	;MIDI receiver FSA pointer
	MITFSAP	;MIDI transmitter FSA pointer
	UARBUF	;UART receiver buffer
	X9
	X8
	X7
	X6
	X5
	X4
	X3
	X2
	X1
	X0
	
	endc


;;; Vectors ;;;

	org	0x0		;Reset vector
	goto	Init

	org	0x4		;Interrupt vector
	;fall through


;;; Interrupt Handler ;;;

Interrupt
	movlb	0		;If Timer2 has overflowed, MIDI Tx DFF has
	btfsc	PIR1,TMR2IF	; clocked the waiting bit, go load the next one
	bra	IntMit		; "
	btfsc	INTCON,TMR0IF	;If Timer0 has overflowed and its interrupt is
	btfss	INTCON,TMR0IE	; enabled, go sample the MIDI Rx pin
	bra	$+2		; "
	bra	IntMir		; "
	btfsc	PIR2,NCO1IF	;If the NCO has overflowed, UART Tx DFF has
	bra	IntUat		; clocked the waiting bit, go load the next one
	btfsc	PIR1,TMR1IF	;If Timer1 has overflowed, go sample the UART Rx
	bra	IntUar		; pin
	movlb	7		;If the IOC flag is set on the MIDI Rx pin, a
	btfsc	IOCAF,MIR_PIN	; new byte is beginning, so get ready
	bra	IntMirStart	; "
	btfsc	IOCAF,UAR_PIN	;If the IOC flag is set on the UART Rx pin, a
	bra	IntUarStart	; new byte is beginning, so get ready
	bra	$		;We should never get here

IntMit
	bcf	PIR1,TMR2IF	;Clear the interrupt
	movf	MITFSAP,W	;Resume the MIDI transmitter state machine
	callw			; "
	movwf	MITFSAP		;Save the returned pointer value
	bsf	INTCON,GIE	;Wait for the next interrupt
	bra	$		; "

IntMir
	bcf	INTCON,TMR0IF	;Clear the interrupt
	movlw	0x82		;Subtract 126 from Timer0 (128 cycles is one bit
	addwf	TMR0,F		; time at 31250 baud, minus two cycles)
	movf	MIRFSAP,W	;Resume the MIDI receiver state machine
	callw			; "
	movwf	MIRFSAP		;Save the returned pointer value
	bsf	INTCON,GIE	;Wait for the next interrupt
	bra	$		; "

IntUat
	bcf	PIR2,NCO1IF	;Clear the interrupt
	movf	UATFSAP,W	;Resume the UART transmitter state machine
	callw			; "
	movwf	UATFSAP		;Save the returned pointer value
	bsf	INTCON,GIE	;Wait for the next interrupt
	bra	$		; "

IntUar
	bcf	T1CON,TMR1ON	;Stop Timer1 so we can update it
	movlw	0x64		;Subtract 412 from Timer1 (417 cycles is one bit
	addwf	TMR1L,F		; time at 9600 baud, minus five cycles because
	movlw	0xFE		; we stop Timer1 for five cycles), this sets up
	addwfc	TMR1H,F		; the correct delay until the next bit
	bsf	T1CON,TMR1ON	;Resume Timer1
	bcf	PIR1,TMR1IF	;Clear the interrupt
	movf	UARFSAP,W	;Resume the UART receiver state machine
	callw			; "
	movwf	UARFSAP		;Save the returned pointer value
	bsf	INTCON,GIE	;Wait for the next interrupt
	bra	$		; "

IntMirStart
	bcf	IOCAN,MIR_PIN	;Disable the interrupt
	bcf	IOCAF,MIR_PIN	;Clear the interrupt
	movlb	0		;Set Timer0 to interrupt in the middle of the
	movlw	0x6A		; LSB or so
	movwf	TMR0		; "
	bcf	INTCON,TMR0IF	;Clear and enable the Timer0 interrupt to sample
	bsf	INTCON,TMR0IE	; the MIDI Rx pin
	movlb	2		;Turn on MIDI Rx LED
	bcf	LATA,MIR_LED	; "
	bsf	INTCON,GIE	;Wait for the next interrupt
	bra	$		; "

IntUarStart
	bcf	IOCAN,UAR_PIN	;Disable the interrupt
	bcf	IOCAF,UAR_PIN	;Clear the interrupt
	movlb	0		;Set Timer1 to interrupt in the middle of the
	movlw	0xBA		; LSB or so
	movwf	TMR1L		; "
	movlw	0xFD		; "
	movwf	TMR1H		; "
	bsf	T1CON,TMR1ON	; "
	bcf	PIR1,TMR1IF	;Clear and enable the Timer1 interrupt to sample
	movlb	1		; the UART Rx pin
	bsf	PIE1,TMR1IE	; "
	movlb	2		;Turn on UART Rx LED
	bcf	LATA,UAR_LED	; "
	bsf	INTCON,GIE	;Wait for the next interrupt
	bra	$		; "


;;; Initialization ;;;

Init
	banksel	OSCCON		;16 MHz high-freq internal oscillator
	movlw	B'01111000'
	movwf	OSCCON

	banksel	IOCAN		;Rx pins interrupt on falling edge
	movlw	(1 << MIR_PIN) | (1 << UAR_PIN)
	movwf	IOCAN

	banksel	CLC1CON		;CLC1 (RA2, UART Tx) is a DFF clocked by the NCO
	clrf	CLC1SEL0	; (its input is CLC1POL[1]); CLC2 (RA5, MIDI Tx)
	clrf	CLC1SEL1	; is a DFF clocked by Timer2 (its input is
	movlw	B'00000010'	; CLC2POL[1])
	movwf	CLC1POL
	movlw	B'01000000'
	movwf	CLC1GLS0
	clrf	CLC1GLS1
	clrf	CLC1GLS2
	clrf	CLC1GLS3
	movlw	B'11000100'
	movwf	CLC1CON
	movlw	B'00000111'
	movwf	CLC2SEL0
	clrf	CLC2SEL1
	movlw	B'00000010'
	movwf	CLC2POL
	movwf	CLC2GLS0
	clrf	CLC2GLS1
	clrf	CLC2GLS2
	clrf	CLC2GLS3
	movlw	B'11000100'
	movwf	CLC2CON

	banksel	NCO1CON		;NCO accumulator increments by 629 in pulse
	movlw	B'10000000'	; mode, resulting in a clock of approximately
	movwf	NCO1CLK		; 9600 Hz, with a pulse width of 16 clocks of
	movlw	0x02		; the 16 MHz oscillator, interrupting on the
	movwf	NCO1INCH	; rising edge
	movlw	0x75
	movwf	NCO1INCL
	movlw	B'10000001'
	movwf	NCO1CON

	banksel	OPTION_REG	;Timer0 ticks with instruction clock
	movlw	B'11011111'
	movwf	OPTION_REG

	banksel	T2CON		;Timer2 ticks with instruction clock, interrupts
	movlw	127		; every 128 ticks
	movwf	PR2
	movlw	B'00000100'
	movwf	T2CON

	banksel	ANSELA		;All pins digital, not analog
	clrf	ANSELA

	banksel	LATA		;Default state of output pins is high
	movlw	B'00111111'
	movwf	LATA

	banksel	TRISA		;Rx pins inputs, all others outputs
	movlw	(1 << UAR_PIN) | (1 << MIR_PIN)
	movwf	TRISA

	movlw	0x20		;Initialize key globals
	movwf	FSR0H
	movwf	FSR1H
	clrf	FSR0L
	clrf	FSR1L
	clrf	FLAGS
	movlp	high UarFsaBit0
	movlw	low UarFsaBit0
	movwf	UARFSAP
	movlw	low UatFsaWait
	movwf	UATFSAP
	movlw	low MirFsaBit0
	movwf	MIRFSAP
	movlw	low MitFsaWait
	movwf	MITFSAP

	movlw	1 << NCO1IE	;Interrupt subsystem, peripheral interrupts, NCO
	movwf	PIE2		; interrupt, Timer2 interrupt, and IOC interrupt
	movlw	1 << TMR2IE	; on
	movwf	PIE1
	movlw	B'11001000'
	movwf	INTCON

	bra	$		;Wait for first interrupt


;;; State Machines ;;;

	org	0x100

UarFsaBit0
	bcf	UARBUF,0	;Load current state of UART Rx pin into buffer
	btfsc	PORTA,UAR_PIN	; "
	bsf	UARBUF,0	; "
	retlw	low UarFsaBit1	;Transition to load next bit next time

UarFsaBit1
	bcf	UARBUF,1	;Load current state of UART Rx pin into buffer
	btfsc	PORTA,UAR_PIN	; "
	bsf	UARBUF,1	; "
	retlw	low UarFsaBit2	;Transition to load next bit next time

UarFsaBit2
	bcf	UARBUF,2	;Load current state of UART Rx pin into buffer
	btfsc	PORTA,UAR_PIN	; "
	bsf	UARBUF,2	; "
	retlw	low UarFsaBit3	;Transition to load next bit next time

UarFsaBit3
	bcf	UARBUF,3	;Load current state of UART Rx pin into buffer
	btfsc	PORTA,UAR_PIN	; "
	bsf	UARBUF,3	; "
	retlw	low UarFsaBit4	;Transition to load next bit next time

UarFsaBit4
	bcf	UARBUF,4	;Load current state of UART Rx pin into buffer
	btfsc	PORTA,UAR_PIN	; "
	bsf	UARBUF,4	; "
	retlw	low UarFsaBit5	;Transition to load next bit next time

UarFsaBit5
	bcf	UARBUF,5	;Load current state of UART Rx pin into buffer
	btfsc	PORTA,UAR_PIN	; "
	bsf	UARBUF,5	; "
	retlw	low UarFsaBit6	;Transition to load next bit next time

UarFsaBit6
	bcf	UARBUF,6	;Load current state of UART Rx pin into buffer
	btfsc	PORTA,UAR_PIN	; "
	bsf	UARBUF,6	; "
	retlw	low UarFsaBit7	;Transition to load last bit next time

UarFsaBit7
	bcf	UARBUF,7	;Load current state of UART Rx pin into buffer
	btfsc	PORTA,UAR_PIN	; "
	bsf	UARBUF,7	; "
	bsf	FLAGS,UARFULL	;Signal to MIDI transmitter that a byte is ready
	bcf	T1CON,TMR1ON	;Stop Timer1 and disable its interrupt
	movlb	1		; "
	bcf	PIE1,TMR1IE	; "
	movlb	7		;Reenable UART receiver falling edge interrupt
	bsf	IOCAN,UAR_PIN	; so we're ready for next byte
	movlb	2		;Turn off UART Rx LED
	bsf	LATA,UAR_LED	; "
	retlw	low UarFsaBit0	;Transition to load first bit next time

UatFsaWait
	movf	FSR0L,W		;If MIDI receiver queue is empty, leave the 1
	xorwf	FSR1L,W		; bit that's in the DFF where it is and wait
	btfsc	STATUS,Z	; another bit time
	retlw	low UatFsaWait	; "
	movlb	30		;Load a 0 start bit to be transmitted and
	bcf	CLC1POL,1	; transition to send bit 0 next time
	retlw	low UatFsaBit0	; "

UatFsaBit0
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC1POL,1	; "
	btfsc	INDF0,0		; "
	bsf	CLC1POL,1	; "
	retlw	low UatFsaBit1	;Transition to load next bit next time

UatFsaBit1
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC1POL,1	; "
	btfsc	INDF0,1		; "
	bsf	CLC1POL,1	; "
	retlw	low UatFsaBit2	;Transition to load next bit next time

UatFsaBit2
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC1POL,1	; "
	btfsc	INDF0,2		; "
	bsf	CLC1POL,1	; "
	retlw	low UatFsaBit3	;Transition to load next bit next time

UatFsaBit3
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC1POL,1	; "
	btfsc	INDF0,3		; "
	bsf	CLC1POL,1	; "
	retlw	low UatFsaBit4	;Transition to load next bit next time

UatFsaBit4
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC1POL,1	; "
	btfsc	INDF0,4		; "
	bsf	CLC1POL,1	; "
	retlw	low UatFsaBit5	;Transition to load next bit next time

UatFsaBit5
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC1POL,1	; "
	btfsc	INDF0,5		; "
	bsf	CLC1POL,1	; "
	retlw	low UatFsaBit6	;Transition to load next bit next time

UatFsaBit6
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC1POL,1	; "
	btfsc	INDF0,6		; "
	bsf	CLC1POL,1	; "
	retlw	low UatFsaBit7	;Transition to load last bit next time

UatFsaBit7
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC1POL,1	; "
	btfsc	INDF0,7		; "
	bsf	CLC1POL,1	; "
	retlw	low UatFsaStop	;Transition to load stop bit next time

UatFsaStop
	movlb	30		;Load stop bit
	bsf	CLC1POL,1	; "
	incf	FSR0L,F		;Advance and loop the queue pop pointer
	bcf	FSR0L,5		; "
	retlw	low UatFsaWait	;Transition to wait for next byte next time

MirFsaBit0
	bcf	INDF1,0		;Load current state of MIDI Rx pin into buffer
	btfsc	PORTA,MIR_PIN	; "
	bsf	INDF1,0		; "
	retlw	low MirFsaBit1	;Transition to load next bit next time

MirFsaBit1
	bcf	INDF1,1		;Load current state of MIDI Rx pin into buffer
	btfsc	PORTA,MIR_PIN	; "
	bsf	INDF1,1		; "
	retlw	low MirFsaBit2	;Transition to load next bit next time

MirFsaBit2
	bcf	INDF1,2		;Load current state of MIDI Rx pin into buffer
	btfsc	PORTA,MIR_PIN	; "
	bsf	INDF1,2		; "
	retlw	low MirFsaBit3	;Transition to load next bit next time

MirFsaBit3
	bcf	INDF1,3		;Load current state of MIDI Rx pin into buffer
	btfsc	PORTA,MIR_PIN	; "
	bsf	INDF1,3		; "
	retlw	low MirFsaBit4	;Transition to load next bit next time

MirFsaBit4
	bcf	INDF1,4		;Load current state of MIDI Rx pin into buffer
	btfsc	PORTA,MIR_PIN	; "
	bsf	INDF1,4		; "
	retlw	low MirFsaBit5	;Transition to load next bit next time

MirFsaBit5
	bcf	INDF1,5		;Load current state of MIDI Rx pin into buffer
	btfsc	PORTA,MIR_PIN	; "
	bsf	INDF1,5		; "
	retlw	low MirFsaBit6	;Transition to load next bit next time

MirFsaBit6
	bcf	INDF1,6		;Load current state of MIDI Rx pin into buffer
	btfsc	PORTA,MIR_PIN	; "
	bsf	INDF1,6		; "
	retlw	low MirFsaBit7	;Transition to load last bit next time

MirFsaBit7
	bcf	INDF1,7		;Load current state of MIDI Rx pin into buffer
	btfsc	PORTA,MIR_PIN	; "
	bsf	INDF1,7		; "
	incf	FSR1L,F		;Advance and wrap the queue push pointer
	bcf	FSR1L,5		; "
	bcf	INTCON,TMR0IE	;Disable the Timer0 interrupt
	movlb	7		;Reenable MIDI receiver falling edge interrupt
	bsf	IOCAN,MIR_PIN	; so we're ready for next byte
	movlb	2		;Turn off MIDI Rx LED
	bsf	LATA,MIR_LED	; "
	retlw	low MirFsaBit0	;Transition to load first bit next time

MitFsaWait
	btfss	FLAGS,UARFULL	;If UART receiver buffer is empty, leave 1 bit
	retlw	low MitFsaWait	; in DFF where it is and wait another bit time
	movlb	30		;Load a 0 start bit to be transmitted and
	bcf	CLC2POL,1	; transition to send bit 0 next time
	retlw	low MitFsaBit0	; "

MitFsaBit0
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC2POL,1	; "
	btfsc	UARBUF,0	; "
	bsf	CLC2POL,1	; "
	retlw	low MitFsaBit1	;Transition to load next bit next time

MitFsaBit1
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC2POL,1	; "
	btfsc	UARBUF,1	; "
	bsf	CLC2POL,1	; "
	retlw	low MitFsaBit2	;Transition to load next bit next time

MitFsaBit2
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC2POL,1	; "
	btfsc	UARBUF,2	; "
	bsf	CLC2POL,1	; "
	retlw	low MitFsaBit3	;Transition to load next bit next time

MitFsaBit3
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC2POL,1	; "
	btfsc	UARBUF,3	; "
	bsf	CLC2POL,1	; "
	retlw	low MitFsaBit4	;Transition to load next bit next time

MitFsaBit4
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC2POL,1	; "
	btfsc	UARBUF,4	; "
	bsf	CLC2POL,1	; "
	retlw	low MitFsaBit5	;Transition to load next bit next time

MitFsaBit5
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC2POL,1	; "
	btfsc	UARBUF,5	; "
	bsf	CLC2POL,1	; "
	retlw	low MitFsaBit6	;Transition to load next bit next time

MitFsaBit6
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC2POL,1	; "
	btfsc	UARBUF,6	; "
	bsf	CLC2POL,1	; "
	retlw	low MitFsaBit7	;Transition to load last bit next time

MitFsaBit7
	movlb	30		;Copy bit from buffer into DFF
	bcf	CLC2POL,1	; "
	btfsc	UARBUF,7	; "
	bsf	CLC2POL,1	; "
	retlw	low MitFsaStop	;Transition to load stop bit next time

MitFsaStop
	movlb	30		;Load stop bit
	bsf	CLC2POL,1	; "
	bcf	FLAGS,UARFULL	;Clear the UART receiver buffer full flag
	retlw	low MitFsaWait	;Transition to wait for next byte next time


;;; End of Program ;;;
	end
