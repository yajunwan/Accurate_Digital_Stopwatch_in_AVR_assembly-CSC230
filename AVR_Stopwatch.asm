; a3_template.asm
; CSC 230 - Summer 2018
; 
; Starter code for A3.
;
; B. Bird - 07/01/2018

.include "lcd_function_defs.inc"

; Stack pointer and SREG registers (in data space)
.equ SPH_DS = 0x5E
.equ SPL_DS = 0x5D
.equ SREG_DS = 0x5F

; Initial address (16-bit) for the stack pointer
.equ STACK_INIT = 0x21FF

; Definitions for button values from the ADC
; Some boards may use the values in option B
; The code below used less than comparisons so option A should work for both
; Option A (v 1.1)
;.equ ADC_BTN_RIGHT = 0x032
;.equ ADC_BTN_UP = 0x0FA
;.equ ADC_BTN_DOWN = 0x1C2
;.equ ADC_BTN_LEFT = 0x28A
;.equ ADC_BTN_SELECT = 0x352
; Option B (v 1.0)
.equ ADC_BTN_RIGHT = 0x032
.equ ADC_BTN_UP = 0x0C3
.equ ADC_BTN_DOWN = 0x17C
.equ ADC_BTN_LEFT = 0x22B
.equ ADC_BTN_SELECT = 0x316

; Definitions for the analog/digital converter (ADC) (taken from m2560def.inc)
; See the datasheet for details
.equ ADCSRA_DS = 0x7A ; Control and Status Register
.equ ADCSRB_DS = 0x7B ; Control and Status Register
.equ ADMUX_DS = 0x7C ; Multiplexer Register
.equ ADCL_DS = 0x78 ; Output register (high bits)
.equ ADCH_DS = 0x79 ; Output register (low bits)

.equ GTCCR_DS = 0x43
.equ OCR0A_DS = 0x47
.equ OCR0B_DS = 0x48
.equ TCCR0A_DS = 0x44
.equ TCCR0B_DS = 0x45
.equ TCNT0_DS  = 0x46
.equ TIFR0_DS  = 0x35
.equ TIMSK0_DS = 0x6E

.def time_tracker = r19


.cseg
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          Reset/Interrupt Vectors                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.org 0x0000 ; RESET vector
	jmp main_begin
	
; Add interrupt handlers for timer interrupts here. See Section 14 (page 101) of the datasheet for addresses.
.org 0x002a
    jmp TIMER0_OC_ISR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               Main Program                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; According to the datasheet, the last interrupt vector has address 0x0070, so the first
; "unreserved" location is 0x0072
.org 0x0072
main_begin:
    clr r22
	; Initialize the stack
	ldi r16, high(STACK_INIT)
	sts SPH_DS, r16
	ldi r16, low(STACK_INIT)
	sts SPL_DS, r16
	ldi	r16, 0x87
	sts	ADCSRA_DS, r16
	
	; Set up ADCSRB (all bits 0)
	ldi	r16, 0x00
	sts	ADCSRB_DS, r16
	
	; Set up ADMUX (MUX4:MUX0 = 00000, ADLAR = 0, REFS1:REFS0 = 1)
	ldi	r16, 0x40
	sts	ADMUX_DS, r16
	clr time_tracker
	; Initialize the LCD
	call lcd_init
	push r16
	ldi r16,0
	sts current_time,r16
	ldi r16,0
	sts current_time+1,r16
	ldi r16,0
	sts current_time+2,r16
	ldi r16,0
	sts current_time+3,r16
	ldi r16,0
	sts current_time+4,r16
	pop r16
	
	
	ldi YL,low(Display_String)
	ldi YH,high(Display_String)
	
	push r16
    ldi r16,'T'
	st Y+,r16
	ldi r16,'i'
	st Y+,r16
	ldi r16,'m'
	st Y+,r16
	ldi r16,'e'
	st Y+,r16
	ldi r16,':'
	st Y+,r16
	ldi r16,' '
	st Y+,r16
	ldi r16,0
	call GET_DIGIT
	st Y+,r16
	st Y+,r16
	ldi r16,':'
	st Y+,r16
	ldi r16,0
	call GET_DIGIT
	st Y+,r16
	st Y+,r16
	ldi r16,'.'
	st Y+,r16
	ldi r16,0
	call GET_DIGIT
	st Y+,r16
	
	ldi r16,0;store a null pointer at the end of the string
	st Y+,r16
	pop r16
	
	; Set up the LCD to display starting on row 0, column 0
	ldi r16, 0 ; Row number
	push r16
	ldi r16, 0 ; Column number
	push r16
	call lcd_gotoxy
	pop r16
	pop r16
	
	; Display the string
	ldi r16, high(Display_String)
	push r16
	ldi r16, low(Display_String)
	push r16
	call lcd_puts
	pop r16
	pop r16
	clr r18;register for pause or unpause symble
	
	push r20
	ldi r20,0
	sts LAST_LAP_END,r20
	;lds r20,current_time+1
	sts LAST_LAP_END+1,r20
	;lds r20,current_time+2
	sts LAST_LAP_END+2,r20
	;lds r20,current_time+3
	sts LAST_LAP_END+3,r20
	;lds r20,current_time+4
	sts LAST_LAP_END+4,r20
	pop r20
	
	call TIMER0_SETUP ; Set up timer 0 control registers (function below)
	
	sei ; Set the I flag in SREG to enable interrupt processing
button_test_loop:
; Start an ADC conversion
; Set the ADSC bit to 1 in the ADCSRA register to start a conversion
	lds r16, ADCSRA_DS
	ori r16, 0x40
	sts ADCSRA_DS, r16
; Wait for the conversion to finish
wait_for_adc:
	lds r16, ADCSRA_DS
	andi r16, 0x40
	brne wait_for_adc

; Load the ADC result into the X pair (XH:XL). Note that XH and XL are defined above.
	lds XL, ADCL_DS
	lds XH, ADCH_DS
; Compare XH:XL with the threshold in r21:r20
	ldi r20, low(ADC_BTN_UP)
	ldi r21, high(ADC_BTN_UP)
	cp XL, r20 ; Low byte
	cpc XH, r21 ; High byte
	brlo up

	ldi r20, low(ADC_BTN_DOWN)
	ldi r21, high(ADC_BTN_DOWN)
	cp XL, r20 ; Low byte
	cpc XH, r21 ; High byte

	brlo down

	ldi r20, low(ADC_BTN_LEFT)
	ldi r21, high(ADC_BTN_LEFT)
	cp XL, r20 ; Low byte
	cpc XH, r21 ; High byte
    brlo left_plus
	
	ldi r20, low(ADC_BTN_SELECT)
	ldi r21, high(ADC_BTN_SELECT)
	cp XL, r20 ; Low byte
	cpc XH, r21 ; High byte
 	brlo select
	
	ldi r24,0
	rjmp button_test_loop

left_plus:
	jmp left
	
	
stop:
	rjmp stop
	
select:
   cpi r24, 1
	breq button_test_loop
	ldi r24, 1
   ldi r22,4
   cpi r18,0
   breq unpause
   cpi r18,1
   breq pause_mode
unpause:
   ldi r18,1
   ldi r23,1
   rjmp button_test_loop 
pause_mode:
   ldi r18,0
   ldi r23,0
   rjmp button_test_loop   
up:
    cpi r24, 1
	breq back
	ldi r24, 1
    ldi r22,4
	call store
    push r20
    lds r20,current_time
    sts LAST_LAP_END,r20
    lds r20,current_time+1
    sts LAST_LAP_END+1,r20
    lds r20,current_time+2
    sts LAST_LAP_END+2,r20
    lds r20,current_time+3
    sts LAST_LAP_END+3,r20
    lds r20,current_time+4
    sts LAST_LAP_END+4,r20
    pop r20	
	call set_lap
	rjmp button_test_loop
back:
    rjmp button_test_loop
down:
    cpi r24, 1
	breq back
	ldi r24, 1
	ldi XL,low(Display_lap)
	ldi XH,high(Display_lap)
   call clear_lap
   call restore_lap
   rjmp button_test_loop
   
   
left:
    cpi r24, 1
	breq back
	ldi r24, 1
    ldi r22,0
    push r16
	ldi r16,0
	sts current_time,r16
	ldi r16,0
	sts current_time+1,r16
	ldi r16,0
	sts current_time+2,r16
	ldi r16,0
	sts current_time+3,r16
	ldi r16,0
	sts current_time+4,r16
	pop r16
	call set_time
    rjmp button_test_loop	
TIMER0_SETUP:
	push r16
	
	; Control register A
	; We set control register A to 0x02 to enable CTC mode. Note that we can use
	; output compare interrupts without CTC mode, but CTC mode has the benefit
	; of automatically clearing the counter after the TOP value is reached.
	; (See timing diagram in section 16.8, page 124 of the datasheet)
	; The documentation for output compare modes in the datasheet is a bit
	; confusing: note that we enable CTC mode but leave all other bits of 
	; control register A in "normal port operation" (the OC0A and OC0B pins
	; discussed in the documentation are for hardware coupling, not interrupts)
	ldi r16, 0x02
	sts TCCR0A_DS, r16
	
	; Control register B
	; Select prescaler = clock/1024 and all other control bits 0 (see page 126 of the datasheet)
	ldi r16, 0x05 
	sts	TCCR0B_DS, r16
	; Once TCCR0B is set, the timer will begin ticking
	
	; Set OCR0A to the output compare value. This will be the last value that the
	; timer's counter actually holds (it will be cleared on the NEXT incrementation)
	; (There is also a register OCR0B for a second simultaneous output compare)
	ldi r16, 6 ; Set the TOP value to 124 (so 125 increments happen before an interrupt)
	sts OCR0A_DS, r16
	; Question: How many output compare interrupts will occur per second?
	
	; Interrupt mask register (to select which interrupts to enable)
	; We set bit 1 of TIMSK0 to enable output compare interrupt A
	ldi r16, 0x02
	sts TIMSK0_DS, r16
	
	; Interrupt flag register
	; Writing a 1 to bit 0 of this register clears any interrupt state that might
	; already exist (thereby resetting the interrupt state).
	ldi r16, 0x01
	sts TIFR0_DS, r16
		
	
	pop r16
	ret



; TIMER0_OC_ISR()
; ISR for the timer 0 output compare A interrupt.
TIMER0_OC_ISR:
	push r16
	lds r16, SREG_DS ; Load the value of SREG into r16
	push r16 ; Push SREG onto the stack
	push r17
	cpi r22,4
	brne timer0_isr_done
	; Increment the value of OVERFLOW_INTERRUPT_COUNTER
	lds r16, OC_INTERRUPT_COUNTER
	inc r16
	sts OC_INTERRUPT_COUNTER, r16
	
	; If the incrementation did not cause an overflow,
	; we're done.
	brne timer0_isr_done
	add time_tracker,r23
	sts current_time+4,time_tracker
	; Otherwise, 256 interrupts have occurred since the last
	; time we flipped the state, so load the LED_STATE value
	; and flip it
	call check_overflow
    call set_time
	push r16 
	ldi r16,0
	sts OC_INTERRUPT_COUNTER,r16
	pop r16
timer0_isr_done:
	pop r17
	; The next stack value is the value of SREG
	pop r16 ; Pop SREG into r16
	sts SREG_DS, r16 ; Store r16 into SREG
	; Now pop the original saved r16 value
	pop r16

	reti ; Return from interrupt
check_overflow:
    push r20
	lds r20,current_time+4
	cpi r20,0x0a
	pop r20
	breq adjust_time1
	ret
adjust_time1:
    clr time_tracker
    push r20
	clr r20
	sts current_time+4,r20
	lds r20,current_time+3
	inc r20
	sts current_time+3,r20
	pop r20
	push r20
	lds r20,current_time+3
	cpi r20,0x0a
	pop r20
	breq adjust_time2
	rjmp TIMER0_OC_ISR
adjust_time2:                                                                 
    push r20
	clr r20
	sts current_time+3,r20
	lds r20, current_time+2
	inc r20
	sts current_time+2,r20
	pop r20
	push r20
	lds r20,current_time+2
	cpi r20,6
	pop r20
	breq adjust_time3
	rjmp TIMER0_OC_ISR
adjust_time3:
    push r20
	clr r20
	sts current_time+2,r20
	lds r20,current_time+1
	inc r20
	sts current_time+1,r20
	cpi r20,0x0a
	pop r20
	breq adjust_time4
	rjmp TIMER0_OC_ISR
adjust_time4:
    push r20
	clr r20
	sts current_time+2,r20
	lds r20,current_time
	inc r20
	sts current_time,r20
	cpi r20,0x0a
	pop r20
	breq stop1
	rjmp TIMER0_OC_ISR
stop1:
    rjmp stop
set_time:
    ldi YL,low(Display_String)
	ldi YH,high(Display_String)
    push r16
    ldi r16,'T'
	st Y+,r16
	ldi r16,'i'
	st Y+,r16
	ldi r16,'m'
	st Y+,r16
	ldi r16,'e'
	st Y+,r16
	ldi r16,':'
	st Y+,r16
	ldi r16,' '
	st Y+,r16
	lds r16,current_time
	call GET_DIGIT
	st Y+,r16
	lds r16,current_time+1
	call GET_DIGIT
	st Y+,r16
	ldi r16,':'
	st Y+,r16
	lds r16,current_time+2
	call GET_DIGIT
	st Y+,r16
	lds r16,current_time+3
	call GET_DIGIT
	st Y+,r16
	ldi r16,'.'
	st Y+,r16
	lds r16,current_time+4
	call GET_DIGIT
	st Y+,r16
    
	ldi r16,0;store a null pointer at the end of the string
	st Y+,r16
	pop r16
	
	; Set up the LCD to display starting on row 0, column 0
	ldi r16, 0 ; Row number
	push r16
	ldi r16, 0 ; Column number
	push r16
	call lcd_gotoxy
	pop r16
	pop r16
	
	; Display the string
	ldi r16, high(Display_String)
	push r16
	ldi r16, low(Display_String)
	push r16
	call lcd_puts
	pop r16
	pop r16
	
	ret
GET_DIGIT:
	push r17
	
	; The character '0' has ASCII value 48, and the character codes
	; for the other digits follow '0' consecutively, so we can obtain
	; the character code for an arbitrary single digit by simply
	; adding 48 (or just using the constant '0') to the digit.
	ldi r17, '0' ; Could also write "ldi r17, 48"
	add r16, r17
	
	pop r17
	ret
set_lap:
        push r16
    ldi XL,low(Display_lap)
	ldi XH,high(Display_lap)
	
	lds r16,LAST_LAP_START
	call GET_DIGIT
	st X+,r16
	lds r16,LAST_LAP_START+1
	call GET_DIGIT
	st X+,r16
	ldi r16,':'
	st X+,r16
	lds r16,LAST_LAP_START+2
	call GET_DIGIT
	st X+,r16
	lds r16,LAST_LAP_START+3
	call GET_DIGIT
	st X+,r16
	ldi r16,'.'
	st X+,r16
	lds r16,LAST_LAP_START+4
	call GET_DIGIT
	st X+,r16
	ldi r16,' '
	st X+,r16
	lds r16,LAST_LAP_END
	call GET_DIGIT
	st X+,r16
	lds r16,LAST_LAP_END+1
	call GET_DIGIT
	st X+,r16
	ldi r16,':'
	st X+,r16
	lds r16,LAST_LAP_END+2
	call GET_DIGIT
	st X+,r16
	lds r16,LAST_LAP_END+3
	call GET_DIGIT
	st X+,r16
	ldi r16,'.'
	st X+,r16
	lds r16,LAST_LAP_END+4
	call GET_DIGIT
	st X+,r16
	
	ldi r16,0;store a null pointer at the end of the string
	st X+,r16
	pop r16
	
	; Set up the LCD to display starting on row 0, column 0
	ldi r16, 1 ; Row number
	push r16
	ldi r16, 0 ; Column number
	push r16
	call lcd_gotoxy
	pop r16
	pop r16
	
	; Display the string
	ldi r16, high(Display_lap)
	push r16
	ldi r16, low(Display_lap)
	push r16
	call lcd_puts
	pop r16
	pop r16	
	ret
	
store:
    push r20
	lds r20,LAST_LAP_END
	sts LAST_LAP_START,r20
	lds r20,LAST_LAP_END+1
	sts LAST_LAP_START+1,r20
	lds r20,LAST_LAP_END+2
	sts LAST_LAP_START+2,r20
	lds r20,LAST_LAP_END+3
	sts LAST_LAP_START+3,r20
	lds r20,LAST_LAP_END+4
	sts LAST_LAP_START+4,r20
	pop r20
	ret
	
clear_lap:
    push r25
    ldi r25,' '
	st X+,r25
	ldi r25,' '
	st X+,r25
	ldi r25,' '
	st X+,r25
	ldi r25,' '
	st X+,r25
	ldi r25,' '
	st X+,r25
	ldi r25,' '
	st X+,r25
	ldi r25,' '
	st X+,r25
	ldi r25,' '
	st X+,r25
	ldi r25,' '
	st X+,r25
	ldi r25,' '
	st X+,r25
	ldi r25,' '
	st X+,r25
	ldi r25,' '
	st X+,r25
	ldi r25,' '
	st X+,r25
	ldi r25,' '
	st X+,r25
	ldi r25,' '
	st X+,r25
done_clear:
    ldi r25,0
	st X+,r25
	ldi r16, 1 ; Row number
	push r16
	ldi r16, 0 ; Column number
	push r16
	call lcd_gotoxy
	pop r16
	pop r16
	
	; Display the string
	ldi r16, high(Display_lap)
	push r16
	ldi r16, low(Display_lap)
	push r16
	call lcd_puts
	pop r16
	pop r16	
	pop r25
    ret
restore_lap:
    push r20
	ldi r20,0
	sts LAST_LAP_END,r20
	;lds r20,current_time+1
	sts LAST_LAP_END+1,r20
	;lds r20,current_time+2
	sts LAST_LAP_END+2,r20
	;lds r20,current_time+3
	sts LAST_LAP_END+3,r20
	;lds r20,current_time+4
	sts LAST_LAP_END+4,r20
	pop r20
	ret
; Include LCD library code
.include "lcd_function_code.asm"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               Data Section                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.dseg
; Note that no .org 0x200 statement should be present
; Put variables and data arrays here...
Display_String: .byte 200
Display_lap: .byte 200	
current_time: .byte 200
ignore_ind: .byte 1
OC_INTERRUPT_COUNTER: .byte 1 
LAST_LAP_START: .byte 200
LAST_LAP_END: .byte 200
save_tracker: .byte 1
