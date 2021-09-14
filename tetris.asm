  ;; game state memory location
  .equ T_X, 0x1000                  ; falling tetrominoe position on x
  .equ T_Y, 0x1004                  ; falling tetrominoe position on y
  .equ T_type, 0x1008               ; falling tetrominoe type
  .equ T_orientation, 0x100C        ; falling tetrominoe orientation
  .equ SCORE,  0x1010               ; score
  .equ GSA, 0x1014                  ; Game State Array starting address
  .equ SEVEN_SEGS, 0x1198           ; 7-segment display addresses
  .equ LEDS, 0x2000                 ; LED address
  .equ RANDOM_NUM, 0x2010           ; Random number generator address
  .equ BUTTONS, 0x2030              ; Buttons addresses

  ;; type enumeration
  .equ C, 0x00
  .equ B, 0x01
  .equ T, 0x02
  .equ S, 0x03
  .equ L, 0x04

  ;; GSA type
  .equ NOTHING, 0x0
  .equ PLACED, 0x1
  .equ FALLING, 0x2

  ;; orientation enumeration
  .equ N, 0
  .equ E, 1
  .equ So, 2
  .equ W, 3
  .equ ORIENTATION_END, 4

  ;; collision boundaries
  .equ COL_X, 4
  .equ COL_Y, 3

  ;; Rotation enumeration
  .equ CLOCKWISE, 0
  .equ COUNTERCLOCKWISE, 1

  ;; Button enumeration
  .equ moveL, 0x01
  .equ rotL, 0x02
  .equ reset, 0x04
  .equ rotR, 0x08
  .equ moveR, 0x10
  .equ moveD, 0x20

  ;; Collision return ENUM
  .equ W_COL, 0
  .equ E_COL, 1
  .equ So_COL, 2
  .equ OVERLAP, 3
  .equ NONE, 4

  ;; start location
  .equ START_X, 6
  .equ START_Y, 1

  ;; game rate of tetrominoe falling down (in terms of game loop iteration)
  .equ RATE, 5

  ;; standard limits
  .equ X_LIMIT, 12
  .equ Y_LIMIT, 8


  ;; TODO Insert your code here
addi sp,zero,0x18D0


jmpi main


; BEGIN:clear_leds
clear_leds:
addi t1, zero, LEDS              ; Temp value to get memory address
stw zero, 0(t1)          ; LED[0] to 0
stw zero, 4(t1)            ; LED[1] to 0
stw zero, 8(t1)             ; LED[2] to 0
ret
; END:clear_leds




; BEGIN:set_pixel
	set_pixel:
		add t0,a0,zero		;; t0=x
		add t4,a0,zero		;;t4=copie de l'arg x
		add t1,a1,zero		;;t1=y
		addi t5,zero,1		;;t5=1
		srli t0, t0, 2		;; t0=x/4
		slli t0, t0, 2
		addi t2,t0,LEDS		;;t2= l'addresse du mot ou se trouve le pixel
		ldw t3,0 (t2)		;;t3=mot ou se trouve le pixel
		slli t4,t4,3		;; t4=x*8
		add t4,t4,t1		;;t4=x*8+y
		andi t4,t4,31		;; t4 mod 32 = index du pixel
		sll t5,t5,t4		;; place le 1 au bon index
		or t3,t3,t5			;; mise a 1 du bit
		stw t3,0 (t2)		;; on remet le mot modifiÈ		
	ret
; END:set_pixel

; BEGIN:wait
	wait:
		addi t0, zero, 1       ; Initialize the counter 2^20
		slli t0,t0,20
	counter:                      ;   ////////// 2^20 = nb d'instructions ou nb de loop ?
		addi t0, t0, -1
		bne t0, zero, counter         ; If neq to zero, go back in loop
	ret
; END:wait

; BEGIN:set_gsa
set_gsa:
    add t0, a0, zero        ; t0 = x
    slli t0, t0, 3          ; t0 = 8*x
    add t0, t0, a1          ; t0 = 8*x + y
    slli t0, t0, 2          ; t0 = (8*x + y)*4
    stw a2, GSA(t0)         ; store word in address GSA + t0
	ret
; END:set_gsa

; BEGIN:in_gsa
in_gsa:
    cmplti t1, a0, 0            ; t1 = 1 if a0 < 0 else 0
    cmpgei t2, a0, 12           ; t2 = 1 if a0 >= 12 else 0
    cmplti t3, a1, 0            ; t3 = 1 if a1 < 0 else 0
    cmpgei t4, a1, 8            ; t4 = 1 if a1 >= 8 else 0
    add t1, t1, t2              ;
    add t1, t1, t3              ; t1 = t1 + t2 + t3 + t4
    add t1, t1, t4              ;
    cmpne v0, t1, zero         ; v0 = 1 if t1 != 0 else 0, 1 if not in GSA else 0
    ret
; END:in_gsa


; BEGIN:get_gsa
get_gsa:
    add t0, a0, zero        ; t0 = x
    slli t0, t0, 3          ; t0 = 8*x
    add t0, t0, a1          ; t0 = 8*x + y
    slli t0, t0, 2          ; t0 = (8*x + y)*4
    ldw v0, GSA(t0)         ; load word in address 0x1014 (GSA) + t0
    ret
; END:get_gsa



;BEGIN:draw_gsa
draw_gsa:
	addi sp, sp, -12
	stw ra, 0(sp)
	stw s0, 4(sp)
	stw s1, 8(sp)
    call clear_leds
    addi s0, zero, 11       ; x axis
    addi s1, zero, 7        ; y axis
    jmpi drawloop
drawinit:
    addi s0, s0, -1
    cmplt t3, s0, zero
    bne t3, zero, drawend 
    addi s1, zero, 7
drawloop:
    cmplt t3, s1, zero
    bne t3, zero, drawinit
	add a0, s0, zero
	add a1, s1, zero
    call get_gsa
    beq v0, zero, intermedraw
	add a0, s0, zero
	add a1, s1, zero
    call set_pixel
intermedraw:
    addi s1, s1, -1
    jmpi drawloop
drawend:
	ldw ra,0(sp)
	ldw s0, 4(sp)
	ldw s1, 8(sp)
	addi sp, sp, 12
	ret
; END:draw_gsa

; BEGIN:draw_tetromino
	draw_tetromino:
		addi sp,sp,-4
		stw ra,0(sp)
		addi sp,sp,-4
		stw a0,0(sp)
		addi sp,sp,-4
		stw s0,0(sp)
		addi sp,sp,-4
		stw s1,0(sp)
		addi sp,sp,-4
		stw s2,0(sp)
		addi sp,sp,-4
		stw s3,0(sp)
		addi sp,sp,-4
		stw s4,0(sp)
		addi sp,sp,-4
		stw s5,0(sp)
		addi sp,sp,-4
		stw s6,0(sp)
		addi sp,sp,-4
		stw s7,0(sp)

		ldw s0,T_X(zero)
		ldw s1,T_Y(zero)
		ldw s2,T_orientation(zero)
		ldw s3,T_type(zero)
		add s7,a0,zero			;; s7=a0

		addi s4,zero,DRAW_Ax
		addi s5,zero,DRAW_Ay
		slli t5,s3,4			;;type * 16
		add s4,s4,t5
		add s5,s5,t5
		slli t5,s2,2			;; orientation * 4
		add s4,s4,t5			;; s4= adresse de l'adresse du 1er offset pour x 
		add s5,s5,t5			;; s5= adresse de l'adresse du 1er offset pour y 
		add t1,zero,s4
		add t2,zero,s5
		ldw s4,0(t1)			;; adresse du 1er offset de x
		ldw s5,0(t2)			;; adresse du 1er offset de y

		add a0,zero,s0
		add a1,zero,s1
		add a2,zero,s7
		call set_gsa

		addi s6,zero,0			;; s6 devient un compteur
		addi s3,zero,3			;; s3 devient un la limite de la boucle

	loop_draw:
		beq s6,s3, end_draw
		ldw t1, 0(s4)			;;t1=offset de x
		ldw t2, 0(s5)			;;t2=offset de y
		add a0, s0,t1			;; coor x du point avec l'offset
		add a1, s1,t2			;; coor y du point avec l'offset
		add a2,zero,s7
		call set_gsa
		addi s4,s4,4
		addi s5,s5,4
		addi s6,s6,1
		jmpi loop_draw
		
		
	end_draw:
		ldw s7, 0(sp)
		addi sp,sp,4
		ldw s6, 0(sp)
		addi sp,sp,4
		ldw s5, 0(sp)
		addi sp,sp,4
		ldw s4, 0(sp)
		addi sp,sp,4
		ldw s3, 0(sp)
		addi sp,sp,4
		ldw s2, 0(sp)
		addi sp,sp,4
		ldw s1, 0(sp)
		addi sp,sp,4
		ldw s0, 0(sp)
		addi sp,sp,4
		ldw a0, 0(sp)
		addi sp,sp,4
		ldw ra, 0(sp)
		addi sp,sp,4
;;-----------------------Faut-il remettre a1 et a2?????
	ret
; END:draw_tetromino

; BEGIN:generate_tetromino
generate_tetromino:
    ldw t0, RANDOM_NUM(zero)			; load random number
    andi t0, t0, 7						; shift to get only the 3 last bits
    cmpgei t1, t0, 5					; if t0 >= 5
    bne t1, zero, generate_tetromino	; then restart random procedure
    addi t1, zero, START_X				; store initial x coordinate 
    stw t1, T_X(zero)
    addi t1, zero, START_Y				; store initial y coordinate
    stw t1, T_Y(zero)
    stw zero, T_orientation(zero)		; store initial orientation (NORTH)
    stw t0, T_type(zero)				; store initial tetromino type 
    ret
; END:generate_tetromino

; BEGIN:rotate_tetromino
	rotate_tetromino:
		ldw t0,T_orientation(zero)
		addi t1,zero,rotR
		beq t1,a0,case_rotR		;;verifie si on veut faire une rotation vers la droite
	case_rotL:
		addi t0,t0,-1
		andi t0,t0,3			;;modulo 4
		stw t0,T_orientation(zero)
		jmpi end_rotate
	case_rotR:
		addi t0,t0,1
		andi t0,t0,3			;;modulo 4
		stw t0,T_orientation(zero)
	end_rotate:	
	ret
; END:rotate_tetromino



; BEGIN:detect_collision
detect_collision:
	addi sp, sp, -32
	stw ra, 28(sp)					; PUSH all saved registers
	stw s0, 24(sp)
	stw s1, 20(sp)
	stw s2, 16(sp)
	stw s3, 12(sp)
	stw s4, 8(sp)
	stw s5, 4(sp)
	stw s7, 0(sp)
	add s7, a0, zero				; copy input value in case of same output
	ldw s0, T_X(zero)
	ldw s1, T_Y(zero)
	ldw t6, T_orientation(zero)
	ldw t7, T_type(zero)
	addi t0, zero, DRAW_Ax			; address of DRAW_Ax
	addi t1, zero, DRAW_Ay			; address of DRAW_Ay
	slli t7, t7, 4					; t7*16
	slli t6, t6, 2					; t6*4
	add t0, t0, t7
	add t0, t0, t6					; DRAW_Ax + t7*16 + t6*4
	add t1, t1, t7					
	add t1, t1, t6					; DRAW_Ay + t7*16 + t6*4
	ldw s4, 0(t0)					; address of x offsets
	ldw s5, 0(t1)					; address of y offsets	

	addi t2, zero, W_COL
	addi t3, zero, E_COL
	addi t4, zero, So_COL
	addi t5, zero, OVERLAP
	beq s7, t2, w_detect_col
	beq s7, t3, e_detect_col
	beq s7, t4, so_detect_col
	beq s7, t5, overlap_detect_col
	jmpi end_detect_col
w_detect_col:
	addi a0, s0, -1				; test for anker point
	add a1, s1, zero
	addi sp, sp, -4				; PUSH(ra)
	stw ra, 0(sp)		
	call in_gsa
	ldw ra, 0(sp)				; POP(ra)
	addi sp, sp, 4
	bne v0, zero, col_problemo
	addi sp, sp, -4				; PUSH(ra)
	stw ra, 0(sp)
	call get_gsa
	ldw ra, 0(sp)				; POP(ra)
	addi sp, sp, 4
	addi t2, zero, 1			; placed gsa
	beq v0, t2, col_problemo	; if gsa of pixel = placed, then output problemo
	add t6, zero, zero
w_detect_col2:
	cmpgei t7, t6, 9
	bne t7, zero, end_detect_col
	add t7, t6, s4
	ldw s2, 0(t7)				; load x offset
	add t7, t6, s5
	ldw s3, 0(t7)				; load y offset
	add s2, s0, s2				; compute x coordinate for cell
	add s3, s1, s3				; compute y coordinate for cell
	addi s2, s2, -1				; shift cell by 1 to west
	add a0, zero, s2			; x pixel that we want to verify
	add a1, zero, s3			; y pixel that we want to verify
	addi sp, sp, -8				; PUSH(ra)
	stw ra, 0(sp)
	stw t6, 4(sp)
	call in_gsa
	ldw t6, 4(sp)
	ldw ra, 0(sp)				; POP(ra)
	addi sp, sp, 8
	bne v0, zero, col_problemo
	addi sp, sp, -8				; PUSH(ra)
	stw ra, 0(sp)
	stw t6, 4(sp)
	call get_gsa
	ldw t6, 4(sp)
	ldw ra, 0(sp)				; POP(ra)
	addi sp, sp, 8
	addi t2, zero, 1			; placed gsa
	beq v0, t2, col_problemo	; if gsa of pixel = placed, then output problemo
	addi t6, t6, 4
	jmpi w_detect_col2
e_detect_col:
	addi a0, s0, 1				; test for anker point
	add a1, s1, zero
	addi sp, sp, -4				; PUSH(ra)
	stw ra, 0(sp)		
	call in_gsa
	ldw ra, 0(sp)				; POP(ra)
	addi sp, sp, 4
	bne v0, zero, col_problemo
	addi sp, sp, -4				; PUSH(ra)
	stw ra, 0(sp)
	call get_gsa
	ldw ra, 0(sp)				; POP(ra)
	addi sp, sp, 4
	addi t2, zero, 1			; placed gsa
	beq v0, t2, col_problemo	; if gsa of pixel = placed, then output problemo
	add t6, zero, zero
e_detect_col2:
	cmpgei t7, t6, 9
	bne t7, zero, end_detect_col
	add t7, t6, s4
	ldw s2, 0(t7)				; load x offset
	add t7, t6, s5
	ldw s3, 0(t7)				; load y offset
	add s2, s0, s2				; compute x coordinate for cell
	add s3, s1, s3				; compute y coordinate for cell
	addi s2, s2, 1				; shift cell by 1 to east
	addi sp, sp, -8				; PUSH(ra)
	stw ra, 0(sp)
	stw t6, 4(sp)
	add a0, zero, s2			; x pixel that we want to verify
	add a1, zero, s3			; y pixel that we want to verify
	call in_gsa
	ldw ra, 0(sp)				; POP(ra)
	ldw t6, 4(sp)
	addi sp, sp, 8
	bne v0, zero, col_problemo
	addi sp, sp, -8				; PUSH(ra)
	stw ra, 0(sp)
	stw t6, 4(sp)
	call get_gsa
	ldw ra, 0(sp)				; POP(ra)
	ldw t6, 4(sp)
	addi sp, sp, 8
	addi t2, zero, 1			; placed gsa
	beq v0, t2, col_problemo	; if gsa of pixel = placed, then output problemo
	addi t6, t6, 4
	jmpi e_detect_col2
so_detect_col:
	add a0, s0, zero			; test for anker point
	addi a1, s1, 1	
	addi sp, sp, -4				; PUSH(ra)
	stw ra, 0(sp)	
	call in_gsa
	ldw ra, 0(sp)				; POP(ra)
	addi sp, sp, 4
	bne v0, zero, col_problemo
	addi sp, sp, -4				; PUSH(ra)
	stw ra, 0(sp)
	call get_gsa
	ldw ra, 0(sp)				; POP(ra)
	addi sp, sp, 4
	addi t2, zero, 1			; placed gsa
	beq v0, t2, col_problemo	; if gsa of pixel = placed, then output problemo
	add t6, zero, zero
so_detect_col2:
	cmpgei t7, t6, 9
	bne t7, zero, end_detect_col
	add t7, t6, s4
	ldw s2, 0(t7)				; load x offset
	add t7, t6, s5
	ldw s3, 0(t7)				; load y offset
	add s2, s0, s2				; compute x coordinate for cell
	add s3, s1, s3				; compute y coordinate for cell
	addi s3, s3, 1				; shift cell by 1 to south
	addi sp, sp, -8				; PUSH(ra)
	stw ra, 0(sp)
	stw t6, 4(sp)
	add a0, zero, s2			; x pixel that we want to verify
	add a1, zero, s3			; y pixel that we want to verify
	call in_gsa
	ldw ra, 0(sp)				; POP(ra)
	ldw t6, 4(sp)
	addi sp, sp, 8
	bne v0, zero, col_problemo
	addi sp, sp, -8				; PUSH(ra)
	stw ra, 0(sp)
	stw t6, 4(sp)
	call get_gsa
	ldw ra, 0(sp)				; POP(ra)
	ldw t6, 4(sp)
	addi sp, sp, 8
	addi t2, zero, 1			; placed gsa
	beq v0, t2, col_problemo	; if gsa of pixel = placed, then output problemo
	addi t6, t6, 4
	jmpi so_detect_col2
overlap_detect_col:
	add a0, s0, zero			; test for anker point
	add a1, s1, zero
	addi sp, sp, -4				; PUSH(ra)
	stw ra, 0(sp)		
	call in_gsa
	ldw ra, 0(sp)				; POP(ra)
	addi sp, sp, 4
	bne v0, zero, col_problemo
	addi sp, sp, -4				; PUSH(ra)
	stw ra, 0(sp)
	call get_gsa
	ldw ra, 0(sp)				; POP(ra)
	addi sp, sp, 4
	addi t2, zero, 1			; placed gsa
	beq v0, t2, col_problemo	; if gsa of pixel = placed, then output problemo
	add t6, zero, zero
overlap_detect_col2:
	cmpgei t7, t6, 9
	bne t7, zero, end_detect_col
	add t7, t6, s4
	ldw s2, 0(t7)				; load x offset
	add t7, t6, s5
	ldw s3, 0(t7)				; load y offset
	add s2, s0, s2				; compute x coordinate for cell
	add s3, s1, s3				; compute y coordinate for cell
	addi sp, sp, -8				; PUSH(ra)
	stw ra, 0(sp)
	stw t6, 4(sp)
	add a0, zero, s2			; x pixel that we want to verify
	add a1, zero, s3			; y pixel that we want to verify
	call in_gsa
	ldw ra, 0(sp)				; POP(ra)
	ldw t6, 4(sp)
	addi sp, sp, 8
	bne v0, zero, col_problemo
	addi sp, sp, -8				; PUSH(ra)
	stw ra, 0(sp)
	stw t6, 4(sp)
	call get_gsa
	ldw ra, 0(sp)				; POP(ra)
	ldw t6, 4(sp)
	addi sp, sp, 8
	addi t2, zero, 1			; placed gsa
	beq v0, t2, col_problemo	; if gsa of pixel = placed, then output problemo
	addi t6, t6, 4
	jmpi overlap_detect_col2
col_problemo:
	add v0, zero, s7
	ldw s7, 0(sp)				; POP all saved registers
	ldw s5, 4(sp)
	ldw s4, 8(sp)
	ldw s3, 12(sp)
	ldw s2, 16(sp)
	ldw s1, 20(sp)
	ldw s0, 24(sp)
	ldw ra, 28(sp)
	addi sp, sp, 32
	ret
end_detect_col:
	addi v0, zero, NONE
	ldw s7, 0(sp)				; POP all saved registers
	ldw s5, 4(sp)
	ldw s4, 8(sp)
	ldw s3, 12(sp)
	ldw s2, 16(sp)
	ldw s1, 20(sp)
	ldw s0, 24(sp)
	ldw ra, 28(sp)
	addi sp, sp, 32
	ret
; END:detect_collision





; BEGIN:act
	act:
		addi sp,sp,-4
		stw ra,0(sp)
		addi sp,sp,-4
		stw a0,0(sp)
		addi sp,sp,-4
		stw s0,0(sp)
		addi sp,sp,-4
		stw s1,0(sp)
		addi sp,sp,-4
		stw s2,0(sp)
		addi sp,sp,-4
		stw s3,0(sp)

		addi t0,zero,moveL
		addi t1,zero,rotL
		addi t2,zero,reset
		addi t3,zero,rotR
		addi t4,zero,moveR
		addi t5,zero,moveD

		beq a0,t0,case_moveL		
		beq a0,t1, case_act_rotL
		beq a0,t2, case_reset
		beq a0,t3, case_act_rotR
		beq a0,t4, case_moveR
		beq a0,t5, case_moveD
		jmpi end_act


	case_moveD:
		addi a0,zero,2
		call detect_collision
		addi t0,zero,4
		bne v0,	t0, end_act_prob
		ldw t1,T_Y(zero)
		addi t1,t1,1	
		stw t1,T_Y(zero)
		addi v0,zero,0
		jmpi end_act
	
	case_moveL:
		addi a0,zero,0
		call detect_collision
		addi t0,zero,4
		bne v0,	t0, end_act_prob
		ldw t1,T_X(zero)
		addi t1,t1,-1	
		stw t1,T_X(zero)
		addi v0,zero,0
		jmpi end_act
	
	case_moveR:
		addi a0,zero,1
		call detect_collision
		addi t0,zero,4
		bne v0,	t0, end_act_prob
		ldw t1,T_X(zero)
		addi t1,t1,1	
		stw t1,T_X(zero)
		addi v0,zero,0
		jmpi end_act

	case_act_rotR:
		ldw s0,T_orientation(zero)		;;S0=orient de base
		ldw s1,T_X(zero)				;;S1=X de base
		ldw s2,T_Y(zero)				;;S2=Y de base
		addi a0,zero,8
		call rotate_tetromino
		addi a0,zero,3
		call detect_collision
		addi t0,zero,4
		beq v0,	t0, end_act_rot
		addi t0,zero,6
		bge s1,t0,deuxieme_moitie
	premiere_moitie:
		addi s3,zero,1
		jmpi suite_rotR
	deuxieme_moitie:
		addi s3,zero,-1
		jmpi suite_rotR
	suite_rotR:
		ldw t1,T_X(zero)
		add t1,t1,s3
		stw t1,T_X(zero)
		addi a0,zero,3
		call detect_collision	
		addi t0,zero,4
		beq v0,	t0, end_act_rot	 ;;-----------------il faut refaire un test sur la position?
		
		ldw t1,T_X(zero)
		add t1,t1,s3
		stw t1,T_X(zero)
		addi a0,zero,3
		call detect_collision	
		addi t0,zero,4
		beq v0,	t0, end_act_rot	
		
		stw s0,T_orientation(zero)		;;S0=orient de base
		stw s1,T_X(zero)				;;S1=X de base
		stw s2,T_Y(zero)				;;S2=Y de base 	
		jmpi end_act_prob						
		
		
	case_act_rotL:

		ldw s0,T_orientation(zero)		;;S0=orient de base
		ldw s1,T_X(zero)				;;S1=X de base
		ldw s2,T_Y(zero)				;;S2=Y de base
		addi a0,zero,2
		call rotate_tetromino
		addi a0,zero,3
		call detect_collision
		addi t0,zero,4
		beq v0,	t0, end_act_rot
		addi t0,zero,6
		bge s1,t0,deuxieme_moitie2
	premiere_moitie2:
		addi s3,zero,1
		jmpi suite_rotL
	deuxieme_moitie2:
		addi s3,zero,-1
		jmpi suite_rotL
	suite_rotL:
		ldw t1,T_X(zero)
		add t1,t1,s3
		stw t1,T_X(zero)
		addi a0,zero,3
		call detect_collision	
		addi t0,zero,4
		beq v0,	t0, end_act_rot	 ;;-----------------il faut refaire un test sur la position?
		
		ldw t1,T_X(zero)
		add t1,t1,s3
		stw t1,T_X(zero)
		addi a0,zero,3
		call detect_collision	
		addi t0,zero,4
		beq v0,	t0, end_act_rot	
		
		stw s0,T_orientation(zero)		;;S0=orient de base
		stw s1,T_X(zero)				;;S1=X de base
		stw s2,T_Y(zero)				;;S2=Y de base 	
		jmpi end_act_prob

	end_act_prob:
		addi v0,zero,1
		jmpi end_act
	case_reset:	
		call reset_game

	end_act_rot:
		addi v0,zero,0
	end_act:
		ldw s3, 0(sp)
		addi sp,sp,4
		ldw s2, 0(sp)
		addi sp,sp,4
		ldw s1, 0(sp)
		addi sp,sp,4
		ldw s0, 0(sp)
		addi sp,sp,4
		ldw a0, 0(sp)
		addi sp,sp,4
		ldw ra, 0(sp)
		addi sp,sp,4


	ret
; END:act

; BEGIN:get_input
	get_input:
		add v0,zero,zero			;;Met v0 a 0 de base sera changer si neccessaire
		addi t0,zero,BUTTONS
		addi t0,t0,4				;;t0=adresse of edgecapture

		ldw t1,0(t0)				;;t1=edgecapture
		add t2,zero,zero			;;t2=compteur/index du bit a 1
		addi t3,zero,5				;;t3=limite de la boucle
		
		loop_get_input:
		beq t2,t3,suite_get_input
		andi t4,t1,1				;;t4=bit a verifier
		bne t4,zero,suite_get_input
		srli t1,t1,1
		addi t2,t2,1
		jmpi loop_get_input
		
		suite_get_input:
		addi t5,zero,1				;;t5=aide a choisir l'action en fonction du bit
		beq t2,t5, put_rotL
		addi t5,zero,2
		beq t2,t5, put_reset
		addi t5,zero,3
		beq t2,t5, put_rotR
		addi t5,zero,4
		beq t2,t5, put_moveR
		beq t2,t3, get_input_end


		put_moveL:
		addi v0,zero, 0x01
		jmpi get_input_end
		put_rotL:
		addi v0,zero, 0x02
		jmpi get_input_end
		put_reset:
		addi v0,zero, 0x04
		jmpi get_input_end
		put_rotR:
		addi v0,zero, 0x08
		jmpi get_input_end
		put_moveR:
		addi v0,zero, 0x10

	get_input_end:
		ldw t6,0(t0)			;;remise a 0 des 5 bits
		srli t6,t6,5
		slli t6,t6,5
		stw t6,0(t0)
		

	ret
; END:get_input


; BEGIN:increment_score
	increment_score:
		ldw t0,SCORE(zero)
		addi t0,t0,1
		stw t0,SCORE(zero)     
	ret
; END:increment_score


; BEGIN:display_score
	display_score:
		ldw t0,SCORE(zero)				;;t0 = le score

		addi t7,zero,1000
		addi t6,zero,100
		addi t5,zero,10

		add t1,zero,zero				;;t1= compteur des dizaines
		add t2,zero,zero				;;t2= compteur des centaines 
		add t3,zero,zero				;;t3= compteur des milliers
		
	loop_milliers:
		blt t0,t7, loop_centaines
		addi t3,t3,1
		sub t0,t0,t7
		jmpi loop_milliers
	loop_centaines:
		blt t0,t6,loop_dizaines
		addi t2,t2,1
		sub t0,t0,t6
		jmpi loop_centaines
	loop_dizaines:	
		blt t0,t5,suite_display_score
		addi t1,t1,1
		sub t0,t0,t5
		jmpi loop_dizaines

	suite_display_score:
		add t7, zero,zero		;;t7 devient une aide pour avoir l'addresse du bon segment

		slli t3,t3,2			;;*4 pour avoir la bonne adresse
		ldw t4,font_data(t3)
		stw t4,SEVEN_SEGS(t7)
		addi t7,t7,4

		slli t2,t2,2
		ldw t4,font_data(t2)
		stw t4,SEVEN_SEGS(t7)
		addi t7,t7,4
		
		slli t1,t1,2
		ldw t4,font_data(t1)
		stw t4,SEVEN_SEGS(t7)
		addi t7,t7,4

		slli t0,t0,2
		ldw t4,font_data(t0)
		stw t4,SEVEN_SEGS(t7)
		addi t7,t7,4
	ret
; END:display_score

; BEGIN:detect_full_line
detect_full_line:
	addi sp, sp, -20
	stw s1, 0(sp)
	stw s2, 4(sp)
	stw s3, 8(sp)
	stw s4, 12(sp)
	stw s5, 16(sp)
	add s4, zero, zero			; x value
	add s5, zero, zero			; y value
	addi s1, zero, 8
	addi s2, zero, 12
	addi s3, zero, 1
	jmpi loop_on_line
reinit_full_line:
	addi s5, s5, 1
	beq s5, s1, end_detect_line
	add s4, zero, zero
loop_on_line:
	addi sp, sp, -4
	stw ra, 0(sp)
	add a0, s4, zero
	add a1, s5, zero
	call get_gsa
	ldw ra, 0(sp)
	addi sp, sp, 4
	bne v0, s3, reinit_full_line
	addi s4, s4, 1
	beq s4, s2, end_detect_line
	jmpi loop_on_line
end_detect_line:
	add v0, zero, s5
	ldw s5, 16(sp)
	ldw s4, 12(sp)
	ldw s3, 8(sp)
	ldw s2, 4(sp)
	ldw s1, 0(sp)
	addi sp, sp, 20
	ret
; END:detect_full_line

; BEGIN:remove_full_line
remove_full_line:
	addi sp, sp, -32					; save ra
	stw ra, 0(sp)
	stw s1, 4(sp)
	stw s2, 8(sp)
	stw s3, 12(sp)
	stw s4, 16(sp)
	stw s5, 20(sp)
	stw s6, 24(sp)
	stw s7, 28(sp)
	addi s3, zero, 12					; upper bound for x axis
	add s6, zero, a0					; exchange a0 with a1 just to match a1 with y axis
	add s5, zero, zero					; initialize x to 0
	add s4, zero, zero					; temp value for loop
	addi s2, zero, 5					; temp value for loop (set to 5 because blink 2 times)
remove_line:
	addi s7, zero, NOTHING				; to remove line, put gsa to NOTHING
	beq s5, s3, blinking_line			; if x >= 12 goto blinking_line
	add a2, s7, zero
	add a1, s6, zero
	add a0, s5, zero
	call set_gsa						; set (x, y) to NOTHING
	addi s5, s5, 1						; increment x 
	jmpi remove_line					
add_line:
	addi s7, zero, PLACED				; to add line, put gsa to PLACED
	beq s5, s3, blinking_line			;  if x >= 12 goto blinking_line
	add a2, s7, zero
	add a1, s6, zero
	add a0, s5, zero
	call set_gsa						; set (x, y) to PLACED
	addi s5, s5, 1						; increment x
	jmpi add_line
blinking_line:
	call draw_gsa						; draw the grid to show updated grid
	call wait							; wait to see the blink
	add s5, zero, zero					; reinitialize x coordinate
	addi s4, s4, 1						; t6 = t6 + 1
	cmpge s1, s4, s2
	bne s1, zero, end_remove_line		; if t6 >= 5, done blinking
	bne s7, zero, remove_line			; if last loop was add, then goto remove
	jmpi add_line						; else goto add
end_remove_line:
	addi s4, s6, -1						; go 1 line up
	add s5, zero, zero					; set x value to 0
	jmpi shift_gsa
reinit_shift_gsa:				
	addi s4, s4, -1 					; go 1 line up (t6 is y)
	cmplt s2, s4, zero
	bne s2, zero, pourdebon_end			; if y < 0, goto end
	add s5, zero, zero					; else reinit x to 0
shift_gsa:
	beq s5, s3, reinit_shift_gsa		; if x >= 12 goto reinit
	add s6, s4, zero					; y (attribute)
	add a1, s6, zero
	add a0, s5, zero
	call get_gsa
	add s7, v0, zero					; 
	addi s6, s4, 1						; giving upper line gsa value tu lower line
	add a2, s7, zero
	add a1, s6, zero
	add a0, s5, zero
	call set_gsa						;
	addi s5, s5, 1		
	jmpi shift_gsa
pourdebon_end:
	ldw s7, 28(sp)
	ldw s6, 24(sp)
	ldw s5, 20(sp)
	ldw s4, 16(sp)
	ldw s3, 12(sp)
	ldw s2, 8(sp)
	ldw s1, 4(sp)
	ldw ra, 0(sp)			
	addi sp, sp, 32
	ret
; END:remove_full_line



; BEGIN:reset_game
	reset_game:
		addi sp,sp,-4
		stw ra,0(sp)
		addi sp,sp,-4
		stw a0,0(sp)
		addi sp,sp,-4
		stw s0,0(sp)
		addi sp,sp,-4
		stw s1,0(sp)
		addi sp,sp,-4
		stw s2,0(sp)
		addi sp,sp,-4
		stw s3,0(sp)

		stw zero,SCORE(zero)
		call display_score
		
		
		add s0,zero,zero		;;s0=x
		add s1,zero,zero		;;s1=y
		addi s2,zero,12			;;s2=limite x
		addi s3,zero,8			;;s3=limite y

		
	outer_res_loop:
		beq s0,s2,suite_reset
			inner_loop:
				beq s1,s3,next_outer_res_loop
				add a0,zero,s0
				add a1,zero,s1
				add a2,zero,zero
				call set_gsa
				addi s1,s1,1
				jmpi inner_loop
	next_outer_res_loop:
		addi s0,s0,1
		add s1,zero,zero
		jmpi outer_res_loop
	

	suite_reset:
		call generate_tetromino
		addi a0,zero,2
		call draw_tetromino
		call draw_gsa		
		
		ldw s3, 0(sp)
		addi sp,sp,4
		ldw s2, 0(sp)
		addi sp,sp,4
		ldw s1, 0(sp)
		addi sp,sp,4
		ldw s0, 0(sp)
		addi sp,sp,4
		ldw a0, 0(sp)
		addi sp,sp,4
		ldw ra, 0(sp)
		addi sp,sp,4
		
	ret
; END:reset_game



; BEGIN:main
	main:
		addi s0,zero,RATE 			;;s0=RATE
		call reset_game
		addi s2,zero,1
		addi s3,zero,8
		
	boucle_1:
	boucle_2:
		add s1,zero,zero 			;;s1=i
	while1:
		beq s1,s0,suite_boucle_2
		call draw_gsa
		call display_score
		addi a0,zero,0
		call draw_tetromino			;;appel a draw_tetromino avec nothing
		call wait
		call get_input
		add a0,v0,zero
		call  act
		addi a0,zero,2				;;appel a draw_tetromino avec falling
		call draw_tetromino
		addi s1,s1,1
		jmpi while1

	suite_boucle_2:
		addi a0,zero,0
		call draw_tetromino			;;appel a draw_tetromino avec nothing
		addi a0,zero,0x20
		call act					;;appel a act pour descendre
		beq	v0,s2, suite_boucle_1
		addi a0,zero,2				;;appel a draw_tetromino avec falling
		call draw_tetromino
		jmpi boucle_2
	suite_boucle_1:
		addi a0,zero,1				;;appel a draw_tetromino avec placed
		call draw_tetromino
	while_2:
		call detect_full_line
		beq v0,s3,fin_while_2
		add a0,v0,zero
		call remove_full_line
		call increment_score
		jmpi while_2
	fin_while_2:
		call generate_tetromino
		addi a0,zero,3
		call detect_collision
		addi t1,zero,3
		beq v0,t1, main
		addi a0,zero,2				;;appel a draw_tetromino avec falling
		call draw_tetromino
		jmpi boucle_1
; END:main

fin:
	;;-------------------------------------------------------------------------
font_data:
    .word 0xFC  ; 0
    .word 0x60  ; 1
    .word 0xDA  ; 2
    .word 0xF2  ; 3
    .word 0x66  ; 4
    .word 0xB6  ; 5
    .word 0xBE  ; 6
    .word 0xE0  ; 7
    .word 0xFE  ; 8
    .word 0xF6  ; 9

C_N_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

C_N_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0xFFFFFFFF

C_E_X:
  .word 0x01
  .word 0x00
  .word 0x01

C_E_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

C_So_X:
  .word 0x01
  .word 0x00
  .word 0x01

C_So_Y:
  .word 0x00
  .word 0x01
  .word 0x01

C_W_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0xFFFFFFFF

C_W_Y:
  .word 0x00
  .word 0x01
  .word 0x01

B_N_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x02

B_N_Y:
  .word 0x00
  .word 0x00
  .word 0x00

B_E_X:
  .word 0x00
  .word 0x00
  .word 0x00

B_E_Y:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x02

B_So_X:
  .word 0xFFFFFFFE
  .word 0xFFFFFFFF
  .word 0x01

B_So_Y:
  .word 0x00
  .word 0x00
  .word 0x00

B_W_X:
  .word 0x00
  .word 0x00
  .word 0x00

B_W_Y:
  .word 0xFFFFFFFE
  .word 0xFFFFFFFF
  .word 0x01

T_N_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_N_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0x00

T_E_X:
  .word 0x00
  .word 0x01
  .word 0x00

T_E_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_So_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_So_Y:
  .word 0x00
  .word 0x01
  .word 0x00

T_W_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0x00

T_W_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_N_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_N_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

S_E_X:
  .word 0x00
  .word 0x01
  .word 0x01

S_E_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_So_X:
  .word 0x01
  .word 0x00
  .word 0xFFFFFFFF

S_So_Y:
  .word 0x00
  .word 0x01
  .word 0x01

S_W_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

S_W_Y:
  .word 0x01
  .word 0x00
  .word 0xFFFFFFFF

L_N_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x01

L_N_Y:
  .word 0x00
  .word 0x00
  .word 0xFFFFFFFF

L_E_X:
  .word 0x00
  .word 0x00
  .word 0x01

L_E_Y:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x01

L_So_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0xFFFFFFFF

L_So_Y:
  .word 0x00
  .word 0x00
  .word 0x01

L_W_X:
  .word 0x00
  .word 0x00
  .word 0xFFFFFFFF

L_W_Y:
  .word 0x01
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

DRAW_Ax:                        ; address of shape arrays, x axis
    .word C_N_X
    .word C_E_X
    .word C_So_X
    .word C_W_X
    .word B_N_X
    .word B_E_X
    .word B_So_X
    .word B_W_X
    .word T_N_X
    .word T_E_X
    .word T_So_X
    .word T_W_X
    .word S_N_X
    .word S_E_X
    .word S_So_X
    .word S_W_X
    .word L_N_X
    .word L_E_X
    .word L_So_X
    .word L_W_X

DRAW_Ay:                        ; address of shape arrays, y_axis
    .word C_N_Y
    .word C_E_Y
    .word C_So_Y
    .word C_W_Y
    .word B_N_Y
    .word B_E_Y
    .word B_So_Y
    .word B_W_Y
    .word T_N_Y
    .word T_E_Y
    .word T_So_Y
    .word T_W_Y
    .word S_N_Y
    .word S_E_Y
    .word S_So_Y
    .word S_W_Y
    .word L_N_Y
    .word L_E_Y
    .word L_So_Y
    .word L_W_Y
