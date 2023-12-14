.386
.model flat, stdcall
.stack 4096
ExitProcess PROTO, dwExitCode: DWORD
Include Irvine32.inc

.data
MAX_BUFFER_SIZE = 1000
MAX_WORD_ARRAY_SIZE = 5000
temp dword 0
LocateArray WORD 100 DUP (?)
user_input BYTE 15 DUP (?)
.data
    ;reading file

    ;COL DWORD 4
    word_counter DWORD 0
    blocker BYTE "OOOOOOOOOOOOOOO",0
    Num_words Dword 1
    str_index DWORD 0
    finish dword 0
    doneStr DWORD 0
    pos dword 0
	error byte 0
    ;pos_index dword 0
    char_typed dword 0
    char_error dword 0
    accuracy dword 0
    score dword 0
    accuracy_str BYTE "accuracy: ",0
    score_str BYTE "score: ",0
    

    three_str BYTE "three.txt",0
    five_str BYTE "five1.txt",0
    ten_str BYTE "tennn.txt",0
    fileName1 BYTE 10 DUP (0)
    word_size Dword 0
    fileHandle HANDLE ?
    bytesRead DWORD ?
    buffer BYTE MAX_BUFFER_SIZE dup(0)
    wordArray BYTE MAX_WORD_ARRAY_SIZE dup(?),0
    speed dword 1000
    msec dword ?
    lose BYTE 0
    win BYTE 0
    intro_str BYTE "WELCOME TO TYPING GAME!", 0
    prompt BYTE "Press any key for easy mode, (N) for normal, (H) for hard", 0
    win_str BYTE "You win.",0
    lose_str BYTE "You lose.",0
    try_again_str BYTE "Press 1 to try again. Any key to stop",0
    redline BYTE "------------------------------------------------------------------------------------------------------------------------",0
    
.code

    ReadingFile Proc 
    ; Open the file
    invoke CreateFile, addr fileName1, GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
    mov filehandle, eax
   
    
    ; Read the file content into the buffer
    invoke ReadFile, filehandle, addr buffer, MAX_BUFFER_SIZE, addr bytesRead, 0
    cmp bytesRead, 0
    je  close_file
    
    ; Tokenize the content and store words in the array
    mov esi, offset buffer
    mov edi, offset wordArray

tokenize_loop:
    ; Read a character from the buffer
    mov al, [esi]
    cmp al, 0
    je  tokenize_done

    ; Check for space or newline character
    cmp al, ' '
    jne  continue
    ;cmp al, 0Ah ; nextline character
    ;jne continue

    ;Store the character in the wordArray
    mov bl, 0
    mov [edi], bl
    inc edi
    inc esi
    inc num_words
    jmp tokenize_loop

    continue:
    mov [edi], al
    inc edi
    inc esi
    jmp tokenize_loop

tokenize_done:
    
close_file:
    ; Close the file
    invoke CloseHandle, filehandle
   
 ret
 ReadingFile ENDP
;----------------------------------------------------------------------------------------------------------------------------------------------------
SetRed PROC uses EAX
mov  eax,red+(black*16)
      call SetTextColor
ret
SetRed ENDP

SetGreen PROC uses EAX
mov  eax,green+(black*16)
      call SetTextColor
ret
SetGreen ENDP

SetWhite PROC uses EAX
mov  eax,white+(black*16)
      call SetTextColor
ret
SetWhite ENDP


Reset_Input PROC uses esi
	mov esi, 0						;index counter
	mov ecx, Word_size					;loop counter
	L1:	
		mov [user_input + esi],0		;set every index to zero
		inc esi
		loop L1
	ret
Reset_Input ENDP
;----------------------------------------------------------------------------------------------------------------------------------------------------
 LocateCursorXY Proc uses ESI EDX
 mov esi, OFFSET [LocateArray]
 mov dx, [esi+eax]
 ;dec dh
 ;dec dh
 call GotoXY
 ret
 LocateCursorXY ENDP

 CheckInput Proc uses esi edi ebx edx ecx eax
 mov esi, OFFSET user_input
 mov edi, OFFSET WordArray

 
 mov eax, 1
 call Delay
 call readKey
 jz DONE							;if not pressed, exit
	
	push eax
	mov eax, str_index				;find index of str_array	
	mov edx, word_size
	mul edx
	add edi, eax				;move edi to current idex
	pop eax
	mov edx, pos
    inc char_typed

 .IF al == [edi+edx]

    mov error, 0
    add esi, pos
    mov [esi], al
    inc pos
    inc edx
    .IF BYTE PTR [edi+edx]==0   ;check if this index is end of the string
				mov BYTE PTR [edi], 0	;clear the string
				inc doneStr				;increment the completed string
				mov finish, 1
				;dec printedStr
				jmp DONE
			.ENDIF	
		.ELSE
			mov al, [edi+edx]			;if not correct, error
			mov error, al
            inc char_error
		.ENDIF
	DONE:
		mov eax, str_index
		;mov eax, ebx
		;mov eax, pos_index
		mov ebx, TYPE locateArray
		mul ebx

		call LocatecursorXY
		mov edx, OFFSET user_input	
		call setGreen				;printed typed chars
		call WriteString
		.IF error != 0
			call setRed
			mov al, error
			call WriteChar				;if there is errors, print with red color
		.ENDIF
		call setWhite
		.IF finish == 1
           
            push edx
            push eax
            call LocateCursorXY
            mov edx, offset blocker
       
            mov eax, Black
            call settextcolor
            call writeString
            pop eax
            pop edx
			call Reset_Input		;clear input string
			mov pos,0				;set input array pos to 0
			mov finish, 0

			add str_index,1

		.ENDIF


COMMENT $
    push eax
    mov  eax, Green+(black*16)
    call SetTextColor
    pop eax
    call writechar
 .ENDIF
 mov  eax, white+(black*16)
 call SetTextColor
 inc esi
 inc edi $

 ret
 CheckInput ENDP



 LocateXY PROC uses EAX EDX ESI ebx ECX
 ;need randomized Y for new word printed
 mov ebx, word_counter
 shl ebx, 1
 mov esi, OFFSET [locateArray]
 add esi, ebx
 mov dx,[esi]
 Call gotoXY
 ret
 LocateXY ENDP

 StoreXY PRoc uses eax esi ebx edx;randomized and store the locations Dh, dl
 call GetmaxXY
 mov eax, 0
 mov al, dl
 call randomize
 call RandomRange
 mov esi, OFFSET [LocateArray] ;still need to increase the location in a loop
 mov ebx, temp
 shl ebx, 1 ;multiply ebx by 2
 add esi, ebx
 mov [esi], ax
 
 ret
 StoreXY endp

 MoveWordDown Proc uses eax edx ESI ecx
 ;deal with word array and locateArray
 ;call LocateXY
 mov esi, OFFSET [LocateArray]
 mov ecx, word_counter
 inc ecx
 ;add eax,2
 mov edx, 0
 L10:
     mov dx, [esi]
     inc dh
     mov [esi], dx
     add esi, 2
 loop L10
 ;Call gotoXY
 ret
 MoveWordDown ENDP
 ;---------------------------------------------------------------------------------------------------
 WriteWord PRoc uses ECX ESI ebx eax
 ;write all the words one by one        
 ;might need another loop
 ;mov ecx, 3   ; three bacuse it is prototype
 ;mov edx, offSet WordArray ; temp might not work
 ;mov ebx, temp
 ;shl ebx, 2
 ;add edx, ebx
 ;add esi, edx
  ;L1:
 ;mov al, [esi]
 ;call WriteChar
 ;inc esi
 ;Loop L1
 ;--------------------------------------------------------------------------------------------------------
 call writeString
 ret
 WriteWord ENDP

 WriteWords Proc uses ECX eax
 mov edx, OFFSET wordArray
 ;inc temp
 ;mov eax, 0
 mov word_counter, 0


 mov ecx, temp
 inc ecx
 
 L5:
 mov eax, white
 call settextcolor
 call locateXY
 call writeString
 inc word_counter
 ;inc eax
 add edx, word_size
 loop L5
 
 ret
 WriteWords ENDP

 blackBlock PROC uses EAX ECX
 
 mov edx, OFFSET wordArray
 ;inc temp
 ;mov eax, 0
 mov word_counter, 0


 mov ecx, temp
 inc ecx
 
 L5:
 mov eax, BLACK
 call setTextcolor
 call locateXY
 call writeString
 inc word_counter
 ;inc eax
 add edx, word_size
 loop L5
 
 ret
 blackBlock ENDP
 .code

 CheckLose PROC uses EAX EDX ebx
 mov edx, str_index
 shl edx,1 
 mov bx, [LocateArray + edx +1]
 ;call getMaxXY
 mov al, 28                              ;end of the game Y value 28
 .IF al == bl
  mov lose, 1
  .ENDIF
 ret
 CheckLose Endp

 CheckWin PROC uses EAX
 mov eax, Num_words
    .IF eax <= str_index
    mov win, 1
    .ENDIF
ret
CheckWin ENDP

 WriteRedLine PROC uses EDX EAX
 mov dl, 0
 mov dh, 28                            ;end of the game Y value 28
 call GotoXY
 mov edx, OFFSET redline
 call setRed

 call writeString
 ret
 WriteRedLine ENDP


 TypingGame PROC
 call ReadingFile
 mov ecx,100
 ;mov eax,500

call Getmseconds
add eax, speed
mov msec, eax
 L2:            ;jump nk yay ayan
 call storeXY
 call WriteWords
 call writeRedline
 call setWhite
 ;call checkkey
 ;call delay
 
 L5:

 mov eax, 1
 call Delay
 ;call locateCursorXY
 call checkinput
  
 call Getmseconds
 .IF eax >= msec
		;call clrscr
        call blackBlock
		inc temp					;increment the number of printed string
		add eax, speed					;delay
		mov msec, eax
  .ELSE
  ;add eax, 500 ;just testing
    jmp L5
	.ENDIF

 call moveWordDown


  call CheckLose
  .IF lose == 1
  jmp loopexit
  .ENDIF

  call CheckWin
  .IF win == 1
  jmp loopexit
  .ENDIF

 loop L2

 loopexit:
 ret
 TypingGame ENDP

 reset_vari PROC
    mov char_typed, 0
    mov char_error, 0
    mov str_index, 0
    mov temp, 0
    mov doneStr, 0
    mov num_words, 1
    mov lose, 0
    mov win, 0
 ret
 reset_vari ENDP


 Menu PROC uses eax edx 
 mov dl, 30h
 mov dh, 12
 call GotoXY
 mov eax, white
 call settextcolor
 mov edx, OFFSET intro_str
 call writeString
 
 mov dl, 20h
 mov dh, 13
 call gotoXY
 mov edx, OFFSET Prompt
 call writeString
 call readChar
 cmp al, 'H'
 je Hard
 cmp al, 'N'
 je Normal

 call easyMode
 ret

 Hard:
 call HardMode
 ret

 Normal:
 call NormalMode
 ret
 Menu ENDP

 HardMode Proc uses esi ecx edi eax
 mov word_size,11
 mov ecx, 10
 mov esi, OFFSET ten_str
 mov edi, OFFSET fileName1
 loop1:
 mov al, [esi]
 mov [edi], al
 inc esi
 inc edi
 loop loop1
 ret
 HardMode ENDP

 NormalMode Proc uses esi ecx edi eax
 mov word_size,6
 mov ecx, 10
 mov esi, OFFSET five_str
 mov edi, OFFSET fileName1
 loop1:
 mov al, [esi]
 mov [edi], al
 inc esi
 inc edi
 loop loop1
 ret
 NormalMode ENDP

 EasyMode Proc uses esi ecx edi eax
 mov word_size,4
 mov ecx, 10
 mov esi, OFFSET three_str
 mov edi, OFFSET fileName1
 loop1:
 mov al, [esi]
 mov [edi], al
 inc esi
 inc edi
 loop loop1
 ret
 EasyMode ENDP

 DisplayResult PROC uses eax edx
 mov eax, white
 call settextcolor
 mov dl, 30h
 mov dh, 14h
 call gotoXY
 .IF win == 1
    mov edx, OFFSET win_str
    call writeString
.ENDIF
 .IF lose == 1
     mov edx, OFFSET lose_str
    call writeString
.ENDIF

 Call Cal_acu
 
 mov dl, 30h
 mov dh, 15h
 call GotoXY

 mov edx, OFFSET accuracy_str
 call writeString
 mov eax, accuracy
 Call WriteDec
 mov eax, 25h
 call writeChar

 mov dl, 30h
 mov dh, 16h
 call GotoXY
 mov edx, OFFSET score_str
 call WriteString
 mov eax, score
 call writeDec

 mov dl, 30h
 mov dh, 17h
 call GotoXY
 mov edx, OFFSET try_again_str
 call writeString

 ret
 DisplayResult ENDP

 Cal_acu proc uses EAX EBX EDX
 mov edx, 0
 mov eax, char_typed
 mov ebx, char_error
 sub eax, ebx
 mov score, eax ;now eax has number of right char
 mov ebx, 100
 mul ebx; 
 mov ebx, char_typed
 div ebx
 
 mov accuracy, eax
 ret
 Cal_acu ENDP

 main proc
 LoopGame:
 call menu
 call clrscr
 call TypingGame
 call displayresult
 call readChar
 .IF al == '1'
    call reset_vari
    call clrscr
    jmp LoopGame
.ENDIF
INVOKE ExitProcess, 0
main ENDP
END main