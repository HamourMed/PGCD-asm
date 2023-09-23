; multi-segment executable file template.

data segment
   
    ancien_ip dw ?
    ancien_cs dw ?

    
    nb1Word dw 0
    nb2Word dw 0
    resWord dw ? 
   
    tabWord dw 498, 5142, 3624
    lenWord label word
    
    Jours dw offset dimanche, offset lundi, offset mardi, offset mercredi, offset jeudi, offset vendredi, offset samedi
    
    dimanche db 'DIMANCHE, $'
    lundi  db 'LUNDI, $'
    mardi   db 'MARDI, $'
    mercredi db 'MERCREDI, $'
    jeudi db 'JEUDI,$'
    vendredi db 'VENDREDI, $'
    samedi   db 'SAMEDI, $'
    
    
    taille db 'N=$'
    msg db 'TAB = $' 
    virg db ' ,$'
    msgPGCD db 'PGCD = $' 

     
   
   
ends
 
 
 
 
stack segment
    dw   128  dup(0)
    tos label word
ends




code segment 
    
       Assume cs:code, ds:data, ss:stack
    
    
   ;--------LA PROCEDURE PGCD_I Pour Word    
     
     PGCD_IWord proc
            push dx 
            push ax
            push bx
            push cx
            
        call LireVecteurDivZero
        call deroutementWord
        
        mov ax, nb1Word
        mov bx, nb2Word
        xor dx, dx
        cmp ax, bx
        ja boucleWord
        xchg ax, bx
        
        boucleWord:
        div bx
        xchg ax, bx
        xchg bx, dx
        xor dx, dx
        jmp boucleWord
        
       suiteWord:
        mov resWord, ax 
        call restaurerVecteurDivZero
        
        
        pop cx
        pop bx
        pop ax        
        pop dx
         
        ret
               
        PGCD_IWord endp  
     
     
   ;--------FIN DE LA PROCEDURE PGCD_IWord   
    
    
    
   ;--------LA PROCEDURE PGCD_IN Word
    
    
     PGCD_INWord proc 
           push ax
            push bx
            push cx     
               
               
        mov bx, offset tabWord
        mov cx, offset lenWord
        sub cx, offset tabWord+2
        shr cx, 1
        
        
        mov ax, [bx] 
        mov nb1Word, ax
        inc bx
        inc bx
        
        mov ax, [bx]
        mov nb2Word, ax  
        
        
        
        bclPGCD_INWord:
        
               
              call PGCD_IWord
              
              inc bx
              inc bx
              
              mov ax, [bx] 
              mov nb1Word, ax
        
              mov ax, resWord
              mov nb2Word, ax
         
              loop bclPGCD_INWord 
              
        pop cx
        pop bx
        pop ax
          ret
       PGCD_INWord endp
     
     
     
     
                                    
                                    
                                    
   ;--------FIN DE LA PROCEDURE PGCD_INWord
    
    
    
    
   ;--------LA ROUTINE D'INTERRUPTION DIV ZERO Pour Word
    
    
    new_routineWord:
        
        pop cx 
                
        push offset suiteWord 
    
    iret 
    
   ;--------FIN DE LA ROUTINE D'INTERRUPTION DIV ZERO Pour Word
    
    
    
    
    
   ;--------LA PROCEDURE DE LA LECTURE DU VECTEUR ZERO 
    
     LireVecteurDivZero proc
        push bx
        push es
        
        
         mov ah, 35h
         mov al, 0
         int 21h
         ; ES: contient le cs de la routine Div Zero
         ; BX: contient le ip de la routine Div Zero
         
         mov ancien_ip, bx
         mov ancien_cs, es
         
        pop es
        pop bx
        ret
    LireVecteurDivZero endp  
    
   ;--------FIN DE LA PROCEDURE LireVecteurDivZero 
    
    
    
    
   ;--------LA PROCEDURE DU DEROUTEMENT DU VECTEUR ZERO POUR WORD
   
   
   deroutementWord proc 
         push ax
         push dx
        
         mov dx, offset new_routineWord 
         mov ax, seg new_routineWord  
         mov ds, ax
         mov al, 0
         mov ah, 25h
         int 21h
         
         mov ax, data
         mov ds, ax
         pop dx
         pop ax
        
        ret 
    deroutementWord endp
   
   
    ;--------FIN DE LA PROCEDURE deroutementWord
    
    
    
    
    ;--------LA PROCEDURE DE RESTAURATION DU VECTEUR ZERO
    
    
     restaurerVecteurDivZero proc
        push ax
        push dx
        
         mov dx, ancien_ip
         mov ax, ancien_cs  
         mov ds, ax
         mov al, 0
         mov ah, 25h
         int 21h
         
         mov ax, data
         mov ds, ax
        pop dx
        pop ax
        
        ret 
    restaurerVecteurDivZero endp
   
   
   
   
     ;--------FIN DE LA PROCEDURE restaurerVecteurDivZero 
     
     
     
     
     ;---------------------LES PROCEDURES D'AFFICHAGES
   
   
   
      ;--------LA PROCEDURE D'AFFICHAGE D'UNE CHAINE DE CARACTERES
       
       afficherChaine proc
        mov ah,9
        int 21h  
        ret
       afficherChaine endp
   
       ;--------FIN DE LA PROCEDURE afficherChaine
   
   
   
       ;--------LA PROCEDURE D'AFFICHAGE D'UN CARACTERE
   
   
          afficherCara proc
        mov ah,2
        int 21h  
        ret
       afficherCara endp
   
   
       ;--------FIN DE LA PROCEDURE afficherCara
       
       
       ;--------LA PROCEDURE D'AFFICHAGE d'un octet en decimal
   
   
       AfficherDecByte proc     
        
           push bx 
           push ax
           push cx 
           push dx
           
           
           xor ah, ah
           mov bl, 10 
           xor cx, cx
           
           
          bclDec:
           div bl
           inc cx
           push ax
           xor ah, ah
           cmp al, 0
           jnz bclDec 
           
         ecriture:
            pop ax
            mov dl, ah
            add dl, 30h
            mov ah, 2
            int 21h
           
              
              
              
           loop ecriture   
              
              
            pop dx  
            pop cx  
            pop ax
            pop bx
        
           ret 
           
           
        AfficherDecByte endp
      
       ;--------FIN DE LA PROCEDURE AfficherDecByte 
       
       
        ;-------LA PROCEDURE D'AFFICHAGE des minutes en decimal
   
   
       AfficherDecMin proc     
        
           push bx 
           push ax
           push cx 
           push dx
           
           
           xor ah, ah
           mov bl, 10 
           mov cx, 2
           
           
          bclMinDec:
           div bl
           
           push ax
           xor ah, ah
           loop bclMinDec 
          
         mov cx, 2 
		  
         ecritureMin:
            pop ax
            mov dl, ah
            add dl, 30h
            mov ah, 2
            int 21h
           
              
              
              
           loop ecritureMin   
              
              
            pop dx  
            pop cx  
            pop ax
            pop bx
        
           ret 
           
           
        AfficherDecMin endp
      
       ;--------FIN DE LA PROCEDURE AfficherDecMin
       
       
       
       
       ;--------LA PROCEDURE D'AFFICHAGE d'un WORD en decimal
      
            AfficherDecWord proc         
        
           push bx 
           push ax
           push cx
           push dx 
           
           
           
           mov bx, 10 
           xor cx, cx
           xor dx, dx
           
          bclWord:
           div bx
           inc cx
           push dx
           xor dx, dx
           cmp ax, 0
           jnz bclWord 
           
         ecritureWord:
           pop dx
           add dl, 30h
           mov ah, 2
           int 21h
           
              
              
              
           loop ecritureWord   
              
              
            pop dx  
            pop cx  
            pop ax
            pop bx
        
           ret 
           
           
        AfficherDecWord endp                
       
              
              
       ;--------FIN DE LA PROCEDURE AfficherDecWord
       
       
       
       ;--------LA PROCEDURE D'AFFICHAGE de la DATE
       
        AfficherDate proc 
            push ax
            push bx
            push cx
            push dx
            push si
                     
       mov ah, 2
       mov bh, 0
       mov dh, 0 
       mov dl, 57 
       int 10h 
        
        
       mov ah, 2ah 
       int 21h 
       mov bx, offset Jours
       push dx
       xor ah, ah  
       mov si, ax 
       shl si, 1
       mov dx, [bx+si] 
       call afficherChaine 
       pop dx 
       
       
       
       mov al, dl
       call AfficherDecByte
       mov dl, '/'
       call afficherCara
       mov al, dh
       call AfficherDecByte
       mov dl, '/'
       call afficherCara 
       mov ax, cx
       call AfficherDecWord
       
         
         pop si
         pop dx
         pop cx
         pop bx
         pop ax     
    
       ret 
       
         AfficherDate endp
       
       ;--------FIN DE LA PROCEDURE AfficherDate
       
       
       
       
       ;--------LA PROCEDURE D'AFFICHAGE d'heure 
       
       
       
           AfficherHeure proc    
            push ax
            push bx
            push cx
            push dx
            
           
        
        mov ah, 2 
       mov bh, 0 
       mov dh, 1 
       mov dl, 67
       int 10h 
         
            mov ah, 2ch
            int 21h
            
            mov al, ch
            call AfficherDecByte
            mov dl, ':'
            call afficherCara
            mov al, cl
            call AfficherDecMin 
            mov dl, 10
            call afficherCara
            mov dl, 13
            call afficherCara
            
            pop dx
         pop cx
         pop bx
         pop ax  
     
            ret
            AfficherHeure endp 
       
       
       
       
       
       ;--------FIN DE LA PROCEDURE AfficherHeure
       
       
       
       
       ;--------LA PROCEDURE D'AFFICHAGE du tableau de Word en Decimal
       
         AfficherTabWord proc    ; Affihage du tableau de Words
        
           push ax
            push bx
            push cx
            push dx
           
           
        mov dx, offset taille
        call afficherChaine
        
        mov ax, offset lenWord 
        sub ax, offset tabWord
        shr ax, 1
        mov cx, ax
        dec cx
        
        call afficherDecWord 
        
       mov ah, 2 
       mov bh, 0 
       mov dh, 2 
       mov dl, 25
       int 10h
          
       mov dx, offset msg
       call afficherChaine
       
       mov bx, offset tabWord
       
       bcltabWord:
       mov ax, [bx]  
       call AfficherDecWord
       mov dx, offset virg
       call afficherChaine
       inc bx
       inc bx
       loop bcltabWord
       
       mov ax, [bx]  
       call AfficherDecWord
       
       mov dl, 10
       call afficherCara
       mov dl, 13
       call afficherCara
              
              
              pop dx
         pop cx
         pop bx
         pop ax 
       
             ret
             
             AfficherTabWord endp
       
       
        ;--------FIN DE LA PROCEDURE AfficherTabWord
       
       
       
       
       
       
        ;--------LA PROCEDURE D'AFFICHAGE du PGCD du tableau de Word en Decimal
       
        AfficherPGCDWord proc
            push ax
            push bx
            push cx
            push dx
            
                    
        
          mov ah, 2 ;numéro de fonction
       mov bh, 0 ;numéro de page écran
       mov dh, 3 ;ligne de l'écran
       mov dl, 28 ;colonne de l'écran
       int 10h 
       
       mov dx, offset msgPGCD
       call afficherChaine
       mov ax, resWord
       call AfficherDecWord
        
        
        
        
           pop dx
         pop cx
         pop bx
         pop ax 
        
        
        ret   
        
        AfficherPGCDWord endp
       
       
       
        ;--------FIN DE LA PROCEDURE AfficherPGCDWord
           
           
          
         
         
           
           
           
           
       
      
    
start:
; set segment registers:
    mov ax, data
    mov ds, ax 
    mov ax, stack
    mov ss, ax
    
    mov sp, offset tos

    
    
    
    
        call AfficherDate
        call AfficherHeure
        call AfficherTabWord 
        call PGCD_INWord
        call AfficherPGCDWord
         
         
         
         
         
         
         
    
    
    
    
    
    
    
    
    
    
    mov ax, 4c00h ; exit to operating system.
    int 21h    
ends

end start ; set entry point and stop the assembler.
