; multi-segment executable file template.

data segment
   
    ancien_ip dw ?
    ancien_cs dw ?

    
    nb1 db 0
    nb2 db 0
    res db ? 
   
    tab db 96, 54, 81, 18 
    len label byte
    
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
    
    
   ;--------LA PROCEDURE PGCD_I    
     
     PGCD_I proc 
        push ax       ; sauvegarde du contexte des registres dans la pile
        push bx
        push cx
        push dx         
                 
                 
        call LireVecteurDivZero
        call deroutement
        
        mov al, nb1
        mov bl, nb2
        xor ah, ah
        cmp al, bl
        ja boucle
        xchg al, bl
        
        boucle:
        div bl
        shr ax, 8
        xchg bl, al
        jmp boucle
        
       suite:
        mov res, al
        
        
        call restaurerVecteurDivZero 
        
         pop dx
         pop cx
         pop bx
         pop ax
        
        
        ret
        
        PGCD_I endp  
     
     
   ;--------FIN DE LA PROCEDURE PGCD_I   
    
    
    
   ;--------LA PROCEDURE PGCD_IN 
    
    
     PGCD_IN proc
        
        push ax
        push bx
        push cx
        push dx 
        
        mov bx, offset tab
        mov cx, offset len
        sub cx, offset tab+1
        
        
        mov al, [bx] 
        mov nb1, al
        inc bx
        
        mov al, [bx]
        mov nb2, al  
        
        
        
        bclPGCD_IN:
        
               
              call PGCD_I
              
              inc bx
              
              mov al, [bx] 
              mov nb1, al
        
              mov al, res
              mov nb2, al
         
              loop bclPGCD_IN 
              
            pop dx
         pop cx
         pop bx
         pop ax  
              
              
              ret
        
        
       PGCD_IN endp
        
     
     
     
     
                                    
                                    
                                    
   ;--------FIN DE LA PROCEDURE PGCD_IN
    
    
    
    
   ;--------LA ROUTINE D'INTERRUPTION DIV ZERO 
    
    
    new_routine:
    
        pop cx 
                
        push offset suite 
    
    iret
    
   ;--------FIN DE LA ROUTINE D'INTERRUPTION DIV ZERO
    
    
    
    
    
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
    
    
    
    
   ;--------LA PROCEDURE DU DEROUTEMENT DU VECTEUR ZERO
   
   
   deroutement proc 
          push ax
         push dx
        
         mov dx, offset new_routine 
         mov ax, seg new_routine  
         mov ds, ax
         mov al, 0
         mov ah, 25h
         int 21h
         
         mov ax, data
         mov ds, ax
         pop dx
         pop ax
        
        ret 
    deroutement endp
   
   
    ;--------FIN DE LA PROCEDURE deroutement
    
    
    
    
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
       
       
       
       
       ;--------LA PROCEDURE D'AFFICHAGE du tableau de Byte en Decimal
       
         AfficherTabByte  proc  
            push ax
            push bx
            push cx
            push dx        
        
        mov dx, offset taille
        call afficherChaine
        
        mov ax, offset len
        sub ax, offset tab
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
       
       mov bx, offset tab
       
       bcltab:
       mov al, [bx]  
       call AfficherDecByte
       mov dx, offset virg
       call afficherChaine
       inc bx
       loop bcltab
       
       mov al, [bx]  
       call AfficherDecByte
       
       mov dl, 10
       call afficherCara
       mov dl, 13
       call afficherCara
                 
           pop dx
         pop cx
         pop bx
         pop ax        
                 
         ret        
       AfficherTabByte endp
       
       
        ;--------FIN DE LA PROCEDURE AfficherTabByte
       
       
       
       
       
       
        ;--------LA PROCEDURE D'AFFICHAGE du PGCD du tableau de Byte en Decimal
       
                AfficherPGCDByte proc 
                    push ax
            push bx
            push cx
            push dx
                        
        
          mov ah, 2 
       mov bh, 0 
       mov dh, 3 
       mov dl, 28 
       int 10h 
       
       mov dx, offset msgPGCD
       call afficherChaine
       mov al, res
       call AfficherDecByte
        
        
        
        
          pop dx
         pop cx
         pop bx
         pop ax 
        
        
        ret
        AfficherPGCDByte endp
       
       
       
        ;--------FIN DE LA PROCEDURE AfficherPGCDByte
           
           
           
           
           
           
           
       
      
    
start:
; set segment registers:
    mov ax, data
    mov ds, ax 
    mov ax, stack
    mov ss, ax
    
    mov sp, offset tos

    
    
    
    
        call AfficherDate
        call AfficherHeure
        call AfficherTabByte 
        call PGCD_IN
        call AfficherPGCDByte
         
         
         
         
         
         
         
    
    
    
    
    
    
    
    
    
    
    mov ax, 4c00h ; exit to operating system.
    int 21h    
ends

end start ; set entry point and stop the assembler.
