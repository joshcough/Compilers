#lang scheme

;loop(a,i,max){
;  if(i < max){
;    a[i] <- a[i-1] + a[i-2]
;    loop(a,i+1,max)
;  }
;}
  
(:main
 (:main 
  (ebx <- 20) ; how many fibs we want
  (eax <- (allocate ebx 1)) ; gives a pointer to an array of size 20 filled with 1's
  
  (push 16) ; push doesnt take what to push, its how many spaces to push (12 for 3 ints. 4 bytes each)
  ; just adjusted that to 16 for a bad reason. really need to adjust other numbers (by adding 4 to them)
  
  
  (set -4 eax) ; eax the pointer to the array
  (set -8 2) ; starting at position 2 in the array
  (set -12 ebx) ; number of fibs
  
  (ecx <- eax) ; save this because the function being called has the right to trash the eax register
               ; we could have pushed 4, and stored this on the stack instead
  (call :loop) ; call must have a magic return
               ; important note, call does an extra push on the stack
  
  (print eax) ; uh oh, print somehow has to know if its getting an int, or a pointer...
  )
  
  (:loop 
   (esi <- (get -12)) ; i
   (edi <- (get -16)) ; max
   (cjump  esi  < edi :true :false)
   :true
   
   ; all of this is for getting the first thing out of the array for adding it a[i-1]
   (eax <- esi) ; eax contains 1
   (eax -= 1) ; decrement it
   (edx eax *= 4) ; (i-1) * 4
   (edx <- (get -8)) ; puts a (the variable) into edx
   (eax += edx)
   (ebx <- (mem eax)) ; get the actual value out of the array
  
   ; all of this is for getting the second thing out of the array for adding it a[i-2]
   (eax <- esi) ; eax contains 1
   (eax -= 2) ; decrement it
   (edx eax *= 4) ; (i-1) * 4
   (edx <- (get -8)) ; puts a (the variable) into edx
   (eax += edx)
   (ecx <- (mem eax)) ; get the actual value out of the array
 
   ; add the two fibs together. still need to store them.
   (ecx += ebx) 
   
   ; set up eax with the value to store
   (eax <- esi)
   (edx eax *= 4)
   (edx <- (get -8))
   (eax += edx)
   
   ; store it!
   ((mem eax) <- ecx)
      
   (esi += 1)
   (set -12 esi)
   (goto :loop)
   
   :false
   
   (ret)))
   
; a + (i-1) * 4 to compute the index into the array. but mult clobbers edx! be careful.
    
