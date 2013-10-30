(eax += edx) -> addl %edx, %eax

(eax += 2)  ->  addl $2, %eax

(eax <- :l1)
:l1

-> 
; the below is assembly for the above L1
movel $li, %eax
l1:

; but that code above is non-moveable and thats bad
; you should to use the x86 call instruction, but we might not.

; we have to implement in C print and allocate
void print(void * in){
  int i = (int)in;
  if( i & 1 ) { ;// bitwise and will be true if last bit is 1
    printf( "%i", i>>1); //arithmetic shift (copies the left bit so if its negative it stays negative)
    } else {
      ;//it must be a pointer
      void** ptr = ((void**)in) + 1; // not sure why...
      ;//first thing
      int size = *((int*) in);
      ;// then we need to iterate over this array recursively calling print. just do what the interpreter does. we have to print the size first.
      ;//prints like this "{s:%i" and some other crap.
    }
      
      
;// next is allocate. but we should write our own alloc instead of using malloc because we might want to write a GC, and malloc isnt friendly for that.
      
;//first, some global state
      
void** heap; // we'll probably malloc to get a huge chunk of memory.
void** alloc_ptr;
void* allocate(int fw_size, void* fw_data) { ; //fw because its not a c int, its our encoded fwa int. 
  int size = fw_size >> 1; // decode that (maybe make sure its an encoded number)
  void** result = alloc-ptr;
  alloc_ptr += size;
  if(alloc_ptfc >= heap + HEAP_SIZE) exit(-1); ;// figure out how to manage those heap variables properly.  
  ;// little loop here to initialize the memory. put in size in first position and put fw_data in each position.
  
  return result;
}


;;;;;;; comparison operators ;;;;;;;;;;
(cjump ebx > edx :true :false)   ; was this < ??
cmpl %edx, %ebx  ; this sets a bunch of bits based on the relative values of those registers
jl true ;jump less that to the true label.
jmp false ; unconditional jump to the false label.

(cjump 3 > edx :true :false)


(eax <- ebx < edx)
