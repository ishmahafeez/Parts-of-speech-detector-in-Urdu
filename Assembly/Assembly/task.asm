include irvine32.inc
.data
;	INPUT FILE HAS TO BE IN DOUBLE SPACE FORMAT

filehandle dword ?;											//
inputfile byte "input.txt",0;								////
;															//////
listofadjectivesfile byte "listofadjectives.txt",0;			////////
adjectivefile byte "adjective.txt",0;						//////////
;															////////////
listofverbsfile byte "listofverbs.txt",0;					//////////////
verbfile byte "verb.txt",0;									////////////////
;															//////////////////
listofpronounsfile byte "listofpronouns.txt",0;				////////////////////
pronounfile byte "pronoun.txt",0;							//////////////////////			DECLARING FILES TO BE USED IN THIS PROGRAM
;															//////////////////////
listofadverbsfile byte "listofadverbs.txt",0;				////////////////////
adverbfile byte "adverb.txt",0;								//////////////////
;															////////////////
listofconjunctionsfile byte "listofconjunctions.txt",0;		//////////////
conjunctionfile byte "conjunction.txt",0;					////////////
;															//////////
listofprepositionsfile byte "listofprepositions.txt",0;		////////
prepositionfile byte "preposition.txt",0;					//////
;															////
nounfile byte "noun.txt",0;									//


s1 byte "Starting...",0
s2 byte "Finished",0

found dword 0 ; boolean variable to check if a word has been found as a part of speech or not

inputarray word 10000 dup(0);								//
listofadjectives word 10000 dup(0);							////
listofverbs word 10000 dup(0);								//////
listofpronouns word 10000 dup(0);							////////       DECLARING ARRAYS OF STRINGS THAT WILL HOLD DATA READ FROM EACH FILE
listofadverbs word 10000 dup(0);							//////
listofconjunctions word 10000 dup(0);						////
listofprepositions word 10000 dup(0);						//


wordfromdataarray word 25 dup(0)     ;stores individual urdu word from the grammar data arrays defined above
wordfrominputarray word 25 dup(0)    ;stored individual urdu word from the input data array defined above

adjectivesfound word 1000 dup(0)	 ;stores the list of adjectives found from input
numofadj dword 0					 ;stores the size of the above list	

verbsfound word 1000 dup(0)			 ;stores the list of verbs found from input
numofverb dword 0					 ;stores the size of the above list	

pronounsfound word 1000 dup(0)		 ;stores the list of pronouns found from input
numofpronoun dword 0				 ;stores the size of the above list	

adverbsfound word 1000 dup(0)		 ;stores the list of adverbs found from input
numofadverb dword 0					 ;stores the size of the above list	

conjunctionsfound word 1000 dup(0)	 ;stores the list of conjunction found from input
numofconjunction dword 0			 ;stores the size of the above list	

prepositionsfound word 1000 dup(0)	 ;stores the list of prepositions found from input
numofpreposition dword 0			 ;stores the size of the above list	

nounsfound word 1000 dup(0)			 ;stores the list of nouns found from input
numofnoun dword 0					 ;stores the size of the above list	




.code
part_of_speech_detector proto, bigstr:ptr word,bigsize:dword,smallstr:ptr word,listoffound:ptr word,numoffound: ptr dword
;bigstr is the dataarray containing all parts of speech data (for ex: listofadjectives)   OFFSET OF LISTS 
;bigsize is the size of bigstr
;small str is the individual urdu word to find if it exists in the bigstr  EACH WORD FROM INPUT ARRAY
;listoffound is the array that will store smalstr if it is found (for ex: adjectivesfound)
;numoffound is the size of the above array. It is passed as reference to get updated in the function once a part of speech is found


get_data proto, filename:ptr byte,destination:ptr word
write_part_of_speech_to_file proto,filename:ptr byte,source:ptr word,sizetowrite :dword


main proc

mov edx,offset s1
call writestring
call crlf

invoke get_data, offset inputfile,offset inputarray;						//		
;																			////
invoke get_data, offset listofadjectivesfile,offset listofadjectives;		//////
;																			////////
invoke get_data, offset listofverbsfile,offset listofverbs;					//////////
;																			////////////
invoke get_data, offset listofpronounsfile,offset listofpronouns;			//////////////		CALLING FUNCTION TO COLLECT THE DATA FROM THE FILES INTO THE ARRAYS 
;																			////////////
invoke get_data, offset listofadverbsfile,offset listofadverbs;				//////////
;																			////////
invoke get_data, offset listofconjunctionsfile,offset listofconjunctions;	//////
;																			////
invoke get_data, offset listofprepositionsfile,offset listofprepositions;	//



;istofadjectives = arrayof all teh adjectives from file
;wordfrominputdata=input ONE WORD words, offset smallstr

;wordfromdataarray= listofadjectives first letter.second letter,  ,,esi ONE WORD
;sizeofowes= length of above

;smallstr=offset wordfrominputarray ,, edi,,edx ONE WORD
;bigstr=offest listofadjectives ,,esi ,,ebx
;ebx=sizeofbigstr ,, sizeofword

;45
;Str1 BYTE ‘127&j ~3#^&*#*#45^’,0
;repe equal nahi hai then quit
;part_of_speech_detector,offset listofadjectives,ebx,offset wordfrominputarray,offset adjectivesfound,offset numofadj





invoke str_length,addr inputarray   ;stores input array length in eax
mov ecx,eax							;the whole process will happen for the size of the input array. This size will not be 10,000 but it will be the actual numbers of characters read from file, hence being more efficient
mov esi,0						;used as index of input data array
mov edi,0						;used as index of the wordfrominputarray that will store the individual urdu word from input array
loop_for_input_array:
	mov ax,inputarray[esi]		;individual characters are read and then put into ax
	cmp ax,2020h				;if the character read is a space (double space is denoted by 2020 in heaxa)
	je checkforpartofspeech		;then word has been built and we can now check for the part of speech it is
	cmp ax,0					;else if the character read is a null
	je store_to_files					;then input array data is all read and we can move on to saving the parts of speech into our files
	mov wordfrominputarray[edi],ax ;else store it into wordfrominputarray so each individual word is build using charcters read from input array =ONE WORD
	add edi,2					;incrementing by 2 to point to next index as it is a WORD 
	jmp loopback_loop_for_input_array

	checkforpartofspeech:
							
		pushad   ;storing all registers on stack as partofspeechdetector uses pretty much all registers whose current values need to be saved


		invoke str_length,addr listofadjectives		;getting exact size of listofadjectives instead of 10000 to be more efficient							//
		mov ebx,eax;																																		////		CHECKS IF THE WORD IS AN ADJECTIVE						////
		invoke part_of_speech_detector,offset listofadjectives,ebx,offset wordfrominputarray,offset adjectivesfound,offset numofadj; by reference			//	





		invoke str_length,addr listofverbs			;getting exact size of listofverbs instead of 10000 to be more efficient								//
		mov ebx,eax;																																		////		CHECKS IF THE WORD IS A VERB
		invoke part_of_speech_detector,offset listofverbs,ebx ,offset wordfrominputarray,offset verbsfound,offset numofverb;								//





		invoke str_length,addr listofprepositions	;getting exact size of listofprepositions instead of 10000 to be more efficient							//
		mov ebx,eax;																																		////		CHECKS IF THE WORD IS A PREPOSITION
		invoke part_of_speech_detector,offset listofprepositions,ebx ,offset wordfrominputarray,offset prepositionsfound,offset numofpreposition;			//





		invoke str_length,addr listofpronouns		;getting exact size of listofpronouns instead of 10000 to be more efficient								//
		mov ebx,eax;																																		////			CHECKS IF THE WORD IS A PRONOUN
		invoke part_of_speech_detector,offset listofpronouns,ebx ,offset wordfrominputarray,offset pronounsfound,offset numofpronoun;						//




		
		invoke str_length,addr listofadverbs		;getting exact size of listofadverbs instead of 10000 to be more efficient								//
		mov ebx,eax;																																		////			CHECKS IF THE WORD IS AN ADVERB
		invoke part_of_speech_detector,offset listofadverbs,ebx ,offset wordfrominputarray,offset adverbsfound,offset numofadverb;							//





		invoke str_length,addr listofconjunctions	;getting exact size of listofconjunctions instead of 10000 to be more efficient							//
		mov ebx,eax	;																																		////			CHECKS IF THE WORD IS A CONJUNCTION
		invoke part_of_speech_detector,offset listofconjunctions,ebx ,offset wordfrominputarray,offset conjunctionsfound,offset numofconjunction;			//


		popad ;retreiving register values


		cmp found,0   ;if the word was not found in any of the parts of speech above, then it must be  noun

		jnz continuelabel  ;else continue to forming next word
		
				pushad
						
						
						invoke str_length,addr nounsfound			
						mov edi,eax						 ;edi will be used as index of nounsfound, it is not set to 0, it is set to the existing size as we have to APPEND to the array
					
						mov esi,0						 ;esi is used as index for wordfrominputarray
						invoke str_length,addr wordfrominputarray    
						push eax      ;storing the size on stack so can be used later 
						mov ecx,eax
						shr ecx,1						;this loop is run for word array copying, so the size is divided by 2
			

					append_to_noun:
						mov ax,wordfrominputarray[esi];			//
						mov nounsfound[edi],ax;					////
						add edi,2;								//////   the word that was not found as any part of speech will be a noun and hence appended to the nounsfound array
						add esi,2;								////
						loop append_to_noun;					//

						mov word ptr nounsfound[edi],000Ah; adding a newline to the end of the array
						pop eax			;size retreived
						add eax,1		; for newline character just added
						add numofnoun,eax		;updating	
						
		
				popad



		continuelabel:
		
	
		mov found,0 ;resetting found to false

		mov edi,0  ;resetting edi to 0

		push ecx;														//
		mov ecx,24			;using ecx as index so starts at 24			////
		reset_word_input:;												//////
			mov wordfrominputarray[ecx],0;								//////			resetting string to empty again to get ready for next word
			loop reset_word_input;										////
			pop ecx;													//

	loopback_loop_for_input_array:
	add esi,2     ;update esi to point to next WORD
	dec ecx			;i didn't use a loop instruction here as it gave an error of "jump too far" so i decremented ecx and compared it to 0 manually
	cmp ecx,0
	jne loop_for_input_array







store_to_files:

invoke write_part_of_speech_to_file,offset adjectivefile,offset adjectivesfound,numofadj;					//
;																											////
invoke write_part_of_speech_to_file,offset verbfile,offset verbsfound,numofverb;							//////
;																											////////
invoke write_part_of_speech_to_file,offset pronounfile,offset pronounsfound,numofpronoun;					//////////
;																											////////////
invoke write_part_of_speech_to_file,offset adverbfile,offset adverbsfound,numofadverb;						//////////////			STORING THE WORDS INTO THE RESPECTIVE PARTS OF SPEECH FILES
;																											////////////
invoke write_part_of_speech_to_file,offset conjunctionfile,offset conjunctionsfound,numofconjunction;		//////////
;																											////////
invoke write_part_of_speech_to_file,offset prepositionfile,offset prepositionsfound,numofpreposition;		//////
;																											////
invoke write_part_of_speech_to_file,offset nounfile,offset nounsfound,numofnoun;							//






exitlabel:
mov edx,offset s2
call writestring
exit

main endp

part_of_speech_detector proc, bigstr:ptr word,bigsize:dword,smallstr:ptr word,listoffound:ptr word,numoffound: ptr dword  
LOCAL sizeofword:dword  ;stores size of individual word read from part_of_speech data array (bigstr) (for ex: listofadjectives)

mov esi,bigstr   ;esi will be used as reference to bigstr	(for ex: listofadjectives)
mov edi,smallstr ;edi will be used as reference to smallstr	(the individual word to find in bigstr, sent from main)

mov ecx,bigsize ;the process below will run till the size of bigstr(actual size, not total amount of size allocated to it as more efficient this way,also sent from main)
mov ebx,0		;ebx is used as index of bigstr
mov edx,0		;edx is used as index of smallstr
	
part_of_speech_detector_L1:
	
	mov ax,[esi+ebx]	;individual characters are read and then put into ax
	cmp ax,2020h		;if the character read is a space
	je compare_word		;then word has been built and we can now check if it is same as the smallstr passed to function
	cmp ax,0			;else if the character read is a null
	je exit_function	;then bigstr data is all read and we can exit the function

	mov wordfromdataarray[edx],ax ;else store it into wordfromdataarray so each individual word is build using characters read from bigstr array (e.g listofadjectives)
	add edx,2						;incrementing by 2 to point to next index as it is a WORD 
	jmp loopback
	compare_word:
		
		pushad				;storing all registers on stack as next step uses pretty much all registers whose current values need to be saved
	
			mov wordfromdataarray[edx],0h  ;adding  null terminator character at the end. Garbage character outputted before urdu word otherwise
			mov esi,offset wordfromdataarray
			invoke str_length,addr wordfromdataarray  ;eax stores length wordofarray
			sub eax,1   ;so that the null character added doesn't get compared
			mov sizeofword,eax
			mov ecx,eax   
			repe cmpsb   ;compare word built with smallstr /if cmpsw to be used then divide ecx by 2
			je write_to_part_of_speech_found_array	;if they match then add it to the respective part_of_speech_found array (ex: adjectivesfound)
			jne exitcompare			;else prepare to build next word

			write_to_part_of_speech_found_array:

					mov found,1  ;the word was found in the respective part_of_speech data array

					mov ebx,listoffound   ;ebx will be used as reference to the part_of_speech_found array
					mov edx,numoffound
					mov edi,[edx]		  ;edi will be used as index of listoffound, it is not set to 0, it is set to the existing size as we have to APPEND to the array
					
					mov esi,0            ;esi is used as index to the word just built
					invoke str_length,addr wordfromdataarray
					mov ecx,eax
					shr ecx,1  ;this loop is run for word array copying, so the size is divided by 2
			

				append_to_array:

					mov ax,wordfromdataarray[esi];		//
					mov [edi+ebx],ax;					////
					add edi,2;							//////  the word that was found as a part of speech will be appended to the part_of_speech_found array
					add esi,2;							////
					loop append_to_array;				//
					mov word ptr[ebx+edi],000Ah		;adding a newline to the end of the array
				
				
				
				mov ecx,sizeofword;			//
				;							////	
				mov esi,numoffound;			//////		updating numoffound variable for part of speech (ex: numofadj)
				add [esi],ecx;				//////
				add dword ptr[esi],2;		////
				;							//
			
			exitcompare:
		popad								;retreiving register values


		mov edx,0	;reset index to 0
		
		push ecx;											//
		mov ecx,24;;using ecx as index so starts at 24		////
		reset_word:;										//////				RESETTING string to empty again to get ready for next word
			mov wordfromdataarray[ecx],0;					//////
			loop reset_word;								////
			pop ecx;										//

			
	loopback:
		cmp found,1;  if word is found 
		je exit_function  ;then no need to search further in list as the word is found,hence more efficient
		add ebx,2		;update ebx to point to next WORD

		dec ecx			;i didn't use a loop instruction here as it gave an error of "jump too far" so i decremented ecx and compared it to 0 manually
		cmp ecx,0
		jne part_of_speech_detector_L1

exit_function:

ret
part_of_speech_detector endp

get_data proc, filename:ptr byte,destination:ptr word

mov edx,filename
call openinputfile
mov filehandle,eax
mov eax,filehandle
mov edx,destination
mov ecx,10000
call readfromfile

ret
get_data endp




write_part_of_speech_to_file proc,filename:ptr byte,source:ptr word,sizetowrite :dword

mov edx, filename  ;create
call createoutputfile
mov filehandle,eax           ;file opened or not?
mov eax,filehandle		
mov edx, source				;array index in edx
mov ecx,sizetowrite			;array size in ecx
call writetofile			;write till ecx 0
mov eax,filehandle
call closefile

ret
write_part_of_speech_to_file endp

end main
