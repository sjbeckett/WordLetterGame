
WordLetterGame <- function(){
WORDS=c("boat","coat","list","just","helm","cake","lake","bake","rake","take","fake","wake","make","kale","dale","bail","fail","rail","mail","nail","hail","tail","bean","mean","dean","male","gale","cyst","frog","slog","dogs","bogs","logs","cogs","hogs","jogs","bats","cats","mats","mast","rats","beat","heat","neat","meat","hate","rate","star","bars","bare","bear","rare","rear","ruts","rust","lust","lute","cute","dude","whos","bird","curd","bugs","hugs","lugs","mugs","jugs","rugs","lump","dump","hump","bump","rump","jump","clue","drug","died","hide","side","bide","ride","wide","herd","puds","ford","cord","lord","buck","duck","ruck","hulk","bulk","tram","plan","rant","math","mode","worm","node","tote") #100 words that serve as solutions.

#choose word
CHOSEN = sample(WORDS,1)
chossplit = strsplit(CHOSEN,"")[[1]]
letters = strsplit("abcdefghijklmnopqrstuvwxyz","")[[1]]
lettersAll=letters

print('Guess the chosen four letter word. Game will end if you win, or type: end')

flag=0
while(flag==0){
	#input word

	print('Available letters:')
	print(letters)
	
		flaginput = 0
		while(flaginput==0){
			INPUT = readline("Enter guess:")
			INPUT = tolower(INPUT)
			
			#check against chosen word
			if(INPUT=="end"){
			print("Game over.")
			flag=2
			flaginput=2
			break
			}
		
			#get characters
			insplit = strsplit(INPUT,"")[[1]]
			
			#input checks
			Ngood = 0
			Rgood = 0
			Cgood = 0
		
			#correct number of characters?
			if (nchar(INPUT)==nchar(CHOSEN)){
				Ngood=1
			}
		
			for(aa in insplit){
				if(length(which(aa == insplit))==1){
					Rgood = Rgood+1
				}
				
				if(length(setdiff(aa,lettersAll))==0){
					Cgood = Cgood+1
				}
			}
				
			
					#if all good, exit while loop and carry on! Else re-enter input.
			if(Ngood == 1 & Rgood==nchar(CHOSEN) & Cgood==nchar(CHOSEN)){
				flaginput=1
			}else{
				print(paste("Word should be",nchar(CHOSEN),"letters long, no repeated characters. Try again!"))
			}
	
		}
	
	if(CHOSEN==INPUT){
		flag = 1
		print(paste("Congratulations, you win!"))
		break;
	}else{
	
	
	#how many correct?
	CORRECT = length(chossplit) - length(setdiff(insplit,chossplit))
	if (flag==0){
	print(paste(CORRECT,"letters in",INPUT,"are correct."))
	}
	if(CORRECT == 0){
	for(aa in insplit){
		IND = which(aa==letters)
		if(length(IND)>0){
			letters = letters[-IND]
		}
	}
	
	}
	
	}

}

}