tower(5,
         [[2,3,4,5,1],
          [5,4,1,3,2],
          [4,1,5,2,3],
          [1,2,3,4,5],
          [3,5,2,1,4]],
         C).

tower(5,
         T,
         counts([2,3,2,1,4],
           [3,1,3,3,2],
           [4,1,2,5,2],
           [2,4,2,1,2])).

tower(5,
         [[2,3,4,5,1],
          [5,4,1,3,2],
          [4,1,5,2,3],
          [1,2,3,4,5],
          [3,5,2,1,4]],
         counts([2,3,2,1,4],
           [3,1,3,3,2],
           [4,1,2,5,2],
           [2,4,2,1,2])).

tower(2, 
	[[1,2],
	 [2,1]],
	 C).

tower(2,
	T,
	counts(
	 	[2,1],
	 	[1,2],
	 	[2,1],
	 	[1,2])).

tower(2, 
	[[1,2],
	 [2,1]],
	 counts(
	 	[2,1],
	 	[1,2],
	 	[2,1],
	 	[1,2])).