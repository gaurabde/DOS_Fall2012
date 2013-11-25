UFID: 7802-9140
Name: Gaurab Dey
E-Mail: gaurabde@ufl.edu/gdey@cise.ufl.edu


Project Discription
---------------------

1) Size of Work
----------------
Project is divided into following structure:
a) Boss:(project1) Main project1 with the main thread, create the SubBoss which control or Workers
b) SubBoss(bRange:Int, bSeqLength:Int): It control all the workers and the AddData chunk creation dynamically depending upon load
	bRange: its the range of the integer given in command line argument
	bSeqLength: its the length of the sequence to be tested for checking the sum of consuctive square roots 
3) Worker: Worker are dynamically created with the AddData chuncks as message for execution
4) AddData(val starting:Int,val end:Int,val seqLength:Int): this are the chuncks of data which are given to Worker for square root calculation and Addition
	starting: starting point of the sequence
	end: end point of the sequence
	seqLength: define the range for the square root to be done in the data chunck

-- Number of Workers and AddData chuncks are dynamillcally created by depending upon the payload(length of the square root sequence for testing and the Last number in the range)
-- Chucks of AddData are send to Workers in form of message
-- Dynamically status of all Worker and AddData monitored and tried to stop all unused to maintain Memory Availability


2) The result of running your program for
scala project1.scala 1000000 4

Result: 0


3) Runtime report:
time scala project1.scala
1.484u 0.424s 0:02.61 72.7%     0+0k 16+64io 0pf+0w

