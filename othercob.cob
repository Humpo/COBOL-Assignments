     Author.     Alexander James Comerford.
     Identification Division.
       Environment Division.
       Input-Output Section.
       File-Control.

          *>This selects the .dat file and assigns that file to the
          *>variable "Input-File"
           Select Input-File
                assign to "/home1/c/a/acsi203/realestate.dat"
                Organization is line sequential.

          *>This selects an output file to the variable name 
          *>"Output-File". The name of the corresponding .dat
          *>file that will be created will be called "prog1out.dat"
           Select Output-File 
              assign to "prog1out.dat"
              Organization is line sequential.
      
       Data Division.
         
       File Section.

       FD  Input-File.
       *>Everything below here is involves "Input-File"
       01 Input-Rec.
       *>Input-Rec contains the format of which each record will be
       *>formatted in, in the "Input-File"
       *>Each data value is labelled accordingly
           02 Property-Address       pic x(27).
           02 City                   pic a(15).
           02 Zip                    pic 9(5).
           02 State                  pic a(2).
           02 Bedrooms               pic 9(1).
           02 Bathrooms              pic 9(1).
           02 Sq-Ft                  pic 9(4).
           02 Property-Type          pic x(8).
           02 Filler                 pic x(28).
           02 Sale-Price             pic 9(6).
           02 Filler                 pic x(17).
           02 Filler                 pic x.
       *>This is the end of what happens to the "Input-File"

       FD Output-File.
       *>Output-Rec contains the amount of charactes each record
       *>of output should have
       01 Output-Rec pic x(132).
        
       Working-Storage Section. 

       01 Report-Header.
       *>Report-Header Contains the specially formated header
       *>that will be put ontop
           02 Filler               pic x(46) value spaces.
           02 Filler               pic x(40)
           Value "Sacramento Area Real Estate Transactions".  
           02 Filler               pic x(46) value spaces.

       01 Column-Headers.  
       *>Column headers contains the entire line of headers each 
       *>column of data will have
       *>Name does not matter because this is one line designed to fit the data
           02 Filler                pic x(16) value "Property-Address".
           02 Filler                pic x(5) value spaces. 
           02 Filler                pic x(4) value "City".
           02 Filler                pic x(5) value spaces.
           02 Filler                pic x(3) value "Zip".
           02 Filler                pic x(5) value spaces.
           02 Filler                pic x(5) value "State".
           02 Filler                pic x(5) value spaces.
           02 Filler                pic x(8) value "Bedrooms". 
           02 Filler                pic x(5) value spaces.
           02 Filler                pic x(9) value "Bathrooms".
           02 Filler                pic x(5) value spaces.
           02 Filler                pic x(5) value "Sq-Ft".
           02 Filler                pic x(5) value spaces.
           02 Filer                 pic x(13) value "Property-Type".
           02 Filler                pic x(5) value spaces.
           02 Filler                pic x(10) value "Sale-Price". 
       01 Info-Line.
       *>Info-Line will be the output the data will be foramtted in
       *>This output matches the lengths of column headers to ensure
       *>that the data is organized and visually appealing 
       *>Fillers' are left as spaces while the associated data value
       *>has an out at the end to ensure that the programmer knows 
       *>where the associated data needs to be moved
           02 Property-Address-out   pic x(16).
           02 Filler                 pic x(5) value spaces.
           02 City-out               pic x(4).
           02 Filler                 pic x(5) value spaces.
           02 Zip-out                pic x(3).
           02 Filler                 pic x(5) value spaces.
           02 State-out              pic x(5).
           02 Filler                 pic x(5) value spaces.
           02 Bedrooms-out           pic x(8).
           02 Filler                 pic x(5) value spaces.
           02 Bathrooms-out          pic x(9).
           02 Filler                 pic x(5) value spaces.
           02 Sq-Ft-out              pic x(5).
           02 Filler                 pic x(5) value spaces.
           02 Property-Type-out      pic x(13).
           02 Filler                 pic x(5) value spaces.
           02 Sale-Price-out         pic x(10).
           02 Filler                 pic x(15) value spaces.
       01 End-Report. 
       *>This is a formated line to be printed at the end of the report
       *>after all data has been outputed
           02 Filler                 pic x(60) value spaces.
           02 Filler                 pic x(13) value "End of Report".
           02 Filler                 pic x(59) value spaces.
       01 Records-Processed.
       *>This is a formated line designed to print the number of 
       *>records processed.           
           02 Filler                 pic x(28) value 
                                     "Number of Records Processed:".
           02 counter1               pic 9(5)  value 00000.
           02 Filler                   pic x(99) value spaces.
       *>eof-flag is created so we can instantiate a loop
       01 eof-flag                   pic x value "N".
          
          
       Procedure Division.
          
       0000-Main-Logic.
           *>Main-logic is designed to open the output and input files,
           *>read in each record while incrementing 
           *>then lastly print the records and close the files
           Perform 1000-Init.
           Perform 2000-Process-Record until eof-flag= "Y".
           Perform 3000-Finish.
           Stop Run.
           
        
       1000-Init.
           *>Open input to be read and output to be written
           Open Input Input-File.
           Open Output Output-File.
           
           *>Move and write the Report header
           Move Report-Header to Output-Rec.
           Write Output-Rec. 

           *>Move and write the column headers
           Move Column-Headers to Output-Rec.
           Write Output-Rec.        

       2000-Process-Record.
           *>This is the complicated stuff
           *>The Input-File is read and when the file reaches
           *>the end of the file it will return "Y" to eof-flag
           *>terminating the loop

       	   Read Input-File at end move "Y" to eof-flag.

           *>A whole bunch of moving is going on so that
           *>the associated data can be able to print
           Move Property-Address to Property-Address-out.
           Move City to City-out.
           Move Zip to Zip-out.
           Move State to State-out.
           Move Bedrooms to Bedrooms-out.
           Move Bathrooms to Bathrooms-out.
           Move Sq-Ft to Sq-Ft-out.
           Move Property-Type to Property-Type-out.
           Move Sale-Price to Sale-Price-out.

           *>Info-Line contains everything above so we move it all to 
           *>be ready for writing 
           Move Info-Line to Output-Rec.

           *>After everything is moved we write
           Write Output-Rec.

           *>At this point a record has been processed so we increment
           *>the counter 
           Add 1 to counter1.
         
       3000-Finish.
           *We do not need the input file for reading anymore
           *So we close it
           Close Input-file.

           *We move&write the number of records
           Move Records-Processed to Output-Rec.
           Write Output-Rec.

           *Lastly we move&write the End-Report statement created 
           Move End-Report to Output-Rec.
           Write Output-Rec. 

           *The output file is complete so we close it
           Close Output-file.
    