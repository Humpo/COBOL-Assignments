       *>Alexander Comerford
       *>csi203
       *>cobol program that organizes and averages a realestate .dat file
       *>and filters impurities
       *>and outputs it to user
       Identification Division.
       Environment Division.
       Input-Output Section.
       File-Control.

          *>This selects the .dat file and assigns that file to the
          *>variable "Input-File"

           Select Input-File
                assign to "/home1/c/a/acsi203/realestate.dat"
                Organization is line sequential.
           Select City-File
                assign to "/home1/c/a/acsi203/city.dat"
                Organization is line sequential.
           Select Output-File 
              assign to "prog4out.dat"
              Organization is line sequential.
           Select Error-File
              assign to "error4out.dat"
              Organization is line sequential.
      
       Data Division.
       File Section.

       FD City-File.
       01 Input-City.
           02 City-for-tax           pic a(15).
           02 Tax-Rate               pic 999.

       FD  Input-File.
       01 Input-Rec.
           02 Property-Address       pic x(27).
           02 City                   pic x(15).
           02 Zip                    pic 9(5).
           02 State                  pic x(2).
              88 isCA                value "CA".
           02 Bedrooms               pic 9.
           02 Bathrooms              pic 9.
           02 Sq-Ft                  pic 9(4).
           02 Property-Type          pic x(8).
              88 Property-Valid      value "Resident", 
                                           "Condo", 
                                           "Multi-Fa". 
           02 Sale-Day-of-week       pic a(3).
           02 Filler                 pic x.
           02 Sale-month             pic a(3).
           02 Filler                 pic x.
           02 Sale-day               pic 9(2).
           02 Filler                 pic x.
           02 Sale-hour              pic 9(2).
           02 Filler                 pic x.
           02 Sale-minute            pic 9(2).
           02 Filler                 pic x.
           02 Sale-second            pic 9(2).
           02 Filler                 pic x.
           02 Time-Zone              pic a(3).
           02 Filler                 pic x.
           02 Sale-year              pic 9(4).
           02 Sale-Price             pic 9(6).
           02 Property-Latitude      pic 9(8).
           02 Property-Longitude     pic 9(9).
           02 Filler                 pic x.
       *>This is the end of what happens to the "Input-File"


       FD Output-File
          linage is 58 lines
              with footing at 52
              lines at top 3
              lines at bottom 3.
       01 Output-Rec                          pic x(160) value Spaces.
       
       FD Error-File.
       01 error-print                         pic x(160) value Spaces.

       Working-Storage Section. 

       01 Report-Header.
       *>Report-Header Contains the specially formated header
       *>that will be put ontop
           02 Filler                  pic x(46) value spaces.
           02 Filler                  pic x(43)
           Value "Sacramento Area Real Estate Transactions - ".
           02 Current-Month           pic xx/.
           02 Current-Day             pic xx/.
           02 Current-Year            pic xxxx.
           02 Filler                  pic x(51) value spaces.

       01 WS-Current-Date-Fields  pic x(16).

       01 Column-Headers.  
       *>Column headers contains the entire line of headers each 
       *>column of data will have
       *>Name does not matter because this is one line designed to fit the data
           02 Filler                pic x(16) value "Property-Address".
           02 Filler                pic x(2) value spaces. 
           02 Filler                pic x(4) value "City".
           02 Filler                pic x(10) value spaces.
           02 Filler                pic x(3) value "Zip".
           02 Filler                pic x(4) value spaces.
           02 Filler                pic x(5) value "State".
           02 Filler                pic x(3) value spaces.
           02 Filler                pic x(8) value "Bedrooms". 
           02 Filler                pic x(3) value spaces.
           02 Filler                pic x(9) value "Bathrooms".
           02 Filler                pic x(5) value spaces.
           02 Filler                pic x(5) value "Sq-Ft".
           02 Filler                pic x(2) value spaces.
           02 Filer                 pic x(13) value "Property-Type".
           02 Filler                pic x(5) value spaces.
           02 Filler                pic x(10) value "Sale-Price". 
           02 Filler                pic x(3) value spaces.
           02 Filler                pic x(13) value "Price/Sq Foot".
           02 Filler                pic x(3) value spaces.
           02 Filler                pic x(5) value "Taxes".
           02 Filler                pic x(10) value spaces.
           02 Filler                pic x(8) value "City Tax".

       01 Info-Line.
           02 Property-Address-out   pic x(16).
           02 Filler                 pic x(2) value spaces.
           02 City-out               pic x(13).
           02 Filler                 pic x(1) value spaces.
           02 Zip-out                pic x(5).
           02 Filler                 pic x(2) value spaces.
           02 State-out              pic x(5).
           02 Filler                 pic x(3) value spaces.
           02 Bedrooms-out           pic x(8).
           02 Filler                 pic x(3) value spaces.
           02 Bathrooms-out          pic x(9).
           02 Filler                 pic x value spaces.
           02 Sq-Ft-out              pic z,zzz,zz9.
           02 Filler                 pic x(2) value spaces.
           02 Property-Type-out      pic x(13).
           02 Filler                 pic x(5) value spaces.
           02 Sale-Price-out         pic $z,zzz,zz9 value spaces.
           02 Filler                 pic x(3) value spaces.
           02 priceSq-Ft-out         pic $z,zzz,zz9.99.
           02 Filler                 pic x(3) value spaces.
           02 taxes-out              pic $z,zzz,zz9.99.
           02 Filler                 pic x(2) value spaces.
           02 city-taxes-out         pic $z,zzz,zz9.99.

       01 Averages.
           02 Filler                 pic x(9) value "Averages:".
           02 Filler                 pic x(35) value spaces.
           02 Bedroom-Average        pic z,zzz,zz9.9.
           02 Filler                 pic x(1) value spaces.
           02 Bathroom-Average       pic z,zzz,zz9.9.
           02 Filler                 pic x(6) value spaces.
           02 Sq-Ft-Average          pic z,zzz,zz9.9.
           02 Filler                 pic x(13) value spaces.
           02 Sale-Price-Average     pic $$,$$$,$$9.99.

       01 Sums.
           02 Bedroom-Sum            pic 99999 value 00000.
           02 Bathroom-Sum           pic 99999 value 00000.
           02 Sq-Ft-Sum              pic 99999999 value 00000000.
           02 Sale-Price-Sum         pic 99999999 value 00000000.
       01 End-Report. 
           02 Filler                 pic x(60) value spaces.
           02 Filler                 pic x(13) value "End of Report".
           02 Filler                 pic x(59) value spaces.
       
       01 Records-Processed.        
           02 Filler                 pic x(28) value 
                                     "Number of Records Processed:".
           02 counter1               pic 999 value 000.
           02 Filler                   pic x(99) value spaces.

       01 eof-flag                   pic x value "N".
       01 eof-city-flag              pic x value "N".
       01 eop-flag                   pic x value "N".
       01 error-flag                 pic x value "N".

       01 sq-ft-sub                  pic 99999 value 00000.
       01 Date-header.
           02 Month-head             pic 99.
           02 Day-head               pic 99.
           02 Year-head              pic 9999.


       *>01 lines-per-page             pic 99 value 0.
       01 page-footer.
          02 Filler                  pic x(80) value spaces.
          02 Filler                  pic x value "-".
          02 page-num                pic 9 value 1.
          02 Filler                  pic x value "-".
       01 table-index                pic 99 value 1.
       01 City-table  occurs 25 times ascending key is city-name
                                         indexed by city-table-index.
          02 city-name            pic a(15).
          02 city-tax             pic 999.

       01 bedroom-page-headers.
          02 Filler               pic x(18) value "Number of Bedrooms".
          02 Filler               pic x(5) value spaces.
          02 Filler               pic x(22) value 
                                  "Accumulated Sale Price".
       01 bedroom-table occurs 6 times.
          02 bedroom-price-sum       pic 999999999 value 0.
       01 bedroom-data-out.
          02 bedrooms-num            pic 9.
          02 Filler                  pic x(23) value spaces.
          02 bed-sum-formatted       pic $zzz,zzz,zz9.99.
       *>************************Error processing*********************
       01 error-out.
           02 Filler                       pic x(15) value
                                              "Record Number: ".
           02 error-record                 pic zzz9.
           02 Filler                       pic xx value "  ".
           02 error-message                pic x(30) value Spaces.
       01 number-of-errors                 pic 999 value 000.
       *>************************Error processing*********************

       Procedure Division.
          
       0000-Main-Logic.
           *>Main-logic is designed to open the output and input files,
           *>read in each record while incrementing 
           *>then lastly print the records and close the files

           Open Input City-File.
           Perform 1500-Load-Table until eof-city-flag= "Y".
           Close City-File.

           Perform 1000-Init.
           Perform 2000-Process-Record until eof-flag= "Y".
           Perform 3000-Finish.
           Stop Run.
           
        
       1000-Init.
           *>Open input to be read and output to be written
           Open Input Input-File.
           Open Output Output-File.
           Open Output Error-File.
           *>Move and write the Report header
           Move Function Current-Date to WS-Current-Date-Fields.
           Move WS-Current-Date-Fields(1:4) to Current-Year.
           Move WS-Current-Date-Fields(5:6) to Current-Month.
           Move WS-Current-Date-Fields(7:8) to Current-Day.
           Move Report-Header to Output-Rec.
           Write Output-Rec.

           Move " " to Output-Rec.
           Write Output-Rec. 

           *>Move and write the column headers
           Move Column-Headers to Output-Rec.
           Write Output-Rec.        

           Move " " to Output-Rec.
           Write Output-Rec.

       1500-Load-Table.
           Read City-File at end move "Y" to eof-city-flag.
           Move city-for-tax to city-name(table-index).
           Move tax-rate to city-tax(table-index).
           Add 1 to table-index.

       2000-Process-Record.
       	    Read Input-File at end move "Y" to eof-flag.
            perform 2200-Validation.
            perform 2100-Move-Write.
            

       2100-Move-Write.
           if error-flag = "Y" 
             Add 1 to number-of-errors
           else
             if bedrooms not equal 0
              Add Sale-Price to bedroom-price-sum(bedrooms)
             end-if

             Move Property-Address to Property-Address-out
             Move City to City-out
             Move Zip to Zip-out
             Move State to State-out
             Move Bedrooms to Bedrooms-out
             Move Bathrooms to Bathrooms-out
             Move Sq-Ft to Sq-Ft-out
             Move Property-Type to Property-Type-out
             Move Sale-Price to Sale-Price-out

             Add Bedrooms to Bedroom-Sum
             Add Bathrooms to Bathroom-Sum
             Add Sq-Ft to Sq-Ft-Sum
             Add Sale-Price to Sale-Price-Sum

             perform 3000-Computation

             Move Info-Line to Output-Rec

             Add 1 to counter1

             *>After everything is moved we write
             Write Output-Rec at eop perform 1999-page-end.
       1999-page-end.
           Write output-rec from page-footer
           after advancing 2 lines.
           Add 1 to page-num.
           write output-rec from Column-Headers
           after advancing page.
       3000-Computation.
           *>if statement to check square ft
           if Sq-Ft > 0 then Compute priceSq-Ft-out = Sale-Price / Sq-Ft.
           else Compute priceSq-Ft-out = 0 Add 1 to Sq-Ft-sub .

           move 1 to table-index
           Search All City-table
               At end display "NONE"
               When city-name(city-table-index) = city
           Compute city-taxes-out = 
                   Sale-price * city-tax(city-table-index) * .001.

           *>If statement for taxes
           if City = "SACRAMENTO"
             if Bedrooms-out > 1 then 
               Compute taxes-out = (Sale-Price * 0.075)
             else Compute taxes-out = (Sale-Price * 0.065)
           else Compute taxes-out = (Sale-Price * 0.06).
       2200-validation.
           if isCA and 
              Property-valid and
              Bedrooms is Numeric and 
              Bathrooms is Numeric and
              Sq-Ft is Numeric and 
              Sale-Price is Numeric
              Move "N" to error-flag
           else
              Write error-print from input-rec.*>WRITES THE ERROR IF THERE IS ONE
              Move counter1 to error-record
              if isCA Continue
              else
                  Move "Y" to error-flag
                  Move "State is Invalid"
                        to error-message
                  Write Error-print from Error-out
              End-If

              if Property-valid Continue
              else
                  Move "Y" to error-flag
                  Move "Property-Type is Invalid" 
                                 to error-message
                  Write Error-print from Error-out
              End-If

              if Bedrooms is Numeric Continue
              else
                  Move "Y" to error-flag
                  Move "Bedrooms is not Numeric"
                                to error-message
                  Write Error-print from Error-out
              End-If

              if Bathrooms is Numeric Continue
              else
                  Move "Y" to error-flag
                  Move "Bathrooms is not Numeric"
                                 to error-message
                  Write Error-print from Error-out
              End-If

              if Sq-Ft is Numeric Continue
              else
                  Move "Y" to error-flag
                  Move "Square-Feet is not Numeric"
                                   to error-message
                  Write Error-print from Error-out
              End-If

              if Sale-Price is Numeric Continue
              else
                  Move "Y" to error-flag
                  Move "Sale-Price not Numeric" 
                  to error-message 
                  Write Error-print from Error-out
              End-If.               
       3000-Finish.
           *>We do not need the input file for reading anymore
           *>So we close it
           Close Input-file.
           Move " " to Output-Rec.
           Write Output-Rec.

           *>Here we process Averages
           Compute Bedroom-Average = Bedroom-Sum/counter1.
           Compute Bathroom-Average = Bathroom-Sum/counter1.
           Compute Sq-Ft-Average = Sq-Ft-Sum/(counter1 - sq-ft-sub).
           Compute Sale-Price-Average = Sale-Price-Sum/counter1.
           Move Averages to Output-Rec.
           Write Output-Rec.

           Move " " to Output-Rec.
           Write Output-Rec.

           *>We move&write the number of records
           Compute Counter1 = Counter1 - 1. 
           *>I EXPLICITELY REMOVED NUMBER OF ERRORS
           *>BECAUSE I WAS TOLD TO KEEP THE ORIGINAL OUTPUT
           *>THE SAME AT prog2out.dat
           *>ADD num-errors field to counter1 to include errors
           Move Records-Processed to Output-Rec.
           Write Output-Rec.

           Write output-rec from " ".
           perform 0000-blank until eop-flag="Y".
           move "N" to eop-flag.
           Add 1 to page-num.

           Move 1 to counter1.
           Write output-rec from bedroom-page-headers 
           after advancing page.
           perform 8000-bedroom-print until counter1=7.
           perform 0000-blank until eop-flag="Y".

           Move End-Report to Output-Rec.
           Write Output-Rec. 

           *>The output file is complete so we close it
           Close Output-file.


           *>write errors-processed.
           Close Error-File.
         8000-bedroom-print.
            *>moves bedrooms to a reasonable format
            *>writes that data out
            Move counter1 to bedrooms-num.
            Move bedroom-price-sum(counter1) to 
                 bed-sum-formatted.
            Write output-rec from bedroom-data-out.

            Add 1 to counter1.
         0000-blank.
            *>this just wites blank lines until the end of the page
            write output-rec from " " at eop 
	    write output-rec from page-footer after advancing 2 lines
            move "Y" to eop-flag.