      program temp
      character file1*80,file2*80
      write(6,'('' input name to read:'')')
      read(5,'(a80)') file1
      write(6,'('' input name to write:'')')
      read(5,'(a80)') file2
      open(2,file=file1,form="formatted",status="old")
      open(3,file=file2,form="formatted",status="unknown")
c
      read(2,'(a80)') file1
      write(3,'(a80)') file1
      read(2,'(a80)') file1
      write(3,'(a80)') file1
      read(2,'(a80)') file1
      write(3,'(a80)') file1
      do n=1,5
        read(2,*) x1,x2,x3,x4,x5,x6,x7
        x4=x4/1.5
        x5=x5/1.5
        x6=x6/1.5
        x7=x7/1.5
        write(3,'(f10.1,2e14.6,4e15.7)') x1,x2,x3,x4,x5,x6,x7
      enddo
      read(2,'(a80)') file1
      write(3,'(a80)') file1
      do n=1,5
        read(2,*) x1,x2,x3,x4,x5,x6,x7
        x4=x4/1.5
        x5=x5/1.5
        x6=x6/1.5
        x7=x7/1.5
        write(3,'(f10.1,2e14.6,4e15.7)') x1,x2,x3,x4,x5,x6,x7
      enddo
      stop
      end
