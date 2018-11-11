with ada.Text_IO; use ada.Text_IO;

procedure Mulstack is
   N: Integer;
   Max: Integer;
   Lo: Integer;

   type Employee_Name is (Zhou, Wei, Burris, Shashidhar, Deering,
                          An, Lester, Yang, Smith, Arcos, Rabieh, Song,
                          Cho, Varol, Karabiyik, Cooper, McGuire, Najar,
                          Hope, Pray, NoHope);

   type action is (i, insert, push, D, delete, pop, q, quit);
   subtype insertAct is action range i..push;
   subtype deleteAct is action range D..pop;
   subtype quitAct is action range q..quit;

   Op : action;
   Stacknum : Integer;
   Name : Employee_Name;

   package OpIO is new Ada.Text_IO.Enumeration_IO(action); use OpIO;
   package StacknumIO is new Ada.Text_IO.Integer_IO(Integer); use StacknumIO;
   package NameIO is new Ada.Text_IO.Enumeration_IO(Employee_Name); use NameIO;
   package FloatIO is new ada.Text_IO.Float_IO(Float); use FloatIO;

   procedure sstack(Max, Lo, N : Integer) is
      UserStack : array (Lo..Max) of Employee_Name;
      L0 : Integer := 4;
      M : Integer := 27;
      MinSpace : Integer :=M - (M-L0)-Integer(Float(M - L0)*0.05);
      Availspace : Integer;
      TotalInc : Integer := 0;
      T,B: array (1..N) of Integer;
      Growth, Otop : array (1..N) of Integer;

      Overflow : exception;

      function isEmpty return Boolean is -- Check if stack is empty
      begin
         if T(Stacknum) = B(Stacknum) then
            return True;
         end if;
         return False;
      end isEmpty;

      function isFull return Boolean is --Check if stack is full
      begin
         if Stacknum = N then
            if T(Stacknum) /= M then
               return False;
            end if;
         elsif T(Stacknum) /= B(Stacknum+1) then
            return False;
         end if;
         return True;
      end isFull;

      procedure PopOp is --Delete item from stack
         Y : Integer;
      begin
         Y := T(Stacknum);
         if isEmpty then
            Put("Underflow, Stack is Empty.");

         else
            Name := UserStack(Y);
            T(Stacknum) := T(Stacknum) - 1;
            Put("Location ");
            Put(Y);
            Put(" = ");
            Put(Name);
         end if;
      end PopOp;

      procedure InitalBase (N, Max, L0 : Integer) is -- Inital setup of stacks in memory
         X : Float;
      begin
         for I in Integer range 1..N loop
            X := Float(I -1);
            X := X/Float(N);
            X := X*Float(M);
            X := X + Float(L0);
            T(I) := Integer(X);
            B(I) := T(I);
         end loop;
      end InitalBase;

      procedure Reallocate is
         J : Integer := N;
         EqualAllo : Float := 0.13;
         GrowAllo : Float := 0.87;
         Alpha : Integer;
         Beta : Integer;
         Sigma : Integer;
         Tau: Integer;
         NBase : array (1..N) of Integer;

         procedure MoveStack is
            D : Integer;
         begin

            for I in Integer range 2..N loop
               if NBase(I) < B(I) then
                  D := B(I) - NBase(I);
                  for L in Integer range (B(I)+1)..T(I) loop
                     UserStack(L - D) := UserStack(L);
                  end loop;
                  B(I) := NBase(I);
                  T(I) := T(I) - D;
               end if;
            end loop;

            for I in reverse N..2 loop
               if NBase(I) > B(I) then
                  D := NBase(I) - B(J);
                  for L in reverse (B(I)+1)..T(I) loop
                     UserStack(L+D) := UserStack(L);
                  end loop;
                  B(I) := NBase(I);
                  T(I) := T(I) + D;
               end if;
            end loop;
         end MoveStack;

         procedure report is
         begin
            for I in Integer range 1..N loop
               for L in Integer range (B(I)+1)..T(I) loop
                  Put("Location ");
                  Put(L);
                  Put(" = ");
                  if UserStack(L) in Employee_Name then
                     Put(UserStack(L));
                     New_Line;
                  end if;
                  end loop;
            end loop;
         end report;

      begin
         report;
         Availspace := M - L0;
         While  J > 0 loop
            Availspace := Availspace - (T(J) - B(J));
            if T(J) > Otop(J) then
               Growth(J) := T(J) - Otop(J);
               TotalInc := TotalInc + Growth(J);
            else
               Growth(J) := 0;
            end if;
            J := J-1;
         end loop;

         if Availspace < (MinSpace - 1) then
            raise Overflow with "No more space available";
         end if;

         Alpha := Integer(EqualAllo * Float(Availspace)/ Float(N));
         Beta := Integer(GrowAllo * Float(Availspace) / Float(TotalInc));

         NBase(1) := B(1);
         Sigma := 0;
         for I in Integer range 2..N loop
            Tau := Sigma + Alpha + Growth(I-1)*Beta;
            NBase(I) := NBase(I-1) + (T(I-1) - B(I-1)) + Integer(Tau) - Integer(Sigma);
            Sigma := Tau;
         end loop;

         T(Stacknum) := T(Stacknum) - 1;
         MoveStack;
         Put("Memory reallocated, thses are now the new Locations of contents:");
         New_Line;
         report;
         T(Stacknum) := T(Stacknum) +1;
         declare -- retry to push item back into stack
            X : Integer;
         begin
            T(Stacknum) := T(Stacknum) + 1;
            X := T(Stacknum);
            UserStack(X) := Name;
            Put("Location ");
            Put(X);
            Put("= ");
            Put(Name);
         end;

         for I in Integer range 1..N loop
            Otop(I) := T(I);
         end loop;
      end Reallocate;

      procedure PushOp(Idata : in Employee_Name) is --Insert item into stack
         X : Integer;
      begin
         T(Stacknum) := T(Stacknum) + 1;
         if isFull then
            Put("Overflow, Reallotcating..."); --report overflow and reallocate
            Reallocate;
            X := T(Stacknum);
            UserStack(X) := Idata;
         else
            X := T(Stacknum);
            UserStack(X) := Idata;
            Put("Location ");
            Put(X);
            Put(" = ");
            Put(Idata);
         end if;
      end pushop;
   begin

      InitalBase(N, Max, L0);--Setup stacks
      Dloop:
      loop
         New_Line;
         Put("Enter your data (Type q to quit): ");--get input to insert or delete from stacks
         Get(Op);
         if Op in quitAct then--exit by user
            exit Dloop;
            elsif Op in deleteAct then
            Get(Stacknum);
            PopOp;
         else
            Get(Stacknum);
            Get(Name);
            PushOp(Name);
         end if;
      end loop Dloop;
         end sstack;


begin
   --Getting array space
   Put("Enter total amount of memory: ");
   Max := Integer'Value(Get_Line);
   New_Line;
   Put("Enter number of stacks: ");
   N := Integer'Value(Get_Line);
   New_Line;
   Put("Enter value of L0: ");
   Lo := Integer'Value(Get_Line);
   New_Line;
   sstack(Max, Lo, N);

   --  Insert code here.
   null;
end Mulstack;
