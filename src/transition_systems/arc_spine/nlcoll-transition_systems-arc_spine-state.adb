------------------------------------------------------------------------------
--                               N L C O L L
--  N a t u r a l   L a n g u a g e   C o m p o n e n t   C o l l e c t i o n
--
--          Copyright 2009-2011 M. Grella, M. Nicola, D. Christen
--
--  In collaboration with Politecnico di Torino and Università di Torino.
--
--  Supported by
--   (a) Fondazione CRT under a "VivoMeglio" grant (Speak2Home)
--   (b) Region Piedmont under a “Converging Technologies” programme (ATLAS)
--        
--  The project aims at developing open-source linguistic technologies for the
--  Italian language to improve the welfare of people, especially impaired users.
--
--  This is free software; you can redistribute it and/or modify it under
--  terms of the GNU General Public License as published by the Free Software
--  Foundation; either version 2, or (at your option) any later version.
--  This software is distributed in the hope that it will be useful, but WITH
--  OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
--  for more details. Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Ada.Text_IO; use Ada.Text_IO;
pragma Unreferenced (Ada.Text_IO);

package body NLColl.Transition_Systems.Arc_Spine.State is

    function Pop
      (Tree_Token_Vector : in out Tree_Token_Vectors.Vector) return Tree_Token_Type is
    begin
        return Element : constant Tree_Token_Type := Tree_Token_Vector.Last_Element do
            Tree_Token_Vector.Delete_Last;
        end return;
    end Pop;

    function Get_Head_Tokens
      (Tree_Token_Vector : in Tree_Token_Vectors.Vector) return Token_Index_Vectors.Vector is
    begin
        return Token_Index_Vector : Token_Index_Vectors.Vector do
            for T of Tree_Token_Vector loop
                Token_Index_Vector.Append(T.Head_Token);
            end loop;
        end return;
    end Get_Head_Tokens;
    
    function Get_Token_From_State
      (Sentence          : in Sentence_Type;
       Tree_Token_Vector : in Tree_Token_Vectors.Vector;
       Index             : in Integer;
       Spine_Direction   : in Spine_Direction_Type;
       K                 : in Index_Type) return Token_Access is
    begin
        
        if Index < Integer(Index_Type'First) then
            return null;
        
        elsif Index_Type(Index) >= Tree_Token_Vector.First_Index
          and then Index_Type(Index) <= Tree_Token_Vector.Last_Index then
            
            declare
                Tree_Token : Tree_Token_Type            renames Tree_Token_Vector.Element(Index_Type(Index));
                Spine      : Token_Index_Vectors.Vector renames Tree_Token.Spine(Spine_Direction);
            begin
                if K >= Spine.First_Index and then K <= Spine.Last_Index then
                    return Sentence.Elements(Index_Type(Spine.Element(K)) - Index_Type (1));
                end if;
            end;
            
        end if;
        
        return null;
    end Get_Token_From_State;

    procedure Perform_Shift
      (State  : in out State_Type;
       Action : in     Action_Type) is
        pragma Unreferenced (Action);
    begin
        State.Stack.Append(State.Buffer.Last_Element);
        State.Buffer.Delete_Last;
    end Perform_Shift;
    
    procedure Perform_Shift
      (State  : in out State_Type) is
    begin
        State.Stack.Append(State.Buffer.Last_Element);
        State.Buffer.Delete_Last;
    end Perform_Shift;
    
    procedure Perform_Root
      (State  : in out State_Type;
       Action : in     Action_Type) is
    begin
        State.Stack.Delete_Last;
        
        State.Dependency_Tree.Set_Arc
          (Dependent => Action.Dependent,
           Governor  => Action.Governor,
           Deprel    => Action.Deprel);
        
    end Perform_Root;
    
    procedure Perform_Root
      (State  : in out State_Type) is
    begin
        Perform_Root
          (State  => State,
           Action =>   
             (Name       => ROOT,
              K          => 0,
              Deprel     => UNKNOWN_DEPREL,
              Confidence => 0.0,
              Governor   => ROOT_ID,
              Dependent  => State.Stack.Last_Element.Head_Token));
    end Perform_Root;
    
    procedure Perform_Arc_Left
      (State  : in out State_Type;
       Action : in     Action_Type) is
        
        K         : Index_Type renames Action.K;
        Stack_0   : Tree_Token_Type := Pop(State.Stack);
        Stack_1   : constant Tree_Token_Type := Pop(State.Stack);
    begin
        
        for I in K + 1 .. Stack_0.Spine(LEFT).Last_Index loop
            Stack_0.Spine(LEFT).Delete_Last;
        end loop;

        Stack_0.Spine(LEFT).Append(Stack_1.Spine(LEFT));
        State.Stack.Append(Stack_0);
        
        Set_Arc
          (Dependency_Tree => State.Dependency_Tree,
           Dependent       => Action.Dependent,
           Governor        => Action.Governor,
           Deprel          => Action.Deprel);

    end Perform_Arc_Left;
    
    procedure Perform_Arc_Right
      (State  : in out State_Type;
       Action : in     Action_Type) is
        
        K           : Index_Type renames Action.K;
        Stack_0     : constant Tree_Token_Type := Pop(State.Stack);
        Stack_1     : Tree_Token_Type          := Pop(State.Stack);
    begin

        for I in K + 1 .. Stack_1.Spine(RIGHT).Last_Index loop
            Stack_1.Spine(RIGHT).Delete_Last;
        end loop;

        Stack_1.Spine(RIGHT).Append(Stack_0.Spine(RIGHT));
        State.Stack.Append(Stack_1);

        Set_Arc
          (Dependency_Tree => State.Dependency_Tree,
           Dependent       => Action.Dependent,
           Governor        => Action.Governor,
           Deprel          => Action.Deprel);
        
    end Perform_Arc_Right;
    
    function Is_Correct
      (State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type;
       Transition           : in Transition_Type;
       Flexibility          : in Boolean := False) return Boolean is
        
        Stack_0_Left_Spine   : Token_Index_Vectors.Vector;
        Stack_0_Right_Spine  : Token_Index_Vectors.Vector;
        Stack_1_Right_Spine  : Token_Index_Vectors.Vector;
        Stack_0_P            : Extended_Token_Index_Type;
        Stack_1_P            : Extended_Token_Index_Type;
        
    begin

        if State.Stack.Is_Empty then
            raise Parser_State_Error with "Error: State.Stack.Is_Empty";
            
        elsif State.Stack.Length = 1 then
                
            case Transition.Name is
                when SHIFT  => return not State.Buffer.Is_Empty;
                when ROOT   => return Gold_Dependency_Tree.Get_Head (State.Stack.Last_Element.Head_Token) = ROOT_ID;
                when others => raise Parser_State_Error with "Transition is not valid";
            end case;
            
        else
            
            Stack_0_Left_Spine  := State.Stack.Last_Element.Spine(LEFT);
            Stack_0_Right_Spine := State.Stack.Last_Element.Spine(RIGHT);
            Stack_1_Right_Spine := Second_To_Last(State.Stack).Spine(RIGHT);
            Stack_0_P           := Gold_Dependency_Tree.Get_Head(State.Stack.Last_Element.Head_Token);
            Stack_1_P           := Gold_Dependency_Tree.Get_Head(Second_To_Last(State.Stack).Head_Token);
        
            case Transition.Name is
                when ARC_LEFT     => return Stack_0_Left_Spine.Element(Transition.K) = Stack_1_P;
                when ARC_RIGHT    => return Stack_1_Right_Spine.Element(Transition.K) = Stack_0_P;
                
                when SHIFT        => 
                    
                    if Flexibility then                                  
                        declare                            
                            C1 : constant Boolean 
                              := State.Stack.Last_Element.Head_Token < Stack_0_P;
                            -- C1 => there is no arc (h → d) in Ag such
                            --       that h is in σ and d = v1,1;
                            
                            C2 : constant Boolean 
                              := (for some G of Get_Governors(Gold_Dependency_Tree, Get_Head_Tokens(State.Buffer)) 
                                  => Stack_0_Right_Spine.Contains(G));
                            -- C2 => there is at least one arc (h → d) in Ag with 
                            --       h = v1,k, k ∈ [1, q], and d in β. 
                        begin
                            return C1 or else C2;                            
                        end;
                    else
                        return not (Stack_0_Left_Spine.Contains(Stack_1_P) or else Stack_1_Right_Spine.Contains(Stack_0_P));
                    end if;

                when NONE | ROOT  => raise Parser_State_Error with "Transition is not valid";
            end case;
            
        end if;

    end Is_Correct;
    
    function Get_Next_Possible_Transitions   
      (State : in State_Type) return Transition_Type_Vectors.Vector is
        
        Possible_Transitions  : Transition_Type_Vectors.Vector;
    begin

        -- ROOT
        
        if Is_Second_To_Last_State (State) then
            Possible_Transitions.Append
              ((Name       => ROOT,
                K          => 0,
                Governor   => ROOT_ID,
                Dependent  => State.Stack.Last_Element.Head_Token));
        end if;

        -- SHIFT
        
        if not State.Buffer.Is_Empty then
            Possible_Transitions.Append
              ((Name       => SHIFT,
                K          => 0,
                Governor   => -1,
                Dependent  => -1));
        end if;
        
        if State.Stack.Length > 1 then
            
            -- ARC_LEFT
                
            for K in State.Stack.Element(State.Stack.Last_Index).Spine(LEFT).First_Index ..  State.Stack.Element(State.Stack.Last_Index).Spine(LEFT).Last_Index loop
                Possible_Transitions.Append
                  ((Name       => ARC_LEFT,
                    K          => K,
                    Governor   => State.Stack.Last_Element.Spine(LEFT).Element(K),
                    Dependent  => State.Stack.Element(State.Stack.Last_Index - 1).Head_Token));
            end loop;

            -- ARC_RIGHT
                
            for K in State.Stack.Element(State.Stack.Last_Index - 1).Spine(RIGHT).First_Index ..  State.Stack.Element(State.Stack.Last_Index - 1).Spine(RIGHT).Last_Index loop
                Possible_Transitions.Append
                  ((Name       => ARC_RIGHT,
                    K          => K,
                    Governor   => State.Stack.Element(State.Stack.Last_Index - 1).Spine(RIGHT).Element(K),
                    Dependent  => State.Stack.Last_Element.Head_Token));
                end loop;
                
        end if;
        
        return Possible_Transitions;
    end Get_Next_Possible_Transitions;

    procedure Perform_Action
      (State  : in out State_Type;
       Action : in     Action_Type) is
    begin
        case Action.Name is
            when SHIFT        => Perform_Shift (State, Action);
            when ROOT         => Perform_Root (State, Action);
            when ARC_LEFT     => Perform_Arc_Left (State, Action);
            when ARC_RIGHT    => Perform_Arc_Right (State, Action);
            when NONE         => null;
        end case;
    end Perform_Action;
    
    function Make_Tree_Token
      (Token_ID : Token_Index_Type) return Tree_Token_Type is
    begin
        return Tree_Token : Tree_Token_Type do
            Tree_Token.Head_Token := Token_ID;
            Tree_Token.Spine(LEFT).Append(Token_ID);
            Tree_Token.Spine(RIGHT).Append(Token_ID);
        end return;
    end Make_Tree_Token;
      
    procedure State_Initialize
      (Sentence : in Sentence_Type;
       State    : in out State_Type) is
    begin
        
        for Token : Token_Access of reverse Sentence.Elements loop
            State.Buffer.Append(Make_Tree_Token(Token.ID));
        end loop;
        
        for I in Sentence.Elements.First_Index .. Sentence.Elements.Last_Index loop
            State.Dependency_Tree.Heads.Append (NONE_ID);
            State.Dependency_Tree.Deprels.Append (Null_Unbounded_String);
            State.Dependency_Tree.Dependents.Append (Empty_Dependents_Structure);
        end loop;
        
        while Is_Deterministic_State (State) loop
            Perform_Shift (State, SHIFT_Action);
        end loop;

        State.Log_Prob    := 0.0;
           
        State.Initialized := True;
    end State_Initialize;

    function State_Initialize
      (Sentence : in Sentence_Type) return State_Type is
    begin
        return State : State_Type do
            State_Initialize
              (Sentence       => Sentence,
               State          => State);
        end return; 
    end State_Initialize;
    
    procedure State_Finalize
      (State : in out State_Type) is
    begin
        State.Stack.Clear;
        State.Buffer.Clear;
        State.Dependency_Tree.Clear;
        State.Log_Prob    := 0.0;
        State.Initialized := False;
    end State_Finalize;
    
    procedure Print_Tree_Token
      (Tree_Token      : in Tree_Token_Type;
       Print_New_Line  : in Boolean := False) is
    begin

        Put("[");
        
        Put("(");
        for I in Tree_Token.Spine(LEFT).First_Index ..  Tree_Token.Spine(LEFT).Last_Index loop
            Put(Img(Integer(Tree_Token.Spine(LEFT).Element(I))));

            if I < Tree_Token.Spine(LEFT).Last_Index then
                Put(" ");
            end if;
        end loop;
        Put(") ");

        Put("(");
        for I in Tree_Token.Spine(RIGHT).First_Index ..  Tree_Token.Spine(RIGHT).Last_Index loop
            Put(Img(Integer(Tree_Token.Spine(RIGHT).Element(I))));
            
            if I < Tree_Token.Spine(RIGHT).Last_Index then
                Put(" ");
            end if;
        end loop;
        Put(")");

        Put("]");

        if Print_New_Line then
            New_Line;
        end if;
    end Print_Tree_Token;

    procedure Print_Tree_Token_Vector
      (Tree_Token_Vector : in Tree_Token_Vectors.Vector;
       Print_Reverse     : in Boolean := False;
       Print_New_Line    : in Boolean := False) is
    begin
        if Tree_Token_Vector.Is_Empty then
            Put("[]");
        end if;

        if Print_Reverse then
            for I in reverse Tree_Token_Vector.First_Index .. Tree_Token_Vector.Last_Index loop
                Print_Tree_Token(Tree_Token_Vector.Element(I));
                
                if I /= Tree_Token_Vector.First_Index then
                    Put(" ");
                end if;
            end loop;
        else
            for I in Tree_Token_Vector.First_Index .. Tree_Token_Vector.Last_Index loop
                Print_Tree_Token(Tree_Token_Vector.Element(I));
                
                if I /= Tree_Token_Vector.Last_Index then
                    Put(" ");
                end if;
            end loop;
        end if;
        
        if Print_New_Line then
            New_Line;
        end if;
    end Print_Tree_Token_Vector;
    
    procedure Print_State
      (State : in State_Type) is
    begin
        
        Put_Line ("Stack: ");
        Print_Tree_Token_Vector(State.Stack);
        New_Line;
        
        Put_Line ("Buffer: ");
        Print_Tree_Token_Vector(State.Buffer, Print_Reverse => True);
    
        New_Line;    
    end Print_State;
      
    package body State_Context is separate;
    
end NLColl.Transition_Systems.Arc_Spine.State;
