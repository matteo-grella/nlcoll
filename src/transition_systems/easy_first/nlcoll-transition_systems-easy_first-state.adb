------------------------------------------------------------------------------
--                               N L C O L L
--  N a t u r a l   L a n g u a g e   C o m p o n e n t   C o l l e c t i o n
--
--                       Copyright 2014 M. Grella
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

package body NLColl.Transition_Systems.Easy_First.State is

    function Extract
      (Tree_Token_Vector : in out Tree_Token_Vectors.Vector;
       Index             : in     Index_Type) return Tree_Token_Type is
    begin
        return Element : constant Tree_Token_Type := Tree_Token_Vector.Element(Index) do
            Tree_Token_Vector.Delete(Index);
        end return;
    end Extract;

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

    procedure Perform_Root
      (State  : in out State_Type;
       Action : in     Action_Type) is
    begin
        State.Pending.Delete_Last;
        
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
              I          => State.Pending.First_Index,
              K          => 0,
              Deprel     => UNKNOWN_DEPREL,
              Score      => 0.0,
              Governor   => ROOT_ID,
              Dependent  => State.Pending.Last_Element.Head_Token));
    end Perform_Root;
    
    procedure Perform_Arc_Left
      (State  : in out State_Type;
       Action : in     Action_Type) is

        I         : Index_Type renames Action.I;
        K         : Index_Type renames Action.K;
        
        K_Last_Index : constant Index_Type 
          := State.Pending.Reference(I + 1).Spine(LEFT).Last_Index;
    begin

        for N in K + 1 .. K_Last_Index loop
            State.Pending.Reference(I + 1).Spine(LEFT).Delete_Last;
        end loop;

        State.Pending.Reference(I + 1).Spine(LEFT).Append
          (State.Pending.Element(I).Spine(LEFT));
        
        State.Pending.Delete(I);
        
        Set_Arc
          (Dependency_Tree => State.Dependency_Tree,
           Dependent       => Action.Dependent,
           Governor        => Action.Governor,
           Deprel          => Action.Deprel);

    end Perform_Arc_Left;
    
    procedure Perform_Arc_Right
      (State  : in out State_Type;
       Action : in     Action_Type) is
        
        I           : Index_Type renames Action.I;
        K           : Index_Type renames Action.K;
        
        K_Last_Index : constant Index_Type 
          := State.Pending.Reference(I).Spine(RIGHT).Last_Index;
    begin

        for N in K + 1 .. K_Last_Index  loop
             State.Pending.Reference(I).Spine(RIGHT).Delete_Last;
        end loop;

        State.Pending.Reference(I).Spine(RIGHT).Append
          (State.Pending.Element(I + 1).Spine(RIGHT));

        State.Pending.Delete(I + 1);
        
        Set_Arc
          (Dependency_Tree => State.Dependency_Tree,
           Dependent       => Action.Dependent,
           Governor        => Action.Governor,
           Deprel          => Action.Deprel);
        
    end Perform_Arc_Right;
    
    function Is_Correct
      (State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type;
       Action               : in Action_Type;
       Top_Down_Strategy    : in Boolean := False) return Boolean is
        
        function Is_Resolved 
          (ID  : Token_Index_Type) return Boolean is
          (Is_Resolved
             (ID                   => ID,
              Dependency_Tree      => State.Dependency_Tree,
              Gold_Dependency_Tree => Gold_Dependency_Tree)) with Inline;
        
        I : Index_Type renames Action.I;
    begin

        return
          (case Action.Name is
               when ROOT      => 
                   Gold_Dependency_Tree.Get_Head(State.Pending.Element(I).Head_Token) = ROOT_ID,
                
               when ARC_LEFT  =>
                   (State.Pending.Element(I + 1).Spine(LEFT).Element(Action.K) = 
                        Gold_Dependency_Tree.Get_Head(State.Pending.Element(I).Head_Token)) 
           and then (not Top_Down_Strategy or else Is_Resolved(State.Pending.Element(I).Head_Token)) ,
            
               when ARC_RIGHT =>
                   (State.Pending.Element(I).Spine(RIGHT).Element(Action.K) = 
                        Gold_Dependency_Tree.Get_Head(State.Pending.Element(I + 1).Head_Token))
           and then (not Top_Down_Strategy or else Is_Resolved(State.Pending.Element(Action.I + 1).Head_Token))
           ,
               
               when others    => raise Parser_State_Error with "Action not supported");

    end Is_Correct;
    
    function Get_Possible_Actions
      (State                : in State_Type) return Action_Type_Vectors.Vector is
        
        Possible_Actions  : Action_Type_Vectors.Vector;
    begin

        -- ROOT
        
        if Is_Second_To_Last_State (State) then
            Possible_Actions.Append
              ((Name       => ROOT,
                I          => State.Pending.Last_Index,
                K          => 0,
                Deprel     => UNKNOWN_DEPREL,
                Score      => 0.0,
                Governor   => ROOT_ID,
                Dependent  => State.Pending.Last_Element.Head_Token));
        end if;

        if State.Pending.Length > 1 then
            
            for I in State.Pending.First_Index .. State.Pending.Last_Index - 1 loop
            
                -- ARC_LEFT
                
                for K in State.Pending.Element(I + 1).Spine(LEFT).First_Index ..  State.Pending.Element(I + 1).Spine(LEFT).Last_Index loop
                    Possible_Actions.Append
                      ((Name       => ARC_LEFT,
                        I          => I,
                        K          => K,
                        Deprel     => UNKNOWN_DEPREL,
                        Score      => 0.0,
                        Governor   => State.Pending.Element(I + 1).Spine(LEFT).Element(K),
                        Dependent  => State.Pending.Element(I).Head_Token));
                end loop;
    
                -- ARC_RIGHT
                
                for K in State.Pending.Element(I).Spine(RIGHT).First_Index ..  State.Pending.Element(I).Spine(RIGHT).Last_Index loop
                    Possible_Actions.Append
                      ((Name       => ARC_RIGHT,
                        I          => I,
                        K          => K,
                        Deprel     => UNKNOWN_DEPREL,
                        Score      => 0.0,
                        Governor   => State.Pending.Element(I).Spine(RIGHT).Element(K),
                        Dependent  => State.Pending.Element(I + 1).Head_Token));
                end loop;
                
            end loop;
            
        end if;
        
        return Possible_Actions;
    end Get_Possible_Actions;

    procedure Perform_Action
      (State  : in out State_Type;
       Action : in     Action_Type) is
    begin
        case Action.Name is
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
        
        for Token : Token_Access of Sentence.Elements loop
            State.Pending.Append(Make_Tree_Token(Token.ID));
            
            State.Dependency_Tree.Heads.Append (NONE_ID);
            State.Dependency_Tree.Deprels.Append (Null_Unbounded_String);
            State.Dependency_Tree.Dependents.Append (Empty_Dependents_Structure);
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
        State.Pending.Clear;
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
        Put_Line ("Pending: ");
        Print_Tree_Token_Vector(State.Pending);
        New_Line;    
    end Print_State;
      
    package body State_Context is separate;
    
end NLColl.Transition_Systems.Easy_First.State;
