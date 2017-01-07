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

separate (NLColl.Transition_Systems.Arc_Spine.State)

package body State_Context is
    
    procedure Print_Context
      (Context : in Context_Type) is
    begin
        Put("Stack(3) => " & Img(Get_Token_ID(Context.Stack(3)))); Put(" | ");
        Put("Stack(2) => " & Img(Get_Token_ID(Context.Stack(2)))); Put(" | ");
        Put("Stack(1) => " & Img(Get_Token_ID(Context.Stack(1)))); Put(" | ");
        Put("Stack(0) => " & Img(Get_Token_ID(Context.Stack(0)))); Put(" | ");
        Put("Buffer(0) => " & Img(Get_Token_ID(Context.Buffer(0)))); Put(" | ");
        Put("Buffer(1) => " & Img(Get_Token_ID(Context.Buffer(1)))); Put(" | ");
        Put("Buffer(2) => " & Img(Get_Token_ID(Context.Buffer(2)))); Put(" | " );
        Put("Grandparent => " & Img(Get_Token_ID(Context.Grandparent))); Put(" | ");
        Put("Great_Grandparent => " & Img(Get_Token_ID(Context.Great_Grandparent))); 
        Put("Governor => " & Img(Get_Token_ID(Context.Governor))); Put(" | ");
        Put("Dependent => " & Img(Get_Token_ID(Context.Dependent))); 
        New_Line;
    end Print_Context;
    
    procedure Get_Context
      (Context          : in out Context_Type;
       State            : in     State_Type;
       Transition       : in     Transition_Type;
       Sentence         : in     Sentence_Type) is

        Buffer_Length : constant Length_Type := Length_Type (State.Buffer.Length);
        Stack_Length  : constant Length_Type := Length_Type (State.Stack.Length);
        pragma Unreferenced (Stack_Length);
        
        Stack_Last_Index  : constant Index_Type := State.Stack.Last_Index;
        Buffer_Last_Index : constant Extended_Index_Type := State.Buffer.Last_Index;
                                                          
        K : Index_Type renames Transition.K;
    begin

        case Transition.Name is
            when ROOT =>
                Context.Stack(0) 
                  := Sentence.Elements(Index_Type(State.Stack.Last_Element.Head_Token) - Index_Type (1));
                
            when ARC_LEFT =>

                Context.Stack(3) := 
                  Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 3), LEFT, 0);

                Context.Stack(2) := 
                  Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 2), LEFT, 0);

                Context.Stack(1) := 
                  Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 1), LEFT, 0);

                Context.Stack(0) := 
                  Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index), LEFT, K);

                if K = 0 then
                    
                    if Buffer_Length > 0 then
                        Context.Buffer(0) 
                          := Sentence.Elements(Index_Type(State.Buffer.Last_Element.Head_Token) - Index_Type (1));
                    end if;
                    if Buffer_Length > 1 then
                        Context.Buffer(1) 
                          := Sentence.Elements(Index_Type(State.Buffer.Element(Buffer_Last_Index - 1).Head_Token) - Index_Type (1));
                    end if;
                    if Buffer_Length > 2 then
                        Context.Buffer(2) 
                          := Sentence.Elements(Index_Type(State.Buffer.Element(Buffer_Last_Index - 2).Head_Token) - Index_Type (1));
                    end if;

                elsif K = 1 then

                    Context.Buffer(0) 
                      := Sentence.Elements(Index_Type(State.Stack.Last_Element.Head_Token) - Index_Type (1));

                    if Buffer_Length > 0 then
                        Context.Buffer(1) 
                          := Sentence.Elements(Index_Type(State.Buffer.Last_Element.Head_Token) - Index_Type (1));
                    end if;
                    if Buffer_Length > 1 then
                        Context.Buffer(2) 
                          := Sentence.Elements(Index_Type(State.Buffer.Element(Buffer_Last_Index - 1).Head_Token) - Index_Type (1));
                    end if;

                elsif K = 2 then

                    Context.Buffer(0) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 1), LEFT, K - 1);
                    Context.Buffer(1) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 1), LEFT, K - 2);
                    
                    if Buffer_Length > 0 then
                        Context.Buffer(2)
                          := Sentence.Elements(Index_Type(State.Stack.Last_Element.Head_Token) - Index_Type (1));
                    end if;

                else
                    Context.Buffer(0) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 1), LEFT, K - 1);
                    Context.Buffer(1) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 1), LEFT, K - 2);
                    Context.Buffer(2) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 1), LEFT, K - 3);
                end if;

                if Context.Stack(0) /= null then

                    Context.Grandparent :=    
                      Get_Token(Sentence.Elements, State.Dependency_Tree.Get_Head(Context.Stack(0).ID));
                    
                    if Context.Grandparent /= null then
                        Context.Great_Grandparent :=
                          Get_Token(Sentence.Elements, State.Dependency_Tree.Get_Head(Context.Grandparent.ID));
                    end if;
                    
                end if;

                if Context.Stack(0) /= null then
                    Context.Governor := Context.Stack(0);                    
                end if;

                if Context.Stack(1) /= null then
                    Context.Dependent := Context.Stack(1);
                end if;
                
            when ARC_RIGHT =>

                if K = 0 then
                    Context.Stack(3) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 3), LEFT, 0);
                    Context.Stack(2) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 2), LEFT, 0);
                elsif K = 1 then
                    Context.Stack(3) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 2), LEFT, 0);
                    Context.Stack(2) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 1), RIGHT, 0);
                else
                    Context.Stack(3) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 1), RIGHT, K - 2);
                    Context.Stack(2) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 1), RIGHT, K - 1);
                end if;

                Context.Stack(1) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 1), RIGHT, K);
                Context.Stack(0) := Sentence.Elements(Index_Type(State.Stack.Last_Element.Head_Token) - Index_Type (1));

                if Buffer_Length > 0 then
                    Context.Buffer(0) := Sentence.Elements(Index_Type(State.Buffer.Last_Element.Head_Token) - Index_Type (1));
                end if;

                if Buffer_Length > 1 then
                    Context.Buffer(1) 
                      := Sentence.Elements(Index_Type(State.Buffer.Element(Buffer_Last_Index - 1).Head_Token) - Index_Type (1));
                end if;

                if Buffer_Length > 2 then
                    Context.Buffer(2) 
                      := Sentence.Elements(Index_Type(State.Buffer.Element(Buffer_Last_Index - 2).Head_Token) - Index_Type (1));
                end if;

                if Context.Stack(1) /= null then
                    Context.Grandparent :=    
                      Get_Token(Sentence.Elements, State.Dependency_Tree.Get_Head(Context.Stack(1).ID));
 
                    if Context.Grandparent /= null then

                        Context.Great_Grandparent :=
                          Get_Token(Sentence.Elements, State.Dependency_Tree.Get_Head(Context.Grandparent.ID));
                    end if;
                end if;
                
                if Context.Stack(0) /= null then
                    Context.Dependent := Context.Stack(0);                    
                end if;

                if Context.Stack(1) /= null then
                    Context.Governor := Context.Stack(1);
                end if;
                
            when SHIFT  =>

                Context.Stack(3) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 3), LEFT, 0);
                Context.Stack(2) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 2), LEFT, 0);
                Context.Stack(1) := Get_Token_From_State(Sentence, State.Stack, Integer(Stack_Last_Index - 1), LEFT, 0);
                Context.Stack(0) := Sentence.Elements(Index_Type(State.Stack.Last_Element.Head_Token) - Index_Type (1));

                if Buffer_Length > 0 then
                    Context.Buffer(0) := Sentence.Elements(Index_Type(State.Buffer.Last_Element.Head_Token) - Index_Type (1));
                end if;
                if Buffer_Length > 1 then
                    Context.Buffer(1) := Sentence.Elements(Index_Type(State.Buffer.Element(Buffer_Last_Index - 1).Head_Token) - Index_Type (1));
                end if;
                if Buffer_Length > 2 then
                    Context.Buffer(2) := Sentence.Elements(Index_Type(State.Buffer.Element(Buffer_Last_Index - 2).Head_Token) - Index_Type (1));
                end if;

            when NONE =>
                raise Parser_State_Error with "Transition cannot be NIL!";

        end case;
    end Get_Context;
    
end State_Context;
