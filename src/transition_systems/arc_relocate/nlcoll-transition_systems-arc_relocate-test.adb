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
with ARColl.Strings.Unbounded; use ARColl.Strings.Unbounded;
with NLColl.Transition_Systems.Arc_Relocate.State; use NLColl.Transition_Systems.Arc_Relocate.State;

with NLColl.Corpus.Treebank; use NLColl.Corpus.Treebank;

package body NLColl.Transition_Systems.Arc_Relocate.Test is

    procedure Test_Transition_Sequence
      (Sentence             : in     Sentence_Type;
       Gold_Dependency_Tree : in     Dependency_Tree_Type;
       Verbose              : in     Boolean := False) is

        State : State_Type;
    begin

        ----
        -- Initialize State
        ----

        State_Initialize
          (Sentence       => Sentence,
           State          => State);

        ----
        -- Loop until the Final State is reached
        ----

        Loop_State : while not Is_Final_State (State) loop

            if Verbose then
                New_Line;
                Put_Line (Standard_Error, "---------------------");
            end if;

            if Verbose then

                ---
                -- Print Current Dependency_Tree
                ---

                New_Line;
                Put_Line (Standard_Error, "[Sentence]");
                To_CoNLL
                  (Sentence        => Sentence,
                   Dependency_Tree => State.Dependency_Tree);

                ---
                -- Print Current State
                ---

                New_Line;
                Put_Line (Standard_Error, "[State]");
                Print_State (State);

            end if;

            declare
                use Actions;
                Next_Gold_Action          : Action_Type;
                -- Correct Action with respect to the Gold Dependency Tree
            begin

                ----
                -- Get Next Correct Action
                ----

                Next_Gold_Action := Get_Next_Correct_Action
                  (State                => State,
                   Gold_Dependency_Tree => Gold_Dependency_Tree);

                if Verbose then
                    New_Line;
                    Put_Line (Standard_Error, "[Next_Gold_Action]");
                    Put_Line
                      (Standard_Error,
                       "(" & Next_Gold_Action.Confidence'Img & ") "
                       & To_String (Next_Gold_Action));
                end if;

                ----
                -- Update State with Next_Gold_Action
                ----

                Perform_Action
                  (State  => State,
                   Action => Next_Gold_Action);

            exception

                when others =>

                    -- Print both the Gold and the Current Dependency Tree
                    -- and the Current Buffer/Stack State

                    New_Line (Standard_Error);

                    -- Gold Dependency Tree

                    To_CoNLL
                      (Sentence        => Sentence,
                       Dependency_Tree => Gold_Dependency_Tree);

                    New_Line (Standard_Error);

                    -- Current Dependency Tree

                    To_CoNLL
                      (Sentence        => Sentence,
                       Dependency_Tree => State.Dependency_Tree);

                    New_Line (Standard_Error);

                    -- Current State

                    Print_State (State);

                    raise;
            end;

        end loop Loop_State;

        if Verbose then

            ---
            -- Print Final Dependency Tree
            ---

            To_CoNLL
              (Sentence        => Sentence,
               Dependency_Tree => State.Dependency_Tree);

            New_Line (Standard_Error);

            ---
            -- Print Final State
            ---

            Print_State (State);

        end if;

        ---
        -- Check if the result is a Tree
        ---

        if not Is_Tree (State.Dependency_Tree) then
            raise Parser_State_Error
              with "Anomaly: Resulting Dependency_Tree is not a Tree";
        end if;


        if State.Dependency_Tree /= Gold_Dependency_Tree then
            raise Parser_State_Error
              with "Anomaly: Dependency_Tree differs from the Gold_Dependency_Tree";
        end if;

        if False then

            -- Print Final Dependency Tree

            To_CoNLL
              (Sentence        => Sentence,
               Dependency_Tree => State.Dependency_Tree);

            New_Line (Standard_Error);

        end if;

        ---
        -- Finalize State
        ---

        State_Finalize (State);

    end Test_Transition_Sequence;

end NLColl.Transition_Systems.Arc_Relocate.Test;
