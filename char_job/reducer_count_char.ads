with Reducer;
with Char_Job;

package Reducer_Count_Char is

--    package Job renames Char_Job;

    package Reducer_MR is new Reducer(Char_Job.Merge_Jobs, Char_Job.Finalize);
end Reducer_Count_Char;
