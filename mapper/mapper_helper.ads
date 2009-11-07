package Mapper_Helper is
  
  function Send_Job_Result_To_Reducer(Job_Result : String; Reducer_IP : String; Reducer_Port : String) return Boolean;
  
end Mapper_Helper;