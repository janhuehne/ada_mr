generic
  with procedure Run;
    
package Generic_Runner is
  
  type Runner_Task; 
  type Runner_Task_Access is access Runner_Task;
  
  task type Runner_Task is
    entry Start;
    entry Stop;
  end Runner_Task;
  
end Generic_Runner;