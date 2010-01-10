generic
  with procedure Run;
    
package Ada_Mr.Generics.Runner is
  
  type Runner_Task; 
  type Runner_Task_Access is access Runner_Task;
  
  task type Runner_Task is
    entry Start;
    entry Stop;
  end Runner_Task;
  
end Ada_Mr.Generics.Runner;