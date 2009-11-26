generic
  with function Exit_Observer return Boolean;
  with function Observe return Boolean;

package Generic_Observer is
  
  task type Observer_Task is
    entry Start;
    entry Stop;
  end Observer_Task;
  
end Generic_Observer;