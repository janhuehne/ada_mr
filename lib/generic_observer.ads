generic
  type To_Controll_Task_Access is private;
  with function Exit_Observer return Boolean;
  with function Observe(To_Controll : To_Controll_Task_Access) return Boolean;

package Generic_Observer is
  
  task type Observer_Task is
    entry Start(Arg : To_Controll_Task_Access);
    entry Stop;
  end Observer_Task;
  
end Generic_Observer;