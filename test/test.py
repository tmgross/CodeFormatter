def example_function(x,y):
  if x>y:
      print("x is greater than y")
  else:
    print("y is greater than or equal to x")
  for i in range(0,10):
      print(i)


class ExampleClass:
  def __init__(self, name):
    self.name=name
    print("ExampleClass created")

  def greet(self):
    print("Hello, " + self.name) 
