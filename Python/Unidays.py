class BasketItem:
  def __init__(self,type):
    self.type = type
  def whatAmI(self):
    print("I am type " + self.type)

b1 = BasketItem("A")
b1.whatAmI()
