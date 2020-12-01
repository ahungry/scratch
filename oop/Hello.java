class MyMath {
  final int a;
  final int b;

  public MyMath (int a, int b) {
    this.a = a;
    this.b = a;
  }

  public int sum () {
    return this.a + this.b;
  }
}

public class Hello {
  public static void main(String[] args) {
    MyMath m = new MyMath(1, 2);

    int result = m.sum();

    System.out.println("The sum is: ");
    System.out.println(result);
  }
}
