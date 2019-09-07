public class HelloJNI {
  static {
    System.loadLibrary("hello"); // Load native library hello.dll (Windows) or libhello.so (Unixes)
    //  at runtime
    // This library contains a native method called sayHello()
  }

  // Declare an instance native method sayHello() which receives no parameter and returns void
  private native void sayHello();
  private native String addOne(int y);

  // Test Driver
  public static void main(String[] args) {
    new HelloJNI().sayHello();  // Create an instance and invoke the native method

    String answer = new HelloJNI().addOne(2);
    System.out.println(answer);
    System.out.println("All done?");
  }
}
