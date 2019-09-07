public class HelloJNI {
  static {
    System.loadLibrary("hello"); // Load native library hello.dll (Windows) or libhello.so (Unixes)
    //  at runtime
    // This library contains a native method called sayHello()
  }

  // Declare an instance native method sayHello() which receives no parameter and returns void
  private native void sayHello();
  private native void fork();
  private native String addOne(int y);

  // Test Driver
  public static void main(String[] args) {
    new HelloJNI().sayHello();  // Create an instance and invoke the native method

    // What will happen??
    new HelloJNI().fork();

    String answer = new HelloJNI().addOne(2);
    System.out.println(answer);
    System.out.println("All done?");
  }
}

// It actually works and presents the following output:
// Will it work wth Clojure?....

    // Hello World!
    // Fork time!And I am not the child, I am the parent!
    // Fork time!I am the child...what will I do??
    // 3
    // All done?
    // 3
    // All done?
