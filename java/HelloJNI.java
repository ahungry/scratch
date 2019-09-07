public class HelloJNI {
  static {
    System.loadLibrary("hello"); // Load native library hello.dll (Windows) or libhello.so (Unixes)
    //  at runtime
    // This library contains a native method called sayHello()
  }

  // Declare an instance native method sayHello() which receives no parameter and returns void
  private native void sayHello();
  private native int fork();
  private native String addOne(int y);

  // Test Driver
  public static void main(String[] args) {
    new HelloJNI().sayHello();  // Create an instance and invoke the native method

    // What will happen??
    int pid = new HelloJNI().fork();
    String answer;

    if (0 == pid)
      {
        answer = new HelloJNI().addOne(2);
      }
    else
      {
        answer = new HelloJNI().addOne(5);
      }

    System.out.println(answer);
    System.out.println("All done?");
  }
}

// It actually works and presents the following output:
// Will it work wth Clojure?....

// Hello World!
// Fork time!I am the parent!
// Fork time!I am the child!
// 6
// All done?
// 3
// All done?
