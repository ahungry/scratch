# Find with something like 'locate jni.h'
JAVA_HOME=/usr/lib/jvm/java-12-openjdk

all: libhello.so

# https://www3.ntu.edu.sg/home/ehchua/programming/java/JavaNativeInterface.html
HelloJNI.h: HelloJNI.java
	javac -h . HelloJNI.java

libhello.so: HelloJNI.h HelloJNI.c
	$(CC) -fPIC -I"$(JAVA_HOME)/include" -I"$(JAVA_HOME)/include/linux" -shared -o libhello.so HelloJNI.c

# The path here should point to where libhello.so is located.
run:
	java -Djava.library.path=. HelloJNI

clean:
	-rm *.so
	-rm *.h
	-rm *.class

.PHONY: clean
