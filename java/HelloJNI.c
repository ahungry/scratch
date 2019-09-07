// Save as "HelloJNI.c"
#include <jni.h>        // JNI header provided by JDK
#include <string.h>
#include <stdlib.h>
#include <stdio.h>      // C Standard IO Header
#include "HelloJNI.h"   // Generated

// Implementation of the native method sayHello()
JNIEXPORT void

JNICALL
Java_HelloJNI_sayHello (JNIEnv *env, jobject thisObj)
{
  printf ("Hello World!\n");
  return;
}

JNIEXPORT jstring

JNICALL
Java_HelloJNI_addOne (JNIEnv *env, jobject thisObj, jint y)
{
  int a = 1;
  char *msg = NULL;
  msg = (char *) malloc (sizeof (char) * 40);
  sprintf (msg, "%d", a + y);

  // return env->NewStringUTF (msg);
  return (*env)->NewStringUTF (env, msg);
}
