#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct Animal {
  char name[50]; // Property
  char* (*get_noise)(void* self); // Function pointer
  void (*speak)(void* self); // Function pointer
} Animal;

char* animal_get_noise (void* a) { return ((Animal*) a)->name; }
void animal_speak (void* a) { printf ("%s\n", (char*) ((Animal*) a)->get_noise (a)); }
Animal make_animal (char* name)
{
  Animal obj;
  strcpy (obj.name, name);
  obj.get_noise = &animal_get_noise;
  obj.speak = &animal_speak;

  return obj;
}

typedef Animal Dog;
char* dog_get_noise (void* a) {
  char* noise = NULL;
  char* name = ((Animal*) a)->name;
  noise = malloc (sizeof name + 10);
  strcpy (noise, "Woof woof ");
  strcpy (noise + 10, name);

  return noise;
}
Dog make_dog (char* name)
{
  Animal obj = make_animal (name);
  obj.get_noise = &dog_get_noise;

  return obj;
}

int
main ()
{
  Animal a = make_animal ("Matt");
  a.speak (&a);

  Dog d = make_dog ("Fido");
  d.speak (&d);

  exit (0);
}
