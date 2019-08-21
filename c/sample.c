int
main (
      int argc,
      char *argv[]
      )
{
  if (argc < 2)
    {
      if (argc < 3)
        {
          if (argc < 4)
            {
              return 5;
            }
          return 3;
        }
      return 1;
    }

  return 0;
}
