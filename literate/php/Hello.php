<?php

class Hello {
  protected $firstName;
  protected $lastName;
  protected $age;

public function __construct (
  $firstName = 'Matthew',
  $lastName = 'Carter',
  $age = 99
) {
  $this->firstName = $firstName;
  $this->lastName = $lastName;
  $this->age = $age;
}

public function doGreeting () {
  echo "Hello {$this->firstName} {$this->lastName}, you are {$this->age} years old!";
}

}
