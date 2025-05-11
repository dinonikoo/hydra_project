// Note: this is an automatically generated file. Do not edit.

import java.io.Serializable;

/**
 * Test
 */
public class Person implements Serializable {
  public final String name;
  
  public final String age;
  
  public Person (String name, String age) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((age));
    this.name = name;
    this.age = age;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Person)) {
      return false;
    }
    Person o = (Person) (other);
    return name.equals(o.name) && age.equals(o.age);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * age.hashCode();
  }
  
  public Person withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new Person(name, age);
  }
  
  public Person withAge(String age) {
    java.util.Objects.requireNonNull((age));
    return new Person(name, age);
  }
}
