// Note: this is an automatically generated file. Do not edit.

import java.io.Serializable;

/**
 * An abstract animal type
 */
public abstract class Animal implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.example.Animal");
  
  public static final hydra.core.Name FIELD_NAME_DOG = new hydra.core.Name("dog");
  
  public static final hydra.core.Name FIELD_NAME_CAT = new hydra.core.Name("cat");
  
  private Animal () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Dog instance) ;
    
    R visit(Cat instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Animal instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Dog instance) {
      return otherwise((instance));
    }
    
    default R visit(Cat instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Dog extends hydra.example.Animal implements Serializable {
    public final hydra.example.Animal value;
    
    public Dog (hydra.example.Animal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Dog)) {
        return false;
      }
      Dog o = (Dog) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Cat extends hydra.example.Animal implements Serializable {
    public final hydra.example.Animal value;
    
    public Cat (hydra.example.Animal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cat)) {
        return false;
      }
      Cat o = (Cat) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
