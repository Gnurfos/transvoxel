
# Implementation of Eric Lengyel's Transvoxel Algorithm in scala/java

Credits:
Eric Lengyel's Transvoxel Algorithm.
https://transvoxel.org/


This does not include any kind of LOD management system, and is engine agnostic: you must determine yourself block size, grid sizes, and transition sides, and then convert the obtained buffers to your preferred representation (a jMonkey example is shown below).

Compatible with Scala 2.12

## Example usage in scala/sbt:


```
// in build.sbt

libraryDependencies += "com.github.gnurfos" % "transvoxel" % "1.0.0"
```

```scala
// Main.scala

import com.github.gnurfos.transvoxel._

object TransvoxelTester {

  def main(args: Array[String]): Unit = {

    val density = new ScalarField {
      override def getDensity(x: Float, y: Float, z: Float): Float = 1 - x - y - z
    }
    val region = Block(0, 0, 0, 10)
    val threshHold = 0.5f
    val gridSize = 10
    val transitionSides = Sides.ValueSet()

    val mesh: Mesh = Transvoxel.extract(density, region, threshHold, gridSize, transitionSides)

    // 3 vertices
    assert(mesh.positions sameElements Array(
      0.5, 0.0, 0.0,
      0.0, 0.5, 0.0,
      0.0, 0.0, 0.5
    ))
    assert(mesh.normals sameElements Array(
      0.57735026f, 0.57735026f, 0.57735026f,
      0.57735026f, 0.57735026f, 0.57735026f,
      0.57735026f, 0.57735026f, 0.57735026f
    ))
    // Just one triangle at the origin corner
    assert(mesh.triIndices sameElements Array(0, 1, 2))
    
  }

}
```

## Example usage in java/maven:

```xml
<!--in pom.xml-->
<dependency>
    <groupId>com.github.gnurfos</groupId>
    <artifactId>transvoxel</artifactId>
    <version>1.0.0</version>
</dependency>
<dependency>
    <groupId>org.scala-lang</groupId>
    <artifactId>scala-library</artifactId>
    <version>2.12.8</version>
</dependency>
```

```java
// Main.java
package com.example;

import com.github.gnurfos.transvoxel.*;

import scala.Enumeration;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;

public class Main {

    public static void main(String[] args) {

        ScalarField field = new ScalarField() {
            public float getDensity(float x, float y, float z) {
                return 1 - x - y - z;
            }
        };
        Block region = new Block(0, 0, 0, 10);
        float threshHold = 0.5f;
        int gridSize = 10;
        Enumeration.ValueSet transitionSides = Sides.none();

        Mesh m = Transvoxel.extract(field, region, threshHold, gridSize, transitionSides);
        float[] positions = m.positions();
        float[] normals = m.normals();
        int[] triIndices = m.triIndices();

        assertArrayEquals(positions, new float[]{
            0.5f, 0.0f, 0.0f,
            0.0f, 0.5f, 0.0f,
            0.0f, 0.0f, 0.5f
        });
        assertArrayEquals(normals, new float[]{
            0.5773502691896258f, 0.5773502691896258f, 0.5773502691896258f,
            0.5773502691896258f, 0.5773502691896258f, 0.5773502691896258f,
            0.5773502691896258f, 0.5773502691896258f, 0.5773502691896258f
        });
        assertArrayEquals(triIndices, new int[]{0, 1, 2});

    }
}

```
You can build transition sides sets directly:
```java
Enumeration.ValueSet transitionSides = Sides.all();
Enumeration.ValueSet transitionSides = Sides.from(
        new Enumeration.Value[]{
            Sides.NegativeX(),
            Sides.PositiveX()
        });
```
or iteratively:
```java
Enumeration.ValueSet transitionSides = Sides.none();
transitionSides = transitionSides.$plus(Sides.NegativeX());
transitionSides = transitionSides.$plus(Sides.PositiveZ());
```



Example conversion to a jmonkey mesh:

```java
    static com.jme3.scene.Mesh convertToJme(com.github.gnurfos.transvoxel.Mesh mesh) {
        com.jme3.scene.Mesh jmeMesh = new com.jme3.scene.Mesh();
        jmeMesh.setBuffer(
                VertexBuffer.Type.Position,
                3,
                mesh.positions());
        jmeMesh.setBuffer(
                VertexBuffer.Type.Normal,
                3,
                mesh.normals());
        jmeMesh.setBuffer(
                VertexBuffer.Type.Index,
                3,
                mesh.triIndices());
        jmeMesh.updateBound();
        return jmeMesh;
    }
```
