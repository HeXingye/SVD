# SVD
This is SVD algorithm in fortran for image comression

## How to Run
First, you should check your computer has installed Lapack.
If you are using Mac, it already has Lapack.
you can check it in
```
cd /usr/lib/
ls -al
...
liblapack.dylib
```

## Compile the file
```
gfortran -llapack solve1.f90
./a.out
```
then run image.m file to construc graph.

## result
the original image is:
![Image text](https://raw.githubusercontent.com/HeXingye/SVD/master/1.tif)
After compression:
![Image text](https://raw.githubusercontent.com/HeXingye/SVD/master/2.tif)
