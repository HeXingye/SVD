A= dlmread('dog_bw_data.dat');
imwrite(mat2gray(A), '1.tif');
%after compression
B= dlmread('io.dat');
imwrite(mat2gray(B), '2.tif');