if exist rflAlign.slb del rflAlign.slb
if exist slidelist.txt del slidelist.txt
dir *.sld /b > slidelist.txt
slidelib rflAlign.slb < slidelist.txt
del slidelist.txt

