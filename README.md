# This is 2chan like strings generator library.

## Dependents Libary List
- cl-base64  
- ironclad  
- cl-ppcre  
- flexi-streams  
- crypt  
About this, use madosuki/cl-crypt instead original. For sake of multibyte.  
- xsubseq  

## Function List
- generate-trip (key &optional (char-code "ASCII"))  
This is create trip function.  
Argument of key is trip key means "#Test".  
Supported only old 10 digit key and new 12 digit key. Not supported raw key and old 8 digit key.  
  
  
- generate-id (&key ipaddr date (key-char-code "ASCII") (char-code "ASCII") solt)  
concatenate solt and ip address and date(e.g "1980/01/01 00:00:00") then that apply sha256-hmac then convert to string.  
A return value is that  string range between 0 to 7.  
  
  
- dat-to-html (dat)  
This function is convert dat to html.  
Argument of dat is filename of dat dile.  
Relate dat-to-keyword-list.  
  
  
- get-current-datetime (universal-time &optional (is-diff nil) (diff 0))  
Get date from universal-time(e.g. "1980/01/01: 00:00:00")  
  
  

- separate-trip-from-input (name)  
Separate trip key from name string.  

  
- shape-text (text)  
apply escape html special char then apply color command then apply dice command .  
For MESSAGE Text.  

  
- replace-not-available-char-when-cp932 (text)  
Replace non cp932 chars to Numeric character reference.  
