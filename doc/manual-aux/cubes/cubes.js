// This program was compiled from OCaml by js_of_ocaml 2.0+git-883a1ec
(function(p){"use strict";var
a8=125,a$=123,ci=254,C=255,b8="x",R=".",ch=108,au=65535,av="+",b_='"',v=16777215,b7="g",a6="f",as=1073741823,cd=250,J=105,bg=0.5,b6="%d",b9="jsError",cf=-88,ab=110,a7=2147483,ar="'",ac=115,aq="int_of_string",cb=-32,bd=102,bb=111,a_=120,A=" ",aa="e",a9=117,I="-",Q=-48,ca="nan",c="",a5=116,b$="%.12g",aw=100,bf=" : file already exists",q="0",d=248,ba="/",at=114,bc=103,cg="fd ",ce=101,be="index out of bounds",u="number",cc=1e3,ax="src/core/lwt.ml";function
cx(a,b){throw[0,a,b]}function
bl(a){if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
b=p.console;b&&b.error&&b.error(a)}var
e=[0];function
ae(a,b){if(!a)return c;if(a&1)return ae(a-1,b)+b;var
d=ae(a>>1,b);return d+d}function
g(a){if(a!=null){this.bytes=this.fullBytes=a;this.last=this.len=a.length}}function
cy(){cx(e[4],new
g(be))}g.prototype={string:null,bytes:null,fullBytes:null,array:null,len:null,last:0,toJsString:function(){var
a=this.getFullBytes();try{return this.string=decodeURIComponent(escape(a))}catch(f){bl('MlString.toJsString: wrong encoding for "%s" ',a);return a}},toBytes:function(){if(this.string!=null)try{var
a=unescape(encodeURIComponent(this.string))}catch(f){bl('MlString.toBytes: wrong encoding for "%s" ',this.string);var
a=this.string}else{var
a=c,d=this.array,e=d.length;for(var
b=0;b<e;b++)a+=String.fromCharCode(d[b])}this.bytes=this.fullBytes=a;this.last=this.len=a.length;return a},getBytes:function(){var
a=this.bytes;if(a==null)a=this.toBytes();return a},getFullBytes:function(){var
a=this.fullBytes;if(a!==null)return a;a=this.bytes;if(a==null)a=this.toBytes();if(this.last<this.len){this.bytes=a+=ae(this.len-this.last,"\0");this.last=this.len}this.fullBytes=a;return a},toArray:function(){var
c=this.bytes;if(c==null)c=this.toBytes();var
b=[],d=this.last;for(var
a=0;a<d;a++)b[a]=c.charCodeAt(a);for(d=this.len;a<d;a++)b[a]=0;this.string=this.bytes=this.fullBytes=null;this.last=this.len;this.array=b;return b},getArray:function(){var
a=this.array;if(!a)a=this.toArray();return a},getLen:function(){var
a=this.len;if(a!==null)return a;this.toBytes();return this.len},toString:function(){var
a=this.string;return a?a:this.toJsString()},valueOf:function(){var
a=this.string;return a?a:this.toJsString()},blitToArray:function(a,b,c,d){var
g=this.array;if(g)if(c<=a)for(var
e=0;e<d;e++)b[c+e]=g[a+e];else
for(var
e=d-1;e>=0;e--)b[c+e]=g[a+e];else{var
f=this.bytes;if(f==null)f=this.toBytes();var
h=this.last-a;if(d<=h)for(var
e=0;e<d;e++)b[c+e]=f.charCodeAt(a+e);else{for(var
e=0;e<h;e++)b[c+e]=f.charCodeAt(a+e);for(;e<d;e++)b[c+e]=0}}},get:function(a){var
c=this.array;if(c)return c[a];var
b=this.bytes;if(b==null)b=this.toBytes();return a<this.last?b.charCodeAt(a):0},safeGet:function(a){if(this.len==null)this.toBytes();if(a<0||a>=this.len)cy();return this.get(a)},set:function(a,b){var
c=this.array;if(!c){if(this.last==a){this.bytes+=String.fromCharCode(b&C);this.last++;return 0}c=this.toArray()}else
if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;c[a]=b&C;return 0},safeSet:function(a,b){if(this.len==null)this.toBytes();if(a<0||a>=this.len)cy();this.set(a,b)},fill:function(a,b,c){if(a>=this.last&&this.last&&c==0)return;var
d=this.array;if(!d)d=this.toArray();else
if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;var
f=a+b;for(var
e=a;e<f;e++)d[e]=c},compare:function(a){if(this.string!=null&&a.string!=null){if(this.string<a.string)return-1;if(this.string>a.string)return 1;return 0}var
b=this.getFullBytes(),c=a.getFullBytes();if(b<c)return-1;if(b>c)return 1;return 0},equal:function(a){if(this.string!=null&&a.string!=null)return this.string==a.string;return this.getFullBytes()==a.getFullBytes()},lessThan:function(a){if(this.string!=null&&a.string!=null)return this.string<a.string;return this.getFullBytes()<a.getFullBytes()},lessEqual:function(a){if(this.string!=null&&a.string!=null)return this.string<=a.string;return this.getFullBytes()<=a.getFullBytes()}};function
L(a){this.string=a}L.prototype=new
g();function
bk(a,b){cx(a,new
L(b))}function
S(a){bk(e[4],a)}function
cl(){S(be)}function
ek(a,b){if(b<0||b>=a.length-1)cl();return a[b+1]}function
el(a,b,c){if(b<0||b>=a.length-1)cl();a[b+1]=c;return 0}function
cm(a,b,c,d,e){if(e===0)return;if(d===c.last&&c.bytes!=null){var
f=a.bytes;if(f==null)f=a.toBytes();if(b>0||a.last>e)f=f.slice(b,b+e);c.bytes+=f;c.last+=f.length;return}var
g=c.array;if(!g)g=c.toArray();else
c.bytes=c.string=null;a.blitToArray(b,g,d,e)}function
D(c,b){if(c.fun)return D(c.fun,b);var
a=c.length,d=a-b.length;if(d==0)return c.apply(null,b);else
if(d<0)return D(c.apply(null,b.slice(0,a)),b.slice(a));else
return function(a){return D(c,b.concat([a]))}}function
em(a){if(isFinite(a)){if(Math.abs(a)>=2.22507385850720138e-308)return 0;if(a!=0)return 1;return 2}return isNaN(a)?4:3}function
en(){return 0}function
cj(a){this.bytes=c;this.len=a}cj.prototype=new
g();function
co(a){if(a<0)S("String.create");return new
cj(a)}function
ep(a,b,c,d){a.fill(b,c,d)}function
bj(a){a=a.toString();var
e=a.length;if(e>31)S("format_int: format too long");var
b={justify:av,signstyle:I,filler:A,alternate:false,base:0,signedconv:false,width:0,uppercase:false,sign:1,prec:-1,conv:a6};for(var
d=0;d<e;d++){var
c=a.charAt(d);switch(c){case
I:b.justify=I;break;case
av:case
A:b.signstyle=c;break;case
q:b.filler=q;break;case"#":b.alternate=true;break;case"1":case"2":case"3":case"4":case"5":case"6":case"7":case"8":case"9":b.width=0;while(c=a.charCodeAt(d)-48,c>=0&&c<=9){b.width=b.width*10+c;d++}d--;break;case
R:b.prec=0;d++;while(c=a.charCodeAt(d)-48,c>=0&&c<=9){b.prec=b.prec*10+c;d++}d--;case"d":case"i":b.signedconv=true;case"u":b.base=10;break;case
b8:b.base=16;break;case"X":b.base=16;b.uppercase=true;break;case"o":b.base=8;break;case
aa:case
a6:case
b7:b.signedconv=true;b.conv=c;break;case"E":case"F":case"G":b.signedconv=true;b.uppercase=true;b.conv=c.toLowerCase();break}}return b}function
bh(a,b){if(a.uppercase)b=b.toUpperCase();var
f=b.length;if(a.signedconv&&(a.sign<0||a.signstyle!=I))f++;if(a.alternate){if(a.base==8)f+=1;if(a.base==16)f+=2}var
d=c;if(a.justify==av&&a.filler==A)for(var
e=f;e<a.width;e++)d+=A;if(a.signedconv)if(a.sign<0)d+=I;else
if(a.signstyle!=I)d+=a.signstyle;if(a.alternate&&a.base==8)d+=q;if(a.alternate&&a.base==16)d+="0x";if(a.justify==av&&a.filler==q)for(var
e=f;e<a.width;e++)d+=q;d+=b;if(a.justify==I)for(var
e=f;e<a.width;e++)d+=A;return new
L(d)}function
eq(a,b){var
c,f=bj(a),e=f.prec<0?6:f.prec;if(b<0){f.sign=-1;b=-b}if(isNaN(b)){c=ca;f.filler=A}else
if(!isFinite(b)){c="inf";f.filler=A}else
switch(f.conv){case
aa:var
c=b.toExponential(e),d=c.length;if(c.charAt(d-3)==aa)c=c.slice(0,d-1)+q+c.slice(d-1);break;case
a6:c=b.toFixed(e);break;case
b7:e=e?e:1;c=b.toExponential(e-1);var
i=c.indexOf(aa),h=+c.slice(i+1);if(h<-4||b.toFixed(0).length>e){var
d=i-1;while(c.charAt(d)==q)d--;if(c.charAt(d)==R)d--;c=c.slice(0,d+1)+c.slice(i);d=c.length;if(c.charAt(d-3)==aa)c=c.slice(0,d-1)+q+c.slice(d-1);break}else{var
g=e;if(h<0){g-=h+1;c=b.toFixed(g)}else
while(c=b.toFixed(g),c.length>e+1)g--;if(g){var
d=c.length-1;while(c.charAt(d)==q)d--;if(c.charAt(d)==R)d--;c=c.slice(0,d+1)}}break}return bh(f,c)}function
er(a,b){if(a.toString()==b6)return new
L(c+b);var
d=bj(a);if(b<0)if(d.signedconv){d.sign=-1;b=-b}else
b>>>=0;var
e=b.toString(d.base);if(d.prec>=0){d.filler=A;var
f=d.prec-e.length;if(f>0)e=ae(f,q)+e}return bh(d,e)}function
et(){return 0}function
ev(a,b){var
c=a[3]<<16,d=b[3]<<16;if(c>d)return 1;if(c<d)return-1;if(a[2]>b[2])return 1;if(a[2]<b[2])return-1;if(a[1]>b[1])return 1;if(a[1]<b[1])return-1;return 0}function
eF(a,b){if(a<b)return-1;if(a==b)return 0;return 1}function
cn(a,b,c){var
f=[];for(;;){if(!(c&&a===b))if(a
instanceof
g)if(b
instanceof
g){if(a!==b){var
e=a.compare(b);if(e!=0)return e}}else
return 1;else
if(a
instanceof
Array&&a[0]===(a[0]|0)){var
h=a[0];if(h===ci)h=0;if(h===cd){a=a[1];continue}else
if(b
instanceof
Array&&b[0]===(b[0]|0)){var
i=b[0];if(i===ci)i=0;if(i===cd){b=b[1];continue}else
if(h!=i)return h<i?-1:1;else
switch(h){case
d:var
e=eF(a[2],b[2]);if(e!=0)return e;break;case
251:S("equal: abstract value");case
C:var
e=ev(a,b);if(e!=0)return e;break;default:if(a.length!=b.length)return a.length<b.length?-1:1;if(a.length>1)f.push(a,b,1)}}else
return 1}else
if(b
instanceof
g||b
instanceof
Array&&b[0]===(b[0]|0))return-1;else
if(typeof
a!=u&&a&&a.compare)return a.compare(b,c);else{if(a<b)return-1;if(a>b)return 1;if(a!=b){if(!c)return NaN;if(a==a)return 1;if(b==b)return-1}}if(f.length==0)return 0;var
j=f.pop();b=f.pop();a=f.pop();if(j+1<a.length)f.push(a,b,j+1);a=a[j];b=b[j]}}function
eu(a,b){return+(cn(a,b,false)>=0)}function
ey(a){return(a[3]|a[2]|a[1])==0}function
eB(a){return[C,a&v,a>>24&v,a>>31&au]}function
eC(a,b){var
c=a[1]-b[1],d=a[2]-b[2]+(c>>24),e=a[3]-b[3]+(d>>24);return[C,c&v,d&v,e&au]}function
cq(a,b){if(a[3]>b[3])return 1;if(a[3]<b[3])return-1;if(a[2]>b[2])return 1;if(a[2]<b[2])return-1;if(a[1]>b[1])return 1;if(a[1]<b[1])return-1;return 0}function
cp(a){a[3]=a[3]<<1|a[2]>>23;a[2]=(a[2]<<1|a[1]>>23)&v;a[1]=a[1]<<1&v}function
ez(a){a[1]=(a[1]>>>1|a[2]<<23)&v;a[2]=(a[2]>>>1|a[3]<<23)&v;a[3]=a[3]>>>1}function
eE(a,b){var
e=0,d=a.slice(),c=b.slice(),f=[C,0,0,0];while(cq(d,c)>0){e++;cp(c)}while(e>=0){e--;cp(f);if(cq(d,c)>=0){f[1]++;d=eC(d,c)}ez(c)}return[0,f,d]}function
eD(a){return a[1]|a[2]<<24}function
ex(a){return a[3]<<16<0}function
eA(a){var
b=-a[1],c=-a[2]+(b>>24),d=-a[3]+(c>>24);return[C,b&v,c&v,d&au]}function
ew(a,b){var
d=bj(a);if(d.signedconv&&ex(b)){d.sign=-1;b=eA(b)}var
e=c,i=eB(d.base),h="0123456789abcdef";do{var
g=eE(b,i);b=g[1];e=h.charAt(eD(g[2]))+e}while(!ey(b));if(d.prec>=0){d.filler=A;var
f=d.prec-e.length;if(f>0)e=ae(f,q)+e}return bh(d,e)}function
eX(a){var
b=0,c=10,d=a.get(0)==45?(b++,-1):1;if(a.get(b)==48)switch(a.get(b+1)){case
a_:case
88:c=16;b+=2;break;case
bb:case
79:c=8;b+=2;break;case
98:case
66:c=2;b+=2;break}return[b,d,c]}function
cv(a){if(a>=48&&a<=57)return a-48;if(a>=65&&a<=90)return a-55;if(a>=97&&a<=122)return a-87;return-1}function
ay(a){bk(e[3],a)}function
eG(a){var
g=eX(a),f=g[0],h=g[1],d=g[2],i=-1>>>0,e=a.get(f),c=cv(e);if(c<0||c>=d)ay(aq);var
b=c;for(;;){f++;e=a.get(f);if(e==95)continue;c=cv(e);if(c<0||c>=d)break;b=d*b+c;if(b>i)ay(aq)}if(f!=a.getLen())ay(aq);b=h*b;if(d==10&&(b|0)!=b)ay(aq);return b|0}function
eH(a){return+(a>31&&a<127)}function
eI(){var
b=p.console?p.console:{},c=["log","debug","info","warn","error","assert","dir","dirxml","trace","group","groupCollapsed","groupEnd","time","timeEnd"];function
d(){}for(var
a=0;a<c.length;a++)if(!b[c[a]])b[c[a]]=d;return b}function
eJ(a){var
c=Array.prototype.slice;return function(){var
b=arguments.length>0?c.call(arguments):[undefined];return D(a,b)}}function
eK(a,b){var
d=[0];for(var
c=1;c<=a;c++)d[c]=b;return d}function
ck(a){var
b=a.length;this.array=a;this.len=this.last=b}ck.prototype=new
g();var
eL=function(){function
m(a,b){return a+b|0}function
l(a,b,c,d,e,f){b=m(m(b,a),m(d,f));return m(b<<e|b>>>32-e,c)}function
h(a,b,c,d,e,f,g){return l(b&c|~b&d,a,b,e,f,g)}function
i(a,b,c,d,e,f,g){return l(b&d|c&~d,a,b,e,f,g)}function
j(a,b,c,d,e,f,g){return l(b^c^d,a,b,e,f,g)}function
k(a,b,c,d,e,f,g){return l(c^(b|~d),a,b,e,f,g)}function
n(a,b){var
g=b;a[g>>2]|=128<<8*(g&3);for(g=(g&~3)+8;(g&63)<60;g+=4)a[(g>>2)-1]=0;a[(g>>2)-1]=b<<3;a[g>>2]=b>>29&536870911;var
l=[1732584193,4023233417,2562383102,271733878];for(g=0;g<a.length;g+=16){var
c=l[0],d=l[1],e=l[2],f=l[3];c=h(c,d,e,f,a[g+0],7,3614090360);f=h(f,c,d,e,a[g+1],12,3905402710);e=h(e,f,c,d,a[g+2],17,606105819);d=h(d,e,f,c,a[g+3],22,3250441966);c=h(c,d,e,f,a[g+4],7,4118548399);f=h(f,c,d,e,a[g+5],12,1200080426);e=h(e,f,c,d,a[g+6],17,2821735955);d=h(d,e,f,c,a[g+7],22,4249261313);c=h(c,d,e,f,a[g+8],7,1770035416);f=h(f,c,d,e,a[g+9],12,2336552879);e=h(e,f,c,d,a[g+10],17,4294925233);d=h(d,e,f,c,a[g+11],22,2304563134);c=h(c,d,e,f,a[g+12],7,1804603682);f=h(f,c,d,e,a[g+13],12,4254626195);e=h(e,f,c,d,a[g+14],17,2792965006);d=h(d,e,f,c,a[g+15],22,1236535329);c=i(c,d,e,f,a[g+1],5,4129170786);f=i(f,c,d,e,a[g+6],9,3225465664);e=i(e,f,c,d,a[g+11],14,643717713);d=i(d,e,f,c,a[g+0],20,3921069994);c=i(c,d,e,f,a[g+5],5,3593408605);f=i(f,c,d,e,a[g+10],9,38016083);e=i(e,f,c,d,a[g+15],14,3634488961);d=i(d,e,f,c,a[g+4],20,3889429448);c=i(c,d,e,f,a[g+9],5,568446438);f=i(f,c,d,e,a[g+14],9,3275163606);e=i(e,f,c,d,a[g+3],14,4107603335);d=i(d,e,f,c,a[g+8],20,1163531501);c=i(c,d,e,f,a[g+13],5,2850285829);f=i(f,c,d,e,a[g+2],9,4243563512);e=i(e,f,c,d,a[g+7],14,1735328473);d=i(d,e,f,c,a[g+12],20,2368359562);c=j(c,d,e,f,a[g+5],4,4294588738);f=j(f,c,d,e,a[g+8],11,2272392833);e=j(e,f,c,d,a[g+11],16,1839030562);d=j(d,e,f,c,a[g+14],23,4259657740);c=j(c,d,e,f,a[g+1],4,2763975236);f=j(f,c,d,e,a[g+4],11,1272893353);e=j(e,f,c,d,a[g+7],16,4139469664);d=j(d,e,f,c,a[g+10],23,3200236656);c=j(c,d,e,f,a[g+13],4,681279174);f=j(f,c,d,e,a[g+0],11,3936430074);e=j(e,f,c,d,a[g+3],16,3572445317);d=j(d,e,f,c,a[g+6],23,76029189);c=j(c,d,e,f,a[g+9],4,3654602809);f=j(f,c,d,e,a[g+12],11,3873151461);e=j(e,f,c,d,a[g+15],16,530742520);d=j(d,e,f,c,a[g+2],23,3299628645);c=k(c,d,e,f,a[g+0],6,4096336452);f=k(f,c,d,e,a[g+7],10,1126891415);e=k(e,f,c,d,a[g+14],15,2878612391);d=k(d,e,f,c,a[g+5],21,4237533241);c=k(c,d,e,f,a[g+12],6,1700485571);f=k(f,c,d,e,a[g+3],10,2399980690);e=k(e,f,c,d,a[g+10],15,4293915773);d=k(d,e,f,c,a[g+1],21,2240044497);c=k(c,d,e,f,a[g+8],6,1873313359);f=k(f,c,d,e,a[g+15],10,4264355552);e=k(e,f,c,d,a[g+6],15,2734768916);d=k(d,e,f,c,a[g+13],21,1309151649);c=k(c,d,e,f,a[g+4],6,4149444226);f=k(f,c,d,e,a[g+11],10,3174756917);e=k(e,f,c,d,a[g+2],15,718787259);d=k(d,e,f,c,a[g+9],21,3951481745);l[0]=m(c,l[0]);l[1]=m(d,l[1]);l[2]=m(e,l[2]);l[3]=m(f,l[3])}var
o=[];for(var
g=0;g<4;g++)for(var
n=0;n<4;n++)o[g*4+n]=l[g]>>8*n&C;return o}return function(a,b,c){var
h=[];if(a.array){var
f=a.array;for(var
d=0;d<c;d+=4){var
e=d+b;h[d>>2]=f[e]|f[e+1]<<8|f[e+2]<<16|f[e+3]<<24}for(;d<c;d++)h[d>>2]|=f[d+b]<<8*(d&3)}else{var
g=a.getFullBytes();for(var
d=0;d<c;d+=4){var
e=d+b;h[d>>2]=g.charCodeAt(e)|g.charCodeAt(e+1)<<8|g.charCodeAt(e+2)<<16|g.charCodeAt(e+3)<<24}for(;d<c;d++)h[d>>2]|=g.charCodeAt(d+b)<<8*(d&3)}return new
ck(n(h,c))}}();function
r(a){bk(e[2],a)}function
cr(a){if(!a.opened)r("Cannot flush a closed channel");if(a.buffer==c)return 0;if(a.output)switch(a.output.length){case
2:a.output(a,a.buffer);break;default:a.output(a.buffer)}a.buffer=c}function
cw(a){a=a
instanceof
g?a.toString():a;r(a+": No such file or directory")}var
eo=ba;function
az(a){a=a
instanceof
g?a.toString():a;if(a.charCodeAt(0)!=47)a=eo+a;var
e=a.split(ba),b=[];for(var
d=0;d<e.length;d++)switch(e[d]){case"..":if(b.length>1)b.pop();break;case
R:case
c:if(b.length==0)b.push(c);break;default:b.push(e[d]);break}b.orig=a;return b}function
K(){this.content={}}K.prototype={exists:function(a){return this.content[a]?1:0},mk:function(a,b){this.content[a]=b},get:function(a){return this.content[a]},list:function(){var
a=[];for(var
b
in
this.content)a.push(b);return a},remove:function(a){delete
this.content[a]}};var
aB=new
K();aB.mk(c,new
K());function
bi(a){var
b=aB;for(var
c=0;c<a.length;c++){if(!(b.exists&&b.exists(a[c])))cw(a.orig);b=b.get(a[c])}return b}function
e7(a){var
c=az(a),b=bi(c);return b
instanceof
K?1:0}function
ad(a){this.data=a}ad.prototype={content:function(){return this.data},truncate:function(){this.data.length=0}};function
es(a,b){var
e=az(a),c=aB;for(var
f=0;f<e.length-1;f++){var
d=e[f];if(!c.exists(d))c.mk(d,new
K());c=c.get(d);if(!(c
instanceof
K))r(e.orig+bf)}var
d=e[e.length-1];if(c.exists(d))r(e.orig+bf);if(b
instanceof
K)c.mk(d,b);else
if(b
instanceof
ad)c.mk(d,b);else
if(b
instanceof
g)c.mk(d,new
ad(b.getArray()));else
if(b
instanceof
Array)c.mk(d,new
ad(b));else
if(b.toString)c.mk(d,new
ad(new
g(b.toString()).getArray()));else
S("caml_fs_register")}function
e6(a){var
b=aB,d=az(a),e;for(var
c=0;c<d.length;c++){if(b.auto)e=b.auto;if(!(b.exists&&b.exists(d[c])))return e?e(d.join(ba)):0;b=b.get(d[c])}return 1}function
af(a,b,c){if(e.fds===undefined)e.fds=new
Array();c=c?c:{};var
d={};d.array=b;d.offset=c.append?d.array.length:0;d.flags=c;e.fds[a]=d;e.fd_last_idx=a;return a}function
fe(a,b,c){var
d={};while(b){switch(b[1]){case
0:d.rdonly=1;break;case
1:d.wronly=1;break;case
2:d.append=1;break;case
3:d.create=1;break;case
4:d.truncate=1;break;case
5:d.excl=1;break;case
6:d.binary=1;break;case
7:d.text=1;break;case
8:d.nonblock=1;break}b=b[2]}var
g=a.toString(),i=az(a);if(d.rdonly&&d.wronly)r(g+" : flags Open_rdonly and Open_wronly are not compatible");if(d.text&&d.binary)r(g+" : flags Open_text and Open_binary are not compatible");if(e6(a)){if(e7(a))r(g+" : is a directory");if(d.create&&d.excl)r(g+bf);var
h=e.fd_last_idx?e.fd_last_idx:0,f=bi(i);if(d.truncate)f.truncate();return af(h+1,f.content(),d)}else
if(d.create){var
h=e.fd_last_idx?e.fd_last_idx:0;es(a,[]);var
f=bi(i);return af(h+1,f.content(),d)}else
cw(a)}af(0,[]);af(1,[]);af(2,[]);function
eM(a){var
b=e.fds[a];if(b.flags.wronly)r(cg+a+" is writeonly");return{data:b,fd:a,opened:true}}function
fb(a){if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
b=p.console;b&&b.log&&b.log(a)}var
aA=new
Array();function
e3(a,b){var
e=new
g(b),d=e.getLen();for(var
c=0;c<d;c++)a.data.array[a.data.offset+c]=e.get(c);a.data.offset+=d;return 0}function
eN(a){var
b;switch(a){case
1:b=fb;break;case
2:b=bl;break;default:b=e3}var
f=e.fds[a];if(f.flags.rdonly)r(cg+a+" is readonly");var
d={data:f,fd:a,opened:true,buffer:c,output:b};aA[d.fd]=d;return d}function
eO(){var
a=0;for(var
b
in
aA)if(aA[b].opened)a=[0,aA[b],a];return a}function
cs(a,b,c,d){if(!a.opened)r("Cannot output to a closed channel");var
f;if(c==0&&b.getLen()==d)f=b;else{f=co(d);cm(b,c,f,0,d)}var
e=f.toString(),g=e.lastIndexOf("\n");if(g<0)a.buffer+=e;else{a.buffer+=e.substr(0,g+1);cr(a);a.buffer+=e.substr(g+1)}}function
cu(a){return new
g(a)}function
eP(a,b){var
c=cu(String.fromCharCode(b));cs(a,c,0,1)}function
eY(a){throw a}function
eZ(){eY(e[6])}function
eQ(a,b){if(b==0)eZ();return a%b}if(!Math.imul)Math.imul=function(a,b){return((a>>16)*b<<16)+(a&au)*b|0};var
eR=Math.imul;function
eT(a,b){return+(cn(a,b,false)!=0)}function
eU(a){return+(a
instanceof
Array)}function
eV(a){return a
instanceof
Array?a[0]:cc}function
e0(a,b){e[a+1]=b}var
ct={};function
e1(a,b){ct[a.toString()]=b;return 0}var
eW=0;function
e2(a){a[2]=eW++;return a}function
e4(){return 32}function
e5(a){if(p.quit)p.quit(a);S("Function 'exit' not implemented")}function
e8(){var
a=new
Date()^4294967295*Math.random();return{valueOf:function(){return a},0:0,1:a,length:2}}function
e9(a){var
b=1;while(a&&a.joo_tramp){a=a.joo_tramp.apply(null,a.joo_args);b++}return a}function
e_(a,b){return{joo_tramp:a,joo_args:b}}function
e$(a,b){if(typeof
b==="function"){a.fun=b;return 0}if(b.fun){a.fun=b.fun;return 0}var
c=b.length;while(c--)a[c]=b[c];return 0}function
eS(a){return ct[a]}function
fa(a){if(a
instanceof
Array)return a;if(p.RangeError&&a
instanceof
p.RangeError&&a.message&&a.message.match(/maximum call stack/i))return[0,e[9]];if(p.InternalError&&a
instanceof
p.InternalError&&a.message&&a.message.match(/too much recursion/i))return[0,e[9]];if(a
instanceof
p.Error)return[0,eS(b9),a];return[0,e[3],new
L(String(a))]}var
i=ek,h=el,$=cm,B=co,aZ=eq,am=er,a0=eH,a4=eJ,x=eK,b3=cr,b2=eN,b4=eP,b5=eQ,b=cu,an=eV,a=e0,aY=e1,w=e2,ap=e9,z=e_,a1=fa;function
j(a,b){return a.length==1?a(b):D(a,[b])}function
n(a,b,c){return a.length==2?a(b,c):D(a,[b,c])}function
l(a,b,c,d){return a.length==3?a(b,c,d):D(a,[b,c,d])}function
ao(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):D(a,[b,c,d,e,f])}var
aE=[d,b("Failure"),-3],aC=[d,b("Invalid_argument"),-4],bG=[d,b("Match_failure"),-8],bF=[d,b("Stack_overflow"),-9],N=[d,b("Assert_failure"),-11],bH=[d,b("Undefined_recursive_module"),-12],aN=b('File "%s", line %d, characters %d-%d: %s');a(11,bH);a(8,bF);a(7,bG);a(6,[d,b("Not_found"),-7]);a(5,[d,b("Division_by_zero"),-6]);a(4,[d,b("End_of_file"),-5]);a(3,aC);a(2,aE);a(1,[d,b("Sys_error"),-2]);aY(b("Pervasives.array_bound_error"),[0,aC,b(be)]);var
dp=[d,b("Out_of_memory"),-1],cD=b(b$),cC=b(R),cA=b("true"),cB=b("false"),cz=b("Pervasives.Exit"),cE=b("Pervasives.do_at_exit"),cG=b("Array.Bottom"),cJ=b("\\b"),cK=b("\\t"),cL=b("\\n"),cM=b("\\r"),cI=b("\\\\"),cH=b("\\'"),cO=b("String.blit"),cN=b("String.sub"),cP=b("Sys.Break"),cQ=b("Queue.Empty"),cS=b("CamlinternalLazy.Undefined"),cT=b("Buffer.add: cannot grow buffer"),c9=b(c),c_=b(c),db=b(b$),dc=b(b_),dd=b(b_),c$=b(ar),da=b(ar),c8=b(ca),c6=b("neg_infinity"),c7=b("infinity"),c5=b(R),c4=b("printf: bad positional specification (0)."),c3=b("%_"),c2=[0,b("printf.ml"),143,8],c0=b(ar),c1=b("Printf: premature end of format string '"),cW=b(ar),cX=b(" in format string '"),cY=b(", at char number "),cZ=b("Printf: bad conversion %"),cU=b("Sformat.index_of_int: negative argument "),di=b(c),dj=b(", %s%s"),dB=[1,1],dC=b("%s\n"),dD=b("(Program not linked with -g, cannot print stack backtrace)\n"),dv=b("Raised at"),dy=b("Re-raised at"),dz=b("Raised by primitive operation at"),dA=b("Called from"),dw=b('%s file "%s", line %d, characters %d-%d'),dx=b("%s unknown location"),dq=b("Out of memory"),dr=b("Stack overflow"),ds=b("Pattern matching failed"),dt=b("Assertion failed"),du=b("Undefined recursive module"),dk=b("(%s%s)"),dl=b(c),dm=b(c),dn=b("(%s)"),dh=b(b6),df=b("%S"),dg=b("_"),dM=b("Random.int"),dF=b(b8),dN=[0,987910699,495797812,364182224,414272206,318284740,990407751,383018966,270373319,840823159,24560019,536292337,512266505,189156120,730249596,143776328,51606627,140166561,366354223,1003410265,700563762,981890670,913149062,526082594,1021425055,784300257,667753350,630144451,949649812,48546892,415514493,258888527,511570777,89983870,283659902,308386020,242688715,482270760,865188196,1027664170,207196989,193777847,619708188,671350186,149669678,257044018,87658204,558145612,183450813,28133145,901332182,710253903,510646120,652377910,409934019,801085050],dP=b("Lwt_sequence.Empty"),dX=[0,b(ax),648,20],dY=[0,b(ax),651,8],dW=[0,b(ax),498,8],dV=[0,b(ax),487,9],dU=b("Lwt.wakeup_result"),dR=b("Fatal error: exception "),dQ=b("Lwt.Canceled"),d6=b("Js.Error"),d7=b(b9),d8=b("Dom_html.Canvas_not_available"),ea=b("Exception during Lwt.async: ");function
aD(a){throw[0,aE,a]}function
M(a){throw[0,aC,a]}w([d,cz,0]);function
k(a,b){var
c=a.getLen(),e=b.getLen(),d=B(c+e|0);$(a,0,d,0,c);$(b,0,d,c,e);return d}function
ag(a){return b(c+a)}eM(0);b2(1);var
T=b2(2);function
bm(a,b){return cs(a,b,0,b.getLen())}function
bn(a){return bm(T,a)}function
aF(a){var
b=eO(0);for(;;){if(b){var
c=b[2],d=b[1];try{b3(d)}catch(f){}var
b=c;continue}return 0}}aY(cE,aF);function
cF(a,b){return b4(a,b)}function
bo(a){return b3(a)}function
bp(a,b){if(0===a)return[0];var
d=x(a,j(b,0)),e=a-1|0,f=1;if(!(e<1)){var
c=f;for(;;){d[c+1]=j(b,c);var
g=c+1|0;if(e!==c){var
c=g;continue}break}}return d}w([d,cG,0]);function
ah(a,b){var
c=B(a);ep(c,0,a,b);return c}function
ai(a,b,c){if(0<=b)if(0<=c)if(!((a.getLen()-c|0)<b)){var
d=B(c);$(a,b,d,0,c);return d}return M(cN)}function
aG(a,b,c,d,e){if(0<=e)if(0<=b)if(!((a.getLen()-e|0)<b))if(0<=d)if(!((c.getLen()-e|0)<d))return $(a,b,c,d,e);return M(cO)}var
aH=e4(0),U=eR(aH/8|0,(1<<(aH-10|0))-1|0)-1|0;w([d,cP,0]);var
cR=w([d,cQ,0]);w([d,cS,0]);function
aI(a){var
b=1<=a?a:1,c=U<b?U:b,d=B(c);return[0,d,0,c,d]}function
aJ(a){return ai(a[1],0,a[2])}function
bq(a,b){var
c=[0,a[3]];for(;;){if(c[1]<(a[2]+b|0)){c[1]=2*c[1]|0;continue}if(U<c[1])if((a[2]+b|0)<=U)c[1]=U;else
aD(cT);var
d=B(c[1]);aG(a[1],0,d,0,a[2]);a[1]=d;a[3]=c[1];return 0}}function
V(a,b){var
c=a[2];if(a[3]<=c)bq(a,1);a[1].safeSet(c,b);a[2]=c+1|0;return 0}function
aK(a,b){var
c=b.getLen(),d=a[2]+c|0;if(a[3]<d)bq(a,c);$(b,0,a[1],a[2],c);a[2]=d;return 0}function
aL(a){return 0<=a?a:aD(k(cU,ag(a)))}function
br(a,b){return aL(a+b|0)}var
cV=1;function
bs(a){return br(cV,a)}function
bt(a){return ai(a,0,a.getLen())}function
bu(a,b,c){var
d=k(cX,k(a,cW)),e=k(cY,k(ag(b),d));return M(k(cZ,k(ah(1,c),e)))}function
W(a,b,c){return bu(bt(a),b,c)}function
aj(a){return M(k(c1,k(bt(a),c0)))}function
E(f,b,c,d){function
j(a){if((f.safeGet(a)+Q|0)<0||9<(f.safeGet(a)+Q|0))return a;var
b=a+1|0;for(;;){var
c=f.safeGet(b);if(48<=c){if(!(58<=c)){var
b=b+1|0;continue}}else
if(36===c)return b+1|0;return a}}var
k=j(b+1|0),g=aI((c-k|0)+10|0);V(g,37);var
e=d,i=0;for(;;){if(e){var
m=[0,e[1],i],e=e[2],i=m;continue}var
a=k,h=i;for(;;){if(a<=c){var
l=f.safeGet(a);if(42===l){if(h){var
n=h[2];aK(g,ag(h[1]));var
a=j(a+1|0),h=n;continue}throw[0,N,c2]}V(g,l);var
a=a+1|0;continue}return aJ(g)}}}function
bv(a,b,c,d,e){var
f=E(b,c,d,e);if(78!==a)if(ab!==a)return f;f.safeSet(f.getLen()-1|0,a9);return f}function
bw(a){return function(d,b){var
k=d.getLen();function
l(a,b){var
m=40===a?41:a8,c=b;for(;;){if(k<=c)return aj(d);if(37===d.safeGet(c)){var
e=c+1|0;if(k<=e)return aj(d);var
f=d.safeGet(e),g=f-40|0;if(g<0||1<g){var
i=g-83|0;if(i<0||2<i)var
h=1;else
switch(i){case
1:var
h=1;break;case
2:var
j=1,h=0;break;default:var
j=0,h=0}if(h){var
c=e+1|0;continue}}else
var
j=0===g?0:1;if(j)return f===m?e+1|0:W(d,b,f);var
c=l(f,e+1|0)+1|0;continue}var
c=c+1|0;continue}}return l(a,b)}}function
bx(i,b,c){var
m=i.getLen()-1|0;function
s(a){var
k=a;a:for(;;){if(k<m){if(37===i.safeGet(k)){var
f=0,h=k+1|0;for(;;){if(m<h)var
e=aj(i);else{var
o=i.safeGet(h);if(58<=o){if(95===o){var
f=1,h=h+1|0;continue}}else
if(32<=o)switch(o+cb|0){case
1:case
2:case
4:case
5:case
6:case
7:case
8:case
9:case
12:case
15:break;case
0:case
3:case
11:case
13:var
h=h+1|0;continue;case
10:var
h=l(b,f,h,J);continue;default:var
h=h+1|0;continue}var
d=h;b:for(;;){if(m<d)var
e=aj(i);else{var
j=i.safeGet(d);if(126<=j)var
g=0;else
switch(j){case
78:case
88:case
aw:case
J:case
bb:case
a9:case
a_:var
e=l(b,f,d,J),g=1;break;case
69:case
70:case
71:case
ce:case
bd:case
bc:var
e=l(b,f,d,bd),g=1;break;case
33:case
37:case
44:case
64:var
e=d+1|0,g=1;break;case
83:case
91:case
ac:var
e=l(b,f,d,ac),g=1;break;case
97:case
at:case
a5:var
e=l(b,f,d,j),g=1;break;case
76:case
ch:case
ab:var
t=d+1|0;if(m<t)var
e=l(b,f,d,J),g=1;else{var
q=i.safeGet(t)+cf|0;if(q<0||32<q)var
r=1;else
switch(q){case
0:case
12:case
17:case
23:case
29:case
32:var
e=n(c,l(b,f,d,j),J),g=1,r=0;break;default:var
r=1}if(r)var
e=l(b,f,d,J),g=1}break;case
67:case
99:var
e=l(b,f,d,99),g=1;break;case
66:case
98:var
e=l(b,f,d,66),g=1;break;case
41:case
a8:var
e=l(b,f,d,j),g=1;break;case
40:var
e=s(l(b,f,d,j)),g=1;break;case
a$:var
u=l(b,f,d,j),v=n(bw(j),i,u),p=u;for(;;){if(p<(v-2|0)){var
p=n(c,p,i.safeGet(p));continue}var
d=v-1|0;continue b}default:var
g=0}if(!g)var
e=W(i,d,j)}break}}var
k=e;continue a}}var
k=k+1|0;continue}return k}}s(0);return 0}function
by(a){var
d=[0,0,0,0];function
b(a,b,c){var
f=41!==c?1:0,g=f?a8!==c?1:0:f;if(g){var
e=97===c?2:1;if(at===c)d[3]=d[3]+1|0;if(a)d[2]=d[2]+e|0;else
d[1]=d[1]+e|0}return b+1|0}bx(a,b,function(a,b){return a+1|0});return d[1]}function
bz(a,b,c){var
g=a.safeGet(c);if((g+Q|0)<0||9<(g+Q|0))return n(b,0,c);var
e=g+Q|0,d=c+1|0;for(;;){var
f=a.safeGet(d);if(48<=f){if(!(58<=f)){var
e=(10*e|0)+(f+Q|0)|0,d=d+1|0;continue}}else
if(36===f)return 0===e?aD(c4):n(b,[0,aL(e-1|0)],d+1|0);return n(b,0,c)}}function
o(a,b){return a?b:bs(b)}function
bA(a,b){return a?a[1]:b}function
bB(aQ,b,c,d,e,f,g){var
A=j(b,g);function
aj(a){return n(d,A,a)}function
aR(a,b,h,aS){var
p=h.getLen();function
C(r,b){var
m=b;for(;;){if(p<=m)return j(a,A);var
d=h.safeGet(m);if(37===d){var
l=function(a,b){return i(aS,bA(a,b))},ay=function(g,f,c,d){var
a=d;for(;;){var
aa=h.safeGet(a)+cb|0;if(!(aa<0||25<aa))switch(aa){case
1:case
2:case
4:case
5:case
6:case
7:case
8:case
9:case
12:case
15:break;case
10:return bz(h,function(a,b){var
d=[0,l(a,f),c];return ay(g,o(a,f),d,b)},a+1|0);default:var
a=a+1|0;continue}var
p=h.safeGet(a);if(!(124<=p))switch(p){case
78:case
88:case
aw:case
J:case
bb:case
a9:case
a_:var
bj=l(g,f),bk=am(bv(p,h,m,a,c),bj);return q(o(g,f),bk,a+1|0);case
69:case
71:case
ce:case
bd:case
bc:var
a8=l(g,f),ba=aZ(E(h,m,a,c),a8);return q(o(g,f),ba,a+1|0);case
76:case
ch:case
ab:var
af=h.safeGet(a+1|0)+cf|0;if(!(af<0||32<af))switch(af){case
0:case
12:case
17:case
23:case
29:case
32:var
S=a+1|0,ag=p-108|0;if(ag<0||2<ag)var
ak=0;else{switch(ag){case
1:var
ak=0,al=0;break;case
2:var
bi=l(g,f),aF=am(E(h,m,S,c),bi),al=1;break;default:var
bh=l(g,f),aF=am(E(h,m,S,c),bh),al=1}if(al)var
aD=aF,ak=1}if(!ak)var
bg=l(g,f),aD=ew(E(h,m,S,c),bg);return q(o(g,f),aD,S+1|0)}var
be=l(g,f),bf=am(bv(ab,h,m,a,c),be);return q(o(g,f),bf,a+1|0);case
37:case
64:return q(f,ah(1,p),a+1|0);case
83:case
ac:var
w=l(g,f);if(ac===p)var
x=w;else{var
b=[0,0],aq=w.getLen()-1|0,aT=0;if(!(aq<0)){var
L=aT;for(;;){var
v=w.safeGet(L),bq=14<=v?34===v?1:92===v?1:0:11<=v?13<=v?1:0:8<=v?1:0,aW=bq?2:a0(v)?1:4;b[1]=b[1]+aW|0;var
aX=L+1|0;if(aq!==L){var
L=aX;continue}break}}if(b[1]===w.getLen())var
aL=w;else{var
i=B(b[1]);b[1]=0;var
ar=w.getLen()-1|0,aU=0;if(!(ar<0)){var
K=aU;for(;;){var
u=w.safeGet(K),y=u-34|0;if(y<0||58<y)if(-20<=y)var
T=1;else{switch(y+34|0){case
8:i.safeSet(b[1],92);b[1]++;i.safeSet(b[1],98);var
I=1;break;case
9:i.safeSet(b[1],92);b[1]++;i.safeSet(b[1],a5);var
I=1;break;case
10:i.safeSet(b[1],92);b[1]++;i.safeSet(b[1],ab);var
I=1;break;case
13:i.safeSet(b[1],92);b[1]++;i.safeSet(b[1],at);var
I=1;break;default:var
T=1,I=0}if(I)var
T=0}else
var
T=(y-1|0)<0||56<(y-1|0)?(i.safeSet(b[1],92),b[1]++,i.safeSet(b[1],u),0):1;if(T)if(a0(u))i.safeSet(b[1],u);else{i.safeSet(b[1],92);b[1]++;i.safeSet(b[1],48+(u/aw|0)|0);b[1]++;i.safeSet(b[1],48+((u/10|0)%10|0)|0);b[1]++;i.safeSet(b[1],48+(u%10|0)|0)}b[1]++;var
aV=K+1|0;if(ar!==K){var
K=aV;continue}break}}var
aL=i}var
x=k(dd,k(aL,dc))}if(a===(m+1|0))var
aH=x;else{var
H=E(h,m,a,c);try{var
U=0,s=1;for(;;){if(H.getLen()<=s)var
as=[0,0,U];else{var
X=H.safeGet(s);if(49<=X)if(58<=X)var
an=0;else
var
as=[0,eG(ai(H,s,(H.getLen()-s|0)-1|0)),U],an=1;else{if(45===X){var
U=1,s=s+1|0;continue}var
an=0}if(!an){var
s=s+1|0;continue}}var
Z=as;break}}catch(f){f=a1(f);if(f[1]!==aE)throw f;var
Z=bu(H,0,ac)}var
M=Z[1],z=x.getLen(),aY=Z[2],N=0,a2=32;if(M===z)if(0===N)var
_=x,ao=1;else
var
ao=0;else
var
ao=0;if(!ao)if(M<=z)var
_=ai(x,N,z);else{var
Y=ah(M,a2);if(aY)aG(x,N,Y,0,z);else
aG(x,N,Y,M-z|0,z);var
_=Y}var
aH=_}return q(o(g,f),aH,a+1|0);case
67:case
99:var
r=l(g,f);if(99===p)var
aB=ah(1,r);else{if(39===r)var
t=cH;else
if(92===r)var
t=cI;else{if(14<=r)var
D=0;else
switch(r){case
8:var
t=cJ,D=1;break;case
9:var
t=cK,D=1;break;case
10:var
t=cL,D=1;break;case
13:var
t=cM,D=1;break;default:var
D=0}if(!D)if(a0(r)){var
ap=B(1);ap.safeSet(0,r);var
t=ap}else{var
F=B(4);F.safeSet(0,92);F.safeSet(1,48+(r/aw|0)|0);F.safeSet(2,48+((r/10|0)%10|0)|0);F.safeSet(3,48+(r%10|0)|0);var
t=F}}var
aB=k(da,k(t,c$))}return q(o(g,f),aB,a+1|0);case
66:case
98:var
a6=a+1|0,a7=l(g,f)?cA:cB;return q(o(g,f),a7,a6);case
40:case
a$:var
R=l(g,f),az=n(bw(p),h,a+1|0);if(a$===p){var
O=aI(R.getLen()),au=function(a,b){V(O,b);return a+1|0};bx(R,function(a,b,c){if(a)aK(O,c3);else
V(O,37);return au(b,c)},au);var
a3=aJ(O);return q(o(g,f),a3,az)}var
aA=o(g,f),bp=br(by(R),aA);return aR(function(a){return C(bp,az)},aA,R,aS);case
33:j(e,A);return C(f,a+1|0);case
41:return q(f,c9,a+1|0);case
44:return q(f,c_,a+1|0);case
70:var
ad=l(g,f);if(0===c)var
aC=db;else{var
$=E(h,m,a,c);if(70===p)$.safeSet($.getLen()-1|0,bc);var
aC=$}var
ax=em(ad);if(3===ax)var
ae=ad<0?c6:c7;else
if(4<=ax)var
ae=c8;else{var
Q=aZ(aC,ad),P=0,a4=Q.getLen();for(;;){if(a4<=P)var
av=k(Q,c5);else{var
G=Q.safeGet(P)-46|0,bt=G<0||23<G?55===G?1:0:(G-1|0)<0||21<(G-1|0)?1:0;if(!bt){var
P=P+1|0;continue}var
av=Q}var
ae=av;break}}return q(o(g,f),ae,a+1|0);case
91:return W(h,a,p);case
97:var
aM=l(g,f),aN=bs(bA(g,f)),aO=l(0,aN),bl=a+1|0,bm=o(g,aN);if(aQ)aj(n(aM,0,aO));else
n(aM,A,aO);return C(bm,bl);case
at:return W(h,a,p);case
a5:var
aP=l(g,f),bn=a+1|0,bo=o(g,f);if(aQ)aj(j(aP,0));else
j(aP,A);return C(bo,bn)}return W(h,a,p)}},f=m+1|0,g=0;return bz(h,function(a,b){return ay(a,r,g,b)},f)}n(c,A,d);var
m=m+1|0;continue}}function
q(a,b,c){aj(b);return C(a,c)}return C(b,0)}var
q=aL(0);function
l(a,b){return aR(f,q,a,b)}var
m=by(g);if(m<0||6<m){var
p=function(f,b){if(m<=f){var
i=x(m,0),j=function(a,b){return h(i,(m-a|0)-1|0,b)},c=0,a=b;for(;;){if(a){var
d=a[2],e=a[1];if(d){j(c,e);var
c=c+1|0,a=d;continue}j(c,e)}return l(g,i)}}return function(a){return p(f+1|0,[0,a,b])}};return p(0,0)}switch(m){case
1:return function(a){var
b=x(1,0);h(b,0,a);return l(g,b)};case
2:return function(a,b){var
c=x(2,0);h(c,0,a);h(c,1,b);return l(g,c)};case
3:return function(a,b,c){var
d=x(3,0);h(d,0,a);h(d,1,b);h(d,2,c);return l(g,d)};case
4:return function(a,b,c,d){var
e=x(4,0);h(e,0,a);h(e,1,b);h(e,2,c);h(e,3,d);return l(g,e)};case
5:return function(a,b,c,d,e){var
f=x(5,0);h(f,0,a);h(f,1,b);h(f,2,c);h(f,3,d);h(f,4,e);return l(g,f)};case
6:return function(a,b,c,d,e,f){var
i=x(6,0);h(i,0,a);h(i,1,b);h(i,2,c);h(i,3,d);h(i,4,e);h(i,5,f);return l(g,i)};default:return l(g,[0])}}function
bC(d){function
e(a){return 0}function
b(a){return d}var
c=0;return function(a){return bB(c,b,cF,bm,bo,e,a)}}function
de(a){return aI(2*a.getLen()|0)}function
s(a){function
b(a){var
b=aJ(a);a[2]=0;return b}return bB(1,de,V,aK,function(a){return 0},b,a)}var
aM=[0,0];function
aO(a,b){var
c=a[b+1];if(eU(c)){if(an(c)===252)return j(s(df),c);if(an(c)===253){var
e=aZ(cD,c),d=0,g=e.getLen();for(;;){if(g<=d)return k(e,cC);var
f=e.safeGet(d),h=48<=f?58<=f?0:1:45===f?1:0;if(h){var
d=d+1|0;continue}return e}}return dg}return j(s(dh),c)}function
bD(a,b){if(a.length-1<=b)return di;var
c=bD(a,b+1|0),d=aO(a,b);return n(s(dj),d,c)}function
bE(a){var
b=aM[1];for(;;){if(b){var
r=b[2],t=b[1];try{var
u=j(t,a),e=u}catch(f){var
e=0}if(e)return e[1];var
b=r;continue}if(a===dp)return dq;if(a===bF)return dr;if(a[1]===bG){var
f=a[2],i=f[3],v=f[2],w=f[1];return ao(s(aN),w,v,i,i+5|0,ds)}if(a[1]===N){var
g=a[2],l=g[3],x=g[2],y=g[1];return ao(s(aN),y,x,l,l+6|0,dt)}if(a[1]===bH){var
h=a[2],m=h[3],z=h[2],A=h[1];return ao(s(aN),A,z,m,m+6|0,du)}if(0===an(a)){var
d=a.length-1,B=a[0+1][0+1];if(d<0||2<d)var
o=bD(a,2),p=aO(a,1),c=n(s(dk),p,o);else
switch(d){case
1:var
c=dm;break;case
2:var
q=aO(a,1),c=j(s(dn),q);break;default:var
c=dl}return k(B,c)}return a[0+1]}}function
bI(a){var
h=en(et(0));if(h){var
d=h[1],f=d.length-1-1|0,q=0;if(!(f<0)){var
c=q;for(;;){if(eT(i(d,c),dB)){var
b=i(d,c),k=0===b[0]?b[1]:b[1],e=k?0===c?dv:dy:0===c?dz:dA;if(0===b[0])var
l=b[5],m=b[4],o=b[3],p=b[2],g=ao(s(dw),e,p,o,m,l);else
var
g=j(s(dx),e);n(bC(a),dC,g)}var
r=c+1|0;if(f!==c){var
c=r;continue}break}}return 0}return j(bC(a),dD)}function
bJ(a){aM[1]=[0,a,aM[1]];return 0}32===aH;var
m=[0,dN.slice(),0];function
aR(a){if(!(as<a))if(0<a)for(;;){m[2]=(m[2]+1|0)%55|0;var
c=i(m[1],m[2]),b=(i(m[1],(m[2]+24|0)%55|0)+(c^c>>>25&31)|0)&as;h(m[1],m[2],b);var
d=b5(b,a);if(((as-a|0)+1|0)<(b-d|0))continue;return d}return M(dM)}w([d,dP,0]);function
bP(a){var
b=[];e$(b,[0,b,b]);return b}var
aS=w([d,dQ,0]),F=[0,0];function
aT(a){var
c=a[1];if(3===c[0]){var
d=c[1],b=aT(d);if(b!==d)a[1]=[3,b];return b}return a}function
Z(a){return aT(a)}var
bQ=[0,function(a){bn(dR);bn(bE(a));b4(T,10);bI(T);bo(T);aF(0);return e5(2)}];function
bR(a,b){try{var
c=j(a,b)}catch(f){f=a1(f);return j(bQ[1],f)}return c}function
a2(a,b,c,d){var
f=c,e=d;for(;;)if(typeof
f===u)return a<50?y(1+a,b,e):z(y,[0,b,e]);else
switch(f[0]){case
1:j(f[1],b);return a<50?y(1+a,b,e):z(y,[0,b,e]);case
2:var
h=[0,f[2],e],f=f[1],e=h;continue;default:var
g=f[1][1];if(g){j(g[1],b);return a<50?y(1+a,b,e):z(y,[0,b,e])}else
return a<50?y(1+a,b,e):z(y,[0,b,e])}}function
y(a,b,c){return c?a<50?a2(1+a,b,c[1],c[2]):z(a2,[0,b,c[1],c[2]]):0}function
dS(b,c,d){return ap(a2(0,b,c,d))}function
fc(b,c){return ap(y(0,b,c))}function
a3(a,b,c){var
e=b,d=c;for(;;)if(typeof
e===u)return a<50?H(1+a,d):z(H,[0,d]);else
switch(e[0]){case
1:var
f=e[1];if(f[4]){f[4]=0;f[1][2]=f[2];f[2][1]=f[1]}return a<50?H(1+a,d):z(H,[0,d]);case
2:var
h=[0,e[2],d],e=e[1],d=h;continue;default:var
g=e[2];F[1]=e[1];bR(g,0);return a<50?H(1+a,d):z(H,[0,d])}}function
H(a,b){return b?a<50?a3(1+a,b[1],b[2]):z(a3,[0,b[1],b[2]]):0}function
dT(b,c){return ap(a3(0,b,c))}function
fd(b){return ap(H(0,b))}function
al(a,b){var
c=1===b[0]?b[1]===aS?(dT(a[4],0),1):0:0;return dS(b,a[2],0)}var
aU=[0,0],O=[0,0,0];function
bS(a,b){var
h=[0,b],i=aT(a),e=i[1];switch(e[0]){case
1:if(e[1]===aS)return 0;break;case
2:var
k=e[1];i[1]=h;var
g=F[1],j=aU[1]?1:(aU[1]=1,0);al(k,h);if(j){F[1]=g;return 0}for(;;){if(0===O[1]){aU[1]=0;F[1]=g;return 0}if(0===O[1])throw cR;O[1]=O[1]-1|0;var
c=O[2],d=c[2];if(d===c)O[2]=0;else
c[2]=d[2];var
f=d[1];al(f[1],f[2]);continue}}return M(dU)}function
bT(a,b){return typeof
a===u?b:typeof
b===u?a:[2,a,b]}function
aV(a){if(typeof
a!==u)switch(a[0]){case
2:var
b=a[1],c=aV(a[2]);return bT(aV(b),c);case
1:break;default:if(!a[1][1])return 0}return a}var
dZ=[0,function(a){return 0}],t=bP(0),d0=[0,0],G=p;function
d1(a){var
e=1-(t[2]===t?1:0);if(e){var
b=bP(0);b[1][2]=t[2];t[2][1]=b[1];b[1]=t[1];t[1][2]=b;t[1]=t;t[2]=t;d0[1]=0;var
c=b[2];for(;;){var
d=c!==b?1:0;if(d){if(c[4])bS(c[3],0);var
c=c[2];continue}return d}}return e}var
bU=w([d,d6,0]),aW=[0,bU,{}],d2=null,d3=undefined,d4=false,d5=Array,dO=an(aW)===d?aW:aW[0+1];aY(d7,dO);bJ(function(a){return a[1]===bU?[0,new
L(a[2].toString())]:0});bJ(function(a){return a
instanceof
d5?0:[0,new
L(a.toString())]});var
bV="2d",d9=w([d,d8,0]);G.HTMLElement===d3;var
d_=eI(0),d$=a7;dZ[1]=function(a){return 1===a?(G.setTimeout(a4(d1),0),0):0};function
bW(a){return d_.log(a.toString())}bQ[1]=function(a){bW(ea);bW(bE(a));return bI(T)};var
f=Math.floor(20*1.73205080756887719/2+bg),bO=e8(0),_=12,bL=0===bO.length-1?[0,0]:bO,aP=bL.length-1,dE=0;if(!0){var
Y=dE;for(;;){h(m[1],Y,Y);var
dL=Y+1|0;if(54!==Y){var
Y=dL;continue}break}}var
aQ=[0,dF],dG=0,dH=55,dI=eu(55,aP)?dH:aP,bM=54+dI|0;if(!(bM<0)){var
X=dG;for(;;){var
bN=X%55|0,dJ=aQ[1],bK=k(dJ,ag(i(bL,b5(X,aP))));aQ[1]=eL(bK,0,bK.getLen());var
ak=aQ[1];h(m[1],bN,(i(m[1],bN)^(((ak.safeGet(0)+(ak.safeGet(1)<<8)|0)+(ak.safeGet(2)<<16)|0)+(ak.safeGet(3)<<24)|0))&as);var
dK=X+1|0;if(bM!==X){var
X=dK;continue}break}}m[2]=0;function
P(a,b,c,d){var
f=b<0?1:0;if(f)var
e=f;else{var
g=c<0?1:0;if(g)var
e=g;else{var
h=d<0?1:0;if(h)var
e=h;else{var
j=b<12?1:0;if(j){var
k=c<12?1:0;if(k)var
l=d<12?1:0,e=l?i(i(i(a,b),c),d):l;else
var
e=k}else
var
e=j}}}return e}var
eb="#a8a8f6",ec="#d9d9d9",ed="#767676";function
aX(a,b,c,d,e){a.save();a.translate((((b-d|0)+12|0)-1|0)*f,(11-c|0)*20+(b+d|0)*20/2);j(e,a);return a.restore()}function
ee(a){a.fillStyle=eb;a.beginPath();a.moveTo(f,0);a.lineTo(2*f,10);a.lineTo(f,20);a.lineTo(0,10);return a.fill()}function
ef(a){a.beginPath();a.moveTo(0,10);a.lineTo(f,0);a.lineTo(2*f,10);return a.stroke()}function
eg(a){a.fillStyle=ed;a.beginPath();a.moveTo(f,20);a.lineTo(f,40);a.lineTo(2*f,30);a.lineTo(2*f,10);return a.fill()}function
eh(a){a.beginPath();a.moveTo(f,40);a.lineTo(f,20);a.lineTo(2*f,10);return a.stroke()}function
ei(a){a.fillStyle=ec;a.beginPath();a.moveTo(f,20);a.lineTo(f,40);a.lineTo(0,30);a.lineTo(0,10);return a.fill()}function
ej(a){a.beginPath();a.moveTo(f,20);a.lineTo(0,10);a.lineTo(0,30);return a.stroke()}function
bX(a,b,c){var
n=c[3],o=c[2],p=c[1],q=0,r=11;if(!(11<0)){var
h=q;for(;;){var
m=[0,11],E=0,F=11;if(!(11<0)){var
j=E;c:for(;;){for(;;){if(0<=m[1])if(!i(i(i(b,h),m[1]),j)){m[1]+=-1;continue}aX(a,h,m[1],j,p);var
H=j+1|0;if(F!==j){var
j=H;continue c}break}break}}var
G=h+1|0;if(r!==h){var
h=G;continue}break}}var
s=0,t=11;if(!(11<0)){var
f=s;for(;;){var
l=[0,11],A=0,B=11;if(!(11<0)){var
g=A;c:for(;;){for(;;){if(0<=l[1])if(!i(i(i(b,l[1]),f),g)){l[1]+=-1;continue}aX(a,l[1],f,g,o);var
D=g+1|0;if(B!==g){var
g=D;continue c}break}break}}var
C=f+1|0;if(t!==f){var
f=C;continue}break}}var
u=0,v=11;if(!(11<0)){var
d=u;for(;;){var
k=[0,11],w=0,x=11;if(!(11<0)){var
e=w;c:for(;;){for(;;){if(0<=k[1])if(!i(i(i(b,d),e),k[1])){k[1]+=-1;continue}aX(a,d,e,k[1],n);var
z=e+1|0;if(x!==e){var
e=z;continue c}break}break}}var
y=d+1|0;if(v!==d){var
d=y;continue}break}}return 0}function
bY(a){var
b=G.document.createElement("canvas");if(1-(b.getContext==d2?1:0)){b.width=(24*(f|0)|0)+1|0;b.height=(24*20|0)+1|0;return b}throw d9}function
bZ(a,b,c){var
d=b.getContext(bV);d.setTransform(1,0,0,1,0,0);d.clearRect(0,0,b.width,b.height);d.setTransform(1,0,0,1,bg,bg);d.globalCompositeOperation="lighter";bX(d,c,[0,ee,eg,ei]);d.globalCompositeOperation="source-over";bX(d,c,[0,ef,eh,ej]);d.beginPath();d.moveTo(0,18*20);d.lineTo(12*f,24*20);d.lineTo(24*f,18*20);d.lineTo(24*f,6*20);d.stroke();return a.drawImage(b,0,0)}function
b0(s,b,c){function
t(a){var
l=[0,0],n=0;if(!0){var
m=n;for(;;){var
d=aR(_),e=aR(_),f=aR(_);if(i(i(i(c,d),e),f)){if(P(c,d+1|0,e,f))var
j=0;else
if(P(c,d,e+1|0,f))var
j=0;else
if(P(c,d,e,f+1|0))var
j=0;else{h(i(i(c,d),e),f,0);var
g=1,j=1}if(!j)var
g=0}else{if(P(c,d-1|0,e,f))if(P(c,d,e-1|0,f))if(P(c,d,e,f-1|0)){h(i(i(c,d),e),f,1);var
g=1,k=1}else
var
k=0;else
var
k=0;else
var
k=0;if(!k)var
g=0}var
o=g||l[1];l[1]=o;var
p=m+1|0;if(99!==m){var
m=p;continue}break}}if(l[1])bZ(s,b,c);return b0(s,b,c)}var
d=[0,[2,[0,1,0,0,0]]],p=[0,0],z=0.2;function
q(a,b){var
c=a7<a?[0,d$,a-a7]:[0,a,0],e=c[2],f=c[1],g=e==0?function(a){return bS(d,a)}:function(a){return q(e,a)};p[1]=[0,G.setTimeout(a4(g),f*cc)];return 0}q(z,0);function
r(a){var
b=p[1];return b?G.clearTimeout(b[1]):0}var
e=Z(d)[1];switch(e[0]){case
1:var
v=e[1]===aS?(bR(r,0),1):0;break;case
2:var
k=e[1],l=[0,F[1],r],m=k[4],x=typeof
m===u?l:[2,l,m];k[4]=x;var
v=1;break;default:var
v=0}var
n=Z(d),a=n[1];switch(a[0]){case
1:return[0,a];case
2:var
o=a[1],j=[0,[2,[0,[0,[0,n]],0,0,0]]],y=F[1],g=[1,function(a){switch(a[0]){case
0:var
r=a[1];F[1]=y;try{var
s=t(r),o=s}catch(f){f=a1(f);var
o=[0,[1,f]]}var
c=Z(j),f=Z(o),i=c[1];if(2===i[0]){var
b=i[1];if(c===f)return 0;var
d=f[1];if(2===d[0]){var
e=d[1];f[1]=[3,c];b[1]=e[1];var
k=bT(b[2],e[2]),l=b[3]+e[3]|0;if(42<l){b[3]=0;b[2]=aV(k)}else{b[3]=l;b[2]=k}var
g=e[4],h=b[4],p=typeof
h===u?g:typeof
g===u?h:[2,h,g];b[4]=p;return 0}c[1]=d;return al(b,d)}throw[0,N,dV];case
1:var
m=Z(j),n=m[1];if(2===n[0]){var
q=n[1];m[1]=a;return al(q,a)}throw[0,N,dW];default:throw[0,N,dX]}}],f=o[2],w=typeof
f===u?g:[2,g,f];o[2]=w;return j;case
3:throw[0,N,dY];default:return t(a[1])}}function
b1(a){var
d=bY(0),e=bY(0);G.document.body.appendChild(d);var
b=d.getContext(bV);b.globalCompositeOperation="copy";var
f=1,c=bp(_,function(a){return bp(_,function(a){return x(12,f)})});bZ(b,e,c);b0(b,e,c);return d4}G.onload=a4(function(a){if(a){var
d=b1(a);if(!(d|0))a.preventDefault();return d}var
c=event,b=b1(c);if(!(b|0))c.returnValue=b;return b});aF(0);return}(this));
