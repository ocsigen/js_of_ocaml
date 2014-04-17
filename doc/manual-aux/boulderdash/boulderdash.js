// This program was compiled from OCaml by js_of_ocaml 2.0+git-883a1ec
(function(b){"use strict";var
b5=125,b9=123,dO=254,ah=255,dN=108,bf='"',L=16777215,b8="=",dE='Content-Disposition: form-data; name="',dF=250,dM="sprites/guy.png",dy="jsError",bo="POST",b4=2147483,dr=-550809787,aO=115,cc=102,dL="&",b7=120,b3="--",b6=117,bn=126925477,d="",bc=781515420,dx="sprites/boulder.png",bk=100,H="0",f=248,cb=103,dK="fd ",dq=936573133,dD=1e3,_="src/core/lwt.ml",dw="x",as=".",bj=65535,aL="+",aK="g",b2="f",aj=105,dv="%d",du=443,dJ=-88,aN=110,dC=785140586,dp="sprites/end.png",bb="?",be="'",bd="int_of_string",ca=-32,dt=982028505,b$=111,G=" ",aM="e",dI="1",dn=0.001,dB="lastIndex",bm=891486873,dm=":",ai="-",ar=-48,dA="nan",b1=116,b_="eos",bi="\r\n",dz="%.12g",cd=0.05,ce=" : file already exists",ds=-83,Z="/",bh=114,bg="#",dH=101,bl="index out of bounds",z="number",dG="select";function
d4(a,b){throw[0,a,b]}function
ci(a){if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
c=b.console;c&&c.error&&c.error(a)}var
n=[0];function
aQ(a,b){if(!a)return d;if(a&1)return aQ(a-1,b)+b;var
c=aQ(a>>1,b);return c+c}function
r(a){if(a!=null){this.bytes=this.fullBytes=a;this.last=this.len=a.length}}function
d5(){d4(n[4],new
r(bl))}r.prototype={string:null,bytes:null,fullBytes:null,array:null,len:null,last:0,toJsString:function(){var
a=this.getFullBytes();try{return this.string=decodeURIComponent(escape(a))}catch(f){ci('MlString.toJsString: wrong encoding for "%s" ',a);return a}},toBytes:function(){if(this.string!=null)try{var
a=unescape(encodeURIComponent(this.string))}catch(f){ci('MlString.toBytes: wrong encoding for "%s" ',this.string);var
a=this.string}else{var
a=d,c=this.array,e=c.length;for(var
b=0;b<e;b++)a+=String.fromCharCode(c[b])}this.bytes=this.fullBytes=a;this.last=this.len=a.length;return a},getBytes:function(){var
a=this.bytes;if(a==null)a=this.toBytes();return a},getFullBytes:function(){var
a=this.fullBytes;if(a!==null)return a;a=this.bytes;if(a==null)a=this.toBytes();if(this.last<this.len){this.bytes=a+=aQ(this.len-this.last,"\0");this.last=this.len}this.fullBytes=a;return a},toArray:function(){var
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
b=this.bytes;if(b==null)b=this.toBytes();return a<this.last?b.charCodeAt(a):0},safeGet:function(a){if(this.len==null)this.toBytes();if(a<0||a>=this.len)d5();return this.get(a)},set:function(a,b){var
c=this.array;if(!c){if(this.last==a){this.bytes+=String.fromCharCode(b&ah);this.last++;return 0}c=this.toArray()}else
if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;c[a]=b&ah;return 0},safeSet:function(a,b){if(this.len==null)this.toBytes();if(a<0||a>=this.len)d5();this.set(a,b)},fill:function(a,b,c){if(a>=this.last&&this.last&&c==0)return;var
d=this.array;if(!d)d=this.toArray();else
if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;var
f=a+b;for(var
e=a;e<f;e++)d[e]=c},compare:function(a){if(this.string!=null&&a.string!=null){if(this.string<a.string)return-1;if(this.string>a.string)return 1;return 0}var
b=this.getFullBytes(),c=a.getFullBytes();if(b<c)return-1;if(b>c)return 1;return 0},equal:function(a){if(this.string!=null&&a.string!=null)return this.string==a.string;return this.getFullBytes()==a.getFullBytes()},lessThan:function(a){if(this.string!=null&&a.string!=null)return this.string<a.string;return this.getFullBytes()<a.getFullBytes()},lessEqual:function(a){if(this.string!=null&&a.string!=null)return this.string<=a.string;return this.getFullBytes()<=a.getFullBytes()}};function
I(a){this.string=a}I.prototype=new
r();function
ch(a,b){d4(a,new
I(b))}function
au(a){ch(n[4],a)}function
dR(){au(bl)}function
hM(a,b){if(b<0||b>=a.length-1)dR();return a[b+1]}function
hN(a,b,c){if(b<0||b>=a.length-1)dR();a[b+1]=c;return 0}function
dS(a,b,c,d,e){if(e===0)return;if(d===c.last&&c.bytes!=null){var
f=a.bytes;if(f==null)f=a.toBytes();if(b>0||a.last>e)f=f.slice(b,b+e);c.bytes+=f;c.last+=f.length;return}var
g=c.array;if(!g)g=c.toArray();else
c.bytes=c.string=null;a.blitToArray(b,g,d,e)}function
$(c,b){if(c.fun)return $(c.fun,b);var
a=c.length,d=a-b.length;if(d==0)return c.apply(null,b);else
if(d<0)return $(c.apply(null,b.slice(0,a)),b.slice(a));else
return function(a){return $(c,b.concat([a]))}}function
hO(a){if(isFinite(a)){if(Math.abs(a)>=2.22507385850720138e-308)return 0;if(a!=0)return 1;return 2}return isNaN(a)?4:3}function
hP(){return 0}function
dP(a){this.bytes=d;this.len=a}dP.prototype=new
r();function
dU(a){if(a<0)au("String.create");return new
dP(a)}function
hY(a,b){var
c=a[3]<<16,d=b[3]<<16;if(c>d)return 1;if(c<d)return-1;if(a[2]>b[2])return 1;if(a[2]<b[2])return-1;if(a[1]>b[1])return 1;if(a[1]<b[1])return-1;return 0}function
h8(a,b){if(a<b)return-1;if(a==b)return 0;return 1}function
dT(a,b,c){var
e=[];for(;;){if(!(c&&a===b))if(a
instanceof
r)if(b
instanceof
r){if(a!==b){var
d=a.compare(b);if(d!=0)return d}}else
return 1;else
if(a
instanceof
Array&&a[0]===(a[0]|0)){var
g=a[0];if(g===dO)g=0;if(g===dF){a=a[1];continue}else
if(b
instanceof
Array&&b[0]===(b[0]|0)){var
h=b[0];if(h===dO)h=0;if(h===dF){b=b[1];continue}else
if(g!=h)return g<h?-1:1;else
switch(g){case
f:var
d=h8(a[2],b[2]);if(d!=0)return d;break;case
251:au("equal: abstract value");case
ah:var
d=hY(a,b);if(d!=0)return d;break;default:if(a.length!=b.length)return a.length<b.length?-1:1;if(a.length>1)e.push(a,b,1)}}else
return 1}else
if(b
instanceof
r||b
instanceof
Array&&b[0]===(b[0]|0))return-1;else
if(typeof
a!=z&&a&&a.compare)return a.compare(b,c);else{if(a<b)return-1;if(a>b)return 1;if(a!=b){if(!c)return NaN;if(a==a)return 1;if(b==b)return-1}}if(e.length==0)return 0;var
i=e.pop();b=e.pop();a=e.pop();if(i+1<a.length)e.push(a,b,i+1);a=a[i];b=b[i]}}function
hR(a,b){return+(dT(a,b,false)==0)}function
hS(a,b,c,d){a.fill(b,c,d)}function
cg(a){a=a.toString();var
e=a.length;if(e>31)au("format_int: format too long");var
b={justify:aL,signstyle:ai,filler:G,alternate:false,base:0,signedconv:false,width:0,uppercase:false,sign:1,prec:-1,conv:b2};for(var
d=0;d<e;d++){var
c=a.charAt(d);switch(c){case
ai:b.justify=ai;break;case
aL:case
G:b.signstyle=c;break;case
H:b.filler=H;break;case
bg:b.alternate=true;break;case
dI:case"2":case"3":case"4":case"5":case"6":case"7":case"8":case"9":b.width=0;while(c=a.charCodeAt(d)-48,c>=0&&c<=9){b.width=b.width*10+c;d++}d--;break;case
as:b.prec=0;d++;while(c=a.charCodeAt(d)-48,c>=0&&c<=9){b.prec=b.prec*10+c;d++}d--;case"d":case"i":b.signedconv=true;case"u":b.base=10;break;case
dw:b.base=16;break;case"X":b.base=16;b.uppercase=true;break;case"o":b.base=8;break;case
aM:case
b2:case
aK:b.signedconv=true;b.conv=c;break;case"E":case"F":case"G":b.signedconv=true;b.uppercase=true;b.conv=c.toLowerCase();break}}return b}function
cf(a,b){if(a.uppercase)b=b.toUpperCase();var
f=b.length;if(a.signedconv&&(a.sign<0||a.signstyle!=ai))f++;if(a.alternate){if(a.base==8)f+=1;if(a.base==16)f+=2}var
c=d;if(a.justify==aL&&a.filler==G)for(var
e=f;e<a.width;e++)c+=G;if(a.signedconv)if(a.sign<0)c+=ai;else
if(a.signstyle!=ai)c+=a.signstyle;if(a.alternate&&a.base==8)c+=H;if(a.alternate&&a.base==16)c+="0x";if(a.justify==aL&&a.filler==H)for(var
e=f;e<a.width;e++)c+=H;c+=b;if(a.justify==ai)for(var
e=f;e<a.width;e++)c+=G;return new
I(c)}function
hT(a,b){var
c,f=cg(a),e=f.prec<0?6:f.prec;if(b<0){f.sign=-1;b=-b}if(isNaN(b)){c=dA;f.filler=G}else
if(!isFinite(b)){c="inf";f.filler=G}else
switch(f.conv){case
aM:var
c=b.toExponential(e),d=c.length;if(c.charAt(d-3)==aM)c=c.slice(0,d-1)+H+c.slice(d-1);break;case
b2:c=b.toFixed(e);break;case
aK:e=e?e:1;c=b.toExponential(e-1);var
i=c.indexOf(aM),h=+c.slice(i+1);if(h<-4||b.toFixed(0).length>e){var
d=i-1;while(c.charAt(d)==H)d--;if(c.charAt(d)==as)d--;c=c.slice(0,d+1)+c.slice(i);d=c.length;if(c.charAt(d-3)==aM)c=c.slice(0,d-1)+H+c.slice(d-1);break}else{var
g=e;if(h<0){g-=h+1;c=b.toFixed(g)}else
while(c=b.toFixed(g),c.length>e+1)g--;if(g){var
d=c.length-1;while(c.charAt(d)==H)d--;if(c.charAt(d)==as)d--;c=c.slice(0,d+1)}}break}return cf(f,c)}function
hU(a,b){if(a.toString()==dv)return new
I(d+b);var
c=cg(a);if(b<0)if(c.signedconv){c.sign=-1;b=-b}else
b>>>=0;var
e=b.toString(c.base);if(c.prec>=0){c.filler=G;var
f=c.prec-e.length;if(f>0)e=aQ(f,H)+e}return cf(c,e)}function
it(a){throw a}function
iu(){it(n[7])}function
dQ(a){var
b=a.length;this.array=a;this.len=this.last=b}dQ.prototype=new
r();function
ak(){this.content={}}ak.prototype={exists:function(a){return this.content[a]?1:0},mk:function(a,b){this.content[a]=b},get:function(a){return this.content[a]},list:function(){var
a=[];for(var
b
in
this.content)a.push(b);return a},remove:function(a){delete
this.content[a]}};var
bt=new
ak();bt.mk(d,new
ak());function
J(a){ch(n[2],a)}function
d3(a){a=a
instanceof
r?a.toString():a;J(a+": No such file or directory")}function
bq(a){var
b=bt;for(var
c=0;c<a.length;c++){if(!(b.exists&&b.exists(a[c])))d3(a.orig);b=b.get(a[c])}return b}var
hQ=Z;function
aP(a){a=a
instanceof
r?a.toString():a;if(a.charCodeAt(0)!=47)a=hQ+a;var
e=a.split(Z),b=[];for(var
c=0;c<e.length;c++)switch(e[c]){case"..":if(b.length>1)b.pop();break;case
as:case
d:if(b.length==0)b.push(d);break;default:b.push(e[c]);break}b.orig=a;return b}function
at(a){this.data=a}at.prototype={content:function(){return this.data},truncate:function(){this.data.length=0}};function
hV(a){var
c=aP(a),b=bq(c);if(b
instanceof
at)return new
dQ(b.content());iu()}function
dV(a,b){var
e=aP(a),c=bt;for(var
f=0;f<e.length-1;f++){var
d=e[f];if(!c.exists(d))c.mk(d,new
ak());c=c.get(d);if(!(c
instanceof
ak))J(e.orig+ce)}var
d=e[e.length-1];if(c.exists(d))J(e.orig+ce);if(b
instanceof
ak)c.mk(d,b);else
if(b
instanceof
at)c.mk(d,b);else
if(b
instanceof
r)c.mk(d,new
at(b.getArray()));else
if(b
instanceof
Array)c.mk(d,new
at(b));else
if(b.toString)c.mk(d,new
at(new
r(b.toString()).getArray()));else
au("caml_fs_register")}function
hW(){return 0}function
h1(a){return(a[3]|a[2]|a[1])==0}function
h4(a){return[ah,a&L,a>>24&L,a>>31&bj]}function
h5(a,b){var
c=a[1]-b[1],d=a[2]-b[2]+(c>>24),e=a[3]-b[3]+(d>>24);return[ah,c&L,d&L,e&bj]}function
dX(a,b){if(a[3]>b[3])return 1;if(a[3]<b[3])return-1;if(a[2]>b[2])return 1;if(a[2]<b[2])return-1;if(a[1]>b[1])return 1;if(a[1]<b[1])return-1;return 0}function
dW(a){a[3]=a[3]<<1|a[2]>>23;a[2]=(a[2]<<1|a[1]>>23)&L;a[1]=a[1]<<1&L}function
h2(a){a[1]=(a[1]>>>1|a[2]<<23)&L;a[2]=(a[2]>>>1|a[3]<<23)&L;a[3]=a[3]>>>1}function
h7(a,b){var
e=0,d=a.slice(),c=b.slice(),f=[ah,0,0,0];while(dX(d,c)>0){e++;dW(c)}while(e>=0){e--;dW(f);if(dX(d,c)>=0){f[1]++;d=h5(d,c)}h2(c)}return[0,f,d]}function
h6(a){return a[1]|a[2]<<24}function
h0(a){return a[3]<<16<0}function
h3(a){var
b=-a[1],c=-a[2]+(b>>24),d=-a[3]+(c>>24);return[ah,b&L,c&L,d&bj]}function
hZ(a,b){var
c=cg(a);if(c.signedconv&&h0(b)){c.sign=-1;b=h3(b)}var
e=d,i=h4(c.base),h="0123456789abcdef";do{var
g=h7(b,i);b=g[1];e=h.charAt(h6(g[2]))+e}while(!h1(b));if(c.prec>=0){c.filler=G;var
f=c.prec-e.length;if(f>0)e=aQ(f,H)+e}return cf(c,e)}function
is(a){var
b=0,c=10,d=a.get(0)==45?(b++,-1):1;if(a.get(b)==48)switch(a.get(b+1)){case
b7:case
88:c=16;b+=2;break;case
b$:case
79:c=8;b+=2;break;case
98:case
66:c=2;b+=2;break}return[b,d,c]}function
d2(a){if(a>=48&&a<=57)return a-48;if(a>=65&&a<=90)return a-55;if(a>=97&&a<=122)return a-87;return-1}function
bp(a){ch(n[3],a)}function
h9(a){var
g=is(a),f=g[0],h=g[1],d=g[2],i=-1>>>0,e=a.get(f),c=d2(e);if(c<0||c>=d)bp(bd);var
b=c;for(;;){f++;e=a.get(f);if(e==95)continue;c=d2(e);if(c<0||c>=d)break;b=d*b+c;if(b>i)bp(bd)}if(f!=a.getLen())bp(bd);b=h*b;if(d==10&&(b|0)!=b)bp(bd);return b|0}function
h_(a){return+(a>31&&a<127)}function
h$(a){return a.getFullBytes()}function
ia(){var
c=b.console?b.console:{},d=["log","debug","info","warn","error","assert","dir","dirxml","trace","group","groupCollapsed","groupEnd","time","timeEnd"];function
e(){}for(var
a=0;a<d.length;a++)if(!c[d[a]])c[d[a]]=e;return c}var
br={amp:/&/g,lt:/</g,quot:/\"/g,all:/[&<\"]/};function
ib(a){if(!br.all.test(a))return a;return a.replace(br.amp,"&amp;").replace(br.lt,"&lt;").replace(br.quot,"&quot;")}function
ic(){var
a=b.navigator?b.navigator.userAgent:d;return a.indexOf("MSIE")!=-1&&a.indexOf("Opera")!=0}function
id(a){return new
r(a)}function
ie(a){var
c=Array.prototype.slice;return function(){var
b=arguments.length>0?c.call(arguments):[undefined];return $(a,b)}}function
ig(a,b){var
d=[0];for(var
c=1;c<=a;c++)d[c]=b;return d}function
dY(a){if(!a.opened)J("Cannot flush a closed channel");if(a.buffer==d)return 0;if(a.output)switch(a.output.length){case
2:a.output(a,a.buffer);break;default:a.output(a.buffer)}a.buffer=d}function
iE(a){var
c=aP(a),b=bq(c);return b
instanceof
ak?1:0}function
iD(a){var
b=bt,d=aP(a),e;for(var
c=0;c<d.length;c++){if(b.auto)e=b.auto;if(!(b.exists&&b.exists(d[c])))return e?e(d.join(Z)):0;b=b.get(d[c])}return 1}function
aR(a,b,c){if(n.fds===undefined)n.fds=new
Array();c=c?c:{};var
d={};d.array=b;d.offset=c.append?d.array.length:0;d.flags=c;n.fds[a]=d;n.fd_last_idx=a;return a}function
iN(a,b,c){var
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
f=a.toString(),h=aP(a);if(d.rdonly&&d.wronly)J(f+" : flags Open_rdonly and Open_wronly are not compatible");if(d.text&&d.binary)J(f+" : flags Open_text and Open_binary are not compatible");if(iD(a)){if(iE(a))J(f+" : is a directory");if(d.create&&d.excl)J(f+ce);var
g=n.fd_last_idx?n.fd_last_idx:0,e=bq(h);if(d.truncate)e.truncate();return aR(g+1,e.content(),d)}else
if(d.create){var
g=n.fd_last_idx?n.fd_last_idx:0;dV(a,[]);var
e=bq(h);return aR(g+1,e.content(),d)}else
d3(a)}aR(0,[]);aR(1,[]);aR(2,[]);function
ih(a){var
b=n.fds[a];if(b.flags.wronly)J(dK+a+" is writeonly");return{data:b,fd:a,opened:true}}function
iK(a){if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
c=b.console;c&&c.log&&c.log(a)}var
bs=new
Array();function
iy(a,b){var
e=new
r(b),d=e.getLen();for(var
c=0;c<d;c++)a.data.array[a.data.offset+c]=e.get(c);a.data.offset+=d;return 0}function
ii(a){var
b;switch(a){case
1:b=iK;break;case
2:b=ci;break;default:b=iy}var
e=n.fds[a];if(e.flags.rdonly)J(dK+a+" is readonly");var
c={data:e,fd:a,opened:true,buffer:d,output:b};bs[c.fd]=c;return c}function
ij(){var
a=0;for(var
b
in
bs)if(bs[b].opened)a=[0,bs[b],a];return a}function
dZ(a,b,c,d){if(!a.opened)J("Cannot output to a closed channel");var
f;if(c==0&&b.getLen()==d)f=b;else{f=dU(d);dS(b,c,f,0,d)}var
e=f.toString(),g=e.lastIndexOf("\n");if(g<0)a.buffer+=e;else{a.buffer+=e.substr(0,g+1);dY(a);a.buffer+=e.substr(g+1)}}function
d1(a){return new
r(a)}function
ik(a,b){var
c=d1(String.fromCharCode(b));dZ(a,c,0,1)}if(!Math.imul)Math.imul=function(a,b){return((a>>16)*b<<16)+(a&bj)*b|0};var
il=Math.imul;function
io(a,b){return+(dT(a,b,false)!=0)}function
ip(a){return+(a
instanceof
Array)}function
iq(a){return a
instanceof
Array?a[0]:dD}function
iv(a,b){n[a+1]=b}var
d0={};function
iw(a,b){d0[a.toString()]=b;return 0}var
ir=0;function
ix(a){a[2]=ir++;return a}function
iz(a,b){var
c=a.fullBytes,d=b.fullBytes;if(c!=null&&d!=null)return c==d?1:0;return a.getFullBytes()==b.getFullBytes()?1:0}function
iA(a,b){return 1-iz(a,b)}function
iB(){return 32}function
iC(a){if(b.quit)b.quit(a);au("Function 'exit' not implemented")}var
hX=new
Date()*dn;function
iF(){return new
Date()*dn-hX}function
iG(a){var
b=1;while(a&&a.joo_tramp){a=a.joo_tramp.apply(null,a.joo_args);b++}return a}function
iH(a,b){return{joo_tramp:a,joo_args:b}}function
iI(a,b){if(typeof
b==="function"){a.fun=b;return 0}if(b.fun){a.fun=b.fun;return 0}var
c=b.length;while(c--)a[c]=b[c];return 0}function
im(a){return d0[a]}function
iJ(a){if(a
instanceof
Array)return a;if(b.RangeError&&a
instanceof
b.RangeError&&a.message&&a.message.match(/maximum call stack/i))return[0,n[9]];if(b.InternalError&&a
instanceof
b.InternalError&&a.message&&a.message.match(/too much recursion/i))return[0,n[9]];if(a
instanceof
b.Error)return[0,im(dy),a];return[0,n[3],new
I(String(a))]}var
h=hM,k=hN,X=dS,S=dU,dk=hR,bX=hT,a8=hU,aq=dV,di=h9,bY=h_,q=h$,dl=ib,Y=id,ba=ie,T=ig,df=dY,de=ii,dg=ik,dh=il,c=d1,a9=iq,R=iv,bW=iw,a=ix,C=iA,aJ=iF,a$=iG,V=iH,dj=iI,y=iJ;function
j(a,b){return a.length==1?a(b):$(a,[b])}function
o(a,b,c){return a.length==2?a(b,c):$(a,[b,c])}function
u(a,b,c,d){return a.length==3?a(b,c,d):$(a,[b,c,d])}function
a_(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):$(a,[b,c,d,e,f])}aq("/maps.txt",'"maps/level0.map"\t"Level without boulders"\n"maps/level1.map"\t"Simple falls"\n"maps/level2.map"\t"More falls"\n"maps/level3.map"\t"Real (yet little) puzzle"\n"maps/level4.map"\t"Bigger puzzle (copie de BeeDeeDash)"\n');aq("/maps/level0.map","###################\n#S........+.......#\n#+....+.........+##\n#...+........+.#.E#\n###################\n");aq("/maps/level1.map","##################\n#S  X #          #\n#   + #.X..XX....#\n#   X #.+..++....#\n#   + #    +     #\n#     #   XX     #\n#     .   +.    E#\n##################\n");aq("/maps/level2.map","####################\n#S#          XXXXX #\n# #    XX    .XXX. #\n# #   .XX.    .X.  #\n# #    +.      +   #\n# ##   XX          #\n#      +X.        E#\n####################\n");aq("/maps/level3.map","##############\n#XXXX#       #\n#XXX+#  +    #\n#X..S#  X##  #\n#+     +X+#  #\n#    #X#X#   #\n#   +E....   #\n#   #++      #\n##############\n");aq("/maps/level4.map","########################################\n#...... ..+.X .....X.X....... ....X....#\n#.XSX...... .........X+..X.... ..... ..#\n#.......... ..X.....X.X..X........X....#\n#X.XX.........X......X..X....X...X.....#\n#X. X......... X..X........X......X.XX.#\n#... ..X........X.....X. X........X.XX.#\n###############################...X..X.#\n#. ...X..+. ..X.X..........+.X+...... .#\n#..+.....X..... ........XX X..X....X...#\n#...X..X.X..............X .X..X........#\n#.X.....X........XXX.......X.. .+....X.#\n#.+.. ..X.  .....X.X+..+....X...X..+. .#\n#. X..............X X..X........+.....X#\n#........###############################\n# X.........X...+....X.....X...X.......#\n# X......... X..X........X......X.XX..E#\n#. ..X........X.....X.  ....+...X.XX...#\n#....X+..X........X......X.X+......X...#\n#... ..X. ..X.XX.........X.X+...... ..X#\n#.+.... ..... ......... .X..X....X...X.#\n########################################\n");var
av=[f,c("Failure"),-3],aS=[f,c("Invalid_argument"),-4],aT=[f,c("Not_found"),-7],cF=[f,c("Match_failure"),-8],cE=[f,c("Stack_overflow"),-9],v=[f,c("Assert_failure"),-11],cG=[f,c("Undefined_recursive_module"),-12],bC=c('File "%s", line %d, characters %d-%d: %s'),c4=c(dG);R(11,cG);R(8,cE);R(7,cF);R(6,aT);R(5,[f,c("Division_by_zero"),-6]);R(4,[f,c("End_of_file"),-5]);R(3,aS);R(2,av);R(1,[f,c("Sys_error"),-2]);bW(c("Pervasives.array_bound_error"),[0,aS,c(bl)]);var
eY=[f,c("Out_of_memory"),-1],d_=c(dz),d9=c(as),d7=c("true"),d8=c("false"),d6=c("Pervasives.Exit"),d$=c("Pervasives.do_at_exit"),eb=c("Array.Bottom"),ec=c("nth"),ed=c("List.nth"),eg=c("\\b"),eh=c("\\t"),ei=c("\\n"),ej=c("\\r"),ef=c("\\\\"),ee=c("\\'"),em=c(d),el=c("String.blit"),ek=c("String.sub"),en=c("Sys.Break"),eo=c("Queue.Empty"),eq=c("CamlinternalLazy.Undefined"),er=c("Buffer.add: cannot grow buffer"),eH=c(d),eI=c(d),eL=c(dz),eM=c(bf),eN=c(bf),eJ=c(be),eK=c(be),eG=c(dA),eE=c("neg_infinity"),eF=c("infinity"),eD=c(as),eC=c("printf: bad positional specification (0)."),eB=c("%_"),eA=[0,c("printf.ml"),143,8],ey=c(be),ez=c("Printf: premature end of format string '"),eu=c(be),ev=c(" in format string '"),ew=c(", at char number "),ex=c("Printf: bad conversion %"),es=c("Sformat.index_of_int: negative argument "),eS=c(d),eT=c(", %s%s"),e_=[1,1],e$=c("%s\n"),fa=c("(Program not linked with -g, cannot print stack backtrace)\n"),e4=c("Raised at"),e7=c("Re-raised at"),e8=c("Raised by primitive operation at"),e9=c("Called from"),e5=c('%s file "%s", line %d, characters %d-%d'),e6=c("%s unknown location"),eZ=c("Out of memory"),e0=c("Stack overflow"),e1=c("Pattern matching failed"),e2=c("Assertion failed"),e3=c("Undefined recursive module"),eU=c("(%s%s)"),eV=c(d),eW=c(d),eX=c("(%s)"),eR=c(dv),eP=c("%S"),eQ=c("_"),fc=c("Lwt_sequence.Empty"),fr=[0,c(_),692,20],fs=[0,c(_),695,8],fp=[0,c(_),670,20],fq=[0,c(_),673,8],fn=[0,c(_),648,20],fo=[0,c(_),651,8],fl=[0,c(_),498,8],fk=[0,c(_),487,9],fj=c("Lwt.wakeup_later_result"),fi=c("Lwt.wakeup_result"),ff=c("Fatal error: exception "),fe=c("Lwt.Canceled"),fm=[0,0],fx=c("Js.Error"),fy=c(dy),fF=c("table"),fE=c("img"),fD=c("br"),fC=c("h1"),fB=c("div"),fA=c("option"),fG=c("Dom_html.Canvas_not_available"),fK=c("browser can't read file: unimplemented"),fJ=[0,c("file.ml"),131,15],fH=c("can't retrieve file name: not implemented"),fN=c("Exception during Lwt.async: "),fP=c("[\\][()\\\\|+*.?{}^$]"),f2=[0,c(d),0],f3=c(d),ge=c(d),gn=c(d),gf=c(bg),gg=c(bb),gm=c(d),gh=c(Z),gi=c(Z),gl=c(dm),gj=c(d),gk=c("http://"),go=c(d),gx=c(d),gp=c(bg),gq=c(bb),gw=c(d),gr=c(Z),gs=c(Z),gv=c(dm),gt=c(d),gu=c("https://"),gy=c(d),gE=c(d),gz=c(bg),gA=c(bb),gD=c(d),gB=c(Z),gC=c("file://"),gd=c(d),gc=c(d),gb=c(d),ga=c(d),f$=c(d),f_=c(d),f4=c(b8),f5=c(dL),fW=c("file"),fX=c("file:"),fY=c("http"),fZ=c("http:"),f0=c("https"),f1=c("https:"),fT=c("%2B"),fR=c("Url.Local_exn"),fS=c(aL),fU=c("Url.Not_an_http_protocol"),f6=c("^([Hh][Tt][Tt][Pp][Ss]?)://([0-9a-zA-Z.-]+|\\[[0-9a-zA-Z.-]+\\]|\\[[0-9A-Fa-f:.]+\\])?(:([0-9]+))?/([^\\?#]*)(\\?([^#]*))?(#(.*))?$"),f8=c("^([Ff][Ii][Ll][Ee])://([^\\?#]*)(\\?([^#]*))?(#(.*))?$"),gV=c(bo),gX=c("multipart/form-data; boundary="),gY=c(bo),gZ=[0,c(bo),[0,c("application/x-www-form-urlencoded")],bn],g0=[0,c(bo),0,bn],g1=c("GET"),gW=c(bb),gQ=c(b8),gR=c(b8),gS=c(dL),gM=c('"; filename="'),gN=c(dE),gK=c(bi),gL=c(b3),gO=c('"\r\n\r\n'),gP=c(dE),gI=c("--\r\n"),gJ=c(b3),gH=c("js_of_ocaml-------------------"),gG=[0,c("xmlHttpRequest.ml"),85,2],gT=c("XmlHttpRequest.Wrong_headers"),hh=c(dM),hi=c(dx),hg=c(bl),hj=c("YOU WIN !"),hk=c("YOU LOSE !"),hl=c(dp),hm=c("sprites/R.png"),hn=c("sprites/L.png"),ho=c("sprites/U.png"),hp=c("sprites/D.png"),hq=c("sprites/push_r.png"),hr=c("sprites/bR.png"),hs=c("sprites/push_l.png"),ht=c("sprites/bL.png"),hK=c(b_),hI=c(b_),hJ=c(b_),hA=c("%g"),hz=c(dI),hB=c("malformed level"),hx=c("border-collapse:collapse;line-height: 0; opacity: 0; margin-left:auto; margin-right:auto"),hy=c("padding: 0; width: 20px; height: 20px;"),hC=c("font-family: sans-serif; text-align: center; background-color: #e8e8e8;"),hD=c("Boulder Dash in Ocaml"),hE=c("Elapsed time: "),hF=c(" Remaining diamonds: "),hG=c(G),hH=c("Choose a level"),hu=[0,c("boulderdash.ml"),300,17],hv=c("boulderdash"),hw=c(b3),hL=c("maps.txt"),g9=c("sprites/empty.png"),g_=c("sprites/grass.png"),g$=c("sprites/diamond.png"),ha=c(dx),hb=c("sprites/door.png"),hc=c(dp),hd=c(dM),he=c("sprites/wall.png"),hf=c("sprites/bam.png"),g7=c("%02d:%02d:%02d"),g6=c("--:--:--"),g5=c("LOADING..."),g2=c("border: 1px black solid; background-color: white ; display: inline ; padding-right: .5em; padding-left: .5em;"),g3=c("background-color: red; color: white; display:inline; position: absolute; top:0; right:0;"),g8=c("Boulderdash.Death");function
M(a){throw[0,av,a]}function
aa(a){throw[0,aS,a]}a([f,d6,0]);function
i(a,b){var
c=a.getLen(),e=b.getLen(),d=S(c+e|0);X(a,0,d,0,c);X(b,0,d,c,e);return d}function
ab(a){return c(d+a)}function
cj(a,b){if(a){var
c=a[1];return[0,c,cj(a[2],b)]}return b}ih(0);de(1);var
aw=de(2);function
ck(a,b){return dZ(a,b,0,b.getLen())}function
cl(a){return ck(aw,a)}function
bu(a){var
b=ij(0);for(;;){if(b){var
c=b[2],d=b[1];try{df(d)}catch(f){}var
b=c;continue}return 0}}bW(d$,bu);function
ea(a,b){return dg(a,b)}function
cm(a){return df(a)}function
cn(a,b){var
d=b.length-1;if(0===d)return[0];var
e=T(d,j(a,b[0+1])),f=d-1|0,g=1;if(!(f<1)){var
c=g;for(;;){e[c+1]=j(a,b[c+1]);var
h=c+1|0;if(f!==c){var
c=h;continue}break}}return e}function
co(a){if(a){var
d=0,c=a,g=a[2],h=a[1];for(;;){if(c){var
d=d+1|0,c=c[2];continue}var
f=T(d,h),e=1,b=g;for(;;){if(b){var
i=b[2];f[e+1]=b[1];var
e=e+1|0,b=i;continue}return f}}}return[0]}a([f,eb,0]);function
al(a){var
b=a,c=0;for(;;){if(b){var
d=[0,b[1],c],b=b[2],c=d;continue}return c}}function
ac(a,b){if(b){var
c=b[2],d=j(a,b[1]);return[0,d,ac(a,c)]}return 0}function
am(a,b){var
c=b;for(;;){if(c){var
d=c[2];j(a,c[1]);var
c=d;continue}return 0}}function
ax(a,b){var
c=S(a);hS(c,0,a,b);return c}function
W(a,b,c){if(0<=b)if(0<=c)if(!((a.getLen()-c|0)<b)){var
d=S(c);X(a,b,d,0,c);return d}return aa(ek)}function
bv(a,b,c,d,e){if(0<=e)if(0<=b)if(!((a.getLen()-e|0)<b))if(0<=d)if(!((c.getLen()-e|0)<d))return X(a,b,c,d,e);return aa(el)}function
ay(d,b){if(b){var
a=b[1],g=[0,0],f=[0,0],h=b[2];am(function(a){g[1]++;f[1]=f[1]+a.getLen()|0;return 0},b);var
e=S(f[1]+dh(d.getLen(),g[1]-1|0)|0);X(a,0,e,0,a.getLen());var
c=[0,a.getLen()];am(function(a){X(d,0,e,c[1],d.getLen());c[1]=c[1]+d.getLen()|0;X(a,0,e,c[1],a.getLen());c[1]=c[1]+a.getLen()|0;return 0},h);return e}return em}var
bw=iB(0),az=dh(bw/8|0,(1<<(bw-10|0))-1|0)-1|0;a([f,en,0]);var
ep=a([f,eo,0]);a([f,eq,0]);function
bx(a){var
b=1<=a?a:1,c=az<b?az:b,d=S(c);return[0,d,0,c,d]}function
by(a){return W(a[1],0,a[2])}function
cp(a,b){var
c=[0,a[3]];for(;;){if(c[1]<(a[2]+b|0)){c[1]=2*c[1]|0;continue}if(az<c[1])if((a[2]+b|0)<=az)c[1]=az;else
M(er);var
d=S(c[1]);bv(a[1],0,d,0,a[2]);a[1]=d;a[3]=c[1];return 0}}function
aA(a,b){var
c=a[2];if(a[3]<=c)cp(a,1);a[1].safeSet(c,b);a[2]=c+1|0;return 0}function
bz(a,b){var
c=b.getLen(),d=a[2]+c|0;if(a[3]<d)cp(a,c);X(b,0,a[1],a[2],c);a[2]=d;return 0}function
bA(a){return 0<=a?a:M(i(es,ab(a)))}function
cq(a,b){return bA(a+b|0)}var
et=1;function
cr(a){return cq(et,a)}function
cs(a){return W(a,0,a.getLen())}function
ct(a,b,c){var
d=i(ev,i(a,eu)),e=i(ew,i(ab(b),d));return aa(i(ex,i(ax(1,c),e)))}function
aB(a,b,c){return ct(cs(a),b,c)}function
aU(a){return aa(i(ez,i(cs(a),ey)))}function
ad(e,b,c,d){function
h(a){if((e.safeGet(a)+ar|0)<0||9<(e.safeGet(a)+ar|0))return a;var
b=a+1|0;for(;;){var
c=e.safeGet(b);if(48<=c){if(!(58<=c)){var
b=b+1|0;continue}}else
if(36===c)return b+1|0;return a}}var
i=h(b+1|0),f=bx((c-i|0)+10|0);aA(f,37);var
a=i,g=al(d);for(;;){if(a<=c){var
j=e.safeGet(a);if(42===j){if(g){var
k=g[2];bz(f,ab(g[1]));var
a=h(a+1|0),g=k;continue}throw[0,v,eA]}aA(f,j);var
a=a+1|0;continue}return by(f)}}function
cu(a,b,c,d,e){var
f=ad(b,c,d,e);if(78!==a)if(aN!==a)return f;f.safeSet(f.getLen()-1|0,b6);return f}function
cv(a){return function(d,b){var
k=d.getLen();function
l(a,b){var
m=40===a?41:b5,c=b;for(;;){if(k<=c)return aU(d);if(37===d.safeGet(c)){var
e=c+1|0;if(k<=e)return aU(d);var
f=d.safeGet(e),g=f-40|0;if(g<0||1<g){var
i=g+ds|0;if(i<0||2<i)var
h=1;else
switch(i){case
1:var
h=1;break;case
2:var
j=1,h=0;break;default:var
j=0,h=0}if(h){var
c=e+1|0;continue}}else
var
j=0===g?0:1;if(j)return f===m?e+1|0:aB(d,b,f);var
c=l(f,e+1|0)+1|0;continue}var
c=c+1|0;continue}}return l(a,b)}}function
cw(i,b,c){var
l=i.getLen()-1|0;function
r(a){var
k=a;a:for(;;){if(k<l){if(37===i.safeGet(k)){var
f=0,h=k+1|0;for(;;){if(l<h)var
e=aU(i);else{var
m=i.safeGet(h);if(58<=m){if(95===m){var
f=1,h=h+1|0;continue}}else
if(32<=m)switch(m+ca|0){case
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
h=u(b,f,h,aj);continue;default:var
h=h+1|0;continue}var
d=h;b:for(;;){if(l<d)var
e=aU(i);else{var
j=i.safeGet(d);if(126<=j)var
g=0;else
switch(j){case
78:case
88:case
bk:case
aj:case
b$:case
b6:case
b7:var
e=u(b,f,d,aj),g=1;break;case
69:case
70:case
71:case
dH:case
cc:case
cb:var
e=u(b,f,d,cc),g=1;break;case
33:case
37:case
44:case
64:var
e=d+1|0,g=1;break;case
83:case
91:case
aO:var
e=u(b,f,d,aO),g=1;break;case
97:case
bh:case
b1:var
e=u(b,f,d,j),g=1;break;case
76:case
dN:case
aN:var
s=d+1|0;if(l<s)var
e=u(b,f,d,aj),g=1;else{var
p=i.safeGet(s)+dJ|0;if(p<0||32<p)var
q=1;else
switch(p){case
0:case
12:case
17:case
23:case
29:case
32:var
e=o(c,u(b,f,d,j),aj),g=1,q=0;break;default:var
q=1}if(q)var
e=u(b,f,d,aj),g=1}break;case
67:case
99:var
e=u(b,f,d,99),g=1;break;case
66:case
98:var
e=u(b,f,d,66),g=1;break;case
41:case
b5:var
e=u(b,f,d,j),g=1;break;case
40:var
e=r(u(b,f,d,j)),g=1;break;case
b9:var
t=u(b,f,d,j),v=o(cv(j),i,t),n=t;for(;;){if(n<(v-2|0)){var
n=o(c,n,i.safeGet(n));continue}var
d=v-1|0;continue b}default:var
g=0}if(!g)var
e=aB(i,d,j)}break}}var
k=e;continue a}}var
k=k+1|0;continue}return k}}r(0);return 0}function
cx(a){var
d=[0,0,0,0];function
b(a,b,c){var
f=41!==c?1:0,g=f?b5!==c?1:0:f;if(g){var
e=97===c?2:1;if(bh===c)d[3]=d[3]+1|0;if(a)d[2]=d[2]+e|0;else
d[1]=d[1]+e|0}return b+1|0}cw(a,b,function(a,b){return a+1|0});return d[1]}function
cy(a,b,c){var
g=a.safeGet(c);if((g+ar|0)<0||9<(g+ar|0))return o(b,0,c);var
e=g+ar|0,d=c+1|0;for(;;){var
f=a.safeGet(d);if(48<=f){if(!(58<=f)){var
e=(10*e|0)+(f+ar|0)|0,d=d+1|0;continue}}else
if(36===f)return 0===e?M(eC):o(b,[0,bA(e-1|0)],d+1|0);return o(b,0,c)}}function
x(a,b){return a?b:cr(b)}function
cz(a,b){return a?a[1]:b}function
cA(aI,b,c,d,e,f,g){var
B=j(b,g);function
ae(a){return o(d,B,a)}function
aJ(a,b,k,aK){var
n=k.getLen();function
C(q,b){var
m=b;for(;;){if(n<=m)return j(a,B);var
d=k.safeGet(m);if(37===d){var
l=function(a,b){return h(aK,cz(a,b))},ar=function(g,f,c,d){var
a=d;for(;;){var
_=k.safeGet(a)+ca|0;if(!(_<0||25<_))switch(_){case
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
10:return cy(k,function(a,b){var
d=[0,l(a,f),c];return ar(g,x(a,f),d,b)},a+1|0);default:var
a=a+1|0;continue}var
n=k.safeGet(a);if(!(124<=n))switch(n){case
78:case
88:case
bk:case
aj:case
b$:case
b6:case
b7:var
a5=l(g,f),a6=a8(cu(n,k,m,a,c),a5);return p(x(g,f),a6,a+1|0);case
69:case
71:case
dH:case
cc:case
cb:var
aY=l(g,f),aZ=bX(ad(k,m,a,c),aY);return p(x(g,f),aZ,a+1|0);case
76:case
dN:case
aN:var
ab=k.safeGet(a+1|0)+dJ|0;if(!(ab<0||32<ab))switch(ab){case
0:case
12:case
17:case
23:case
29:case
32:var
Q=a+1|0,ac=n-108|0;if(ac<0||2<ac)var
af=0;else{switch(ac){case
1:var
af=0,ag=0;break;case
2:var
a4=l(g,f),az=a8(ad(k,m,Q,c),a4),ag=1;break;default:var
a3=l(g,f),az=a8(ad(k,m,Q,c),a3),ag=1}if(ag)var
ay=az,af=1}if(!af)var
a2=l(g,f),ay=hZ(ad(k,m,Q,c),a2);return p(x(g,f),ay,Q+1|0)}var
a0=l(g,f),a1=a8(cu(aN,k,m,a,c),a0);return p(x(g,f),a1,a+1|0);case
37:case
64:return p(f,ax(1,n),a+1|0);case
83:case
aO:var
v=l(g,f);if(aO===n)var
w=v;else{var
b=[0,0],al=v.getLen()-1|0,aL=0;if(!(al<0)){var
J=aL;for(;;){var
u=v.safeGet(J),bb=14<=u?34===u?1:92===u?1:0:11<=u?13<=u?1:0:8<=u?1:0,aQ=bb?2:bY(u)?1:4;b[1]=b[1]+aQ|0;var
aR=J+1|0;if(al!==J){var
J=aR;continue}break}}if(b[1]===v.getLen())var
aD=v;else{var
h=S(b[1]);b[1]=0;var
am=v.getLen()-1|0,aM=0;if(!(am<0)){var
I=aM;for(;;){var
t=v.safeGet(I),z=t-34|0;if(z<0||58<z)if(-20<=z)var
R=1;else{switch(z+34|0){case
8:h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],98);var
H=1;break;case
9:h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],b1);var
H=1;break;case
10:h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],aN);var
H=1;break;case
13:h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],bh);var
H=1;break;default:var
R=1,H=0}if(H)var
R=0}else
var
R=(z-1|0)<0||56<(z-1|0)?(h.safeSet(b[1],92),b[1]++,h.safeSet(b[1],t),0):1;if(R)if(bY(t))h.safeSet(b[1],t);else{h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],48+(t/bk|0)|0);b[1]++;h.safeSet(b[1],48+((t/10|0)%10|0)|0);b[1]++;h.safeSet(b[1],48+(t%10|0)|0)}b[1]++;var
aP=I+1|0;if(am!==I){var
I=aP;continue}break}}var
aD=h}var
w=i(eN,i(aD,eM))}if(a===(m+1|0))var
aC=w;else{var
G=ad(k,m,a,c);try{var
T=0,r=1;for(;;){if(G.getLen()<=r)var
an=[0,0,T];else{var
U=G.safeGet(r);if(49<=U)if(58<=U)var
ah=0;else
var
an=[0,di(W(G,r,(G.getLen()-r|0)-1|0)),T],ah=1;else{if(45===U){var
T=1,r=r+1|0;continue}var
ah=0}if(!ah){var
r=r+1|0;continue}}var
X=an;break}}catch(f){f=y(f);if(f[1]!==av)throw f;var
X=ct(G,0,aO)}var
K=X[1],A=w.getLen(),aS=X[2],L=0,aT=32;if(K===A)if(0===L)var
Y=w,ai=1;else
var
ai=0;else
var
ai=0;if(!ai)if(K<=A)var
Y=W(w,L,A);else{var
V=ax(K,aT);if(aS)bv(w,L,V,0,A);else
bv(w,L,V,K-A|0,A);var
Y=V}var
aC=Y}return p(x(g,f),aC,a+1|0);case
67:case
99:var
q=l(g,f);if(99===n)var
au=ax(1,q);else{if(39===q)var
s=ee;else
if(92===q)var
s=ef;else{if(14<=q)var
D=0;else
switch(q){case
8:var
s=eg,D=1;break;case
9:var
s=eh,D=1;break;case
10:var
s=ei,D=1;break;case
13:var
s=ej,D=1;break;default:var
D=0}if(!D)if(bY(q)){var
ak=S(1);ak.safeSet(0,q);var
s=ak}else{var
E=S(4);E.safeSet(0,92);E.safeSet(1,48+(q/bk|0)|0);E.safeSet(2,48+((q/10|0)%10|0)|0);E.safeSet(3,48+(q%10|0)|0);var
s=E}}var
au=i(eK,i(s,eJ))}return p(x(g,f),au,a+1|0);case
66:case
98:var
aW=a+1|0,aX=l(g,f)?d7:d8;return p(x(g,f),aX,aW);case
40:case
b9:var
P=l(g,f),as=o(cv(n),k,a+1|0);if(b9===n){var
M=bx(P.getLen()),ao=function(a,b){aA(M,b);return a+1|0};cw(P,function(a,b,c){if(a)bz(M,eB);else
aA(M,37);return ao(b,c)},ao);var
aU=by(M);return p(x(g,f),aU,as)}var
at=x(g,f),ba=cq(cx(P),at);return aJ(function(a){return C(ba,as)},at,P,aK);case
33:j(e,B);return C(f,a+1|0);case
41:return p(f,eH,a+1|0);case
44:return p(f,eI,a+1|0);case
70:var
$=l(g,f);if(0===c)var
aw=eL;else{var
Z=ad(k,m,a,c);if(70===n)Z.safeSet(Z.getLen()-1|0,cb);var
aw=Z}var
aq=hO($);if(3===aq)var
aa=$<0?eE:eF;else
if(4<=aq)var
aa=eG;else{var
O=bX(aw,$),N=0,aV=O.getLen();for(;;){if(aV<=N)var
ap=i(O,eD);else{var
F=O.safeGet(N)-46|0,bc=F<0||23<F?55===F?1:0:(F-1|0)<0||21<(F-1|0)?1:0;if(!bc){var
N=N+1|0;continue}var
ap=O}var
aa=ap;break}}return p(x(g,f),aa,a+1|0);case
91:return aB(k,a,n);case
97:var
aE=l(g,f),aF=cr(cz(g,f)),aG=l(0,aF),a7=a+1|0,a9=x(g,aF);if(aI)ae(o(aE,0,aG));else
o(aE,B,aG);return C(a9,a7);case
bh:return aB(k,a,n);case
b1:var
aH=l(g,f),a_=a+1|0,a$=x(g,f);if(aI)ae(j(aH,0));else
j(aH,B);return C(a$,a_)}return aB(k,a,n)}},f=m+1|0,g=0;return cy(k,function(a,b){return ar(a,q,g,b)},f)}o(c,B,d);var
m=m+1|0;continue}}function
p(a,b,c){ae(b);return C(a,c)}return C(b,0)}var
p=bA(0);function
l(a,b){return aJ(f,p,a,b)}var
m=cx(g);if(m<0||6<m){var
n=function(f,b){if(m<=f){var
h=T(m,0),i=function(a,b){return k(h,(m-a|0)-1|0,b)},c=0,a=b;for(;;){if(a){var
d=a[2],e=a[1];if(d){i(c,e);var
c=c+1|0,a=d;continue}i(c,e)}return l(g,h)}}return function(a){return n(f+1|0,[0,a,b])}};return n(0,0)}switch(m){case
1:return function(a){var
b=T(1,0);k(b,0,a);return l(g,b)};case
2:return function(a,b){var
c=T(2,0);k(c,0,a);k(c,1,b);return l(g,c)};case
3:return function(a,b,c){var
d=T(3,0);k(d,0,a);k(d,1,b);k(d,2,c);return l(g,d)};case
4:return function(a,b,c,d){var
e=T(4,0);k(e,0,a);k(e,1,b);k(e,2,c);k(e,3,d);return l(g,e)};case
5:return function(a,b,c,d,e){var
f=T(5,0);k(f,0,a);k(f,1,b);k(f,2,c);k(f,3,d);k(f,4,e);return l(g,f)};case
6:return function(a,b,c,d,e,f){var
h=T(6,0);k(h,0,a);k(h,1,b);k(h,2,c);k(h,3,d);k(h,4,e);k(h,5,f);return l(g,h)};default:return l(g,[0])}}function
cB(d){function
e(a){return 0}function
b(a){return d}var
c=0;return function(a){return cA(c,b,ea,ck,cm,e,a)}}function
eO(a){return bx(2*a.getLen()|0)}function
A(a){function
b(a){var
b=by(a);a[2]=0;return b}return cA(1,eO,aA,bz,function(a){return 0},b,a)}var
bB=[0,0];function
bD(a,b){var
c=a[b+1];if(ip(c)){if(a9(c)===252)return j(A(eP),c);if(a9(c)===253){var
e=bX(d_,c),d=0,g=e.getLen();for(;;){if(g<=d)return i(e,d9);var
f=e.safeGet(d),h=48<=f?58<=f?0:1:45===f?1:0;if(h){var
d=d+1|0;continue}return e}}return eQ}return j(A(eR),c)}function
cC(a,b){if(a.length-1<=b)return eS;var
c=cC(a,b+1|0),d=bD(a,b);return o(A(eT),d,c)}function
cD(a){var
b=bB[1];for(;;){if(b){var
r=b[2],s=b[1];try{var
t=j(s,a),e=t}catch(f){var
e=0}if(e)return e[1];var
b=r;continue}if(a===eY)return eZ;if(a===cE)return e0;if(a[1]===cF){var
f=a[2],k=f[3],u=f[2],w=f[1];return a_(A(bC),w,u,k,k+5|0,e1)}if(a[1]===v){var
g=a[2],l=g[3],x=g[2],y=g[1];return a_(A(bC),y,x,l,l+6|0,e2)}if(a[1]===cG){var
h=a[2],m=h[3],z=h[2],B=h[1];return a_(A(bC),B,z,m,m+6|0,e3)}if(0===a9(a)){var
d=a.length-1,C=a[0+1][0+1];if(d<0||2<d)var
n=cC(a,2),p=bD(a,1),c=o(A(eU),p,n);else
switch(d){case
1:var
c=eW;break;case
2:var
q=bD(a,1),c=j(A(eX),q);break;default:var
c=eV}return i(C,c)}return a[0+1]}}function
cH(a){var
i=hP(hW(0));if(i){var
d=i[1],f=d.length-1-1|0,q=0;if(!(f<0)){var
c=q;for(;;){if(io(h(d,c),e_)){var
b=h(d,c),k=0===b[0]?b[1]:b[1],e=k?0===c?e4:e7:0===c?e8:e9;if(0===b[0])var
l=b[5],m=b[4],n=b[3],p=b[2],g=a_(A(e5),e,p,n,m,l);else
var
g=j(A(e6),e);o(cB(a),e$,g)}var
r=c+1|0;if(f!==c){var
c=r;continue}break}}return 0}return j(cB(a),fa)}function
cI(a){bB[1]=[0,a,bB[1]];return 0}32===bw;var
fd=a([f,fc,0]);function
cJ(a){var
b=a[4],c=b?(a[4]=0,a[1][2]=a[2],a[2][1]=a[1],0):b;return c}function
bE(a){var
b=[];dj(b,[0,b,b]);return b}function
bF(a){return a[2]===a?1:0}var
aV=a([f,fe,0]),D=[0,0];function
aW(a){var
c=a[1];if(3===c[0]){var
d=c[1],b=aW(d);if(b!==d)a[1]=[3,b];return b}return a}function
ae(a){return aW(a)}var
cK=[0,function(a){cl(ff);cl(cD(a));dg(aw,10);cH(aw);cm(aw);bu(0);return iC(2)}];function
cL(a,b){try{var
c=j(a,b)}catch(f){f=y(f);return j(cK[1],f)}return c}function
bZ(a,b,c,d){var
f=c,e=d;for(;;)if(typeof
f===z)return a<50?U(1+a,b,e):V(U,[0,b,e]);else
switch(f[0]){case
1:j(f[1],b);return a<50?U(1+a,b,e):V(U,[0,b,e]);case
2:var
h=[0,f[2],e],f=f[1],e=h;continue;default:var
g=f[1][1];if(g){j(g[1],b);return a<50?U(1+a,b,e):V(U,[0,b,e])}else
return a<50?U(1+a,b,e):V(U,[0,b,e])}}function
U(a,b,c){return c?a<50?bZ(1+a,b,c[1],c[2]):V(bZ,[0,b,c[1],c[2]]):0}function
fg(b,c,d){return a$(bZ(0,b,c,d))}function
iL(b,c){return a$(U(0,b,c))}function
b0(a,b,c){var
e=b,d=c;for(;;)if(typeof
e===z)return a<50?ag(1+a,d):V(ag,[0,d]);else
switch(e[0]){case
1:cJ(e[1]);return a<50?ag(1+a,d):V(ag,[0,d]);case
2:var
g=[0,e[2],d],e=e[1],d=g;continue;default:var
f=e[2];D[1]=e[1];cL(f,0);return a<50?ag(1+a,d):V(ag,[0,d])}}function
ag(a,b){return b?a<50?b0(1+a,b[1],b[2]):V(b0,[0,b[1],b[2]]):0}function
fh(b,c){return a$(b0(0,b,c))}function
iM(b){return a$(ag(0,b))}function
aX(a,b){var
c=1===b[0]?b[1]===aV?(fh(a[4],0),1):0:0;return fg(b,a[2],0)}var
aY=[0,0],w=[0,0,0];function
cM(a,b){var
f=D[1],g=aY[1]?1:(aY[1]=1,0);aX(a,b);if(g){D[1]=f;return 0}for(;;){if(0===w[1]){aY[1]=0;D[1]=f;return 0}if(0===w[1])throw ep;w[1]=w[1]-1|0;var
c=w[2],d=c[2];if(d===c)w[2]=0;else
c[2]=d[2];var
e=d[1];aX(e[1],e[2]);continue}}function
cN(a){return[0,a]}function
cO(a,b){var
d=aW(a),c=d[1];switch(c[0]){case
1:if(c[1]===aV)return 0;break;case
2:var
e=c[1];d[1]=b;return cM(e,b)}return aa(fi)}function
aZ(a,b){return cO(a,cN(b))}function
cP(a,b){return typeof
a===z?b:typeof
b===z?a:[2,a,b]}function
bG(a){if(typeof
a!==z)switch(a[0]){case
2:var
b=a[1],c=bG(a[2]);return cP(bG(b),c);case
1:break;default:if(!a[1][1])return 0}return a}function
cQ(a,b){var
d=ae(a),g=ae(b),j=d[1];if(2===j[0]){var
c=j[1];if(d===g)return 0;var
e=g[1];if(2===e[0]){var
f=e[1];g[1]=[3,d];c[1]=f[1];var
k=cP(c[2],f[2]),l=c[3]+f[3]|0;if(42<l){c[3]=0;c[2]=bG(k)}else{c[3]=l;c[2]=k}var
h=f[4],i=c[4],m=typeof
i===z?h:typeof
h===z?i:[2,i,h];c[4]=m;return 0}d[1]=e;return aX(c,e)}throw[0,v,fk]}function
a0(a,b){var
c=ae(a),d=c[1];if(2===d[0]){var
e=d[1];c[1]=b;return aX(e,b)}throw[0,v,fl]}function
l(a){return[0,[0,a]]}var
cR=[0,fm];function
aC(a){return[0,[1,a]]}function
bH(a){return[0,[2,[0,[0,[0,a]],0,0,0]]]}function
bI(a){var
b=[0,[2,[0,1,0,0,0]]];return[0,b,b]}function
bJ(a,b){var
d=[1,b],c=a[2],e=typeof
c===z?d:[2,d,c];a[2]=e;return 0}function
bK(a,b){var
c=ae(a)[1];switch(c[0]){case
1:if(c[1]===aV)return cL(b,0);break;case
2:var
d=c[1],e=[0,D[1],b],f=d[4],g=typeof
f===z?e:[2,e,f];d[4]=g;return 0}return 0}function
m(a,b){var
e=ae(a),c=e[1];switch(c[0]){case
1:return[0,c];case
2:var
f=c[1],d=bH(e),g=D[1];bJ(f,function(a){switch(a[0]){case
0:var
e=a[1];D[1]=g;try{var
f=j(b,e),c=f}catch(f){f=y(f);var
c=aC(f)}return cQ(d,c);case
1:return a0(d,a);default:throw[0,v,fn]}});return d;case
3:throw[0,v,fo];default:return j(b,c[1])}}function
cS(a,b){return m(a,b)}function
cT(a,b){var
e=ae(a),c=e[1];switch(c[0]){case
1:return[0,c];case
2:var
i=c[1],d=bH(e),k=D[1];bJ(i,function(a){switch(a[0]){case
0:var
e=a[1];D[1]=k;try{var
f=[0,j(b,e)],c=f}catch(f){f=y(f);var
c=[1,f]}return a0(d,c);case
1:return a0(d,a);default:throw[0,v,fp]}});return d;case
3:throw[0,v,fq];default:var
f=c[1];try{var
h=[0,j(b,f)],g=h}catch(f){f=y(f);var
g=[1,f]}return[0,g]}}function
cU(a,b){try{var
i=j(a,0),g=i}catch(f){f=y(f);var
g=aC(f)}var
c=ae(g),d=c[1];switch(d[0]){case
1:return j(b,d[1]);case
2:var
f=d[1],e=bH(c),h=D[1];bJ(f,function(a){switch(a[0]){case
0:return a0(e,a);case
1:var
d=a[1];D[1]=h;try{var
f=j(b,d),c=f}catch(f){f=y(f);var
c=aC(f)}return cQ(e,c);default:throw[0,v,fr]}});return e;case
3:throw[0,v,fs];default:return c}}var
ft=[0,function(a){return 0}],N=bE(0),fu=[0,0];function
fv(a){var
e=1-bF(N);if(e){var
b=bE(0);b[1][2]=N[2];N[2][1]=b[1];b[1]=N[1];N[1][2]=b;N[1]=N;N[2]=N;fu[1]=0;var
c=b[2];for(;;){var
d=c!==b?1:0;if(d){if(c[4])aZ(c[3],0);var
c=c[2];continue}return d}}return e}function
cV(c,b){if(b){var
d=b[2],a=b[1],e=function(a){return cV(c,d)};return cS(j(c,a),e)}return cR}var
E=b,O=null,an=undefined;function
a1(a,b,c){return a==O?j(b,0):j(c,a)}function
cW(a){function
b(a){return[0,a]}return a1(a,function(a){return 0},b)}function
aD(a){return a!==an?1:0}function
bL(a,b,c){return a===an?j(b,0):j(c,a)}function
K(a,b){return a===an?j(b,0):a}function
a2(a){function
b(a){return[0,a]}return bL(a,function(a){return 0},b)}var
bM=true,a3=false,aE=RegExp,bN=Array;function
B(a,b){return a[b]}function
cX(a){return a}var
cY=a([f,fx,0]),bO=[0,cY,{}],fw=Math,fb=a9(bO)===f?bO:bO[0+1];bW(fy,fb);function
cZ(a){return escape(a)}cI(function(a){return a[1]===cY?[0,new
I(a[2].toString())]:0});cI(function(a){return a
instanceof
bN?0:[0,new
I(a.toString())]});function
ao(a){return a}function
aF(a){return a}function
p(a,b){a.appendChild(b);return 0}function
c0(a,b){a.removeChild(b);return 0}function
F(d){return aF(ba(function(a){if(a){var
e=j(d,a);if(!(e|0))a.preventDefault();return e}var
c=event,b=j(d,c);if(!(b|0))c.returnValue=b;return b}))}var
c1=ic(0)|0,fz=E.document;function
a4(a,b){return a?j(b,a[1]):0}function
bP(a,b){return a.createElement(b.toString())}function
ap(a,b){return bP(a,b)}var
c2=[0,dC];function
c3(a){return ap(a,fA)}function
aG(a){return ap(a,fB)}function
c5(a){return ap(a,fD)}a([f,fG,0]);ao(E.HTMLElement)===an;var
fI=E.FileReader,fL=ia(0),fM=b4;function
a5(a){var
b=bI(0),c=b[1],d=[0,0],g=b[2];function
e(a,b){var
c=b4<a?[0,fM,a-b4]:[0,a,0],f=c[2],h=c[1],i=f==0?function(a){return aZ(g,a)}:function(a){return e(f,a)};d[1]=[0,E.setTimeout(ba(i),h*dD)];return 0}e(a,0);bK(c,function(a){var
b=d[1];return b?E.clearTimeout(b[1]):0});return c}ft[1]=function(a){return 1===a?(E.setTimeout(ba(fv),0),0):0};function
c6(a){return fL.log(a.toString())}cK[1]=function(a){c6(fN);c6(cD(a));return cH(aw)};function
c7(a){return new
aE(q(a),aK)}var
fO=new
aE("[$]",aK),fQ=c7(fP);function
c9(a,b){return b.split(ax(1,a).toString())}var
c_=a([f,fR,0]);function
af(a){throw c_}var
c8=c7(Y(q(fS).replace(fQ,"\\$&"))),c$=new
aE("\\+",aK);function
P(a){c$[c(dB)]=0;return Y(unescape(a.replace(c$,G)))}function
s(a,b){var
e=a?a[1]:1;if(e){var
f=Y(cZ(q(b)));c8[c(dB)]=0;var
d=q(f);return Y(d.replace(c8,q(fT).replace(fO,"$$$$")))}return Y(cZ(q(b)))}var
fV=a([f,fU,0]);function
aH(a){try{var
c=a.getLen();if(0===c)var
d=f2;else{var
b=0,g=47,f=a.getLen();for(;;){if(f<=b)throw aT;if(a.safeGet(b)!==g){var
b=b+1|0;continue}if(0===b)var
e=[0,f3,aH(W(a,1,c-1|0))];else
var
h=aH(W(a,b+1|0,(c-b|0)-1|0)),e=[0,W(a,0,b),h];var
d=e;break}}}catch(f){f=y(f);if(f===aT)return[0,a,0];throw f}return d}function
a6(a){return ay(f5,ac(function(a){var
b=a[1],c=i(f4,s(0,a[2]));return i(s(0,b),c)},a))}function
bQ(a){var
d=c9(38,a),b=d.length;function
e(a,b){var
c=b;for(;;){if(0<=c){try{var
f=c-1|0,g=function(a){function
e(a){var
c=a[2],d=a[1];function
b(a){return P(K(a,af))}var
e=b(c);return[0,b(d),e]}var
b=c9(61,a);if(2===b.length)var
d=B(b,1),c=ao([0,B(b,0),d]);else
var
c=an;return bL(c,af,e)},h=e([0,bL(B(d,c),af,g),a],f)}catch(f){f=y(f);if(f===c_){var
c=c-1|0;continue}throw f}return h}return a}}return e(0,b-1|0)}var
f7=new
aE(q(f6)),f9=new
aE(q(f8));function
da(a){switch(a[0]){case
1:var
c=a[1],h=c[6],j=c[5],k=c[2],w=c[3],x=c[1],y=C(h,go)?i(gp,s(0,h)):gx,z=j?i(gq,a6(j)):gw,A=i(z,y),B=i(gs,i(ay(gr,ac(function(a){return s(0,a)},w)),A)),D=du===k?gt:i(gv,ab(k)),E=i(D,B);return i(gu,i(s(0,x),E));case
2:var
d=a[1],l=d[4],m=d[3],F=d[1],G=C(l,gy)?i(gz,s(0,l)):gE,H=m?i(gA,a6(m)):gD,I=i(H,G);return i(gC,i(ay(gB,ac(function(a){return s(0,a)},F)),I));default:var
b=a[1],e=b[6],f=b[5],g=b[2],n=b[3],o=b[1],p=C(e,ge)?i(gf,s(0,e)):gn,q=f?i(gg,a6(f)):gm,r=i(q,p),t=i(gi,i(ay(gh,ac(function(a){return s(0,a)},n)),r)),u=80===g?gj:i(gl,ab(g)),v=i(u,t);return i(gk,i(s(0,o),v))}}var
aI=location;P(aI.hostname);P(aI.protocol);try{}catch(f){f=y(f);if(f[1]!==av)throw f}aH(P(aI.pathname));bQ(aI.search);P(aI.href);var
gF=E.FormData;function
db(a,b){if(bm<=a[1]){var
d=a[2];d[1]=[0,b,d[1]];return 0}var
e=a[2],c=b[2],f=b[1];return bc<=c[1]?e.append(f.toString(),c[2]):e.append(f.toString(),c[2])}function
bR(a){return ActiveXObject}var
gU=a([f,gT,0]);function
e(a){return a.toString()}var
g=E.document;function
Q(a,b){return p(a,g.createTextNode(e(b)))}function
bS(a,b){var
c=a.firstChild;if(c!=O)c0(a,c);return p(a,b)}var
dc=e(g2),g4=e(g3),bT=a([f,g8,0]);function
dd(a){switch(a){case
1:return e(g_);case
2:return e(g$);case
3:return e(ha);case
4:return e(hb);case
5:return e(hc);case
6:return e(hd);case
7:return e(he);case
8:return e(hf);default:return e(g9)}}function
t(a,b,c,d){k(h(a[1],c),b,d);var
e=h(h(a[2],c),b);return e.src=dd(d)}function
a7(b){var
d=[0,0],e=b[1].length-1-2|0;if(!(e<1)){var
a=e;for(;;){var
f=h(b[1],a).length-1-2|0,j=1;if(!(f<1)){var
c=j;for(;;){var
g=6===h(h(b[1],a+1|0),c)?1:0,n=g?3===h(h(b[1],a),c)?1:0:g,p=0===h(h(b[1],a),c)?3===h(h(b[1],a-1|0),c)?(t(b,c,a-1|0,0),t(b,c,a,3),d[1]=1,1):0:0,q=0===h(h(b[1],a),c)?0===h(h(b[1],a-1|0),c)?3===h(h(b[1],a),c-1|0)?3===h(h(b[1],a-1|0),c-1|0)?(t(b,c-1|0,a-1|0,0),t(b,c,a,3),d[1]=1,1):0:0:0:0,r=0===h(h(b[1],a),c)?0===h(h(b[1],a-1|0),c)?3===h(h(b[1],a),c+1|0)?3===h(h(b[1],a-1|0),c+1|0)?(t(b,c+1|0,a-1|0,0),t(b,c,a,3),d[1]=1,1):0:0:0:0;if(!n)if(6===h(h(b[1],a+1|0),c))if(3===h(h(b[1],a),c)){t(b,c,a+1|0,8);throw bT}var
o=c+1|0;if(f!==c){var
c=o;continue}break}}var
k=a-1|0;if(1!==a){var
a=k;continue}break}}if(d[1]){var
i=function(a){return a7(b)};return m(a5(cd),i)}return l(0)}function
bU(g,x,v){var
L=v[3];function
n(a){var
M=g[1].length-1-1|0,Y=0;if(!(M<0)){var
c=Y;for(;;){var
V=h(g[1],c).length-1-1|0,ah=0;if(!(V<0)){var
d=ah;for(;;){h(h(g[2],c),d).onmouseover=O;h(h(g[2],c),d).onmouseout=O;h(h(g[2],c),d).onclick=O;var
aj=d+1|0;if(V!==d){var
d=aj;continue}break}}var
ai=c+1|0;if(M!==c){var
c=ai;continue}break}}function
n(a,b){if(!g[8]){g[8]=1;var
c=function(a){g[8]=0;return l(0)};m(j(a,0),c)}return a3}function
N(a,b,c){function
d(a){g[9][1]=[0,b];return l(0)}return m(j(a,0),d)}function
o(a,b){var
c=g[9][1];return c?(j(c[1],0),g[9][1]=0,j(a,0)):j(a,0)}function
i(a,I,H,G,e,f){var
s=a,p=G,k=e,i=f;for(;;){var
b=s[2],c=s[1],D=h(h(g[1],b),c);if(5===D)var
C=0;else
if(3<=D)var
E=0,C=1;else
var
C=0;if(!C)var
E=1;if(E){var
d=h(h(g[2],b),c).src,q=function(p,b,c){return function(a){h(h(g[2],b),c).src=H;return j(p,0)}}(p,b,c),r=function(k,b,c,d){return function(a){h(h(g[2],b),c).src=d;return j(k,0)}}(k,b,c,d),J=function(i,b,c){return function(a){function
d(a){if(2===h(h(g[1],b),c))g[5]=g[5]-1|0;t(g,c,b,6);function
d(a){function
d(a){t(g,c,b,0);return l(0)}return m(a7(g),d)}return m(a5(cd),d)}return m(j(i,0),d)}}(i,b,c),u=function(i,b,c){return function(a){var
d=g[3];t(g,d[1],d[2],0);function
e(a){return bU(g,x,v)}function
f(a){return a===bT?(g[6]=1,l(0)):aC(a)}return m(cU(function(a){function
d(a){if(2===h(h(g[1],b),c))g[5]=g[5]-1|0;t(g,c,b,6);g[3]=[0,c,b];return a7(g)}return m(j(i,0),d)},f),e)}}(i,b,c),K=h(h(g[2],b),c),w=function(q){return function(a){return o(q,a)}}(q),y=function(r,w){return function(a){return N(w,r,a)}}(r,w);K.onmouseover=F(function(y){return function(a){return n(y,a)}}(y));var
L=h(h(g[2],b),c),z=function(a){return l(0)},A=function(z){return function(a){return o(z,a)}}(z);L.onmouseout=F(function(A){return function(a){return n(A,a)}}(A));var
M=h(h(g[2],b),c),B=function(u){return function(a){return o(u,a)}}(u);M.onclick=F(function(B){return function(a){return n(B,a)}}(B));if(5===h(h(g[1],b),c))return 0;var
s=j(I,[0,c,b]),p=q,k=r,i=J;continue}return 0}}function
P(a,b,c,d){var
p=a[2],q=a[1],k=j(b,a),f=k[2],i=k[1],s=j(b,k),u=s[2],w=s[1];try{var
A=3===h(h(g[1],f),i)?1:0,P=A?0===h(h(g[1],u),w)?1:0:A,z=P}catch(f){f=y(f);if(f[1]===aS)if(C(f[2],hg))var
r=0;else
var
z=0,r=1;else
var
r=0;if(!r)throw f}if(z){var
B=function(a){h(h(g[2],p),q).src=d;h(h(g[2],f),i).src=c;return l(0)},D=function(a){var
b=h(h(g[2],p),q);b.src=e(hh);var
c=h(h(g[2],f),i);return c.src=e(hi)},E=function(a){t(g,q,p,0);t(g,i,f,6);g[3]=k;t(g,w,u,3);function
b(a){return bU(g,x,v)}function
c(a){return a===bT?(g[6]=1,l(0)):aC(a)}return m(cU(function(a){return a7(g)},c),b)},G=h(h(g[2],f),i),H=function(a){return o(B,a)},I=function(a){return N(H,D,a)};G.onmouseover=F(function(a){return n(I,a)});var
J=h(h(g[2],f),i),K=function(a){return l(0)},L=function(a){return o(K,a)};J.onmouseout=F(function(a){return n(L,a)});var
M=h(h(g[2],f),i),O=function(a){return o(E,a)};return M.onclick=F(function(a){return n(O,a)})}return 0}if(dk(g[3],g[4])){j(L,0);E.alert(e(hj))}else
if(g[6]){j(L,0);E.alert(e(hk))}else{if(0===g[5]){var
Q=g[4],R=Q[2],S=Q[1],Z=h(h(g[2],R),S);Z.src=e(hl);k(h(g[1],R),S,5)}var
z=function(a){return[0,a[1]+1|0,a[2]]},A=function(a){return[0,a[1]-1|0,a[2]]},T=function(a){return[0,a[1],a[2]-1|0]},U=function(a){return[0,a[1],a[2]+1|0]},q=function(a){return 0},b=function(a){return l(0)},_=e(hm);i(z(g[3]),z,_,b,q,b);var
$=e(hn);i(A(g[3]),A,$,b,q,b);var
ab=e(ho);i(T(g[3]),T,ab,b,q,b);var
ac=e(hp);i(U(g[3]),U,ac,b,q,b);var
ad=e(hq),ae=e(hr);P(g[3],z,ae,ad);var
af=e(hs),ag=e(ht);P(g[3],A,ag,af);j(x,g[5])}var
p=g[7];if(p[1])if(bF(p[2]))p[1]=0;else{var
K=p[2],X=0;if(bF(K))throw fd;var
G=K[2];cJ(G);var
W=G[3],u=cN(X),H=aW(W),s=H[1];switch(s[0]){case
1:var
f=s[1]===aV?1:0;break;case
2:var
I=s[1];H[1]=u;if(aY[1]){var
J=[0,I,u];if(0===w[1]){var
r=[];dj(r,[0,J,r]);w[1]=1;w[2]=r;var
f=1}else{var
B=w[2],D=[0,J,B[2]];w[1]=w[1]+1|0;B[2]=D;w[2]=D;var
f=1}}else{cM(I,u);var
f=1}break;default:var
f=0}if(!f)aa(fj)}return l(0)}var
c=g[7];if(c[1]){var
a=c[2],d=[0,1,0,0,0],f=[0,[2,d]],b=[0,a[1],a,f,1];a[1][2]=b;a[1]=b;d[4]=[1,b];var
i=f}else{c[1]=1;var
i=cR}return m(i,n)}function
bV(a,b){return b?a.style.cssText=b[1]:0}E.onload=F(function(a){var
f=g.getElementById(e(hv));if(f==O)throw[0,v,hu];var
D=aG(g),n=[0,aJ(0)],b=aG(g);b.style.cssText=dc;Q(b,g6);var
c=[0,1];function
r(a){var
f=aJ(0)-n[1];if(!c[1]){var
d=f|0;bS(b,g.createTextNode(e(u(A(g7),d/3600|0,(d/60|0)%60|0,d%60|0))))}function
h(a){return r(0)}return m(a5(1),h)}r(0);function
w(a){c[1]=1;return 0}var
t=[0,b,function(a){n[1]=aJ(0);c[1]=0;return 0},w],R=t[2],G=t[1];function
E(a,b){var
u=aG(g);u.style.cssText=g4;Q(u,g5);p(f,u);function
bg(a){function
c(a){c0(f,u);return l(a)}return m(j(b,a),c)}try{var
bf=l(hV(a)),aJ=bf}catch(f){f=y(f);if(f!==aT)throw f;var
be=function(a){var
b=a[2],c=a[4];if(0!==b)if(200!==b)return[0,[2,[0,0,0,0,0]]];return l(c)},az=0,aA=0,aB=0,aC=0,aE=0,aI=0,t=0,L=0,bd=0,aX=0?bd[1]:0,aY=aI?aI[1]:0,a0=aC?aC[1]:function(a,b){return 1};if(aE){var
$=aE[1];if(t){var
a4=t[1];am(function(a){return db($,[0,a[1],a[2]])},a4)}var
h=[0,$]}else
if(t){var
bb=t[1],T=a2(ao(gF)),ax=T?[0,808620462,new(T[1])()]:[0,bm,[0,0]];am(function(a){return db(ax,[0,a[1],a[2]])},bb);var
h=[0,ax]}else
var
h=0;if(h){var
aa=h[1];if(L)var
ad=[0,gV,L,bn];else{if(bm<=aa[1]){var
A=0,x=0,k=aa[2][1];for(;;){if(k){var
N=k[2],D=k[1],aK=bc<=D[2][1]?0:1;if(aK){var
A=[0,D,A],k=N;continue}var
x=[0,D,x],k=N;continue}var
aL=al(x);al(A);if(aL)var
U=function(a){return ab(fw.random()*1e9|0)},aS=U(0),V=i(gH,i(U(0),aS)),av=[0,gY,[0,i(gX,V)],[0,164354597,V]];else
var
av=gZ;var
aw=av;break}}else
var
aw=g0;var
ad=aw}var
n=ad}else
var
n=[0,g1,L,bn];var
ae=n[3],ag=n[2],S=q(a),a5=n[1],aM=function(a){var
c=cX(a),b=Y(K(B(c,1),af).toLowerCase());if(C(b,fW))if(C(b,fX)){if(C(b,fY))if(C(b,fZ)){if(C(b,f0))if(C(b,f1))var
d=1,f=0;else
var
f=1;else
var
f=1;if(f)var
e=1,d=2}else
var
d=0;else
var
d=0;switch(d){case
1:var
g=0;break;case
2:var
g=1;break;default:var
e=0,g=1}if(g){var
h=P(K(B(c,5),af)),k=function(a){return q(f$)},l=P(K(B(c,9),k)),m=function(a){return q(ga)},n=bQ(K(B(c,7),m)),o=aH(h),p=function(a){return q(gb)},i=Y(K(B(c,4),p)),r=C(i,f_)?di(i):e?du:80,j=[0,P(K(B(c,2),af)),r,o,h,n,l],s=e?[1,j]:[0,j];return[0,s]}}throw fV},aN=function(a){function
b(a){var
b=cX(a),c=P(K(B(b,2),af));function
d(a){return q(gc)}var
e=Y(K(B(b,6),d));function
f(a){return q(gd)}var
g=bQ(K(B(b,4),f));return[0,[2,[0,aH(c),c,g,e]]]}function
c(a){return 0}return a1(f9.exec(S),c,b)},R=a1(f7.exec(S),aN,aM);if(R){var
E=R[1];switch(E[0]){case
1:var
Z=E[1],_=Z.slice(),aW=Z[5];_[5]=0;var
r=[0,da([1,_]),aW],w=1;break;case
2:var
w=0;break;default:var
W=E[1],X=W.slice(),aV=W[5];X[5]=0;var
r=[0,da([0,X]),aV],w=1}}else
var
w=0;if(!w)var
r=[0,a,0];var
ah=r[1],ai=cj(r[2],aY),aj=ai?i(ah,i(gW,a6(ai))):ah,ak=bI(0),ap=ak[2],aq=ak[1];try{var
aR=new
XMLHttpRequest(),c=aR}catch(f){try{var
aQ=new(bR(0))("Msxml2.XMLHTTP"),c=aQ}catch(f){try{var
aP=new(bR(0))("Msxml3.XMLHTTP"),c=aP}catch(f){try{var
aO=new(bR(0))("Microsoft.XMLHTTP")}catch(f){throw[0,v,gG]}var
c=aO}}}if(az)c.overrideMimeType(az[1].toString());c.open(a5.toString(),aj.toString(),bM);if(ag)c.setRequestHeader("Content-type",ag[1].toString());am(function(a){return c.setRequestHeader(a[1].toString(),a[2].toString())},aX);var
G=function(a){function
b(a){return[0,new
I(a)]}function
d(a){return 0}return a1(c.getResponseHeader(q(a)),d,b)},ar=[0,0],H=function(a){var
b=ar[1]?0:o(a0,c.status,G)?0:(cO(ap,[1,[0,gU,[0,c.status,G]]]),c.abort(),1);ar[1]=1;return 0};c.onreadystatechange=ba(function(a){switch(c.readyState){case
2:if(!c1)return H(0);break;case
3:if(c1)return H(0);break;case
4:H(0);var
b=function(a){var
b=cW(c.responseXML);if(b){var
d=b[1];return aF(d.documentElement)===O?0:[0,d]}return 0};return aZ(ap,[0,aj,c.status,G,new
I(c.responseText),b])}return 0});if(aB){var
a7=aB[1];c.onprogress=F(function(a){o(a7,a.loaded,a.total);return bM})}var
as=c.upload;if(as!==an)if(aA){var
a8=aA[1];as.onprogress=F(function(a){o(a8,a.loaded,a.total);return bM})}if(h){var
J=h[1];if(bm<=J[1]){var
at=J[2];if(typeof
ae===z){var
a9=at[1];c.send(aF(ay(gS,ac(function(a){var
b=a[2],c=a[1];if(bc<=b[1]){var
d=i(gQ,s(0,new
I(b[2].name)));return i(s(0,c),d)}var
e=i(gR,s(0,new
I(b[2])));return i(s(0,c),e)},a9)).toString()))}else{var
au=ae[2],a_=function(a){var
b=aF(a.join(d));return aD(c.sendAsBinary)?c.sendAsBinary(b):c.send(b)},a$=at[1],e=new
bN(),aU=function(a){e.push(i(gJ,i(au,gI)).toString());return e};cT(cT(cV(function(a){e.push(i(gL,i(au,gK)).toString());var
g=a[2],n=a[1];if(bc<=g[1]){var
b=g[2],r=function(a){var
c=a2(b.name),g="Content-Type: application/octet-stream\r\n",h='"\r\n';if(c)var
f=c[1];else
var
d=a2(b.fileName),f=d?d[1]:M(fH);e.push(i(gN,i(n,gM)).toString(),f,h,g);e.push(bi,a,bi);return l(0)},k=a2(ao(fI)),d=-1041425454;if(k){var
c=new(k[1])(),h=bI(0),j=h[1],p=h[2];c.onloadend=F(function(a){if(2===c.readyState){var
b=c.result,e=dk(typeof
b,"string")?aF(b):O,d=cW(e);if(!d)throw[0,v,fJ];aZ(p,d[1])}return a3});bK(j,function(a){return c.abort()});if(typeof
d===z)if(dr===d)c.readAsDataURL(b);else
if(dq<=d)c.readAsText(b);else
c.readAsBinaryString(b);else
c.readAsText(b,d[2]);var
o=j}else{var
f=function(a){return M(fK)};if(typeof
d===z)var
m=dr===d?aD(b.getAsDataURL)?b.getAsDataURL():f(0):dq<=d?aD(b.getAsText)?b.getAsText("utf8"):f(0):aD(b.getAsBinary)?b.getAsBinary():f(0);else
var
q=d[2],m=aD(b.getAsText)?b.getAsText(q):f(0);var
o=l(m)}return cS(o,r)}var
s=g[2];e.push(i(gP,i(n,gO)).toString(),s,bi);return l(0)},a$),aU),a_)}}else
c.send(J[2])}else
c.send(O);bK(aq,function(a){return c.abort()});var
aJ=m(aq,be)}return m(aJ,bg)}var
k=aG(g);k.style.cssText=dc;Q(k,hw);function
S(a){return bS(k,g.createTextNode(ab(a).toString()))}function
x(n){var
C=f.style;C.cssText=e(hC);var
x=ap(g,fC);Q(x,hD);p(f,x);var
a=aG(g);Q(a,hE);p(a,G);Q(a,hF);p(a,k);Q(a,hG);var
o=0,q=0;for(;;){if(0===q)if(0===o)var
b=bP(g,c4),r=1;else
var
r=0;else
var
r=0;if(!r){var
s=c2[1];if(dC===s){try{var
v=fz.createElement('<input name="x">'),w=v.tagName.toLowerCase()==="input"?1:0,B=w?v.name===dw?1:0:w,u=B}catch(f){var
u=0}var
z=u?dt:-1003883683;c2[1]=z;continue}if(dt<=s){var
c=new
bN();c.push("<",dG);a4(q,function(a){c.push(' type="',dl(a),bf);return 0});a4(o,function(a){c.push(' name="',dl(a),bf);return 0});c.push(">");var
b=g.createElement(c.join(d))}else{var
i=bP(g,c4);a4(q,function(a){return i.type=a});a4(o,function(a){return i.name=a});var
b=i}}var
y=c3(g);Q(y,hH);p(b,y);am(function(a){var
d=a[2],c=c3(g);Q(c,d);return p(b,c)},n);b.onchange=F(function(a){var
d=b.selectedIndex-1|0;if(0<=d){var
i=0,f=n;for(;;){if(f){var
i=i+1|0,f=f[2];continue}if(d<i){if(0<=d){var
c=n,k=d;for(;;){if(c){var
r=c[2],s=c[1];if(0!==k){var
c=r,k=k-1|0;continue}var
o=s}else
var
o=M(ec);break}}else
var
o=aa(ed);var
u=o[1];E(u,function(a){var
v=[0,0],b=[0,0],z=a.getLen()-1|0,O=0;if(!(z<0)){var
r=O;for(;;){var
d=a.safeGet(r);if(47<=d)if(83<=d)if(89<=d)var
f=0;else{switch(d+ds|0){case
0:b[1]=[0,6,b[1]];var
s=1;break;case
4:b[1]=[0,6,b[1]];var
s=1;break;case
5:b[1]=[0,3,b[1]];var
s=1;break;default:var
f=0,s=0}if(s)var
f=1}else
var
f=69===d?(b[1]=[0,4,b[1]],1):0;else
if(10===d){var
X=v[1];v[1]=[0,al(b[1]),X];b[1]=0;var
f=1}else
if(32<=d){switch(d+ca|0){case
0:b[1]=[0,0,b[1]];var
n=1;break;case
3:b[1]=[0,7,b[1]];var
n=1;break;case
11:b[1]=[0,2,b[1]];var
n=1;break;case
14:b[1]=[0,1,b[1]];var
n=1;break;default:var
f=0,n=0}if(n)var
f=1}else
var
f=0;if(!f)M(hB);var
W=r+1|0;if(z!==r){var
r=W;continue}break}}var
w=co(ac(co,al(v[1])));function
P(a){var
b=ap(g,fE);b.src=dd(a);return b}var
q=cn(function(a){return cn(P,a)},w),B=[0,0],C=[0,0],E=[0,0],F=[0,0],G=[0,0],Q=e(hx),T=[0,e(hy)],k=ap(g,fF);bV(k,[0,Q]);var
x=q.length-1-1|0,U=0,I=0;if(!(x<0)){var
c=I;for(;;){var
o=k.insertRow(-1);bV(o,U);var
y=h(q,c).length-1-1|0,J=0;if(!(y<0)){var
i=J;for(;;){var
u=o.insertCell(-1);bV(u,T);var
L=h(h(q,c),i);switch(h(h(w,c),i)){case
2:G[1]++;break;case
4:E[1]=i;F[1]=c;break;case
6:B[1]=i;C[1]=c;break}p(u,L);p(o,u);var
N=i+1|0;if(y!==i){var
i=N;continue}break}}p(k,o);var
K=c+1|0;if(x!==c){var
c=K;continue}break}}bS(D,k);function
V(a){var
b=aJ(0);function
c(a){var
d=aJ(0);if(1<=d-b){var
f=k.style;f.opacity=ao(e(hz));return l(0)}function
g(a){var
f=k.style;f.opacity=ao(e(j(A(hA),d-b)));return c(0)}return m(a5(cd),g)}function
d(a){j(R,0);return l(0)}return m(c(0),d)}var
H=[0,0,bE(0)];return m(bU([0,w,q,[0,B[1],C[1]],[0,E[1],F[1]],G[1],0,H,0,[0,0]],S,t),V)});var
q=1}else
var
q=0;break}}else
var
q=0;return a3});p(a,b);p(a,c5(g));p(a,c5(g));p(a,D);p(f,a);return l(0)}}m(E(hL,function(d){function
e(a){var
f=d.getLen(),b=a;for(;;){if(f<=b)return M(hI);if(34===d.safeGet(b)){var
e=b+1|0,c=b+2|0;for(;;){if(f<=e)return M(hJ);if(34===d.safeGet(c))return[0,W(d,e,c-e|0),c+1|0];var
c=c+1|0;continue}}var
b=b+1|0;continue}}var
f=0,a=0;for(;;){try{var
h=e(f),j=h[1],i=e(h[2]),k=[0,[0,[0,j,i[1]],i[2]]],b=k}catch(f){f=y(f);if(f[1]===av)if(C(f[2],hK))var
c=0;else
var
b=0,c=1;else
var
c=0;if(!c)throw f}if(b){var
g=b[1],f=g[2],a=[0,g[1],a];continue}return l(al(a))}}),x);return a3});bu(0);return}(this));
