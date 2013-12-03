// This program was compiled from OCaml by js_of_ocaml 1.99dev
(function(d){"use strict";var
bN=125,bR=123,_=255,dh="x",a4=".",bW=108,ax="+",a3=65535,G=16777215,bO="g",bM="f",dl=250,aa=105,dg="%d",df=443,az=110,dj=785140586,dd=-550809787,aA=115,a1="int_of_string",bU=102,de=982028505,bS=111,bQ=120,F=" ",dp="1",ay="e",db=0.001,a6=891486873,bP=117,$="-",a7=126925477,z="",bL=116,a0=781515420,a5=100,bV=0.05,D="0",a2=114,bT=103,dc=936573133,di="#",dn=101,dm="index out of bounds",y="number",dk=1e3;function
hV(a,b){if(typeof
b==="function"){a.fun=b;return 0}if(b.fun){a.fun=b.fun;return 0}var
c=b.length;while(c--)a[c]=b[c];return 0}var
hj=new
Date()*db;function
hU(){return new
Date()*db-hj}function
hT(){return[0,new
H("Unix"),32,0]}function
hS(){a_("Function 'exit' not implemented")}function
hR(a,b){return 1-hQ(a,b)}function
hQ(a,b){var
c=a.fullBytes,d=b.fullBytes;if(c!=null&&d!=null)return c==d?1:0;return a.getFullBytes()==b.getFullBytes()?1:0}function
hP(a,b){hI[a]=b;return 0}var
hI={};function
hO(a,b){bZ[a+1]=b}function
hL(a){return a
instanceof
Array?a[0]:dk}function
hK(a){return+(a
instanceof
Array)}function
hJ(a,b){return+(bX(a,b,false)!=0)}function
hW(a,b){return bX(a,b,true)}function
hH(a,b){return((a>>16)*b<<16)+(a&a3)*b|0}function
hG(a,b){var
c=dx(String.fromCharCode(b));dw(a,c,0,1)}function
dx(a){return new
P(a)}function
dw(a,b,c,d){var
f;if(c==0&&b.getLen()==d)f=b;else{f=ds(d);dr(b,c,f,0,d)}var
e=f.toString(),g=e.lastIndexOf("\n");if(g<0)ag+=e;else{ag+=e.substr(0,g);dv(a);ag+=e.substr(g+1)}}function
hF(){return 0}function
hE(a){return a}function
dv(a){d.console&&d.console.log&&ag!=z&&d.console.log(ag);ag=z}var
ag=z;function
hD(a,b){var
d=[0];for(var
c=1;c<=a;c++)d[c]=b;return d}function
hC(a){var
c=Array.prototype.slice;return function(){var
b=arguments.length>0?c.call(arguments):[undefined];return S(a,b)}}function
hB(a){return new
P(a)}function
hA(){var
a=d.navigator?d.navigator.userAgent:z;return a.indexOf("MSIE")!=-1&&a.indexOf("Opera")!=0}var
a$={amp:/&/g,lt:/</g,quot:/\"/g,all:/[&<\"]/};function
hz(a){if(!a$.all.test(a))return a;return a.replace(a$.amp,"&amp;").replace(a$.lt,"&lt;").replace(a$.quot,"&quot;")}function
hy(){var
b=d.console?d.console:{},c=["log","debug","info","warn","error","assert","dir","dirxml","trace","group","groupCollapsed","groupEnd","time","timeEnd"];function
e(){}for(var
a=0;a<c.length;a++)if(!b[c[a]])b[c[a]]=e;return b}function
hx(a){return a.getFullBytes()}function
hw(a){return+(a>31&&a<127)}function
hv(a){var
g=hM(a),e=g[0],h=g[1],f=g[2],i=-1>>>0,d=a.get(e),c=dy(d);if(c<0||c>=f)a9(a1);var
b=c;for(;;){e++;d=a.get(e);if(d==95)continue;c=dy(d);if(c<0||c>=f)break;b=f*b+c;if(b>i)a9(a1)}if(e!=a.getLen())a9(a1);b=h*b;if((b|0)!=b)a9(a1);return b}function
a9(a){dz(bZ[3],a)}var
bZ=[0];function
dy(a){if(a>=48&&a<=57)return a-48;if(a>=65&&a<=90)return a-55;if(a>=97&&a<=122)return a-87;return-1}function
hM(a){var
b=0,c=10,d=a.get(0)==45?(b++,-1):1;if(a.get(b)==48)switch(a.get(b+1)){case
bQ:case
88:c=16;b+=2;break;case
bS:case
79:c=8;b+=2;break;case
98:case
66:c=2;b+=2;break}return[b,d,c]}function
hl(a,b){var
c=b0(a);if(c.signedconv&&hm(b)){c.sign=-1;b=hp(b)}var
d=z,h=hq(c.base),g="0123456789abcdef";do{var
f=ht(b,h);b=f[1];d=g.charAt(hs(f[2]))+d}while(!hn(b));if(c.prec>=0){c.filler=F;var
e=c.prec-d.length;if(e>0)d=aB(e,D)+d}return bY(c,d)}function
hp(a){var
b=-a[1],c=-a[2]+(b>>24),d=-a[3]+(c>>24);return[_,b&G,c&G,d&a3]}function
hm(a){return a[3]<<16<0}function
hs(a){return a[1]|a[2]<<24}function
ht(a,b){var
e=0,d=a.slice(),c=b.slice(),f=[_,0,0,0];while(du(d,c)>0){e++;dt(c)}while(e>=0){e--;dt(f);if(du(d,c)>=0){f[1]++;d=hr(d,c)}ho(c)}return[0,f,d]}function
ho(a){a[1]=(a[1]>>>1|a[2]<<23)&G;a[2]=(a[2]>>>1|a[3]<<23)&G;a[3]=a[3]>>>1}function
dt(a){a[3]=a[3]<<1|a[2]>>23;a[2]=(a[2]<<1|a[1]>>23)&G;a[1]=a[1]<<1&G}function
du(a,b){if(a[3]>b[3])return 1;if(a[3]<b[3])return-1;if(a[2]>b[2])return 1;if(a[2]<b[2])return-1;if(a[1]>b[1])return 1;if(a[1]<b[1])return-1;return 0}function
hr(a,b){var
c=a[1]-b[1],d=a[2]-b[2]+(c>>24),e=a[3]-b[3]+(d>>24);return[_,c&G,d&G,e&a3]}function
hq(a){return[_,a&G,a>>24&G,a>>31&a3]}function
hn(a){return(a[3]|a[2]|a[1])==0}function
hi(){return 0}function
hh(a,b){if(a.toString()==dg)return new
H(z+b);var
c=b0(a);if(b<0){if(c.signedconv){c.sign=-1;b=-b}else
b>>>=0}var
d=b.toString(c.base);if(c.prec>=0){c.filler=F;var
e=c.prec-d.length;if(e>0)d=aB(e,D)+d}return bY(c,d)}function
hg(a,b){var
c,f=b0(a),e=f.prec<0?6:f.prec;if(b<0){f.sign=-1;b=-b}if(isNaN(b)){c="nan";f.filler=F}else
if(!isFinite(b)){c="inf";f.filler=F}else
switch(f.conv){case
ay:var
c=b.toExponential(e),d=c.length;if(c.charAt(d-3)==ay)c=c.slice(0,d-1)+D+c.slice(d-1);break;case
bM:c=b.toFixed(e);break;case
bO:e=e?e:1;c=b.toExponential(e-1);var
i=c.indexOf(ay),h=+c.slice(i+1);if(h<-4||b.toFixed(0).length>e){var
d=i-1;while(c.charAt(d)==D)d--;if(c.charAt(d)==a4)d--;c=c.slice(0,d+1)+c.slice(i);d=c.length;if(c.charAt(d-3)==ay)c=c.slice(0,d-1)+D+c.slice(d-1);break}else{var
g=e;if(h<0){g-=h+1;c=b.toFixed(g)}else
while(c=b.toFixed(g),c.length>e+1)g--;if(g){var
d=c.length-1;while(c.charAt(d)==D)d--;if(c.charAt(d)==a4)d--;c=c.slice(0,d+1)}}break}return bY(f,c)}function
bY(a,b){if(a.uppercase)b=b.toUpperCase();var
e=b.length;if(a.signedconv&&(a.sign<0||a.signstyle!=$))e++;if(a.alternate){if(a.base==8)e+=1;if(a.base==16)e+=2}var
c=z;if(a.justify==ax&&a.filler==F)for(var
d=e;d<a.width;d++)c+=F;if(a.signedconv){if(a.sign<0)c+=$;else
if(a.signstyle!=$)c+=a.signstyle}if(a.alternate&&a.base==8)c+=D;if(a.alternate&&a.base==16)c+="0x";if(a.justify==ax&&a.filler==D)for(var
d=e;d<a.width;d++)c+=D;c+=b;if(a.justify==$)for(var
d=e;d<a.width;d++)c+=F;return new
H(c)}function
b0(a){a=a.toString();var
e=a.length;if(e>31)a_("format_int: format too long");var
b={justify:ax,signstyle:$,filler:F,alternate:false,base:0,signedconv:false,width:0,uppercase:false,sign:1,prec:-1,conv:bM};for(var
d=0;d<e;d++){var
c=a.charAt(d);switch(c){case
$:b.justify=$;break;case
ax:case
F:b.signstyle=c;break;case
D:b.filler=D;break;case
di:b.alternate=true;break;case
dp:case"2":case"3":case"4":case"5":case"6":case"7":case"8":case"9":b.width=0;while(c=a.charCodeAt(d)-48,c>=0&&c<=9){b.width=b.width*10+c;d++}d--;break;case
a4:b.prec=0;d++;while(c=a.charCodeAt(d)-48,c>=0&&c<=9){b.prec=b.prec*10+c;d++}d--;case"d":case"i":b.signedconv=true;case"u":b.base=10;break;case
dh:b.base=16;break;case"X":b.base=16;b.uppercase=true;break;case"o":b.base=8;break;case
ay:case
bM:case
bO:b.signedconv=true;b.conv=c;break;case"E":case"F":case"G":b.signedconv=true;b.uppercase=true;b.conv=c.toLowerCase();break}}return b}function
hf(a,b,c,d){a.fill(b,c,d)}function
he(a,b){return+(bX(a,b,false)==0)}function
bX(a,b,c){var
e=[];for(;;){if(!(c&&a===b)){if(a
instanceof
P){if(b
instanceof
P){if(a!=b){var
d=a.compare(b);if(d!=0)return d}}else
return 1}else
if(a
instanceof
Array&&a[0]===(a[0]|0)){var
g=a[0];if(g===dl){a=a[1];continue}else
if(b
instanceof
Array&&b[0]===(b[0]|0)){var
h=b[0];if(h===dl){b=b[1];continue}else
if(g!=h){return g<h?-1:1}else{switch(g){case
248:{var
d=hu(a[2],b[2]);if(d!=0)return d;break}case
_:{var
d=hk(a,b);if(d!=0)return d;break}default:if(a.length!=b.length)return a.length<b.length?-1:1;if(a.length>1)e.push(a,b,1)}}}else
return 1}else
if(b
instanceof
P||b
instanceof
Array&&b[0]===(b[0]|0)){return-1}else{if(a<b)return-1;if(a>b)return 1;if(c&&a!=b){if(a==a)return 1;if(b==b)return-1}}}if(e.length==0)return 0;var
f=e.pop();b=e.pop();a=e.pop();if(f+1<a.length)e.push(a,b,f+1);a=a[f];b=b[f]}}function
hu(a,b){if(a<b)return-1;if(a==b)return 0;return 1}function
hk(a,b){var
c=a[3]<<16,d=b[3]<<16;if(c>d)return 1;if(c<d)return-1;if(a[2]>b[2])return 1;if(a[2]<b[2])return-1;if(a[1]>b[1])return 1;if(a[1]<b[1])return-1;return 0}function
ds(a){if(a<0)a_("String.create");return new
dq(a)}function
hd(a){if(isFinite(a)){if(Math.abs(a)>=2.22507385850720138e-308)return 0;if(a!=0)return 1;return 2}return isNaN(a)?4:3}function
S(c,b){if(c.fun)return S(c.fun,b);var
a=c.length,d=a-b.length;if(d==0)return c.apply(null,b);else
if(d<0)return S(c.apply(null,b.slice(0,a)),b.slice(a));else
return function(a){return S(c,b.concat([a]))}}function
dr(a,b,c,d,e){if(e===0)return;if(d===c.last&&c.bytes!=null){var
f=a.bytes;if(f==null)f=a.toBytes();if(b>0||a.last>e)f=f.slice(b,b+e);c.bytes+=f;c.last+=f.length;return}var
g=c.array;if(!g)g=c.toArray();else{c.bytes=c.string=null}a.blitToArray(b,g,d,e)}function
hc(a,b,c){if(b<0||b>=a.length-1)a8();a[b+1]=c;return 0}function
hb(a,b){if(b<0||b>=a.length-1)a8();return a[b+1]}function
aB(a,b){if(!a){return z}if(a&1){return aB(a-1,b)+b}var
c=aB(a>>1,b);return c+c}function
P(a){if(a!=null){this.bytes=this.fullBytes=a;this.last=this.len=a.length}}P.prototype={string:null,bytes:null,fullBytes:null,array:null,len:null,last:0,toJsString:function(){var
a=this.getFullBytes();try{return this.string=decodeURIComponent(escape(a))}catch(f){d.console&&d.console.error&&d.console.error('MlString.toJsString: wrong encoding for \"%s\" ',a);return a}},toBytes:function(){if(this.string!=null){try{var
a=unescape(encodeURIComponent(this.string))}catch(f){d.console&&d.console.error&&d.console.error('MlString.toBytes: wrong encoding for \"%s\" ',this.string);var
a=this.string}}else{var
a=z,c=this.array,e=c.length;for(var
b=0;b<e;b++)a+=String.fromCharCode(c[b])}this.bytes=this.fullBytes=a;this.last=this.len=a.length;return a},getBytes:function(){var
a=this.bytes;if(a==null)a=this.toBytes();return a},getFullBytes:function(){var
a=this.fullBytes;if(a!==null)return a;a=this.bytes;if(a==null)a=this.toBytes();if(this.last<this.len){this.bytes=a+=aB(this.len-this.last,"\0");this.last=this.len}this.fullBytes=a;return a},toArray:function(){var
c=this.bytes;if(c==null)c=this.toBytes();var
b=[],d=this.last;for(var
a=0;a<d;a++)b[a]=c.charCodeAt(a);for(d=this.len;a<d;a++)b[a]=0;this.string=this.bytes=this.fullBytes=null;this.last=this.len;this.array=b;return b},getArray:function(){var
a=this.array;if(!a)a=this.toArray();return a},getLen:function(){var
a=this.len;if(a!==null)return a;this.toBytes();return this.len},toString:function(){var
a=this.string;return a?a:this.toJsString()},valueOf:function(){var
a=this.string;return a?a:this.toJsString()},blitToArray:function(a,b,c,d){var
g=this.array;if(g){if(c<=a){for(var
e=0;e<d;e++)b[c+e]=g[a+e]}else{for(var
e=d-1;e>=0;e--)b[c+e]=g[a+e]}}else{var
f=this.bytes;if(f==null)f=this.toBytes();var
h=this.last-a;if(d<=h)for(var
e=0;e<d;e++)b[c+e]=f.charCodeAt(a+e);else{for(var
e=0;e<h;e++)b[c+e]=f.charCodeAt(a+e);for(;e<d;e++)b[c+e]=0}}},get:function(a){var
c=this.array;if(c)return c[a];var
b=this.bytes;if(b==null)b=this.toBytes();return a<this.last?b.charCodeAt(a):0},safeGet:function(a){if(this.len==null)this.toBytes();if(a<0||a>=this.len)a8();return this.get(a)},set:function(a,b){var
c=this.array;if(!c){if(this.last==a){this.bytes+=String.fromCharCode(b&_);this.last++;return 0}c=this.toArray()}else
if(this.bytes!=null){this.bytes=this.fullBytes=this.string=null}c[a]=b&_;return 0},safeSet:function(a,b){if(this.len==null)this.toBytes();if(a<0||a>=this.len)a8();this.set(a,b)},fill:function(a,b,c){if(a>=this.last&&this.last&&c==0)return;var
d=this.array;if(!d)d=this.toArray();else
if(this.bytes!=null){this.bytes=this.fullBytes=this.string=null}var
f=a+b;for(var
e=a;e<f;e++)d[e]=c},compare:function(a){if(this.string!=null&&a.string!=null){if(this.string<a.string)return-1;if(this.string>a.string)return 1;return 0}var
b=this.getFullBytes(),c=a.getFullBytes();if(b<c)return-1;if(b>c)return 1;return 0},equal:function(a){if(this.string!=null&&a.string!=null)return this.string==a.string;return this.getFullBytes()==a.getFullBytes()},lessThan:function(a){if(this.string!=null&&a.string!=null)return this.string<a.string;return this.getFullBytes()<a.getFullBytes()},lessEqual:function(a){if(this.string!=null&&a.string!=null)return this.string<=a.string;return this.getFullBytes()<=a.getFullBytes()}};function
H(a){this.string=a}H.prototype=new
P();function
dq(a){this.bytes=z;this.len=a}dq.prototype=new
P();function
ha(a){var
b=a.length;this.array=a;this.len=this.last=b}ha.prototype=new
P();function
a8(){a_(dm)}function
a_(a){dz(bZ[4],a)}function
dz(a,b){hN(a,new
H(b))}function
hN(a,b){throw[0,a,b]}var
c=z,aV="\r\n",c4=F,aS='"',bE=di,cZ="&",cV="'",cW="''",bG="--",cU=a4,av="/",cY=":",bF="=",aT="?",c0='Content-Disposition: form-data; name="',aU="POST",bH="eos",g$="false",bK=bO,cX="select",c2="sprites/boulder.png",c3="sprites/end.png",c1="sprites/guy.png",a="src/core/lwt.ml",g_="true",h=hb,k=hc,Z=dr,N=ds,c$=he,bI=hg,aX=hh,c8=hv,bJ=hw,o=hx,da=hz,R=hB,aZ=hC,O=hD,c5=dv,c6=hG,c7=hH,b=dx,c9=hL,aW=hO,x=hR,aw=hU,c_=hV;function
j(a,b){return a.length==1?a(b):S(a,[b])}function
n(a,b,c){return a.length==2?a(b,c):S(a,[b,c])}function
r(a,b,c,d){return a.length==3?a(b,c,d):S(a,[b,c,d])}function
aY(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):S(a,[b,c,d,e,f])}var
ah=[0,b("Failure")],ba=[0,b("Invalid_argument")],bc=[0,b("Not_found")],s=[0,b("Assert_failure")],bj=b('File "%s", line %d, characters %d-%d: %s'),cI=b(cX);aW(6,bc);aW(5,[0,b("Division_by_zero")]);aW(3,ba);aW(2,ah);var
em=[0,b("Out_of_memory")],eq=[0,b("Match_failure")],eo=[0,b("Stack_overflow")],et=[0,b("Undefined_recursive_module")],dD=b("%.12g"),dC=b(cU),dA=b(g_),dB=b(g$),dE=b("Pervasives.do_at_exit"),dG=b("nth"),dH=b("List.nth"),dK=b("\\b"),dL=b("\\t"),dM=b("\\n"),dN=b("\\r"),dJ=b("\\\\"),dI=b("\\'"),dQ=b(c),dP=b("String.blit"),dO=b("String.sub"),dT=b("Queue.Empty"),dV=b("Buffer.add: cannot grow buffer"),d8=b(c),d9=b(c),ea=b(aS),eb=b(aS),d_=b(cV),d$=b(cV),d7=b(cU),d6=b("printf: bad positional specification (0)."),d5=b("%_"),d4=[0,b("printf.ml"),144,8],d2=b(cW),d3=b("Printf: premature end of format string ``"),dY=b(cW),dZ=b(" in format string ``"),d0=b(", at char number "),d1=b("Printf: bad conversion %"),dW=b("Sformat.index_of_int: negative argument "),eg=b(c),eh=b(", %s%s"),eB=[1,1],eC=b("%s\n"),eD=b("(Program not linked with -g, cannot print stack backtrace)\n"),ev=b("Raised at"),ey=b("Re-raised at"),ez=b("Raised by primitive operation at"),eA=b("Called from"),ew=b('%s file "%s", line %d, characters %d-%d'),ex=b("%s unknown location"),en=b("Out of memory"),ep=b("Stack overflow"),er=b("Pattern matching failed"),es=b("Assertion failed"),eu=b("Undefined recursive module"),ei=b("(%s%s)"),ej=b(c),ek=b(c),el=b("(%s)"),ef=b(dg),ed=b("%S"),ee=b("_"),eE=b("Lwt_sequence.Empty"),eS=[0,b(a),692,20],eT=[0,b(a),695,8],eQ=[0,b(a),670,20],eR=[0,b(a),673,8],eO=[0,b(a),648,20],eP=[0,b(a),651,8],eM=[0,b(a),498,8],eL=[0,b(a),487,9],eK=b("Lwt.wakeup_later_result"),eJ=b("Lwt.wakeup_result"),eI=b("Fatal error: exception "),eG=b("Lwt.Canceled"),eN=[0,0],e5=b("table"),e4=b("img"),e3=b("br"),e2=b("h1"),e1=b("div"),e0=b("option"),e9=b("browser can't read file: unimplemented"),e8=[0,b("file.ml"),131,15],e6=b("can't retrieve file name: not implemented"),e$=b("Exception during Lwt.async: "),fb=b("[\\][()\\\\|+*.?{}^$]"),fo=[0,b(c),0],fp=b(c),fC=b(c),fD=b(bE),fL=b(c),fE=b(aT),fK=b(c),fF=b(av),fG=b(av),fJ=b(cY),fH=b(c),fI=b("http://"),fM=b(c),fN=b(bE),fV=b(c),fO=b(aT),fU=b(c),fP=b(av),fQ=b(av),fT=b(cY),fR=b(c),fS=b("https://"),fW=b(c),fX=b(bE),f2=b(c),fY=b(aT),f1=b(c),fZ=b(av),f0=b("file://"),fB=b(c),fA=b(c),fz=b(c),fy=b(c),fx=b(c),fw=b(c),fq=b(bF),fr=b(cZ),fi=b("file"),fj=b("file:"),fk=b("http"),fl=b("http:"),fm=b("https"),fn=b("https:"),ff=b("%2B"),fd=b("Url.Local_exn"),fe=b(ax),fg=b("Url.Not_an_http_protocol"),fs=b("^([Hh][Tt][Tt][Pp][Ss]?)://([0-9a-zA-Z.-]+|\\[[0-9a-zA-Z.-]+\\]|\\[[0-9A-Fa-f:.]+\\])?(:([0-9]+))?/([^\\?#]*)(\\?([^#]*))?(#(.*))?$"),fu=b("^([Ff][Ii][Ll][Ee])://([^\\?#]*)(\\?([^#]*))?(#(.*))?$"),gh=b(aU),gj=b("multipart/form-data; boundary="),gk=b(aU),gl=[0,b(aU),[0,b("application/x-www-form-urlencoded")],a7],gm=[0,b(aU),0,a7],gn=b("GET"),gi=b(aT),gc=b(bF),gd=b(bF),ge=b(cZ),f_=b('"; filename="'),f$=b(c0),f8=b(aV),f9=b(bG),ga=b('"\r\n\r\n'),gb=b(c0),f6=b("--\r\n"),f7=b(bG),f5=b("js_of_ocaml-------------------"),f4=[0,b("xmlHttpRequest.ml"),85,2],gf=b("XmlHttpRequest.Wrong_headers"),gF=b(c1),gG=b(c2),gE=b(dm),gH=b("YOU WIN !"),gI=b("YOU LOSE !"),gJ=b(c3),gK=b("sprites/R.png"),gL=b("sprites/L.png"),gM=b("sprites/U.png"),gN=b("sprites/D.png"),gO=b("sprites/push_r.png"),gP=b("sprites/bR.png"),gQ=b("sprites/push_l.png"),gR=b("sprites/bL.png"),g8=b(bH),g6=b(bH),g7=b(bH),gY=b("%g"),gX=b(dp),gZ=b("malformed level"),gV=b("border-collapse:collapse;line-height: 0; opacity: 0; margin-left:auto; margin-right:auto"),gW=b("padding: 0; width: 20px; height: 20px;"),g0=b("font-family: sans-serif; text-align: center; background-color: #e8e8e8;"),g1=b("Boulder Dash in Ocaml"),g2=b("Elapsed time: "),g3=b(" Remaining diamonds: "),g4=b(c4),g5=b("Choose a level"),gS=[0,b("boulderdash.ml"),294,17],gT=b("boulderdash"),gU=b(bG),g9=b("maps.txt"),gv=b("sprites/empty.png"),gw=b("sprites/grass.png"),gx=b("sprites/diamond.png"),gy=b(c2),gz=b("sprites/door.png"),gA=b(c3),gB=b(c1),gC=b("sprites/wall.png"),gD=b("sprites/bam.png"),gt=b("%02d:%02d:%02d"),gs=b("--:--:--"),gr=b("LOADING..."),go=b("border: 1px black solid; background-color: white ; display: inline ; padding-right: .5em; padding-left: .5em;"),gp=b("background-color: red; color: white; display:inline; position: absolute; top:0; right:0;"),gu=b("Boulderdash.Death");function
I(a){throw[0,ah,a]}function
T(a){throw[0,ba,a]}function
i(a,b){var
c=a.getLen(),e=b.getLen(),d=N(c+e|0);Z(a,0,d,0,c);Z(b,0,d,c,e);return d}function
U(a){return b(z+a)}function
b1(a){var
c=bI(dD,a),b=0,f=c.getLen();for(;;){if(f<=b)var
e=i(c,dC);else{var
d=c.safeGet(b),g=48<=d?58<=d?0:1:45===d?1:0;if(g){var
b=b+1|0;continue}var
e=c}return e}}function
b2(a,b){if(a){var
c=a[1];return[0,c,b2(a[2],b)]}return b}var
ai=hE(2);function
b3(a,b){return dw(a,b,0,b.getLen())}function
b4(a){return b3(ai,a)}function
bb(a){var
b=hF(0);for(;;){if(b){var
c=b[2],d=b[1];try{c5(d)}catch(f){}var
b=c;continue}return 0}}hP(dE,bb);function
dF(a,b){return c6(a,b)}function
b5(a){return c5(a)}function
b6(a,b){var
d=b.length-1;if(0===d)return[0];var
e=O(d,j(a,b[0+1])),f=d-1|0,g=1;if(!(f<1)){var
c=g;for(;;){e[c+1]=j(a,b[c+1]);var
h=c+1|0;if(f!==c){var
c=h;continue}break}}return e}function
b7(a){if(a){var
d=0,c=a,g=a[2],h=a[1];for(;;){if(c){var
d=d+1|0,c=c[2];continue}var
f=O(d,h),e=1,b=g;for(;;){if(b){var
i=b[2];f[e+1]=b[1];var
e=e+1|0,b=i;continue}return f}}}return[0]}function
ab(a){var
b=a,c=0;for(;;){if(b){var
d=[0,b[1],c],b=b[2],c=d;continue}return c}}function
V(a,b){if(b){var
c=b[2],d=j(a,b[1]);return[0,d,V(a,c)]}return 0}function
ac(a,b){var
c=b;for(;;){if(c){var
d=c[2];j(a,c[1]);var
c=d;continue}return 0}}function
aj(a,b){var
c=N(a);hf(c,0,a,b);return c}function
Q(a,b,c){if(0<=b&&0<=c&&!((a.getLen()-c|0)<b)){var
d=N(c);Z(a,b,d,0,c);return d}return T(dO)}function
aC(a,b,c,d,e){if(0<=e&&0<=b&&!((a.getLen()-e|0)<b)&&0<=d&&!((c.getLen()-e|0)<d))return Z(a,b,c,d,e);return T(dP)}function
ak(d,b){if(b){var
a=b[1],g=[0,0],f=[0,0],h=b[2];ac(function(a){g[1]++;f[1]=f[1]+a.getLen()|0;return 0},b);var
e=N(f[1]+c7(d.getLen(),g[1]-1|0)|0);Z(a,0,e,0,a.getLen());var
c=[0,a.getLen()];ac(function(a){Z(d,0,e,c[1],d.getLen());c[1]=c[1]+d.getLen()|0;Z(a,0,e,c[1],a.getLen());c[1]=c[1]+a.getLen()|0;return 0},h);return e}return dQ}var
bd=hT(0)[2],al=c7(bd/8|0,(1<<(bd-10|0))-1|0)-1|0,dR=252,dS=253,dU=[0,dT];function
be(a){var
b=1<=a?a:1,c=al<b?al:b,d=N(c);return[0,d,0,c,d]}function
bf(a){return Q(a[1],0,a[2])}function
b8(a,b){var
c=[0,a[3]];for(;;){if(c[1]<(a[2]+b|0)){c[1]=2*c[1]|0;continue}if(al<c[1])if((a[2]+b|0)<=al)c[1]=al;else
I(dV);var
d=N(c[1]);aC(a[1],0,d,0,a[2]);a[1]=d;a[3]=c[1];return 0}}function
am(a,b){var
c=a[2];if(a[3]<=c)b8(a,1);a[1].safeSet(c,b);a[2]=c+1|0;return 0}function
bg(a,b){var
c=b.getLen(),d=a[2]+c|0;if(a[3]<d)b8(a,c);aC(b,0,a[1],a[2],c);a[2]=d;return 0}function
bh(a){return 0<=a?a:I(i(dW,U(a)))}function
b9(a,b){return bh(a+b|0)}var
dX=1;function
b_(a){return b9(dX,a)}function
b$(a){return Q(a,0,a.getLen())}function
ca(a,b,c){var
d=i(dZ,i(a,dY)),e=i(d0,i(U(b),d));return T(i(d1,i(aj(1,c),e)))}function
an(a,b,c){return ca(b$(a),b,c)}function
aD(a){return T(i(d3,i(b$(a),d2)))}function
W(e,b,c,d){function
h(a){if((e.safeGet(a)-48|0)<0||9<(e.safeGet(a)-48|0))return a;var
b=a+1|0;for(;;){var
c=e.safeGet(b);if(48<=c){if(!(58<=c)){var
b=b+1|0;continue}var
d=0}else
if(36===c){var
f=b+1|0,d=1}else
var
d=0;if(!d)var
f=a;return f}}var
i=h(b+1|0),f=be((c-i|0)+10|0);am(f,37);var
a=i,g=ab(d);for(;;){if(a<=c){var
j=e.safeGet(a);if(42===j){if(g){var
k=g[2];bg(f,U(g[1]));var
a=h(a+1|0),g=k;continue}throw[0,s,d4]}am(f,j);var
a=a+1|0;continue}return bf(f)}}function
cb(a,b,c,d,e){var
f=W(b,c,d,e);if(78!==a&&az!==a)return f;f.safeSet(f.getLen()-1|0,bP);return f}function
cc(a){return function(c,b){var
m=c.getLen();function
n(a,b){var
o=40===a?41:bN;function
k(a){var
d=a;for(;;){if(m<=d)return aD(c);if(37===c.safeGet(d)){var
e=d+1|0;if(m<=e)var
f=aD(c);else{var
g=c.safeGet(e),h=g-40|0;if(h<0||1<h){var
l=h-83|0;if(l<0||2<l)var
j=1;else
switch(l){case
1:var
j=1;break;case
2:var
i=1,j=0;break;default:var
i=0,j=0}if(j){var
f=k(e+1|0),i=2}}else
var
i=0===h?0:1;switch(i){case
1:var
f=g===o?e+1|0:an(c,b,g);break;case
2:break;default:var
f=k(n(g,e+1|0)+1|0)}}return f}var
d=d+1|0;continue}}return k(b)}return n(a,b)}}function
cd(i,b,c){var
l=i.getLen()-1|0;function
s(a){var
k=a;a:for(;;){if(k<l){if(37===i.safeGet(k)){var
e=0,h=k+1|0;for(;;){if(l<h)var
w=aD(i);else{var
m=i.safeGet(h);if(58<=m){if(95===m){var
e=1,h=h+1|0;continue}}else
if(32<=m)switch(m-32|0){case
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
h=r(b,e,h,aa);continue;default:var
h=h+1|0;continue}var
d=h;b:for(;;){if(l<d)var
f=aD(i);else{var
j=i.safeGet(d);if(126<=j)var
g=0;else
switch(j){case
78:case
88:case
a5:case
aa:case
bS:case
bP:case
bQ:var
f=r(b,e,d,aa),g=1;break;case
69:case
70:case
71:case
dn:case
bU:case
bT:var
f=r(b,e,d,bU),g=1;break;case
33:case
37:case
44:case
64:var
f=d+1|0,g=1;break;case
83:case
91:case
aA:var
f=r(b,e,d,aA),g=1;break;case
97:case
a2:case
bL:var
f=r(b,e,d,j),g=1;break;case
76:case
bW:case
az:var
t=d+1|0;if(l<t){var
f=r(b,e,d,aa),g=1}else{var
p=i.safeGet(t)-88|0;if(p<0||32<p)var
q=1;else
switch(p){case
0:case
12:case
17:case
23:case
29:case
32:var
f=n(c,r(b,e,d,j),aa),g=1,q=0;break;default:var
q=1}if(q){var
f=r(b,e,d,aa),g=1}}break;case
67:case
99:var
f=r(b,e,d,99),g=1;break;case
66:case
98:var
f=r(b,e,d,66),g=1;break;case
41:case
bN:var
f=r(b,e,d,j),g=1;break;case
40:var
f=s(r(b,e,d,j)),g=1;break;case
bR:var
u=r(b,e,d,j),v=n(cc(j),i,u),o=u;for(;;){if(o<(v-2|0)){var
o=n(c,o,i.safeGet(o));continue}var
d=v-1|0;continue b}default:var
g=0}if(!g)var
f=an(i,d,j)}var
w=f;break}}var
k=w;continue a}}var
k=k+1|0;continue}return k}}s(0);return 0}function
ce(a){var
d=[0,0,0,0];function
b(a,b,c){var
f=41!==c?1:0,g=f?bN!==c?1:0:f;if(g){var
e=97===c?2:1;if(a2===c)d[3]=d[3]+1|0;if(a)d[2]=d[2]+e|0;else
d[1]=d[1]+e|0}return b+1|0}cd(a,b,function(a,b){return a+1|0});return d[1]}function
cf(a,b,c){var
h=a.safeGet(c);if((h-48|0)<0||9<(h-48|0))return n(b,0,c);var
e=h-48|0,d=c+1|0;for(;;){var
f=a.safeGet(d);if(48<=f){if(!(58<=f)){var
e=(10*e|0)+(f-48|0)|0,d=d+1|0;continue}var
g=0}else
if(36===f)if(0===e){var
i=I(d6),g=1}else{var
i=n(b,[0,bh(e-1|0)],d+1|0),g=1}else
var
g=0;if(!g)var
i=n(b,0,c);return i}}function
u(a,b){return a?b:b_(b)}function
cg(a,b){return a?a[1]:b}function
ch(aL,b,c,af,e,f,g){var
w=j(b,g);function
aM(a,b,l,aN){var
k=l.getLen();function
D(m,b){var
p=b;for(;;){if(k<=p)return j(a,w);var
d=l.safeGet(p);if(37===d){var
o=function(a,b){return h(aN,cg(a,b))},au=function(g,f,c,d){var
a=d;for(;;){var
ab=l.safeGet(a)-32|0;if(!(ab<0||25<ab))switch(ab){case
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
10:return cf(l,function(a,b){var
d=[0,o(a,f),c];return au(g,u(a,f),d,b)},a+1|0);default:var
a=a+1|0;continue}var
q=l.safeGet(a);if(124<=q)var
h=0;else
switch(q){case
78:case
88:case
a5:case
aa:case
bS:case
bP:case
bQ:var
a_=o(g,f),a$=aX(cb(q,l,p,a,c),a_),k=r(u(g,f),a$,a+1|0),h=1;break;case
69:case
71:case
dn:case
bU:case
bT:var
a1=o(g,f),a3=bI(W(l,p,a,c),a1),k=r(u(g,f),a3,a+1|0),h=1;break;case
76:case
bW:case
az:var
ad=l.safeGet(a+1|0)-88|0;if(ad<0||32<ad)var
ag=1;else
switch(ad){case
0:case
12:case
17:case
23:case
29:case
32:var
T=a+1|0,ae=q-bW|0;if(ae<0||2<ae)var
ai=0;else{switch(ae){case
1:var
ai=0,ak=0;break;case
2:var
a9=o(g,f),aE=aX(W(l,p,T,c),a9),ak=1;break;default:var
a8=o(g,f),aE=aX(W(l,p,T,c),a8),ak=1}if(ak){var
aD=aE,ai=1}}if(!ai){var
a7=o(g,f),aD=hl(W(l,p,T,c),a7)}var
k=r(u(g,f),aD,T+1|0),h=1,ag=0;break;default:var
ag=1}if(ag){var
a4=o(g,f),a6=aX(cb(az,l,p,a,c),a4),k=r(u(g,f),a6,a+1|0),h=1}break;case
37:case
64:var
k=r(f,aj(1,q),a+1|0),h=1;break;case
83:case
aA:var
z=o(g,f);if(aA===q)var
A=z;else{var
b=[0,0],ap=z.getLen()-1|0,aP=0;if(!(ap<0)){var
L=aP;for(;;){var
y=z.safeGet(L),bi=14<=y?34===y?1:92===y?1:0:11<=y?13<=y?1:0:8<=y?1:0,aS=bi?2:bJ(y)?1:4;b[1]=b[1]+aS|0;var
aT=L+1|0;if(ap!==L){var
L=aT;continue}break}}if(b[1]===z.getLen())var
aG=z;else{var
m=N(b[1]);b[1]=0;var
aq=z.getLen()-1|0,aQ=0;if(!(aq<0)){var
K=aQ;for(;;){var
x=z.safeGet(K),B=x-34|0;if(B<0||58<B)if(-20<=B)var
U=1;else{switch(B+34|0){case
8:m.safeSet(b[1],92);b[1]++;m.safeSet(b[1],98);var
J=1;break;case
9:m.safeSet(b[1],92);b[1]++;m.safeSet(b[1],bL);var
J=1;break;case
10:m.safeSet(b[1],92);b[1]++;m.safeSet(b[1],az);var
J=1;break;case
13:m.safeSet(b[1],92);b[1]++;m.safeSet(b[1],a2);var
J=1;break;default:var
U=1,J=0}if(J)var
U=0}else
var
U=(B-1|0)<0||56<(B-1|0)?(m.safeSet(b[1],92),b[1]++,m.safeSet(b[1],x),0):1;if(U)if(bJ(x))m.safeSet(b[1],x);else{m.safeSet(b[1],92);b[1]++;m.safeSet(b[1],48+(x/a5|0)|0);b[1]++;m.safeSet(b[1],48+((x/10|0)%10|0)|0);b[1]++;m.safeSet(b[1],48+(x%10|0)|0)}b[1]++;var
aR=K+1|0;if(aq!==K){var
K=aR;continue}break}}var
aG=m}var
A=i(eb,i(aG,ea))}if(a===(p+1|0))var
aF=A;else{var
I=W(l,p,a,c);try{var
V=0,t=1;for(;;){if(I.getLen()<=t)var
ar=[0,0,V];else{var
X=I.safeGet(t);if(49<=X)if(58<=X)var
al=0;else{var
ar=[0,c8(Q(I,t,(I.getLen()-t|0)-1|0)),V],al=1}else{if(45===X){var
V=1,t=t+1|0;continue}var
al=0}if(!al){var
t=t+1|0;continue}}var
Z=ar;break}}catch(f){if(f[1]!==ah)throw f;var
Z=ca(I,0,aA)}var
M=Z[1],C=A.getLen(),aU=Z[2],O=0,aV=32;if(M===C&&0===O){var
_=A,aO=1}else
var
aO=0;if(!aO)if(M<=C)var
_=Q(A,O,C);else{var
Y=aj(M,aV);if(aU)aC(A,O,Y,0,C);else
aC(A,O,Y,M-C|0,C);var
_=Y}var
aF=_}var
k=r(u(g,f),aF,a+1|0),h=1;break;case
67:case
99:var
s=o(g,f);if(99===q)var
ax=aj(1,s);else{if(39===s)var
v=dI;else
if(92===s)var
v=dJ;else{if(14<=s)var
E=0;else
switch(s){case
8:var
v=dK,E=1;break;case
9:var
v=dL,E=1;break;case
10:var
v=dM,E=1;break;case
13:var
v=dN,E=1;break;default:var
E=0}if(!E)if(bJ(s)){var
ao=N(1);ao.safeSet(0,s);var
v=ao}else{var
F=N(4);F.safeSet(0,92);F.safeSet(1,48+(s/a5|0)|0);F.safeSet(2,48+((s/10|0)%10|0)|0);F.safeSet(3,48+(s%10|0)|0);var
v=F}}var
ax=i(d$,i(v,d_))}var
k=r(u(g,f),ax,a+1|0),h=1;break;case
66:case
98:var
aZ=a+1|0,a0=o(g,f)?dA:dB,k=r(u(g,f),a0,aZ),h=1;break;case
40:case
bR:var
S=o(g,f),av=n(cc(q),l,a+1|0);if(bR===q){var
P=be(S.getLen()),as=function(a,b){am(P,b);return a+1|0};cd(S,function(a,b,c){if(a)bg(P,d5);else
am(P,37);return as(b,c)},as);var
aW=bf(P),k=r(u(g,f),aW,av),h=1}else{var
aw=u(g,f),bh=b9(ce(S),aw),k=aM(function(a){return D(bh,av)},aw,S,aN),h=1}break;case
33:j(e,w);var
k=D(f,a+1|0),h=1;break;case
41:var
k=r(f,d8,a+1|0),h=1;break;case
44:var
k=r(f,d9,a+1|0),h=1;break;case
70:var
ac=o(g,f);if(0===c)var
ay=b1(ac);else{var
$=W(l,p,a,c);if(70===q)$.safeSet($.getLen()-1|0,bT);var
H=bI($,ac);if(3<=hd(ac))var
aB=H;else{var
R=0,aY=H.getLen();for(;;){if(aY<=R)var
at=i(H,d7);else{var
G=H.safeGet(R)-46|0,bj=G<0||23<G?55===G?1:0:(G-1|0)<0||21<(G-1|0)?1:0;if(!bj){var
R=R+1|0;continue}var
at=H}var
aB=at;break}}var
ay=aB}var
k=r(u(g,f),ay,a+1|0),h=1;break;case
91:var
k=an(l,a,q),h=1;break;case
97:var
aH=o(g,f),aI=b_(cg(g,f)),aJ=o(0,aI),ba=a+1|0,bb=u(g,aI);if(aL)n(af,w,n(aH,0,aJ));else
n(aH,w,aJ);var
k=D(bb,ba),h=1;break;case
a2:var
k=an(l,a,q),h=1;break;case
bL:var
aK=o(g,f),bc=a+1|0,bd=u(g,f);if(aL)n(af,w,j(aK,0));else
j(aK,w);var
k=D(bd,bc),h=1;break;default:var
h=0}if(!h)var
k=an(l,a,q);return k}},f=p+1|0,g=0;return cf(l,function(a,b){return au(a,m,g,b)},f)}n(c,w,d);var
p=p+1|0;continue}}function
r(a,b,c){n(af,w,b);return D(a,c)}return D(b,0)}var
o=bh(0);function
l(a,b){return aM(f,o,a,b)}var
d=ce(g);if(d<0||6<d){var
m=function(h,b){if(d<=h){var
i=O(d,0),j=function(a,b){return k(i,(d-a|0)-1|0,b)},c=0,a=b;for(;;){if(a){var
e=a[2],f=a[1];if(e){j(c,f);var
c=c+1|0,a=e;continue}j(c,f)}return l(g,i)}}return function(a){return m(h+1|0,[0,a,b])}},a=m(0,0)}else
switch(d){case
1:var
a=function(a){var
b=O(1,0);k(b,0,a);return l(g,b)};break;case
2:var
a=function(a,b){var
c=O(2,0);k(c,0,a);k(c,1,b);return l(g,c)};break;case
3:var
a=function(a,b,c){var
d=O(3,0);k(d,0,a);k(d,1,b);k(d,2,c);return l(g,d)};break;case
4:var
a=function(a,b,c,d){var
e=O(4,0);k(e,0,a);k(e,1,b);k(e,2,c);k(e,3,d);return l(g,e)};break;case
5:var
a=function(a,b,c,d,e){var
f=O(5,0);k(f,0,a);k(f,1,b);k(f,2,c);k(f,3,d);k(f,4,e);return l(g,f)};break;case
6:var
a=function(a,b,c,d,e,f){var
h=O(6,0);k(h,0,a);k(h,1,b);k(h,2,c);k(h,3,d);k(h,4,e);k(h,5,f);return l(g,h)};break;default:var
a=l(g,[0])}return a}function
ci(d){function
e(a){return 0}function
b(a){return d}var
c=0;return function(a){return ch(c,b,dF,b3,b5,e,a)}}function
ec(a){return be(2*a.getLen()|0)}function
v(a){function
b(a){var
b=bf(a);a[2]=0;return b}return ch(1,ec,am,bg,function(a){return 0},b,a)}var
bi=[0,0];function
bk(a,b){var
c=a[b+1];return hK(c)?c9(c)===dR?j(v(ed),c):c9(c)===dS?b1(c):ee:j(v(ef),c)}function
cj(a,b){if(a.length-1<=b)return eg;var
c=cj(a,b+1|0),d=bk(a,b);return n(v(eh),d,c)}function
ck(a){var
c=bi[1];for(;;){if(c){var
t=c[2],u=c[1];try{var
w=j(u,a),g=w}catch(f){var
g=0}if(!g){var
c=t;continue}var
b=g[1]}else
if(a[1]===em)var
b=en;else
if(a[1]===eo)var
b=ep;else
if(a[1]===eq){var
f=a[2],l=f[3],x=f[2],y=f[1],b=aY(v(bj),y,x,l,l+5|0,er)}else
if(a[1]===s){var
h=a[2],m=h[3],z=h[2],A=h[1],b=aY(v(bj),A,z,m,m+6|0,es)}else
if(a[1]===et){var
k=a[2],o=k[3],B=k[2],C=k[1],b=aY(v(bj),C,B,o,o+6|0,eu)}else{var
e=a.length-1,D=a[0+1][0+1];if(e<0||2<e){var
p=cj(a,2),q=bk(a,1),d=n(v(ei),q,p)}else
switch(e){case
1:var
d=ek;break;case
2:var
r=bk(a,1),d=j(v(el),r);break;default:var
d=ej}var
b=i(D,d)}return b}}function
cl(a){var
f=hi(0);if(f){var
d=f[1],g=d.length-1-1|0,q=0;if(!(g<0)){var
c=q;for(;;){if(hJ(h(d,c),eB)){var
b=h(d,c),k=0===b[0]?b[1]:b[1],e=k?0===c?ev:ey:0===c?ez:eA;if(0===b[0]){var
l=b[5],m=b[4],o=b[3],p=b[2],i=aY(v(ew),e,p,o,m,l)}else
var
i=j(v(ex),e);n(ci(a),eC,i)}var
r=c+1|0;if(g!==c){var
c=r;continue}break}}return 0}return j(ci(a),eD)}32===bd;var
eF=[0,eE];function
cm(a){var
b=a[4],c=b?(a[4]=0,a[1][2]=a[2],a[2][1]=a[1],0):b;return c}function
bl(a){var
b=[];c_(b,[0,b,b]);return b}function
bm(a){return a[2]===a?1:0}var
aE=[0,eG],A=[0,0],eH=42;function
aF(a){var
c=a[1];{if(3===c[0]){var
d=c[1],b=aF(d);if(b!==d)a[1]=[3,b];return b}return a}}function
X(a){return aF(a)}var
cn=[0,function(a){b4(eI);b4(ck(a));c6(ai,10);cl(ai);b5(ai);bb(0);return hS(2)}];function
co(a,b){try{var
c=j(a,b)}catch(f){return j(cn[1],f)}return c}function
cp(a,b,c){var
d=b,e=c;for(;;)if(typeof
d===y)return aG(a,e);else
switch(d[0]){case
1:j(d[1],a);return aG(a,e);case
2:var
g=[0,d[2],e],d=d[1],e=g;continue;default:var
f=d[1][1];return f?(j(f[1],a),aG(a,e)):aG(a,e)}}function
aG(a,b){return b?cp(a,b[1],b[2]):0}function
cq(a,b){var
c=a,d=b;for(;;)if(typeof
c===y)return bn(d);else
switch(c[0]){case
1:cm(c[1]);return bn(d);case
2:var
f=[0,c[2],d],c=c[1],d=f;continue;default:var
e=c[2];A[1]=c[1];co(e,0);return bn(d)}}function
bn(a){return a?cq(a[1],a[2]):0}function
aH(a,b){var
c=1===b[0]?b[1][1]===aE?(cq(a[4],0),1):0:0;return cp(b,a[2],0)}var
aI=[0,0],t=[0,0,0];function
cr(a,b){var
f=A[1],h=aI[1]?1:(aI[1]=1,0);aH(a,b);if(h){A[1]=f;var
g=0}else
for(;;){if(0!==t[1]){if(0===t[1])throw[0,dU];t[1]=t[1]-1|0;var
c=t[2],d=c[2];if(d===c)t[2]=0;else
c[2]=d[2];var
e=d[1];aH(e[1],e[2]);continue}aI[1]=0;A[1]=f;var
g=0;break}return g}function
cs(a){return[0,a]}function
ct(a,b){var
d=aF(a),c=d[1];switch(c[0]){case
1:if(c[1][1]===aE)return 0;break;case
2:var
e=c[1];d[1]=b;return cr(e,b);default:}return T(eJ)}function
aJ(a,b){return ct(a,cs(b))}function
cu(a,b){return typeof
a===y?b:typeof
b===y?a:[2,a,b]}function
bo(a){if(typeof
a!==y)switch(a[0]){case
2:var
b=a[1],c=bo(a[2]);return cu(bo(b),c);case
1:break;default:if(!a[1][1])return 0}return a}function
cv(a,b){var
d=X(a),g=X(b),j=d[1];{if(2===j[0]){var
c=j[1];if(d===g)return 0;var
e=g[1];{if(2===e[0]){var
f=e[1];g[1]=[3,d];c[1]=f[1];var
k=cu(c[2],f[2]),l=c[3]+f[3]|0;if(eH<l){c[3]=0;c[2]=bo(k)}else{c[3]=l;c[2]=k}var
h=f[4],i=c[4],m=typeof
i===y?h:typeof
h===y?i:[2,i,h];c[4]=m;return 0}d[1]=e;return aH(c,e)}}throw[0,s,eL]}}function
aK(a,b){var
c=X(a),d=c[1];{if(2===d[0]){var
e=d[1];c[1]=b;return aH(e,b)}throw[0,s,eM]}}function
l(a){return[0,[0,a]]}var
cw=[0,eN];function
ao(a){return[0,[1,a]]}function
bp(a){return[0,[2,[0,[0,[0,a]],0,0,0]]]}function
bq(a){var
b=[0,[2,[0,1,0,0,0]]];return[0,b,b]}function
br(a,b){var
d=[1,b],c=a[2],e=typeof
c===y?d:[2,d,c];a[2]=e;return 0}function
bs(a,b){var
c=X(a)[1];switch(c[0]){case
1:if(c[1][1]===aE)return co(b,0);break;case
2:var
d=c[1],e=[0,A[1],b],f=d[4],g=typeof
f===y?e:[2,e,f];d[4]=g;return 0;default:}return 0}function
m(a,b){var
e=X(a),c=e[1];switch(c[0]){case
1:return[0,c];case
2:var
f=c[1],d=bp(e),g=A[1];br(f,function(a){switch(a[0]){case
0:var
e=a[1];A[1]=g;try{var
f=j(b,e),c=f}catch(f){var
c=ao(f)}return cv(d,c);case
1:return aK(d,a);default:throw[0,s,eO]}});return d;case
3:throw[0,s,eP];default:return j(b,c[1])}}function
cx(a,b){return m(a,b)}function
cy(a,b){var
f=X(a),c=f[1];switch(c[0]){case
1:var
e=[0,c];break;case
2:var
k=c[1],d=bp(f),l=A[1];br(k,function(a){switch(a[0]){case
0:var
e=a[1];A[1]=l;try{var
f=[0,j(b,e)],c=f}catch(f){var
c=[1,f]}return aK(d,c);case
1:return aK(d,a);default:throw[0,s,eQ]}});var
e=d;break;case
3:throw[0,s,eR];default:var
h=c[1];try{var
i=[0,j(b,h)],g=i}catch(f){var
g=[1,f]}var
e=[0,g]}return e}function
cz(a,b){try{var
i=j(a,0),g=i}catch(f){var
g=ao(f)}var
c=X(g),d=c[1];switch(d[0]){case
1:return j(b,d[1]);case
2:var
f=d[1],e=bp(c),h=A[1];br(f,function(a){switch(a[0]){case
0:return aK(e,a);case
1:var
d=a[1];A[1]=h;try{var
f=j(b,d),c=f}catch(f){var
c=ao(f)}return cv(e,c);default:throw[0,s,eS]}});return e;case
3:throw[0,s,eT];default:return c}}var
eU=[0,function(a){return 0}],J=bl(0),eV=[0,0];function
eW(a){var
e=1-bm(J);if(e){var
b=bl(0);b[1][2]=J[2];J[2][1]=b[1];b[1]=J[1];J[1][2]=b;J[1]=J;J[2]=J;eV[1]=0;var
c=b[2];for(;;){var
d=c!==b?1:0;if(d){if(c[4])aJ(c[3],0);var
c=c[2];continue}return d}}return e}function
cA(c,b){if(b){var
d=b[2],a=b[1],e=function(a){return cA(c,d)};return cx(j(c,a),e)}return cw}var
B=d,K=null,ad=undefined;function
aL(a,b,c){return a==K?j(b,0):j(c,a)}function
cB(a){function
b(a){return[0,a]}return aL(a,function(a){return 0},b)}function
ap(a){return a!==ad?1:0}function
bt(a,b,c){return a===ad?j(b,0):j(c,a)}function
E(a,b){return a===ad?j(b,0):a}function
aM(a){function
b(a){return[0,a]}return bt(a,function(a){return 0},b)}var
bu=true,aN=false,aq=RegExp,bv=Array;function
w(a,b){return a[b]}function
cC(a){return a}var
eX=Math;function
cD(a){return escape(a)}function
eY(a){return a
instanceof
bv?0:[0,new
H(a.toString())]}bi[1]=[0,eY,bi[1]];function
ae(a){return a}function
ar(a){return a}function
g(a,b){a.appendChild(b);return 0}function
cE(a,b){a.removeChild(b);return 0}function
C(d){return ar(aZ(function(a){if(a){var
e=j(d,a);if(!(e|0))a.preventDefault();return e}var
c=event,b=j(d,c);if(!(b|0))c.returnValue=b;return b}))}var
cF=hA(0)|0,eZ=B.document;function
aO(a,b){return a?j(b,a[1]):0}function
bw(a,b){return a.createElement(b.toString())}function
af(a,b){return bw(a,b)}var
cG=[0,dj];function
cH(a){return af(a,e0)}function
as(a){return af(a,e1)}function
cJ(a){return af(a,e3)}ae(B.HTMLElement)===ad;var
e7=B.FileReader,e_=hy(0),bx=2147483;function
aP(a){var
b=bq(0),c=b[1],d=[0,0],g=b[2];function
e(a,b){var
c=bx<a?[0,bx,a-bx]:[0,a,0],f=c[2],h=c[1],i=f==0?function(a){return aJ(g,a)}:function(a){return e(f,a)};d[1]=[0,B.setTimeout(aZ(i),h*dk)];return 0}e(a,0);bs(c,function(a){var
b=d[1];return b?B.clearTimeout(b[1]):0});return c}eU[1]=function(a){return 1===a?(B.setTimeout(aZ(eW),0),0):0};function
cK(a){return e_.log(a.toString())}cn[1]=function(a){cK(e$);cK(ck(a));return cl(ai)};function
cL(a){return new
aq(o(a),bK)}var
fa=new
aq("[$]",bK),fc=cL(fb);function
cN(a,b){return b.split(aj(1,a).toString())}var
cO=[0,fd];function
Y(a){throw[0,cO]}var
cM=cL(R(o(fe).replace(fc,"\\$&"))),cP=new
aq("\\+",bK);function
L(a){cP.lastIndex=0;return R(unescape(a.replace(cP,c4)))}function
p(a,b){var
d=a?a[1]:1;if(d){var
e=R(cD(o(b)));cM.lastIndex=0;var
c=o(e);return R(c.replace(cM,o(ff).replace(fa,"$$$$")))}return R(cD(o(b)))}var
fh=[0,fg];function
at(a){try{var
c=a.getLen();if(0===c)var
d=fo;else{var
b=0,g=47,f=a.getLen();for(;;){if(f<=b)throw[0,bc];if(a.safeGet(b)!==g){var
b=b+1|0;continue}if(0===b)var
e=[0,fp,at(Q(a,1,c-1|0))];else{var
h=at(Q(a,b+1|0,(c-b|0)-1|0)),e=[0,Q(a,0,b),h]}var
d=e;break}}}catch(f){if(f[1]===bc)return[0,a,0];throw f}return d}function
aQ(a){return ak(fr,V(function(a){var
b=a[1],c=i(fq,p(0,a[2]));return i(p(0,b),c)},a))}function
by(a){var
d=cN(38,a),b=d.length;function
e(a,b){var
c=b;for(;;){if(0<=c){try{var
f=c-1|0,g=function(a){function
e(a){var
c=a[2],d=a[1];function
b(a){return L(E(a,Y))}var
e=b(c);return[0,b(d),e]}var
b=cN(61,a);if(2===b.length){var
d=w(b,1),c=ae([0,w(b,0),d])}else
var
c=ad;return bt(c,Y,e)},h=e([0,bt(w(d,c),Y,g),a],f)}catch(f){if(f[1]===cO){var
c=c-1|0;continue}throw f}return h}return a}}return e(0,b-1|0)}var
ft=new
aq(o(fs)),fv=new
aq(o(fu));function
cQ(a){switch(a[0]){case
1:var
c=a[1],h=c[6],j=c[5],k=c[2],w=c[3],y=c[1],z=x(h,fM)?i(fN,p(0,h)):fV,A=j?i(fO,aQ(j)):fU,B=i(A,z),C=i(fQ,i(ak(fP,V(function(a){return p(0,a)},w)),B)),D=df===k?fR:i(fT,U(k)),E=i(D,C);return i(fS,i(p(0,y),E));case
2:var
d=a[1],l=d[4],m=d[3],F=d[1],G=x(l,fW)?i(fX,p(0,l)):f2,H=m?i(fY,aQ(m)):f1,I=i(H,G);return i(f0,i(ak(fZ,V(function(a){return p(0,a)},F)),I));default:var
b=a[1],e=b[6],f=b[5],g=b[2],n=b[3],o=b[1],q=x(e,fC)?i(fD,p(0,e)):fL,r=f?i(fE,aQ(f)):fK,s=i(r,q),t=i(fG,i(ak(fF,V(function(a){return p(0,a)},n)),s)),u=80===g?fH:i(fJ,U(g)),v=i(u,t);return i(fI,i(p(0,o),v))}}var
au=location;L(au.hostname);L(au.protocol);try{}catch(f){if(f[1]!==ah)throw f}at(L(au.pathname));by(au.search);L(au.href);var
f3=B.FormData;function
cR(a,b){if(a6<=a[1]){var
d=a[2];d[1]=[0,b,d[1]];return 0}var
e=a[2],c=b[2],f=b[1];return a0<=c[1]?e.append(f.toString(),c[2]):e.append(f.toString(),c[2])}function
bz(a){return ActiveXObject}var
gg=[0,gf];function
e(a){return a.toString()}var
f=B.document;function
M(a,b){return g(a,f.createTextNode(e(b)))}function
bA(c,b){function
d(a){return cE(c,a)}var
a=c.firstChild;if(a!=K)d(a);return g(c,b)}var
cS=e(go),bB=[0,gu],gq=e(gp);function
cT(a){switch(a){case
1:return e(gw);case
2:return e(gx);case
3:return e(gy);case
4:return e(gz);case
5:return e(gA);case
6:return e(gB);case
7:return e(gC);case
8:return e(gD);default:return e(gv)}}function
q(a,b,c,d){k(h(a[1],c),b,d);var
e=h(h(a[2],c),b);return e.src=cT(d)}function
aR(b){var
d=[0,0],e=b[1].length-1-2|0,i=1;if(!(e<1)){var
a=e;for(;;){var
f=h(b[1],a).length-1-2|0,k=1;if(!(f<1)){var
c=k;for(;;){var
g=6===h(h(b[1],a+1|0),c)?1:0,o=g?3===h(h(b[1],a),c)?1:0:g,r=0===h(h(b[1],a),c)?3===h(h(b[1],a-1|0),c)?(q(b,c,a-1|0,0),q(b,c,a,3),d[1]=1,1):0:0,s=0===h(h(b[1],a),c)?0===h(h(b[1],a-1|0),c)?3===h(h(b[1],a),c-1|0)?3===h(h(b[1],a-1|0),c-1|0)?(q(b,c-1|0,a-1|0,0),q(b,c,a,3),d[1]=1,1):0:0:0:0,t=0===h(h(b[1],a),c)?0===h(h(b[1],a-1|0),c)?3===h(h(b[1],a),c+1|0)?3===h(h(b[1],a-1|0),c+1|0)?(q(b,c+1|0,a-1|0,0),q(b,c,a,3),d[1]=1,1):0:0:0:0;if(!o&&6===h(h(b[1],a+1|0),c)&&3===h(h(b[1],a),c)){q(b,c,a+1|0,8);throw[0,bB]}var
p=c+1|0;if(f!==c){var
c=p;continue}break}}var
n=a-1|0;if(i!==a){var
a=n;continue}break}}if(d[1]){var
j=function(a){return aR(b)};return m(aP(bV),j)}return l(0)}function
bC(g,y,w){var
L=w[3];function
n(a){var
M=g[1].length-1-1|0,Y=0;if(!(M<0)){var
c=Y;for(;;){var
V=h(g[1],c).length-1-1|0,ag=0;if(!(V<0)){var
d=ag;for(;;){h(h(g[2],c),d).onmouseover=K;h(h(g[2],c),d).onmouseout=K;h(h(g[2],c),d).onclick=K;var
ai=d+1|0;if(V!==d){var
d=ai;continue}break}}var
ah=c+1|0;if(M!==c){var
c=ah;continue}break}}function
n(a,b){if(!g[8]){g[8]=1;var
c=function(a){g[8]=0;return l(0)};m(j(a,0),c)}return aN}function
N(a,b,c){function
d(a){g[9][1]=[0,b];return l(0)}return m(j(a,0),d)}function
o(a,b){var
c=g[9][1];return c?(j(c[1],0),g[9][1]=0,j(a,0)):j(a,0)}function
f(a,I,H,G,e,f){var
t=a,p=G,k=e,i=f;for(;;){var
b=t[2],c=t[1],D=h(h(g[1],b),c);if(5===D||!(3<=D))var
F=0;else{var
E=0,F=1}if(!F)var
E=1;if(E){var
d=h(h(g[2],b),c).src,r=function(p,b,c){return function(a){h(h(g[2],b),c).src=H;return j(p,0)}}(p,b,c),s=function(k,b,c,d){return function(a){h(h(g[2],b),c).src=d;return j(k,0)}}(k,b,c,d),J=function(i,b,c){return function(a){function
d(a){if(2===h(h(g[1],b),c))g[5]=g[5]-1|0;q(g,c,b,6);function
d(a){function
d(a){q(g,c,b,0);return l(0)}return m(aR(g),d)}return m(aP(bV),d)}return m(j(i,0),d)}}(i,b,c),u=function(i,b,c){return function(a){var
d=g[3];q(g,d[1],d[2],0);function
e(a){return bC(g,y,w)}function
f(a){return a[1]===bB?(g[6]=1,l(0)):ao(a)}return m(cz(function(a){function
d(a){if(2===h(h(g[1],b),c))g[5]=g[5]-1|0;q(g,c,b,6);g[3]=[0,c,b];return aR(g)}return m(j(i,0),d)},f),e)}}(i,b,c),K=h(h(g[2],b),c),v=function(r){return function(a){return o(r,a)}}(r),x=function(s,v){return function(a){return N(v,s,a)}}(s,v);K.onmouseover=C(function(x){return function(a){return n(x,a)}}(x));var
L=h(h(g[2],b),c),z=function(a){return l(0)},A=function(z){return function(a){return o(z,a)}}(z);L.onmouseout=C(function(A){return function(a){return n(A,a)}}(A));var
M=h(h(g[2],b),c),B=function(u){return function(a){return o(u,a)}}(u);M.onclick=C(function(B){return function(a){return n(B,a)}}(B));if(5===h(h(g[1],b),c))return 0;var
t=j(I,[0,c,b]),p=r,k=s,i=J;continue}return 0}}function
O(a,b,c,d){var
p=a[2],r=a[1],k=j(b,a),f=k[2],i=k[1],s=j(b,k),t=s[2],u=s[1];try{var
z=3===h(h(g[1],f),i)?1:0,O=z?0===h(h(g[1],t),u)?1:0:z,v=O}catch(f){if(f[1]===ba&&!x(f[2],gE)){var
v=0,A=1}else
var
A=0;if(!A)throw f}if(v){var
B=function(a){h(h(g[2],p),r).src=d;h(h(g[2],f),i).src=c;return l(0)},D=function(a){var
b=h(h(g[2],p),r);b.src=e(gF);var
c=h(h(g[2],f),i);return c.src=e(gG)},E=function(a){q(g,r,p,0);q(g,i,f,6);g[3]=k;q(g,u,t,3);function
b(a){return bC(g,y,w)}function
c(a){return a[1]===bB?(g[6]=1,l(0)):ao(a)}return m(cz(function(a){return aR(g)},c),b)},F=h(h(g[2],f),i),G=function(a){return o(B,a)},H=function(a){return N(G,D,a)};F.onmouseover=C(function(a){return n(H,a)});var
I=h(h(g[2],f),i),J=function(a){return l(0)},K=function(a){return o(J,a)};I.onmouseout=C(function(a){return n(K,a)});var
L=h(h(g[2],f),i),M=function(a){return o(E,a)};return L.onclick=C(function(a){return n(M,a)})}return 0}if(c$(g[3],g[4])){j(L,0);B.alert(e(gH))}else
if(g[6]){j(L,0);B.alert(e(gI))}else{if(0===g[5]){var
P=g[4],Q=P[2],R=P[1],Z=h(h(g[2],Q),R);Z.src=e(gJ);k(h(g[1],Q),R,5)}var
z=function(a){return[0,a[1]+1|0,a[2]]},A=function(a){return[0,a[1]-1|0,a[2]]},S=function(a){return[0,a[1],a[2]-1|0]},U=function(a){return[0,a[1],a[2]+1|0]},p=function(a){return 0},b=function(a){return l(0)},_=e(gK);f(z(g[3]),z,_,b,p,b);var
$=e(gL);f(A(g[3]),A,$,b,p,b);var
aa=e(gM);f(S(g[3]),S,aa,b,p,b);var
ab=e(gN);f(U(g[3]),U,ab,b,p,b);var
ac=e(gO),ad=e(gP);O(g[3],z,ad,ac);var
ae=e(gQ),af=e(gR);O(g[3],A,af,ae);j(y,g[5])}var
i=g[7];if(i[1])if(bm(i[2]))i[1]=0;else{var
J=i[2],X=0;if(bm(J))throw[0,eF];var
F=J[2];cm(F);var
W=F[3],v=cs(X),G=aF(W),u=G[1];switch(u[0]){case
1:var
r=u[1][1]===aE?1:0;break;case
2:var
H=u[1];G[1]=v;if(aI[1]){var
I=[0,H,v];if(0===t[1]){var
s=[];c_(s,[0,I,s]);t[1]=1;t[2]=s}else{var
D=t[2],E=[0,I,D[2]];t[1]=t[1]+1|0;D[2]=E;t[2]=E}var
r=1}else{cr(H,v);var
r=1}break;default:var
r=0}if(!r)T(eK)}return l(0)}var
c=g[7];if(c[1]){var
a=c[2],d=[0,1,0,0,0],f=[0,[2,d]],b=[0,a[1],a,f,1];a[1][2]=b;a[1]=b;d[4]=[1,b];var
i=f}else{c[1]=1;var
i=cw}return m(i,n)}function
bD(a,b){return b?a.style.cssText=b[1]:0}B.onload=C(function(a){function
G(a){throw[0,s,gS]}var
A=f.getElementById(e(gT)),q=A==K?G(0):A,B=as(f),u=[0,aw(0)],b=as(f);b.style.cssText=cS;M(b,gs);var
d=[0,1];function
z(a){var
g=aw(0)-u[1];if(!d[1]){var
c=g|0;bA(b,f.createTextNode(e(r(v(gt),c/3600|0,(c/60|0)%60|0,c%60|0))))}function
h(a){return z(0)}return m(aP(1),h)}z(0);function
F(a){d[1]=1;return 0}var
t=[0,b,function(a){u[1]=aw(0);d[1]=0;return 0},F],O=t[2],J=t[1];function
D(a,b){var
v=as(f);v.style.cssText=gq;M(v,gr);g(q,v);function
bf(a){function
c(a){cE(q,v);return l(a)}return m(j(b,a),c)}function
be(a){var
b=a[2],c=a[4];if(0!==b&&200!==b)return[0,[2,[0,0,0,0,0]]];return l(c)}var
aC=0,aD=0,aE=0,aF=0,aG=0,aH=0,u=0,O=0,bd=0,a2=0?bd[1]:0,a3=aH?aH[1]:0,a4=aF?aF[1]:function(a,b){return 1};if(aG){var
af=aG[1];if(u){var
a5=u[1];ac(function(a){return cR(af,[0,a[1],a[2]])},a5)}var
h=[0,af]}else
if(u){var
bc=u[1],T=aM(ae(f3)),aB=T?[0,808620462,new(T[1])()]:[0,a6,[0,0]];ac(function(a){return cR(aB,[0,a[1],a[2]])},bc);var
h=[0,aB]}else
var
h=0;if(h){var
ag=h[1];if(O)var
ah=[0,gh,O,a7];else{if(a6<=ag[1]){var
B=0,A=0,k=ag[2][1];for(;;){if(k){var
P=k[2],D=k[1],aI=a0<=D[2][1]?0:1;if(aI){var
B=[0,D,B],k=P;continue}var
A=[0,D,A],k=P;continue}var
aK=ab(A);ab(B);if(aK){var
W=function(a){return U(eX.random()*1e9|0)},aW=W(0),X=i(f5,i(W(0),aW)),az=[0,gk,[0,i(gj,X)],[0,164354597,X]]}else
var
az=gl;var
aA=az;break}}else
var
aA=gm;var
ah=aA}var
r=ah}else
var
r=[0,gn,O,a7];var
ai=r[3],aj=r[2],S=o(a),a8=r[1];function
aO(a){var
c=cC(a),b=R(E(w(c,1),Y).toLowerCase());if(x(b,fi)&&x(b,fj)){if(x(b,fk)&&x(b,fl)){if(x(b,fm)&&x(b,fn)){var
e=1,j=0}else
var
j=1;if(j){var
d=1,e=2}}else
var
e=0;switch(e){case
1:var
f=0;break;case
2:var
f=1;break;default:var
d=0,f=1}if(f){var
g=L(E(w(c,5),Y)),k=function(a){return o(fx)},l=L(E(w(c,9),k)),m=function(a){return o(fy)},n=by(E(w(c,7),m)),p=at(g),q=function(a){return o(fz)},h=R(E(w(c,4),q)),r=x(h,fw)?c8(h):d?df:80,i=[0,L(E(w(c,2),Y)),r,p,g,n,l],s=d?[1,i]:[0,i];return[0,s]}}throw[0,fh]}function
aP(a){function
b(a){var
b=cC(a),c=L(E(w(b,2),Y));function
d(a){return o(fA)}var
e=R(E(w(b,6),d));function
f(a){return o(fB)}var
g=by(E(w(b,4),f));return[0,[2,[0,at(c),c,g,e]]]}function
c(a){return 0}return aL(fv.exec(S),c,b)}var
Q=aL(ft.exec(S),aP,aO);if(Q){var
F=Q[1];switch(F[0]){case
0:var
Z=F[1],_=Z.slice(),aY=Z[5];_[5]=0;var
t=[0,cQ([0,_]),aY],z=1;break;case
1:var
$=F[1],aa=$.slice(),a1=$[5];aa[5]=0;var
t=[0,cQ([1,aa]),a1],z=1;break;default:var
z=0}}else
var
z=0;if(!z)var
t=[0,a,0];var
al=t[1],am=b2(t[2],a3),an=am?i(al,i(gi,aQ(am))):al,ao=bq(0),aq=ao[2],au=ao[1];try{var
aU=new
XMLHttpRequest(),d=aU}catch(f){try{var
aT=new(bz(0))("Msxml2.XMLHTTP"),d=aT}catch(f){try{var
aS=new(bz(0))("Msxml3.XMLHTTP"),d=aS}catch(f){try{var
aR=new(bz(0))("Microsoft.XMLHTTP")}catch(f){throw[0,s,f4]}var
d=aR}}}if(aC)d.overrideMimeType(aC[1].toString());d.open(a8.toString(),an.toString(),bu);if(aj)d.setRequestHeader("Content-type",aj[1].toString());ac(function(a){return d.setRequestHeader(a[1].toString(),a[2].toString())},a2);function
G(a){function
b(a){return[0,new
H(a)]}function
c(a){return 0}return aL(d.getResponseHeader(o(a)),c,b)}var
av=[0,0];function
J(a){var
b=av[1]?0:n(a4,d.status,G)?0:(ct(aq,[1,[0,gg,[0,d.status,G]]]),d.abort(),1);av[1]=1;return 0}d.onreadystatechange=aZ(function(a){switch(d.readyState){case
2:if(!cF)return J(0);break;case
3:if(cF)return J(0);break;case
4:J(0);var
b=function(a){var
b=cB(d.responseXML);if(b){var
c=b[1];return ar(c.documentElement)===K?0:[0,c]}return 0};return aJ(aq,[0,an,d.status,G,new
H(d.responseText),b]);default:}return 0});if(aE){var
a9=aE[1];d.onprogress=C(function(a){n(a9,a.loaded,a.total);return bu})}function
a_(a){if(aD){var
b=aD[1];return a.onprogress=C(function(a){n(b,a.loaded,a.total);return bu})}return 0}var
aw=d.upload;if(aw!==ad)a_(aw);if(h){var
N=h[1];if(a6<=N[1]){var
ax=N[2];if(typeof
ai===y){var
a$=ax[1];d.send(ar(ak(ge,V(function(a){var
b=a[2],c=a[1];if(a0<=b[1]){var
d=i(gc,p(0,new
H(b[2].name)));return i(p(0,c),d)}var
e=i(gd,p(0,new
H(b[2])));return i(p(0,c),e)},a$)).toString()))}else{var
ay=ai[2],ba=function(a){var
b=ar(a.join(c));return ap(d.sendAsBinary)?d.sendAsBinary(b):d.send(b)},bb=ax[1],e=new
bv(),aX=function(a){e.push(i(f7,i(ay,f6)).toString());return e};cy(cy(cA(function(a){e.push(i(f9,i(ay,f8)).toString());var
g=a[2],o=a[1];if(a0<=g[1]){var
b=g[2],r=function(a){var
c=aM(b.name),g="Content-Type: application/octet-stream\r\n",h='"\r\n';if(c)var
f=c[1];else{var
d=aM(b.fileName),f=d?d[1]:I(e6)}e.push(i(f$,i(o,f_)).toString(),f,h,g);e.push(aV,a,aV);return l(0)},k=aM(ae(e7)),d=-1041425454;if(k){var
c=new(k[1])(),h=bq(0),j=h[1],p=h[2];c.onloadend=C(function(a){if(2===c.readyState){var
b=c.result,e=c$(typeof
b,"string")?ar(b):K,d=cB(e);if(!d)throw[0,s,e8];aJ(p,d[1])}return aN});bs(j,function(a){return c.abort()});if(typeof
d===y)if(dd===d)c.readAsDataURL(b);else
if(dc<=d)c.readAsText(b);else
c.readAsBinaryString(b);else
c.readAsText(b,d[2]);var
n=j}else{var
f=function(a){return I(e9)};if(typeof
d===y)var
m=dd===d?ap(b.getAsDataURL)?b.getAsDataURL():f(0):dc<=d?ap(b.getAsText)?b.getAsText("utf8"):f(0):ap(b.getAsBinary)?b.getAsBinary():f(0);else{var
q=d[2],m=ap(b.getAsText)?b.getAsText(q):f(0)}var
n=l(m)}return cx(n,r)}var
t=g[2];e.push(i(gb,i(o,ga)).toString(),t,aV);return l(0)},bb),aX),ba)}}else
d.send(N[2])}else
d.send(K);bs(au,function(a){return d.abort()});return m(m(au,be),bf)}var
k=as(f);k.style.cssText=cS;M(k,gU);function
P(a){return bA(k,f.createTextNode(U(a).toString()))}function
N(o){var
F=q.style;F.cssText=e(g0);var
x=af(f,e2);M(x,g1);g(q,x);var
a=as(f);M(a,g2);g(a,J);M(a,g3);g(a,k);M(a,g4);var
n=0,p=0;for(;;){if(0===p&&0===n){var
b=bw(f,cI),z=1}else
var
z=0;if(!z){var
r=cG[1];if(dj===r){try{var
u=eZ.createElement('<input name="x">'),w=u.tagName.toLowerCase()==="input"?1:0,E=w?u.name===dh?1:0:w,s=E}catch(f){var
s=0}var
A=s?de:-1003883683;cG[1]=A;continue}if(de<=r){var
d=new
bv();d.push("<",cX);aO(p,function(a){d.push(' type="',da(a),aS);return 0});aO(n,function(a){d.push(' name="',da(a),aS);return 0});d.push(">");var
b=f.createElement(d.join(c))}else{var
i=bw(f,cI);aO(p,function(a){return i.type=a});aO(n,function(a){return i.name=a});var
b=i}}var
y=cH(f);M(y,g5);g(b,y);ac(function(a){var
d=a[2],c=cH(f);M(c,d);return g(b,c)},o);b.onchange=C(function(a){var
d=b.selectedIndex-1|0;if(0<=d){var
k=0,i=o;for(;;){if(i){var
k=k+1|0,i=i[2];continue}if(d<k){if(0<=d){var
c=o,n=d;for(;;){if(c){var
s=c[2],u=c[1];if(0!==n){var
c=s,n=n-1|0;continue}var
p=u}else
var
p=I(dG);var
q=p;break}}else
var
q=T(dH);var
w=q[1];D(w,function(a){var
w=[0,0],b=[0,0],A=a.getLen()-1|0,Q=0;if(!(A<0)){var
r=Q;for(;;){var
d=a.safeGet(r);if(47<=d)if(83<=d)if(89<=d)var
i=0;else{switch(d-83|0){case
0:b[1]=[0,6,b[1]];var
s=1;break;case
4:b[1]=[0,6,b[1]];var
s=1;break;case
5:b[1]=[0,3,b[1]];var
s=1;break;default:var
i=0,s=0}if(s)var
i=1}else
var
i=69===d?(b[1]=[0,4,b[1]],1):0;else
if(10===d){var
Y=w[1];w[1]=[0,ab(b[1]),Y];b[1]=0;var
i=1}else
if(32<=d){switch(d-32|0){case
0:b[1]=[0,0,b[1]];var
o=1;break;case
3:b[1]=[0,7,b[1]];var
o=1;break;case
11:b[1]=[0,2,b[1]];var
o=1;break;case
14:b[1]=[0,1,b[1]];var
o=1;break;default:var
i=0,o=0}if(o)var
i=1}else
var
i=0;if(!i)I(gZ);var
X=r+1|0;if(A!==r){var
r=X;continue}break}}var
x=b7(V(b7,ab(w[1])));function
R(a){var
b=af(f,e4);b.src=cT(a);return b}var
q=b6(function(a){return b6(R,a)},x),C=[0,0],D=[0,0],E=[0,0],F=[0,0],G=[0,0],S=e(gV),T=[0,e(gW)],n=af(f,e5);bD(n,[0,S]);var
y=q.length-1-1|0,U=0,J=0;if(!(y<0)){var
c=J;for(;;){var
p=n.insertRow(-1);bD(p,U);var
z=h(q,c).length-1-1|0,K=0;if(!(z<0)){var
k=K;for(;;){var
u=p.insertCell(-1);bD(u,T);var
M=h(h(q,c),k);switch(h(h(x,c),k)){case
2:G[1]++;break;case
4:E[1]=k;F[1]=c;break;case
6:C[1]=k;D[1]=c;break;default:}g(u,M);g(p,u);var
N=k+1|0;if(z!==k){var
k=N;continue}break}}g(n,p);var
L=c+1|0;if(y!==c){var
c=L;continue}break}}bA(B,n);function
W(a){var
b=aw(0);function
c(a){var
d=aw(0);if(1<=d-b){var
f=n.style;f.opacity=ae(e(gX));return l(0)}function
g(a){var
f=n.style;f.opacity=ae(e(j(v(gY),d-b)));return c(0)}return m(aP(bV),g)}function
d(a){j(O,0);return l(0)}return m(c(0),d)}var
H=[0,0,bl(0)];return m(bC([0,x,q,[0,C[1],D[1]],[0,E[1],F[1]],G[1],0,H,0,[0,0]],P,t),W)});var
r=1}else
var
r=0;break}}else
var
r=0;return aN});g(a,b);g(a,cJ(f));g(a,cJ(f));g(a,B);g(q,a);return l(0)}}m(D(g9,function(d){function
c(a){var
f=d.getLen(),b=a;for(;;){if(f<=b)var
h=I(g6);else{if(34!==d.safeGet(b)){var
b=b+1|0;continue}var
e=b+1|0,c=b+2|0;for(;;){if(f<=e)var
g=I(g7);else{if(34!==d.safeGet(c)){var
c=c+1|0;continue}var
g=[0,Q(d,e,c-e|0),c+1|0]}var
h=g;break}}return h}}var
e=0,a=0;for(;;){try{var
g=c(e),j=g[1],h=c(g[2]),k=[0,[0,[0,j,h[1]],h[2]]],b=k}catch(f){if(f[1]===ah&&!x(f[2],g8)){var
b=0,i=1}else
var
i=0;if(!i)throw f}if(b){var
f=b[1],e=f[2],a=[0,f[1],a];continue}return l(ab(a))}}),N);return aN});bb(0);return}(this));
