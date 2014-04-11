// This program was compiled from OCaml by js_of_ocaml 2.00dev+git-f7cce66
(function(joo_global_object_A_)
   {"use strict";
    var
     num_125_bA_=125,
     num_123_bD_=123,
     num_254_z_=254,
     num_255_M_=255,
     str_x_cQ_="x",
     num_0_99_cT_=0.99,
     str_ac_=".",
     num_108_c4_=108,
     num_65535_aV_=65535,
     str_aW_="+",
     str_aT_='"',
     num_16777215_y_=16777215,
     str_g_cP_="g",
     str_f_by_="f",
     num_250_cY_=250,
     num_105_T_=105,
     num_0_5_c3_=0.5,
     str_d_cO_="%d",
     str_jsError_cS_="jsError",
     num_88_c1_=-88,
     num_110_as_=110,
     num_2147483_bz_=2147483,
     str_input_cM_="input",
     str_copy_cR_="copy",
     num_785140586_cX_=785140586,
     str_aS_="'",
     num_115_at_=115,
     str_int_of_string_aR_="int_of_string",
     num_32_cW_=-32,
     num_102_bH_=102,
     num_982028505_cN_=982028505,
     num_111_bF_=111,
     num_120_bC_=120,
     str_G_=" ",
     str_e_ar_="e",
     num_117_bB_=117,
     str_S_="-",
     num_48_ab_=-48,
     str_nan_cV_="nan",
     str_f_="",
     num_116_bx_=116,
     num_600_aq_=600,
     str_12g_cU_="%.12g",
     num_100_aX_=100,
     str_file_already_abr_bI_=" : file already exists",
     str_0_u_="0",
     str_bE_="/",
     str_planet_ml_aY_="planet.ml",
     num_114_aU_=114,
     num_103_bG_=103,
     str_fd_c2_="fd ",
     num_101_c0_=101,
     str_index_out_of_bounds_cZ_="index out of bounds",
     str_number_H_="number",
     num_1e3_ap_=1e3,
     str_src_core_lwt_ml_ad_="src/core/lwt.ml";
    function caml_raise_with_arg_dg_(tag_a_,arg_b_){throw [0,tag_a_,arg_b_]}
    function js_print_stderr_bN_(s_a_)
     {if(s_a_.charCodeAt(s_a_.length-1)==10)s_a_=s_a_.substr(0,s_a_.length-1);
      var v_b_=joo_global_object_A_.console;
      v_b_&&v_b_.error&&v_b_.error(s_a_)}
    var caml_global_data_g_=[0];
    function caml_str_repeat_av_(n_a_,s_b_)
     {if(!n_a_)return str_f_;
      if(n_a_&1)return caml_str_repeat_av_(n_a_-1,s_b_)+s_b_;
      var r_c_=caml_str_repeat_av_(n_a_>>1,s_b_);
      return r_c_+r_c_}
    function MlString_q_(param_a_)
     {if(param_a_!=null)
       {this.bytes=this.fullBytes=param_a_;this.last=this.len=param_a_.length}}
    function mlstring_bound_error_dh_()
     {caml_raise_with_arg_dg_
       (caml_global_data_g_[4],new MlString_q_(str_index_out_of_bounds_cZ_))}
    MlString_q_.prototype=
    {string:null,
     bytes:null,
     fullBytes:null,
     array:null,
     len:null,
     last:0,
     toJsString:
     function()
      {var a_a_=this.getFullBytes();
       try
        {return this.string=decodeURIComponent(escape(a_a_))}
       catch(e_f_)
        {js_print_stderr_bN_
          ('MlString.toJsString: wrong encoding for "%s" ',a_a_);
         return a_a_}},
     toBytes:
     function()
      {if(this.string!=null)
        try
         {var b_a_=unescape(encodeURIComponent(this.string))}
        catch(e_f_)
         {js_print_stderr_bN_
           ('MlString.toBytes: wrong encoding for "%s" ',this.string);
          var b_a_=this.string}
       else
        {var b_a_=str_f_,a_c_=this.array,l_d_=a_c_.length;
         for(var i_b_=0;i_b_<l_d_;i_b_++)b_a_+=String.fromCharCode(a_c_[i_b_])}
       this.bytes=this.fullBytes=b_a_;
       this.last=this.len=b_a_.length;
       return b_a_},
     getBytes:
     function()
      {var b_a_=this.bytes;if(b_a_==null)b_a_=this.toBytes();return b_a_},
     getFullBytes:
     function()
      {var b_a_=this.fullBytes;
       if(b_a_!==null)return b_a_;
       b_a_=this.bytes;
       if(b_a_==null)b_a_=this.toBytes();
       if(this.last<this.len)
        {this.bytes=b_a_+=caml_str_repeat_av_(this.len-this.last,"\0");
         this.last=this.len}
       this.fullBytes=b_a_;
       return b_a_},
     toArray:
     function()
      {var b_c_=this.bytes;
       if(b_c_==null)b_c_=this.toBytes();
       var a_b_=[],l_d_=this.last;
       for(var i_a_=0;i_a_<l_d_;i_a_++)a_b_[i_a_]=b_c_.charCodeAt(i_a_);
       for(l_d_=this.len;i_a_<l_d_;i_a_++)a_b_[i_a_]=0;
       this.string=this.bytes=this.fullBytes=null;
       this.last=this.len;
       this.array=a_b_;
       return a_b_},
     getArray:
     function(){var a_a_=this.array;if(!a_a_)a_a_=this.toArray();return a_a_},
     getLen:
     function()
      {var len_a_=this.len;
       if(len_a_!==null)return len_a_;
       this.toBytes();
       return this.len},
     toString:
     function(){var s_a_=this.string;return s_a_?s_a_:this.toJsString()},
     valueOf:
     function(){var s_a_=this.string;return s_a_?s_a_:this.toJsString()},
     blitToArray:
     function(i1_a_,a2_b_,i2_c_,l_d_)
      {var a1_g_=this.array;
       if(a1_g_)
        if(i2_c_<=i1_a_)
         for(var i_e_=0;i_e_<l_d_;i_e_++)a2_b_[i2_c_+i_e_]=a1_g_[i1_a_+i_e_];
        else
         for(var i_e_=l_d_-1;i_e_>=0;i_e_--)
          a2_b_[i2_c_+i_e_]=a1_g_[i1_a_+i_e_];
       else
        {var b_f_=this.bytes;
         if(b_f_==null)b_f_=this.toBytes();
         var l1_h_=this.last-i1_a_;
         if(l_d_<=l1_h_)
          for(var i_e_=0;i_e_<l_d_;i_e_++)
           a2_b_[i2_c_+i_e_]=b_f_.charCodeAt(i1_a_+i_e_);
         else
          {for(var i_e_=0;i_e_<l1_h_;i_e_++)
            a2_b_[i2_c_+i_e_]=b_f_.charCodeAt(i1_a_+i_e_);
           for(;i_e_<l_d_;i_e_++)a2_b_[i2_c_+i_e_]=0}}},
     get:
     function(i_a_)
      {var a_c_=this.array;
       if(a_c_)return a_c_[i_a_];
       var b_b_=this.bytes;
       if(b_b_==null)b_b_=this.toBytes();
       return i_a_<this.last?b_b_.charCodeAt(i_a_):0},
     safeGet:
     function(i_a_)
      {if(this.len==null)this.toBytes();
       if(i_a_<0||i_a_>=this.len)mlstring_bound_error_dh_();
       return this.get(i_a_)},
     set:
     function(i_a_,c_b_)
      {var a_c_=this.array;
       if(!a_c_)
        {if(this.last==i_a_)
          {this.bytes+=String.fromCharCode(c_b_&num_255_M_);
           this.last++;
           return 0}
         a_c_=this.toArray()}
       else
        if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;
       a_c_[i_a_]=c_b_&num_255_M_;
       return 0},
     safeSet:
     function(i_a_,c_b_)
      {if(this.len==null)this.toBytes();
       if(i_a_<0||i_a_>=this.len)mlstring_bound_error_dh_();
       this.set(i_a_,c_b_)},
     fill:
     function(ofs_a_,len_b_,c_c_)
      {if(ofs_a_>=this.last&&this.last&&c_c_==0)return;
       var a_d_=this.array;
       if(!a_d_)
        a_d_=this.toArray();
       else
        if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;
       var l_f_=ofs_a_+len_b_;
       for(var i_e_=ofs_a_;i_e_<l_f_;i_e_++)a_d_[i_e_]=c_c_},
     compare:
     function(s2_a_)
      {if(this.string!=null&&s2_a_.string!=null)
        {if(this.string<s2_a_.string)return -1;
         if(this.string>s2_a_.string)return 1;
         return 0}
       var b1_b_=this.getFullBytes(),b2_c_=s2_a_.getFullBytes();
       if(b1_b_<b2_c_)return -1;
       if(b1_b_>b2_c_)return 1;
       return 0},
     equal:
     function(s2_a_)
      {if(this.string!=null&&s2_a_.string!=null)
        return this.string==s2_a_.string;
       return this.getFullBytes()==s2_a_.getFullBytes()},
     lessThan:
     function(s2_a_)
      {if(this.string!=null&&s2_a_.string!=null)
        return this.string<s2_a_.string;
       return this.getFullBytes()<s2_a_.getFullBytes()},
     lessEqual:
     function(s2_a_)
      {if(this.string!=null&&s2_a_.string!=null)
        return this.string<=s2_a_.string;
       return this.getFullBytes()<=s2_a_.getFullBytes()}};
    function MlWrappedString_V_(s_a_){this.string=s_a_}
    MlWrappedString_V_.prototype=new MlString_q_();
    function caml_raise_with_string_bM_(tag_a_,msg_b_)
     {caml_raise_with_arg_dg_(tag_a_,new MlWrappedString_V_(msg_b_))}
    function caml_invalid_argument_ae_(msg_a_)
     {caml_raise_with_string_bM_(caml_global_data_g_[4],msg_a_)}
    function caml_array_bound_error_c6_()
     {caml_invalid_argument_ae_(str_index_out_of_bounds_cZ_)}
    function caml_array_get_ff_(array_a_,index_b_)
     {if(index_b_<0||index_b_>=array_a_.length-1)caml_array_bound_error_c6_();
      return array_a_[index_b_+1]}
    function caml_array_set_fg_(array_a_,index_b_,newval_c_)
     {if(index_b_<0||index_b_>=array_a_.length-1)caml_array_bound_error_c6_();
      array_a_[index_b_+1]=newval_c_;
      return 0}
    function caml_blit_string_c7_(s1_a_,i1_b_,s2_c_,i2_d_,len_e_)
     {if(len_e_===0)return;
      if(i2_d_===s2_c_.last&&s2_c_.bytes!=null)
       {var b_f_=s1_a_.bytes;
        if(b_f_==null)b_f_=s1_a_.toBytes();
        if(i1_b_>0||s1_a_.last>len_e_)b_f_=b_f_.slice(i1_b_,i1_b_+len_e_);
        s2_c_.bytes+=b_f_;
        s2_c_.last+=b_f_.length;
        return}
      var a_g_=s2_c_.array;
      if(!a_g_)a_g_=s2_c_.toArray();else s2_c_.bytes=s2_c_.string=null;
      s1_a_.blitToArray(i1_b_,a_g_,i2_d_,len_e_)}
    function caml_call_gen_N_(f_c_,args_b_)
     {if(f_c_.fun)return caml_call_gen_N_(f_c_.fun,args_b_);
      var n_a_=f_c_.length,d_d_=n_a_-args_b_.length;
      if(d_d_==0)
       return f_c_.apply(null,args_b_);
      else
       if(d_d_<0)
        return caml_call_gen_N_
                (f_c_.apply(null,args_b_.slice(0,n_a_)),args_b_.slice(n_a_));
       else
        return function(x_a_)
         {return caml_call_gen_N_(f_c_,args_b_.concat([x_a_]))}}
    function caml_classify_float_fh_(x_a_)
     {if(isFinite(x_a_))
       {if(Math.abs(x_a_)>=2.22507385850720138e-308)return 0;
        if(x_a_!=0)return 1;
        return 2}
      return isNaN(x_a_)?4:3}
    function caml_convert_raw_backtrace_fj_(){return 0}
    function MlMakeString_c5_(l_a_){this.bytes=str_f_;this.len=l_a_}
    MlMakeString_c5_.prototype=new MlString_q_();
    function caml_create_string_c8_(len_a_)
     {if(len_a_<0)caml_invalid_argument_ae_("String.create");
      return new MlMakeString_c5_(len_a_)}
    function caml_fill_string_fl_(s_a_,i_b_,l_c_,c_d_)
     {s_a_.fill(i_b_,l_c_,c_d_)}
    function caml_parse_format_bL_(fmt_a_)
     {fmt_a_=fmt_a_.toString();
      var len_e_=fmt_a_.length;
      if(len_e_>31)caml_invalid_argument_ae_("format_int: format too long");
      var
       f_b_=
        {justify:str_aW_,
         signstyle:str_S_,
         filler:str_G_,
         alternate:false,
         base:0,
         signedconv:false,
         width:0,
         uppercase:false,
         sign:1,
         prec:-1,
         conv:str_f_by_};
      for(var i_d_=0;i_d_<len_e_;i_d_++)
       {var c_c_=fmt_a_.charAt(i_d_);
        switch(c_c_)
         {case str_S_:f_b_.justify=str_S_;break;
          case str_aW_:
          case str_G_:f_b_.signstyle=c_c_;break;
          case str_0_u_:f_b_.filler=str_0_u_;break;
          case "#":f_b_.alternate=true;break;
          case "1":
          case "2":
          case "3":
          case "4":
          case "5":
          case "6":
          case "7":
          case "8":
          case "9":
           f_b_.width=0;
           while(c_c_=fmt_a_.charCodeAt(i_d_)-48,c_c_>=0&&c_c_<=9)
            {f_b_.width=f_b_.width*10+c_c_;i_d_++}
           i_d_--;
           break;
          case str_ac_:
           f_b_.prec=0;
           i_d_++;
           while(c_c_=fmt_a_.charCodeAt(i_d_)-48,c_c_>=0&&c_c_<=9)
            {f_b_.prec=f_b_.prec*10+c_c_;i_d_++}
           i_d_--;
          case "d":
          case "i":f_b_.signedconv=true;
          case "u":f_b_.base=10;break;
          case str_x_cQ_:f_b_.base=16;break;
          case "X":f_b_.base=16;f_b_.uppercase=true;break;
          case "o":f_b_.base=8;break;
          case str_e_ar_:
          case str_f_by_:
          case str_g_cP_:f_b_.signedconv=true;f_b_.conv=c_c_;break;
          case "E":
          case "F":
          case "G":
           f_b_.signedconv=true;
           f_b_.uppercase=true;
           f_b_.conv=c_c_.toLowerCase();
           break
          }}
      return f_b_}
    function caml_finish_formatting_bJ_(f_a_,rawbuffer_b_)
     {if(f_a_.uppercase)rawbuffer_b_=rawbuffer_b_.toUpperCase();
      var len_e_=rawbuffer_b_.length;
      if(f_a_.signedconv&&(f_a_.sign<0||f_a_.signstyle!=str_S_))len_e_++;
      if(f_a_.alternate){if(f_a_.base==8)len_e_+=1;if(f_a_.base==16)len_e_+=2}
      var buffer_c_=str_f_;
      if(f_a_.justify==str_aW_&&f_a_.filler==str_G_)
       for(var i_d_=len_e_;i_d_<f_a_.width;i_d_++)buffer_c_+=str_G_;
      if(f_a_.signedconv)
       if(f_a_.sign<0)
        buffer_c_+=str_S_;
       else
        if(f_a_.signstyle!=str_S_)buffer_c_+=f_a_.signstyle;
      if(f_a_.alternate&&f_a_.base==8)buffer_c_+=str_0_u_;
      if(f_a_.alternate&&f_a_.base==16)buffer_c_+="0x";
      if(f_a_.justify==str_aW_&&f_a_.filler==str_0_u_)
       for(var i_d_=len_e_;i_d_<f_a_.width;i_d_++)buffer_c_+=str_0_u_;
      buffer_c_+=rawbuffer_b_;
      if(f_a_.justify==str_S_)
       for(var i_d_=len_e_;i_d_<f_a_.width;i_d_++)buffer_c_+=str_G_;
      return new MlWrappedString_V_(buffer_c_)}
    function caml_format_float_fm_(fmt_a_,x_b_)
     {var
       s_c_,
       f_f_=caml_parse_format_bL_(fmt_a_),
       prec_e_=f_f_.prec<0?6:f_f_.prec;
      if(x_b_<0){f_f_.sign=-1;x_b_=-x_b_}
      if(isNaN(x_b_))
       {s_c_=str_nan_cV_;f_f_.filler=str_G_}
      else
       if(!isFinite(x_b_))
        {s_c_="inf";f_f_.filler=str_G_}
       else
        switch(f_f_.conv)
         {case str_e_ar_:
           var s_c_=x_b_.toExponential(prec_e_),i_d_=s_c_.length;
           if(s_c_.charAt(i_d_-3)==str_e_ar_)
            s_c_=s_c_.slice(0,i_d_-1)+str_0_u_+s_c_.slice(i_d_-1);
           break;
          case str_f_by_:s_c_=x_b_.toFixed(prec_e_);break;
          case str_g_cP_:
           prec_e_=prec_e_?prec_e_:1;
           s_c_=x_b_.toExponential(prec_e_-1);
           var j_i_=s_c_.indexOf(str_e_ar_),exp_h_=+s_c_.slice(j_i_+1);
           if(exp_h_<-4||x_b_.toFixed(0).length>prec_e_)
            {var i_d_=j_i_-1;
             while(s_c_.charAt(i_d_)==str_0_u_)i_d_--;
             if(s_c_.charAt(i_d_)==str_ac_)i_d_--;
             s_c_=s_c_.slice(0,i_d_+1)+s_c_.slice(j_i_);
             i_d_=s_c_.length;
             if(s_c_.charAt(i_d_-3)==str_e_ar_)
              s_c_=s_c_.slice(0,i_d_-1)+str_0_u_+s_c_.slice(i_d_-1);
             break}
           else
            {var p_g_=prec_e_;
             if(exp_h_<0)
              {p_g_-=exp_h_+1;s_c_=x_b_.toFixed(p_g_)}
             else
              while(s_c_=x_b_.toFixed(p_g_),s_c_.length>prec_e_+1)p_g_--;
             if(p_g_)
              {var i_d_=s_c_.length-1;
               while(s_c_.charAt(i_d_)==str_0_u_)i_d_--;
               if(s_c_.charAt(i_d_)==str_ac_)i_d_--;
               s_c_=s_c_.slice(0,i_d_+1)}}
           break
          }
      return caml_finish_formatting_bJ_(f_f_,s_c_)}
    function caml_format_int_fn_(fmt_a_,i_b_)
     {if(fmt_a_.toString()==str_d_cO_)
       return new MlWrappedString_V_(str_f_+i_b_);
      var f_c_=caml_parse_format_bL_(fmt_a_);
      if(i_b_<0)if(f_c_.signedconv){f_c_.sign=-1;i_b_=-i_b_}else i_b_>>>=0;
      var s_d_=i_b_.toString(f_c_.base);
      if(f_c_.prec>=0)
       {f_c_.filler=str_G_;
        var n_e_=f_c_.prec-s_d_.length;
        if(n_e_>0)s_d_=caml_str_repeat_av_(n_e_,str_0_u_)+s_d_}
      return caml_finish_formatting_bJ_(f_c_,s_d_)}
    function caml_get_exception_raw_backtrace_fp_(){return 0}
    function caml_int64_is_zero_ft_(x_a_){return (x_a_[3]|x_a_[2]|x_a_[1])==0}
    function caml_int64_of_int32_fw_(x_a_)
     {return [num_255_M_,
              x_a_&num_16777215_y_,
              x_a_>>24&num_16777215_y_,
              x_a_>>31&num_65535_aV_]}
    function caml_int64_sub_fx_(x_a_,y_b_)
     {var
       z1_c_=x_a_[1]-y_b_[1],
       z2_d_=x_a_[2]-y_b_[2]+(z1_c_>>24),
       z3_e_=x_a_[3]-y_b_[3]+(z2_d_>>24);
      return [num_255_M_,
              z1_c_&num_16777215_y_,
              z2_d_&num_16777215_y_,
              z3_e_&num_65535_aV_]}
    function caml_int64_ucompare_c__(x_a_,y_b_)
     {if(x_a_[3]>y_b_[3])return 1;
      if(x_a_[3]<y_b_[3])return -1;
      if(x_a_[2]>y_b_[2])return 1;
      if(x_a_[2]<y_b_[2])return -1;
      if(x_a_[1]>y_b_[1])return 1;
      if(x_a_[1]<y_b_[1])return -1;
      return 0}
    function caml_int64_lsl1_c9_(x_a_)
     {x_a_[3]=x_a_[3]<<1|x_a_[2]>>23;
      x_a_[2]=(x_a_[2]<<1|x_a_[1]>>23)&num_16777215_y_;
      x_a_[1]=x_a_[1]<<1&num_16777215_y_}
    function caml_int64_lsr1_fu_(x_a_)
     {x_a_[1]=(x_a_[1]>>>1|x_a_[2]<<23)&num_16777215_y_;
      x_a_[2]=(x_a_[2]>>>1|x_a_[3]<<23)&num_16777215_y_;
      x_a_[3]=x_a_[3]>>>1}
    function caml_int64_udivmod_fz_(x_a_,y_b_)
     {var
       offset_e_=0,
       modulus_d_=x_a_.slice(),
       divisor_c_=y_b_.slice(),
       quotient_f_=[num_255_M_,0,0,0];
      while(caml_int64_ucompare_c__(modulus_d_,divisor_c_)>0)
       {offset_e_++;caml_int64_lsl1_c9_(divisor_c_)}
      while(offset_e_>=0)
       {offset_e_--;
        caml_int64_lsl1_c9_(quotient_f_);
        if(caml_int64_ucompare_c__(modulus_d_,divisor_c_)>=0)
         {quotient_f_[1]++;
          modulus_d_=caml_int64_sub_fx_(modulus_d_,divisor_c_)}
        caml_int64_lsr1_fu_(divisor_c_)}
      return [0,quotient_f_,modulus_d_]}
    function caml_int64_to_int32_fy_(x_a_){return x_a_[1]|x_a_[2]<<24}
    function caml_int64_is_negative_fs_(x_a_){return x_a_[3]<<16<0}
    function caml_int64_neg_fv_(x_a_)
     {var
       y1_b_=-x_a_[1],
       y2_c_=-x_a_[2]+(y1_b_>>24),
       y3_d_=-x_a_[3]+(y2_c_>>24);
      return [num_255_M_,
              y1_b_&num_16777215_y_,
              y2_c_&num_16777215_y_,
              y3_d_&num_65535_aV_]}
    function caml_int64_format_fr_(fmt_a_,x_b_)
     {var f_c_=caml_parse_format_bL_(fmt_a_);
      if(f_c_.signedconv&&caml_int64_is_negative_fs_(x_b_))
       {f_c_.sign=-1;x_b_=caml_int64_neg_fv_(x_b_)}
      var
       buffer_d_=str_f_,
       wbase_i_=caml_int64_of_int32_fw_(f_c_.base),
       cvtbl_h_="0123456789abcdef";
      do
       {var p_g_=caml_int64_udivmod_fz_(x_b_,wbase_i_);
        x_b_=p_g_[1];
        buffer_d_=cvtbl_h_.charAt(caml_int64_to_int32_fy_(p_g_[2]))+buffer_d_}
      while
       (!caml_int64_is_zero_ft_(x_b_));
      if(f_c_.prec>=0)
       {f_c_.filler=str_G_;
        var n_e_=f_c_.prec-buffer_d_.length;
        if(n_e_>0)buffer_d_=caml_str_repeat_av_(n_e_,str_0_u_)+buffer_d_}
      return caml_finish_formatting_bJ_(f_c_,buffer_d_)}
    function caml_parse_sign_and_base_fS_(s_a_)
     {var i_b_=0,base_c_=10,sign_d_=s_a_.get(0)==45?(i_b_++,-1):1;
      if(s_a_.get(i_b_)==48)
       switch(s_a_.get(i_b_+1))
        {case num_120_bC_:
         case 88:base_c_=16;i_b_+=2;break;
         case num_111_bF_:
         case 79:base_c_=8;i_b_+=2;break;
         case 98:
         case 66:base_c_=2;i_b_+=2;break
         }
      return [i_b_,sign_d_,base_c_]}
    function caml_parse_digit_dd_(c_a_)
     {if(c_a_>=48&&c_a_<=57)return c_a_-48;
      if(c_a_>=65&&c_a_<=90)return c_a_-55;
      if(c_a_>=97&&c_a_<=122)return c_a_-87;
      return -1}
    function caml_failwith_aZ_(msg_a_)
     {caml_raise_with_string_bM_(caml_global_data_g_[3],msg_a_)}
    function caml_int_of_string_fB_(s_a_)
     {var
       r_g_=caml_parse_sign_and_base_fS_(s_a_),
       i_f_=r_g_[0],
       sign_h_=r_g_[1],
       base_d_=r_g_[2],
       threshold_i_=-1>>>0,
       c_e_=s_a_.get(i_f_),
       d_c_=caml_parse_digit_dd_(c_e_);
      if(d_c_<0||d_c_>=base_d_)caml_failwith_aZ_(str_int_of_string_aR_);
      var res_b_=d_c_;
      for(;;)
       {i_f_++;
        c_e_=s_a_.get(i_f_);
        if(c_e_==95)continue;
        d_c_=caml_parse_digit_dd_(c_e_);
        if(d_c_<0||d_c_>=base_d_)break;
        res_b_=base_d_*res_b_+d_c_;
        if(res_b_>threshold_i_)caml_failwith_aZ_(str_int_of_string_aR_)}
      if(i_f_!=s_a_.getLen())caml_failwith_aZ_(str_int_of_string_aR_);
      res_b_=sign_h_*res_b_;
      if(base_d_==10&&(res_b_|0)!=res_b_)
       caml_failwith_aZ_(str_int_of_string_aR_);
      return res_b_|0}
    function caml_is_printable_fC_(c_a_){return +(c_a_>31&&c_a_<127)}
    function caml_js_call_fD_(f_a_,o_b_,args_c_)
     {return f_a_.apply(o_b_,args_c_.slice(1))}
    function caml_js_get_console_fE_()
     {var
       c_b_=joo_global_object_A_.console?joo_global_object_A_.console:{},
       m_c_=
        ["log",
         "debug",
         "info",
         "warn",
         "error",
         "assert",
         "dir",
         "dirxml",
         "trace",
         "group",
         "groupCollapsed",
         "groupEnd",
         "time",
         "timeEnd"];
      function f_d_(){}
      for(var i_a_=0;i_a_<m_c_.length;i_a_++)
       if(!c_b_[m_c_[i_a_]])c_b_[m_c_[i_a_]]=f_d_;
      return c_b_}
    var caml_js_regexps_a0_={amp:/&/g,lt:/</g,quot:/\"/g,all:/[&<\"]/};
    function caml_js_html_escape_fF_(s_a_)
     {if(!caml_js_regexps_a0_.all.test(s_a_))return s_a_;
      return s_a_.replace(caml_js_regexps_a0_.amp,"&amp;").replace
               (caml_js_regexps_a0_.lt,"&lt;").replace
              (caml_js_regexps_a0_.quot,"&quot;")}
    function caml_js_wrap_callback_fG_(f_a_)
     {var toArray_c_=Array.prototype.slice;
      return function()
       {var args_b_=arguments.length>0?toArray_c_.call(arguments):[undefined];
        return caml_call_gen_N_(f_a_,args_b_)}}
    function caml_make_vect_fH_(len_a_,init_b_)
     {var b_d_=[0];
      for(var i_c_=1;i_c_<=len_a_;i_c_++)b_d_[i_c_]=init_b_;
      return b_d_}
    function caml_raise_sys_error_v_(msg_a_)
     {caml_raise_with_string_bM_(caml_global_data_g_[2],msg_a_)}
    function caml_ml_flush_c$_(oc_a_)
     {if(!oc_a_.opened)
       caml_raise_sys_error_v_("Cannot flush a closed channel");
      if(oc_a_.buffer==str_f_)return 0;
      if(oc_a_.output)
       switch(oc_a_.output.length)
        {case 2:oc_a_.output(oc_a_,oc_a_.buffer);break;
         default:oc_a_.output(oc_a_.buffer)}
      oc_a_.buffer=str_f_}
    function caml_raise_no_such_file_df_(name_a_)
     {name_a_=name_a_ instanceof MlString_q_?name_a_.toString():name_a_;
      caml_raise_sys_error_v_(name_a_+": No such file or directory")}
    var caml_current_dir_fk_=str_bE_;
    function caml_make_path_a1_(name_a_)
     {name_a_=name_a_ instanceof MlString_q_?name_a_.toString():name_a_;
      if(name_a_.charCodeAt(0)!=47)name_a_=caml_current_dir_fk_+name_a_;
      var comp_d_=name_a_.split(str_bE_),ncomp_b_=[];
      for(var i_c_=0;i_c_<comp_d_.length;i_c_++)
       switch(comp_d_[i_c_])
        {case "..":if(ncomp_b_.length>1)ncomp_b_.pop();break;
         case str_ac_:
         case str_f_:if(ncomp_b_.length==0)ncomp_b_.push(str_f_);break;
         default:ncomp_b_.push(comp_d_[i_c_]);break}
      ncomp_b_.orig=name_a_;
      return ncomp_b_}
    function MlDir_U_(){this.content={}}
    MlDir_U_.prototype=
    {exists:function(name_a_){return this.content[name_a_]?1:0},
     mk:function(name_a_,c_b_){this.content[name_a_]=c_b_},
     get:function(name_a_){return this.content[name_a_]},
     list:
     function()
      {var a_a_=[];for(var n_b_ in this.content)a_a_.push(n_b_);return a_a_},
     remove:function(name_a_){delete this.content[name_a_]}};
    var caml_root_dir_a3_=new MlDir_U_();
    caml_root_dir_a3_.mk(str_f_,new MlDir_U_());
    function caml_fs_content_bK_(path_a_)
     {var dir_b_=caml_root_dir_a3_;
      for(var i_c_=0;i_c_<path_a_.length;i_c_++)
       {if(!(dir_b_.exists&&dir_b_.exists(path_a_[i_c_])))
         caml_raise_no_such_file_df_(path_a_.orig);
        dir_b_=dir_b_.get(path_a_[i_c_])}
      return dir_b_}
    function caml_sys_is_directory_f2_(name_a_)
     {var
       path_c_=caml_make_path_a1_(name_a_),
       dir_b_=caml_fs_content_bK_(path_c_);
      return dir_b_ instanceof MlDir_U_?1:0}
    function MlFile_au_(content_a_){this.data=content_a_}
    MlFile_au_.prototype=
    {content:function(){return this.data},
     truncate:function(){this.data.length=0}};
    function caml_fs_register_fo_(name_a_,content_b_)
     {var path_e_=caml_make_path_a1_(name_a_),dir_c_=caml_root_dir_a3_;
      for(var i_f_=0;i_f_<path_e_.length-1;i_f_++)
       {var d_d_=path_e_[i_f_];
        if(!dir_c_.exists(d_d_))dir_c_.mk(d_d_,new MlDir_U_());
        dir_c_=dir_c_.get(d_d_);
        if(!(dir_c_ instanceof MlDir_U_))
         caml_raise_sys_error_v_(path_e_.orig+str_file_already_abr_bI_)}
      var d_d_=path_e_[path_e_.length-1];
      if(dir_c_.exists(d_d_))
       caml_raise_sys_error_v_(path_e_.orig+str_file_already_abr_bI_);
      if(content_b_ instanceof MlDir_U_)
       dir_c_.mk(d_d_,content_b_);
      else
       if(content_b_ instanceof MlFile_au_)
        dir_c_.mk(d_d_,content_b_);
       else
        if(content_b_ instanceof MlString_q_)
         dir_c_.mk(d_d_,new MlFile_au_(content_b_.getArray()));
        else
         if(content_b_ instanceof Array)
          dir_c_.mk(d_d_,new MlFile_au_(content_b_));
         else
          if(content_b_.toString)
           dir_c_.mk
            (d_d_,
             new MlFile_au_(new MlString_q_(content_b_.toString()).getArray()));
          else
           caml_invalid_argument_ae_("caml_fs_register")}
    function caml_sys_file_exists_f0_(name_a_)
     {var
       dir_b_=caml_root_dir_a3_,
       path_d_=caml_make_path_a1_(name_a_),
       auto_load_e_;
      for(var i_c_=0;i_c_<path_d_.length;i_c_++)
       {if(dir_b_.auto)auto_load_e_=dir_b_.auto;
        if(!(dir_b_.exists&&dir_b_.exists(path_d_[i_c_])))
         return auto_load_e_?auto_load_e_(path_d_.join(str_bE_)):0;
        dir_b_=dir_b_.get(path_d_[i_c_])}
      return 1}
    function caml_sys_open_internal_aw_(idx_a_,v_b_,flags_c_)
     {if(caml_global_data_g_.fds===undefined)
       caml_global_data_g_.fds=new Array();
      flags_c_=flags_c_?flags_c_:{};
      var data_d_={};
      data_d_.array=v_b_;
      data_d_.offset=flags_c_.append?data_d_.array.length:0;
      data_d_.flags=flags_c_;
      caml_global_data_g_.fds[idx_a_]=data_d_;
      caml_global_data_g_.fd_last_idx=idx_a_;
      return idx_a_}
    function caml_sys_open_f__(name_a_,flags_b_,perms_c_)
     {var f_d_={};
      while(flags_b_)
       {switch(flags_b_[1])
         {case 0:f_d_.rdonly=1;break;
          case 1:f_d_.wronly=1;break;
          case 2:f_d_.append=1;break;
          case 3:f_d_.create=1;break;
          case 4:f_d_.truncate=1;break;
          case 5:f_d_.excl=1;break;
          case 6:f_d_.binary=1;break;
          case 7:f_d_.text=1;break;
          case 8:f_d_.nonblock=1;break
          }
        flags_b_=flags_b_[2]}
      var name2_f_=name_a_.toString(),path_i_=caml_make_path_a1_(name_a_);
      if(f_d_.rdonly&&f_d_.wronly)
       caml_raise_sys_error_v_
        (name2_f_+" : flags Open_rdonly and Open_wronly are not compatible");
      if(f_d_.text&&f_d_.binary)
       caml_raise_sys_error_v_
        (name2_f_+" : flags Open_text and Open_binary are not compatible");
      if(caml_sys_file_exists_f0_(name_a_))
       {if(caml_sys_is_directory_f2_(name_a_))
         caml_raise_sys_error_v_(name2_f_+" : is a directory");
        if(f_d_.create&&f_d_.excl)
         caml_raise_sys_error_v_(name2_f_+str_file_already_abr_bI_);
        var
         idx_h_=
          caml_global_data_g_.fd_last_idx?caml_global_data_g_.fd_last_idx:0,
         file_e_=caml_fs_content_bK_(path_i_);
        if(f_d_.truncate)file_e_.truncate();
        return caml_sys_open_internal_aw_(idx_h_+1,file_e_.content(),f_d_)}
      else
       if(f_d_.create)
        {var
          idx_h_=
           caml_global_data_g_.fd_last_idx?caml_global_data_g_.fd_last_idx:0;
         caml_fs_register_fo_(name_a_,[]);
         var file_e_=caml_fs_content_bK_(path_i_);
         return caml_sys_open_internal_aw_(idx_h_+1,file_e_.content(),f_d_)}
       else
        caml_raise_no_such_file_df_(name_a_)}
    caml_sys_open_internal_aw_(0,[]);
    caml_sys_open_internal_aw_(1,[]);
    caml_sys_open_internal_aw_(2,[]);
    function caml_ml_open_descriptor_in_fI_(fd_a_)
     {var data_b_=caml_global_data_g_.fds[fd_a_];
      if(data_b_.flags.wronly)
       caml_raise_sys_error_v_(str_fd_c2_+fd_a_+" is writeonly");
      return {data:data_b_,fd:fd_a_,opened:true}}
    function js_print_stdout_f7_(s_a_)
     {if(s_a_.charCodeAt(s_a_.length-1)==10)s_a_=s_a_.substr(0,s_a_.length-1);
      var v_b_=joo_global_object_A_.console;
      v_b_&&v_b_.log&&v_b_.log(s_a_)}
    var caml_ml_out_channels_a2_=new Array();
    function caml_std_output_fX_(chan_a_,s_b_)
     {var str_e_=new MlString_q_(s_b_),slen_d_=str_e_.getLen();
      for(var i_c_=0;i_c_<slen_d_;i_c_++)
       chan_a_.data.array[chan_a_.data.offset+i_c_]=str_e_.get(i_c_);
      chan_a_.data.offset+=slen_d_;
      return 0}
    function caml_ml_open_descriptor_out_fJ_(fd_a_)
     {var output_b_;
      switch(fd_a_)
       {case 1:output_b_=js_print_stdout_f7_;break;
        case 2:output_b_=js_print_stderr_bN_;break;
        default:output_b_=caml_std_output_fX_}
      var data_d_=caml_global_data_g_.fds[fd_a_];
      if(data_d_.flags.rdonly)
       caml_raise_sys_error_v_(str_fd_c2_+fd_a_+" is readonly");
      var
       channel_c_=
        {data:data_d_,fd:fd_a_,opened:true,buffer:str_f_,output:output_b_};
      caml_ml_out_channels_a2_[channel_c_.fd]=channel_c_;
      return channel_c_}
    function caml_ml_out_channels_list_fK_()
     {var l_a_=0;
      for(var c_b_ in caml_ml_out_channels_a2_)
       if(caml_ml_out_channels_a2_[c_b_].opened)
        l_a_=[0,caml_ml_out_channels_a2_[c_b_],l_a_];
      return l_a_}
    function caml_ml_output_da_(oc_a_,buffer_b_,offset_c_,len_d_)
     {if(!oc_a_.opened)
       caml_raise_sys_error_v_("Cannot output to a closed channel");
      var string_f_;
      if(offset_c_==0&&buffer_b_.getLen()==len_d_)
       string_f_=buffer_b_;
      else
       {string_f_=caml_create_string_c8_(len_d_);
        caml_blit_string_c7_(buffer_b_,offset_c_,string_f_,0,len_d_)}
      var
       jsstring_e_=string_f_.toString(),
       id_g_=jsstring_e_.lastIndexOf("\n");
      if(id_g_<0)
       oc_a_.buffer+=jsstring_e_;
      else
       {oc_a_.buffer+=jsstring_e_.substr(0,id_g_+1);
        caml_ml_flush_c$_(oc_a_);
        oc_a_.buffer+=jsstring_e_.substr(id_g_+1)}}
    function caml_new_string_dc_(x_a_){return new MlString_q_(x_a_)}
    function caml_ml_output_char_fL_(oc_a_,c_b_)
     {var s_c_=caml_new_string_dc_(String.fromCharCode(c_b_));
      caml_ml_output_da_(oc_a_,s_c_,0,1)}
    function caml_raise_constant_de_(tag_a_){throw [0,tag_a_]}
    function caml_raise_zero_divide_fU_()
     {caml_raise_constant_de_(caml_global_data_g_[6])}
    function caml_mod_fM_(x_a_,y_b_)
     {if(y_b_==0)caml_raise_zero_divide_fU_();return x_a_%y_b_}
    if(!Math.imul)
     Math.imul=
     function(x_a_,y_b_)
      {return ((x_a_>>16)*y_b_<<16)+(x_a_&num_65535_aV_)*y_b_|0};
    var caml_mul_fN_=Math.imul;
    function caml_int64_compare_fq_(x_a_,y_b_)
     {var x3_c_=x_a_[3]<<16,y3_d_=y_b_[3]<<16;
      if(x3_c_>y3_d_)return 1;
      if(x3_c_<y3_d_)return -1;
      if(x_a_[2]>y_b_[2])return 1;
      if(x_a_[2]<y_b_[2])return -1;
      if(x_a_[1]>y_b_[1])return 1;
      if(x_a_[1]<y_b_[1])return -1;
      return 0}
    function caml_int_compare_fA_(a_a_,b_b_)
     {if(a_a_<b_b_)return -1;if(a_a_==b_b_)return 0;return 1}
    function caml_compare_val_fi_(a_a_,b_b_,total_c_)
     {var stack_e_=[];
      for(;;)
       {if(!(total_c_&&a_a_===b_b_))
         if(a_a_ instanceof MlString_q_)
          if(b_b_ instanceof MlString_q_)
           {if(a_a_!==b_b_)
             {var x_d_=a_a_.compare(b_b_);if(x_d_!=0)return x_d_}}
          else
           return 1;
         else
          if(a_a_ instanceof Array&&a_a_[0]===(a_a_[0]|0))
           {var ta_f_=a_a_[0];
            if(ta_f_===num_254_z_)ta_f_=0;
            if(ta_f_===num_250_cY_)
             {a_a_=a_a_[1];continue}
            else
             if(b_b_ instanceof Array&&b_b_[0]===(b_b_[0]|0))
              {var tb_g_=b_b_[0];
               if(tb_g_===num_254_z_)tb_g_=0;
               if(tb_g_===num_250_cY_)
                {b_b_=b_b_[1];continue}
               else
                if(ta_f_!=tb_g_)
                 return ta_f_<tb_g_?-1:1;
                else
                 switch(ta_f_)
                  {case 248:
                    var x_d_=caml_int_compare_fA_(a_a_[2],b_b_[2]);
                    if(x_d_!=0)return x_d_;
                    break;
                   case 251:caml_invalid_argument_ae_("equal: abstract value");
                   case num_255_M_:
                    var x_d_=caml_int64_compare_fq_(a_a_,b_b_);
                    if(x_d_!=0)return x_d_;
                    break;
                   default:
                    if(a_a_.length!=b_b_.length)
                     return a_a_.length<b_b_.length?-1:1;
                    if(a_a_.length>1)stack_e_.push(a_a_,b_b_,1)}}
             else
              return 1}
          else
           if
            (b_b_ instanceof MlString_q_||
             b_b_ instanceof Array&&
             b_b_[0]===
             (b_b_[0]|0))
            return -1;
           else
            {if(a_a_<b_b_)return -1;
             if(a_a_>b_b_)return 1;
             if(a_a_!=b_b_)
              {if(!total_c_)return NaN;
               if(a_a_==a_a_)return 1;
               if(b_b_==b_b_)return -1}}
        if(stack_e_.length==0)return 0;
        var i_h_=stack_e_.pop();
        b_b_=stack_e_.pop();
        a_a_=stack_e_.pop();
        if(i_h_+1<a_a_.length)stack_e_.push(a_a_,b_b_,i_h_+1);
        a_a_=a_a_[i_h_];
        b_b_=b_b_[i_h_]}}
    function caml_notequal_fP_(x_a_,y_b_)
     {return +(caml_compare_val_fi_(x_a_,y_b_,false)!=0)}
    function caml_obj_is_block_fQ_(x_a_){return +(x_a_ instanceof Array)}
    function caml_obj_tag_fR_(x_a_)
     {return x_a_ instanceof Array?x_a_[0]:num_1e3_ap_}
    function caml_register_global_fV_(n_a_,v_b_)
     {caml_global_data_g_[n_a_+1]=v_b_}
    var caml_named_values_db_={};
    function caml_register_named_value_fW_(nm_a_,v_b_)
     {caml_named_values_db_[nm_a_.toString()]=v_b_;return 0}
    function caml_sys_const_word_size_fY_(){return 32}
    function caml_sys_exit_fZ_()
     {caml_invalid_argument_ae_("Function 'exit' not implemented")}
    function caml_raise_not_found_fT_()
     {caml_raise_constant_de_(caml_global_data_g_[7])}
    function caml_sys_getenv_f1_(){caml_raise_not_found_fT_()}
    function caml_trampoline_f3_(res_a_)
     {var c_b_=1;
      while(res_a_&&res_a_.joo_tramp)
       {res_a_=res_a_.joo_tramp.apply(null,res_a_.joo_args);c_b_++}
      return res_a_}
    function caml_trampoline_return_f4_(f_a_,args_b_)
     {return {joo_tramp:f_a_,joo_args:args_b_}}
    function caml_update_dummy_f5_(x_a_,y_b_)
     {if(typeof y_b_==="function"){x_a_.fun=y_b_;return 0}
      if(y_b_.fun){x_a_.fun=y_b_.fun;return 0}
      var i_c_=y_b_.length;
      while(i_c_--)x_a_[i_c_]=y_b_[i_c_];
      return 0}
    function caml_named_value_fO_(nm_a_){return caml_named_values_db_[nm_a_]}
    function caml_wrap_exception_f6_(e_a_)
     {if(e_a_ instanceof Array)return e_a_;
      if
       (joo_global_object_A_.RangeError&&
        e_a_ instanceof joo_global_object_A_.RangeError&&
        e_a_.message&&
        e_a_.message.match(/maximum call stack/i))
       return [0,caml_global_data_g_[9]];
      if
       (joo_global_object_A_.InternalError&&
        e_a_ instanceof joo_global_object_A_.InternalError&&
        e_a_.message&&
        e_a_.message.match(/too much recursion/i))
       return [0,caml_global_data_g_[9]];
      if(e_a_ instanceof joo_global_object_A_.Error)
       return [0,caml_named_value_fO_(str_jsError_cS_),e_a_];
      return [0,caml_global_data_g_[3],new MlWrappedString_V_(String(e_a_))]}
    var
     caml_array_get_k_=caml_array_get_ff_,
     caml_array_set_h_=caml_array_set_fg_,
     caml_blit_string_aM_=caml_blit_string_c7_,
     caml_create_string_L_=caml_create_string_c8_,
     caml_format_float_bs_=caml_format_float_fm_,
     caml_format_int_aN_=caml_format_int_fn_,
     caml_is_printable_bt_=caml_is_printable_fC_,
     caml_js_html_escape_cL_=caml_js_html_escape_fF_,
     caml_js_wrap_callback_bw_=caml_js_wrap_callback_fG_,
     caml_make_vect_x_=caml_make_vect_fH_,
     caml_ml_flush_cG_=caml_ml_flush_c$_,
     caml_ml_open_descriptor_out_cF_=caml_ml_open_descriptor_out_fJ_,
     caml_ml_output_char_cI_=caml_ml_output_char_fL_,
     caml_mod_ao_=caml_mod_fM_,
     caml_new_string_b_=caml_new_string_dc_,
     caml_obj_tag_cJ_=caml_obj_tag_fR_,
     caml_register_global_D_=caml_register_global_fV_,
     caml_register_named_value_cH_=caml_register_named_value_fW_,
     caml_sys_getenv_cK_=caml_sys_getenv_f1_,
     caml_trampoline_aQ_=caml_trampoline_f3_,
     caml_trampoline_return_F_=caml_trampoline_return_f4_,
     caml_wrap_exception_aa_=caml_wrap_exception_f6_;
    function caml_call_gen1_i_(fun_a_,var0_b_)
     {return fun_a_.length==1
              ?fun_a_(var0_b_)
              :caml_call_gen_N_(fun_a_,[var0_b_])}
    function caml_call_gen2_n_(fun_a_,var0_b_,var1_c_)
     {return fun_a_.length==2
              ?fun_a_(var0_b_,var1_c_)
              :caml_call_gen_N_(fun_a_,[var0_b_,var1_c_])}
    function caml_call_gen3_p_(fun_a_,var0_b_,var1_c_,var2_d_)
     {return fun_a_.length==3
              ?fun_a_(var0_b_,var1_c_,var2_d_)
              :caml_call_gen_N_(fun_a_,[var0_b_,var1_c_,var2_d_])}
    function caml_call_gen5_aO_
     (fun_a_,var0_b_,var1_c_,var2_d_,var3_e_,var4_f_)
     {return fun_a_.length==5
              ?fun_a_(var0_b_,var1_c_,var2_d_,var3_e_,var4_f_)
              :caml_call_gen_N_
                (fun_a_,[var0_b_,var1_c_,var2_d_,var3_e_,var4_f_])}
    var
     _a5_=[0,caml_new_string_b_("Failure")],
     _bO_=[0,caml_new_string_b_("Invalid_argument")],
     _ag_=[0,caml_new_string_b_("Not_found")],
     _b__=[0,caml_new_string_b_("Match_failure")],
     _b9_=[0,caml_new_string_b_("Stack_overflow")],
     _r_=[0,caml_new_string_b_("Assert_failure")],
     _b$_=[0,caml_new_string_b_("Undefined_recursive_module")],
     _bc_=caml_new_string_b_('File "%s", line %d, characters %d-%d: %s');
    caml_register_global_D_(11,_b$_);
    caml_register_global_D_(8,_b9_);
    caml_register_global_D_(7,_b__);
    caml_register_global_D_(6,_ag_);
    caml_register_global_D_(5,[0,caml_new_string_b_("Division_by_zero")]);
    caml_register_global_D_(4,[0,caml_new_string_b_("End_of_file")]);
    caml_register_global_D_(3,_bO_);
    caml_register_global_D_(2,_a5_);
    caml_register_global_D_(1,[0,caml_new_string_b_("Sys_error")]);
    var
     _d6_=[0,caml_new_string_b_("Out_of_memory")],
     _dl_=caml_new_string_b_(str_12g_cU_),
     _dk_=caml_new_string_b_(str_ac_),
     _di_=caml_new_string_b_("true"),
     _dj_=caml_new_string_b_("false"),
     _dm_=caml_new_string_b_("Pervasives.do_at_exit"),
     _dq_=caml_new_string_b_("\\b"),
     _dr_=caml_new_string_b_("\\t"),
     _ds_=caml_new_string_b_("\\n"),
     _dt_=caml_new_string_b_("\\r"),
     _dp_=caml_new_string_b_("\\\\"),
     _do_=caml_new_string_b_("\\'"),
     _dw_=caml_new_string_b_("String.contains_from"),
     _dv_=caml_new_string_b_("String.blit"),
     _du_=caml_new_string_b_("String.sub"),
     _dx_=caml_new_string_b_("Queue.Empty"),
     _dz_=caml_new_string_b_("Buffer.add: cannot grow buffer"),
     _dP_=caml_new_string_b_(str_f_),
     _dQ_=caml_new_string_b_(str_f_),
     _dT_=caml_new_string_b_(str_12g_cU_),
     _dU_=caml_new_string_b_(str_aT_),
     _dV_=caml_new_string_b_(str_aT_),
     _dR_=caml_new_string_b_(str_aS_),
     _dS_=caml_new_string_b_(str_aS_),
     _dO_=caml_new_string_b_(str_nan_cV_),
     _dM_=caml_new_string_b_("neg_infinity"),
     _dN_=caml_new_string_b_("infinity"),
     _dL_=caml_new_string_b_(str_ac_),
     _dK_=caml_new_string_b_("printf: bad positional specification (0)."),
     _dJ_=caml_new_string_b_("%_"),
     _dI_=[0,caml_new_string_b_("printf.ml"),143,8],
     _dG_=caml_new_string_b_(str_aS_),
     _dH_=caml_new_string_b_("Printf: premature end of format string '"),
     _dC_=caml_new_string_b_(str_aS_),
     _dD_=caml_new_string_b_(" in format string '"),
     _dE_=caml_new_string_b_(", at char number "),
     _dF_=caml_new_string_b_("Printf: bad conversion %"),
     _dA_=caml_new_string_b_("Sformat.index_of_int: negative argument "),
     _d0_=caml_new_string_b_(str_f_),
     _d1_=caml_new_string_b_(", %s%s"),
     _eg_=[1,1],
     _eh_=caml_new_string_b_("%s\n"),
     _ei_=
      caml_new_string_b_
       ("(Program not linked with -g, cannot print stack backtrace)\n"),
     _ea_=caml_new_string_b_("Raised at"),
     _ed_=caml_new_string_b_("Re-raised at"),
     _ee_=caml_new_string_b_("Raised by primitive operation at"),
     _ef_=caml_new_string_b_("Called from"),
     _eb_=caml_new_string_b_('%s file "%s", line %d, characters %d-%d'),
     _ec_=caml_new_string_b_("%s unknown location"),
     _d7_=caml_new_string_b_("Out of memory"),
     _d8_=caml_new_string_b_("Stack overflow"),
     _d9_=caml_new_string_b_("Pattern matching failed"),
     _d__=caml_new_string_b_("Assertion failed"),
     _d$_=caml_new_string_b_("Undefined recursive module"),
     _d2_=caml_new_string_b_("(%s%s)"),
     _d3_=caml_new_string_b_(str_f_),
     _d4_=caml_new_string_b_(str_f_),
     _d5_=caml_new_string_b_("(%s)"),
     _dZ_=caml_new_string_b_(str_d_cO_),
     _dX_=caml_new_string_b_("%S"),
     _dY_=caml_new_string_b_("_"),
     _fd_=caml_new_string_b_("OCAMLRUNPARAM"),
     _fb_=caml_new_string_b_("CAMLRUNPARAM"),
     _ej_=caml_new_string_b_(str_f_),
     _eu_=[0,caml_new_string_b_(str_src_core_lwt_ml_ad_),814,20],
     _ev_=[0,caml_new_string_b_(str_src_core_lwt_ml_ad_),816,8],
     _es_=[0,caml_new_string_b_(str_src_core_lwt_ml_ad_),648,20],
     _et_=[0,caml_new_string_b_(str_src_core_lwt_ml_ad_),651,8],
     _er_=[0,caml_new_string_b_(str_src_core_lwt_ml_ad_),498,8],
     _eq_=[0,caml_new_string_b_(str_src_core_lwt_ml_ad_),487,9],
     _ep_=caml_new_string_b_("Lwt.wakeup_result"),
     _em_=caml_new_string_b_("Fatal error: exception "),
     _el_=caml_new_string_b_("Lwt.Canceled"),
     _ez_=caml_new_string_b_("Js.Error"),
     name_eA_=caml_new_string_b_(str_jsError_cS_),
     _eP_=caml_new_string_b_("canvas"),
     _eM_=caml_new_string_b_("img"),
     _eL_=caml_new_string_b_("br"),
     _eK_=caml_new_string_b_("p"),
     _eJ_=caml_new_string_b_("div"),
     _eI_=caml_new_string_b_("label"),
     _eH_=caml_new_string_b_(str_input_cM_),
     _eG_=caml_new_string_b_("select"),
     _eF_=caml_new_string_b_("option"),
     _eB_=caml_new_string_b_("mouseup"),
     _eD_=caml_new_string_b_("mousemove"),
     _eN_=caml_new_string_b_("Dom_html.Canvas_not_available"),
     _eS_=caml_new_string_b_("Exception during Lwt.async: "),
     _fa_=caml_new_string_b_("% 2.f"),
     _e5_=caml_new_string_b_("Resume"),
     _e6_=caml_new_string_b_("Pause"),
     _e7_=caml_new_string_b_("Fixed position"),
     _e8_=caml_new_string_b_("Follow rotation"),
     param_e9_=
      [0,
       caml_new_string_b_("December solstice"),
       [0,
        caml_new_string_b_("Equinox"),
        [0,caml_new_string_b_("June solstice"),0]]],
     _e__=caml_new_string_b_("Lighting"),
     _e$_=caml_new_string_b_("Clip"),
     _e2_=[0,caml_new_string_b_(str_planet_ml_aY_),415,0],
     _e1_=[0,caml_new_string_b_(str_planet_ml_aY_),416,0],
     _e0_=[0,caml_new_string_b_(str_planet_ml_aY_),417,0],
     _eZ_=[0,caml_new_string_b_(str_planet_ml_aY_),418,0],
     v_e3_=[num_254_z_,0,0,1];
    function failwith_a4_(s_a_){throw [0,_a5_,s_a_]}
    function invalid_arg_W_(s_a_){throw [0,_bO_,s_a_]}
    function _j_(s1_a_,s2_b_)
     {var
       l1_c_=s1_a_.getLen(),
       l2_e_=s2_b_.getLen(),
       s_d_=caml_create_string_L_(l1_c_+l2_e_|0);
      caml_blit_string_aM_(s1_a_,0,s_d_,0,l1_c_);
      caml_blit_string_aM_(s2_b_,0,s_d_,l1_c_,l2_e_);
      return s_d_}
    function string_of_int_a6_(n_a_){return caml_new_string_b_(str_f_+n_a_)}
    caml_ml_open_descriptor_in_fI_(0);
    caml_ml_open_descriptor_out_cF_(1);
    var stderr_af_=caml_ml_open_descriptor_out_cF_(2);
    function output_string_bP_(oc_a_,s_b_)
     {return caml_ml_output_da_(oc_a_,s_b_,0,s_b_.getLen())}
    function prerr_string_bQ_(s_a_){return output_string_bP_(stderr_af_,s_a_)}
    function do_at_exit_a7_(param_a_)
     {var param_b_=caml_ml_out_channels_list_fK_(0);
      for(;;)
       {if(param_b_)
         {var l_c_=param_b_[2],a_d_=param_b_[1];
          try {caml_ml_flush_cG_(a_d_)}catch(_f_){}
          var param_b_=l_c_;
          continue}
        return 0}}
    caml_register_named_value_cH_(_dm_,do_at_exit_a7_);
    function _dn_(_a_,_b_){return caml_ml_output_char_cI_(_a_,_b_)}
    function _bR_(_a_){return caml_ml_flush_cG_(_a_)}
    function _ax_(f_a_,a_b_)
     {var l_d_=a_b_.length-1;
      if(0===l_d_)return [0];
      var
       r_e_=caml_make_vect_x_(l_d_,caml_call_gen1_i_(f_a_,a_b_[0+1])),
       _f_=l_d_-1|0,
       _g_=1;
      if(!(_f_<1))
       {var i_c_=_g_;
        for(;;)
         {r_e_[i_c_+1]=caml_call_gen1_i_(f_a_,a_b_[i_c_+1]);
          var _h_=i_c_+1|0;
          if(_f_!==i_c_){var i_c_=_h_;continue}
          break}}
      return r_e_}
    function _ay_(n_a_,c_b_)
     {var s_c_=caml_create_string_L_(n_a_);
      caml_fill_string_fl_(s_c_,0,n_a_,c_b_);
      return s_c_}
    function _az_(s_a_,ofs_b_,len_c_)
     {if(0<=ofs_b_)
       if(0<=len_c_)
        if(!((s_a_.getLen()-len_c_|0)<ofs_b_))
         {var r_d_=caml_create_string_L_(len_c_);
          caml_blit_string_aM_(s_a_,ofs_b_,r_d_,0,len_c_);
          return r_d_}
      return invalid_arg_W_(_du_)}
    function _aA_(s1_a_,ofs1_b_,s2_c_,ofs2_d_,len_e_)
     {if(0<=len_e_)
       if(0<=ofs1_b_)
        if(!((s1_a_.getLen()-len_e_|0)<ofs1_b_))
         if(0<=ofs2_d_)
          if(!((s2_c_.getLen()-len_e_|0)<ofs2_d_))
           return caml_blit_string_aM_(s1_a_,ofs1_b_,s2_c_,ofs2_d_,len_e_);
      return invalid_arg_W_(_dv_)}
    var
     _a8_=caml_sys_const_word_size_fY_(0),
     _ah_=caml_mul_fN_(_a8_/8|0,(1<<(_a8_-10|0))-1|0)-1|0,
     _dy_=[0,_dx_];
    function _a9_(n_a_)
     {var
       n_b_=1<=n_a_?n_a_:1,
       n_c_=_ah_<n_b_?_ah_:n_b_,
       s_d_=caml_create_string_L_(n_c_);
      return [0,s_d_,0,n_c_,s_d_]}
    function _a__(b_a_){return _az_(b_a_[1],0,b_a_[2])}
    function _bU_(b_a_,more_b_)
     {var new_len_c_=[0,b_a_[3]];
      for(;;)
       {if(new_len_c_[1]<(b_a_[2]+more_b_|0))
         {new_len_c_[1]=2*new_len_c_[1]|0;continue}
        if(_ah_<new_len_c_[1])
         if((b_a_[2]+more_b_|0)<=_ah_)
          new_len_c_[1]=_ah_;
         else
          failwith_a4_(_dz_);
        var new_buffer_d_=caml_create_string_L_(new_len_c_[1]);
        _aA_(b_a_[1],0,new_buffer_d_,0,b_a_[2]);
        b_a_[1]=new_buffer_d_;
        b_a_[3]=new_len_c_[1];
        return 0}}
    function _ai_(b_a_,c_b_)
     {var pos_c_=b_a_[2];
      if(b_a_[3]<=pos_c_)_bU_(b_a_,1);
      b_a_[1].safeSet(pos_c_,c_b_);
      b_a_[2]=pos_c_+1|0;
      return 0}
    function _a$_(b_a_,s_b_)
     {var len_c_=s_b_.getLen(),new_position_d_=b_a_[2]+len_c_|0;
      if(b_a_[3]<new_position_d_)_bU_(b_a_,len_c_);
      _aA_(s_b_,0,b_a_[1],b_a_[2],len_c_);
      b_a_[2]=new_position_d_;
      return 0}
    function index_of_int_ba_(i_a_)
     {return 0<=i_a_?i_a_:failwith_a4_(_j_(_dA_,string_of_int_a6_(i_a_)))}
    function add_int_index_bV_(i_a_,idx_b_)
     {return index_of_int_ba_(i_a_+idx_b_|0)}
    var _dB_=1;
    function _bW_(_a_){return add_int_index_bV_(_dB_,_a_)}
    function _bX_(fmt_a_){return _az_(fmt_a_,0,fmt_a_.getLen())}
    function bad_conversion_bY_(sfmt_a_,i_b_,c_c_)
     {var
       _d_=_j_(_dD_,_j_(sfmt_a_,_dC_)),
       _e_=_j_(_dE_,_j_(string_of_int_a6_(i_b_),_d_));
      return invalid_arg_W_(_j_(_dF_,_j_(_ay_(1,c_c_),_e_)))}
    function bad_conversion_format_aj_(fmt_a_,i_b_,c_c_)
     {return bad_conversion_bY_(_bX_(fmt_a_),i_b_,c_c_)}
    function incomplete_format_aC_(fmt_a_)
     {return invalid_arg_W_(_j_(_dH_,_j_(_bX_(fmt_a_),_dG_)))}
    function extract_format_O_(fmt_f_,start_b_,stop_c_,widths_d_)
     {function skip_positional_spec_j_(start_a_)
       {if
         ((fmt_f_.safeGet(start_a_)+num_48_ab_|0)<
          0||
          9<
          (fmt_f_.safeGet(start_a_)+num_48_ab_|0))
         return start_a_;
        var i_b_=start_a_+1|0;
        for(;;)
         {var match_c_=fmt_f_.safeGet(i_b_);
          if(48<=match_c_)
           {if(!(58<=match_c_)){var i_b_=i_b_+1|0;continue}}
          else
           if(36===match_c_)return i_b_+1|0;
          return start_a_}}
      var
       i_k_=skip_positional_spec_j_(start_b_+1|0),
       b_g_=_a9_((stop_c_-i_k_|0)+10|0);
      _ai_(b_g_,37);
      var l1_e_=widths_d_,l2_i_=0;
      for(;;)
       {if(l1_e_)
         {var _m_=[0,l1_e_[1],l2_i_],l1_e_=l1_e_[2],l2_i_=_m_;continue}
        var i_a_=i_k_,widths_h_=l2_i_;
        for(;;)
         {if(i_a_<=stop_c_)
           {var c_l_=fmt_f_.safeGet(i_a_);
            if(42===c_l_)
             {if(widths_h_)
               {var t_n_=widths_h_[2];
                _a$_(b_g_,string_of_int_a6_(widths_h_[1]));
                var i_a_=skip_positional_spec_j_(i_a_+1|0),widths_h_=t_n_;
                continue}
              throw [0,_r_,_dI_]}
            _ai_(b_g_,c_l_);
            var i_a_=i_a_+1|0;
            continue}
          return _a__(b_g_)}}}
    function extract_format_int_bZ_(conv_a_,fmt_b_,start_c_,stop_d_,widths_e_)
     {var sfmt_f_=extract_format_O_(fmt_b_,start_c_,stop_d_,widths_e_);
      if(78!==conv_a_)if(num_110_as_!==conv_a_)return sfmt_f_;
      sfmt_f_.safeSet(sfmt_f_.getLen()-1|0,num_117_bB_);
      return sfmt_f_}
    function sub_format_for_printf_b0_(conv_a_)
     {return function(fmt_d_,i_b_)
       {var len_k_=fmt_d_.getLen();
        function sub_fmt_l_(c_a_,j_b_)
         {var close_m_=40===c_a_?41:num_125_bA_,j_c_=j_b_;
          for(;;)
           {if(len_k_<=j_c_)return incomplete_format_aC_(fmt_d_);
            if(37===fmt_d_.safeGet(j_c_))
             {var j_e_=j_c_+1|0;
              if(len_k_<=j_e_)return incomplete_format_aC_(fmt_d_);
              var c_f_=fmt_d_.safeGet(j_e_),_g_=c_f_-40|0;
              if(_g_<0||1<_g_)
               {var _i_=_g_-83|0;
                if(_i_<0||2<_i_)
                 var _h_=1;
                else
                 switch(_i_)
                  {case 1:var _h_=1;break;
                   case 2:var _j_=1,_h_=0;break;
                   default:var _j_=0,_h_=0}
                if(_h_){var j_c_=j_e_+1|0;continue}}
              else
               var _j_=0===_g_?0:1;
              if(_j_)
               return c_f_===close_m_
                       ?j_e_+1|0
                       :bad_conversion_format_aj_(fmt_d_,j_b_,c_f_);
              var j_c_=sub_fmt_l_(c_f_,j_e_+1|0)+1|0;
              continue}
            var j_c_=j_c_+1|0;
            continue}}
        return sub_fmt_l_(conv_a_,i_b_)}}
    function iter_on_format_args_b1_(fmt_i_,add_conv_b_,add_char_c_)
     {var lim_l_=fmt_i_.getLen()-1|0;
      function scan_fmt_s_(i_a_)
       {var i_k_=i_a_;
        a:
        for(;;)
         {if(i_k_<lim_l_)
           {if(37===fmt_i_.safeGet(i_k_))
             {var skip_f_=0,i_h_=i_k_+1|0;
              for(;;)
               {if(lim_l_<i_h_)
                 var _e_=incomplete_format_aC_(fmt_i_);
                else
                 {var match_m_=fmt_i_.safeGet(i_h_);
                  if(58<=match_m_)
                   {if(95===match_m_){var skip_f_=1,i_h_=i_h_+1|0;continue}}
                  else
                   if(32<=match_m_)
                    switch(match_m_+num_32_cW_|0)
                     {case 1:
                      case 2:
                      case 4:
                      case 5:
                      case 6:
                      case 7:
                      case 8:
                      case 9:
                      case 12:
                      case 15:break;
                      case 0:
                      case 3:
                      case 11:
                      case 13:var i_h_=i_h_+1|0;continue;
                      case 10:
                       var
                        i_h_=
                         caml_call_gen3_p_(add_conv_b_,skip_f_,i_h_,num_105_T_);
                       continue;
                      default:var i_h_=i_h_+1|0;continue}
                  var i_d_=i_h_;
                  b:
                  for(;;)
                   {if(lim_l_<i_d_)
                     var _e_=incomplete_format_aC_(fmt_i_);
                    else
                     {var conv_j_=fmt_i_.safeGet(i_d_);
                      if(126<=conv_j_)
                       var _g_=0;
                      else
                       switch(conv_j_)
                        {case 78:
                         case 88:
                         case num_100_aX_:
                         case num_105_T_:
                         case num_111_bF_:
                         case num_117_bB_:
                         case num_120_bC_:
                          var
                           _e_=caml_call_gen3_p_(add_conv_b_,skip_f_,i_d_,num_105_T_),
                           _g_=1;
                          break;
                         case 69:
                         case 70:
                         case 71:
                         case num_101_c0_:
                         case num_102_bH_:
                         case num_103_bG_:
                          var
                           _e_=caml_call_gen3_p_(add_conv_b_,skip_f_,i_d_,num_102_bH_),
                           _g_=1;
                          break;
                         case 33:
                         case 37:
                         case 44:
                         case 64:var _e_=i_d_+1|0,_g_=1;break;
                         case 83:
                         case 91:
                         case num_115_at_:
                          var
                           _e_=caml_call_gen3_p_(add_conv_b_,skip_f_,i_d_,num_115_at_),
                           _g_=1;
                          break;
                         case 97:
                         case num_114_aU_:
                         case num_116_bx_:
                          var
                           _e_=caml_call_gen3_p_(add_conv_b_,skip_f_,i_d_,conv_j_),
                           _g_=1;
                          break;
                         case 76:
                         case num_108_c4_:
                         case num_110_as_:
                          var j_t_=i_d_+1|0;
                          if(lim_l_<j_t_)
                           var
                            _e_=caml_call_gen3_p_(add_conv_b_,skip_f_,i_d_,num_105_T_),
                            _g_=1;
                          else
                           {var _q_=fmt_i_.safeGet(j_t_)+num_88_c1_|0;
                            if(_q_<0||32<_q_)
                             var _r_=1;
                            else
                             switch(_q_)
                              {case 0:
                               case 12:
                               case 17:
                               case 23:
                               case 29:
                               case 32:
                                var
                                 _e_=
                                  caml_call_gen2_n_
                                   (add_char_c_,
                                    caml_call_gen3_p_(add_conv_b_,skip_f_,i_d_,conv_j_),
                                    num_105_T_),
                                 _g_=1,
                                 _r_=0;
                                break;
                               default:var _r_=1}
                            if(_r_)
                             var
                              _e_=caml_call_gen3_p_(add_conv_b_,skip_f_,i_d_,num_105_T_),
                              _g_=1}
                          break;
                         case 67:
                         case 99:
                          var
                           _e_=caml_call_gen3_p_(add_conv_b_,skip_f_,i_d_,99),
                           _g_=1;
                          break;
                         case 66:
                         case 98:
                          var
                           _e_=caml_call_gen3_p_(add_conv_b_,skip_f_,i_d_,66),
                           _g_=1;
                          break;
                         case 41:
                         case num_125_bA_:
                          var
                           _e_=caml_call_gen3_p_(add_conv_b_,skip_f_,i_d_,conv_j_),
                           _g_=1;
                          break;
                         case 40:
                          var
                           _e_=
                            scan_fmt_s_
                             (caml_call_gen3_p_(add_conv_b_,skip_f_,i_d_,conv_j_)),
                           _g_=1;
                          break;
                         case num_123_bD_:
                          var
                           i_u_=caml_call_gen3_p_(add_conv_b_,skip_f_,i_d_,conv_j_),
                           j_v_=
                            caml_call_gen2_n_
                             (sub_format_for_printf_b0_(conv_j_),fmt_i_,i_u_),
                           i_o_=i_u_;
                          for(;;)
                           {if(i_o_<(j_v_-2|0))
                             {var
                               i_o_=
                                caml_call_gen2_n_(add_char_c_,i_o_,fmt_i_.safeGet(i_o_));
                              continue}
                            var i_d_=j_v_-1|0;
                            continue b}
                         default:var _g_=0}
                      if(!_g_)
                       var _e_=bad_conversion_format_aj_(fmt_i_,i_d_,conv_j_)}
                    break}}
                var i_k_=_e_;
                continue a}}
            var i_k_=i_k_+1|0;
            continue}
          return i_k_}}
      scan_fmt_s_(0);
      return 0}
    function count_printing_arguments_of_format_b2_(fmt_a_)
     {var ac_d_=[0,0,0,0];
      function add_conv_b_(skip_a_,i_b_,c_c_)
       {var _f_=41!==c_c_?1:0,_g_=_f_?num_125_bA_!==c_c_?1:0:_f_;
        if(_g_)
         {var inc_e_=97===c_c_?2:1;
          if(num_114_aU_===c_c_)ac_d_[3]=ac_d_[3]+1|0;
          if(skip_a_)
           ac_d_[2]=ac_d_[2]+inc_e_|0;
          else
           ac_d_[1]=ac_d_[1]+inc_e_|0}
        return i_b_+1|0}
      iter_on_format_args_b1_
       (fmt_a_,add_conv_b_,function(i_a_,param_b_){return i_a_+1|0});
      return ac_d_[1]}
    function scan_positional_spec_b3_(fmt_a_,got_spec_b_,i_c_)
     {var d_g_=fmt_a_.safeGet(i_c_);
      if((d_g_+num_48_ab_|0)<0||9<(d_g_+num_48_ab_|0))
       return caml_call_gen2_n_(got_spec_b_,0,i_c_);
      var accu_e_=d_g_+num_48_ab_|0,j_d_=i_c_+1|0;
      for(;;)
       {var d_f_=fmt_a_.safeGet(j_d_);
        if(48<=d_f_)
         {if(!(58<=d_f_))
           {var accu_e_=(10*accu_e_|0)+(d_f_+num_48_ab_|0)|0,j_d_=j_d_+1|0;
            continue}}
        else
         if(36===d_f_)
          return 0===accu_e_
                  ?failwith_a4_(_dK_)
                  :caml_call_gen2_n_
                    (got_spec_b_,[0,index_of_int_ba_(accu_e_-1|0)],j_d_+1|0);
        return caml_call_gen2_n_(got_spec_b_,0,i_c_)}}
    function next_index_o_(spec_a_,n_b_){return spec_a_?n_b_:_bW_(n_b_)}
    function get_index_b4_(spec_a_,n_b_){return spec_a_?spec_a_[1]:n_b_}
    function _b5_(to_s_aK_,get_out_b_,outc_c_,outs_d_,flush_e_,k_f_,fmt_g_)
     {var out_A_=caml_call_gen1_i_(get_out_b_,fmt_g_);
      function outs_ae_(s_a_){return caml_call_gen2_n_(outs_d_,out_A_,s_a_)}
      function pr_aL_(k_a_,n_b_,fmt_h_,v_aM_)
       {var len_p_=fmt_h_.getLen();
        function doprn_B_(n_r_,i_b_)
         {var i_m_=i_b_;
          for(;;)
           {if(len_p_<=i_m_)return caml_call_gen1_i_(k_a_,out_A_);
            var c_d_=fmt_h_.safeGet(i_m_);
            if(37===c_d_)
             {var
               get_arg_l_=
                function(spec_a_,n_b_)
                 {return caml_array_get_k_(v_aM_,get_index_b4_(spec_a_,n_b_))},
               scan_flags_au_=
                function(spec_g_,n_f_,widths_c_,i_d_)
                 {var i_a_=i_d_;
                  for(;;)
                   {var switcher___=fmt_h_.safeGet(i_a_)+num_32_cW_|0;
                    if(!(switcher___<0||25<switcher___))
                     switch(switcher___)
                      {case 1:
                       case 2:
                       case 4:
                       case 5:
                       case 6:
                       case 7:
                       case 8:
                       case 9:
                       case 12:
                       case 15:break;
                       case 10:
                        return scan_positional_spec_b3_
                                (fmt_h_,
                                 function(wspec_a_,i_b_)
                                  {var _d_=[0,get_arg_l_(wspec_a_,n_f_),widths_c_];
                                   return scan_flags_au_
                                           (spec_g_,next_index_o_(wspec_a_,n_f_),_d_,i_b_)},
                                 i_a_+1|0);
                       default:var i_a_=i_a_+1|0;continue}
                    var conv_p_=fmt_h_.safeGet(i_a_);
                    if(!(124<=conv_p_))
                     switch(conv_p_)
                      {case 78:
                       case 88:
                       case num_100_aX_:
                       case num_105_T_:
                       case num_111_bF_:
                       case num_117_bB_:
                       case num_120_bC_:
                        var
                         x_ba_=get_arg_l_(spec_g_,n_f_),
                         s_bb_=
                          caml_format_int_aN_
                           (extract_format_int_bZ_(conv_p_,fmt_h_,i_m_,i_a_,widths_c_),
                            x_ba_);
                        return cont_s_q_(next_index_o_(spec_g_,n_f_),s_bb_,i_a_+1|0);
                       case 69:
                       case 71:
                       case num_101_c0_:
                       case num_102_bH_:
                       case num_103_bG_:
                        var
                         x_a1_=get_arg_l_(spec_g_,n_f_),
                         s_a2_=
                          caml_format_float_bs_
                           (extract_format_O_(fmt_h_,i_m_,i_a_,widths_c_),x_a1_);
                        return cont_s_q_(next_index_o_(spec_g_,n_f_),s_a2_,i_a_+1|0);
                       case 76:
                       case num_108_c4_:
                       case num_110_as_:
                        var _ac_=fmt_h_.safeGet(i_a_+1|0)+num_88_c1_|0;
                        if(!(_ac_<0||32<_ac_))
                         switch(_ac_)
                          {case 0:
                           case 12:
                           case 17:
                           case 23:
                           case 29:
                           case 32:
                            var i_R_=i_a_+1|0,switcher_ad_=conv_p_-108|0;
                            if(switcher_ad_<0||2<switcher_ad_)
                             var _af_=0;
                            else
                             {switch(switcher_ad_)
                               {case 1:var _af_=0,_ag_=0;break;
                                case 2:
                                 var
                                  x_a8_=get_arg_l_(spec_g_,n_f_),
                                  _aD_=
                                   caml_format_int_aN_
                                    (extract_format_O_(fmt_h_,i_m_,i_R_,widths_c_),x_a8_),
                                  _ag_=1;
                                 break;
                                default:
                                 var
                                  x_a7_=get_arg_l_(spec_g_,n_f_),
                                  _aD_=
                                   caml_format_int_aN_
                                    (extract_format_O_(fmt_h_,i_m_,i_R_,widths_c_),x_a7_),
                                  _ag_=1}
                              if(_ag_)var s_aC_=_aD_,_af_=1}
                            if(!_af_)
                             var
                              x_a6_=get_arg_l_(spec_g_,n_f_),
                              s_aC_=
                               caml_int64_format_fr_
                                (extract_format_O_(fmt_h_,i_m_,i_R_,widths_c_),x_a6_);
                            return cont_s_q_(next_index_o_(spec_g_,n_f_),s_aC_,i_R_+1|0)
                           }
                        var
                         x_a3_=get_arg_l_(spec_g_,n_f_),
                         s_a4_=
                          caml_format_int_aN_
                           (extract_format_int_bZ_
                             (num_110_as_,fmt_h_,i_m_,i_a_,widths_c_),
                            x_a3_);
                        return cont_s_q_(next_index_o_(spec_g_,n_f_),s_a4_,i_a_+1|0);
                       case 37:
                       case 64:return cont_s_q_(n_f_,_ay_(1,conv_p_),i_a_+1|0);
                       case 83:
                       case num_115_at_:
                        var s_w_=get_arg_l_(spec_g_,n_f_);
                        if(num_115_at_===conv_p_)
                         var x_x_=s_w_;
                        else
                         {var n_b_=[0,0],_am_=s_w_.getLen()-1|0,_aO_=0;
                          if(!(_am_<0))
                           {var i_I_=_aO_;
                            for(;;)
                             {var
                               c_v_=s_w_.safeGet(i_I_),
                               _bh_=
                                14<=c_v_
                                 ?34===c_v_?1:92===c_v_?1:0
                                 :11<=c_v_?13<=c_v_?1:0:8<=c_v_?1:0,
                               _aR_=_bh_?2:caml_is_printable_bt_(c_v_)?1:4;
                              n_b_[1]=n_b_[1]+_aR_|0;
                              var _aS_=i_I_+1|0;
                              if(_am_!==i_I_){var i_I_=_aS_;continue}
                              break}}
                          if(n_b_[1]===s_w_.getLen())
                           var _aF_=s_w_;
                          else
                           {var s_k_=caml_create_string_L_(n_b_[1]);
                            n_b_[1]=0;
                            var _an_=s_w_.getLen()-1|0,_aP_=0;
                            if(!(_an_<0))
                             {var i_H_=_aP_;
                              for(;;)
                               {var c_u_=s_w_.safeGet(i_H_),_y_=c_u_-34|0;
                                if(_y_<0||58<_y_)
                                 if(-20<=_y_)
                                  var _S_=1;
                                 else
                                  {switch(_y_+34|0)
                                    {case 8:
                                      s_k_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s_k_.safeSet(n_b_[1],98);
                                      var _G_=1;
                                      break;
                                     case 9:
                                      s_k_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s_k_.safeSet(n_b_[1],num_116_bx_);
                                      var _G_=1;
                                      break;
                                     case 10:
                                      s_k_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s_k_.safeSet(n_b_[1],num_110_as_);
                                      var _G_=1;
                                      break;
                                     case 13:
                                      s_k_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s_k_.safeSet(n_b_[1],num_114_aU_);
                                      var _G_=1;
                                      break;
                                     default:var _S_=1,_G_=0}
                                   if(_G_)var _S_=0}
                                else
                                 var
                                  _S_=
                                   (_y_-1|0)<0||56<(_y_-1|0)
                                    ?(s_k_.safeSet(n_b_[1],92),
                                      n_b_[1]++,
                                      s_k_.safeSet(n_b_[1],c_u_),
                                      0)
                                    :1;
                                if(_S_)
                                 if(caml_is_printable_bt_(c_u_))
                                  s_k_.safeSet(n_b_[1],c_u_);
                                 else
                                  {s_k_.safeSet(n_b_[1],92);
                                   n_b_[1]++;
                                   s_k_.safeSet(n_b_[1],48+(c_u_/num_100_aX_|0)|0);
                                   n_b_[1]++;
                                   s_k_.safeSet(n_b_[1],48+((c_u_/10|0)%10|0)|0);
                                   n_b_[1]++;
                                   s_k_.safeSet(n_b_[1],48+(c_u_%10|0)|0)}
                                n_b_[1]++;
                                var _aQ_=i_H_+1|0;
                                if(_an_!==i_H_){var i_H_=_aQ_;continue}
                                break}}
                            var _aF_=s_k_}
                          var x_x_=_j_(_dV_,_j_(_aF_,_dU_))}
                        if(i_a_===(i_m_+1|0))
                         var s_aE_=x_x_;
                        else
                         {var sfmt_F_=extract_format_O_(fmt_h_,i_m_,i_a_,widths_c_);
                          try
                           {var neg_U_=0,i_s_=1;
                            for(;;)
                             {if(sfmt_F_.getLen()<=i_s_)
                               var _ao_=[0,0,neg_U_];
                              else
                               {var match_V_=sfmt_F_.safeGet(i_s_);
                                if(49<=match_V_)
                                 if(58<=match_V_)
                                  var _ah_=0;
                                 else
                                  var
                                   _ao_=
                                    [0,
                                     caml_int_of_string_fB_
                                      (_az_(sfmt_F_,i_s_,(sfmt_F_.getLen()-i_s_|0)-1|0)),
                                     neg_U_],
                                   _ah_=1;
                                else
                                 {if(45===match_V_){var neg_U_=1,i_s_=i_s_+1|0;continue}
                                  var _ah_=0}
                                if(!_ah_){var i_s_=i_s_+1|0;continue}}
                              var match_X_=_ao_;
                              break}}
                          catch(_f_)
                           {_f_=caml_wrap_exception_aa_(_f_);
                            if(_f_[1]!==_a5_)throw _f_;
                            var match_X_=bad_conversion_bY_(sfmt_F_,0,num_115_at_)}
                          var
                           p_J_=match_X_[1],
                           len_z_=x_x_.getLen(),
                           neg_aT_=match_X_[2],
                           i_K_=0,
                           pad_char_aV_=32;
                          if(p_J_===len_z_)
                           if(0===i_K_)var _Y_=x_x_,_ak_=1;else var _ak_=0;
                          else
                           var _ak_=0;
                          if(!_ak_)
                           if(p_J_<=len_z_)
                            var _Y_=_az_(x_x_,i_K_,len_z_);
                           else
                            {var res_W_=_ay_(p_J_,pad_char_aV_);
                             if(neg_aT_)
                              _aA_(x_x_,i_K_,res_W_,0,len_z_);
                             else
                              _aA_(x_x_,i_K_,res_W_,p_J_-len_z_|0,len_z_);
                             var _Y_=res_W_}
                          var s_aE_=_Y_}
                        return cont_s_q_(next_index_o_(spec_g_,n_f_),s_aE_,i_a_+1|0);
                       case 67:
                       case 99:
                        var c_r_=get_arg_l_(spec_g_,n_f_);
                        if(99===conv_p_)
                         var s_ax_=_ay_(1,c_r_);
                        else
                         {if(39===c_r_)
                           var _t_=_do_;
                          else
                           if(92===c_r_)
                            var _t_=_dp_;
                           else
                            {if(14<=c_r_)
                              var _C_=0;
                             else
                              switch(c_r_)
                               {case 8:var _t_=_dq_,_C_=1;break;
                                case 9:var _t_=_dr_,_C_=1;break;
                                case 10:var _t_=_ds_,_C_=1;break;
                                case 13:var _t_=_dt_,_C_=1;break;
                                default:var _C_=0}
                             if(!_C_)
                              if(caml_is_printable_bt_(c_r_))
                               {var s_al_=caml_create_string_L_(1);
                                s_al_.safeSet(0,c_r_);
                                var _t_=s_al_}
                              else
                               {var s_D_=caml_create_string_L_(4);
                                s_D_.safeSet(0,92);
                                s_D_.safeSet(1,48+(c_r_/num_100_aX_|0)|0);
                                s_D_.safeSet(2,48+((c_r_/10|0)%10|0)|0);
                                s_D_.safeSet(3,48+(c_r_%10|0)|0);
                                var _t_=s_D_}}
                          var s_ax_=_j_(_dS_,_j_(_t_,_dR_))}
                        return cont_s_q_(next_index_o_(spec_g_,n_f_),s_ax_,i_a_+1|0);
                       case 66:
                       case 98:
                        var _aZ_=i_a_+1|0,_a0_=get_arg_l_(spec_g_,n_f_)?_di_:_dj_;
                        return cont_s_q_(next_index_o_(spec_g_,n_f_),_a0_,_aZ_);
                       case 40:
                       case num_123_bD_:
                        var
                         xf_Q_=get_arg_l_(spec_g_,n_f_),
                         i_av_=
                          caml_call_gen2_n_
                           (sub_format_for_printf_b0_(conv_p_),fmt_h_,i_a_+1|0);
                        if(num_123_bD_===conv_p_)
                         {var
                           b_M_=_a9_(xf_Q_.getLen()),
                           add_char_ap_=
                            function(i_a_,c_b_){_ai_(b_M_,c_b_);return i_a_+1|0};
                          iter_on_format_args_b1_
                           (xf_Q_,
                            function(skip_a_,i_b_,c_c_)
                             {if(skip_a_)_a$_(b_M_,_dJ_);else _ai_(b_M_,37);
                              return add_char_ap_(i_b_,c_c_)},
                            add_char_ap_);
                          var _aW_=_a__(b_M_);
                          return cont_s_q_(next_index_o_(spec_g_,n_f_),_aW_,i_av_)}
                        var
                         n_aw_=next_index_o_(spec_g_,n_f_),
                         m_bg_=
                          add_int_index_bV_
                           (count_printing_arguments_of_format_b2_(xf_Q_),n_aw_);
                        return pr_aL_
                                (function(param_a_){return doprn_B_(m_bg_,i_av_)},
                                 n_aw_,
                                 xf_Q_,
                                 v_aM_);
                       case 33:
                        caml_call_gen1_i_(flush_e_,out_A_);
                        return doprn_B_(n_f_,i_a_+1|0);
                       case 41:return cont_s_q_(n_f_,_dP_,i_a_+1|0);
                       case 44:return cont_s_q_(n_f_,_dQ_,i_a_+1|0);
                       case 70:
                        var x_$_=get_arg_l_(spec_g_,n_f_);
                        if(0===widths_c_)
                         var _aB_=_dT_;
                        else
                         {var sfmt_Z_=extract_format_O_(fmt_h_,i_m_,i_a_,widths_c_);
                          if(70===conv_p_)
                           sfmt_Z_.safeSet(sfmt_Z_.getLen()-1|0,num_103_bG_);
                          var _aB_=sfmt_Z_}
                        var match_ar_=caml_classify_float_fh_(x_$_);
                        if(3===match_ar_)
                         var s_ab_=x_$_<0?_dM_:_dN_;
                        else
                         if(4<=match_ar_)
                          var s_ab_=_dO_;
                         else
                          {var
                            s_P_=caml_format_float_bs_(_aB_,x_$_),
                            i_N_=0,
                            l_aY_=s_P_.getLen();
                           for(;;)
                            {if(l_aY_<=i_N_)
                              var _aq_=_j_(s_P_,_dL_);
                             else
                              {var
                                _E_=s_P_.safeGet(i_N_)-46|0,
                                _bi_=
                                 _E_<0||23<_E_?55===_E_?1:0:(_E_-1|0)<0||21<(_E_-1|0)?1:0;
                               if(!_bi_){var i_N_=i_N_+1|0;continue}
                               var _aq_=s_P_}
                             var s_ab_=_aq_;
                             break}}
                        return cont_s_q_(next_index_o_(spec_g_,n_f_),s_ab_,i_a_+1|0);
                       case 91:
                        return bad_conversion_format_aj_(fmt_h_,i_a_,conv_p_);
                       case 97:
                        var
                         printer_aG_=get_arg_l_(spec_g_,n_f_),
                         n_aH_=_bW_(get_index_b4_(spec_g_,n_f_)),
                         arg_aI_=get_arg_l_(0,n_aH_),
                         i_bc_=i_a_+1|0,
                         n_bd_=next_index_o_(spec_g_,n_aH_);
                        if(to_s_aK_)
                         outs_ae_(caml_call_gen2_n_(printer_aG_,0,arg_aI_));
                        else
                         caml_call_gen2_n_(printer_aG_,out_A_,arg_aI_);
                        return doprn_B_(n_bd_,i_bc_);
                       case num_114_aU_:
                        return bad_conversion_format_aj_(fmt_h_,i_a_,conv_p_);
                       case num_116_bx_:
                        var
                         printer_aJ_=get_arg_l_(spec_g_,n_f_),
                         i_be_=i_a_+1|0,
                         n_bf_=next_index_o_(spec_g_,n_f_);
                        if(to_s_aK_)
                         outs_ae_(caml_call_gen1_i_(printer_aJ_,0));
                        else
                         caml_call_gen1_i_(printer_aJ_,out_A_);
                        return doprn_B_(n_bf_,i_be_)
                       }
                    return bad_conversion_format_aj_(fmt_h_,i_a_,conv_p_)}},
               i_f_=i_m_+1|0,
               widths_g_=0;
              return scan_positional_spec_b3_
                      (fmt_h_,
                       function(spec_a_,i_b_)
                        {return scan_flags_au_(spec_a_,n_r_,widths_g_,i_b_)},
                       i_f_)}
            caml_call_gen2_n_(outc_c_,out_A_,c_d_);
            var i_m_=i_m_+1|0;
            continue}}
        function cont_s_q_(n_a_,s_b_,i_c_)
         {outs_ae_(s_b_);return doprn_B_(n_a_,i_c_)}
        return doprn_B_(n_b_,0)}
      var _q_=index_of_int_ba_(0);
      function kpr_l_(_a_,_b_){return pr_aL_(k_f_,_q_,_a_,_b_)}
      var nargs_m_=count_printing_arguments_of_format_b2_(fmt_g_);
      if(nargs_m_<0||6<nargs_m_)
       {var
         loop_p_=
          function(i_f_,args_b_)
           {if(nargs_m_<=i_f_)
             {var
               a_i_=caml_make_vect_x_(nargs_m_,0),
               f_j_=
                function(i_a_,arg_b_)
                 {return caml_array_set_h_(a_i_,(nargs_m_-i_a_|0)-1|0,arg_b_)},
               i_c_=0,
               param_a_=args_b_;
              for(;;)
               {if(param_a_)
                 {var _d_=param_a_[2],_e_=param_a_[1];
                  if(_d_)
                   {f_j_(i_c_,_e_);var i_c_=i_c_+1|0,param_a_=_d_;continue}
                  f_j_(i_c_,_e_)}
                return kpr_l_(fmt_g_,a_i_)}}
            return function(x_a_){return loop_p_(i_f_+1|0,[0,x_a_,args_b_])}};
        return loop_p_(0,0)}
      switch(nargs_m_)
       {case 1:
         return function(x_a_)
          {var a_b_=caml_make_vect_x_(1,0);
           caml_array_set_h_(a_b_,0,x_a_);
           return kpr_l_(fmt_g_,a_b_)};
        case 2:
         return function(x_a_,y_b_)
          {var a_c_=caml_make_vect_x_(2,0);
           caml_array_set_h_(a_c_,0,x_a_);
           caml_array_set_h_(a_c_,1,y_b_);
           return kpr_l_(fmt_g_,a_c_)};
        case 3:
         return function(x_a_,y_b_,z_c_)
          {var a_d_=caml_make_vect_x_(3,0);
           caml_array_set_h_(a_d_,0,x_a_);
           caml_array_set_h_(a_d_,1,y_b_);
           caml_array_set_h_(a_d_,2,z_c_);
           return kpr_l_(fmt_g_,a_d_)};
        case 4:
         return function(x_a_,y_b_,z_c_,t_d_)
          {var a_e_=caml_make_vect_x_(4,0);
           caml_array_set_h_(a_e_,0,x_a_);
           caml_array_set_h_(a_e_,1,y_b_);
           caml_array_set_h_(a_e_,2,z_c_);
           caml_array_set_h_(a_e_,3,t_d_);
           return kpr_l_(fmt_g_,a_e_)};
        case 5:
         return function(x_a_,y_b_,z_c_,t_d_,u_e_)
          {var a_f_=caml_make_vect_x_(5,0);
           caml_array_set_h_(a_f_,0,x_a_);
           caml_array_set_h_(a_f_,1,y_b_);
           caml_array_set_h_(a_f_,2,z_c_);
           caml_array_set_h_(a_f_,3,t_d_);
           caml_array_set_h_(a_f_,4,u_e_);
           return kpr_l_(fmt_g_,a_f_)};
        case 6:
         return function(x_a_,y_b_,z_c_,t_d_,u_e_,v_f_)
          {var a_i_=caml_make_vect_x_(6,0);
           caml_array_set_h_(a_i_,0,x_a_);
           caml_array_set_h_(a_i_,1,y_b_);
           caml_array_set_h_(a_i_,2,z_c_);
           caml_array_set_h_(a_i_,3,t_d_);
           caml_array_set_h_(a_i_,4,u_e_);
           caml_array_set_h_(a_i_,5,v_f_);
           return kpr_l_(fmt_g_,a_i_)};
        default:return kpr_l_(fmt_g_,[0])}}
    function _b6_(oc_d_)
     {function k_e_(_a_){return 0}
      function _b_(param_a_){return oc_d_}
      var _c_=0;
      return function(_a_)
       {return _b5_(_c_,_b_,_dn_,output_string_bP_,_bR_,k_e_,_a_)}}
    function _dW_(fmt_a_){return _a9_(2*fmt_a_.getLen()|0)}
    function _t_(fmt_a_)
     {function _b_(b_a_){var s_b_=_a__(b_a_);b_a_[2]=0;return s_b_}
      return _b5_(1,_dW_,_ai_,_a$_,function(_a_){return 0},_b_,fmt_a_)}
    var _bb_=[0,0];
    function _bd_(x_a_,i_b_)
     {var f_c_=x_a_[i_b_+1];
      if(caml_obj_is_block_fQ_(f_c_))
       {if(caml_obj_tag_cJ_(f_c_)===252)
         return caml_call_gen1_i_(_t_(_dX_),f_c_);
        if(caml_obj_tag_cJ_(f_c_)===253)
         {var s_e_=caml_format_float_bs_(_dl_,f_c_),i_d_=0,l_g_=s_e_.getLen();
          for(;;)
           {if(l_g_<=i_d_)return _j_(s_e_,_dk_);
            var
             match_f_=s_e_.safeGet(i_d_),
             _h_=48<=match_f_?58<=match_f_?0:1:45===match_f_?1:0;
            if(_h_){var i_d_=i_d_+1|0;continue}
            return s_e_}}
        return _dY_}
      return caml_call_gen1_i_(_t_(_dZ_),f_c_)}
    function _b7_(x_a_,i_b_)
     {if(x_a_.length-1<=i_b_)return _d0_;
      var _c_=_b7_(x_a_,i_b_+1|0),_d_=_bd_(x_a_,i_b_);
      return caml_call_gen2_n_(_t_(_d1_),_d_,_c_)}
    function _b8_(x_a_)
     {var param_b_=_bb_[1];
      for(;;)
       {if(param_b_)
         {var tl_s_=param_b_[2],hd_u_=param_b_[1];
          try
           {var _v_=caml_call_gen1_i_(hd_u_,x_a_),_e_=_v_}
          catch(_f_){var _e_=0}
          if(_e_)return _e_[1];
          var param_b_=tl_s_;
          continue}
        if(x_a_[1]===_d6_)return _d7_;
        if(x_a_[1]===_b9_)return _d8_;
        if(x_a_[1]===_b__)
         {var
           match_f_=x_a_[2],
           char_k_=match_f_[3],
           line_w_=match_f_[2],
           file_x_=match_f_[1];
          return caml_call_gen5_aO_
                  (_t_(_bc_),file_x_,line_w_,char_k_,char_k_+5|0,_d9_)}
        if(x_a_[1]===_r_)
         {var
           match_g_=x_a_[2],
           char_l_=match_g_[3],
           line_y_=match_g_[2],
           file_z_=match_g_[1];
          return caml_call_gen5_aO_
                  (_t_(_bc_),file_z_,line_y_,char_l_,char_l_+6|0,_d__)}
        if(x_a_[1]===_b$_)
         {var
           match_h_=x_a_[2],
           char_m_=match_h_[3],
           line_A_=match_h_[2],
           file_B_=match_h_[1];
          return caml_call_gen5_aO_
                  (_t_(_bc_),file_B_,line_A_,char_m_,char_m_+6|0,_d$_)}
        var n_d_=x_a_.length-1,constructor_C_=x_a_[0+1][0+1];
        if(n_d_<0||2<n_d_)
         var
          _o_=_b7_(x_a_,2),
          _p_=_bd_(x_a_,1),
          _c_=caml_call_gen2_n_(_t_(_d2_),_p_,_o_);
        else
         switch(n_d_)
          {case 1:var _c_=_d4_;break;
           case 2:
            var _q_=_bd_(x_a_,1),_c_=caml_call_gen1_i_(_t_(_d5_),_q_);break;
           default:var _c_=_d3_}
        return _j_(constructor_C_,_c_)}}
    function _ca_(outchan_a_)
     {var
       backtrace_h_=
        caml_convert_raw_backtrace_fj_
         (caml_get_exception_raw_backtrace_fp_(0));
      if(backtrace_h_)
       {var a_d_=backtrace_h_[1],_f_=a_d_.length-1-1|0,_q_=0;
        if(!(_f_<0))
         {var i_c_=_q_;
          for(;;)
           {if(caml_notequal_fP_(caml_array_get_k_(a_d_,i_c_),_eg_))
             {var
               li_b_=caml_array_get_k_(a_d_,i_c_),
               is_raise_j_=0===li_b_[0]?li_b_[1]:li_b_[1],
               info_e_=is_raise_j_?0===i_c_?_ea_:_ed_:0===i_c_?_ee_:_ef_;
              if(0===li_b_[0])
               var
                endchar_l_=li_b_[5],
                startchar_m_=li_b_[4],
                lineno_o_=li_b_[3],
                filename_p_=li_b_[2],
                _g_=
                 caml_call_gen5_aO_
                  (_t_(_eb_),
                   info_e_,
                   filename_p_,
                   lineno_o_,
                   startchar_m_,
                   endchar_l_);
              else
               var _g_=caml_call_gen1_i_(_t_(_ec_),info_e_);
              caml_call_gen2_n_(_b6_(outchan_a_),_eh_,_g_)}
            var _r_=i_c_+1|0;
            if(_f_!==i_c_){var i_c_=_r_;continue}
            break}}
        return 0}
      return caml_call_gen1_i_(_b6_(outchan_a_),_ei_)}
    function _cb_(fn_a_){_bb_[1]=[0,fn_a_,_bb_[1]];return 0}
    32===_a8_;
    try
     {var _fe_=caml_sys_getenv_cK_(_fd_),params_be_=_fe_}
    catch(_f_)
     {_f_=caml_wrap_exception_aa_(_f_);
      if(_f_[1]!==_ag_)throw _f_;
      try
       {var _fc_=caml_sys_getenv_cK_(_fb_),_cc_=_fc_}
      catch(_f_)
       {_f_=caml_wrap_exception_aa_(_f_);
        if(_f_[1]!==_ag_)throw _f_;
        var _cc_=_ej_}
      var params_be_=_cc_}
    var lim_bS_=params_be_.getLen(),c_ek_=82,i_bT_=0;
    if(0<=0)
     if(lim_bS_<i_bT_)
      var _aP_=0;
     else
      try
       {var i_aB_=i_bT_;
        for(;;)
         {if(lim_bS_<=i_aB_)throw [0,_ag_];
          if(params_be_.safeGet(i_aB_)!==c_ek_){var i_aB_=i_aB_+1|0;continue}
          var _aP_=1;
          break}}
      catch(_f_)
       {_f_=caml_wrap_exception_aa_(_f_);
        if(_f_[1]!==_ag_)throw _f_;
        var _aP_=1}
    else
     var _aP_=0;
    if(!_aP_)invalid_arg_W_(_dw_);
    function _cd_(param_a_)
     {var seq_b_=[];
      caml_update_dummy_f5_(seq_b_,[0,seq_b_,seq_b_]);
      return seq_b_}
    var Canceled_bf_=[0,_el_],current_data_P_=[0,0];
    function repr_rec_bg_(t_a_)
     {var _c_=t_a_[1];
      if(3===_c_[0])
       {var t_d_=_c_[1],t_b_=repr_rec_bg_(t_d_);
        if(t_b_!==t_d_)t_a_[1]=[3,t_b_];
        return t_b_}
      return t_a_}
    function repr_Y_(t_a_){return repr_rec_bg_(t_a_)}
    var
     async_exception_hook_bh_=
      [0,
       function(exn_a_)
        {prerr_string_bQ_(_em_);
         prerr_string_bQ_(_b8_(exn_a_));
         caml_ml_output_char_cI_(stderr_af_,10);
         _ca_(stderr_af_);
         _bR_(stderr_af_);
         do_at_exit_a7_(0);
         return caml_sys_exit_fZ_(2)}];
    function call_unsafe_ce_(f_a_,x_b_)
     {try
       {var _c_=caml_call_gen1_i_(f_a_,x_b_)}
      catch(exn_f_)
       {exn_f_=caml_wrap_exception_aa_(exn_f_);
        return caml_call_gen1_i_(async_exception_hook_bh_[1],exn_f_)}
      return _c_}
    function run_waiters_rec_bu_(counter_a_,state_b_,ws_c_,rem_d_)
     {var ws_f_=ws_c_,rem_e_=rem_d_;
      for(;;)
       if(typeof ws_f_===str_number_H_)
        return counter_a_<50
                ?run_waiters_rec_next_E_(1+counter_a_,state_b_,rem_e_)
                :caml_trampoline_return_F_
                  (run_waiters_rec_next_E_,[0,state_b_,rem_e_]);
       else
        switch(ws_f_[0])
         {case 1:
           caml_call_gen1_i_(ws_f_[1],state_b_);
           return counter_a_<50
                   ?run_waiters_rec_next_E_(1+counter_a_,state_b_,rem_e_)
                   :caml_trampoline_return_F_
                     (run_waiters_rec_next_E_,[0,state_b_,rem_e_]);
          case 2:
           var _h_=[0,ws_f_[2],rem_e_],ws_f_=ws_f_[1],rem_e_=_h_;continue;
          default:
           var _g_=ws_f_[1][1];
           if(_g_)
            {caml_call_gen1_i_(_g_[1],state_b_);
             return counter_a_<50
                     ?run_waiters_rec_next_E_(1+counter_a_,state_b_,rem_e_)
                     :caml_trampoline_return_F_
                       (run_waiters_rec_next_E_,[0,state_b_,rem_e_])}
           else
            return counter_a_<50
                    ?run_waiters_rec_next_E_(1+counter_a_,state_b_,rem_e_)
                    :caml_trampoline_return_F_
                      (run_waiters_rec_next_E_,[0,state_b_,rem_e_])}}
    function run_waiters_rec_next_E_(counter_a_,state_b_,rem_c_)
     {return rem_c_
              ?counter_a_<50
                ?run_waiters_rec_bu_
                  (1+counter_a_,state_b_,rem_c_[1],rem_c_[2])
                :caml_trampoline_return_F_
                  (run_waiters_rec_bu_,[0,state_b_,rem_c_[1],rem_c_[2]])
              :0}
    function run_waiters_rec_en_(state_b_,ws_c_,rem_d_)
     {return caml_trampoline_aQ_(run_waiters_rec_bu_(0,state_b_,ws_c_,rem_d_))}
    function run_waiters_rec_next_f8_(state_b_,rem_c_)
     {return caml_trampoline_aQ_(run_waiters_rec_next_E_(0,state_b_,rem_c_))}
    function run_cancel_handlers_rec_bv_(counter_a_,chs_b_,rem_c_)
     {var chs_e_=chs_b_,rem_d_=rem_c_;
      for(;;)
       if(typeof chs_e_===str_number_H_)
        return counter_a_<50
                ?run_cancel_handlers_rec_next_R_(1+counter_a_,rem_d_)
                :caml_trampoline_return_F_
                  (run_cancel_handlers_rec_next_R_,[0,rem_d_]);
       else
        switch(chs_e_[0])
         {case 1:
           var n_f_=chs_e_[1];
           if(n_f_[4]){n_f_[4]=0;n_f_[1][2]=n_f_[2];n_f_[2][1]=n_f_[1]}
           return counter_a_<50
                   ?run_cancel_handlers_rec_next_R_(1+counter_a_,rem_d_)
                   :caml_trampoline_return_F_
                     (run_cancel_handlers_rec_next_R_,[0,rem_d_]);
          case 2:
           var _h_=[0,chs_e_[2],rem_d_],chs_e_=chs_e_[1],rem_d_=_h_;continue;
          default:
           var f_g_=chs_e_[2];
           current_data_P_[1]=chs_e_[1];
           call_unsafe_ce_(f_g_,0);
           return counter_a_<50
                   ?run_cancel_handlers_rec_next_R_(1+counter_a_,rem_d_)
                   :caml_trampoline_return_F_
                     (run_cancel_handlers_rec_next_R_,[0,rem_d_])}}
    function run_cancel_handlers_rec_next_R_(counter_a_,rem_b_)
     {return rem_b_
              ?counter_a_<50
                ?run_cancel_handlers_rec_bv_(1+counter_a_,rem_b_[1],rem_b_[2])
                :caml_trampoline_return_F_
                  (run_cancel_handlers_rec_bv_,[0,rem_b_[1],rem_b_[2]])
              :0}
    function run_cancel_handlers_rec_eo_(chs_b_,rem_c_)
     {return caml_trampoline_aQ_(run_cancel_handlers_rec_bv_(0,chs_b_,rem_c_))}
    function run_cancel_handlers_rec_next_f9_(rem_b_)
     {return caml_trampoline_aQ_(run_cancel_handlers_rec_next_R_(0,rem_b_))}
    function unsafe_run_waiters_aD_(sleeper_a_,state_b_)
     {var
       _c_=
        1===state_b_[0]
         ?state_b_[1][1]===Canceled_bf_
           ?(run_cancel_handlers_rec_eo_(sleeper_a_[4],0),1)
           :0
         :0;
      return run_waiters_rec_en_(state_b_,sleeper_a_[2],0)}
    var wakening_bi_=[0,0],to_wakeup_X_=[0,0,0];
    function wakeup_bj_(t_a_,v_b_)
     {var result_h_=[0,v_b_],t_i_=repr_rec_bg_(t_a_),_e_=t_i_[1];
      switch(_e_[0])
       {case 1:if(_e_[1][1]===Canceled_bf_)return 0;break;
        case 2:
         var sleeper_k_=_e_[1];
         t_i_[1]=result_h_;
         var
          snapshot_g_=current_data_P_[1],
          already_wakening_j_=wakening_bi_[1]?1:(wakening_bi_[1]=1,0);
         unsafe_run_waiters_aD_(sleeper_k_,result_h_);
         if(already_wakening_j_){current_data_P_[1]=snapshot_g_;return 0}
         for(;;)
          {if(0===to_wakeup_X_[1])
            {wakening_bi_[1]=0;current_data_P_[1]=snapshot_g_;return 0}
           if(0===to_wakeup_X_[1])throw [0,_dy_];
           to_wakeup_X_[1]=to_wakeup_X_[1]-1|0;
           var tail_c_=to_wakeup_X_[2],head_d_=tail_c_[2];
           if(head_d_===tail_c_)to_wakeup_X_[2]=0;else tail_c_[2]=head_d_[2];
           var _f_=head_d_[1];
           unsafe_run_waiters_aD_(_f_[1],_f_[2]);
           continue}
        }
      return invalid_arg_W_(_ep_)}
    function append_cf_(l1_a_,l2_b_)
     {return typeof l1_a_===str_number_H_
              ?l2_b_
              :typeof l2_b_===str_number_H_?l1_a_:[2,l1_a_,l2_b_]}
    function cleanup_bk_(ws_a_)
     {if(typeof ws_a_!==str_number_H_)
       switch(ws_a_[0])
        {case 2:
          var l1_b_=ws_a_[1],_c_=cleanup_bk_(ws_a_[2]);
          return append_cf_(cleanup_bk_(l1_b_),_c_);
         case 1:break;
         default:if(!ws_a_[1][1])return 0}
      return ws_a_}
    function task_cg_(param_a_)
     {var t_b_=[0,[2,[0,1,0,0,0]]];return [0,t_b_,t_b_]}
    function add_immutable_waiter_ch_(sleeper_a_,waiter_b_)
     {var
       waiter_d_=[1,waiter_b_],
       _c_=sleeper_a_[2],
       waiter_e_=typeof _c_===str_number_H_?waiter_d_:[2,waiter_d_,_c_];
      sleeper_a_[2]=waiter_e_;
      return 0}
    function bind_bl_(t_a_,f_b_)
     {var t_d_=repr_Y_(t_a_),_c_=t_d_[1];
      switch(_c_[0])
       {case 1:return [0,_c_];
        case 2:
         var
          res_k_=[0,[2,[0,[0,[0,t_d_]],0,0,0]]],
          sleeper_e_=_c_[1],
          data_u_=current_data_P_[1];
         add_immutable_waiter_ch_
          (sleeper_e_,
           function(state_a_)
            {switch(state_a_[0])
              {case 0:
                var v_v_=state_a_[1];
                current_data_P_[1]=data_u_;
                try
                 {var _w_=caml_call_gen1_i_(f_b_,v_v_),_q_=_w_}
                catch(exn_f_)
                 {exn_f_=caml_wrap_exception_aa_(exn_f_);
                  var _q_=[0,[1,exn_f_]]}
                var t1_d_=repr_Y_(res_k_),t2_g_=repr_Y_(_q_),_l_=t1_d_[1];
                if(2===_l_[0])
                 {var sleeper1_c_=_l_[1];
                  if(t1_d_===t2_g_)return 0;
                  var _e_=t2_g_[1];
                  if(2===_e_[0])
                   {var sleeper2_f_=_e_[1];
                    t2_g_[1]=[3,t1_d_];
                    sleeper1_c_[1]=sleeper2_f_[1];
                    var
                     waiters_m_=append_cf_(sleeper1_c_[2],sleeper2_f_[2]),
                     removed_n_=sleeper1_c_[3]+sleeper2_f_[3]|0;
                    if(42<removed_n_)
                     {sleeper1_c_[3]=0;sleeper1_c_[2]=cleanup_bk_(waiters_m_)}
                    else
                     {sleeper1_c_[3]=removed_n_;sleeper1_c_[2]=waiters_m_}
                    var
                     _h_=sleeper2_f_[4],
                     _j_=sleeper1_c_[4],
                     _s_=
                      typeof _j_===str_number_H_
                       ?_h_
                       :typeof _h_===str_number_H_?_j_:[2,_j_,_h_];
                    sleeper1_c_[4]=_s_;
                    return 0}
                  t1_d_[1]=_e_;
                  return unsafe_run_waiters_aD_(sleeper1_c_,_e_)}
                throw [0,_r_,_eq_];
               case 1:
                var t_o_=repr_Y_(res_k_),_p_=t_o_[1];
                if(2===_p_[0])
                 {var sleeper_t_=_p_[1];
                  t_o_[1]=state_a_;
                  return unsafe_run_waiters_aD_(sleeper_t_,state_a_)}
                throw [0,_r_,_er_];
               default:throw [0,_r_,_es_]}});
         return res_k_;
        case 3:throw [0,_r_,_et_];
        default:return caml_call_gen1_i_(f_b_,_c_[1])}}
    var
     pause_hook_ew_=[0,function(_a_){return 0}],
     s1_c_=_cd_(0),
     _ex_=[0,0],
     window_Z_=joo_global_object_A_,
     no_handler_aE_=null,
     undefined_ci_=undefined,
     true_B_=true,
     false_bm_=false,
     a67e0736b_cj_=Array,
     a2c37ab8e_aF_=Date,
     Error_ck_=[0,_ez_];
    function _ey_(param_a_)
     {var _f_=1-(s1_c_[2]===s1_c_?1:0);
      if(_f_)
       {var seq_b_=_cd_(0);
        seq_b_[1][2]=s1_c_[2];
        s1_c_[2][1]=seq_b_[1];
        seq_b_[1]=s1_c_[1];
        s1_c_[1][2]=seq_b_;
        s1_c_[1]=s1_c_;
        s1_c_[2]=s1_c_;
        _ex_[1]=0;
        var curr_d_=seq_b_[2];
        for(;;)
         {var _e_=curr_d_!==seq_b_?1:0;
          if(_e_)
           {if(curr_d_[4])wakeup_bj_(curr_d_[3],0);
            var curr_d_=curr_d_[2];
            continue}
          return _e_}}
      return _f_}
    caml_register_named_value_cH_(name_eA_,[0,Error_ck_,{}][0+1]);
    _cb_
     (function(param_a_)
       {return param_a_[1]===Error_ck_
                ?[0,new MlWrappedString_V_(param_a_[2].toString())]
                :0});
    _cb_
     (function(e_a_)
       {return e_a_ instanceof a67e0736b_cj_
                ?0
                :[0,new MlWrappedString_V_(e_a_.toString())]});
    function _cl_(_a_){return _a_}
    function add_d_(p_a_,n_b_){p_a_.appendChild(n_b_);return 0}
    function handler_C_(f_d_)
     {return _cl_
              (caml_js_wrap_callback_bw_
                (function(e_a_)
                  {if(e_a_)
                    {var res_e_=caml_call_gen1_i_(f_d_,e_a_);
                     if(!(res_e_|0))e_a_.preventDefault();
                     return res_e_}
                   var e_c_=event,res_b_=caml_call_gen1_i_(f_d_,e_c_);
                   if(!(res_b_|0))e_c_.returnValue=res_b_;
                   return res_b_}))}
    function _cm_(s_a_){return s_a_.toString()}
    function addEventListener_cn_(a8d589752_e_,typ_b_,h_c_,capt_d_)
     {if(a8d589752_e_.addEventListener===undefined_ci_)
       {var
         ev_f_="on".concat(typ_b_),
         callback_g_=
          function(e_a_)
           {var _d_=[0,h_c_,e_a_,[0]];
            return function(_a_,_b_){return caml_js_call_fD_(_d_,_a_,_b_)}};
        a8d589752_e_.attachEvent(ev_f_,callback_g_);
        return function(param_a_)
         {return a8d589752_e_.detachEvent(ev_f_,callback_g_)}}
      a8d589752_e_.addEventListener(typ_b_,h_c_,capt_d_);
      return function(param_a_)
       {return a8d589752_e_.removeEventListener(typ_b_,h_c_,capt_d_)}}
    function f_co_(id_a_){return caml_call_gen1_i_(id_a_,0)}
    var
     mouseup_eC_=_cm_(_eB_),
     d_aG_="2d",
     mousemove_eE_=_cm_(_eD_),
     doc_e_=window_Z_.document;
    function opt_iter_aH_(x_a_,f_b_)
     {return x_a_?caml_call_gen1_i_(f_b_,x_a_[1]):0}
    function createElement_bn_(doc_a_,name_b_)
     {return doc_a_.createElement(name_b_.toString())}
    function unsafeCreateElement_Q_(doc_a_,name_b_)
     {return createElement_bn_(doc_a_,name_b_)}
    var createElementSyntax_cp_=[0,num_785140586_cX_];
    function unsafeCreateElementEx_cq_(type_a_,name_b_,doc_c_,elt_d_)
     {for(;;)
       {if(0===type_a_)if(0===name_b_)return createElement_bn_(doc_c_,elt_d_);
        var _i_=createElementSyntax_cp_[1];
        if(num_785140586_cX_===_i_)
         {try
           {var
             el_k_=doc_e_.createElement('<input name="x">'),
             _l_=el_k_.tagName.toLowerCase()===str_input_cM_?1:0,
             _n_=_l_?el_k_.name===str_x_cQ_?1:0:_l_,
             _j_=_n_}
          catch(_f_){var _j_=0}
          var _m_=_j_?num_982028505_cN_:-1003883683;
          createElementSyntax_cp_[1]=_m_;
          continue}
        if(num_982028505_cN_<=_i_)
         {var a_g_=new a67e0736b_cj_();
          a_g_.push("<",elt_d_.toString());
          opt_iter_aH_
           (type_a_,
            function(t_a_)
             {a_g_.push(' type="',caml_js_html_escape_cL_(t_a_),str_aT_);
              return 0});
          opt_iter_aH_
           (name_b_,
            function(n_a_)
             {a_g_.push(' name="',caml_js_html_escape_cL_(n_a_),str_aT_);
              return 0});
          a_g_.push(">");
          return doc_c_.createElement(a_g_.join(str_f_))}
        var res_h_=createElement_bn_(doc_c_,elt_d_);
        opt_iter_aH_(type_a_,function(t_a_){return res_h_.type=t_a_});
        opt_iter_aH_(name_b_,function(n_a_){return res_h_.name=n_a_});
        return res_h_}}
    function createInput_bo_(type_a_,name_b_,doc_c_)
     {return unsafeCreateElementEx_cq_(type_a_,name_b_,doc_c_,_eH_)}
    function createLabel_cr_(doc_a_)
     {return unsafeCreateElement_Q_(doc_a_,_eI_)}
    function createDiv_aI_(doc_a_){return unsafeCreateElement_Q_(doc_a_,_eJ_)}
    var Canvas_not_available_eO_=[0,_eN_];
    window_Z_.HTMLElement===undefined_ci_;
    var
     a80410c35_eQ_=caml_js_get_console_fE_(0),
     overflow_limit_eR_=num_2147483_bz_;
    pause_hook_ew_[1]=
    function(param_a_)
     {return 1===param_a_
              ?(window_Z_.setTimeout(caml_js_wrap_callback_bw_(_ey_),0),0)
              :0};
    function _cs_(s_a_){return a80410c35_eQ_.log(s_a_.toString())}
    async_exception_hook_bh_[1]=
    function(exn_a_){_cs_(_eS_);_cs_(_b8_(exn_a_));return _ca_(stderr_af_)};
    var
     pi_l_=4*0.785398163397448279,
     obliquity_ak_=23.5*pi_l_/180,
     button_type_ct_="button",
     width_aJ_=num_600_aq_,
     dark_bp_=0.0400000000000000078;
    function toggle_button_cu_(txt1_a_,txt2_b_,action_c_)
     {var
       state_d_=[0,0],
       txt1_g_=txt1_a_.toString(),
       txt2_h_=txt2_b_.toString(),
       a521a4c10_f_=createInput_bo_([0,button_type_ct_],0,doc_e_);
      a521a4c10_f_.value=txt1_g_;
      a521a4c10_f_.onclick=
      handler_C_
       (function(param_a_)
         {state_d_[1]=1-state_d_[1];
          var txt2_b_=state_d_[1]?txt2_h_:txt1_g_;
          a521a4c10_f_.value=txt2_b_;
          caml_call_gen1_i_(action_c_,state_d_[1]);
          return true_B_});
      return a521a4c10_f_}
    function checkbox_cv_(txt_a_,checked_b_,action_c_)
     {var b_f_=createInput_bo_([0,"checkbox"],0,doc_e_);
      b_f_.checked=!!checked_b_;
      b_f_.onclick=
      handler_C_
       (function(param_a_)
         {caml_call_gen1_i_(action_c_,b_f_.checked|0);return true_B_});
      var lab_g_=createLabel_cr_(doc_e_);
      add_d_(lab_g_,b_f_);
      add_d_(lab_g_,doc_e_.createTextNode(txt_a_.toString()));
      return lab_g_}
    function vertex_m_(x_a_,y_b_,z_c_){return [num_254_z_,x_a_,y_b_,z_c_]}
    function vect_cw_(param_a_,_b_)
     {return [num_254_z_,
              _b_[1]-param_a_[1],
              _b_[2]-param_a_[2],
              _b_[3]-param_a_[3]]}
    function matrix_vect_mul_al_(m_a_,param_b_)
     {var
       z_c_=param_b_[3],
       y_d_=param_b_[2],
       x_e_=param_b_[1],
       r3_f_=m_a_[3],
       r2_g_=m_a_[2],
       r1_h_=m_a_[1];
      return [num_254_z_,
              x_e_*r1_h_[1]+y_d_*r1_h_[2]+z_c_*r1_h_[3],
              x_e_*r2_g_[1]+y_d_*r2_g_[2]+z_c_*r2_g_[3],
              x_e_*r3_f_[1]+y_d_*r3_f_[2]+z_c_*r3_f_[3]]}
    function matrix_transp_cx_(m_a_)
     {var r3_b_=m_a_[3],r2_c_=m_a_[2],r1_d_=m_a_[1];
      return [0,
              [num_254_z_,r1_d_[1],r2_c_[1],r3_b_[1]],
              [num_254_z_,r1_d_[2],r2_c_[2],r3_b_[2]],
              [num_254_z_,r1_d_[3],r2_c_[3],r3_b_[3]]]}
    function matrix_mul_aK_(m_a_,m_b_)
     {var
       m_c_=matrix_transp_cx_(m_b_),
       _d_=matrix_vect_mul_al_(m_c_,m_a_[3]),
       _e_=matrix_vect_mul_al_(m_c_,m_a_[2]);
      return [0,matrix_vect_mul_al_(m_c_,m_a_[1]),_e_,_d_]}
    function xz_rotation_bq_(phi_a_)
     {var
       cos_phi_b_=Math.cos(phi_a_),
       sin_phi_c_=Math.sin(phi_a_),
       _d_=vertex_m_(-sin_phi_c_,0,cos_phi_b_),
       _e_=vertex_m_(0,1,0);
      return [0,vertex_m_(cos_phi_b_,0,sin_phi_c_),_e_,_d_]}
    function xy_rotation_cy_(phi_a_)
     {var
       cos_phi_b_=Math.cos(phi_a_),
       sin_phi_c_=Math.sin(phi_a_),
       _d_=vertex_m_(0,0,1),
       _e_=vertex_m_(-sin_phi_c_,cos_phi_b_,0);
      return [0,vertex_m_(cos_phi_b_,sin_phi_c_,0),_e_,_d_]}
    var matrix_identity_cz_=xz_rotation_bq_(0);
    function face_am_(v1_a_,v2_b_,v3_c_){return [0,v1_a_,v2_b_,v3_c_]}
    function create_canvas_aL_(w_a_,h_b_)
     {var c_c_=unsafeCreateElement_Q_(doc_e_,_eP_);
      if(1-(c_c_.getContext==no_handler_aE_?1:0))
       {c_c_.width=w_a_;c_c_.height=h_b_;return c_c_}
      throw [0,Canvas_not_available_eO_]}
    function min_$_(u_a_,v_b_){return u_a_<v_b_?u_a_:v_b_}
    function max_s_(u_a_,v_b_){return u_a_<v_b_?v_b_:u_a_}
    var
     t_delta_cA_=pi_l_/8,
     north_w_=8*12|0,
     vertices_I_=caml_make_vect_x_(north_w_+2|0,vertex_m_(0,0,0)),
     a_J_=caml_make_vect_x_(north_w_*2|0,face_am_(0,0,0)),
     south_cB_=north_w_+1|0;
    caml_array_set_h_(vertices_I_,north_w_,vertex_m_(0,-1,0));
    caml_array_set_h_(vertices_I_,south_cB_,vertex_m_(0,1,0));
    var
     _cC_=12-1|0,
     t_div_K_=8,
     p_delta_eT_=2*pi_l_/12,
     t_offset_eU_=(pi_l_-t_delta_cA_)/2,
     _eV_=0;
    if(!(_cC_<0))
     {var i_an_=_eV_;
      for(;;)
       {var _cD_=t_div_K_-1|0,_eW_=0;
        if(!(_cD_<0))
         {var j___=_eW_;
          for(;;)
           {var
             phi_cE_=i_an_*p_delta_eT_,
             theta_br_=j___*t_delta_cA_-t_offset_eU_,
             k_a_=(i_an_*t_div_K_|0)+j___|0;
            caml_array_set_h_
             (vertices_I_,
              k_a_,
              vertex_m_
               (Math.cos(phi_cE_)*Math.cos(theta_br_),
                Math.sin(theta_br_),
                Math.sin(phi_cE_)*Math.cos(theta_br_)));
            if(0===j___)
             {caml_array_set_h_
               (a_J_,
                2*k_a_|0,
                face_am_(north_w_,k_a_,caml_mod_ao_(k_a_+t_div_K_|0,north_w_)));
              caml_array_set_h_
               (a_J_,
                (2*k_a_|0)+1|0,
                face_am_
                 (south_cB_,
                  caml_mod_ao_((k_a_+(2*t_div_K_|0)|0)-1|0,north_w_),
                  (k_a_+t_div_K_|0)-1|0))}
            else
             {caml_array_set_h_
               (a_J_,
                2*k_a_|0,
                face_am_(k_a_,caml_mod_ao_(k_a_+t_div_K_|0,north_w_),k_a_-1|0));
              caml_array_set_h_
               (a_J_,
                (2*k_a_|0)+1|0,
                face_am_
                 (k_a_-1|0,
                  caml_mod_ao_(k_a_+t_div_K_|0,north_w_),
                  caml_mod_ao_((k_a_+t_div_K_|0)-1|0,north_w_)))}
            var _eY_=j___+1|0;
            if(_cD_!==j___){var j___=_eY_;continue}
            break}}
        var _eX_=i_an_+1|0;
        if(_cC_!==i_an_){var i_an_=_eX_;continue}
        break}}
    var src_e4_="../planet/texture.jpg";
    window_Z_.onload=
    handler_C_
     (function(param_a_)
       {function _p_(texture_a_)
         {var
           w___=texture_a_.width,
           h_aa_=texture_a_.height,
           canvas_A_=create_canvas_aL_(w___,h_aa_),
           ctx_g_=canvas_A_.getContext(d_aG_),
           h_q_=h_aa_/8|0,
           w_h_=w___/8|0,
           img_ab_=ctx_g_.getImageData(0,0,w_h_,h_q_),
           data_ac_=img_ab_.data,
           inv_gamma_aj_=num_0_5_c3_;
          function update_shadow_ad_(obliquity_a_)
           {var
             _d_=h_q_-1|0,
             cos_obl_m_=Math.cos(obliquity_a_),
             sin_obl_n_=-Math.sin(obliquity_a_),
             _o_=0;
            if(!(_d_<0))
             {var j_b_=_o_;
              for(;;)
               {var _e_=(w_h_/2|0)-1|0,_p_=0;
                if(!(_e_<0))
                 {var i_c_=_p_;
                  for(;;)
                   {var
                     theta_f_=(j_b_/h_q_-num_0_5_c3_)*pi_l_,
                     x_i_=
                      Math.cos(i_c_/w_h_*2*pi_l_)*
                      Math.cos(theta_f_)*
                      cos_obl_m_+
                      Math.sin(theta_f_)*
                      sin_obl_n_,
                     k_s_=4*(i_c_+j_b_*w_h_)|0,
                     k_t_=4*(w_h_-i_c_+j_b_*w_h_-1)|0,
                     c_j_=0<x_i_?dark_bp_:dark_bp_-x_i_*(1-dark_bp_)*1.2,
                     c_u_=c_j_<=1?c_j_:1,
                     c_k_=num_255_M_-(255.99*Math.pow(c_u_,inv_gamma_aj_)|0)|0;
                    data_ac_[k_s_+3|0]=c_k_;
                    data_ac_[k_t_+3|0]=c_k_;
                    var _v_=i_c_+1|0;
                    if(_e_!==i_c_){var i_c_=_v_;continue}
                    break}}
                var _r_=j_b_+1|0;
                if(_d_!==j_b_){var j_b_=_r_;continue}
                break}}
            ctx_g_.putImageData(img_ab_,0,0);
            ctx_g_.globalCompositeOperation=str_copy_cR_;
            ctx_g_.save();
            ctx_g_.scale(8*(w_h_+2|0)/w_h_,8*(h_q_+2|0)/h_q_);
            ctx_g_.translate(-1,-1);
            ctx_g_.drawImage(canvas_A_,0,0);
            return ctx_g_.restore()}
          update_shadow_ad_(obliquity_ak_);
          var
           w_D_=texture_a_.width,
           shd_ae_=create_canvas_aL_(w_D_,texture_a_.height),
           ctx_E_=shd_ae_.getContext(d_aG_),
           no_lighting_L_=[0,0],
           canvas_N_=create_canvas_aL_(width_aJ_,width_aJ_),
           canvas_as_=create_canvas_aL_(width_aJ_,width_aJ_);
          add_d_(doc_e_.body,canvas_N_);
          var
           ctx_O_=canvas_N_.getContext(d_aG_),
           a8b7209fa_c_=canvas_as_.getContext(d_aG_),
           tw_b_=texture_a_.width,
           th_u_=texture_a_.height,
           r_n_=300,
           uv_R_=
            _ax_
             (function(v_a_)
               {var
                 u_c_=
                  (tw_b_-
                   Math.atan2(v_a_[3],v_a_[1])*
                   ((tw_b_/2-num_0_99_cT_)/pi_l_)|
                   0)%
                  tw_b_,
                 v_d_=
                  th_u_/
                  2+
                  Math.asin(v_a_[2])*
                  ((th_u_-num_0_99_cT_)/pi_l_)|
                  0;
                if(0<=u_c_)
                 {if(u_c_<tw_b_)
                   {if(0<=v_d_)
                     {if(v_d_<th_u_)return [0,u_c_,v_d_];throw [0,_r_,_eZ_]}
                    throw [0,_r_,_e0_]}
                  throw [0,_r_,_e1_]}
                throw [0,_r_,_e2_]},
              vertices_I_),
           normals_aM_=
            _ax_
             (function(param_a_)
               {var
                 v1_j_=caml_array_get_k_(vertices_I_,param_a_[1]),
                 v2_l_=caml_array_get_k_(vertices_I_,param_a_[2]),
                 _b_=
                  vect_cw_(v1_j_,caml_array_get_k_(vertices_I_,param_a_[3])),
                 param_c_=vect_cw_(v1_j_,v2_l_),
                 z2_d_=_b_[3],
                 y2_e_=_b_[2],
                 x2_f_=_b_[1],
                 z1_g_=param_c_[3],
                 y1_h_=param_c_[2],
                 x1_i_=param_c_[1];
                return [num_254_z_,
                        y1_h_*z2_d_-y2_e_*z1_g_,
                        z1_g_*x2_f_-z2_d_*x1_i_,
                        x1_i_*y2_e_-x2_f_*y1_h_]},
              a_J_),
           paused_S_=[0,0],
           follow_at_=[0,0],
           lighting_au_=[0,1],
           clipped_av_=[0,1],
           obl_af_=[0,obliquity_ak_],
           face_info_aN_=
            _ax_
             (function(f_a_)
               {var
                 match_K_=caml_array_get_k_(uv_R_,f_a_[1]),
                 v1_l_=match_K_[2],
                 u1_L_=match_K_[1],
                 match_M_=caml_array_get_k_(uv_R_,f_a_[2]),
                 v2_m_=match_M_[2],
                 u2_n_=match_M_[1],
                 match_N_=caml_array_get_k_(uv_R_,f_a_[3]),
                 v3_o_=match_N_[2],
                 u3_f_=match_N_[1],
                 mid_c_=tw_b_/2;
                if(u1_L_==0)
                 {if(mid_c_<u2_n_)
                   var _C_=1;
                  else
                   if(mid_c_<u3_f_)var _C_=1;else var _B_=0,_C_=0;
                  if(_C_)var u1_g_=tw_b_-2,_B_=1}
                else
                 var _B_=0;
                if(!_B_)var u1_g_=u1_L_;
                if(u2_n_==0)
                 {if(mid_c_<u1_g_)
                   var _E_=1;
                  else
                   if(mid_c_<u3_f_)var _E_=1;else var _D_=0,_E_=0;
                  if(_E_)var u2_h_=tw_b_-2,_D_=1}
                else
                 var _D_=0;
                if(!_D_)var u2_h_=u2_n_;
                if(u3_f_==0)
                 {if(mid_c_<u2_h_)
                   var _G_=1;
                  else
                   if(mid_c_<u1_g_)var _G_=1;else var _F_=0,_G_=0;
                  if(_G_)var u3_i_=tw_b_-2,_F_=1}
                else
                 var _F_=0;
                if(!_F_)var u3_i_=u3_f_;
                var mth_p_=th_u_-2;
                if(v1_l_==0)
                 var _H_=0;
                else
                 if(mth_p_<=v1_l_)var _H_=0;else var u1_j_=u1_g_,_H_=1;
                if(!_H_)var u1_j_=(u2_h_+u3_i_)/2;
                if(v2_m_==0)
                 var _I_=0;
                else
                 if(mth_p_<=v2_m_)var _I_=0;else var u2_q_=u2_h_,_I_=1;
                if(!_I_)var u2_q_=(u1_j_+u3_i_)/2;
                if(v3_o_==0)
                 var _J_=0;
                else
                 if(mth_p_<=v3_o_)var _J_=0;else var u3_O_=u3_i_,_J_=1;
                if(!_J_)var u3_O_=(u2_q_+u1_j_)/2;
                var
                 u1_d_=max_s_(1,u1_j_),
                 u2_r_=max_s_(1,u2_q_),
                 u3_t_=max_s_(1,u3_O_),
                 v1_e_=max_s_(1,v1_l_),
                 v2_v_=max_s_(1,v2_m_),
                 v3_w_=max_s_(1,v3_o_),
                 du2_x_=u2_r_-u1_d_,
                 du3_y_=u3_t_-u1_d_,
                 dv2_z_=v2_v_-v1_e_,
                 dv3_A_=v3_w_-v1_e_,
                 su_P_=dv2_z_*du3_y_-dv3_A_*du2_x_,
                 sv_Q_=du2_x_*dv3_A_-du3_y_*dv2_z_,
                 u_S_=max_s_(0,min_$_(u1_d_,min_$_(u2_r_,u3_t_))-4),
                 v_T_=max_s_(0,min_$_(v1_e_,min_$_(v2_v_,v3_w_))-4),
                 u_U_=min_$_(tw_b_,max_s_(u1_d_,max_s_(u2_r_,u3_t_))+4);
                return [0,
                        u1_d_,
                        v1_e_,
                        du2_x_/su_P_,
                        dv2_z_/sv_Q_,
                        du3_y_/su_P_,
                        dv3_A_/sv_Q_,
                        u_S_,
                        v_T_,
                        u_U_-u_S_,
                        min_$_(th_u_,max_s_(v1_e_,max_s_(v2_v_,v3_w_))+4)-v_T_]},
              a_J_),
           m_obliq_aw_=[0,xy_rotation_cy_(-obliquity_ak_)],
           m_p_=[0,matrix_identity_cz_],
           phi_rot_F_=[0,0],
           rateText_ay_=doc_e_.createTextNode(str_f_),
           ctrl_v_=createDiv_aI_(doc_e_);
          ctrl_v_.className="controls";
          var d_ag_=createDiv_aI_(doc_e_);
          add_d_
           (d_ag_,doc_e_.createTextNode("Click and drag mouse to rotate."));
          add_d_(ctrl_v_,d_ag_);
          var form_j_=createDiv_aI_(doc_e_);
          function br_w_(param_a_){return unsafeCreateElement_Q_(doc_e_,_eL_)}
          add_d_
           (form_j_,
            toggle_button_cu_
             (_e6_,_e5_,function(p_a_){paused_S_[1]=p_a_;return 0}));
          add_d_(form_j_,br_w_(0));
          add_d_
           (form_j_,
            toggle_button_cu_
             (_e8_,_e7_,function(f_a_){follow_at_[1]=f_a_;return 0}));
          add_d_(form_j_,br_w_(0));
          var b_K_=createInput_bo_([0,button_type_ct_],0,doc_e_);
          b_K_.value="Reset orientation";
          b_K_.onclick=
          handler_C_
           (function(param_a_)
             {m_p_[1]=matrix_identity_cz_;
              phi_rot_F_[1]=0;
              m_obliq_aw_[1]=xy_rotation_cy_(-obl_af_[1]);
              return true_B_});
          add_d_(form_j_,b_K_);
          add_d_(form_j_,br_w_(0));
          var lab_T_=createLabel_cr_(doc_e_);
          add_d_(lab_T_,doc_e_.createTextNode("Date:"));
          var
           s_y_=unsafeCreateElementEx_cq_(0,0,doc_e_,_eG_),
           param_x_=param_e9_;
          for(;;)
           {if(param_x_)
             {var
               l_ah_=param_x_[2],
               a_ai_=param_x_[1],
               o_W_=unsafeCreateElement_Q_(doc_e_,_eF_);
              add_d_(o_W_,doc_e_.createTextNode(a_ai_.toString()));
              s_y_.add(o_W_,no_handler_aE_);
              var param_x_=l_ah_;
              continue}
            s_y_.onchange=
            handler_C_
             (function(param_a_)
               {var
                 match_b_=s_y_.selectedIndex,
                 o_c_=0===match_b_?obliquity_ak_:1===match_b_?0:-obliquity_ak_;
                update_shadow_ad_(o_c_);
                obl_af_[1]=o_c_;
                return true_B_});
            add_d_(lab_T_,s_y_);
            add_d_(form_j_,lab_T_);
            add_d_(ctrl_v_,form_j_);
            var form_o_=createDiv_aI_(doc_e_);
            add_d_
             (form_o_,
              checkbox_cv_
               (_e__,1,function(l_a_){lighting_au_[1]=l_a_;return 0}));
            add_d_(form_o_,br_w_(0));
            add_d_
             (form_o_,
              checkbox_cv_
               (_e$_,1,function(l_a_){clipped_av_[1]=l_a_;return 0}));
            add_d_(form_o_,br_w_(0));
            add_d_(form_o_,doc_e_.createTextNode("Frames per second: "));
            add_d_(form_o_,rateText_ay_);
            add_d_(ctrl_v_,form_o_);
            add_d_(doc_e_.body,ctrl_v_);
            var p_X_=unsafeCreateElement_Q_(doc_e_,_eK_);
            p_X_.innerHTML=
            "Credit: <a href='http://visibleearth.nasa.gov/'>Visual Earth</a>, Nasa";
            add_d_(doc_e_.body,p_X_);
            var mx_U_=[0,0],my_V_=[0,0];
            canvas_N_.onmousedown=
            handler_C_
             (function(ev_a_)
               {mx_U_[1]=ev_a_.clientX;
                my_V_[1]=ev_a_.clientY;
                var
                 c2_b_=[0,no_handler_aE_],
                 c1_d_=
                  addEventListener_cn_
                   (doc_e_,
                    mousemove_eE_,
                    handler_C_
                     (function(ev_a_)
                       {var
                         x_d_=ev_a_.clientX,
                         y_e_=ev_a_.clientY,
                         dx_f_=x_d_-mx_U_[1]|0,
                         dy_g_=y_e_-my_V_[1]|0;
                        if(0!==dy_g_)
                         {var
                           phi_h_=2*dy_g_/num_600_aq_,
                           cos_phi_b_=Math.cos(phi_h_),
                           sin_phi_c_=Math.sin(phi_h_),
                           _l_=m_p_[1],
                           _i_=vertex_m_(0,-sin_phi_c_,cos_phi_b_),
                           _j_=vertex_m_(0,cos_phi_b_,sin_phi_c_);
                          m_p_[1]=matrix_mul_aK_([0,vertex_m_(1,0,0),_j_,_i_],_l_)}
                        if(0!==dx_f_)
                         {var _k_=m_p_[1];
                          m_p_[1]=
                          matrix_mul_aK_(xz_rotation_bq_(2*dx_f_/num_600_aq_),_k_)}
                        mx_U_[1]=x_d_;
                        my_V_[1]=y_e_;
                        return true_B_}),
                    true_B_);
                c2_b_[1]=
                _cl_
                 (addEventListener_cn_
                   (doc_e_,
                    mouseup_eC_,
                    handler_C_
                     (function(param_a_)
                       {f_co_(c1_d_);
                        var _c_=c2_b_[1];
                        if(_c_!=no_handler_aE_)f_co_(_c_);
                        return true_B_}),
                    true_B_));
                return false_bm_});
            var
             ti_az_=[0,new a2c37ab8e_aF_().getTime()],
             fps_G_=[0,0],
             loop_aA_=
              function(t_aL_,phi_b_)
               {var rotation_aO_=xz_rotation_bq_(phi_b_-phi_rot_F_[1]);
                if(lighting_au_[1])
                 {no_lighting_L_[1]=0;
                  ctx_E_.drawImage(texture_a_,0,0);
                  var i_K_=(2*pi_l_-phi_b_%(2*pi_l_))*w_D_/2/pi_l_%w_D_|0;
                  ctx_E_.drawImage(canvas_A_,i_K_,0);
                  ctx_E_.drawImage(canvas_A_,i_K_-w_D_,0)}
                else
                 if(!no_lighting_L_[1])
                  {ctx_E_.drawImage(texture_a_,0,0);no_lighting_L_[1]=1}
                var
                 _aP_=matrix_mul_aK_(m_obliq_aw_[1],rotation_aO_),
                 m_aB_=matrix_mul_aK_(m_p_[1],_aP_),
                 _o_=
                  _ax_
                   (function(v_a_){return matrix_vect_mul_al_(m_aB_,v_a_)},
                    vertices_I_),
                 v_m_=matrix_vect_mul_al_(matrix_transp_cx_(m_aB_),v_e3_);
                a8b7209fa_c_.clearRect(0,0,num_600_aq_,num_600_aq_);
                a8b7209fa_c_.save();
                if(clipped_av_[1])
                 {a8b7209fa_c_.beginPath();
                  a8b7209fa_c_.arc(r_n_,r_n_,r_n_*0.95,0,-2*pi_l_,true_B_);
                  a8b7209fa_c_.clip()}
                a8b7209fa_c_.setTransform(r_n_-2,0,0,r_n_-2,r_n_,r_n_);
                a8b7209fa_c_.globalCompositeOperation="lighter";
                var _r_=a_J_.length-1-1|0,_aG_=0;
                if(!(_r_<0))
                 {var i_e_=_aG_;
                  for(;;)
                   {var
                     param_h_=a_J_[i_e_+1],
                     match_M_=caml_array_get_k_(_o_,param_h_[1]),
                     y1_f_=match_M_[2],
                     x1_g_=match_M_[1],
                     match_N_=caml_array_get_k_(_o_,param_h_[2]),
                     y2_Q_=match_N_[2],
                     x2_R_=match_N_[1],
                     match_T_=caml_array_get_k_(_o_,param_h_[3]),
                     y3_U_=match_T_[2],
                     x3_V_=match_T_[1],
                     param_q_=caml_array_get_k_(normals_aM_,i_e_);
                    if
                     (0<=
                      param_q_[1]*
                      v_m_[1]+
                      param_q_[2]*
                      v_m_[2]+
                      param_q_[3]*
                      v_m_[3])
                     {a8b7209fa_c_.beginPath();
                      a8b7209fa_c_.moveTo(x1_g_,y1_f_);
                      a8b7209fa_c_.lineTo(x2_R_,y2_Q_);
                      a8b7209fa_c_.lineTo(x3_V_,y3_U_);
                      a8b7209fa_c_.closePath();
                      a8b7209fa_c_.save();
                      a8b7209fa_c_.clip();
                      var
                       match_d_=caml_array_get_k_(face_info_aN_,i_e_),
                       dv_W_=match_d_[10],
                       du_X_=match_d_[9],
                       v___=match_d_[8],
                       u_$_=match_d_[7],
                       dv3_aa_=match_d_[6],
                       du3_ab_=match_d_[5],
                       dv2_ac_=match_d_[4],
                       du2_ad_=match_d_[3],
                       v1_af_=match_d_[2],
                       u1_ag_=match_d_[1],
                       dx2_ah_=x2_R_-x1_g_,
                       dx3_ai_=x3_V_-x1_g_,
                       dy2_aj_=y2_Q_-y1_f_,
                       dy3_ak_=y3_U_-y1_f_,
                       a_am_=dx2_ah_*dv3_aa_-dx3_ai_*dv2_ac_,
                       b_an_=dx2_ah_*du3_ab_-dx3_ai_*du2_ad_,
                       d_ao_=dy2_aj_*dv3_aa_-dy3_ak_*dv2_ac_,
                       e_ar_=dy2_aj_*du3_ab_-dy3_ak_*du2_ad_;
                      a8b7209fa_c_.transform
                       (a_am_,
                        d_ao_,
                        b_an_,
                        e_ar_,
                        x1_g_-a_am_*u1_ag_-b_an_*v1_af_,
                        y1_f_-d_ao_*u1_ag_-e_ar_*v1_af_);
                      a8b7209fa_c_.drawImage
                       (shd_ae_,u_$_,v___,du_X_,dv_W_,u_$_,v___,du_X_,dv_W_);
                      a8b7209fa_c_.restore()}
                    var _aH_=i_e_+1|0;
                    if(_r_!==i_e_){var i_e_=_aH_;continue}
                    break}}
                a8b7209fa_c_.restore();
                ctx_O_.globalCompositeOperation=str_copy_cR_;
                ctx_O_.drawImage(canvas_as_,0,0);
                try {ctx_O_.getImageData(0,0,1,1)}catch(_f_){}
                var
                 t_aC_=new a2c37ab8e_aF_().getTime(),
                 hz_aD_=num_1e3_ap_/(t_aC_-ti_az_[1]),
                 hz_aQ_=fps_G_[1]==0?hz_aD_:0.9*fps_G_[1]+0.1*hz_aD_;
                fps_G_[1]=hz_aQ_;
                var _aR_=fps_G_[1];
                rateText_ay_.data=
                caml_call_gen1_i_(_t_(_fa_),_aR_).toString();
                ti_az_[1]=t_aC_;
                function _aS_(param_a_)
                 {var
                   t_d_=new a2c37ab8e_aF_().getTime(),
                   dt_c_=t_d_-t_aL_,
                   dt_f_=dt_c_<0?0:num_1e3_ap_<dt_c_?0:dt_c_,
                   angle_e_=2*pi_l_*dt_f_/num_1e3_ap_/10,
                   _h_=
                    paused_S_[1]
                     ?0
                     :follow_at_[1]?(phi_rot_F_[1]=phi_rot_F_[1]+angle_e_,1):0,
                   phi_g_=paused_S_[1]?phi_b_:phi_b_+angle_e_;
                  return loop_aA_(t_d_,phi_g_)}
                var
                 match_w_=task_cg_(0),
                 t_x_=match_w_[1],
                 id_y_=[0,0],
                 d_aT_=0.01,
                 w_aJ_=match_w_[2];
                function wait_z_(d_a_,param_b_)
                 {var
                   match_c_=
                    num_2147483_bz_<d_a_
                     ?[0,overflow_limit_eR_,d_a_-num_2147483_bz_]
                     :[0,d_a_,0],
                   remain_d_=match_c_[2],
                   step_e_=match_c_[1],
                   cb_f_=
                    remain_d_==0
                     ?function(_a_){return wakeup_bj_(w_aJ_,_a_)}
                     :function(_a_){return wait_z_(remain_d_,_a_)};
                  id_y_[1]=
                  [0,
                   window_Z_.setTimeout
                    (caml_js_wrap_callback_bw_(cb_f_),step_e_*num_1e3_ap_)];
                  return 0}
                wait_z_(d_aT_,0);
                function f_C_(param_a_)
                 {var _b_=id_y_[1];
                  return _b_?window_Z_.clearTimeout(_b_[1]):0}
                var _j_=repr_Y_(t_x_)[1];
                switch(_j_[0])
                 {case 1:
                   var
                    _aE_=
                     _j_[1][1]===Canceled_bf_?(call_unsafe_ce_(f_C_,0),1):0;
                   break;
                  case 2:
                   var
                    sleeper_s_=_j_[1],
                    handler_u_=[0,current_data_P_[1],f_C_],
                    _v_=sleeper_s_[4],
                    handler_aI_=
                     typeof _v_===str_number_H_?handler_u_:[2,handler_u_,_v_];
                   sleeper_s_[4]=handler_aI_;
                   var _aE_=1;
                   break;
                  default:var _aE_=0}
                return bind_bl_(t_x_,_aS_)};
            return loop_aA_(new a2c37ab8e_aF_().getTime(),0)}}
        var v_c_=unsafeCreateElement_Q_(doc_e_,_eM_);
        function _o_(param_a_){return [0,[0,v_c_]]}
        var match_g_=task_cg_(0),w_j_=match_g_[2],t_n_=match_g_[1];
        v_c_.onload=
        handler_C_(function(param_a_){wakeup_bj_(w_j_,0);return false_bm_});
        v_c_.src=src_e4_;
        var _b_=repr_Y_(bind_bl_(bind_bl_(t_n_,_o_),_p_))[1];
        switch(_b_[0])
         {case 1:throw _b_[1];
          case 2:
           var sleeper_h_=_b_[1];
           add_immutable_waiter_ch_
            (sleeper_h_,
             function(param_a_)
              {switch(param_a_[0])
                {case 0:return 0;
                 case 1:
                  return caml_call_gen1_i_
                          (async_exception_hook_bh_[1],param_a_[1]);
                 default:throw [0,_r_,_eu_]}});
           break;
          case 3:throw [0,_r_,_ev_]
          }
        return false_bm_});
    do_at_exit_a7_(0);
    return}
  (this));
