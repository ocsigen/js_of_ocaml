// This program was compiled from OCaml by js_of_ocaml 2.00dev+git-f7cce66
(function(joo_global_object_b_)
   {"use strict";
    var
     num_125_bU_=125,
     num_123_b0_=123,
     num_254_S_=254,
     num_255_aa_=255,
     num_108_dt_=108,
     str_dh_='"',
     num_16777215_E_=16777215,
     str_bZ_="=",
     str_Content_Disposit_abr_dm_='Content-Disposition: form-data; name="',
     num_250_dn_=250,
     num_0_5_b5_=0.5,
     str_jsError_dg_="jsError",
     str_POST_bd_="POST",
     num_2147483_bT_=2147483,
     num_550809787_dd_=-550809787,
     num_115_aE_=115,
     num_102_b3_=102,
     str_ds_="&",
     num_120_bY_=120,
     str_dc_="--",
     num_117_bX_=117,
     num_126925477_bc_=126925477,
     str_d_="",
     num_781515420_a4_=781515420,
     num_100_ba_=100,
     str_0_y_="0",
     num_103_b2_=103,
     str_fd_dr_="fd ",
     num_936573133_db_=936573133,
     num_1e3_a$_=1e3,
     str_src_core_lwt_ml_R_="src/core/lwt.ml",
     str_ak_=".",
     num_65535_a__=65535,
     str_aB_="+",
     str_g_aA_="g",
     str_f_a3_="f",
     num_105_ac_=105,
     str_d_df_="%d",
     num_443_de_=443,
     num_88_dq_=-88,
     num_110_aD_=110,
     str_a2_="?",
     str_a6_="'",
     str_int_of_string_a5_="int_of_string",
     num_32_dl_=-32,
     num_111_b1_=111,
     str_D_=" ",
     str_e_aC_="e",
     str_lastIndex_dk_="lastIndex",
     num_891486873_bb_=891486873,
     str_da_=":",
     str_ab_="-",
     num_48_aj_=-48,
     str_nan_dj_="nan",
     num_116_bS_=116,
     str_canvas_bW_="canvas",
     str_a9_="\r\n",
     str_12g_di_="%.12g",
     str_file_already_abr_b4_=" : file already exists",
     str_Q_="/",
     num_114_a8_=114,
     str_a7_="#",
     num_101_dp_=101,
     num_0_1_bV_=0.1,
     str_index_out_of_bounds_do_="index out of bounds",
     str_number_u_="number";
    function caml_raise_with_arg_dH_(tag_a_,arg_b_){throw [0,tag_a_,arg_b_]}
    function js_print_stderr_b__(s_a_)
     {if(s_a_.charCodeAt(s_a_.length-1)==10)s_a_=s_a_.substr(0,s_a_.length-1);
      var v_c_=joo_global_object_b_.console;
      v_c_&&v_c_.error&&v_c_.error(s_a_)}
    var caml_global_data_e_=[0];
    function caml_str_repeat_aH_(n_a_,s_b_)
     {if(!n_a_)return str_d_;
      if(n_a_&1)return caml_str_repeat_aH_(n_a_-1,s_b_)+s_b_;
      var r_c_=caml_str_repeat_aH_(n_a_>>1,s_b_);
      return r_c_+r_c_}
    function MlString_o_(param_a_)
     {if(param_a_!=null)
       {this.bytes=this.fullBytes=param_a_;this.last=this.len=param_a_.length}}
    function mlstring_bound_error_dI_()
     {caml_raise_with_arg_dH_
       (caml_global_data_e_[4],new MlString_o_(str_index_out_of_bounds_do_))}
    MlString_o_.prototype=
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
        {js_print_stderr_b__
          ('MlString.toJsString: wrong encoding for "%s" ',a_a_);
         return a_a_}},
     toBytes:
     function()
      {if(this.string!=null)
        try
         {var b_a_=unescape(encodeURIComponent(this.string))}
        catch(e_f_)
         {js_print_stderr_b__
           ('MlString.toBytes: wrong encoding for "%s" ',this.string);
          var b_a_=this.string}
       else
        {var b_a_=str_d_,a_c_=this.array,l_e_=a_c_.length;
         for(var i_b_=0;i_b_<l_e_;i_b_++)b_a_+=String.fromCharCode(a_c_[i_b_])}
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
        {this.bytes=b_a_+=caml_str_repeat_aH_(this.len-this.last,"\0");
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
       if(i_a_<0||i_a_>=this.len)mlstring_bound_error_dI_();
       return this.get(i_a_)},
     set:
     function(i_a_,c_b_)
      {var a_c_=this.array;
       if(!a_c_)
        {if(this.last==i_a_)
          {this.bytes+=String.fromCharCode(c_b_&num_255_aa_);
           this.last++;
           return 0}
         a_c_=this.toArray()}
       else
        if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;
       a_c_[i_a_]=c_b_&num_255_aa_;
       return 0},
     safeSet:
     function(i_a_,c_b_)
      {if(this.len==null)this.toBytes();
       if(i_a_<0||i_a_>=this.len)mlstring_bound_error_dI_();
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
    function MlWrappedString_p_(s_a_){this.string=s_a_}
    MlWrappedString_p_.prototype=new MlString_o_();
    function caml_raise_with_string_b9_(tag_a_,msg_b_)
     {caml_raise_with_arg_dH_(tag_a_,new MlWrappedString_p_(msg_b_))}
    function caml_invalid_argument_al_(msg_a_)
     {caml_raise_with_string_b9_(caml_global_data_e_[4],msg_a_)}
    function caml_array_bound_error_dv_()
     {caml_invalid_argument_al_(str_index_out_of_bounds_do_)}
    function caml_array_get_gS_(array_a_,index_b_)
     {if(index_b_<0||index_b_>=array_a_.length-1)caml_array_bound_error_dv_();
      return array_a_[index_b_+1]}
    function caml_array_set_gT_(array_a_,index_b_,newval_c_)
     {if(index_b_<0||index_b_>=array_a_.length-1)caml_array_bound_error_dv_();
      array_a_[index_b_+1]=newval_c_;
      return 0}
    function caml_blit_string_dw_(s1_a_,i1_b_,s2_c_,i2_d_,len_e_)
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
    function caml_call_gen_T_(f_c_,args_b_)
     {if(f_c_.fun)return caml_call_gen_T_(f_c_.fun,args_b_);
      var n_a_=f_c_.length,d_d_=n_a_-args_b_.length;
      if(d_d_==0)
       return f_c_.apply(null,args_b_);
      else
       if(d_d_<0)
        return caml_call_gen_T_
                (f_c_.apply(null,args_b_.slice(0,n_a_)),args_b_.slice(n_a_));
       else
        return function(x_a_)
         {return caml_call_gen_T_(f_c_,args_b_.concat([x_a_]))}}
    function caml_classify_float_gU_(x_a_)
     {if(isFinite(x_a_))
       {if(Math.abs(x_a_)>=2.22507385850720138e-308)return 0;
        if(x_a_!=0)return 1;
        return 2}
      return isNaN(x_a_)?4:3}
    function caml_convert_raw_backtrace_gV_(){return 0}
    function MlMakeString_du_(l_a_){this.bytes=str_d_;this.len=l_a_}
    MlMakeString_du_.prototype=new MlString_o_();
    function caml_create_string_dy_(len_a_)
     {if(len_a_<0)caml_invalid_argument_al_("String.create");
      return new MlMakeString_du_(len_a_)}
    function caml_int64_compare_g4_(x_a_,y_b_)
     {var x3_c_=x_a_[3]<<16,y3_d_=y_b_[3]<<16;
      if(x3_c_>y3_d_)return 1;
      if(x3_c_<y3_d_)return -1;
      if(x_a_[2]>y_b_[2])return 1;
      if(x_a_[2]<y_b_[2])return -1;
      if(x_a_[1]>y_b_[1])return 1;
      if(x_a_[1]<y_b_[1])return -1;
      return 0}
    function caml_int_compare_hc_(a_a_,b_b_)
     {if(a_a_<b_b_)return -1;if(a_a_==b_b_)return 0;return 1}
    function caml_compare_val_dx_(a_a_,b_b_,total_c_)
     {var stack_e_=[];
      for(;;)
       {if(!(total_c_&&a_a_===b_b_))
         if(a_a_ instanceof MlString_o_)
          if(b_b_ instanceof MlString_o_)
           {if(a_a_!==b_b_)
             {var x_d_=a_a_.compare(b_b_);if(x_d_!=0)return x_d_}}
          else
           return 1;
         else
          if(a_a_ instanceof Array&&a_a_[0]===(a_a_[0]|0))
           {var ta_f_=a_a_[0];
            if(ta_f_===num_254_S_)ta_f_=0;
            if(ta_f_===num_250_dn_)
             {a_a_=a_a_[1];continue}
            else
             if(b_b_ instanceof Array&&b_b_[0]===(b_b_[0]|0))
              {var tb_g_=b_b_[0];
               if(tb_g_===num_254_S_)tb_g_=0;
               if(tb_g_===num_250_dn_)
                {b_b_=b_b_[1];continue}
               else
                if(ta_f_!=tb_g_)
                 return ta_f_<tb_g_?-1:1;
                else
                 switch(ta_f_)
                  {case 248:
                    var x_d_=caml_int_compare_hc_(a_a_[2],b_b_[2]);
                    if(x_d_!=0)return x_d_;
                    break;
                   case 251:caml_invalid_argument_al_("equal: abstract value");
                   case num_255_aa_:
                    var x_d_=caml_int64_compare_g4_(a_a_,b_b_);
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
            (b_b_ instanceof MlString_o_||
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
    function caml_equal_gX_(x_a_,y_b_)
     {return +(caml_compare_val_dx_(x_a_,y_b_,false)==0)}
    function caml_fill_string_gY_(s_a_,i_b_,l_c_,c_d_)
     {s_a_.fill(i_b_,l_c_,c_d_)}
    function caml_failwith_aG_(msg_a_)
     {caml_raise_with_string_b9_(caml_global_data_e_[3],msg_a_)}
    function caml_float_of_string_gZ_(s_a_)
     {var res_b_;
      s_a_=s_a_.getFullBytes();
      res_b_=+s_a_;
      if(s_a_.length>0&&res_b_===res_b_)return res_b_;
      s_a_=s_a_.replace(/_/g,str_d_);
      res_b_=+s_a_;
      if(s_a_.length>0&&res_b_===res_b_||/^[+-]?nan$/i.test(s_a_))
       return res_b_;
      if(/^ *0x[0-9a-f_]+p[+-]?[0-9_]+/i.test(s_a_))
       {var pidx_c_=s_a_.indexOf("p");
        pidx_c_=pidx_c_==-1?s_a_.indexOf("P"):pidx_c_;
        var exp_e_=+s_a_.substring(pidx_c_+1);
        res_b_=+s_a_.substring(0,pidx_c_);
        return res_b_*Math.pow(2,exp_e_)}
      caml_failwith_aG_("float_of_string")}
    function caml_parse_format_b8_(fmt_a_)
     {fmt_a_=fmt_a_.toString();
      var len_e_=fmt_a_.length;
      if(len_e_>31)caml_invalid_argument_al_("format_int: format too long");
      var
       f_b_=
        {justify:str_aB_,
         signstyle:str_ab_,
         filler:str_D_,
         alternate:false,
         base:0,
         signedconv:false,
         width:0,
         uppercase:false,
         sign:1,
         prec:-1,
         conv:str_f_a3_};
      for(var i_d_=0;i_d_<len_e_;i_d_++)
       {var c_c_=fmt_a_.charAt(i_d_);
        switch(c_c_)
         {case str_ab_:f_b_.justify=str_ab_;break;
          case str_aB_:
          case str_D_:f_b_.signstyle=c_c_;break;
          case str_0_y_:f_b_.filler=str_0_y_;break;
          case str_a7_:f_b_.alternate=true;break;
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
          case str_ak_:
           f_b_.prec=0;
           i_d_++;
           while(c_c_=fmt_a_.charCodeAt(i_d_)-48,c_c_>=0&&c_c_<=9)
            {f_b_.prec=f_b_.prec*10+c_c_;i_d_++}
           i_d_--;
          case "d":
          case "i":f_b_.signedconv=true;
          case "u":f_b_.base=10;break;
          case "x":f_b_.base=16;break;
          case "X":f_b_.base=16;f_b_.uppercase=true;break;
          case "o":f_b_.base=8;break;
          case str_e_aC_:
          case str_f_a3_:
          case str_g_aA_:f_b_.signedconv=true;f_b_.conv=c_c_;break;
          case "E":
          case "F":
          case "G":
           f_b_.signedconv=true;
           f_b_.uppercase=true;
           f_b_.conv=c_c_.toLowerCase();
           break
          }}
      return f_b_}
    function caml_finish_formatting_b6_(f_a_,rawbuffer_b_)
     {if(f_a_.uppercase)rawbuffer_b_=rawbuffer_b_.toUpperCase();
      var len_f_=rawbuffer_b_.length;
      if(f_a_.signedconv&&(f_a_.sign<0||f_a_.signstyle!=str_ab_))len_f_++;
      if(f_a_.alternate){if(f_a_.base==8)len_f_+=1;if(f_a_.base==16)len_f_+=2}
      var buffer_c_=str_d_;
      if(f_a_.justify==str_aB_&&f_a_.filler==str_D_)
       for(var i_e_=len_f_;i_e_<f_a_.width;i_e_++)buffer_c_+=str_D_;
      if(f_a_.signedconv)
       if(f_a_.sign<0)
        buffer_c_+=str_ab_;
       else
        if(f_a_.signstyle!=str_ab_)buffer_c_+=f_a_.signstyle;
      if(f_a_.alternate&&f_a_.base==8)buffer_c_+=str_0_y_;
      if(f_a_.alternate&&f_a_.base==16)buffer_c_+="0x";
      if(f_a_.justify==str_aB_&&f_a_.filler==str_0_y_)
       for(var i_e_=len_f_;i_e_<f_a_.width;i_e_++)buffer_c_+=str_0_y_;
      buffer_c_+=rawbuffer_b_;
      if(f_a_.justify==str_ab_)
       for(var i_e_=len_f_;i_e_<f_a_.width;i_e_++)buffer_c_+=str_D_;
      return new MlWrappedString_p_(buffer_c_)}
    function caml_format_float_g0_(fmt_a_,x_b_)
     {var
       s_c_,
       f_f_=caml_parse_format_b8_(fmt_a_),
       prec_e_=f_f_.prec<0?6:f_f_.prec;
      if(x_b_<0){f_f_.sign=-1;x_b_=-x_b_}
      if(isNaN(x_b_))
       {s_c_=str_nan_dj_;f_f_.filler=str_D_}
      else
       if(!isFinite(x_b_))
        {s_c_="inf";f_f_.filler=str_D_}
       else
        switch(f_f_.conv)
         {case str_e_aC_:
           var s_c_=x_b_.toExponential(prec_e_),i_d_=s_c_.length;
           if(s_c_.charAt(i_d_-3)==str_e_aC_)
            s_c_=s_c_.slice(0,i_d_-1)+str_0_y_+s_c_.slice(i_d_-1);
           break;
          case str_f_a3_:s_c_=x_b_.toFixed(prec_e_);break;
          case str_g_aA_:
           prec_e_=prec_e_?prec_e_:1;
           s_c_=x_b_.toExponential(prec_e_-1);
           var j_i_=s_c_.indexOf(str_e_aC_),exp_h_=+s_c_.slice(j_i_+1);
           if(exp_h_<-4||x_b_.toFixed(0).length>prec_e_)
            {var i_d_=j_i_-1;
             while(s_c_.charAt(i_d_)==str_0_y_)i_d_--;
             if(s_c_.charAt(i_d_)==str_ak_)i_d_--;
             s_c_=s_c_.slice(0,i_d_+1)+s_c_.slice(j_i_);
             i_d_=s_c_.length;
             if(s_c_.charAt(i_d_-3)==str_e_aC_)
              s_c_=s_c_.slice(0,i_d_-1)+str_0_y_+s_c_.slice(i_d_-1);
             break}
           else
            {var p_g_=prec_e_;
             if(exp_h_<0)
              {p_g_-=exp_h_+1;s_c_=x_b_.toFixed(p_g_)}
             else
              while(s_c_=x_b_.toFixed(p_g_),s_c_.length>prec_e_+1)p_g_--;
             if(p_g_)
              {var i_d_=s_c_.length-1;
               while(s_c_.charAt(i_d_)==str_0_y_)i_d_--;
               if(s_c_.charAt(i_d_)==str_ak_)i_d_--;
               s_c_=s_c_.slice(0,i_d_+1)}}
           break
          }
      return caml_finish_formatting_b6_(f_f_,s_c_)}
    function caml_format_int_g1_(fmt_a_,i_b_)
     {if(fmt_a_.toString()==str_d_df_)
       return new MlWrappedString_p_(str_d_+i_b_);
      var f_c_=caml_parse_format_b8_(fmt_a_);
      if(i_b_<0)if(f_c_.signedconv){f_c_.sign=-1;i_b_=-i_b_}else i_b_>>>=0;
      var s_e_=i_b_.toString(f_c_.base);
      if(f_c_.prec>=0)
       {f_c_.filler=str_D_;
        var n_f_=f_c_.prec-s_e_.length;
        if(n_f_>0)s_e_=caml_str_repeat_aH_(n_f_,str_0_y_)+s_e_}
      return caml_finish_formatting_b6_(f_c_,s_e_)}
    function caml_get_exception_raw_backtrace_g3_(){return 0}
    function caml_int64_is_zero_g7_(x_a_){return (x_a_[3]|x_a_[2]|x_a_[1])==0}
    function caml_int64_of_int32_g__(x_a_)
     {return [num_255_aa_,
              x_a_&num_16777215_E_,
              x_a_>>24&num_16777215_E_,
              x_a_>>31&num_65535_a__]}
    function caml_int64_sub_g$_(x_a_,y_b_)
     {var
       z1_c_=x_a_[1]-y_b_[1],
       z2_d_=x_a_[2]-y_b_[2]+(z1_c_>>24),
       z3_e_=x_a_[3]-y_b_[3]+(z2_d_>>24);
      return [num_255_aa_,
              z1_c_&num_16777215_E_,
              z2_d_&num_16777215_E_,
              z3_e_&num_65535_a__]}
    function caml_int64_ucompare_dA_(x_a_,y_b_)
     {if(x_a_[3]>y_b_[3])return 1;
      if(x_a_[3]<y_b_[3])return -1;
      if(x_a_[2]>y_b_[2])return 1;
      if(x_a_[2]<y_b_[2])return -1;
      if(x_a_[1]>y_b_[1])return 1;
      if(x_a_[1]<y_b_[1])return -1;
      return 0}
    function caml_int64_lsl1_dz_(x_a_)
     {x_a_[3]=x_a_[3]<<1|x_a_[2]>>23;
      x_a_[2]=(x_a_[2]<<1|x_a_[1]>>23)&num_16777215_E_;
      x_a_[1]=x_a_[1]<<1&num_16777215_E_}
    function caml_int64_lsr1_g8_(x_a_)
     {x_a_[1]=(x_a_[1]>>>1|x_a_[2]<<23)&num_16777215_E_;
      x_a_[2]=(x_a_[2]>>>1|x_a_[3]<<23)&num_16777215_E_;
      x_a_[3]=x_a_[3]>>>1}
    function caml_int64_udivmod_hb_(x_a_,y_b_)
     {var
       offset_e_=0,
       modulus_d_=x_a_.slice(),
       divisor_c_=y_b_.slice(),
       quotient_f_=[num_255_aa_,0,0,0];
      while(caml_int64_ucompare_dA_(modulus_d_,divisor_c_)>0)
       {offset_e_++;caml_int64_lsl1_dz_(divisor_c_)}
      while(offset_e_>=0)
       {offset_e_--;
        caml_int64_lsl1_dz_(quotient_f_);
        if(caml_int64_ucompare_dA_(modulus_d_,divisor_c_)>=0)
         {quotient_f_[1]++;
          modulus_d_=caml_int64_sub_g$_(modulus_d_,divisor_c_)}
        caml_int64_lsr1_g8_(divisor_c_)}
      return [0,quotient_f_,modulus_d_]}
    function caml_int64_to_int32_ha_(x_a_){return x_a_[1]|x_a_[2]<<24}
    function caml_int64_is_negative_g6_(x_a_){return x_a_[3]<<16<0}
    function caml_int64_neg_g9_(x_a_)
     {var
       y1_b_=-x_a_[1],
       y2_c_=-x_a_[2]+(y1_b_>>24),
       y3_d_=-x_a_[3]+(y2_c_>>24);
      return [num_255_aa_,
              y1_b_&num_16777215_E_,
              y2_c_&num_16777215_E_,
              y3_d_&num_65535_a__]}
    function caml_int64_format_g5_(fmt_a_,x_b_)
     {var f_c_=caml_parse_format_b8_(fmt_a_);
      if(f_c_.signedconv&&caml_int64_is_negative_g6_(x_b_))
       {f_c_.sign=-1;x_b_=caml_int64_neg_g9_(x_b_)}
      var
       buffer_e_=str_d_,
       wbase_i_=caml_int64_of_int32_g__(f_c_.base),
       cvtbl_h_="0123456789abcdef";
      do
       {var p_g_=caml_int64_udivmod_hb_(x_b_,wbase_i_);
        x_b_=p_g_[1];
        buffer_e_=cvtbl_h_.charAt(caml_int64_to_int32_ha_(p_g_[2]))+buffer_e_}
      while
       (!caml_int64_is_zero_g7_(x_b_));
      if(f_c_.prec>=0)
       {f_c_.filler=str_D_;
        var n_f_=f_c_.prec-buffer_e_.length;
        if(n_f_>0)buffer_e_=caml_str_repeat_aH_(n_f_,str_0_y_)+buffer_e_}
      return caml_finish_formatting_b6_(f_c_,buffer_e_)}
    function caml_parse_sign_and_base_hu_(s_a_)
     {var i_b_=0,base_c_=10,sign_d_=s_a_.get(0)==45?(i_b_++,-1):1;
      if(s_a_.get(i_b_)==48)
       switch(s_a_.get(i_b_+1))
        {case num_120_bY_:
         case 88:base_c_=16;i_b_+=2;break;
         case num_111_b1_:
         case 79:base_c_=8;i_b_+=2;break;
         case 98:
         case 66:base_c_=2;i_b_+=2;break
         }
      return [i_b_,sign_d_,base_c_]}
    function caml_parse_digit_dF_(c_a_)
     {if(c_a_>=48&&c_a_<=57)return c_a_-48;
      if(c_a_>=65&&c_a_<=90)return c_a_-55;
      if(c_a_>=97&&c_a_<=122)return c_a_-87;
      return -1}
    function caml_int_of_string_hd_(s_a_)
     {var
       r_g_=caml_parse_sign_and_base_hu_(s_a_),
       i_f_=r_g_[0],
       sign_h_=r_g_[1],
       base_d_=r_g_[2],
       threshold_i_=-1>>>0,
       c_e_=s_a_.get(i_f_),
       d_c_=caml_parse_digit_dF_(c_e_);
      if(d_c_<0||d_c_>=base_d_)caml_failwith_aG_(str_int_of_string_a5_);
      var res_b_=d_c_;
      for(;;)
       {i_f_++;
        c_e_=s_a_.get(i_f_);
        if(c_e_==95)continue;
        d_c_=caml_parse_digit_dF_(c_e_);
        if(d_c_<0||d_c_>=base_d_)break;
        res_b_=base_d_*res_b_+d_c_;
        if(res_b_>threshold_i_)caml_failwith_aG_(str_int_of_string_a5_)}
      if(i_f_!=s_a_.getLen())caml_failwith_aG_(str_int_of_string_a5_);
      res_b_=sign_h_*res_b_;
      if(base_d_==10&&(res_b_|0)!=res_b_)
       caml_failwith_aG_(str_int_of_string_a5_);
      return res_b_|0}
    function caml_is_printable_he_(c_a_){return +(c_a_>31&&c_a_<127)}
    function caml_js_from_byte_string_hf_(s_a_){return s_a_.getFullBytes()}
    function caml_js_get_console_hg_()
     {var
       c_c_=joo_global_object_b_.console?joo_global_object_b_.console:{},
       m_d_=
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
      function f_e_(){}
      for(var i_a_=0;i_a_<m_d_.length;i_a_++)
       if(!c_c_[m_d_[i_a_]])c_c_[m_d_[i_a_]]=f_e_;
      return c_c_}
    function caml_js_on_ie_hh_()
     {var
       ua_a_=
        joo_global_object_b_.navigator
         ?joo_global_object_b_.navigator.userAgent
         :str_d_;
      return ua_a_.indexOf("MSIE")!=-1&&ua_a_.indexOf("Opera")!=0}
    function caml_js_to_byte_string_hi_(s_a_){return new MlString_o_(s_a_)}
    function caml_js_wrap_callback_hj_(f_a_)
     {var toArray_c_=Array.prototype.slice;
      return function()
       {var args_b_=arguments.length>0?toArray_c_.call(arguments):[undefined];
        return caml_call_gen_T_(f_a_,args_b_)}}
    function caml_make_vect_hk_(len_a_,init_b_)
     {var b_d_=[0];
      for(var i_c_=1;i_c_<=len_a_;i_c_++)b_d_[i_c_]=init_b_;
      return b_d_}
    function caml_raise_sys_error_z_(msg_a_)
     {caml_raise_with_string_b9_(caml_global_data_e_[2],msg_a_)}
    function caml_ml_flush_dB_(oc_a_)
     {if(!oc_a_.opened)
       caml_raise_sys_error_z_("Cannot flush a closed channel");
      if(oc_a_.buffer==str_d_)return 0;
      if(oc_a_.output)
       switch(oc_a_.output.length)
        {case 2:oc_a_.output(oc_a_,oc_a_.buffer);break;
         default:oc_a_.output(oc_a_.buffer)}
      oc_a_.buffer=str_d_}
    function caml_raise_no_such_file_dG_(name_a_)
     {name_a_=name_a_ instanceof MlString_o_?name_a_.toString():name_a_;
      caml_raise_sys_error_z_(name_a_+": No such file or directory")}
    var caml_current_dir_gW_=str_Q_;
    function caml_make_path_be_(name_a_)
     {name_a_=name_a_ instanceof MlString_o_?name_a_.toString():name_a_;
      if(name_a_.charCodeAt(0)!=47)name_a_=caml_current_dir_gW_+name_a_;
      var comp_e_=name_a_.split(str_Q_),ncomp_b_=[];
      for(var i_c_=0;i_c_<comp_e_.length;i_c_++)
       switch(comp_e_[i_c_])
        {case "..":if(ncomp_b_.length>1)ncomp_b_.pop();break;
         case str_ak_:
         case str_d_:if(ncomp_b_.length==0)ncomp_b_.push(str_d_);break;
         default:ncomp_b_.push(comp_e_[i_c_]);break}
      ncomp_b_.orig=name_a_;
      return ncomp_b_}
    function MlDir_ad_(){this.content={}}
    MlDir_ad_.prototype=
    {exists:function(name_a_){return this.content[name_a_]?1:0},
     mk:function(name_a_,c_b_){this.content[name_a_]=c_b_},
     get:function(name_a_){return this.content[name_a_]},
     list:
     function()
      {var a_a_=[];for(var n_b_ in this.content)a_a_.push(n_b_);return a_a_},
     remove:function(name_a_){delete this.content[name_a_]}};
    var caml_root_dir_bg_=new MlDir_ad_();
    caml_root_dir_bg_.mk(str_d_,new MlDir_ad_());
    function caml_fs_content_b7_(path_a_)
     {var dir_b_=caml_root_dir_bg_;
      for(var i_c_=0;i_c_<path_a_.length;i_c_++)
       {if(!(dir_b_.exists&&dir_b_.exists(path_a_[i_c_])))
         caml_raise_no_such_file_dG_(path_a_.orig);
        dir_b_=dir_b_.get(path_a_[i_c_])}
      return dir_b_}
    function caml_sys_is_directory_hD_(name_a_)
     {var
       path_c_=caml_make_path_be_(name_a_),
       dir_b_=caml_fs_content_b7_(path_c_);
      return dir_b_ instanceof MlDir_ad_?1:0}
    function MlFile_aF_(content_a_){this.data=content_a_}
    MlFile_aF_.prototype=
    {content:function(){return this.data},
     truncate:function(){this.data.length=0}};
    function caml_fs_register_g2_(name_a_,content_b_)
     {var path_e_=caml_make_path_be_(name_a_),dir_c_=caml_root_dir_bg_;
      for(var i_f_=0;i_f_<path_e_.length-1;i_f_++)
       {var d_d_=path_e_[i_f_];
        if(!dir_c_.exists(d_d_))dir_c_.mk(d_d_,new MlDir_ad_());
        dir_c_=dir_c_.get(d_d_);
        if(!(dir_c_ instanceof MlDir_ad_))
         caml_raise_sys_error_z_(path_e_.orig+str_file_already_abr_b4_)}
      var d_d_=path_e_[path_e_.length-1];
      if(dir_c_.exists(d_d_))
       caml_raise_sys_error_z_(path_e_.orig+str_file_already_abr_b4_);
      if(content_b_ instanceof MlDir_ad_)
       dir_c_.mk(d_d_,content_b_);
      else
       if(content_b_ instanceof MlFile_aF_)
        dir_c_.mk(d_d_,content_b_);
       else
        if(content_b_ instanceof MlString_o_)
         dir_c_.mk(d_d_,new MlFile_aF_(content_b_.getArray()));
        else
         if(content_b_ instanceof Array)
          dir_c_.mk(d_d_,new MlFile_aF_(content_b_));
         else
          if(content_b_.toString)
           dir_c_.mk
            (d_d_,
             new MlFile_aF_(new MlString_o_(content_b_.toString()).getArray()));
          else
           caml_invalid_argument_al_("caml_fs_register")}
    function caml_sys_file_exists_hC_(name_a_)
     {var
       dir_b_=caml_root_dir_bg_,
       path_d_=caml_make_path_be_(name_a_),
       auto_load_e_;
      for(var i_c_=0;i_c_<path_d_.length;i_c_++)
       {if(dir_b_.auto)auto_load_e_=dir_b_.auto;
        if(!(dir_b_.exists&&dir_b_.exists(path_d_[i_c_])))
         return auto_load_e_?auto_load_e_(path_d_.join(str_Q_)):0;
        dir_b_=dir_b_.get(path_d_[i_c_])}
      return 1}
    function caml_sys_open_internal_aI_(idx_a_,v_b_,flags_c_)
     {if(caml_global_data_e_.fds===undefined)
       caml_global_data_e_.fds=new Array();
      flags_c_=flags_c_?flags_c_:{};
      var data_d_={};
      data_d_.array=v_b_;
      data_d_.offset=flags_c_.append?data_d_.array.length:0;
      data_d_.flags=flags_c_;
      caml_global_data_e_.fds[idx_a_]=data_d_;
      caml_global_data_e_.fd_last_idx=idx_a_;
      return idx_a_}
    function caml_sys_open_hL_(name_a_,flags_b_,perms_c_)
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
      var name2_g_=name_a_.toString(),path_i_=caml_make_path_be_(name_a_);
      if(f_d_.rdonly&&f_d_.wronly)
       caml_raise_sys_error_z_
        (name2_g_+" : flags Open_rdonly and Open_wronly are not compatible");
      if(f_d_.text&&f_d_.binary)
       caml_raise_sys_error_z_
        (name2_g_+" : flags Open_text and Open_binary are not compatible");
      if(caml_sys_file_exists_hC_(name_a_))
       {if(caml_sys_is_directory_hD_(name_a_))
         caml_raise_sys_error_z_(name2_g_+" : is a directory");
        if(f_d_.create&&f_d_.excl)
         caml_raise_sys_error_z_(name2_g_+str_file_already_abr_b4_);
        var
         idx_h_=
          caml_global_data_e_.fd_last_idx?caml_global_data_e_.fd_last_idx:0,
         file_f_=caml_fs_content_b7_(path_i_);
        if(f_d_.truncate)file_f_.truncate();
        return caml_sys_open_internal_aI_(idx_h_+1,file_f_.content(),f_d_)}
      else
       if(f_d_.create)
        {var
          idx_h_=
           caml_global_data_e_.fd_last_idx?caml_global_data_e_.fd_last_idx:0;
         caml_fs_register_g2_(name_a_,[]);
         var file_f_=caml_fs_content_b7_(path_i_);
         return caml_sys_open_internal_aI_(idx_h_+1,file_f_.content(),f_d_)}
       else
        caml_raise_no_such_file_dG_(name_a_)}
    caml_sys_open_internal_aI_(0,[]);
    caml_sys_open_internal_aI_(1,[]);
    caml_sys_open_internal_aI_(2,[]);
    function caml_ml_open_descriptor_in_hl_(fd_a_)
     {var data_b_=caml_global_data_e_.fds[fd_a_];
      if(data_b_.flags.wronly)
       caml_raise_sys_error_z_(str_fd_dr_+fd_a_+" is writeonly");
      return {data:data_b_,fd:fd_a_,opened:true}}
    function js_print_stdout_hI_(s_a_)
     {if(s_a_.charCodeAt(s_a_.length-1)==10)s_a_=s_a_.substr(0,s_a_.length-1);
      var v_c_=joo_global_object_b_.console;
      v_c_&&v_c_.log&&v_c_.log(s_a_)}
    var caml_ml_out_channels_bf_=new Array();
    function caml_std_output_hx_(chan_a_,s_b_)
     {var str_e_=new MlString_o_(s_b_),slen_d_=str_e_.getLen();
      for(var i_c_=0;i_c_<slen_d_;i_c_++)
       chan_a_.data.array[chan_a_.data.offset+i_c_]=str_e_.get(i_c_);
      chan_a_.data.offset+=slen_d_;
      return 0}
    function caml_ml_open_descriptor_out_hm_(fd_a_)
     {var output_b_;
      switch(fd_a_)
       {case 1:output_b_=js_print_stdout_hI_;break;
        case 2:output_b_=js_print_stderr_b__;break;
        default:output_b_=caml_std_output_hx_}
      var data_f_=caml_global_data_e_.fds[fd_a_];
      if(data_f_.flags.rdonly)
       caml_raise_sys_error_z_(str_fd_dr_+fd_a_+" is readonly");
      var
       channel_c_=
        {data:data_f_,fd:fd_a_,opened:true,buffer:str_d_,output:output_b_};
      caml_ml_out_channels_bf_[channel_c_.fd]=channel_c_;
      return channel_c_}
    function caml_ml_out_channels_list_hn_()
     {var l_a_=0;
      for(var c_b_ in caml_ml_out_channels_bf_)
       if(caml_ml_out_channels_bf_[c_b_].opened)
        l_a_=[0,caml_ml_out_channels_bf_[c_b_],l_a_];
      return l_a_}
    function caml_ml_output_dC_(oc_a_,buffer_b_,offset_c_,len_d_)
     {if(!oc_a_.opened)
       caml_raise_sys_error_z_("Cannot output to a closed channel");
      var string_f_;
      if(offset_c_==0&&buffer_b_.getLen()==len_d_)
       string_f_=buffer_b_;
      else
       {string_f_=caml_create_string_dy_(len_d_);
        caml_blit_string_dw_(buffer_b_,offset_c_,string_f_,0,len_d_)}
      var
       jsstring_e_=string_f_.toString(),
       id_g_=jsstring_e_.lastIndexOf("\n");
      if(id_g_<0)
       oc_a_.buffer+=jsstring_e_;
      else
       {oc_a_.buffer+=jsstring_e_.substr(0,id_g_+1);
        caml_ml_flush_dB_(oc_a_);
        oc_a_.buffer+=jsstring_e_.substr(id_g_+1)}}
    function caml_new_string_dE_(x_a_){return new MlString_o_(x_a_)}
    function caml_ml_output_char_ho_(oc_a_,c_b_)
     {var s_c_=caml_new_string_dE_(String.fromCharCode(c_b_));
      caml_ml_output_dC_(oc_a_,s_c_,0,1)}
    if(!Math.imul)
     Math.imul=
     function(x_a_,y_b_)
      {return ((x_a_>>16)*y_b_<<16)+(x_a_&num_65535_a__)*y_b_|0};
    var caml_mul_hp_=Math.imul;
    function caml_notequal_hr_(x_a_,y_b_)
     {return +(caml_compare_val_dx_(x_a_,y_b_,false)!=0)}
    function caml_obj_is_block_hs_(x_a_){return +(x_a_ instanceof Array)}
    function caml_obj_tag_ht_(x_a_)
     {return x_a_ instanceof Array?x_a_[0]:num_1e3_a$_}
    function caml_register_global_hv_(n_a_,v_b_)
     {caml_global_data_e_[n_a_+1]=v_b_}
    var caml_named_values_dD_={};
    function caml_register_named_value_hw_(nm_a_,v_b_)
     {caml_named_values_dD_[nm_a_.toString()]=v_b_;return 0}
    function caml_string_equal_hy_(s1_a_,s2_b_)
     {var b1_c_=s1_a_.fullBytes,b2_d_=s2_b_.fullBytes;
      if(b1_c_!=null&&b2_d_!=null)return b1_c_==b2_d_?1:0;
      return s1_a_.getFullBytes()==s2_b_.getFullBytes()?1:0}
    function caml_string_notequal_hz_(s1_a_,s2_b_)
     {return 1-caml_string_equal_hy_(s1_a_,s2_b_)}
    function caml_sys_const_word_size_hA_(){return 32}
    function caml_sys_exit_hB_()
     {caml_invalid_argument_al_("Function 'exit' not implemented")}
    function caml_trampoline_hE_(res_a_)
     {var c_b_=1;
      while(res_a_&&res_a_.joo_tramp)
       {res_a_=res_a_.joo_tramp.apply(null,res_a_.joo_args);c_b_++}
      return res_a_}
    function caml_trampoline_return_hF_(f_a_,args_b_)
     {return {joo_tramp:f_a_,joo_args:args_b_}}
    function caml_update_dummy_hG_(x_a_,y_b_)
     {if(typeof y_b_==="function"){x_a_.fun=y_b_;return 0}
      if(y_b_.fun){x_a_.fun=y_b_.fun;return 0}
      var i_c_=y_b_.length;
      while(i_c_--)x_a_[i_c_]=y_b_[i_c_];
      return 0}
    function caml_named_value_hq_(nm_a_){return caml_named_values_dD_[nm_a_]}
    function caml_wrap_exception_hH_(e_a_)
     {if(e_a_ instanceof Array)return e_a_;
      if
       (joo_global_object_b_.RangeError&&
        e_a_ instanceof joo_global_object_b_.RangeError&&
        e_a_.message&&
        e_a_.message.match(/maximum call stack/i))
       return [0,caml_global_data_e_[9]];
      if
       (joo_global_object_b_.InternalError&&
        e_a_ instanceof joo_global_object_b_.InternalError&&
        e_a_.message&&
        e_a_.message.match(/too much recursion/i))
       return [0,caml_global_data_e_[9]];
      if(e_a_ instanceof joo_global_object_b_.Error)
       return [0,caml_named_value_hq_(str_jsError_dg_),e_a_];
      return [0,caml_global_data_e_[3],new MlWrappedString_p_(String(e_a_))]}
    var
     caml_array_get_k_=caml_array_get_gS_,
     caml_array_set_j_=caml_array_set_gT_,
     caml_blit_string___=caml_blit_string_dw_,
     caml_create_string_J_=caml_create_string_dy_,
     caml_float_of_string_ai_=caml_float_of_string_gZ_,
     caml_format_float_bO_=caml_format_float_g0_,
     caml_format_int_aX_=caml_format_int_g1_,
     caml_int_of_string_aY_=caml_int_of_string_hd_,
     caml_is_printable_bP_=caml_is_printable_he_,
     caml_js_from_byte_string_f_=caml_js_from_byte_string_hf_,
     caml_js_to_byte_string_N_=caml_js_to_byte_string_hi_,
     caml_js_wrap_callback_a1_=caml_js_wrap_callback_hj_,
     caml_make_vect_K_=caml_make_vect_hk_,
     caml_ml_flush_c5_=caml_ml_flush_dB_,
     caml_ml_open_descriptor_out_c4_=caml_ml_open_descriptor_out_hm_,
     caml_ml_output_char_c7_=caml_ml_output_char_ho_,
     caml_mul_c8_=caml_mul_hp_,
     caml_new_string_c_=caml_new_string_dE_,
     caml_notequal_c__=caml_notequal_hr_,
     caml_obj_tag_c9_=caml_obj_tag_ht_,
     caml_register_global_a_=caml_register_global_hv_,
     caml_register_named_value_c6_=caml_register_named_value_hw_,
     caml_string_notequal_s_=caml_string_notequal_hz_,
     caml_trampoline_a0_=caml_trampoline_hE_,
     caml_trampoline_return_M_=caml_trampoline_return_hF_,
     caml_update_dummy_c$_=caml_update_dummy_hG_,
     caml_wrap_exception_C_=caml_wrap_exception_hH_;
    function caml_call_gen1_i_(fun_a_,var0_b_)
     {return fun_a_.length==1
              ?fun_a_(var0_b_)
              :caml_call_gen_T_(fun_a_,[var0_b_])}
    function caml_call_gen2_l_(fun_a_,var0_b_,var1_c_)
     {return fun_a_.length==2
              ?fun_a_(var0_b_,var1_c_)
              :caml_call_gen_T_(fun_a_,[var0_b_,var1_c_])}
    function caml_call_gen3_r_(fun_a_,var0_b_,var1_c_,var2_d_)
     {return fun_a_.length==3
              ?fun_a_(var0_b_,var1_c_,var2_d_)
              :caml_call_gen_T_(fun_a_,[var0_b_,var1_c_,var2_d_])}
    function caml_call_gen5_aZ_
     (fun_a_,var0_b_,var1_c_,var2_d_,var3_e_,var4_f_)
     {return fun_a_.length==5
              ?fun_a_(var0_b_,var1_c_,var2_d_,var3_e_,var4_f_)
              :caml_call_gen_T_
                (fun_a_,[var0_b_,var1_c_,var2_d_,var3_e_,var4_f_])}
    var
     _aJ_=[0,caml_new_string_c_("Failure")],
     _b$_=[0,caml_new_string_c_("Invalid_argument")],
     _bj_=[0,caml_new_string_c_("Not_found")],
     _cw_=[0,caml_new_string_c_("Match_failure")],
     _cv_=[0,caml_new_string_c_("Stack_overflow")],
     _t_=[0,caml_new_string_c_("Assert_failure")],
     _cx_=[0,caml_new_string_c_("Undefined_recursive_module")],
     _bq_=caml_new_string_c_('File "%s", line %d, characters %d-%d: %s'),
     s_c3_=caml_new_string_c_("monkey.model");
    caml_register_global_a_(11,_cx_);
    caml_register_global_a_(8,_cv_);
    caml_register_global_a_(7,_cw_);
    caml_register_global_a_(6,_bj_);
    caml_register_global_a_(5,[0,caml_new_string_c_("Division_by_zero")]);
    caml_register_global_a_(4,[0,caml_new_string_c_("End_of_file")]);
    caml_register_global_a_(3,_b$_);
    caml_register_global_a_(2,_aJ_);
    caml_register_global_a_(1,[0,caml_new_string_c_("Sys_error")]);
    var
     _ev_=[0,caml_new_string_c_("Out_of_memory")],
     _dM_=caml_new_string_c_(str_12g_di_),
     _dL_=caml_new_string_c_(str_ak_),
     _dJ_=caml_new_string_c_("true"),
     _dK_=caml_new_string_c_("false"),
     _dN_=caml_new_string_c_("Pervasives.do_at_exit"),
     _dR_=caml_new_string_c_("\\b"),
     _dS_=caml_new_string_c_("\\t"),
     _dT_=caml_new_string_c_("\\n"),
     _dU_=caml_new_string_c_("\\r"),
     _dQ_=caml_new_string_c_("\\\\"),
     _dP_=caml_new_string_c_("\\'"),
     _dX_=caml_new_string_c_(str_d_),
     _dW_=caml_new_string_c_("String.blit"),
     _dV_=caml_new_string_c_("String.sub"),
     _dY_=caml_new_string_c_("Queue.Empty"),
     _d0_=caml_new_string_c_("Buffer.add: cannot grow buffer"),
     _ee_=caml_new_string_c_(str_d_),
     _ef_=caml_new_string_c_(str_d_),
     _ei_=caml_new_string_c_(str_12g_di_),
     _ej_=caml_new_string_c_(str_dh_),
     _ek_=caml_new_string_c_(str_dh_),
     _eg_=caml_new_string_c_(str_a6_),
     _eh_=caml_new_string_c_(str_a6_),
     _ed_=caml_new_string_c_(str_nan_dj_),
     _eb_=caml_new_string_c_("neg_infinity"),
     _ec_=caml_new_string_c_("infinity"),
     _ea_=caml_new_string_c_(str_ak_),
     _d$_=caml_new_string_c_("printf: bad positional specification (0)."),
     _d__=caml_new_string_c_("%_"),
     _d9_=[0,caml_new_string_c_("printf.ml"),143,8],
     _d7_=caml_new_string_c_(str_a6_),
     _d8_=caml_new_string_c_("Printf: premature end of format string '"),
     _d3_=caml_new_string_c_(str_a6_),
     _d4_=caml_new_string_c_(" in format string '"),
     _d5_=caml_new_string_c_(", at char number "),
     _d6_=caml_new_string_c_("Printf: bad conversion %"),
     _d1_=caml_new_string_c_("Sformat.index_of_int: negative argument "),
     _ep_=caml_new_string_c_(str_d_),
     _eq_=caml_new_string_c_(", %s%s"),
     _eH_=[1,1],
     _eI_=caml_new_string_c_("%s\n"),
     _eJ_=
      caml_new_string_c_
       ("(Program not linked with -g, cannot print stack backtrace)\n"),
     _eB_=caml_new_string_c_("Raised at"),
     _eE_=caml_new_string_c_("Re-raised at"),
     _eF_=caml_new_string_c_("Raised by primitive operation at"),
     _eG_=caml_new_string_c_("Called from"),
     _eC_=caml_new_string_c_('%s file "%s", line %d, characters %d-%d'),
     _eD_=caml_new_string_c_("%s unknown location"),
     _ew_=caml_new_string_c_("Out of memory"),
     _ex_=caml_new_string_c_("Stack overflow"),
     _ey_=caml_new_string_c_("Pattern matching failed"),
     _ez_=caml_new_string_c_("Assertion failed"),
     _eA_=caml_new_string_c_("Undefined recursive module"),
     _er_=caml_new_string_c_("(%s%s)"),
     _es_=caml_new_string_c_(str_d_),
     _et_=caml_new_string_c_(str_d_),
     _eu_=caml_new_string_c_("(%s)"),
     _eo_=caml_new_string_c_(str_d_df_),
     _em_=caml_new_string_c_("%S"),
     _en_=caml_new_string_c_("_"),
     _eX_=[0,caml_new_string_c_(str_src_core_lwt_ml_R_),692,20],
     _eY_=[0,caml_new_string_c_(str_src_core_lwt_ml_R_),695,8],
     _eV_=[0,caml_new_string_c_(str_src_core_lwt_ml_R_),670,20],
     _eW_=[0,caml_new_string_c_(str_src_core_lwt_ml_R_),673,8],
     _eT_=[0,caml_new_string_c_(str_src_core_lwt_ml_R_),648,20],
     _eU_=[0,caml_new_string_c_(str_src_core_lwt_ml_R_),651,8],
     _eQ_=[0,caml_new_string_c_(str_src_core_lwt_ml_R_),498,8],
     _eP_=[0,caml_new_string_c_(str_src_core_lwt_ml_R_),487,9],
     _eO_=caml_new_string_c_("Lwt.wakeup_result"),
     _eL_=caml_new_string_c_("Fatal error: exception "),
     _eK_=caml_new_string_c_("Lwt.Canceled"),
     state_return_unit_eR_=[0,0],
     _e3_=caml_new_string_c_("Js.Error"),
     name_e4_=caml_new_string_c_(str_jsError_dg_),
     _e__=caml_new_string_c_("script"),
     _e8_=caml_new_string_c_(str_canvas_bW_),
     _fc_=caml_new_string_c_("browser can't read file: unimplemented"),
     _fb_=[0,caml_new_string_c_("file.ml"),131,15],
     _e$_=caml_new_string_c_("can't retrieve file name: not implemented"),
     _fe_=caml_new_string_c_("Exception during Lwt.async: "),
     _fg_=caml_new_string_c_("[\\][()\\\\|+*.?{}^$]"),
     _ft_=[0,caml_new_string_c_(str_d_),0],
     _fu_=caml_new_string_c_(str_d_),
     _fH_=caml_new_string_c_(str_d_),
     _fI_=caml_new_string_c_(str_a7_),
     _fQ_=caml_new_string_c_(str_d_),
     _fJ_=caml_new_string_c_(str_a2_),
     _fP_=caml_new_string_c_(str_d_),
     _fK_=caml_new_string_c_(str_Q_),
     _fL_=caml_new_string_c_(str_Q_),
     _fO_=caml_new_string_c_(str_da_),
     _fM_=caml_new_string_c_(str_d_),
     _fN_=caml_new_string_c_("http://"),
     _fR_=caml_new_string_c_(str_d_),
     _fS_=caml_new_string_c_(str_a7_),
     _f0_=caml_new_string_c_(str_d_),
     _fT_=caml_new_string_c_(str_a2_),
     _fZ_=caml_new_string_c_(str_d_),
     _fU_=caml_new_string_c_(str_Q_),
     _fV_=caml_new_string_c_(str_Q_),
     _fY_=caml_new_string_c_(str_da_),
     _fW_=caml_new_string_c_(str_d_),
     _fX_=caml_new_string_c_("https://"),
     _f1_=caml_new_string_c_(str_d_),
     _f2_=caml_new_string_c_(str_a7_),
     _f7_=caml_new_string_c_(str_d_),
     _f3_=caml_new_string_c_(str_a2_),
     _f6_=caml_new_string_c_(str_d_),
     _f4_=caml_new_string_c_(str_Q_),
     _f5_=caml_new_string_c_("file://"),
     _fG_=caml_new_string_c_(str_d_),
     _fF_=caml_new_string_c_(str_d_),
     _fE_=caml_new_string_c_(str_d_),
     _fD_=caml_new_string_c_(str_d_),
     _fC_=caml_new_string_c_(str_d_),
     _fB_=caml_new_string_c_(str_d_),
     _fv_=caml_new_string_c_(str_bZ_),
     _fw_=caml_new_string_c_(str_ds_),
     _fn_=caml_new_string_c_("file"),
     _fo_=caml_new_string_c_("file:"),
     _fp_=caml_new_string_c_("http"),
     _fq_=caml_new_string_c_("http:"),
     _fr_=caml_new_string_c_("https"),
     _fs_=caml_new_string_c_("https:"),
     s_by_fk_=caml_new_string_c_("%2B"),
     _fi_=caml_new_string_c_("Url.Local_exn"),
     s_fj_=caml_new_string_c_(str_aB_),
     _fl_=caml_new_string_c_("Url.Not_an_http_protocol"),
     _fx_=
      caml_new_string_c_
       ("^([Hh][Tt][Tt][Pp][Ss]?)://([0-9a-zA-Z.-]+|\\[[0-9a-zA-Z.-]+\\]|\\[[0-9A-Fa-f:.]+\\])?(:([0-9]+))?/([^\\?#]*)(\\?([^#]*))?(#(.*))?$"),
     _fz_=
      caml_new_string_c_
       ("^([Ff][Ii][Ll][Ee])://([^\\?#]*)(\\?([^#]*))?(#(.*))?$"),
     _gm_=caml_new_string_c_(str_POST_bd_),
     _go_=caml_new_string_c_("multipart/form-data; boundary="),
     _gp_=caml_new_string_c_(str_POST_bd_),
     _gq_=
      [0,
       caml_new_string_c_(str_POST_bd_),
       [0,caml_new_string_c_("application/x-www-form-urlencoded")],
       num_126925477_bc_],
     _gr_=[0,caml_new_string_c_(str_POST_bd_),0,num_126925477_bc_],
     _gs_=caml_new_string_c_("GET"),
     _gn_=caml_new_string_c_(str_a2_),
     _gh_=caml_new_string_c_(str_bZ_),
     _gi_=caml_new_string_c_(str_bZ_),
     _gj_=caml_new_string_c_(str_ds_),
     _gd_=caml_new_string_c_('"; filename="'),
     _ge_=caml_new_string_c_(str_Content_Disposit_abr_dm_),
     _gb_=caml_new_string_c_(str_a9_),
     _gc_=caml_new_string_c_(str_dc_),
     _gf_=caml_new_string_c_('"\r\n\r\n'),
     _gg_=caml_new_string_c_(str_Content_Disposit_abr_dm_),
     _f$_=caml_new_string_c_("--\r\n"),
     _ga_=caml_new_string_c_(str_dc_),
     _f__=caml_new_string_c_("js_of_ocaml-------------------"),
     _f9_=[0,caml_new_string_c_("xmlHttpRequest.ml"),85,2],
     _gk_=caml_new_string_c_("XmlHttpRequest.Wrong_headers"),
     _gR_=caml_new_string_c_("uncaught exception: %s"),
     _gQ_=caml_new_string_c_("%.1f"),
     canvas_id_gL_=caml_new_string_c_(str_canvas_bW_),
     _gM_=caml_new_string_c_("fragment-shader"),
     _gN_=caml_new_string_c_("vertex-shader"),
     _gO_=caml_new_string_c_("program loaded"),
     _gP_=caml_new_string_c_("ready"),
     _gF_=[0,1,[0,2,[0,3,[0,4,0]]]],
     _gG_=caml_new_string_c_(str_f_a3_),
     _gH_=caml_new_string_c_("v"),
     _gI_=caml_new_string_c_("vn"),
     _gE_=[0,1,[0,2,0]],
     _gy_=caml_new_string_c_("can't find script element %s"),
     _gx_=caml_new_string_c_("Unable to link the shader program."),
     _gw_=
      caml_new_string_c_("An error occurred compiling the shaders: \n%s\n%s"),
     f_gv_=caml_new_string_c_("can't initialise webgl context"),
     _gu_=caml_new_string_c_("can't find canvas element %s"),
     _gt_=caml_new_string_c_("WebGL error"),
     _gz_=caml_new_string_c_("(v|vn|f)\\ ([^\\ ]+)\\ ([^\\ ]+)\\ ([^\\ ]+)"),
     _gB_=caml_new_string_c_("([0-9]+)//([0-9]+)");
    function failwith_U_(s_a_){throw [0,_aJ_,s_a_]}
    function invalid_arg_am_(s_a_){throw [0,_b$_,s_a_]}
    function _h_(s1_a_,s2_b_)
     {var
       l1_c_=s1_a_.getLen(),
       l2_e_=s2_b_.getLen(),
       s_d_=caml_create_string_J_(l1_c_+l2_e_|0);
      caml_blit_string___(s1_a_,0,s_d_,0,l1_c_);
      caml_blit_string___(s2_b_,0,s_d_,l1_c_,l2_e_);
      return s_d_}
    function string_of_int_ae_(n_a_){return caml_new_string_c_(str_d_+n_a_)}
    function _ca_(l1_a_,l2_b_)
     {if(l1_a_){var hd_c_=l1_a_[1];return [0,hd_c_,_ca_(l1_a_[2],l2_b_)]}
      return l2_b_}
    caml_ml_open_descriptor_in_hl_(0);
    caml_ml_open_descriptor_out_c4_(1);
    var stderr_an_=caml_ml_open_descriptor_out_c4_(2);
    function output_string_cb_(oc_a_,s_b_)
     {return caml_ml_output_dC_(oc_a_,s_b_,0,s_b_.getLen())}
    function prerr_string_cc_(s_a_){return output_string_cb_(stderr_an_,s_a_)}
    function do_at_exit_bh_(param_a_)
     {var param_b_=caml_ml_out_channels_list_hn_(0);
      for(;;)
       {if(param_b_)
         {var l_c_=param_b_[2],a_d_=param_b_[1];
          try {caml_ml_flush_c5_(a_d_)}catch(_f_){}
          var param_b_=l_c_;
          continue}
        return 0}}
    caml_register_named_value_c6_(_dN_,do_at_exit_bh_);
    function _dO_(_a_,_b_){return caml_ml_output_char_c7_(_a_,_b_)}
    function _cd_(_a_){return caml_ml_flush_c5_(_a_)}
    function _aK_(l_a_,f_b_)
     {if(0===l_a_)return [0];
      var
       res_d_=caml_make_vect_K_(l_a_,caml_call_gen1_i_(f_b_,0)),
       _e_=l_a_-1|0,
       _f_=1;
      if(!(_e_<1))
       {var i_c_=_f_;
        for(;;)
         {res_d_[i_c_+1]=caml_call_gen1_i_(f_b_,i_c_);
          var _g_=i_c_+1|0;
          if(_e_!==i_c_){var i_c_=_g_;continue}
          break}}
      return res_d_}
    function _bi_(l_a_)
     {if(l_a_)
       {var accu_d_=0,param_c_=l_a_,tl_g_=l_a_[2],hd_h_=l_a_[1];
        for(;;)
         {if(param_c_){var accu_d_=accu_d_+1|0,param_c_=param_c_[2];continue}
          var a_f_=caml_make_vect_K_(accu_d_,hd_h_),i_e_=1,param_b_=tl_g_;
          for(;;)
           {if(param_b_)
             {var tl_i_=param_b_[2];
              a_f_[i_e_+1]=param_b_[1];
              var i_e_=i_e_+1|0,param_b_=tl_i_;
              continue}
            return a_f_}}}
      return [0]}
    function _af_(l_a_)
     {var l1_b_=l_a_,l2_c_=0;
      for(;;)
       {if(l1_b_)
         {var _d_=[0,l1_b_[1],l2_c_],l1_b_=l1_b_[2],l2_c_=_d_;continue}
        return l2_c_}}
    function _F_(f_a_,param_b_)
     {if(param_b_)
       {var l_c_=param_b_[2],r_d_=caml_call_gen1_i_(f_a_,param_b_[1]);
        return [0,r_d_,_F_(f_a_,l_c_)]}
      return 0}
    function _ao_(f_a_,param_b_)
     {var param_c_=param_b_;
      for(;;)
       {if(param_c_)
         {var l_d_=param_c_[2];
          caml_call_gen1_i_(f_a_,param_c_[1]);
          var param_c_=l_d_;
          continue}
        return 0}}
    function _ap_(n_a_,c_b_)
     {var s_c_=caml_create_string_J_(n_a_);
      caml_fill_string_gY_(s_c_,0,n_a_,c_b_);
      return s_c_}
    function _V_(s_a_,ofs_b_,len_c_)
     {if(0<=ofs_b_)
       if(0<=len_c_)
        if(!((s_a_.getLen()-len_c_|0)<ofs_b_))
         {var r_d_=caml_create_string_J_(len_c_);
          caml_blit_string___(s_a_,ofs_b_,r_d_,0,len_c_);
          return r_d_}
      return invalid_arg_am_(_dV_)}
    function _aL_(s1_a_,ofs1_b_,s2_c_,ofs2_d_,len_e_)
     {if(0<=len_e_)
       if(0<=ofs1_b_)
        if(!((s1_a_.getLen()-len_e_|0)<ofs1_b_))
         if(0<=ofs2_d_)
          if(!((s2_c_.getLen()-len_e_|0)<ofs2_d_))
           return caml_blit_string___(s1_a_,ofs1_b_,s2_c_,ofs2_d_,len_e_);
      return invalid_arg_am_(_dW_)}
    function _aq_(sep_d_,l_b_)
     {if(l_b_)
       {var hd_a_=l_b_[1],num_g_=[0,0],len_f_=[0,0],tl_h_=l_b_[2];
        _ao_
         (function(s_a_)
           {num_g_[1]++;len_f_[1]=len_f_[1]+s_a_.getLen()|0;return 0},
          l_b_);
        var
         r_e_=
          caml_create_string_J_
           (len_f_[1]+caml_mul_c8_(sep_d_.getLen(),num_g_[1]-1|0)|0);
        caml_blit_string___(hd_a_,0,r_e_,0,hd_a_.getLen());
        var pos_c_=[0,hd_a_.getLen()];
        _ao_
         (function(s_a_)
           {caml_blit_string___(sep_d_,0,r_e_,pos_c_[1],sep_d_.getLen());
            pos_c_[1]=pos_c_[1]+sep_d_.getLen()|0;
            caml_blit_string___(s_a_,0,r_e_,pos_c_[1],s_a_.getLen());
            pos_c_[1]=pos_c_[1]+s_a_.getLen()|0;
            return 0},
          tl_h_);
        return r_e_}
      return _dX_}
    var
     _bk_=caml_sys_const_word_size_hA_(0),
     _ar_=caml_mul_c8_(_bk_/8|0,(1<<(_bk_-10|0))-1|0)-1|0,
     _dZ_=[0,_dY_];
    function _ce_(param_a_){return [0,0,0]}
    function _cf_(q_a_)
     {if(0===q_a_[1])throw [0,_dZ_];
      q_a_[1]=q_a_[1]-1|0;
      var tail_b_=q_a_[2],head_c_=tail_b_[2];
      if(head_c_===tail_b_)q_a_[2]=0;else tail_b_[2]=head_c_[2];
      return head_c_[1]}
    function _cg_(q_a_){return q_a_[1]}
    function _bl_(n_a_)
     {var
       n_b_=1<=n_a_?n_a_:1,
       n_c_=_ar_<n_b_?_ar_:n_b_,
       s_d_=caml_create_string_J_(n_c_);
      return [0,s_d_,0,n_c_,s_d_]}
    function _bm_(b_a_){return _V_(b_a_[1],0,b_a_[2])}
    function _ch_(b_a_,more_b_)
     {var new_len_c_=[0,b_a_[3]];
      for(;;)
       {if(new_len_c_[1]<(b_a_[2]+more_b_|0))
         {new_len_c_[1]=2*new_len_c_[1]|0;continue}
        if(_ar_<new_len_c_[1])
         if((b_a_[2]+more_b_|0)<=_ar_)
          new_len_c_[1]=_ar_;
         else
          failwith_U_(_d0_);
        var new_buffer_d_=caml_create_string_J_(new_len_c_[1]);
        _aL_(b_a_[1],0,new_buffer_d_,0,b_a_[2]);
        b_a_[1]=new_buffer_d_;
        b_a_[3]=new_len_c_[1];
        return 0}}
    function _as_(b_a_,c_b_)
     {var pos_c_=b_a_[2];
      if(b_a_[3]<=pos_c_)_ch_(b_a_,1);
      b_a_[1].safeSet(pos_c_,c_b_);
      b_a_[2]=pos_c_+1|0;
      return 0}
    function _bn_(b_a_,s_b_)
     {var len_c_=s_b_.getLen(),new_position_d_=b_a_[2]+len_c_|0;
      if(b_a_[3]<new_position_d_)_ch_(b_a_,len_c_);
      _aL_(s_b_,0,b_a_[1],b_a_[2],len_c_);
      b_a_[2]=new_position_d_;
      return 0}
    function index_of_int_bo_(i_a_)
     {return 0<=i_a_?i_a_:failwith_U_(_h_(_d1_,string_of_int_ae_(i_a_)))}
    function add_int_index_ci_(i_a_,idx_b_)
     {return index_of_int_bo_(i_a_+idx_b_|0)}
    var _d2_=1;
    function _cj_(_a_){return add_int_index_ci_(_d2_,_a_)}
    function _ck_(fmt_a_){return _V_(fmt_a_,0,fmt_a_.getLen())}
    function bad_conversion_cl_(sfmt_a_,i_b_,c_c_)
     {var
       _d_=_h_(_d4_,_h_(sfmt_a_,_d3_)),
       _e_=_h_(_d5_,_h_(string_of_int_ae_(i_b_),_d_));
      return invalid_arg_am_(_h_(_d6_,_h_(_ap_(1,c_c_),_e_)))}
    function bad_conversion_format_at_(fmt_a_,i_b_,c_c_)
     {return bad_conversion_cl_(_ck_(fmt_a_),i_b_,c_c_)}
    function incomplete_format_aM_(fmt_a_)
     {return invalid_arg_am_(_h_(_d8_,_h_(_ck_(fmt_a_),_d7_)))}
    function extract_format_W_(fmt_e_,start_b_,stop_c_,widths_d_)
     {function skip_positional_spec_h_(start_a_)
       {if
         ((fmt_e_.safeGet(start_a_)+num_48_aj_|0)<
          0||
          9<
          (fmt_e_.safeGet(start_a_)+num_48_aj_|0))
         return start_a_;
        var i_b_=start_a_+1|0;
        for(;;)
         {var match_c_=fmt_e_.safeGet(i_b_);
          if(48<=match_c_)
           {if(!(58<=match_c_)){var i_b_=i_b_+1|0;continue}}
          else
           if(36===match_c_)return i_b_+1|0;
          return start_a_}}
      var
       i_i_=skip_positional_spec_h_(start_b_+1|0),
       b_f_=_bl_((stop_c_-i_i_|0)+10|0);
      _as_(b_f_,37);
      var i_a_=i_i_,widths_g_=_af_(widths_d_);
      for(;;)
       {if(i_a_<=stop_c_)
         {var c_j_=fmt_e_.safeGet(i_a_);
          if(42===c_j_)
           {if(widths_g_)
             {var t_k_=widths_g_[2];
              _bn_(b_f_,string_of_int_ae_(widths_g_[1]));
              var i_a_=skip_positional_spec_h_(i_a_+1|0),widths_g_=t_k_;
              continue}
            throw [0,_t_,_d9_]}
          _as_(b_f_,c_j_);
          var i_a_=i_a_+1|0;
          continue}
        return _bm_(b_f_)}}
    function extract_format_int_cm_(conv_a_,fmt_b_,start_c_,stop_d_,widths_e_)
     {var sfmt_f_=extract_format_W_(fmt_b_,start_c_,stop_d_,widths_e_);
      if(78!==conv_a_)if(num_110_aD_!==conv_a_)return sfmt_f_;
      sfmt_f_.safeSet(sfmt_f_.getLen()-1|0,num_117_bX_);
      return sfmt_f_}
    function sub_format_for_printf_cn_(conv_a_)
     {return function(fmt_d_,i_b_)
       {var len_k_=fmt_d_.getLen();
        function sub_fmt_l_(c_a_,j_b_)
         {var close_m_=40===c_a_?41:num_125_bU_,j_c_=j_b_;
          for(;;)
           {if(len_k_<=j_c_)return incomplete_format_aM_(fmt_d_);
            if(37===fmt_d_.safeGet(j_c_))
             {var j_e_=j_c_+1|0;
              if(len_k_<=j_e_)return incomplete_format_aM_(fmt_d_);
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
                       :bad_conversion_format_at_(fmt_d_,j_b_,c_f_);
              var j_c_=sub_fmt_l_(c_f_,j_e_+1|0)+1|0;
              continue}
            var j_c_=j_c_+1|0;
            continue}}
        return sub_fmt_l_(conv_a_,i_b_)}}
    function iter_on_format_args_co_(fmt_i_,add_conv_b_,add_char_c_)
     {var lim_m_=fmt_i_.getLen()-1|0;
      function scan_fmt_s_(i_a_)
       {var i_k_=i_a_;
        a:
        for(;;)
         {if(i_k_<lim_m_)
           {if(37===fmt_i_.safeGet(i_k_))
             {var skip_f_=0,i_h_=i_k_+1|0;
              for(;;)
               {if(lim_m_<i_h_)
                 var _e_=incomplete_format_aM_(fmt_i_);
                else
                 {var match_n_=fmt_i_.safeGet(i_h_);
                  if(58<=match_n_)
                   {if(95===match_n_){var skip_f_=1,i_h_=i_h_+1|0;continue}}
                  else
                   if(32<=match_n_)
                    switch(match_n_+num_32_dl_|0)
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
                         caml_call_gen3_r_(add_conv_b_,skip_f_,i_h_,num_105_ac_);
                       continue;
                      default:var i_h_=i_h_+1|0;continue}
                  var i_d_=i_h_;
                  b:
                  for(;;)
                   {if(lim_m_<i_d_)
                     var _e_=incomplete_format_aM_(fmt_i_);
                    else
                     {var conv_j_=fmt_i_.safeGet(i_d_);
                      if(126<=conv_j_)
                       var _g_=0;
                      else
                       switch(conv_j_)
                        {case 78:
                         case 88:
                         case num_100_ba_:
                         case num_105_ac_:
                         case num_111_b1_:
                         case num_117_bX_:
                         case num_120_bY_:
                          var
                           _e_=caml_call_gen3_r_(add_conv_b_,skip_f_,i_d_,num_105_ac_),
                           _g_=1;
                          break;
                         case 69:
                         case 70:
                         case 71:
                         case num_101_dp_:
                         case num_102_b3_:
                         case num_103_b2_:
                          var
                           _e_=caml_call_gen3_r_(add_conv_b_,skip_f_,i_d_,num_102_b3_),
                           _g_=1;
                          break;
                         case 33:
                         case 37:
                         case 44:
                         case 64:var _e_=i_d_+1|0,_g_=1;break;
                         case 83:
                         case 91:
                         case num_115_aE_:
                          var
                           _e_=caml_call_gen3_r_(add_conv_b_,skip_f_,i_d_,num_115_aE_),
                           _g_=1;
                          break;
                         case 97:
                         case num_114_a8_:
                         case num_116_bS_:
                          var
                           _e_=caml_call_gen3_r_(add_conv_b_,skip_f_,i_d_,conv_j_),
                           _g_=1;
                          break;
                         case 76:
                         case num_108_dt_:
                         case num_110_aD_:
                          var j_t_=i_d_+1|0;
                          if(lim_m_<j_t_)
                           var
                            _e_=caml_call_gen3_r_(add_conv_b_,skip_f_,i_d_,num_105_ac_),
                            _g_=1;
                          else
                           {var _p_=fmt_i_.safeGet(j_t_)+num_88_dq_|0;
                            if(_p_<0||32<_p_)
                             var _q_=1;
                            else
                             switch(_p_)
                              {case 0:
                               case 12:
                               case 17:
                               case 23:
                               case 29:
                               case 32:
                                var
                                 _e_=
                                  caml_call_gen2_l_
                                   (add_char_c_,
                                    caml_call_gen3_r_(add_conv_b_,skip_f_,i_d_,conv_j_),
                                    num_105_ac_),
                                 _g_=1,
                                 _q_=0;
                                break;
                               default:var _q_=1}
                            if(_q_)
                             var
                              _e_=caml_call_gen3_r_(add_conv_b_,skip_f_,i_d_,num_105_ac_),
                              _g_=1}
                          break;
                         case 67:
                         case 99:
                          var
                           _e_=caml_call_gen3_r_(add_conv_b_,skip_f_,i_d_,99),
                           _g_=1;
                          break;
                         case 66:
                         case 98:
                          var
                           _e_=caml_call_gen3_r_(add_conv_b_,skip_f_,i_d_,66),
                           _g_=1;
                          break;
                         case 41:
                         case num_125_bU_:
                          var
                           _e_=caml_call_gen3_r_(add_conv_b_,skip_f_,i_d_,conv_j_),
                           _g_=1;
                          break;
                         case 40:
                          var
                           _e_=
                            scan_fmt_s_
                             (caml_call_gen3_r_(add_conv_b_,skip_f_,i_d_,conv_j_)),
                           _g_=1;
                          break;
                         case num_123_b0_:
                          var
                           i_u_=caml_call_gen3_r_(add_conv_b_,skip_f_,i_d_,conv_j_),
                           j_v_=
                            caml_call_gen2_l_
                             (sub_format_for_printf_cn_(conv_j_),fmt_i_,i_u_),
                           i_o_=i_u_;
                          for(;;)
                           {if(i_o_<(j_v_-2|0))
                             {var
                               i_o_=
                                caml_call_gen2_l_(add_char_c_,i_o_,fmt_i_.safeGet(i_o_));
                              continue}
                            var i_d_=j_v_-1|0;
                            continue b}
                         default:var _g_=0}
                      if(!_g_)
                       var _e_=bad_conversion_format_at_(fmt_i_,i_d_,conv_j_)}
                    break}}
                var i_k_=_e_;
                continue a}}
            var i_k_=i_k_+1|0;
            continue}
          return i_k_}}
      scan_fmt_s_(0);
      return 0}
    function count_printing_arguments_of_format_cp_(fmt_a_)
     {var ac_d_=[0,0,0,0];
      function add_conv_b_(skip_a_,i_b_,c_c_)
       {var _f_=41!==c_c_?1:0,_g_=_f_?num_125_bU_!==c_c_?1:0:_f_;
        if(_g_)
         {var inc_e_=97===c_c_?2:1;
          if(num_114_a8_===c_c_)ac_d_[3]=ac_d_[3]+1|0;
          if(skip_a_)
           ac_d_[2]=ac_d_[2]+inc_e_|0;
          else
           ac_d_[1]=ac_d_[1]+inc_e_|0}
        return i_b_+1|0}
      iter_on_format_args_co_
       (fmt_a_,add_conv_b_,function(i_a_,param_b_){return i_a_+1|0});
      return ac_d_[1]}
    function scan_positional_spec_cq_(fmt_a_,got_spec_b_,i_c_)
     {var d_g_=fmt_a_.safeGet(i_c_);
      if((d_g_+num_48_aj_|0)<0||9<(d_g_+num_48_aj_|0))
       return caml_call_gen2_l_(got_spec_b_,0,i_c_);
      var accu_e_=d_g_+num_48_aj_|0,j_d_=i_c_+1|0;
      for(;;)
       {var d_f_=fmt_a_.safeGet(j_d_);
        if(48<=d_f_)
         {if(!(58<=d_f_))
           {var accu_e_=(10*accu_e_|0)+(d_f_+num_48_aj_|0)|0,j_d_=j_d_+1|0;
            continue}}
        else
         if(36===d_f_)
          return 0===accu_e_
                  ?failwith_U_(_d$_)
                  :caml_call_gen2_l_
                    (got_spec_b_,[0,index_of_int_bo_(accu_e_-1|0)],j_d_+1|0);
        return caml_call_gen2_l_(got_spec_b_,0,i_c_)}}
    function next_index_q_(spec_a_,n_b_){return spec_a_?n_b_:_cj_(n_b_)}
    function get_index_cr_(spec_a_,n_b_){return spec_a_?spec_a_[1]:n_b_}
    function _cs_(to_s_aK_,get_out_b_,outc_c_,outs_d_,flush_e_,k_f_,fmt_g_)
     {var out_A_=caml_call_gen1_i_(get_out_b_,fmt_g_);
      function outs_af_(s_a_){return caml_call_gen2_l_(outs_d_,out_A_,s_a_)}
      function pr_aM_(k_a_,n_b_,fmt_j_,v_aN_)
       {var len_o_=fmt_j_.getLen();
        function doprn_B_(n_r_,i_b_)
         {var i_n_=i_b_;
          for(;;)
           {if(len_o_<=i_n_)return caml_call_gen1_i_(k_a_,out_A_);
            var c_d_=fmt_j_.safeGet(i_n_);
            if(37===c_d_)
             {var
               get_arg_m_=
                function(spec_a_,n_b_)
                 {return caml_array_get_k_(v_aN_,get_index_cr_(spec_a_,n_b_))},
               scan_flags_au_=
                function(spec_g_,n_f_,widths_c_,i_d_)
                 {var i_a_=i_d_;
                  for(;;)
                   {var switcher_$_=fmt_j_.safeGet(i_a_)+num_32_dl_|0;
                    if(!(switcher_$_<0||25<switcher_$_))
                     switch(switcher_$_)
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
                        return scan_positional_spec_cq_
                                (fmt_j_,
                                 function(wspec_a_,i_b_)
                                  {var _d_=[0,get_arg_m_(wspec_a_,n_f_),widths_c_];
                                   return scan_flags_au_
                                           (spec_g_,next_index_q_(wspec_a_,n_f_),_d_,i_b_)},
                                 i_a_+1|0);
                       default:var i_a_=i_a_+1|0;continue}
                    var conv_o_=fmt_j_.safeGet(i_a_);
                    if(!(124<=conv_o_))
                     switch(conv_o_)
                      {case 78:
                       case 88:
                       case num_100_ba_:
                       case num_105_ac_:
                       case num_111_b1_:
                       case num_117_bX_:
                       case num_120_bY_:
                        var
                         x_a9_=get_arg_m_(spec_g_,n_f_),
                         s_a__=
                          caml_format_int_aX_
                           (extract_format_int_cm_(conv_o_,fmt_j_,i_n_,i_a_,widths_c_),
                            x_a9_);
                        return cont_s_p_(next_index_q_(spec_g_,n_f_),s_a__,i_a_+1|0);
                       case 69:
                       case 71:
                       case num_101_dp_:
                       case num_102_b3_:
                       case num_103_b2_:
                        var
                         x_a1_=get_arg_m_(spec_g_,n_f_),
                         s_a2_=
                          caml_format_float_bO_
                           (extract_format_W_(fmt_j_,i_n_,i_a_,widths_c_),x_a1_);
                        return cont_s_p_(next_index_q_(spec_g_,n_f_),s_a2_,i_a_+1|0);
                       case 76:
                       case num_108_dt_:
                       case num_110_aD_:
                        var _ad_=fmt_j_.safeGet(i_a_+1|0)+num_88_dq_|0;
                        if(!(_ad_<0||32<_ad_))
                         switch(_ad_)
                          {case 0:
                           case 12:
                           case 17:
                           case 23:
                           case 29:
                           case 32:
                            var i_R_=i_a_+1|0,switcher_ae_=conv_o_-108|0;
                            if(switcher_ae_<0||2<switcher_ae_)
                             var _ag_=0;
                            else
                             {switch(switcher_ae_)
                               {case 1:var _ag_=0,_ah_=0;break;
                                case 2:
                                 var
                                  x_a7_=get_arg_m_(spec_g_,n_f_),
                                  _aA_=
                                   caml_format_int_aX_
                                    (extract_format_W_(fmt_j_,i_n_,i_R_,widths_c_),x_a7_),
                                  _ah_=1;
                                 break;
                                default:
                                 var
                                  x_a6_=get_arg_m_(spec_g_,n_f_),
                                  _aA_=
                                   caml_format_int_aX_
                                    (extract_format_W_(fmt_j_,i_n_,i_R_,widths_c_),x_a6_),
                                  _ah_=1}
                              if(_ah_)var s_az_=_aA_,_ag_=1}
                            if(!_ag_)
                             var
                              x_a5_=get_arg_m_(spec_g_,n_f_),
                              s_az_=
                               caml_int64_format_g5_
                                (extract_format_W_(fmt_j_,i_n_,i_R_,widths_c_),x_a5_);
                            return cont_s_p_(next_index_q_(spec_g_,n_f_),s_az_,i_R_+1|0)
                           }
                        var
                         x_a3_=get_arg_m_(spec_g_,n_f_),
                         s_a4_=
                          caml_format_int_aX_
                           (extract_format_int_cm_
                             (num_110_aD_,fmt_j_,i_n_,i_a_,widths_c_),
                            x_a3_);
                        return cont_s_p_(next_index_q_(spec_g_,n_f_),s_a4_,i_a_+1|0);
                       case 37:
                       case 64:return cont_s_p_(n_f_,_ap_(1,conv_o_),i_a_+1|0);
                       case 83:
                       case num_115_aE_:
                        var s_w_=get_arg_m_(spec_g_,n_f_);
                        if(num_115_aE_===conv_o_)
                         var x_x_=s_w_;
                        else
                         {var n_b_=[0,0],_al_=s_w_.getLen()-1|0,_aO_=0;
                          if(!(_al_<0))
                           {var i_K_=_aO_;
                            for(;;)
                             {var
                               c_v_=s_w_.safeGet(i_K_),
                               _bf_=
                                14<=c_v_
                                 ?34===c_v_?1:92===c_v_?1:0
                                 :11<=c_v_?13<=c_v_?1:0:8<=c_v_?1:0,
                               _aR_=_bf_?2:caml_is_printable_bP_(c_v_)?1:4;
                              n_b_[1]=n_b_[1]+_aR_|0;
                              var _aS_=i_K_+1|0;
                              if(_al_!==i_K_){var i_K_=_aS_;continue}
                              break}}
                          if(n_b_[1]===s_w_.getLen())
                           var _aC_=s_w_;
                          else
                           {var s_k_=caml_create_string_J_(n_b_[1]);
                            n_b_[1]=0;
                            var _am_=s_w_.getLen()-1|0,_aP_=0;
                            if(!(_am_<0))
                             {var i_I_=_aP_;
                              for(;;)
                               {var c_u_=s_w_.safeGet(i_I_),_y_=c_u_-34|0;
                                if(_y_<0||58<_y_)
                                 if(-20<=_y_)
                                  var _S_=1;
                                 else
                                  {switch(_y_+34|0)
                                    {case 8:
                                      s_k_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s_k_.safeSet(n_b_[1],98);
                                      var _H_=1;
                                      break;
                                     case 9:
                                      s_k_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s_k_.safeSet(n_b_[1],num_116_bS_);
                                      var _H_=1;
                                      break;
                                     case 10:
                                      s_k_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s_k_.safeSet(n_b_[1],num_110_aD_);
                                      var _H_=1;
                                      break;
                                     case 13:
                                      s_k_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s_k_.safeSet(n_b_[1],num_114_a8_);
                                      var _H_=1;
                                      break;
                                     default:var _S_=1,_H_=0}
                                   if(_H_)var _S_=0}
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
                                 if(caml_is_printable_bP_(c_u_))
                                  s_k_.safeSet(n_b_[1],c_u_);
                                 else
                                  {s_k_.safeSet(n_b_[1],92);
                                   n_b_[1]++;
                                   s_k_.safeSet(n_b_[1],48+(c_u_/num_100_ba_|0)|0);
                                   n_b_[1]++;
                                   s_k_.safeSet(n_b_[1],48+((c_u_/10|0)%10|0)|0);
                                   n_b_[1]++;
                                   s_k_.safeSet(n_b_[1],48+(c_u_%10|0)|0)}
                                n_b_[1]++;
                                var _aQ_=i_I_+1|0;
                                if(_am_!==i_I_){var i_I_=_aQ_;continue}
                                break}}
                            var _aC_=s_k_}
                          var x_x_=_h_(_ek_,_h_(_aC_,_ej_))}
                        if(i_a_===(i_n_+1|0))
                         var s_aB_=x_x_;
                        else
                         {var sfmt_G_=extract_format_W_(fmt_j_,i_n_,i_a_,widths_c_);
                          try
                           {var neg_T_=0,i_s_=1;
                            for(;;)
                             {if(sfmt_G_.getLen()<=i_s_)
                               var _an_=[0,0,neg_T_];
                              else
                               {var match_U_=sfmt_G_.safeGet(i_s_);
                                if(49<=match_U_)
                                 if(58<=match_U_)
                                  var _ai_=0;
                                 else
                                  var
                                   _an_=
                                    [0,
                                     caml_int_of_string_aY_
                                      (_V_(sfmt_G_,i_s_,(sfmt_G_.getLen()-i_s_|0)-1|0)),
                                     neg_T_],
                                   _ai_=1;
                                else
                                 {if(45===match_U_){var neg_T_=1,i_s_=i_s_+1|0;continue}
                                  var _ai_=0}
                                if(!_ai_){var i_s_=i_s_+1|0;continue}}
                              var match_Y_=_an_;
                              break}}
                          catch(_f_)
                           {_f_=caml_wrap_exception_C_(_f_);
                            if(_f_[1]!==_aJ_)throw _f_;
                            var match_Y_=bad_conversion_cl_(sfmt_G_,0,num_115_aE_)}
                          var
                           p_L_=match_Y_[1],
                           len_z_=x_x_.getLen(),
                           neg_aT_=match_Y_[2],
                           i_M_=0,
                           pad_char_aU_=32;
                          if(p_L_===len_z_)
                           if(0===i_M_)var _Z_=x_x_,_aj_=1;else var _aj_=0;
                          else
                           var _aj_=0;
                          if(!_aj_)
                           if(p_L_<=len_z_)
                            var _Z_=_V_(x_x_,i_M_,len_z_);
                           else
                            {var res_X_=_ap_(p_L_,pad_char_aU_);
                             if(neg_aT_)
                              _aL_(x_x_,i_M_,res_X_,0,len_z_);
                             else
                              _aL_(x_x_,i_M_,res_X_,p_L_-len_z_|0,len_z_);
                             var _Z_=res_X_}
                          var s_aB_=_Z_}
                        return cont_s_p_(next_index_q_(spec_g_,n_f_),s_aB_,i_a_+1|0);
                       case 67:
                       case 99:
                        var c_r_=get_arg_m_(spec_g_,n_f_);
                        if(99===conv_o_)
                         var s_ax_=_ap_(1,c_r_);
                        else
                         {if(39===c_r_)
                           var _t_=_dP_;
                          else
                           if(92===c_r_)
                            var _t_=_dQ_;
                           else
                            {if(14<=c_r_)
                              var _D_=0;
                             else
                              switch(c_r_)
                               {case 8:var _t_=_dR_,_D_=1;break;
                                case 9:var _t_=_dS_,_D_=1;break;
                                case 10:var _t_=_dT_,_D_=1;break;
                                case 13:var _t_=_dU_,_D_=1;break;
                                default:var _D_=0}
                             if(!_D_)
                              if(caml_is_printable_bP_(c_r_))
                               {var s_ak_=caml_create_string_J_(1);
                                s_ak_.safeSet(0,c_r_);
                                var _t_=s_ak_}
                              else
                               {var s_E_=caml_create_string_J_(4);
                                s_E_.safeSet(0,92);
                                s_E_.safeSet(1,48+(c_r_/num_100_ba_|0)|0);
                                s_E_.safeSet(2,48+((c_r_/10|0)%10|0)|0);
                                s_E_.safeSet(3,48+(c_r_%10|0)|0);
                                var _t_=s_E_}}
                          var s_ax_=_h_(_eh_,_h_(_t_,_eg_))}
                        return cont_s_p_(next_index_q_(spec_g_,n_f_),s_ax_,i_a_+1|0);
                       case 66:
                       case 98:
                        var _aZ_=i_a_+1|0,_a0_=get_arg_m_(spec_g_,n_f_)?_dJ_:_dK_;
                        return cont_s_p_(next_index_q_(spec_g_,n_f_),_a0_,_aZ_);
                       case 40:
                       case num_123_b0_:
                        var
                         xf_Q_=get_arg_m_(spec_g_,n_f_),
                         i_av_=
                          caml_call_gen2_l_
                           (sub_format_for_printf_cn_(conv_o_),fmt_j_,i_a_+1|0);
                        if(num_123_b0_===conv_o_)
                         {var
                           b_N_=_bl_(xf_Q_.getLen()),
                           add_char_ao_=
                            function(i_a_,c_b_){_as_(b_N_,c_b_);return i_a_+1|0};
                          iter_on_format_args_co_
                           (xf_Q_,
                            function(skip_a_,i_b_,c_c_)
                             {if(skip_a_)_bn_(b_N_,_d__);else _as_(b_N_,37);
                              return add_char_ao_(i_b_,c_c_)},
                            add_char_ao_);
                          var _aV_=_bm_(b_N_);
                          return cont_s_p_(next_index_q_(spec_g_,n_f_),_aV_,i_av_)}
                        var
                         n_aw_=next_index_q_(spec_g_,n_f_),
                         m_be_=
                          add_int_index_ci_
                           (count_printing_arguments_of_format_cp_(xf_Q_),n_aw_);
                        return pr_aM_
                                (function(param_a_){return doprn_B_(m_be_,i_av_)},
                                 n_aw_,
                                 xf_Q_,
                                 v_aN_);
                       case 33:
                        caml_call_gen1_i_(flush_e_,out_A_);
                        return doprn_B_(n_f_,i_a_+1|0);
                       case 41:return cont_s_p_(n_f_,_ee_,i_a_+1|0);
                       case 44:return cont_s_p_(n_f_,_ef_,i_a_+1|0);
                       case 70:
                        var x_aa_=get_arg_m_(spec_g_,n_f_);
                        if(0===widths_c_)
                         var _ay_=_ei_;
                        else
                         {var sfmt___=extract_format_W_(fmt_j_,i_n_,i_a_,widths_c_);
                          if(70===conv_o_)
                           sfmt___.safeSet(sfmt___.getLen()-1|0,num_103_b2_);
                          var _ay_=sfmt___}
                        var match_ar_=caml_classify_float_gU_(x_aa_);
                        if(3===match_ar_)
                         var s_ab_=x_aa_<0?_eb_:_ec_;
                        else
                         if(4<=match_ar_)
                          var s_ab_=_ed_;
                         else
                          {var
                            s_P_=caml_format_float_bO_(_ay_,x_aa_),
                            i_O_=0,
                            l_aW_=s_P_.getLen();
                           for(;;)
                            {if(l_aW_<=i_O_)
                              var _aq_=_h_(s_P_,_ea_);
                             else
                              {var
                                _F_=s_P_.safeGet(i_O_)-46|0,
                                _bg_=
                                 _F_<0||23<_F_?55===_F_?1:0:(_F_-1|0)<0||21<(_F_-1|0)?1:0;
                               if(!_bg_){var i_O_=i_O_+1|0;continue}
                               var _aq_=s_P_}
                             var s_ab_=_aq_;
                             break}}
                        return cont_s_p_(next_index_q_(spec_g_,n_f_),s_ab_,i_a_+1|0);
                       case 91:
                        return bad_conversion_format_at_(fmt_j_,i_a_,conv_o_);
                       case 97:
                        var
                         printer_aF_=get_arg_m_(spec_g_,n_f_),
                         n_aG_=_cj_(get_index_cr_(spec_g_,n_f_)),
                         arg_aH_=get_arg_m_(0,n_aG_),
                         i_a$_=i_a_+1|0,
                         n_bb_=next_index_q_(spec_g_,n_aG_);
                        if(to_s_aK_)
                         outs_af_(caml_call_gen2_l_(printer_aF_,0,arg_aH_));
                        else
                         caml_call_gen2_l_(printer_aF_,out_A_,arg_aH_);
                        return doprn_B_(n_bb_,i_a$_);
                       case num_114_a8_:
                        return bad_conversion_format_at_(fmt_j_,i_a_,conv_o_);
                       case num_116_bS_:
                        var
                         printer_aI_=get_arg_m_(spec_g_,n_f_),
                         i_bc_=i_a_+1|0,
                         n_bd_=next_index_q_(spec_g_,n_f_);
                        if(to_s_aK_)
                         outs_af_(caml_call_gen1_i_(printer_aI_,0));
                        else
                         caml_call_gen1_i_(printer_aI_,out_A_);
                        return doprn_B_(n_bd_,i_bc_)
                       }
                    return bad_conversion_format_at_(fmt_j_,i_a_,conv_o_)}},
               i_f_=i_n_+1|0,
               widths_g_=0;
              return scan_positional_spec_cq_
                      (fmt_j_,
                       function(spec_a_,i_b_)
                        {return scan_flags_au_(spec_a_,n_r_,widths_g_,i_b_)},
                       i_f_)}
            caml_call_gen2_l_(outc_c_,out_A_,c_d_);
            var i_n_=i_n_+1|0;
            continue}}
        function cont_s_p_(n_a_,s_b_,i_c_)
         {outs_af_(s_b_);return doprn_B_(n_a_,i_c_)}
        return doprn_B_(n_b_,0)}
      var _p_=index_of_int_bo_(0);
      function kpr_m_(_a_,_b_){return pr_aM_(k_f_,_p_,_a_,_b_)}
      var nargs_n_=count_printing_arguments_of_format_cp_(fmt_g_);
      if(nargs_n_<0||6<nargs_n_)
       {var
         loop_o_=
          function(i_f_,args_b_)
           {if(nargs_n_<=i_f_)
             {var
               a_h_=caml_make_vect_K_(nargs_n_,0),
               f_i_=
                function(i_a_,arg_b_)
                 {return caml_array_set_j_(a_h_,(nargs_n_-i_a_|0)-1|0,arg_b_)},
               i_c_=0,
               param_a_=args_b_;
              for(;;)
               {if(param_a_)
                 {var _d_=param_a_[2],_e_=param_a_[1];
                  if(_d_)
                   {f_i_(i_c_,_e_);var i_c_=i_c_+1|0,param_a_=_d_;continue}
                  f_i_(i_c_,_e_)}
                return kpr_m_(fmt_g_,a_h_)}}
            return function(x_a_){return loop_o_(i_f_+1|0,[0,x_a_,args_b_])}};
        return loop_o_(0,0)}
      switch(nargs_n_)
       {case 1:
         return function(x_a_)
          {var a_b_=caml_make_vect_K_(1,0);
           caml_array_set_j_(a_b_,0,x_a_);
           return kpr_m_(fmt_g_,a_b_)};
        case 2:
         return function(x_a_,y_b_)
          {var a_c_=caml_make_vect_K_(2,0);
           caml_array_set_j_(a_c_,0,x_a_);
           caml_array_set_j_(a_c_,1,y_b_);
           return kpr_m_(fmt_g_,a_c_)};
        case 3:
         return function(x_a_,y_b_,z_c_)
          {var a_d_=caml_make_vect_K_(3,0);
           caml_array_set_j_(a_d_,0,x_a_);
           caml_array_set_j_(a_d_,1,y_b_);
           caml_array_set_j_(a_d_,2,z_c_);
           return kpr_m_(fmt_g_,a_d_)};
        case 4:
         return function(x_a_,y_b_,z_c_,t_d_)
          {var a_e_=caml_make_vect_K_(4,0);
           caml_array_set_j_(a_e_,0,x_a_);
           caml_array_set_j_(a_e_,1,y_b_);
           caml_array_set_j_(a_e_,2,z_c_);
           caml_array_set_j_(a_e_,3,t_d_);
           return kpr_m_(fmt_g_,a_e_)};
        case 5:
         return function(x_a_,y_b_,z_c_,t_d_,u_e_)
          {var a_f_=caml_make_vect_K_(5,0);
           caml_array_set_j_(a_f_,0,x_a_);
           caml_array_set_j_(a_f_,1,y_b_);
           caml_array_set_j_(a_f_,2,z_c_);
           caml_array_set_j_(a_f_,3,t_d_);
           caml_array_set_j_(a_f_,4,u_e_);
           return kpr_m_(fmt_g_,a_f_)};
        case 6:
         return function(x_a_,y_b_,z_c_,t_d_,u_e_,v_f_)
          {var a_h_=caml_make_vect_K_(6,0);
           caml_array_set_j_(a_h_,0,x_a_);
           caml_array_set_j_(a_h_,1,y_b_);
           caml_array_set_j_(a_h_,2,z_c_);
           caml_array_set_j_(a_h_,3,t_d_);
           caml_array_set_j_(a_h_,4,u_e_);
           caml_array_set_j_(a_h_,5,v_f_);
           return kpr_m_(fmt_g_,a_h_)};
        default:return kpr_m_(fmt_g_,[0])}}
    function _ct_(oc_d_)
     {function k_e_(_a_){return 0}
      function _b_(param_a_){return oc_d_}
      var _c_=0;
      return function(_a_)
       {return _cs_(_c_,_b_,_dO_,output_string_cb_,_cd_,k_e_,_a_)}}
    function _el_(fmt_a_){return _bl_(2*fmt_a_.getLen()|0)}
    function _aN_(k_c_)
     {function _b_(b_a_)
       {var s_b_=_bm_(b_a_);b_a_[2]=0;return caml_call_gen1_i_(k_c_,s_b_)}
      function _d_(_a_){return 0}
      var _e_=1;
      return function(_a_){return _cs_(_e_,_el_,_as_,_bn_,_d_,_b_,_a_)}}
    function _v_(fmt_a_)
     {return caml_call_gen1_i_(_aN_(function(s_a_){return s_a_}),fmt_a_)}
    var _bp_=[0,0];
    function _br_(x_a_,i_b_)
     {var f_c_=x_a_[i_b_+1];
      if(caml_obj_is_block_hs_(f_c_))
       {if(caml_obj_tag_c9_(f_c_)===252)
         return caml_call_gen1_i_(_v_(_em_),f_c_);
        if(caml_obj_tag_c9_(f_c_)===253)
         {var s_e_=caml_format_float_bO_(_dM_,f_c_),i_d_=0,l_g_=s_e_.getLen();
          for(;;)
           {if(l_g_<=i_d_)return _h_(s_e_,_dL_);
            var
             match_f_=s_e_.safeGet(i_d_),
             _j_=48<=match_f_?58<=match_f_?0:1:45===match_f_?1:0;
            if(_j_){var i_d_=i_d_+1|0;continue}
            return s_e_}}
        return _en_}
      return caml_call_gen1_i_(_v_(_eo_),f_c_)}
    function _cu_(x_a_,i_b_)
     {if(x_a_.length-1<=i_b_)return _ep_;
      var _c_=_cu_(x_a_,i_b_+1|0),_d_=_br_(x_a_,i_b_);
      return caml_call_gen2_l_(_v_(_eq_),_d_,_c_)}
    function _bs_(x_a_)
     {var param_b_=_bp_[1];
      for(;;)
       {if(param_b_)
         {var tl_r_=param_b_[2],hd_s_=param_b_[1];
          try
           {var _u_=caml_call_gen1_i_(hd_s_,x_a_),_e_=_u_}
          catch(_f_){var _e_=0}
          if(_e_)return _e_[1];
          var param_b_=tl_r_;
          continue}
        if(x_a_[1]===_ev_)return _ew_;
        if(x_a_[1]===_cv_)return _ex_;
        if(x_a_[1]===_cw_)
         {var
           match_f_=x_a_[2],
           char_k_=match_f_[3],
           line_w_=match_f_[2],
           file_x_=match_f_[1];
          return caml_call_gen5_aZ_
                  (_v_(_bq_),file_x_,line_w_,char_k_,char_k_+5|0,_ey_)}
        if(x_a_[1]===_t_)
         {var
           match_g_=x_a_[2],
           char_m_=match_g_[3],
           line_y_=match_g_[2],
           file_z_=match_g_[1];
          return caml_call_gen5_aZ_
                  (_v_(_bq_),file_z_,line_y_,char_m_,char_m_+6|0,_ez_)}
        if(x_a_[1]===_cx_)
         {var
           match_j_=x_a_[2],
           char_n_=match_j_[3],
           line_A_=match_j_[2],
           file_B_=match_j_[1];
          return caml_call_gen5_aZ_
                  (_v_(_bq_),file_B_,line_A_,char_n_,char_n_+6|0,_eA_)}
        var n_d_=x_a_.length-1,constructor_C_=x_a_[0+1][0+1];
        if(n_d_<0||2<n_d_)
         var
          _o_=_cu_(x_a_,2),
          _p_=_br_(x_a_,1),
          _c_=caml_call_gen2_l_(_v_(_er_),_p_,_o_);
        else
         switch(n_d_)
          {case 1:var _c_=_et_;break;
           case 2:
            var _q_=_br_(x_a_,1),_c_=caml_call_gen1_i_(_v_(_eu_),_q_);break;
           default:var _c_=_es_}
        return _h_(constructor_C_,_c_)}}
    function _cy_(outchan_a_)
     {var
       backtrace_h_=
        caml_convert_raw_backtrace_gV_
         (caml_get_exception_raw_backtrace_g3_(0));
      if(backtrace_h_)
       {var a_d_=backtrace_h_[1],_f_=a_d_.length-1-1|0,_q_=0;
        if(!(_f_<0))
         {var i_c_=_q_;
          for(;;)
           {if(caml_notequal_c__(caml_array_get_k_(a_d_,i_c_),_eH_))
             {var
               li_b_=caml_array_get_k_(a_d_,i_c_),
               is_raise_j_=0===li_b_[0]?li_b_[1]:li_b_[1],
               info_e_=is_raise_j_?0===i_c_?_eB_:_eE_:0===i_c_?_eF_:_eG_;
              if(0===li_b_[0])
               var
                endchar_m_=li_b_[5],
                startchar_n_=li_b_[4],
                lineno_o_=li_b_[3],
                filename_p_=li_b_[2],
                _g_=
                 caml_call_gen5_aZ_
                  (_v_(_eC_),
                   info_e_,
                   filename_p_,
                   lineno_o_,
                   startchar_n_,
                   endchar_m_);
              else
               var _g_=caml_call_gen1_i_(_v_(_eD_),info_e_);
              caml_call_gen2_l_(_ct_(outchan_a_),_eI_,_g_)}
            var _r_=i_c_+1|0;
            if(_f_!==i_c_){var i_c_=_r_;continue}
            break}}
        return 0}
      return caml_call_gen1_i_(_ct_(outchan_a_),_eJ_)}
    function _cz_(fn_a_){_bp_[1]=[0,fn_a_,_bp_[1]];return 0}
    32===_bk_;
    function _cA_(param_a_)
     {var seq_b_=[];
      caml_update_dummy_c$_(seq_b_,[0,seq_b_,seq_b_]);
      return seq_b_}
    var Canceled_bt_=[0,_eK_],current_data_w_=[0,0];
    function repr_rec_bu_(t_a_)
     {var _c_=t_a_[1];
      if(3===_c_[0])
       {var t_d_=_c_[1],t_b_=repr_rec_bu_(t_d_);
        if(t_b_!==t_d_)t_a_[1]=[3,t_b_];
        return t_b_}
      return t_a_}
    function repr_X_(t_a_){return repr_rec_bu_(t_a_)}
    var
     async_exception_hook_cB_=
      [0,
       function(exn_a_)
        {prerr_string_cc_(_eL_);
         prerr_string_cc_(_bs_(exn_a_));
         caml_ml_output_char_c7_(stderr_an_,10);
         _cy_(stderr_an_);
         _cd_(stderr_an_);
         do_at_exit_bh_(0);
         return caml_sys_exit_hB_(2)}];
    function call_unsafe_cC_(f_a_,x_b_)
     {try
       {var _c_=caml_call_gen1_i_(f_a_,x_b_)}
      catch(exn_f_)
       {exn_f_=caml_wrap_exception_C_(exn_f_);
        return caml_call_gen1_i_(async_exception_hook_cB_[1],exn_f_)}
      return _c_}
    function run_waiters_rec_bQ_(counter_a_,state_b_,ws_c_,rem_d_)
     {var ws_f_=ws_c_,rem_e_=rem_d_;
      for(;;)
       if(typeof ws_f_===str_number_u_)
        return counter_a_<50
                ?run_waiters_rec_next_L_(1+counter_a_,state_b_,rem_e_)
                :caml_trampoline_return_M_
                  (run_waiters_rec_next_L_,[0,state_b_,rem_e_]);
       else
        switch(ws_f_[0])
         {case 1:
           caml_call_gen1_i_(ws_f_[1],state_b_);
           return counter_a_<50
                   ?run_waiters_rec_next_L_(1+counter_a_,state_b_,rem_e_)
                   :caml_trampoline_return_M_
                     (run_waiters_rec_next_L_,[0,state_b_,rem_e_]);
          case 2:
           var _h_=[0,ws_f_[2],rem_e_],ws_f_=ws_f_[1],rem_e_=_h_;continue;
          default:
           var _g_=ws_f_[1][1];
           if(_g_)
            {caml_call_gen1_i_(_g_[1],state_b_);
             return counter_a_<50
                     ?run_waiters_rec_next_L_(1+counter_a_,state_b_,rem_e_)
                     :caml_trampoline_return_M_
                       (run_waiters_rec_next_L_,[0,state_b_,rem_e_])}
           else
            return counter_a_<50
                    ?run_waiters_rec_next_L_(1+counter_a_,state_b_,rem_e_)
                    :caml_trampoline_return_M_
                      (run_waiters_rec_next_L_,[0,state_b_,rem_e_])}}
    function run_waiters_rec_next_L_(counter_a_,state_b_,rem_c_)
     {return rem_c_
              ?counter_a_<50
                ?run_waiters_rec_bQ_
                  (1+counter_a_,state_b_,rem_c_[1],rem_c_[2])
                :caml_trampoline_return_M_
                  (run_waiters_rec_bQ_,[0,state_b_,rem_c_[1],rem_c_[2]])
              :0}
    function run_waiters_rec_eM_(state_b_,ws_c_,rem_d_)
     {return caml_trampoline_a0_(run_waiters_rec_bQ_(0,state_b_,ws_c_,rem_d_))}
    function run_waiters_rec_next_hJ_(state_b_,rem_c_)
     {return caml_trampoline_a0_(run_waiters_rec_next_L_(0,state_b_,rem_c_))}
    function run_cancel_handlers_rec_bR_(counter_a_,chs_b_,rem_c_)
     {var chs_e_=chs_b_,rem_d_=rem_c_;
      for(;;)
       if(typeof chs_e_===str_number_u_)
        return counter_a_<50
                ?run_cancel_handlers_rec_next_$_(1+counter_a_,rem_d_)
                :caml_trampoline_return_M_
                  (run_cancel_handlers_rec_next_$_,[0,rem_d_]);
       else
        switch(chs_e_[0])
         {case 1:
           var n_f_=chs_e_[1];
           if(n_f_[4]){n_f_[4]=0;n_f_[1][2]=n_f_[2];n_f_[2][1]=n_f_[1]}
           return counter_a_<50
                   ?run_cancel_handlers_rec_next_$_(1+counter_a_,rem_d_)
                   :caml_trampoline_return_M_
                     (run_cancel_handlers_rec_next_$_,[0,rem_d_]);
          case 2:
           var _h_=[0,chs_e_[2],rem_d_],chs_e_=chs_e_[1],rem_d_=_h_;continue;
          default:
           var f_g_=chs_e_[2];
           current_data_w_[1]=chs_e_[1];
           call_unsafe_cC_(f_g_,0);
           return counter_a_<50
                   ?run_cancel_handlers_rec_next_$_(1+counter_a_,rem_d_)
                   :caml_trampoline_return_M_
                     (run_cancel_handlers_rec_next_$_,[0,rem_d_])}}
    function run_cancel_handlers_rec_next_$_(counter_a_,rem_b_)
     {return rem_b_
              ?counter_a_<50
                ?run_cancel_handlers_rec_bR_(1+counter_a_,rem_b_[1],rem_b_[2])
                :caml_trampoline_return_M_
                  (run_cancel_handlers_rec_bR_,[0,rem_b_[1],rem_b_[2]])
              :0}
    function run_cancel_handlers_rec_eN_(chs_b_,rem_c_)
     {return caml_trampoline_a0_(run_cancel_handlers_rec_bR_(0,chs_b_,rem_c_))}
    function run_cancel_handlers_rec_next_hK_(rem_b_)
     {return caml_trampoline_a0_(run_cancel_handlers_rec_next_$_(0,rem_b_))}
    function unsafe_run_waiters_aO_(sleeper_a_,state_b_)
     {var
       _c_=
        1===state_b_[0]
         ?state_b_[1][1]===Canceled_bt_
           ?(run_cancel_handlers_rec_eN_(sleeper_a_[4],0),1)
           :0
         :0;
      return run_waiters_rec_eM_(state_b_,sleeper_a_[2],0)}
    var wakening_bv_=[0,0],q_cD_=_ce_(0);
    function wakeup_result_cE_(t_a_,result_b_)
     {var t_f_=repr_rec_bu_(t_a_),_c_=t_f_[1];
      switch(_c_[0])
       {case 1:if(_c_[1][1]===Canceled_bt_)return 0;break;
        case 2:
         var sleeper_h_=_c_[1];
         t_f_[1]=result_b_;
         var
          snapshot_d_=current_data_w_[1],
          already_wakening_g_=wakening_bv_[1]?1:(wakening_bv_[1]=1,0);
         unsafe_run_waiters_aO_(sleeper_h_,result_b_);
         if(already_wakening_g_){current_data_w_[1]=snapshot_d_;return 0}
         for(;;)
          {if(0===q_cD_[1])
            {wakening_bv_[1]=0;current_data_w_[1]=snapshot_d_;return 0}
           var closure_e_=_cf_(q_cD_);
           unsafe_run_waiters_aO_(closure_e_[1],closure_e_[2]);
           continue}
        }
      return invalid_arg_am_(_eO_)}
    function wakeup_aP_(t_a_,v_b_){return wakeup_result_cE_(t_a_,[0,v_b_])}
    function append_cF_(l1_a_,l2_b_)
     {return typeof l1_a_===str_number_u_
              ?l2_b_
              :typeof l2_b_===str_number_u_?l1_a_:[2,l1_a_,l2_b_]}
    function cleanup_bw_(ws_a_)
     {if(typeof ws_a_!==str_number_u_)
       switch(ws_a_[0])
        {case 2:
          var l1_b_=ws_a_[1],_c_=cleanup_bw_(ws_a_[2]);
          return append_cF_(cleanup_bw_(l1_b_),_c_);
         case 1:break;
         default:if(!ws_a_[1][1])return 0}
      return ws_a_}
    function connect_cG_(t1_a_,t2_b_)
     {var t1_d_=repr_X_(t1_a_),t2_g_=repr_X_(t2_b_),_j_=t1_d_[1];
      if(2===_j_[0])
       {var sleeper1_c_=_j_[1];
        if(t1_d_===t2_g_)return 0;
        var _e_=t2_g_[1];
        if(2===_e_[0])
         {var sleeper2_f_=_e_[1];
          t2_g_[1]=[3,t1_d_];
          sleeper1_c_[1]=sleeper2_f_[1];
          var
           waiters_k_=append_cF_(sleeper1_c_[2],sleeper2_f_[2]),
           removed_l_=sleeper1_c_[3]+sleeper2_f_[3]|0;
          if(42<removed_l_)
           {sleeper1_c_[3]=0;sleeper1_c_[2]=cleanup_bw_(waiters_k_)}
          else
           {sleeper1_c_[3]=removed_l_;sleeper1_c_[2]=waiters_k_}
          var
           _h_=sleeper2_f_[4],
           _i_=sleeper1_c_[4],
           _m_=
            typeof _i_===str_number_u_
             ?_h_
             :typeof _h_===str_number_u_?_i_:[2,_i_,_h_];
          sleeper1_c_[4]=_m_;
          return 0}
        t1_d_[1]=_e_;
        return unsafe_run_waiters_aO_(sleeper1_c_,_e_)}
      throw [0,_t_,_eP_]}
    function fast_connect_aQ_(t_a_,state_b_)
     {var t_c_=repr_X_(t_a_),_d_=t_c_[1];
      if(2===_d_[0])
       {var sleeper_e_=_d_[1];
        t_c_[1]=state_b_;
        return unsafe_run_waiters_aO_(sleeper_e_,state_b_)}
      throw [0,_t_,_eQ_]}
    function return_bx_(v_a_){return [0,[0,v_a_]]}
    var return_unit_eS_=[0,state_return_unit_eR_];
    function fail_by_(e_a_){return [0,[1,e_a_]]}
    function temp_bz_(t_a_){return [0,[2,[0,[0,[0,t_a_]],0,0,0]]]}
    function task_bA_(param_a_)
     {var t_b_=[0,[2,[0,1,0,0,0]]];return [0,t_b_,t_b_]}
    function add_immutable_waiter_bB_(sleeper_a_,waiter_b_)
     {var
       waiter_d_=[1,waiter_b_],
       _c_=sleeper_a_[2],
       waiter_e_=typeof _c_===str_number_u_?waiter_d_:[2,waiter_d_,_c_];
      sleeper_a_[2]=waiter_e_;
      return 0}
    function on_cancel_bC_(t_a_,f_b_)
     {var _c_=repr_X_(t_a_)[1];
      switch(_c_[0])
       {case 1:
         if(_c_[1][1]===Canceled_bt_)return call_unsafe_cC_(f_b_,0);break;
        case 2:
         var
          sleeper_d_=_c_[1],
          handler_e_=[0,current_data_w_[1],f_b_],
          _f_=sleeper_d_[4],
          handler_g_=typeof _f_===str_number_u_?handler_e_:[2,handler_e_,_f_];
         sleeper_d_[4]=handler_g_;
         return 0
        }
      return 0}
    function _aR_(t_a_,f_b_)
     {var t_e_=repr_X_(t_a_),_c_=t_e_[1];
      switch(_c_[0])
       {case 1:return [0,_c_];
        case 2:
         var
          sleeper_f_=_c_[1],
          res_d_=temp_bz_(t_e_),
          data_g_=current_data_w_[1];
         add_immutable_waiter_bB_
          (sleeper_f_,
           function(state_a_)
            {switch(state_a_[0])
              {case 0:
                var v_e_=state_a_[1];
                current_data_w_[1]=data_g_;
                try
                 {var _f_=caml_call_gen1_i_(f_b_,v_e_),_c_=_f_}
                catch(exn_f_)
                 {exn_f_=caml_wrap_exception_C_(exn_f_);
                  var _c_=fail_by_(exn_f_)}
                return connect_cG_(res_d_,_c_);
               case 1:return fast_connect_aQ_(res_d_,state_a_);
               default:throw [0,_t_,_eT_]}});
         return res_d_;
        case 3:throw [0,_t_,_eU_];
        default:return caml_call_gen1_i_(f_b_,_c_[1])}}
    function _bD_(t_a_,f_b_)
     {var t_e_=repr_X_(t_a_),_c_=t_e_[1];
      switch(_c_[0])
       {case 1:return [0,_c_];
        case 2:
         var
          sleeper_j_=_c_[1],
          res_d_=temp_bz_(t_e_),
          data_k_=current_data_w_[1];
         add_immutable_waiter_bB_
          (sleeper_j_,
           function(state_a_)
            {switch(state_a_[0])
              {case 0:
                var v_e_=state_a_[1];
                current_data_w_[1]=data_k_;
                try
                 {var _f_=[0,caml_call_gen1_i_(f_b_,v_e_)],_c_=_f_}
                catch(exn_f_)
                 {exn_f_=caml_wrap_exception_C_(exn_f_);var _c_=[1,exn_f_]}
                return fast_connect_aQ_(res_d_,_c_);
               case 1:return fast_connect_aQ_(res_d_,state_a_);
               default:throw [0,_t_,_eV_]}});
         return res_d_;
        case 3:throw [0,_t_,_eW_];
        default:
         var v_f_=_c_[1];
         try
          {var _h_=[0,caml_call_gen1_i_(f_b_,v_f_)],_g_=_h_}
         catch(exn_f_)
          {exn_f_=caml_wrap_exception_C_(exn_f_);var _g_=[1,exn_f_]}
         return [0,_g_]}}
    var pause_hook_eZ_=[0,function(_a_){return 0}],s1_A_=_cA_(0),_e0_=[0,0];
    function _e1_(param_a_)
     {var _e_=1-(s1_A_[2]===s1_A_?1:0);
      if(_e_)
       {var seq_b_=_cA_(0);
        seq_b_[1][2]=s1_A_[2];
        s1_A_[2][1]=seq_b_[1];
        seq_b_[1]=s1_A_[1];
        s1_A_[1][2]=seq_b_;
        s1_A_[1]=s1_A_;
        s1_A_[2]=s1_A_;
        _e0_[1]=0;
        var curr_c_=seq_b_[2];
        for(;;)
         {var _d_=curr_c_!==seq_b_?1:0;
          if(_d_)
           {if(curr_c_[4])wakeup_aP_(curr_c_[3],0);
            var curr_c_=curr_c_[2];
            continue}
          return _d_}}
      return _e_}
    function iter_s_cH_(f_c_,l_b_)
     {if(l_b_)
       {var
         l_d_=l_b_[2],
         x_a_=l_b_[1],
         _e_=function(param_a_){return iter_s_cH_(f_c_,l_d_)};
        return _aR_(caml_call_gen1_i_(f_c_,x_a_),_e_)}
      return return_unit_eS_}
    var
     window_x_=joo_global_object_b_,
     no_handler_g_=null,
     undefined_G_=undefined;
    function _bE_(x_a_,f_b_)
     {return x_a_==no_handler_g_?no_handler_g_:caml_call_gen1_i_(f_b_,x_a_)}
    function _aS_(x_a_,f_b_,g_c_)
     {return x_a_==no_handler_g_
              ?caml_call_gen1_i_(f_b_,0)
              :caml_call_gen1_i_(g_c_,x_a_)}
    function _bF_(x_a_,f_b_)
     {return x_a_==no_handler_g_?caml_call_gen1_i_(f_b_,0):x_a_}
    function _bG_(x_a_)
     {function _b_(x_a_){return [0,x_a_]}
      return _aS_(x_a_,function(param_a_){return 0},_b_)}
    function _au_(x_a_){return x_a_!==undefined_G_?1:0}
    function _bH_(x_a_,f_b_,g_c_)
     {return x_a_===undefined_G_
              ?caml_call_gen1_i_(f_b_,0)
              :caml_call_gen1_i_(g_c_,x_a_)}
    function _B_(x_a_,f_b_)
     {return x_a_===undefined_G_?caml_call_gen1_i_(f_b_,0):x_a_}
    function _ag_(x_a_)
     {function _b_(x_a_){return [0,x_a_]}
      return _bH_(x_a_,function(param_a_){return 0},_b_)}
    var true_Y_=true,false_H_=false,aa78544e1_av_=RegExp,abd4b70c7_cI_=Array;
    function array_get_m_(_a_,_b_){return _a_[_b_]}
    function str_array_cJ_(_a_){return _a_}
    function f_bI_(_a_){return _a_}
    var ac1bdc845_cK_=Date,Error_cL_=[0,_e3_];
    caml_register_named_value_c6_(name_e4_,[0,Error_cL_,{}][0+1]);
    var a593685be_e2_=Math;
    function escape_cM_(s_a_){return escape(s_a_)}
    _cz_
     (function(param_a_)
       {return param_a_[1]===Error_cL_
                ?[0,new MlWrappedString_p_(param_a_[2].toString())]
                :0});
    _cz_
     (function(e_a_)
       {return e_a_ instanceof abd4b70c7_cI_
                ?0
                :[0,new MlWrappedString_p_(e_a_.toString())]});
    function _aw_(_a_){return _a_}
    function _O_(_a_){return _a_}
    function handler_aT_(f_d_)
     {return _O_
              (caml_js_wrap_callback_a1_
                (function(e_a_)
                  {if(e_a_)
                    {var res_e_=caml_call_gen1_i_(f_d_,e_a_);
                     if(!(res_e_|0))e_a_.preventDefault();
                     return res_e_}
                   var e_c_=event,res_b_=caml_call_gen1_i_(f_d_,e_c_);
                   if(!(res_b_|0))e_c_.returnValue=res_b_;
                   return res_b_}))}
    var
     onIE_cN_=caml_js_on_ie_hh_(0)|0,
     document_aU_=window_x_.document,
     html_element_cO_=window_x_.HTMLElement,
     a82ca6ea6_e5_=window_x_.Float32Array,
     _e6_=
      _aw_(html_element_cO_)===undefined_G_
       ?function(e_a_)
         {return _aw_(e_a_.innerHTML)===undefined_G_?no_handler_g_:_O_(e_a_)}
       :function(e_a_)
         {return e_a_ instanceof html_element_cO_?_O_(e_a_):no_handler_g_};
    function _cP_(tag_a_,e_b_)
     {var _c_=tag_a_.toString();
      return e_b_.tagName.toLowerCase()===_c_?_O_(e_b_):no_handler_g_}
    function _e7_(e_a_){return _cP_(_e8_,e_a_)}
    function _e9_(e_a_){return _cP_(_e__,e_a_)}
    var
     a3257b5a8_bJ_=caml_js_get_console_hg_(0),
     _fa_=window_x_.FileReader,
     overflow_limit_fd_=num_2147483_bT_;
    pause_hook_eZ_[1]=
    function(param_a_)
     {return 1===param_a_
              ?(window_x_.setTimeout(caml_js_wrap_callback_a1_(_e1_),0),0)
              :0};
    function _cQ_(s_a_){return a3257b5a8_bJ_.log(s_a_.toString())}
    async_exception_hook_cB_[1]=
    function(exn_a_){_cQ_(_fe_);_cQ_(_bs_(exn_a_));return _cy_(stderr_an_)};
    function regexp_aV_(s_a_)
     {return new aa78544e1_av_(caml_js_from_byte_string_f_(s_a_),str_g_aA_)}
    function string_match_cR_(r_a_,s_b_,i_c_)
     {r_a_.lastIndex=i_c_;
      var
       x_d_=r_a_.exec(caml_js_from_byte_string_f_(s_b_)),
       _e_=x_d_==no_handler_g_?no_handler_g_:f_bI_(x_d_);
      return _bG_(_e_)}
    function matched_group_cS_(r_a_,i_b_)
     {var
       x_c_=array_get_m_(r_a_,i_b_),
       _d_=x_c_===undefined_G_?undefined_G_:caml_js_to_byte_string_N_(x_c_);
      return _ag_(_d_)}
    var
     quote_repl_re_ff_=new aa78544e1_av_("[$]",str_g_aA_),
     _fh_=regexp_aV_(_fg_);
    function split_cU_(c_a_,s_b_)
     {return str_array_cJ_(s_b_.split(_ap_(1,c_a_).toString()))}
    var Local_exn_cV_=[0,_fi_];
    function interrupt_Z_(param_a_){throw [0,Local_exn_cV_]}
    var
     plus_re_cT_=
      regexp_aV_
       (caml_js_to_byte_string_N_
         (caml_js_from_byte_string_f_(s_fj_).replace(_fh_,"\\$&"))),
     plus_re_js_string_cW_=new aa78544e1_av_("\\+",str_g_aA_);
    function urldecode_js_string_string_I_(s_a_)
     {plus_re_js_string_cW_[caml_new_string_c_(str_lastIndex_dk_)]=0;
      return caml_js_to_byte_string_N_
              (unescape(s_a_.replace(plus_re_js_string_cW_,str_D_)))}
    function urlencode_n_(opt_a_,s_b_)
     {var with_plus_e_=opt_a_?opt_a_[1]:1;
      if(with_plus_e_)
       {var
         s_g_=
          caml_js_to_byte_string_N_
           (escape_cM_(caml_js_from_byte_string_f_(s_b_)));
        plus_re_cT_[caml_new_string_c_(str_lastIndex_dk_)]=0;
        var a6bc4b4e3_d_=caml_js_from_byte_string_f_(s_g_);
        return caml_js_to_byte_string_N_
                (a6bc4b4e3_d_.replace
                  (plus_re_cT_,
                   caml_js_from_byte_string_f_(s_by_fk_).replace
                    (quote_repl_re_ff_,"$$$$")))}
      return caml_js_to_byte_string_N_
              (escape_cM_(caml_js_from_byte_string_f_(s_b_)))}
    var Not_an_http_protocol_fm_=[0,_fl_];
    function path_of_path_string_ax_(s_a_)
     {try
       {var length_c_=s_a_.getLen();
        if(0===length_c_)
         var _d_=_ft_;
        else
         {var i_b_=0,c_g_=47,lim_f_=s_a_.getLen();
          for(;;)
           {if(lim_f_<=i_b_)throw [0,_bj_];
            if(s_a_.safeGet(i_b_)!==c_g_){var i_b_=i_b_+1|0;continue}
            if(0===i_b_)
             var
              _e_=
               [0,_fu_,path_of_path_string_ax_(_V_(s_a_,1,length_c_-1|0))];
            else
             var
              _h_=
               path_of_path_string_ax_
                (_V_(s_a_,i_b_+1|0,(length_c_-i_b_|0)-1|0)),
              _e_=[0,_V_(s_a_,0,i_b_),_h_];
            var _d_=_e_;
            break}}}
      catch(_f_)
       {_f_=caml_wrap_exception_C_(_f_);
        if(_f_[1]===_bj_)return [0,s_a_,0];
        throw _f_}
      return _d_}
    function encode_arguments_aW_(l_a_)
     {return _aq_
              (_fw_,
               _F_
                (function(param_a_)
                  {var
                    n_b_=param_a_[1],
                    _c_=_h_(_fv_,urlencode_n_(0,param_a_[2]));
                   return _h_(urlencode_n_(0,n_b_),_c_)},
                 l_a_))}
    function decode_arguments_js_string_bK_(s_a_)
     {var arr_d_=split_cU_(38,s_a_),len_b_=arr_d_.length;
      function aux_e_(acc_a_,idx_b_)
       {var idx_c_=idx_b_;
        for(;;)
         {if(0<=idx_c_)
           {try
             {var
               _f_=idx_c_-1|0,
               _g_=
                function(s_a_)
                 {function _e_(param_a_)
                   {var y_c_=param_a_[2],x_d_=param_a_[1];
                    function get_b_(t_a_)
                     {return urldecode_js_string_string_I_
                              (_B_(t_a_,interrupt_Z_))}
                    var _e_=get_b_(y_c_);
                    return [0,get_b_(x_d_),_e_]}
                  var arr_bis_b_=split_cU_(61,s_a_);
                  if(2===arr_bis_b_.length)
                   var
                    _d_=array_get_m_(arr_bis_b_,1),
                    _c_=_aw_([0,array_get_m_(arr_bis_b_,0),_d_]);
                  else
                   var _c_=undefined_G_;
                  return _bH_(_c_,interrupt_Z_,_e_)},
               _h_=
                aux_e_
                 ([0,
                   _bH_(array_get_m_(arr_d_,idx_c_),interrupt_Z_,_g_),
                   acc_a_],
                  _f_)}
            catch(_f_)
             {_f_=caml_wrap_exception_C_(_f_);
              if(_f_[1]===Local_exn_cV_){var idx_c_=idx_c_-1|0;continue}
              throw _f_}
            return _h_}
          return acc_a_}}
      return aux_e_(0,len_b_-1|0)}
    var
     url_re_fy_=new aa78544e1_av_(caml_js_from_byte_string_f_(_fx_)),
     file_re_fA_=new aa78544e1_av_(caml_js_from_byte_string_f_(_fz_));
    function string_of_url_cX_(param_a_)
     {switch(param_a_[0])
       {case 1:
         var
          match_c_=param_a_[1],
          frag_i_=match_c_[6],
          args_j_=match_c_[5],
          port_k_=match_c_[2],
          path_x_=match_c_[3],
          host_y_=match_c_[1],
          _z_=
           caml_string_notequal_s_(frag_i_,_fR_)
            ?_h_(_fS_,urlencode_n_(0,frag_i_))
            :_f0_,
          _A_=args_j_?_h_(_fT_,encode_arguments_aW_(args_j_)):_fZ_,
          _B_=_h_(_A_,_z_),
          _C_=
           _h_
            (_fV_,
             _h_
              (_aq_
                (_fU_,
                 _F_(function(eta_a_){return urlencode_n_(0,eta_a_)},path_x_)),
               _B_)),
          _D_=num_443_de_===port_k_?_fW_:_h_(_fY_,string_of_int_ae_(port_k_)),
          _E_=_h_(_D_,_C_);
         return _h_(_fX_,_h_(urlencode_n_(0,host_y_),_E_));
        case 2:
         var
          match_d_=param_a_[1],
          frag_l_=match_d_[4],
          args_m_=match_d_[3],
          path_G_=match_d_[1],
          _H_=
           caml_string_notequal_s_(frag_l_,_f1_)
            ?_h_(_f2_,urlencode_n_(0,frag_l_))
            :_f7_,
          _I_=args_m_?_h_(_f3_,encode_arguments_aW_(args_m_)):_f6_,
          _J_=_h_(_I_,_H_);
         return _h_
                 (_f5_,
                  _h_
                   (_aq_
                     (_f4_,
                      _F_(function(eta_a_){return urlencode_n_(0,eta_a_)},path_G_)),
                    _J_));
        default:
         var
          match_b_=param_a_[1],
          frag_e_=match_b_[6],
          args_f_=match_b_[5],
          port_g_=match_b_[2],
          path_o_=match_b_[3],
          host_p_=match_b_[1],
          _q_=
           caml_string_notequal_s_(frag_e_,_fH_)
            ?_h_(_fI_,urlencode_n_(0,frag_e_))
            :_fQ_,
          _r_=args_f_?_h_(_fJ_,encode_arguments_aW_(args_f_)):_fP_,
          _t_=_h_(_r_,_q_),
          _u_=
           _h_
            (_fL_,
             _h_
              (_aq_
                (_fK_,
                 _F_(function(eta_a_){return urlencode_n_(0,eta_a_)},path_o_)),
               _t_)),
          _v_=80===port_g_?_fM_:_h_(_fO_,string_of_int_ae_(port_g_)),
          _w_=_h_(_v_,_u_);
         return _h_(_fN_,_h_(urlencode_n_(0,host_p_),_w_))}}
    var l_ay_=location;
    urldecode_js_string_string_I_(l_ay_.hostname);
    urldecode_js_string_string_I_(l_ay_.protocol);
    try
     {}
    catch(_f_){_f_=caml_wrap_exception_C_(_f_);if(_f_[1]!==_aJ_)throw _f_}
    path_of_path_string_ax_(urldecode_js_string_string_I_(l_ay_.pathname));
    decode_arguments_js_string_bK_(l_ay_.search);
    urldecode_js_string_string_I_(l_ay_.href);
    var formData_f8_=window_x_.FormData;
    function _cY_(form_contents_a_,form_elt_b_)
     {if(num_891486873_bb_<=form_contents_a_[1])
       {var list_d_=form_contents_a_[2];
        list_d_[1]=[0,form_elt_b_,list_d_[1]];
        return 0}
      var f_e_=form_contents_a_[2],_c_=form_elt_b_[2],_f_=form_elt_b_[1];
      return num_781515420_a4_<=_c_[1]
              ?f_e_.append(_f_.toString(),_c_[2])
              :f_e_.append(_f_.toString(),_c_[2])}
    function _bL_(param_a_){return ActiveXObject}
    ({"alpha":true_Y_,
      "depth":true_Y_,
      "stencil":false_H_,
      "antialias":true_Y_,
      "premultipliedAlpha":false_H_,
      "preserveDrawingBuffer":false_H_,
      "preferLowPowerToHighPerformance":false_H_,
      "failIfMajorPerformanceCaveat":false_H_});
    var _gl_=[0,_gk_];
    function error_ah_(f_a_)
     {return caml_call_gen1_i_
              (_aN_
                (function(s_a_)
                  {a3257b5a8_bJ_.error(s_a_.toString());
                   return failwith_U_(s_a_)}),
               f_a_)}
    function debug_cZ_(f_a_)
     {return caml_call_gen1_i_
              (_aN_(function(s_a_){return a3257b5a8_bJ_.log(s_a_.toString())}),
               f_a_)}
    function check_error_bM_(gl_a_)
     {var _b_=gl_a_.NO;
      return caml_notequal_c__(gl_a_.getError(),_b_)?error_ah_(_gt_):0}
    function load_shader_c0_(gl_a_,shader_b_,text_c_)
     {gl_a_.shaderSource(shader_b_,text_c_);
      gl_a_.compileShader(shader_b_);
      if(gl_a_.getShaderParameter(shader_b_,gl_a_.COMPILE_STATUS)|0)return 0;
      var
       _d_=new MlWrappedString_p_(gl_a_.getShaderInfoLog(shader_b_)),
       _e_=new MlWrappedString_p_(text_c_);
      return caml_call_gen2_l_(error_ah_(_gw_),_e_,_d_)}
    function get_source_c1_(src_id_b_)
     {function _a_(param_a_)
       {return caml_call_gen1_i_(error_ah_(_gy_),src_id_b_)}
      return _bF_
              (_bE_(document_aU_.getElementById(src_id_b_.toString()),_e9_),
               _a_).text}
    function float32array_az_(a_a_)
     {var a_d_=new a82ca6ea6_e5_(a_a_.length-1),_c_=a_a_.length-1-1|0,_e_=0;
      if(!(_c_<0))
       {var i_b_=_e_;
        for(;;)
         {a_d_[i_b_]=a_a_[i_b_+1];
          var _f_=i_b_+1|0;
          if(_c_!==i_b_){var i_b_=_f_;continue}
          break}}
      return a_d_}
    function _P_(i_a_,j_b_){return (i_a_*4|0)+j_b_|0}
    function _bN_(m1_e_,m2_b_)
     {return _aK_
              (16,
               function(p_a_)
                {var
                  j_c_=p_a_%4|0,
                  i_d_=p_a_/4|0,
                  _f_=caml_array_get_k_(m2_b_,_P_(3,j_c_)),
                  _g_=caml_array_get_k_(m1_e_,_P_(i_d_,3))*_f_,
                  _h_=caml_array_get_k_(m2_b_,_P_(2,j_c_)),
                  _i_=caml_array_get_k_(m1_e_,_P_(i_d_,2))*_h_,
                  _j_=caml_array_get_k_(m2_b_,_P_(1,j_c_)),
                  _l_=caml_array_get_k_(m1_e_,_P_(i_d_,1))*_j_,
                  _m_=caml_array_get_k_(m2_b_,_P_(0,j_c_));
                 return caml_array_get_k_(m1_e_,_P_(i_d_,0))*_m_+_l_+_i_+_g_})}
    var line_regexp_gA_=regexp_aV_(_gz_),couple_regexp_gC_=regexp_aV_(_gB_);
    function read_coord_couple_gD_(c_a_)
     {var match_d_=string_match_cR_(couple_regexp_gC_,c_a_,0);
      if(match_d_)
       {var
         res_g_=match_d_[1],
         match_b_=
          _F_(function(_a_){return matched_group_cS_(res_g_,_a_)},_gE_);
        if(match_b_)
         {var _e_=match_b_[1];
          if(_e_)
           {var _c_=match_b_[2];
            if(_c_)
             {var _f_=_c_[1];
              if(_f_)
               if(!_c_[2])
                return [0,
                        [0,
                         caml_int_of_string_aY_(_e_[1]),
                         caml_int_of_string_aY_(_f_[1])]]}}}
        return 0}
      return 0}
    function concat_c2_(a_e_)
     {var r_b_=[0,0],_g_=a_e_.length-1-1|0,_i_=0;
      if(!(_g_<0))
       {var i_a_=_i_;
        a:
        for(;;)
         {var len_d_=0,param_c_=a_e_[i_a_+1],_l_=r_b_[1];
          for(;;)
           {if(param_c_){var len_d_=len_d_+1|0,param_c_=param_c_[2];continue}
            r_b_[1]=_l_+len_d_|0;
            var _m_=i_a_+1|0;
            if(_g_!==i_a_){var i_a_=_m_;continue a}
            break}
          break}}
      var pos_h_=[0,-1],l_f_=[0,0],_j_=r_b_[1];
      return _aK_
              (_j_,
               function(param_a_)
                {for(;;)
                  {var _b_=l_f_[1];
                   if(_b_){var t_c_=_b_[1];l_f_[1]=_b_[2];return t_c_}
                   pos_h_[1]++;
                   l_f_[1]=caml_array_get_k_(a_e_,pos_h_[1]);
                   continue}})}
    var pi_gJ_=4*0.785398163397448279;
    function start_gK_(param_a_)
     {var
       pos_p_=param_a_[1],
       norm_z_=param_a_[2],
       n_q_=document_aU_.createTextNode("loading"),
       span_m_=_bE_(document_aU_.getElementById("fps"),_e6_);
      if(span_m_!=no_handler_g_)span_m_.appendChild(n_q_);
      function _u_(param_a_)
       {return caml_call_gen1_i_(error_ah_(_gu_),canvas_id_gL_)}
      var
       c_h_=
        _bF_(_bE_(document_aU_.getElementById(str_canvas_bW_),_e7_),_u_);
      function _w_(param_a_)
       {return caml_call_gen1_i_
                (_aN_
                  (function(s_a_)
                    {window_x_.alert(s_a_.toString());return failwith_U_(s_a_)}),
                 f_gv_)}
      try
       {var
         x_f_=c_h_.getContext("webgl"),
         _y_=
          1-(x_f_==no_handler_g_?1:0)
           ?x_f_
           :c_h_.getContext("experimental-webgl"),
         _j_=_y_}
      catch(_f_){var _j_=no_handler_g_}
      var
       gl_b_=_bF_(_j_,_w_),
       frag_src_A_=get_source_c1_(_gM_),
       vert_src_B_=get_source_c1_(_gN_),
       vertexShader_k_=gl_b_.createShader(gl_b_.VERTEX_SHADER),
       fragmentShader_l_=gl_b_.createShader(gl_b_.FRAGMENT_SHADER);
      load_shader_c0_(gl_b_,vertexShader_k_,vert_src_B_);
      load_shader_c0_(gl_b_,fragmentShader_l_,frag_src_A_);
      var prog_d_=gl_b_.createProgram();
      gl_b_.attachShader(prog_d_,vertexShader_k_);
      gl_b_.attachShader(prog_d_,fragmentShader_l_);
      gl_b_.linkProgram(prog_d_);
      if(!(gl_b_.getProgramParameter(prog_d_,gl_b_.LINK_STATUS)|0))
       error_ah_(_gx_);
      gl_b_.useProgram(prog_d_);
      check_error_bM_(gl_b_);
      debug_cZ_(_gO_);
      gl_b_.enable(gl_b_.DEPTH_TEST);
      gl_b_.depthFunc(gl_b_.LESS);
      var
       proj_loc_C_=gl_b_.getUniformLocation(prog_d_,"u_proj"),
       lightPos_loc_D_=gl_b_.getUniformLocation(prog_d_,"u_lightPos"),
       ambientLight_loc_E_=gl_b_.getUniformLocation(prog_d_,"u_ambientLight"),
       lightPos_F_=float32array_az_([num_254_S_,3,0,-1]),
       ambientLight_G_=
        float32array_az_([num_254_S_,num_0_1_bV_,num_0_1_bV_,num_0_1_bV_]);
      gl_b_.uniform3fv(lightPos_loc_D_,lightPos_F_);
      gl_b_.uniform3fv(ambientLight_loc_E_,ambientLight_G_);
      var pos_attr_n_=gl_b_.getAttribLocation(prog_d_,"a_position");
      gl_b_.enableVertexAttribArray(pos_attr_n_);
      var array_buffer_I_=gl_b_.createBuffer();
      gl_b_.bindBuffer(gl_b_.ARRAY_BUFFER,array_buffer_I_);
      gl_b_.bufferData(gl_b_.ARRAY_BUFFER,pos_p_,gl_b_.STATIC_DRAW);
      gl_b_.vertexAttribPointer(pos_attr_n_,3,gl_b_.FLOAT,false_H_,0,0);
      var norm_attr_o_=gl_b_.getAttribLocation(prog_d_,"a_normal");
      gl_b_.enableVertexAttribArray(norm_attr_o_);
      var norm_buffer_J_=gl_b_.createBuffer();
      gl_b_.bindBuffer(gl_b_.ARRAY_BUFFER,norm_buffer_J_);
      gl_b_.bufferData(gl_b_.ARRAY_BUFFER,norm_z_,gl_b_.STATIC_DRAW);
      gl_b_.vertexAttribPointer(norm_attr_o_,3,gl_b_.FLOAT,false_H_,0,0);
      var
       t_e_=pi_gJ_/2,
       mat_K_=
        _bN_
         ([num_254_S_,
           1,
           0,
           0,
           0,
           0,
           Math.cos(t_e_),
           Math.sin(t_e_),
           0,
           0,
           -Math.sin(t_e_),
           Math.cos(t_e_),
           0,
           0,
           0,
           0,
           1],
          _bN_
           ([num_254_S_,
             num_0_5_b5_,
             0,
             0,
             0,
             0,
             num_0_5_b5_,
             0,
             0,
             0,
             0,
             num_0_5_b5_,
             0,
             0,
             0,
             0,
             1],
            [num_254_S_,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1]));
      check_error_bM_(gl_b_);
      debug_cZ_(_gP_);
      function get_time_r_(param_a_){return new ac1bdc845_cK_().getTime()}
      var last_draw_s_=[0,get_time_r_(0)],q_c_=_ce_(0);
      function f_t_(param_a_)
       {var t_e_=1*(new ac1bdc845_cK_().getTime()/num_1e3_a$_);
        gl_b_.uniformMatrix4fv
         (proj_loc_C_,
          false_H_,
          float32array_az_
           (_bN_
             (mat_K_,
              [num_254_S_,
               Math.cos(t_e_),
               0,
               -Math.sin(t_e_),
               0,
               0,
               1,
               0,
               0,
               Math.sin(t_e_),
               0,
               Math.cos(t_e_),
               0,
               0,
               0,
               0,
               1])));
        gl_b_.clear(gl_b_.DEPTH_BUFFER_BIT|gl_b_.COLOR_BUFFER_BIT);
        gl_b_.drawArrays(gl_b_.TRIANGLES,0,pos_p_.length/3|0);
        check_error_bM_(gl_b_);
        var now_u_=get_time_r_(0),x_w_=now_u_-last_draw_s_[1];
        if(0===q_c_[1])
         {var cell_f_=[];
          caml_update_dummy_c$_(cell_f_,[0,x_w_,cell_f_]);
          q_c_[1]=1;
          q_c_[2]=cell_f_}
        else
         {var tail_g_=q_c_[2],cell_h_=[0,x_w_,tail_g_[2]];
          q_c_[1]=q_c_[1]+1|0;
          tail_g_[2]=cell_h_;
          q_c_[2]=cell_h_}
        last_draw_s_[1]=now_u_;
        if(50<_cg_(q_c_))_cf_(q_c_);
        var _D_=_cg_(q_c_),accu_y_=0;
        if(0===q_c_[1])
         var _A_=accu_y_;
        else
         {var tail_j_=q_c_[2],accu_k_=accu_y_,cell_d_=tail_j_[2];
          for(;;)
           {var accu_z_=accu_k_+cell_d_[1];
            if(cell_d_!==tail_j_)
             {var accu_k_=accu_z_,cell_d_=cell_d_[2];continue}
            var _A_=accu_z_;
            break}}
        n_q_.data=
        caml_call_gen1_i_(_v_(_gQ_),1/_A_*_D_*num_1e3_a$_).toString();
        var
         match_l_=task_bA_(0),
         t_m_=match_l_[1],
         id_n_=[0,0],
         d_E_=0.02,
         w_B_=match_l_[2];
        function wait_o_(d_a_,param_b_)
         {var
           match_c_=
            num_2147483_bT_<d_a_
             ?[0,overflow_limit_fd_,d_a_-num_2147483_bT_]
             :[0,d_a_,0],
           remain_d_=match_c_[2],
           step_e_=match_c_[1],
           cb_f_=
            remain_d_==0
             ?function(_a_){return wakeup_aP_(w_B_,_a_)}
             :function(_a_){return wait_o_(remain_d_,_a_)};
          id_n_[1]=
          [0,
           window_x_.setTimeout
            (caml_js_wrap_callback_a1_(cb_f_),step_e_*num_1e3_a$_)];
          return 0}
        wait_o_(d_E_,0);
        on_cancel_bC_
         (t_m_,
          function(param_a_)
           {var _b_=id_n_[1];return _b_?window_x_.clearTimeout(_b_[1]):0});
        return _aR_(t_m_,f_t_)}
      return f_t_(0)}
    window_x_.onload=
    handler_aT_
     (function(param_a_)
       {function f_aO_(exn_a_)
         {var _b_=_bs_(exn_a_);return caml_call_gen1_i_(error_ah_(_gR_),_b_)}
        try
         {var
           _bp_=
            function(frame_a_)
             {var a_Z_=str_array_cJ_(frame_a_[4].toString().split("\n"));
              window_x_.arr=a_Z_;
              var vertex_B_=[0,0],norm_C_=[0,0],face_D_=[0,0],i_A_=0;
              for(;;)
               {var match_Y_=_ag_(array_get_m_(a_Z_,i_A_));
                if(match_Y_)
                 {var
                   match_K_=
                    string_match_cR_
                     (line_regexp_gA_,new MlWrappedString_p_(match_Y_[1]),0);
                  if(match_K_)
                   {var
                     res_h_=match_K_[1],
                     match_e_=
                      _F_
                       (function(res_h_)
                          {return function(_a_){return matched_group_cS_(res_h_,_a_)}}
                         (res_h_),
                        _gF_);
                    if(match_e_)
                     {var _L_=match_e_[1];
                      if(_L_)
                       {var _i_=_L_[1];
                        if(caml_string_notequal_s_(_i_,_gG_))
                         if(caml_string_notequal_s_(_i_,_gH_))
                          if(caml_string_notequal_s_(_i_,_gI_))
                           var _b_=0;
                          else
                           {var _j_=match_e_[2];
                            if(_j_)
                             {var _M_=_j_[1];
                              if(_M_)
                               {var _l_=_j_[2];
                                if(_l_)
                                 {var _N_=_l_[1];
                                  if(_N_)
                                   {var _n_=_l_[2];
                                    if(_n_)
                                     {var _O_=_n_[1];
                                      if(_O_)
                                       if(_n_[2])
                                        var _b_=0;
                                       else
                                        var
                                         match_d_=
                                          [0,
                                           [1,
                                            [0,
                                             caml_float_of_string_ai_(_M_[1]),
                                             caml_float_of_string_ai_(_N_[1]),
                                             caml_float_of_string_ai_(_O_[1])]]],
                                         _b_=1;
                                      else
                                       var _b_=0}
                                    else
                                     var _b_=0}
                                  else
                                   var _b_=0}
                                else
                                 var _b_=0}
                              else
                               var _b_=0}
                            else
                             var _b_=0}
                         else
                          {var _o_=match_e_[2];
                           if(_o_)
                            {var _P_=_o_[1];
                             if(_P_)
                              {var _q_=_o_[2];
                               if(_q_)
                                {var _Q_=_q_[1];
                                 if(_Q_)
                                  {var _r_=_q_[2];
                                   if(_r_)
                                    {var _R_=_r_[1];
                                     if(_R_)
                                      if(_r_[2])
                                       var _b_=0;
                                      else
                                       var
                                        match_d_=
                                         [0,
                                          [0,
                                           [0,
                                            caml_float_of_string_ai_(_P_[1]),
                                            caml_float_of_string_ai_(_Q_[1]),
                                            caml_float_of_string_ai_(_R_[1])]]],
                                        _b_=1;
                                     else
                                      var _b_=0}
                                   else
                                    var _b_=0}
                                 else
                                  var _b_=0}
                               else
                                var _b_=0}
                             else
                              var _b_=0}
                           else
                            var _b_=0}
                        else
                         {var _t_=match_e_[2];
                          if(_t_)
                           {var _S_=_t_[1];
                            if(_S_)
                             {var _u_=_t_[2];
                              if(_u_)
                               {var _T_=_u_[1];
                                if(_T_)
                                 {var _v_=_u_[2];
                                  if(_v_)
                                   {var _U_=_v_[1];
                                    if(_U_)
                                     if(_v_[2])
                                      var _b_=0;
                                     else
                                      {var
                                        r_w_=
                                         _F_
                                          (read_coord_couple_gD_,[0,_S_[1],[0,_T_[1],[0,_U_[1],0]]]);
                                       if(r_w_)
                                        {var _V_=r_w_[1];
                                         if(_V_)
                                          {var _y_=r_w_[2];
                                           if(_y_)
                                            {var _W_=_y_[1];
                                             if(_W_)
                                              {var _z_=_y_[2];
                                               if(_z_)
                                                {var _X_=_z_[1];
                                                 if(_X_)
                                                  if(_z_[2])
                                                   var _c_=1;
                                                  else
                                                   var match_d_=[0,[2,[0,_V_[1],_W_[1],_X_[1]]]],_b_=1,_c_=0;
                                                 else
                                                  var _c_=1}
                                               else
                                                var _c_=1}
                                             else
                                              var _c_=1}
                                           else
                                            var _c_=1}
                                         else
                                          var _c_=1}
                                       else
                                        var _c_=1;
                                       if(_c_)var match_d_=0,_b_=1}
                                    else
                                     var _b_=0}
                                  else
                                   var _b_=0}
                                else
                                 var _b_=0}
                              else
                               var _b_=0}
                            else
                             var _b_=0}
                          else
                           var _b_=0}}
                      else
                       var _b_=0}
                    else
                     var _b_=0;
                    if(!_b_)var match_d_=0}
                  else
                   var match_d_=0;
                  if(match_d_)
                   {var _f_=match_d_[1];
                    switch(_f_[0])
                     {case 1:
                       var match_G_=_f_[1];
                       norm_C_[1]=
                       [0,[0,match_G_[1],match_G_[2],match_G_[3]],norm_C_[1]];
                       break;
                      case 2:
                       var match_H_=_f_[1];
                       face_D_[1]=
                       [0,[0,match_H_[1],match_H_[2],match_H_[3]],face_D_[1]];
                       break;
                      default:
                       var match_E_=_f_[1];
                       vertex_B_[1]=
                       [0,[0,match_E_[1],match_E_[2],match_E_[3]],vertex_B_[1]]}}
                  var i_A_=i_A_+1|0;
                  continue}
                var
                 face_g_=_bi_(_af_(face_D_[1])),
                 norm_I_=_bi_(_af_(norm_C_[1])),
                 vertex_J_=_bi_(_af_(vertex_B_[1])),
                 vertex___=
                  _aK_
                   (face_g_.length-1,
                    function(i_a_)
                     {var
                       _b_=caml_array_get_k_(face_g_,i_a_),
                       match_c_=caml_array_get_k_(vertex_J_,_b_[1][1]-1|0),
                       match_d_=caml_array_get_k_(vertex_J_,_b_[2][1]-1|0),
                       match_e_=caml_array_get_k_(vertex_J_,_b_[3][1]-1|0);
                      return [0,
                              match_c_[1],
                              [0,
                               match_c_[2],
                               [0,
                                match_c_[3],
                                [0,
                                 match_d_[1],
                                 [0,
                                  match_d_[2],
                                  [0,
                                   match_d_[3],
                                   [0,match_e_[1],[0,match_e_[2],[0,match_e_[3],0]]]]]]]]]}),
                 norm_$_=
                  _aK_
                   (face_g_.length-1,
                    function(i_a_)
                     {var
                       _b_=caml_array_get_k_(face_g_,i_a_),
                       match_c_=caml_array_get_k_(norm_I_,_b_[1][2]-1|0),
                       match_d_=caml_array_get_k_(norm_I_,_b_[2][2]-1|0),
                       match_e_=caml_array_get_k_(norm_I_,_b_[3][2]-1|0);
                      return [0,
                              match_c_[1],
                              [0,
                               match_c_[2],
                               [0,
                                match_c_[3],
                                [0,
                                 match_d_[1],
                                 [0,
                                  match_d_[2],
                                  [0,
                                   match_d_[3],
                                   [0,match_e_[1],[0,match_e_[2],[0,match_e_[3],0]]]]]]]]]}),
                 vertex_aa_=float32array_az_(concat_c2_(vertex___));
                return [0,vertex_aa_,float32array_az_(concat_c2_(norm_$_))]}},
           override_mime_type_aH_=0,
           upload_progress_aI_=0,
           progress_aJ_=0,
           _aL_=0,
           form_arg_aM_=0,
           _aN_=0,
           post_args_r_=0,
           content_type_M_=0,
           opt_bq_=0,
           headers_bd_=0?opt_bq_[1]:0,
           get_args_be_=_aN_?_aN_[1]:0,
           check_headers_bf_=_aL_?_aL_[1]:function(param_a_,_b_){return 1};
          if(form_arg_aM_)
           {var form_arg_aj_=form_arg_aM_[1];
            if(post_args_r_)
             {var post_args_bg_=post_args_r_[1];
              _ao_
               (function(param_a_)
                 {return _cY_(form_arg_aj_,[0,param_a_[1],param_a_[2]])},
                post_args_bg_)}
            var form_arg_c_=[0,form_arg_aj_]}
          else
           if(post_args_r_)
            {var
              post_args_bo_=post_args_r_[1],
              match_W_=_ag_(_aw_(formData_f8_)),
              contents_aG_=
               match_W_
                ?[0,808620462,new (match_W_[1])()]
                :[0,num_891486873_bb_,[0,0]];
             _ao_
              (function(param_a_)
                {return _cY_(contents_aG_,[0,param_a_[1],param_a_[2]])},
               post_args_bo_);
             var form_arg_c_=[0,contents_aG_]}
           else
            var form_arg_c_=0;
          if(form_arg_c_)
           {var _ak_=form_arg_c_[1];
            if(content_type_M_)
             var _al_=[0,_gm_,content_type_M_,num_126925477_bc_];
            else
             {if(num_891486873_bb_<=_ak_[1])
               {var yes_z_=0,no_y_=0,param_j_=_ak_[2][1];
                for(;;)
                 {if(param_j_)
                   {var
                     l_P_=param_j_[2],
                     x_A_=param_j_[1],
                     _aU_=num_781515420_a4_<=x_A_[2][1]?0:1;
                    if(_aU_){var yes_z_=[0,x_A_,yes_z_],param_j_=l_P_;continue}
                    var no_y_=[0,x_A_,no_y_],param_j_=l_P_;
                    continue}
                  var files_aV_=_af_(no_y_);
                  _af_(yes_z_);
                  if(files_aV_)
                   var
                    nine_digits___=
                     function(param_a_)
                      {return string_of_int_ae_(a593685be_e2_.random()*1e9|0)},
                    _a8_=nine_digits___(0),
                    boundary_$_=_h_(_f__,_h_(nine_digits___(0),_a8_)),
                    _aE_=
                     [0,_gp_,[0,_h_(_go_,boundary_$_)],[0,164354597,boundary_$_]];
                  else
                   var _aE_=_gq_;
                  var _aF_=_aE_;
                  break}}
              else
               var _aF_=_gr_;
              var _al_=_aF_}
            var match_o_=_al_}
          else
           var match_o_=[0,_gs_,content_type_M_,num_126925477_bc_];
          var
           post_encode_am_=match_o_[3],
           content_type_an_=match_o_[2],
           s_V_=caml_js_from_byte_string_f_(s_c3_),
           method_bh_=match_o_[1],
           _a0_=
            function(handle_a_)
             {var
               res_c_=f_bI_(handle_a_),
               match_b_=
                caml_js_to_byte_string_N_
                 (_B_(array_get_m_(res_c_,1),interrupt_Z_).toLowerCase());
              if(caml_string_notequal_s_(match_b_,_fn_))
               if(caml_string_notequal_s_(match_b_,_fo_))
                {if(caml_string_notequal_s_(match_b_,_fp_))
                  if(caml_string_notequal_s_(match_b_,_fq_))
                   {if(caml_string_notequal_s_(match_b_,_fr_))
                     if(caml_string_notequal_s_(match_b_,_fs_))
                      var _d_=1,_g_=0;
                     else
                      var _g_=1;
                    else
                     var _g_=1;
                    if(_g_)var ssl_e_=1,_d_=2}
                  else
                   var _d_=0;
                 else
                  var _d_=0;
                 switch(_d_)
                  {case 1:var _h_=0;break;
                   case 2:var _h_=1;break;
                   default:var ssl_e_=0,_h_=1}
                 if(_h_)
                  {var
                    path_str_i_=
                     urldecode_js_string_string_I_
                      (_B_(array_get_m_(res_c_,5),interrupt_Z_)),
                    _l_=
                     function(param_a_){return caml_js_from_byte_string_f_(_fC_)},
                    _n_=
                     urldecode_js_string_string_I_
                      (_B_(array_get_m_(res_c_,9),_l_)),
                    _o_=
                     function(param_a_){return caml_js_from_byte_string_f_(_fD_)},
                    _p_=
                     decode_arguments_js_string_bK_
                      (_B_(array_get_m_(res_c_,7),_o_)),
                    _q_=path_of_path_string_ax_(path_str_i_),
                    _r_=
                     function(param_a_){return caml_js_from_byte_string_f_(_fE_)},
                    s_j_=
                     caml_js_to_byte_string_N_(_B_(array_get_m_(res_c_,4),_r_)),
                    _t_=
                     caml_string_notequal_s_(s_j_,_fB_)
                      ?caml_int_of_string_aY_(s_j_)
                      :ssl_e_?num_443_de_:80,
                    url_k_=
                     [0,
                      urldecode_js_string_string_I_
                       (_B_(array_get_m_(res_c_,2),interrupt_Z_)),
                      _t_,
                      _q_,
                      path_str_i_,
                      _p_,
                      _n_],
                    _u_=ssl_e_?[1,url_k_]:[0,url_k_];
                   return [0,_u_]}}
              throw [0,Not_an_http_protocol_fm_]},
           _a2_=
            function(param_a_)
             {function _b_(handle_a_)
               {var
                 res_b_=f_bI_(handle_a_),
                 path_str_c_=
                  urldecode_js_string_string_I_
                   (_B_(array_get_m_(res_b_,2),interrupt_Z_));
                function _d_(param_a_)
                 {return caml_js_from_byte_string_f_(_fF_)}
                var
                 _e_=
                  caml_js_to_byte_string_N_(_B_(array_get_m_(res_b_,6),_d_));
                function _g_(param_a_)
                 {return caml_js_from_byte_string_f_(_fG_)}
                var
                 _h_=
                  decode_arguments_js_string_bK_
                   (_B_(array_get_m_(res_b_,4),_g_));
                return [0,
                        [2,
                         [0,path_of_path_string_ax_(path_str_c_),path_str_c_,_h_,_e_]]]}
              function _c_(param_a_){return 0}
              return _aS_(file_re_fA_.exec(s_V_),_c_,_b_)},
           match_T_=_aS_(url_re_fy_.exec(s_V_),_a2_,_a0_);
          if(match_T_)
           {var _E_=match_T_[1];
            switch(_E_[0])
             {case 0:
               var url_aa_=_E_[1],_ab_=url_aa_.slice(),_a$_=url_aa_[5];
               _ab_[5]=0;
               var match_q_=[0,string_of_url_cX_([0,_ab_]),_a$_],_v_=1;
               break;
              case 1:
               var url_ac_=_E_[1],_ad_=url_ac_.slice(),_ba_=url_ac_[5];
               _ad_[5]=0;
               var match_q_=[0,string_of_url_cX_([1,_ad_]),_ba_],_v_=1;
               break;
              default:var _v_=0}}
          else
           var _v_=0;
          if(!_v_)var match_q_=[0,s_c3_,0];
          var
           url_ap_=match_q_[1],
           l_ar_=_ca_(match_q_[2],get_args_be_),
           url_as_=
            l_ar_?_h_(url_ap_,_h_(_gn_,encode_arguments_aW_(l_ar_))):url_ap_,
           match_at_=task_bA_(0),
           w_av_=match_at_[2],
           res_ay_=match_at_[1];
          try
           {var _a7_=new XMLHttpRequest(),req_b_=_a7_}
          catch(_f_)
           {try
             {var _a6_=new (_bL_(0))("Msxml2.XMLHTTP"),req_b_=_a6_}
            catch(_f_)
             {try
               {var _a5_=new (_bL_(0))("Msxml3.XMLHTTP"),req_b_=_a5_}
              catch(_f_)
               {try
                 {var _a3_=new (_bL_(0))("Microsoft.XMLHTTP")}
                catch(_f_){throw [0,_t_,_f9_]}
                var req_b_=_a3_}}}
          if(override_mime_type_aH_)
           req_b_.overrideMimeType(override_mime_type_aH_[1].toString());
          req_b_.open(method_bh_.toString(),url_as_.toString(),true_Y_);
          if(content_type_an_)
           req_b_.setRequestHeader
            ("Content-type",content_type_an_[1].toString());
          _ao_
           (function(param_a_)
             {return req_b_.setRequestHeader
                      (param_a_[1].toString(),param_a_[2].toString())},
            headers_bd_);
          var
           headers_J_=
            function(s_a_)
             {function _c_(v_a_){return [0,new MlWrappedString_p_(v_a_)]}
              function _d_(param_a_){return 0}
              return _aS_
                      (req_b_.getResponseHeader(caml_js_from_byte_string_f_(s_a_)),
                       _d_,
                       _c_)},
           checked_aA_=[0,0],
           do_check_headers_K_=
            function(param_a_)
             {var
               _c_=
                checked_aA_[1]
                 ?0
                 :caml_call_gen2_l_
                    (check_headers_bf_,req_b_.status,headers_J_)
                   ?0
                   :(wakeup_result_cE_
                      (w_av_,[1,[0,_gl_,[0,req_b_.status,headers_J_]]]),
                     req_b_.abort(),
                     1);
              checked_aA_[1]=1;
              return 0};
          req_b_.onreadystatechange=
          caml_js_wrap_callback_a1_
           (function(param_a_)
             {switch(req_b_.readyState)
               {case 2:if(!onIE_cN_)return do_check_headers_K_(0);break;
                case 3:if(onIE_cN_)return do_check_headers_K_(0);break;
                case 4:
                 do_check_headers_K_(0);
                 var
                  _c_=
                   function(param_a_)
                    {var match_c_=_bG_(req_b_.responseXML);
                     if(match_c_)
                      {var doc_d_=match_c_[1];
                       return _O_(doc_d_.documentElement)===no_handler_g_
                               ?0
                               :[0,doc_d_]}
                     return 0};
                 return wakeup_aP_
                         (w_av_,
                          [0,
                           url_as_,
                           req_b_.status,
                           headers_J_,
                           new MlWrappedString_p_(req_b_.responseText),
                           _c_])
                }
              return 0});
          if(progress_aJ_)
           {var progress_bj_=progress_aJ_[1];
            req_b_.onprogress=
            handler_aT_
             (function(e_a_)
               {caml_call_gen2_l_(progress_bj_,e_a_.loaded,e_a_.total);
                return true_Y_})}
          var upload_aB_=req_b_.upload;
          if(upload_aB_!==undefined_G_)
           if(upload_progress_aI_)
            {var upload_progress_bk_=upload_progress_aI_[1];
             upload_aB_.onprogress=
             handler_aT_
              (function(e_a_)
                {caml_call_gen2_l_(upload_progress_bk_,e_a_.loaded,e_a_.total);
                 return true_Y_})}
          if(form_arg_c_)
           {var _L_=form_arg_c_[1];
            if(num_891486873_bb_<=_L_[1])
             {var l_aC_=_L_[2];
              if(typeof post_encode_am_===str_number_u_)
               {var _bl_=l_aC_[1];
                req_b_.send
                 (_O_
                   (_aq_
                      (_gj_,
                       _F_
                        (function(param_a_)
                          {var _b_=param_a_[2],_c_=param_a_[1];
                           if(num_781515420_a4_<=_b_[1])
                            {var
                              _d_=
                               _h_
                                (_gh_,urlencode_n_(0,new MlWrappedString_p_(_b_[2].name)));
                             return _h_(urlencode_n_(0,_c_),_d_)}
                           var
                            _e_=
                             _h_(_gi_,urlencode_n_(0,new MlWrappedString_p_(_b_[2])));
                           return _h_(urlencode_n_(0,_c_),_e_)},
                         _bl_)).toString
                     ()))}
              else
               {var
                 boundary_aD_=post_encode_am_[2],
                 _bm_=
                  function(data_a_)
                   {var data_c_=_O_(data_a_.join(str_d_));
                    return _au_(req_b_.sendAsBinary)
                            ?req_b_.sendAsBinary(data_c_)
                            :req_b_.send(data_c_)},
                 _bn_=l_aC_[1],
                 b_e_=new abd4b70c7_cI_(),
                 _a__=
                  function(param_a_)
                   {b_e_.push(_h_(_ga_,_h_(boundary_aD_,_f$_)).toString());
                    return b_e_};
                _bD_
                 (_bD_
                   (iter_s_cH_
                     (function(v_a_)
                       {b_e_.push(_h_(_gc_,_h_(boundary_aD_,_gb_)).toString());
                        var _i_=v_a_[2],_n_=v_a_[1];
                        if(num_781515420_a4_<=_i_[1])
                         {var
                           value_b_=_i_[2],
                           _r_=
                            function(file_a_)
                             {var
                               match_c_=_ag_(value_b_.name),
                               _g_="Content-Type: application/octet-stream\r\n",
                               _i_='"\r\n';
                              if(match_c_)
                               var _f_=match_c_[1];
                              else
                               var
                                match_d_=_ag_(value_b_.fileName),
                                _f_=match_d_?match_d_[1]:failwith_U_(_e$_);
                              b_e_.push(_h_(_ge_,_h_(_n_,_gd_)).toString(),_f_,_i_,_g_);
                              b_e_.push(str_a9_,file_a_,str_a9_);
                              return return_bx_(0)},
                           match_l_=_ag_(_aw_(_fa_)),
                           kind_d_=-1041425454;
                          if(match_l_)
                           {var
                             reader_c_=new (match_l_[1])(),
                             match_j_=task_bA_(0),
                             res_k_=match_j_[1],
                             w_p_=match_j_[2];
                            reader_c_.onloadend=
                            handler_aT_
                             (function(param_a_)
                               {if(2===reader_c_.readyState)
                                 {var
                                   e_b_=reader_c_.result,
                                   _e_=
                                    caml_equal_gX_(typeof e_b_,"string")?_O_(e_b_):no_handler_g_,
                                   match_d_=_bG_(_e_);
                                  if(!match_d_)throw [0,_t_,_fb_];
                                  wakeup_aP_(w_p_,match_d_[1])}
                                return false_H_});
                            on_cancel_bC_
                             (res_k_,function(param_a_){return reader_c_.abort()});
                            if(typeof kind_d_===str_number_u_)
                             if(num_550809787_dd_===kind_d_)
                              reader_c_.readAsDataURL(value_b_);
                             else
                              if(num_936573133_db_<=kind_d_)
                               reader_c_.readAsText(value_b_);
                              else
                               reader_c_.readAsBinaryString(value_b_);
                            else
                             reader_c_.readAsText(value_b_,kind_d_[2]);
                            var _o_=res_k_}
                          else
                           {var fail_f_=function(param_a_){return failwith_U_(_fc_)};
                            if(typeof kind_d_===str_number_u_)
                             var
                              _m_=
                               num_550809787_dd_===kind_d_
                                ?_au_(value_b_.getAsDataURL)
                                  ?value_b_.getAsDataURL()
                                  :fail_f_(0)
                                :num_936573133_db_<=kind_d_
                                  ?_au_(value_b_.getAsText)
                                    ?value_b_.getAsText("utf8")
                                    :fail_f_(0)
                                  :_au_(value_b_.getAsBinary)
                                    ?value_b_.getAsBinary()
                                    :fail_f_(0);
                            else
                             var
                              e_q_=kind_d_[2],
                              _m_=
                               _au_(value_b_.getAsText)?value_b_.getAsText(e_q_):fail_f_(0);
                            var _o_=return_bx_(_m_)}
                          return _aR_(_o_,_r_)}
                        var value_s_=_i_[2];
                        b_e_.push
                         (_h_(_gg_,_h_(_n_,_gf_)).toString(),value_s_,str_a9_);
                        return return_bx_(0)},
                      _bn_),
                    _a__),
                  _bm_)}}
            else
             req_b_.send(_L_[2])}
          else
           req_b_.send(no_handler_g_);
          on_cancel_bC_(res_ay_,function(param_a_){return req_b_.abort()});
          var _br_=_aR_(_bD_(res_ay_,_bp_),start_gK_),_Q_=_br_}
        catch(exn_f_)
         {exn_f_=caml_wrap_exception_C_(exn_f_);var _Q_=fail_by_(exn_f_)}
        var t_R_=repr_X_(_Q_),_D_=t_R_[1];
        switch(_D_[0])
         {case 1:f_aO_(_D_[1]);break;
          case 2:
           var
            sleeper_aX_=_D_[1],
            res_S_=temp_bz_(t_R_),
            data_aZ_=current_data_w_[1];
           add_immutable_waiter_bB_
            (sleeper_aX_,
             function(state_a_)
              {switch(state_a_[0])
                {case 0:return fast_connect_aQ_(res_S_,state_a_);
                 case 1:
                  var exn_c_=state_a_[1];
                  current_data_w_[1]=data_aZ_;
                  try
                   {var _d_=f_aO_(exn_c_),_b_=_d_}
                  catch(exn_f_)
                   {exn_f_=caml_wrap_exception_C_(exn_f_);
                    var _b_=fail_by_(exn_f_)}
                  return connect_cG_(res_S_,_b_);
                 default:throw [0,_t_,_eX_]}});
           break;
          case 3:throw [0,_t_,_eY_]
          }
        return true_Y_});
    do_at_exit_bh_(0);
    return}
  (this));
