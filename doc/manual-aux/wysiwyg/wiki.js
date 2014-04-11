// This program was compiled from OCaml by js_of_ocaml 2.00dev+git-f7cce66
(function(joo_global_object_t_)
   {"use strict";
    var
     num_125_a9_=125,
     num_123_ba_=123,
     num_254_cD_=254,
     num_255_J_=255,
     num_108_cC_=108,
     str_az_='"',
     num_16777215_s_=16777215,
     str_b$_="\\\\",
     num_250_ct_=250,
     str_jsError_cj_="jsError",
     num_2147483_a8_=2147483,
     str_input_b__="input",
     num_115_ab_=115,
     num_102_bf_=102,
     num_120_a$_=120,
     str_formatblock_aw_="formatblock",
     num_117_a__=117,
     str_e_="",
     str_cs_="^",
     num_100_aD_=100,
     str_0_l_="0",
     str_a_cB_="</a>",
     num_103_be_=103,
     str_p_cz_="p",
     str_fd_cA_="fd ",
     str_inserthtml_aF_="inserthtml",
     str_cr_="[[",
     num_1e3_cq_=1e3,
     str_src_core_lwt_ml_aE_="src/core/lwt.ml",
     str_main_ml_bd_="main.ml",
     str_x_ci_="x",
     str_Q_=".",
     num_65535_aB_=65535,
     str_aC_="+",
     str_g_ch_="g",
     str_f_a7_="f",
     str_b9_="\n\n",
     num_105_L_=105,
     str_d_cg_="%d",
     num_88_cy_=-88,
     num_110_aa_=110,
     num_785140586_cp_=785140586,
     str_ay_="'",
     str_int_of_string_ax_="int_of_string",
     num_32_co_=-32,
     num_982028505_cf_=982028505,
     num_111_bc_=111,
     str_cols_b8_="cols",
     str_z_=" ",
     str_e_$_="e",
     str_I_cn_="I",
     str_h1_cx_="h1",
     str_h3_ce_="h3",
     str_cw_="]]",
     str_K_="-",
     num_48_P_=-48,
     str_nan_cm_="nan",
     num_116_a6_=116,
     str_a_href_cd_='<a href="',
     str_12g_cl_="%.12g",
     str_file_already_abr_bg_=" : file already exists",
     str_rows_cc_="rows",
     str_bb_="/",
     str_5px_b7_="5px",
     num_114_aA_=114,
     str_ck_="#",
     num_101_cv_=101,
     str_h2_cb_="h2",
     str_index_out_of_bounds_cu_="index out of bounds",
     str_B_ca_="B",
     str_number_A_="number";
    function caml_raise_with_arg_cR_(tag_a_,arg_b_){throw [0,tag_a_,arg_b_]}
    function js_print_stderr_bl_(s_a_)
     {if(s_a_.charCodeAt(s_a_.length-1)==10)s_a_=s_a_.substr(0,s_a_.length-1);
      var v_b_=joo_global_object_t_.console;
      v_b_&&v_b_.error&&v_b_.error(s_a_)}
    var caml_global_data_f_=[0];
    function caml_str_repeat_ad_(n_a_,s_b_)
     {if(!n_a_)return str_e_;
      if(n_a_&1)return caml_str_repeat_ad_(n_a_-1,s_b_)+s_b_;
      var r_c_=caml_str_repeat_ad_(n_a_>>1,s_b_);
      return r_c_+r_c_}
    function MlString_d_(param_a_)
     {if(param_a_!=null)
       {this.bytes=this.fullBytes=param_a_;this.last=this.len=param_a_.length}}
    function mlstring_bound_error_cS_()
     {caml_raise_with_arg_cR_
       (caml_global_data_f_[4],new MlString_d_(str_index_out_of_bounds_cu_))}
    MlString_d_.prototype=
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
        {js_print_stderr_bl_
          ('MlString.toJsString: wrong encoding for "%s" ',a_a_);
         return a_a_}},
     toBytes:
     function()
      {if(this.string!=null)
        try
         {var b_a_=unescape(encodeURIComponent(this.string))}
        catch(e_f_)
         {js_print_stderr_bl_
           ('MlString.toBytes: wrong encoding for "%s" ',this.string);
          var b_a_=this.string}
       else
        {var b_a_=str_e_,a_c_=this.array,l_d_=a_c_.length;
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
        {this.bytes=b_a_+=caml_str_repeat_ad_(this.len-this.last,"\0");
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
       if(i_a_<0||i_a_>=this.len)mlstring_bound_error_cS_();
       return this.get(i_a_)},
     set:
     function(i_a_,c_b_)
      {var a_c_=this.array;
       if(!a_c_)
        {if(this.last==i_a_)
          {this.bytes+=String.fromCharCode(c_b_&num_255_J_);
           this.last++;
           return 0}
         a_c_=this.toArray()}
       else
        if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;
       a_c_[i_a_]=c_b_&num_255_J_;
       return 0},
     safeSet:
     function(i_a_,c_b_)
      {if(this.len==null)this.toBytes();
       if(i_a_<0||i_a_>=this.len)mlstring_bound_error_cS_();
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
    function MlWrappedString_g_(s_a_){this.string=s_a_}
    MlWrappedString_g_.prototype=new MlString_d_();
    function caml_raise_with_string_bk_(tag_a_,msg_b_)
     {caml_raise_with_arg_cR_(tag_a_,new MlWrappedString_g_(msg_b_))}
    function caml_invalid_argument_R_(msg_a_)
     {caml_raise_with_string_bk_(caml_global_data_f_[4],msg_a_)}
    function caml_array_bound_error_cF_()
     {caml_invalid_argument_R_(str_index_out_of_bounds_cu_)}
    function caml_array_get_fo_(array_a_,index_b_)
     {if(index_b_<0||index_b_>=array_a_.length-1)caml_array_bound_error_cF_();
      return array_a_[index_b_+1]}
    function caml_array_set_fp_(array_a_,index_b_,newval_c_)
     {if(index_b_<0||index_b_>=array_a_.length-1)caml_array_bound_error_cF_();
      array_a_[index_b_+1]=newval_c_;
      return 0}
    function caml_blit_string_cG_(s1_a_,i1_b_,s2_c_,i2_d_,len_e_)
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
    function caml_call_gen_B_(f_c_,args_b_)
     {if(f_c_.fun)return caml_call_gen_B_(f_c_.fun,args_b_);
      var n_a_=f_c_.length,d_d_=n_a_-args_b_.length;
      if(d_d_==0)
       return f_c_.apply(null,args_b_);
      else
       if(d_d_<0)
        return caml_call_gen_B_
                (f_c_.apply(null,args_b_.slice(0,n_a_)),args_b_.slice(n_a_));
       else
        return function(x_a_)
         {return caml_call_gen_B_(f_c_,args_b_.concat([x_a_]))}}
    function caml_classify_float_fq_(x_a_)
     {if(isFinite(x_a_))
       {if(Math.abs(x_a_)>=2.22507385850720138e-308)return 0;
        if(x_a_!=0)return 1;
        return 2}
      return isNaN(x_a_)?4:3}
    function caml_convert_raw_backtrace_fr_(){return 0}
    function MlMakeString_cE_(l_a_){this.bytes=str_e_;this.len=l_a_}
    MlMakeString_cE_.prototype=new MlString_d_();
    function caml_create_string_cI_(len_a_)
     {if(len_a_<0)caml_invalid_argument_R_("String.create");
      return new MlMakeString_cE_(len_a_)}
    function caml_fill_string_ft_(s_a_,i_b_,l_c_,c_d_)
     {s_a_.fill(i_b_,l_c_,c_d_)}
    function caml_parse_format_bj_(fmt_a_)
     {fmt_a_=fmt_a_.toString();
      var len_e_=fmt_a_.length;
      if(len_e_>31)caml_invalid_argument_R_("format_int: format too long");
      var
       f_b_=
        {justify:str_aC_,
         signstyle:str_K_,
         filler:str_z_,
         alternate:false,
         base:0,
         signedconv:false,
         width:0,
         uppercase:false,
         sign:1,
         prec:-1,
         conv:str_f_a7_};
      for(var i_d_=0;i_d_<len_e_;i_d_++)
       {var c_c_=fmt_a_.charAt(i_d_);
        switch(c_c_)
         {case str_K_:f_b_.justify=str_K_;break;
          case str_aC_:
          case str_z_:f_b_.signstyle=c_c_;break;
          case str_0_l_:f_b_.filler=str_0_l_;break;
          case str_ck_:f_b_.alternate=true;break;
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
          case str_Q_:
           f_b_.prec=0;
           i_d_++;
           while(c_c_=fmt_a_.charCodeAt(i_d_)-48,c_c_>=0&&c_c_<=9)
            {f_b_.prec=f_b_.prec*10+c_c_;i_d_++}
           i_d_--;
          case "d":
          case "i":f_b_.signedconv=true;
          case "u":f_b_.base=10;break;
          case str_x_ci_:f_b_.base=16;break;
          case "X":f_b_.base=16;f_b_.uppercase=true;break;
          case "o":f_b_.base=8;break;
          case str_e_$_:
          case str_f_a7_:
          case str_g_ch_:f_b_.signedconv=true;f_b_.conv=c_c_;break;
          case "E":
          case "F":
          case "G":
           f_b_.signedconv=true;
           f_b_.uppercase=true;
           f_b_.conv=c_c_.toLowerCase();
           break
          }}
      return f_b_}
    function caml_finish_formatting_bh_(f_a_,rawbuffer_b_)
     {if(f_a_.uppercase)rawbuffer_b_=rawbuffer_b_.toUpperCase();
      var len_f_=rawbuffer_b_.length;
      if(f_a_.signedconv&&(f_a_.sign<0||f_a_.signstyle!=str_K_))len_f_++;
      if(f_a_.alternate){if(f_a_.base==8)len_f_+=1;if(f_a_.base==16)len_f_+=2}
      var buffer_c_=str_e_;
      if(f_a_.justify==str_aC_&&f_a_.filler==str_z_)
       for(var i_d_=len_f_;i_d_<f_a_.width;i_d_++)buffer_c_+=str_z_;
      if(f_a_.signedconv)
       if(f_a_.sign<0)
        buffer_c_+=str_K_;
       else
        if(f_a_.signstyle!=str_K_)buffer_c_+=f_a_.signstyle;
      if(f_a_.alternate&&f_a_.base==8)buffer_c_+=str_0_l_;
      if(f_a_.alternate&&f_a_.base==16)buffer_c_+="0x";
      if(f_a_.justify==str_aC_&&f_a_.filler==str_0_l_)
       for(var i_d_=len_f_;i_d_<f_a_.width;i_d_++)buffer_c_+=str_0_l_;
      buffer_c_+=rawbuffer_b_;
      if(f_a_.justify==str_K_)
       for(var i_d_=len_f_;i_d_<f_a_.width;i_d_++)buffer_c_+=str_z_;
      return new MlWrappedString_g_(buffer_c_)}
    function caml_format_float_fu_(fmt_a_,x_b_)
     {var
       s_c_,
       f_f_=caml_parse_format_bj_(fmt_a_),
       prec_e_=f_f_.prec<0?6:f_f_.prec;
      if(x_b_<0){f_f_.sign=-1;x_b_=-x_b_}
      if(isNaN(x_b_))
       {s_c_=str_nan_cm_;f_f_.filler=str_z_}
      else
       if(!isFinite(x_b_))
        {s_c_="inf";f_f_.filler=str_z_}
       else
        switch(f_f_.conv)
         {case str_e_$_:
           var s_c_=x_b_.toExponential(prec_e_),i_d_=s_c_.length;
           if(s_c_.charAt(i_d_-3)==str_e_$_)
            s_c_=s_c_.slice(0,i_d_-1)+str_0_l_+s_c_.slice(i_d_-1);
           break;
          case str_f_a7_:s_c_=x_b_.toFixed(prec_e_);break;
          case str_g_ch_:
           prec_e_=prec_e_?prec_e_:1;
           s_c_=x_b_.toExponential(prec_e_-1);
           var j_i_=s_c_.indexOf(str_e_$_),exp_h_=+s_c_.slice(j_i_+1);
           if(exp_h_<-4||x_b_.toFixed(0).length>prec_e_)
            {var i_d_=j_i_-1;
             while(s_c_.charAt(i_d_)==str_0_l_)i_d_--;
             if(s_c_.charAt(i_d_)==str_Q_)i_d_--;
             s_c_=s_c_.slice(0,i_d_+1)+s_c_.slice(j_i_);
             i_d_=s_c_.length;
             if(s_c_.charAt(i_d_-3)==str_e_$_)
              s_c_=s_c_.slice(0,i_d_-1)+str_0_l_+s_c_.slice(i_d_-1);
             break}
           else
            {var p_g_=prec_e_;
             if(exp_h_<0)
              {p_g_-=exp_h_+1;s_c_=x_b_.toFixed(p_g_)}
             else
              while(s_c_=x_b_.toFixed(p_g_),s_c_.length>prec_e_+1)p_g_--;
             if(p_g_)
              {var i_d_=s_c_.length-1;
               while(s_c_.charAt(i_d_)==str_0_l_)i_d_--;
               if(s_c_.charAt(i_d_)==str_Q_)i_d_--;
               s_c_=s_c_.slice(0,i_d_+1)}}
           break
          }
      return caml_finish_formatting_bh_(f_f_,s_c_)}
    function caml_format_int_fv_(fmt_a_,i_b_)
     {if(fmt_a_.toString()==str_d_cg_)
       return new MlWrappedString_g_(str_e_+i_b_);
      var f_c_=caml_parse_format_bj_(fmt_a_);
      if(i_b_<0)if(f_c_.signedconv){f_c_.sign=-1;i_b_=-i_b_}else i_b_>>>=0;
      var s_d_=i_b_.toString(f_c_.base);
      if(f_c_.prec>=0)
       {f_c_.filler=str_z_;
        var n_f_=f_c_.prec-s_d_.length;
        if(n_f_>0)s_d_=caml_str_repeat_ad_(n_f_,str_0_l_)+s_d_}
      return caml_finish_formatting_bh_(f_c_,s_d_)}
    function caml_get_exception_raw_backtrace_fx_(){return 0}
    function caml_int64_compare_fz_(x_a_,y_b_)
     {var x3_c_=x_a_[3]<<16,y3_d_=y_b_[3]<<16;
      if(x3_c_>y3_d_)return 1;
      if(x3_c_<y3_d_)return -1;
      if(x_a_[2]>y_b_[2])return 1;
      if(x_a_[2]<y_b_[2])return -1;
      if(x_a_[1]>y_b_[1])return 1;
      if(x_a_[1]<y_b_[1])return -1;
      return 0}
    function caml_int_compare_fJ_(a_a_,b_b_)
     {if(a_a_<b_b_)return -1;if(a_a_==b_b_)return 0;return 1}
    function caml_compare_val_cH_(a_a_,b_b_,total_c_)
     {var stack_f_=[];
      for(;;)
       {if(!(total_c_&&a_a_===b_b_))
         if(a_a_ instanceof MlString_d_)
          if(b_b_ instanceof MlString_d_)
           {if(a_a_!==b_b_)
             {var x_e_=a_a_.compare(b_b_);if(x_e_!=0)return x_e_}}
          else
           return 1;
         else
          if(a_a_ instanceof Array&&a_a_[0]===(a_a_[0]|0))
           {var ta_g_=a_a_[0];
            if(ta_g_===num_254_cD_)ta_g_=0;
            if(ta_g_===num_250_ct_)
             {a_a_=a_a_[1];continue}
            else
             if(b_b_ instanceof Array&&b_b_[0]===(b_b_[0]|0))
              {var tb_h_=b_b_[0];
               if(tb_h_===num_254_cD_)tb_h_=0;
               if(tb_h_===num_250_ct_)
                {b_b_=b_b_[1];continue}
               else
                if(ta_g_!=tb_h_)
                 return ta_g_<tb_h_?-1:1;
                else
                 switch(ta_g_)
                  {case 248:
                    var x_e_=caml_int_compare_fJ_(a_a_[2],b_b_[2]);
                    if(x_e_!=0)return x_e_;
                    break;
                   case 251:caml_invalid_argument_R_("equal: abstract value");
                   case num_255_J_:
                    var x_e_=caml_int64_compare_fz_(a_a_,b_b_);
                    if(x_e_!=0)return x_e_;
                    break;
                   default:
                    if(a_a_.length!=b_b_.length)
                     return a_a_.length<b_b_.length?-1:1;
                    if(a_a_.length>1)stack_f_.push(a_a_,b_b_,1)}}
             else
              return 1}
          else
           if
            (b_b_ instanceof MlString_d_||
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
        if(stack_f_.length==0)return 0;
        var i_i_=stack_f_.pop();
        b_b_=stack_f_.pop();
        a_a_=stack_f_.pop();
        if(i_i_+1<a_a_.length)stack_f_.push(a_a_,b_b_,i_i_+1);
        a_a_=a_a_[i_i_];
        b_b_=b_b_[i_i_]}}
    function caml_greaterequal_fy_(x_a_,y_b_)
     {return +(caml_compare_val_cH_(x_a_,y_b_,false)>=0)}
    function caml_int64_is_zero_fC_(x_a_){return (x_a_[3]|x_a_[2]|x_a_[1])==0}
    function caml_int64_of_int32_fF_(x_a_)
     {return [num_255_J_,
              x_a_&num_16777215_s_,
              x_a_>>24&num_16777215_s_,
              x_a_>>31&num_65535_aB_]}
    function caml_int64_sub_fG_(x_a_,y_b_)
     {var
       z1_c_=x_a_[1]-y_b_[1],
       z2_d_=x_a_[2]-y_b_[2]+(z1_c_>>24),
       z3_e_=x_a_[3]-y_b_[3]+(z2_d_>>24);
      return [num_255_J_,
              z1_c_&num_16777215_s_,
              z2_d_&num_16777215_s_,
              z3_e_&num_65535_aB_]}
    function caml_int64_ucompare_cK_(x_a_,y_b_)
     {if(x_a_[3]>y_b_[3])return 1;
      if(x_a_[3]<y_b_[3])return -1;
      if(x_a_[2]>y_b_[2])return 1;
      if(x_a_[2]<y_b_[2])return -1;
      if(x_a_[1]>y_b_[1])return 1;
      if(x_a_[1]<y_b_[1])return -1;
      return 0}
    function caml_int64_lsl1_cJ_(x_a_)
     {x_a_[3]=x_a_[3]<<1|x_a_[2]>>23;
      x_a_[2]=(x_a_[2]<<1|x_a_[1]>>23)&num_16777215_s_;
      x_a_[1]=x_a_[1]<<1&num_16777215_s_}
    function caml_int64_lsr1_fD_(x_a_)
     {x_a_[1]=(x_a_[1]>>>1|x_a_[2]<<23)&num_16777215_s_;
      x_a_[2]=(x_a_[2]>>>1|x_a_[3]<<23)&num_16777215_s_;
      x_a_[3]=x_a_[3]>>>1}
    function caml_int64_udivmod_fI_(x_a_,y_b_)
     {var
       offset_e_=0,
       modulus_d_=x_a_.slice(),
       divisor_c_=y_b_.slice(),
       quotient_f_=[num_255_J_,0,0,0];
      while(caml_int64_ucompare_cK_(modulus_d_,divisor_c_)>0)
       {offset_e_++;caml_int64_lsl1_cJ_(divisor_c_)}
      while(offset_e_>=0)
       {offset_e_--;
        caml_int64_lsl1_cJ_(quotient_f_);
        if(caml_int64_ucompare_cK_(modulus_d_,divisor_c_)>=0)
         {quotient_f_[1]++;
          modulus_d_=caml_int64_sub_fG_(modulus_d_,divisor_c_)}
        caml_int64_lsr1_fD_(divisor_c_)}
      return [0,quotient_f_,modulus_d_]}
    function caml_int64_to_int32_fH_(x_a_){return x_a_[1]|x_a_[2]<<24}
    function caml_int64_is_negative_fB_(x_a_){return x_a_[3]<<16<0}
    function caml_int64_neg_fE_(x_a_)
     {var
       y1_b_=-x_a_[1],
       y2_c_=-x_a_[2]+(y1_b_>>24),
       y3_d_=-x_a_[3]+(y2_c_>>24);
      return [num_255_J_,
              y1_b_&num_16777215_s_,
              y2_c_&num_16777215_s_,
              y3_d_&num_65535_aB_]}
    function caml_int64_format_fA_(fmt_a_,x_b_)
     {var f_c_=caml_parse_format_bj_(fmt_a_);
      if(f_c_.signedconv&&caml_int64_is_negative_fB_(x_b_))
       {f_c_.sign=-1;x_b_=caml_int64_neg_fE_(x_b_)}
      var
       buffer_d_=str_e_,
       wbase_i_=caml_int64_of_int32_fF_(f_c_.base),
       cvtbl_h_="0123456789abcdef";
      do
       {var p_g_=caml_int64_udivmod_fI_(x_b_,wbase_i_);
        x_b_=p_g_[1];
        buffer_d_=cvtbl_h_.charAt(caml_int64_to_int32_fH_(p_g_[2]))+buffer_d_}
      while
       (!caml_int64_is_zero_fC_(x_b_));
      if(f_c_.prec>=0)
       {f_c_.filler=str_z_;
        var n_f_=f_c_.prec-buffer_d_.length;
        if(n_f_>0)buffer_d_=caml_str_repeat_ad_(n_f_,str_0_l_)+buffer_d_}
      return caml_finish_formatting_bh_(f_c_,buffer_d_)}
    function caml_parse_sign_and_base_fZ_(s_a_)
     {var i_b_=0,base_c_=10,sign_d_=s_a_.get(0)==45?(i_b_++,-1):1;
      if(s_a_.get(i_b_)==48)
       switch(s_a_.get(i_b_+1))
        {case num_120_a$_:
         case 88:base_c_=16;i_b_+=2;break;
         case num_111_bc_:
         case 79:base_c_=8;i_b_+=2;break;
         case 98:
         case 66:base_c_=2;i_b_+=2;break
         }
      return [i_b_,sign_d_,base_c_]}
    function caml_parse_digit_cP_(c_a_)
     {if(c_a_>=48&&c_a_<=57)return c_a_-48;
      if(c_a_>=65&&c_a_<=90)return c_a_-55;
      if(c_a_>=97&&c_a_<=122)return c_a_-87;
      return -1}
    function caml_failwith_aG_(msg_a_)
     {caml_raise_with_string_bk_(caml_global_data_f_[3],msg_a_)}
    function caml_int_of_string_fK_(s_a_)
     {var
       r_g_=caml_parse_sign_and_base_fZ_(s_a_),
       i_f_=r_g_[0],
       sign_h_=r_g_[1],
       base_d_=r_g_[2],
       threshold_i_=-1>>>0,
       c_e_=s_a_.get(i_f_),
       d_c_=caml_parse_digit_cP_(c_e_);
      if(d_c_<0||d_c_>=base_d_)caml_failwith_aG_(str_int_of_string_ax_);
      var res_b_=d_c_;
      for(;;)
       {i_f_++;
        c_e_=s_a_.get(i_f_);
        if(c_e_==95)continue;
        d_c_=caml_parse_digit_cP_(c_e_);
        if(d_c_<0||d_c_>=base_d_)break;
        res_b_=base_d_*res_b_+d_c_;
        if(res_b_>threshold_i_)caml_failwith_aG_(str_int_of_string_ax_)}
      if(i_f_!=s_a_.getLen())caml_failwith_aG_(str_int_of_string_ax_);
      res_b_=sign_h_*res_b_;
      if(base_d_==10&&(res_b_|0)!=res_b_)
       caml_failwith_aG_(str_int_of_string_ax_);
      return res_b_|0}
    function caml_is_printable_fL_(c_a_){return +(c_a_>31&&c_a_<127)}
    function caml_js_get_console_fM_()
     {var
       c_b_=joo_global_object_t_.console?joo_global_object_t_.console:{},
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
    var caml_js_regexps_aH_={amp:/&/g,lt:/</g,quot:/\"/g,all:/[&<\"]/};
    function caml_js_html_escape_fN_(s_a_)
     {if(!caml_js_regexps_aH_.all.test(s_a_))return s_a_;
      return s_a_.replace(caml_js_regexps_aH_.amp,"&amp;").replace
               (caml_js_regexps_aH_.lt,"&lt;").replace
              (caml_js_regexps_aH_.quot,"&quot;")}
    function caml_js_wrap_callback_fO_(f_a_)
     {var toArray_c_=Array.prototype.slice;
      return function()
       {var args_b_=arguments.length>0?toArray_c_.call(arguments):[undefined];
        return caml_call_gen_B_(f_a_,args_b_)}}
    function caml_make_vect_fP_(len_a_,init_b_)
     {var b_d_=[0];
      for(var i_c_=1;i_c_<=len_a_;i_c_++)b_d_[i_c_]=init_b_;
      return b_d_}
    function caml_raise_sys_error_n_(msg_a_)
     {caml_raise_with_string_bk_(caml_global_data_f_[2],msg_a_)}
    function caml_ml_flush_cL_(oc_a_)
     {if(!oc_a_.opened)
       caml_raise_sys_error_n_("Cannot flush a closed channel");
      if(oc_a_.buffer==str_e_)return 0;
      if(oc_a_.output)
       switch(oc_a_.output.length)
        {case 2:oc_a_.output(oc_a_,oc_a_.buffer);break;
         default:oc_a_.output(oc_a_.buffer)}
      oc_a_.buffer=str_e_}
    function caml_raise_no_such_file_cQ_(name_a_)
     {name_a_=name_a_ instanceof MlString_d_?name_a_.toString():name_a_;
      caml_raise_sys_error_n_(name_a_+": No such file or directory")}
    var caml_current_dir_fs_=str_bb_;
    function caml_make_path_aI_(name_a_)
     {name_a_=name_a_ instanceof MlString_d_?name_a_.toString():name_a_;
      if(name_a_.charCodeAt(0)!=47)name_a_=caml_current_dir_fs_+name_a_;
      var comp_f_=name_a_.split(str_bb_),ncomp_b_=[];
      for(var i_c_=0;i_c_<comp_f_.length;i_c_++)
       switch(comp_f_[i_c_])
        {case "..":if(ncomp_b_.length>1)ncomp_b_.pop();break;
         case str_Q_:
         case str_e_:if(ncomp_b_.length==0)ncomp_b_.push(str_e_);break;
         default:ncomp_b_.push(comp_f_[i_c_]);break}
      ncomp_b_.orig=name_a_;
      return ncomp_b_}
    function MlDir_M_(){this.content={}}
    MlDir_M_.prototype=
    {exists:function(name_a_){return this.content[name_a_]?1:0},
     mk:function(name_a_,c_b_){this.content[name_a_]=c_b_},
     get:function(name_a_){return this.content[name_a_]},
     list:
     function()
      {var a_a_=[];for(var n_b_ in this.content)a_a_.push(n_b_);return a_a_},
     remove:function(name_a_){delete this.content[name_a_]}};
    var caml_root_dir_aK_=new MlDir_M_();
    caml_root_dir_aK_.mk(str_e_,new MlDir_M_());
    function caml_fs_content_bi_(path_a_)
     {var dir_b_=caml_root_dir_aK_;
      for(var i_c_=0;i_c_<path_a_.length;i_c_++)
       {if(!(dir_b_.exists&&dir_b_.exists(path_a_[i_c_])))
         caml_raise_no_such_file_cQ_(path_a_.orig);
        dir_b_=dir_b_.get(path_a_[i_c_])}
      return dir_b_}
    function caml_sys_is_directory_f8_(name_a_)
     {var
       path_c_=caml_make_path_aI_(name_a_),
       dir_b_=caml_fs_content_bi_(path_c_);
      return dir_b_ instanceof MlDir_M_?1:0}
    function MlFile_ac_(content_a_){this.data=content_a_}
    MlFile_ac_.prototype=
    {content:function(){return this.data},
     truncate:function(){this.data.length=0}};
    function caml_fs_register_fw_(name_a_,content_b_)
     {var path_f_=caml_make_path_aI_(name_a_),dir_c_=caml_root_dir_aK_;
      for(var i_g_=0;i_g_<path_f_.length-1;i_g_++)
       {var d_e_=path_f_[i_g_];
        if(!dir_c_.exists(d_e_))dir_c_.mk(d_e_,new MlDir_M_());
        dir_c_=dir_c_.get(d_e_);
        if(!(dir_c_ instanceof MlDir_M_))
         caml_raise_sys_error_n_(path_f_.orig+str_file_already_abr_bg_)}
      var d_e_=path_f_[path_f_.length-1];
      if(dir_c_.exists(d_e_))
       caml_raise_sys_error_n_(path_f_.orig+str_file_already_abr_bg_);
      if(content_b_ instanceof MlDir_M_)
       dir_c_.mk(d_e_,content_b_);
      else
       if(content_b_ instanceof MlFile_ac_)
        dir_c_.mk(d_e_,content_b_);
       else
        if(content_b_ instanceof MlString_d_)
         dir_c_.mk(d_e_,new MlFile_ac_(content_b_.getArray()));
        else
         if(content_b_ instanceof Array)
          dir_c_.mk(d_e_,new MlFile_ac_(content_b_));
         else
          if(content_b_.toString)
           dir_c_.mk
            (d_e_,
             new MlFile_ac_(new MlString_d_(content_b_.toString()).getArray()));
          else
           caml_invalid_argument_R_("caml_fs_register")}
    function caml_sys_file_exists_f7_(name_a_)
     {var
       dir_b_=caml_root_dir_aK_,
       path_d_=caml_make_path_aI_(name_a_),
       auto_load_e_;
      for(var i_c_=0;i_c_<path_d_.length;i_c_++)
       {if(dir_b_.auto)auto_load_e_=dir_b_.auto;
        if(!(dir_b_.exists&&dir_b_.exists(path_d_[i_c_])))
         return auto_load_e_?auto_load_e_(path_d_.join(str_bb_)):0;
        dir_b_=dir_b_.get(path_d_[i_c_])}
      return 1}
    function caml_sys_open_internal_ae_(idx_a_,v_b_,flags_c_)
     {if(caml_global_data_f_.fds===undefined)
       caml_global_data_f_.fds=new Array();
      flags_c_=flags_c_?flags_c_:{};
      var data_d_={};
      data_d_.array=v_b_;
      data_d_.offset=flags_c_.append?data_d_.array.length:0;
      data_d_.flags=flags_c_;
      caml_global_data_f_.fds[idx_a_]=data_d_;
      caml_global_data_f_.fd_last_idx=idx_a_;
      return idx_a_}
    function caml_sys_open_ge_(name_a_,flags_b_,perms_c_)
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
      var name2_g_=name_a_.toString(),path_i_=caml_make_path_aI_(name_a_);
      if(f_d_.rdonly&&f_d_.wronly)
       caml_raise_sys_error_n_
        (name2_g_+" : flags Open_rdonly and Open_wronly are not compatible");
      if(f_d_.text&&f_d_.binary)
       caml_raise_sys_error_n_
        (name2_g_+" : flags Open_text and Open_binary are not compatible");
      if(caml_sys_file_exists_f7_(name_a_))
       {if(caml_sys_is_directory_f8_(name_a_))
         caml_raise_sys_error_n_(name2_g_+" : is a directory");
        if(f_d_.create&&f_d_.excl)
         caml_raise_sys_error_n_(name2_g_+str_file_already_abr_bg_);
        var
         idx_h_=
          caml_global_data_f_.fd_last_idx?caml_global_data_f_.fd_last_idx:0,
         file_e_=caml_fs_content_bi_(path_i_);
        if(f_d_.truncate)file_e_.truncate();
        return caml_sys_open_internal_ae_(idx_h_+1,file_e_.content(),f_d_)}
      else
       if(f_d_.create)
        {var
          idx_h_=
           caml_global_data_f_.fd_last_idx?caml_global_data_f_.fd_last_idx:0;
         caml_fs_register_fw_(name_a_,[]);
         var file_e_=caml_fs_content_bi_(path_i_);
         return caml_sys_open_internal_ae_(idx_h_+1,file_e_.content(),f_d_)}
       else
        caml_raise_no_such_file_cQ_(name_a_)}
    caml_sys_open_internal_ae_(0,[]);
    caml_sys_open_internal_ae_(1,[]);
    caml_sys_open_internal_ae_(2,[]);
    function caml_ml_open_descriptor_in_fQ_(fd_a_)
     {var data_b_=caml_global_data_f_.fds[fd_a_];
      if(data_b_.flags.wronly)
       caml_raise_sys_error_n_(str_fd_cA_+fd_a_+" is writeonly");
      return {data:data_b_,fd:fd_a_,opened:true}}
    function js_print_stdout_gb_(s_a_)
     {if(s_a_.charCodeAt(s_a_.length-1)==10)s_a_=s_a_.substr(0,s_a_.length-1);
      var v_b_=joo_global_object_t_.console;
      v_b_&&v_b_.log&&v_b_.log(s_a_)}
    var caml_ml_out_channels_aJ_=new Array();
    function caml_std_output_f2_(chan_a_,s_b_)
     {var str_f_=new MlString_d_(s_b_),slen_e_=str_f_.getLen();
      for(var i_c_=0;i_c_<slen_e_;i_c_++)
       chan_a_.data.array[chan_a_.data.offset+i_c_]=str_f_.get(i_c_);
      chan_a_.data.offset+=slen_e_;
      return 0}
    function caml_ml_open_descriptor_out_fR_(fd_a_)
     {var output_b_;
      switch(fd_a_)
       {case 1:output_b_=js_print_stdout_gb_;break;
        case 2:output_b_=js_print_stderr_bl_;break;
        default:output_b_=caml_std_output_f2_}
      var data_d_=caml_global_data_f_.fds[fd_a_];
      if(data_d_.flags.rdonly)
       caml_raise_sys_error_n_(str_fd_cA_+fd_a_+" is readonly");
      var
       channel_c_=
        {data:data_d_,fd:fd_a_,opened:true,buffer:str_e_,output:output_b_};
      caml_ml_out_channels_aJ_[channel_c_.fd]=channel_c_;
      return channel_c_}
    function caml_ml_out_channels_list_fS_()
     {var l_a_=0;
      for(var c_b_ in caml_ml_out_channels_aJ_)
       if(caml_ml_out_channels_aJ_[c_b_].opened)
        l_a_=[0,caml_ml_out_channels_aJ_[c_b_],l_a_];
      return l_a_}
    function caml_ml_output_cM_(oc_a_,buffer_b_,offset_c_,len_d_)
     {if(!oc_a_.opened)
       caml_raise_sys_error_n_("Cannot output to a closed channel");
      var string_f_;
      if(offset_c_==0&&buffer_b_.getLen()==len_d_)
       string_f_=buffer_b_;
      else
       {string_f_=caml_create_string_cI_(len_d_);
        caml_blit_string_cG_(buffer_b_,offset_c_,string_f_,0,len_d_)}
      var
       jsstring_e_=string_f_.toString(),
       id_g_=jsstring_e_.lastIndexOf("\n");
      if(id_g_<0)
       oc_a_.buffer+=jsstring_e_;
      else
       {oc_a_.buffer+=jsstring_e_.substr(0,id_g_+1);
        caml_ml_flush_cL_(oc_a_);
        oc_a_.buffer+=jsstring_e_.substr(id_g_+1)}}
    function caml_new_string_cO_(x_a_){return new MlString_d_(x_a_)}
    function caml_ml_output_char_fT_(oc_a_,c_b_)
     {var s_c_=caml_new_string_cO_(String.fromCharCode(c_b_));
      caml_ml_output_cM_(oc_a_,s_c_,0,1)}
    if(!Math.imul)
     Math.imul=
     function(x_a_,y_b_)
      {return ((x_a_>>16)*y_b_<<16)+(x_a_&num_65535_aB_)*y_b_|0};
    var caml_mul_fU_=Math.imul;
    function caml_notequal_fW_(x_a_,y_b_)
     {return +(caml_compare_val_cH_(x_a_,y_b_,false)!=0)}
    function caml_obj_is_block_fX_(x_a_){return +(x_a_ instanceof Array)}
    function caml_obj_tag_fY_(x_a_)
     {return x_a_ instanceof Array?x_a_[0]:num_1e3_cq_}
    function caml_register_global_f0_(n_a_,v_b_)
     {caml_global_data_f_[n_a_+1]=v_b_}
    var caml_named_values_cN_={};
    function caml_register_named_value_f1_(nm_a_,v_b_)
     {caml_named_values_cN_[nm_a_.toString()]=v_b_;return 0}
    function caml_string_equal_f3_(s1_a_,s2_b_)
     {var b1_c_=s1_a_.fullBytes,b2_d_=s2_b_.fullBytes;
      if(b1_c_!=null&&b2_d_!=null)return b1_c_==b2_d_?1:0;
      return s1_a_.getFullBytes()==s2_b_.getFullBytes()?1:0}
    function caml_string_notequal_f4_(s1_a_,s2_b_)
     {return 1-caml_string_equal_f3_(s1_a_,s2_b_)}
    function caml_sys_const_word_size_f5_(){return 32}
    function caml_sys_exit_f6_()
     {caml_invalid_argument_R_("Function 'exit' not implemented")}
    function caml_trampoline_f9_(res_a_)
     {var c_b_=1;
      while(res_a_&&res_a_.joo_tramp)
       {res_a_=res_a_.joo_tramp.apply(null,res_a_.joo_args);c_b_++}
      return res_a_}
    function caml_trampoline_return_f__(f_a_,args_b_)
     {return {joo_tramp:f_a_,joo_args:args_b_}}
    function caml_update_dummy_f$_(x_a_,y_b_)
     {if(typeof y_b_==="function"){x_a_.fun=y_b_;return 0}
      if(y_b_.fun){x_a_.fun=y_b_.fun;return 0}
      var i_c_=y_b_.length;
      while(i_c_--)x_a_[i_c_]=y_b_[i_c_];
      return 0}
    function caml_named_value_fV_(nm_a_){return caml_named_values_cN_[nm_a_]}
    function caml_wrap_exception_ga_(e_a_)
     {if(e_a_ instanceof Array)return e_a_;
      if
       (joo_global_object_t_.RangeError&&
        e_a_ instanceof joo_global_object_t_.RangeError&&
        e_a_.message&&
        e_a_.message.match(/maximum call stack/i))
       return [0,caml_global_data_f_[9]];
      if
       (joo_global_object_t_.InternalError&&
        e_a_ instanceof joo_global_object_t_.InternalError&&
        e_a_.message&&
        e_a_.message.match(/too much recursion/i))
       return [0,caml_global_data_f_[9]];
      if(e_a_ instanceof joo_global_object_t_.Error)
       return [0,caml_named_value_fV_(str_jsError_cj_),e_a_];
      return [0,caml_global_data_f_[3],new MlWrappedString_g_(String(e_a_))]}
    var
     caml_array_get_a0_=caml_array_get_fo_,
     caml_array_set_j_=caml_array_set_fp_,
     caml_blit_string_G_=caml_blit_string_cG_,
     caml_create_string_w_=caml_create_string_cI_,
     caml_format_float_a1_=caml_format_float_fu_,
     caml_format_int_as_=caml_format_int_fv_,
     caml_is_printable_a2_=caml_is_printable_fL_,
     caml_js_html_escape_b6_=caml_js_html_escape_fN_,
     caml_js_wrap_callback_a5_=caml_js_wrap_callback_fO_,
     caml_make_vect_H_=caml_make_vect_fP_,
     caml_ml_flush_b1_=caml_ml_flush_cL_,
     caml_ml_open_descriptor_out_b0_=caml_ml_open_descriptor_out_fR_,
     caml_ml_output_char_b3_=caml_ml_output_char_fT_,
     caml_mul_b4_=caml_mul_fU_,
     caml_new_string_b_=caml_new_string_cO_,
     caml_obj_tag_b5_=caml_obj_tag_fY_,
     caml_register_global_a_=caml_register_global_f0_,
     caml_register_named_value_b2_=caml_register_named_value_f1_,
     caml_string_notequal_c_=caml_string_notequal_f4_,
     caml_trampoline_av_=caml_trampoline_f9_,
     caml_trampoline_return_y_=caml_trampoline_return_f__,
     caml_wrap_exception_at_=caml_wrap_exception_ga_;
    function caml_call_gen1_h_(fun_a_,var0_b_)
     {return fun_a_.length==1
              ?fun_a_(var0_b_)
              :caml_call_gen_B_(fun_a_,[var0_b_])}
    function caml_call_gen2_o_(fun_a_,var0_b_,var1_c_)
     {return fun_a_.length==2
              ?fun_a_(var0_b_,var1_c_)
              :caml_call_gen_B_(fun_a_,[var0_b_,var1_c_])}
    function caml_call_gen3_k_(fun_a_,var0_b_,var1_c_,var2_d_)
     {return fun_a_.length==3
              ?fun_a_(var0_b_,var1_c_,var2_d_)
              :caml_call_gen_B_(fun_a_,[var0_b_,var1_c_,var2_d_])}
    function caml_call_gen5_au_
     (fun_a_,var0_b_,var1_c_,var2_d_,var3_e_,var4_f_)
     {return fun_a_.length==5
              ?fun_a_(var0_b_,var1_c_,var2_d_,var3_e_,var4_f_)
              :caml_call_gen_B_
                (fun_a_,[var0_b_,var1_c_,var2_d_,var3_e_,var4_f_])}
    var
     _aM_=[0,caml_new_string_b_("Failure")],
     _bm_=[0,caml_new_string_b_("Invalid_argument")],
     _bH_=[0,caml_new_string_b_("Match_failure")],
     _bG_=[0,caml_new_string_b_("Stack_overflow")],
     _u_=[0,caml_new_string_b_("Assert_failure")],
     _bI_=[0,caml_new_string_b_("Undefined_recursive_module")],
     _aS_=caml_new_string_b_('File "%s", line %d, characters %d-%d: %s');
    caml_register_global_a_(11,_bI_);
    caml_register_global_a_(8,_bG_);
    caml_register_global_a_(7,_bH_);
    caml_register_global_a_(6,[0,caml_new_string_b_("Not_found")]);
    caml_register_global_a_(5,[0,caml_new_string_b_("Division_by_zero")]);
    caml_register_global_a_(4,[0,caml_new_string_b_("End_of_file")]);
    caml_register_global_a_(3,_bm_);
    caml_register_global_a_(2,_aM_);
    caml_register_global_a_(1,[0,caml_new_string_b_("Sys_error")]);
    var
     _dF_=[0,caml_new_string_b_("Out_of_memory")],
     _cW_=caml_new_string_b_(str_12g_cl_),
     _cV_=caml_new_string_b_(str_Q_),
     _cT_=caml_new_string_b_("true"),
     _cU_=caml_new_string_b_("false"),
     _cX_=caml_new_string_b_("Pervasives.do_at_exit"),
     _c1_=caml_new_string_b_("\\b"),
     _c2_=caml_new_string_b_("\\t"),
     _c3_=caml_new_string_b_("\\n"),
     _c4_=caml_new_string_b_("\\r"),
     _c0_=caml_new_string_b_(str_b$_),
     _cZ_=caml_new_string_b_("\\'"),
     _c7_=caml_new_string_b_(str_e_),
     _c6_=caml_new_string_b_("String.blit"),
     _c5_=caml_new_string_b_("String.sub"),
     _c8_=caml_new_string_b_("Queue.Empty"),
     _c__=caml_new_string_b_("Buffer.add: cannot grow buffer"),
     _do_=caml_new_string_b_(str_e_),
     _dp_=caml_new_string_b_(str_e_),
     _ds_=caml_new_string_b_(str_12g_cl_),
     _dt_=caml_new_string_b_(str_az_),
     _du_=caml_new_string_b_(str_az_),
     _dq_=caml_new_string_b_(str_ay_),
     _dr_=caml_new_string_b_(str_ay_),
     _dn_=caml_new_string_b_(str_nan_cm_),
     _dl_=caml_new_string_b_("neg_infinity"),
     _dm_=caml_new_string_b_("infinity"),
     _dk_=caml_new_string_b_(str_Q_),
     _dj_=caml_new_string_b_("printf: bad positional specification (0)."),
     _di_=caml_new_string_b_("%_"),
     _dh_=[0,caml_new_string_b_("printf.ml"),143,8],
     _df_=caml_new_string_b_(str_ay_),
     _dg_=caml_new_string_b_("Printf: premature end of format string '"),
     _db_=caml_new_string_b_(str_ay_),
     _dc_=caml_new_string_b_(" in format string '"),
     _dd_=caml_new_string_b_(", at char number "),
     _de_=caml_new_string_b_("Printf: bad conversion %"),
     _c$_=caml_new_string_b_("Sformat.index_of_int: negative argument "),
     _dz_=caml_new_string_b_(str_e_),
     _dA_=caml_new_string_b_(", %s%s"),
     _dR_=[1,1],
     _dS_=caml_new_string_b_("%s\n"),
     _dT_=
      caml_new_string_b_
       ("(Program not linked with -g, cannot print stack backtrace)\n"),
     _dL_=caml_new_string_b_("Raised at"),
     _dO_=caml_new_string_b_("Re-raised at"),
     _dP_=caml_new_string_b_("Raised by primitive operation at"),
     _dQ_=caml_new_string_b_("Called from"),
     _dM_=caml_new_string_b_('%s file "%s", line %d, characters %d-%d'),
     _dN_=caml_new_string_b_("%s unknown location"),
     _dG_=caml_new_string_b_("Out of memory"),
     _dH_=caml_new_string_b_("Stack overflow"),
     _dI_=caml_new_string_b_("Pattern matching failed"),
     _dJ_=caml_new_string_b_("Assertion failed"),
     _dK_=caml_new_string_b_("Undefined recursive module"),
     _dB_=caml_new_string_b_("(%s%s)"),
     _dC_=caml_new_string_b_(str_e_),
     _dD_=caml_new_string_b_(str_e_),
     _dE_=caml_new_string_b_("(%s)"),
     _dy_=caml_new_string_b_(str_d_cg_),
     _dw_=caml_new_string_b_("%S"),
     _dx_=caml_new_string_b_("_"),
     _d1_=[0,caml_new_string_b_(str_src_core_lwt_ml_aE_),648,20],
     _d2_=[0,caml_new_string_b_(str_src_core_lwt_ml_aE_),651,8],
     _d0_=[0,caml_new_string_b_(str_src_core_lwt_ml_aE_),498,8],
     _dZ_=[0,caml_new_string_b_(str_src_core_lwt_ml_aE_),487,9],
     _dY_=caml_new_string_b_("Lwt.wakeup_result"),
     _dV_=caml_new_string_b_("Fatal error: exception "),
     _dU_=caml_new_string_b_("Lwt.Canceled"),
     _d7_=caml_new_string_b_("Js.Error"),
     name_d8_=caml_new_string_b_(str_jsError_cj_),
     _ea_=caml_new_string_b_("iframe"),
     _d$_=caml_new_string_b_("br"),
     _d__=caml_new_string_b_("textarea"),
     _d9_=caml_new_string_b_(str_input_b__),
     _ed_=caml_new_string_b_("Exception during Lwt.async: "),
     _eL_=caml_new_string_b_("^error_in_anchor^"),
     _eA_=[0,caml_new_string_b_(str_main_ml_bd_),72,76],
     _eB_=caml_new_string_b_("global"),
     _eC_=caml_new_string_b_("wiki"),
     _eD_=caml_new_string_b_("^error2_in_anchor^"),
     _eE_=[0,caml_new_string_b_(str_cw_),0],
     _eF_=caml_new_string_b_(str_cr_),
     _eG_=caml_new_string_b_(str_e_),
     _eH_=[0,caml_new_string_b_(str_cw_),0],
     _eI_=caml_new_string_b_("|"),
     _eJ_=caml_new_string_b_(str_cr_),
     _eK_=caml_new_string_b_(str_e_),
     _ez_=[0,caml_new_string_b_(str_main_ml_bd_),67,35],
     _eg_=caml_new_string_b_("#text"),
     _eh_=caml_new_string_b_("A"),
     _ei_=caml_new_string_b_(str_B_ca_),
     _ej_=caml_new_string_b_("BR"),
     _ek_=caml_new_string_b_("DIV"),
     _el_=caml_new_string_b_("H1"),
     _em_=caml_new_string_b_("H2"),
     _en_=caml_new_string_b_("H3"),
     _eo_=caml_new_string_b_("HR"),
     _ep_=caml_new_string_b_(str_I_cn_),
     _eq_=caml_new_string_b_("P"),
     _er_=caml_new_string_b_(str_cs_),
     _es_=caml_new_string_b_(str_cs_),
     _et_=caml_new_string_b_(str_b9_),
     _eu_=[0,caml_new_string_b_("//")],
     _ev_=caml_new_string_b_("----"),
     _ex_=caml_new_string_b_(str_b$_),
     _ey_=[0,caml_new_string_b_("**")],
     _ew_=caml_new_string_b_(str_b9_),
     _ef_=caml_new_string_b_(str_e_),
     _fh_=caml_new_string_b_("lololo"),
     _fi_=caml_new_string_b_("Enter a wikipage"),
     _fj_=caml_new_string_b_(str_e_),
     _fk_=[0,caml_new_string_b_(str_a_cB_),0],
     _fl_=caml_new_string_b_('" wysitype="wiki">'),
     _fm_=caml_new_string_b_(str_a_href_cd_),
     _e9_=caml_new_string_b_("http://google.ru"),
     _e__=caml_new_string_b_("Enter a link"),
     _e$_=caml_new_string_b_("desc"),
     _fa_=caml_new_string_b_("Enter description"),
     _fb_=[0,caml_new_string_b_(str_a_cB_),0],
     _fc_=caml_new_string_b_('" wysitype="global">'),
     _fd_=caml_new_string_b_(str_a_href_cd_),
     _fe_=caml_new_string_b_(str_e_),
     _eN_=caml_new_string_b_("inserthorizontalrule"),
     _eO_=caml_new_string_b_("hr"),
     _eP_=caml_new_string_b_("removeformat"),
     _eQ_=caml_new_string_b_("remove format"),
     _eR_=caml_new_string_b_("bold"),
     _eS_=caml_new_string_b_(str_B_ca_),
     _eT_=caml_new_string_b_("italic"),
     _eU_=caml_new_string_b_(str_I_cn_),
     _eV_=caml_new_string_b_(str_formatblock_aw_),
     _eW_=caml_new_string_b_(str_p_cz_),
     _eX_=[0,[0,caml_new_string_b_(str_p_cz_)]],
     _eY_=caml_new_string_b_(str_formatblock_aw_),
     _eZ_=caml_new_string_b_(str_h1_cx_),
     _e0_=[0,[0,caml_new_string_b_(str_h1_cx_)]],
     _e1_=caml_new_string_b_(str_formatblock_aw_),
     _e2_=caml_new_string_b_(str_h2_cb_),
     _e3_=[0,[0,caml_new_string_b_(str_h2_cb_)]],
     _e4_=caml_new_string_b_(str_formatblock_aw_),
     _e5_=caml_new_string_b_(str_h3_ce_),
     _e6_=[0,[0,caml_new_string_b_(str_h3_ce_)]],
     _e7_=caml_new_string_b_(str_inserthtml_aF_),
     _e8_=caml_new_string_b_("link"),
     _ff_=caml_new_string_b_(str_inserthtml_aF_),
     _fg_=caml_new_string_b_("link2wiki"),
     _fn_=caml_new_string_b_(str_e_),
     _eM_=[0,caml_new_string_b_(str_main_ml_bd_),96,17],
     _ee_=caml_new_string_b_("Main.Break");
    function failwith_aL_(s_a_){throw [0,_aM_,s_a_]}
    function invalid_arg_S_(s_a_){throw [0,_bm_,s_a_]}
    function _i_(s1_a_,s2_b_)
     {var
       l1_c_=s1_a_.getLen(),
       l2_e_=s2_b_.getLen(),
       s_d_=caml_create_string_w_(l1_c_+l2_e_|0);
      caml_blit_string_G_(s1_a_,0,s_d_,0,l1_c_);
      caml_blit_string_G_(s2_b_,0,s_d_,l1_c_,l2_e_);
      return s_d_}
    function string_of_int_aN_(n_a_){return caml_new_string_b_(str_e_+n_a_)}
    caml_ml_open_descriptor_in_fQ_(0);
    caml_ml_open_descriptor_out_b0_(1);
    var stderr_T_=caml_ml_open_descriptor_out_b0_(2);
    function output_string_bn_(oc_a_,s_b_)
     {return caml_ml_output_cM_(oc_a_,s_b_,0,s_b_.getLen())}
    function prerr_string_bo_(s_a_){return output_string_bn_(stderr_T_,s_a_)}
    function do_at_exit_aO_(param_a_)
     {var param_b_=caml_ml_out_channels_list_fS_(0);
      for(;;)
       {if(param_b_)
         {var l_c_=param_b_[2],a_d_=param_b_[1];
          try {caml_ml_flush_b1_(a_d_)}catch(_f_){}
          var param_b_=l_c_;
          continue}
        return 0}}
    caml_register_named_value_b2_(_cX_,do_at_exit_aO_);
    function _cY_(_a_,_b_){return caml_ml_output_char_b3_(_a_,_b_)}
    function _bp_(_a_){return caml_ml_flush_b1_(_a_)}
    function _bq_(f_a_,param_b_)
     {var param_c_=param_b_;
      for(;;)
       {if(param_c_)
         {var l_d_=param_c_[2];
          caml_call_gen1_h_(f_a_,param_c_[1]);
          var param_c_=l_d_;
          continue}
        return 0}}
    function _U_(n_a_,c_b_)
     {var s_c_=caml_create_string_w_(n_a_);
      caml_fill_string_ft_(s_c_,0,n_a_,c_b_);
      return s_c_}
    function _af_(s_a_,ofs_b_,len_c_)
     {if(0<=ofs_b_)
       if(0<=len_c_)
        if(!((s_a_.getLen()-len_c_|0)<ofs_b_))
         {var r_d_=caml_create_string_w_(len_c_);
          caml_blit_string_G_(s_a_,ofs_b_,r_d_,0,len_c_);
          return r_d_}
      return invalid_arg_S_(_c5_)}
    function _ag_(s1_a_,ofs1_b_,s2_c_,ofs2_d_,len_e_)
     {if(0<=len_e_)
       if(0<=ofs1_b_)
        if(!((s1_a_.getLen()-len_e_|0)<ofs1_b_))
         if(0<=ofs2_d_)
          if(!((s2_c_.getLen()-len_e_|0)<ofs2_d_))
           return caml_blit_string_G_(s1_a_,ofs1_b_,s2_c_,ofs2_d_,len_e_);
      return invalid_arg_S_(_c6_)}
    function _ah_(sep_d_,l_b_)
     {if(l_b_)
       {var hd_a_=l_b_[1],num_g_=[0,0],len_f_=[0,0],tl_h_=l_b_[2];
        _bq_
         (function(s_a_)
           {num_g_[1]++;len_f_[1]=len_f_[1]+s_a_.getLen()|0;return 0},
          l_b_);
        var
         r_e_=
          caml_create_string_w_
           (len_f_[1]+caml_mul_b4_(sep_d_.getLen(),num_g_[1]-1|0)|0);
        caml_blit_string_G_(hd_a_,0,r_e_,0,hd_a_.getLen());
        var pos_c_=[0,hd_a_.getLen()];
        _bq_
         (function(s_a_)
           {caml_blit_string_G_(sep_d_,0,r_e_,pos_c_[1],sep_d_.getLen());
            pos_c_[1]=pos_c_[1]+sep_d_.getLen()|0;
            caml_blit_string_G_(s_a_,0,r_e_,pos_c_[1],s_a_.getLen());
            pos_c_[1]=pos_c_[1]+s_a_.getLen()|0;
            return 0},
          tl_h_);
        return r_e_}
      return _c7_}
    var
     _aP_=caml_sys_const_word_size_f5_(0),
     _V_=caml_mul_b4_(_aP_/8|0,(1<<(_aP_-10|0))-1|0)-1|0,
     _c9_=[0,_c8_];
    function _ai_(n_a_)
     {var
       n_b_=1<=n_a_?n_a_:1,
       n_c_=_V_<n_b_?_V_:n_b_,
       s_d_=caml_create_string_w_(n_c_);
      return [0,s_d_,0,n_c_,s_d_]}
    function _aj_(b_a_){return _af_(b_a_[1],0,b_a_[2])}
    function _br_(b_a_,more_b_)
     {var new_len_c_=[0,b_a_[3]];
      for(;;)
       {if(new_len_c_[1]<(b_a_[2]+more_b_|0))
         {new_len_c_[1]=2*new_len_c_[1]|0;continue}
        if(_V_<new_len_c_[1])
         if((b_a_[2]+more_b_|0)<=_V_)
          new_len_c_[1]=_V_;
         else
          failwith_aL_(_c__);
        var new_buffer_d_=caml_create_string_w_(new_len_c_[1]);
        _ag_(b_a_[1],0,new_buffer_d_,0,b_a_[2]);
        b_a_[1]=new_buffer_d_;
        b_a_[3]=new_len_c_[1];
        return 0}}
    function _W_(b_a_,c_b_)
     {var pos_c_=b_a_[2];
      if(b_a_[3]<=pos_c_)_br_(b_a_,1);
      b_a_[1].safeSet(pos_c_,c_b_);
      b_a_[2]=pos_c_+1|0;
      return 0}
    function _m_(b_a_,s_b_)
     {var len_c_=s_b_.getLen(),new_position_d_=b_a_[2]+len_c_|0;
      if(b_a_[3]<new_position_d_)_br_(b_a_,len_c_);
      _ag_(s_b_,0,b_a_[1],b_a_[2],len_c_);
      b_a_[2]=new_position_d_;
      return 0}
    function index_of_int_aQ_(i_a_)
     {return 0<=i_a_?i_a_:failwith_aL_(_i_(_c$_,string_of_int_aN_(i_a_)))}
    function add_int_index_bs_(i_a_,idx_b_)
     {return index_of_int_aQ_(i_a_+idx_b_|0)}
    var _da_=1;
    function _bt_(_a_){return add_int_index_bs_(_da_,_a_)}
    function _bu_(fmt_a_){return _af_(fmt_a_,0,fmt_a_.getLen())}
    function bad_conversion_bv_(sfmt_a_,i_b_,c_c_)
     {var
       _d_=_i_(_dc_,_i_(sfmt_a_,_db_)),
       _e_=_i_(_dd_,_i_(string_of_int_aN_(i_b_),_d_));
      return invalid_arg_S_(_i_(_de_,_i_(_U_(1,c_c_),_e_)))}
    function bad_conversion_format_X_(fmt_a_,i_b_,c_c_)
     {return bad_conversion_bv_(_bu_(fmt_a_),i_b_,c_c_)}
    function incomplete_format_ak_(fmt_a_)
     {return invalid_arg_S_(_i_(_dg_,_i_(_bu_(fmt_a_),_df_)))}
    function extract_format_C_(fmt_f_,start_b_,stop_c_,widths_d_)
     {function skip_positional_spec_j_(start_a_)
       {if
         ((fmt_f_.safeGet(start_a_)+num_48_P_|0)<
          0||
          9<
          (fmt_f_.safeGet(start_a_)+num_48_P_|0))
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
       b_g_=_ai_((stop_c_-i_k_|0)+10|0);
      _W_(b_g_,37);
      var l1_e_=widths_d_,l2_i_=0;
      for(;;)
       {if(l1_e_)
         {var _n_=[0,l1_e_[1],l2_i_],l1_e_=l1_e_[2],l2_i_=_n_;continue}
        var i_a_=i_k_,widths_h_=l2_i_;
        for(;;)
         {if(i_a_<=stop_c_)
           {var c_l_=fmt_f_.safeGet(i_a_);
            if(42===c_l_)
             {if(widths_h_)
               {var t_o_=widths_h_[2];
                _m_(b_g_,string_of_int_aN_(widths_h_[1]));
                var i_a_=skip_positional_spec_j_(i_a_+1|0),widths_h_=t_o_;
                continue}
              throw [0,_u_,_dh_]}
            _W_(b_g_,c_l_);
            var i_a_=i_a_+1|0;
            continue}
          return _aj_(b_g_)}}}
    function extract_format_int_bw_(conv_a_,fmt_b_,start_c_,stop_d_,widths_e_)
     {var sfmt_f_=extract_format_C_(fmt_b_,start_c_,stop_d_,widths_e_);
      if(78!==conv_a_)if(num_110_aa_!==conv_a_)return sfmt_f_;
      sfmt_f_.safeSet(sfmt_f_.getLen()-1|0,num_117_a__);
      return sfmt_f_}
    function sub_format_for_printf_bx_(conv_a_)
     {return function(fmt_d_,i_b_)
       {var len_k_=fmt_d_.getLen();
        function sub_fmt_l_(c_a_,j_b_)
         {var close_m_=40===c_a_?41:num_125_a9_,j_c_=j_b_;
          for(;;)
           {if(len_k_<=j_c_)return incomplete_format_ak_(fmt_d_);
            if(37===fmt_d_.safeGet(j_c_))
             {var j_e_=j_c_+1|0;
              if(len_k_<=j_e_)return incomplete_format_ak_(fmt_d_);
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
                       :bad_conversion_format_X_(fmt_d_,j_b_,c_f_);
              var j_c_=sub_fmt_l_(c_f_,j_e_+1|0)+1|0;
              continue}
            var j_c_=j_c_+1|0;
            continue}}
        return sub_fmt_l_(conv_a_,i_b_)}}
    function iter_on_format_args_by_(fmt_i_,add_conv_b_,add_char_c_)
     {var lim_m_=fmt_i_.getLen()-1|0;
      function scan_fmt_s_(i_a_)
       {var i_l_=i_a_;
        a:
        for(;;)
         {if(i_l_<lim_m_)
           {if(37===fmt_i_.safeGet(i_l_))
             {var skip_f_=0,i_h_=i_l_+1|0;
              for(;;)
               {if(lim_m_<i_h_)
                 var _e_=incomplete_format_ak_(fmt_i_);
                else
                 {var match_n_=fmt_i_.safeGet(i_h_);
                  if(58<=match_n_)
                   {if(95===match_n_){var skip_f_=1,i_h_=i_h_+1|0;continue}}
                  else
                   if(32<=match_n_)
                    switch(match_n_+num_32_co_|0)
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
                         caml_call_gen3_k_(add_conv_b_,skip_f_,i_h_,num_105_L_);
                       continue;
                      default:var i_h_=i_h_+1|0;continue}
                  var i_d_=i_h_;
                  b:
                  for(;;)
                   {if(lim_m_<i_d_)
                     var _e_=incomplete_format_ak_(fmt_i_);
                    else
                     {var conv_j_=fmt_i_.safeGet(i_d_);
                      if(126<=conv_j_)
                       var _g_=0;
                      else
                       switch(conv_j_)
                        {case 78:
                         case 88:
                         case num_100_aD_:
                         case num_105_L_:
                         case num_111_bc_:
                         case num_117_a__:
                         case num_120_a$_:
                          var
                           _e_=caml_call_gen3_k_(add_conv_b_,skip_f_,i_d_,num_105_L_),
                           _g_=1;
                          break;
                         case 69:
                         case 70:
                         case 71:
                         case num_101_cv_:
                         case num_102_bf_:
                         case num_103_be_:
                          var
                           _e_=caml_call_gen3_k_(add_conv_b_,skip_f_,i_d_,num_102_bf_),
                           _g_=1;
                          break;
                         case 33:
                         case 37:
                         case 44:
                         case 64:var _e_=i_d_+1|0,_g_=1;break;
                         case 83:
                         case 91:
                         case num_115_ab_:
                          var
                           _e_=caml_call_gen3_k_(add_conv_b_,skip_f_,i_d_,num_115_ab_),
                           _g_=1;
                          break;
                         case 97:
                         case num_114_aA_:
                         case num_116_a6_:
                          var
                           _e_=caml_call_gen3_k_(add_conv_b_,skip_f_,i_d_,conv_j_),
                           _g_=1;
                          break;
                         case 76:
                         case num_108_cC_:
                         case num_110_aa_:
                          var j_t_=i_d_+1|0;
                          if(lim_m_<j_t_)
                           var
                            _e_=caml_call_gen3_k_(add_conv_b_,skip_f_,i_d_,num_105_L_),
                            _g_=1;
                          else
                           {var _q_=fmt_i_.safeGet(j_t_)+num_88_cy_|0;
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
                                  caml_call_gen2_o_
                                   (add_char_c_,
                                    caml_call_gen3_k_(add_conv_b_,skip_f_,i_d_,conv_j_),
                                    num_105_L_),
                                 _g_=1,
                                 _r_=0;
                                break;
                               default:var _r_=1}
                            if(_r_)
                             var
                              _e_=caml_call_gen3_k_(add_conv_b_,skip_f_,i_d_,num_105_L_),
                              _g_=1}
                          break;
                         case 67:
                         case 99:
                          var
                           _e_=caml_call_gen3_k_(add_conv_b_,skip_f_,i_d_,99),
                           _g_=1;
                          break;
                         case 66:
                         case 98:
                          var
                           _e_=caml_call_gen3_k_(add_conv_b_,skip_f_,i_d_,66),
                           _g_=1;
                          break;
                         case 41:
                         case num_125_a9_:
                          var
                           _e_=caml_call_gen3_k_(add_conv_b_,skip_f_,i_d_,conv_j_),
                           _g_=1;
                          break;
                         case 40:
                          var
                           _e_=
                            scan_fmt_s_
                             (caml_call_gen3_k_(add_conv_b_,skip_f_,i_d_,conv_j_)),
                           _g_=1;
                          break;
                         case num_123_ba_:
                          var
                           i_u_=caml_call_gen3_k_(add_conv_b_,skip_f_,i_d_,conv_j_),
                           j_v_=
                            caml_call_gen2_o_
                             (sub_format_for_printf_bx_(conv_j_),fmt_i_,i_u_),
                           i_p_=i_u_;
                          for(;;)
                           {if(i_p_<(j_v_-2|0))
                             {var
                               i_p_=
                                caml_call_gen2_o_(add_char_c_,i_p_,fmt_i_.safeGet(i_p_));
                              continue}
                            var i_d_=j_v_-1|0;
                            continue b}
                         default:var _g_=0}
                      if(!_g_)
                       var _e_=bad_conversion_format_X_(fmt_i_,i_d_,conv_j_)}
                    break}}
                var i_l_=_e_;
                continue a}}
            var i_l_=i_l_+1|0;
            continue}
          return i_l_}}
      scan_fmt_s_(0);
      return 0}
    function count_printing_arguments_of_format_bz_(fmt_a_)
     {var ac_d_=[0,0,0,0];
      function add_conv_b_(skip_a_,i_b_,c_c_)
       {var _f_=41!==c_c_?1:0,_g_=_f_?num_125_a9_!==c_c_?1:0:_f_;
        if(_g_)
         {var inc_e_=97===c_c_?2:1;
          if(num_114_aA_===c_c_)ac_d_[3]=ac_d_[3]+1|0;
          if(skip_a_)
           ac_d_[2]=ac_d_[2]+inc_e_|0;
          else
           ac_d_[1]=ac_d_[1]+inc_e_|0}
        return i_b_+1|0}
      iter_on_format_args_by_
       (fmt_a_,add_conv_b_,function(i_a_,param_b_){return i_a_+1|0});
      return ac_d_[1]}
    function scan_positional_spec_bA_(fmt_a_,got_spec_b_,i_c_)
     {var d_g_=fmt_a_.safeGet(i_c_);
      if((d_g_+num_48_P_|0)<0||9<(d_g_+num_48_P_|0))
       return caml_call_gen2_o_(got_spec_b_,0,i_c_);
      var accu_e_=d_g_+num_48_P_|0,j_d_=i_c_+1|0;
      for(;;)
       {var d_f_=fmt_a_.safeGet(j_d_);
        if(48<=d_f_)
         {if(!(58<=d_f_))
           {var accu_e_=(10*accu_e_|0)+(d_f_+num_48_P_|0)|0,j_d_=j_d_+1|0;
            continue}}
        else
         if(36===d_f_)
          return 0===accu_e_
                  ?failwith_aL_(_dj_)
                  :caml_call_gen2_o_
                    (got_spec_b_,[0,index_of_int_aQ_(accu_e_-1|0)],j_d_+1|0);
        return caml_call_gen2_o_(got_spec_b_,0,i_c_)}}
    function next_index_p_(spec_a_,n_b_){return spec_a_?n_b_:_bt_(n_b_)}
    function get_index_bB_(spec_a_,n_b_){return spec_a_?spec_a_[1]:n_b_}
    function _bC_(to_s_aR_,get_out_b_,outc_c_,outs_d_,flush_e_,k_f_,fmt_g_)
     {var out_D_=caml_call_gen1_h_(get_out_b_,fmt_g_);
      function outs_an_(s_a_){return caml_call_gen2_o_(outs_d_,out_D_,s_a_)}
      function pr_aS_(k_a_,n_b_,fmt_j_,v_aT_)
       {var len_k_=fmt_j_.getLen();
        function doprn_E_(n_q_,i_b_)
         {var i_n_=i_b_;
          for(;;)
           {if(len_k_<=i_n_)return caml_call_gen1_h_(k_a_,out_D_);
            var c_d_=fmt_j_.safeGet(i_n_);
            if(37===c_d_)
             {var
               get_arg_l_=
                function(spec_a_,n_b_)
                 {return caml_array_get_a0_(v_aT_,get_index_bB_(spec_a_,n_b_))},
               scan_flags_aC_=
                function(spec_g_,n_f_,widths_c_,i_d_)
                 {var i_a_=i_d_;
                  for(;;)
                   {var switcher_ae_=fmt_j_.safeGet(i_a_)+num_32_co_|0;
                    if(!(switcher_ae_<0||25<switcher_ae_))
                     switch(switcher_ae_)
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
                        return scan_positional_spec_bA_
                                (fmt_j_,
                                 function(wspec_a_,i_b_)
                                  {var _d_=[0,get_arg_l_(wspec_a_,n_f_),widths_c_];
                                   return scan_flags_aC_
                                           (spec_g_,next_index_p_(wspec_a_,n_f_),_d_,i_b_)},
                                 i_a_+1|0);
                       default:var i_a_=i_a_+1|0;continue}
                    var conv_q_=fmt_j_.safeGet(i_a_);
                    if(!(124<=conv_q_))
                     switch(conv_q_)
                      {case 78:
                       case 88:
                       case num_100_aD_:
                       case num_105_L_:
                       case num_111_bc_:
                       case num_117_a__:
                       case num_120_a$_:
                        var
                         x_bj_=get_arg_l_(spec_g_,n_f_),
                         s_bk_=
                          caml_format_int_as_
                           (extract_format_int_bw_(conv_q_,fmt_j_,i_n_,i_a_,widths_c_),
                            x_bj_);
                        return cont_s_r_(next_index_p_(spec_g_,n_f_),s_bk_,i_a_+1|0);
                       case 69:
                       case 71:
                       case num_101_cv_:
                       case num_102_bf_:
                       case num_103_be_:
                        var
                         x_a8_=get_arg_l_(spec_g_,n_f_),
                         s_a9_=
                          caml_format_float_a1_
                           (extract_format_C_(fmt_j_,i_n_,i_a_,widths_c_),x_a8_);
                        return cont_s_r_(next_index_p_(spec_g_,n_f_),s_a9_,i_a_+1|0);
                       case 76:
                       case num_108_cC_:
                       case num_110_aa_:
                        var _al_=fmt_j_.safeGet(i_a_+1|0)+num_88_cy_|0;
                        if(!(_al_<0||32<_al_))
                         switch(_al_)
                          {case 0:
                           case 12:
                           case 17:
                           case 23:
                           case 29:
                           case 32:
                            var i_T_=i_a_+1|0,switcher_am_=conv_q_-108|0;
                            if(switcher_am_<0||2<switcher_am_)
                             var _ao_=0;
                            else
                             {switch(switcher_am_)
                               {case 1:var _ao_=0,_ap_=0;break;
                                case 2:
                                 var
                                  x_bi_=get_arg_l_(spec_g_,n_f_),
                                  _aJ_=
                                   caml_format_int_as_
                                    (extract_format_C_(fmt_j_,i_n_,i_T_,widths_c_),x_bi_),
                                  _ap_=1;
                                 break;
                                default:
                                 var
                                  x_bh_=get_arg_l_(spec_g_,n_f_),
                                  _aJ_=
                                   caml_format_int_as_
                                    (extract_format_C_(fmt_j_,i_n_,i_T_,widths_c_),x_bh_),
                                  _ap_=1}
                              if(_ap_)var s_aI_=_aJ_,_ao_=1}
                            if(!_ao_)
                             var
                              x_bg_=get_arg_l_(spec_g_,n_f_),
                              s_aI_=
                               caml_int64_format_fA_
                                (extract_format_C_(fmt_j_,i_n_,i_T_,widths_c_),x_bg_);
                            return cont_s_r_(next_index_p_(spec_g_,n_f_),s_aI_,i_T_+1|0)
                           }
                        var
                         x_bb_=get_arg_l_(spec_g_,n_f_),
                         s_bd_=
                          caml_format_int_as_
                           (extract_format_int_bw_
                             (num_110_aa_,fmt_j_,i_n_,i_a_,widths_c_),
                            x_bb_);
                        return cont_s_r_(next_index_p_(spec_g_,n_f_),s_bd_,i_a_+1|0);
                       case 37:
                       case 64:return cont_s_r_(n_f_,_U_(1,conv_q_),i_a_+1|0);
                       case 83:
                       case num_115_ab_:
                        var s_y_=get_arg_l_(spec_g_,n_f_);
                        if(num_115_ab_===conv_q_)
                         var x_z_=s_y_;
                        else
                         {var n_b_=[0,0],_av_=s_y_.getLen()-1|0,_aU_=0;
                          if(!(_av_<0))
                           {var i_M_=_aU_;
                            for(;;)
                             {var
                               c_x_=s_y_.safeGet(i_M_),
                               _bq_=
                                14<=c_x_
                                 ?34===c_x_?1:92===c_x_?1:0
                                 :11<=c_x_?13<=c_x_?1:0:8<=c_x_?1:0,
                               _aX_=_bq_?2:caml_is_printable_a2_(c_x_)?1:4;
                              n_b_[1]=n_b_[1]+_aX_|0;
                              var _aY_=i_M_+1|0;
                              if(_av_!==i_M_){var i_M_=_aY_;continue}
                              break}}
                          if(n_b_[1]===s_y_.getLen())
                           var _aL_=s_y_;
                          else
                           {var s_k_=caml_create_string_w_(n_b_[1]);
                            n_b_[1]=0;
                            var _aw_=s_y_.getLen()-1|0,_aV_=0;
                            if(!(_aw_<0))
                             {var i_K_=_aV_;
                              for(;;)
                               {var c_v_=s_y_.safeGet(i_K_),_A_=c_v_-34|0;
                                if(_A_<0||58<_A_)
                                 if(-20<=_A_)
                                  var _V_=1;
                                 else
                                  {switch(_A_+34|0)
                                    {case 8:
                                      s_k_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s_k_.safeSet(n_b_[1],98);
                                      var _J_=1;
                                      break;
                                     case 9:
                                      s_k_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s_k_.safeSet(n_b_[1],num_116_a6_);
                                      var _J_=1;
                                      break;
                                     case 10:
                                      s_k_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s_k_.safeSet(n_b_[1],num_110_aa_);
                                      var _J_=1;
                                      break;
                                     case 13:
                                      s_k_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s_k_.safeSet(n_b_[1],num_114_aA_);
                                      var _J_=1;
                                      break;
                                     default:var _V_=1,_J_=0}
                                   if(_J_)var _V_=0}
                                else
                                 var
                                  _V_=
                                   (_A_-1|0)<0||56<(_A_-1|0)
                                    ?(s_k_.safeSet(n_b_[1],92),
                                      n_b_[1]++,
                                      s_k_.safeSet(n_b_[1],c_v_),
                                      0)
                                    :1;
                                if(_V_)
                                 if(caml_is_printable_a2_(c_v_))
                                  s_k_.safeSet(n_b_[1],c_v_);
                                 else
                                  {s_k_.safeSet(n_b_[1],92);
                                   n_b_[1]++;
                                   s_k_.safeSet(n_b_[1],48+(c_v_/num_100_aD_|0)|0);
                                   n_b_[1]++;
                                   s_k_.safeSet(n_b_[1],48+((c_v_/10|0)%10|0)|0);
                                   n_b_[1]++;
                                   s_k_.safeSet(n_b_[1],48+(c_v_%10|0)|0)}
                                n_b_[1]++;
                                var _aW_=i_K_+1|0;
                                if(_aw_!==i_K_){var i_K_=_aW_;continue}
                                break}}
                            var _aL_=s_k_}
                          var x_z_=_i_(_du_,_i_(_aL_,_dt_))}
                        if(i_a_===(i_n_+1|0))
                         var s_aK_=x_z_;
                        else
                         {var sfmt_I_=extract_format_C_(fmt_j_,i_n_,i_a_,widths_c_);
                          try
                           {var neg_Y_=0,i_t_=1;
                            for(;;)
                             {if(sfmt_I_.getLen()<=i_t_)
                               var _ax_=[0,0,neg_Y_];
                              else
                               {var match_Z_=sfmt_I_.safeGet(i_t_);
                                if(49<=match_Z_)
                                 if(58<=match_Z_)
                                  var _aq_=0;
                                 else
                                  var
                                   _ax_=
                                    [0,
                                     caml_int_of_string_fK_
                                      (_af_(sfmt_I_,i_t_,(sfmt_I_.getLen()-i_t_|0)-1|0)),
                                     neg_Y_],
                                   _aq_=1;
                                else
                                 {if(45===match_Z_){var neg_Y_=1,i_t_=i_t_+1|0;continue}
                                  var _aq_=0}
                                if(!_aq_){var i_t_=i_t_+1|0;continue}}
                              var match_$_=_ax_;
                              break}}
                          catch(_f_)
                           {_f_=caml_wrap_exception_at_(_f_);
                            if(_f_[1]!==_aM_)throw _f_;
                            var match_$_=bad_conversion_bv_(sfmt_I_,0,num_115_ab_)}
                          var
                           p_N_=match_$_[1],
                           len_B_=x_z_.getLen(),
                           neg_aZ_=match_$_[2],
                           i_O_=0,
                           pad_char_a0_=32;
                          if(p_N_===len_B_)
                           if(0===i_O_)var _ac_=x_z_,_ar_=1;else var _ar_=0;
                          else
                           var _ar_=0;
                          if(!_ar_)
                           if(p_N_<=len_B_)
                            var _ac_=_af_(x_z_,i_O_,len_B_);
                           else
                            {var res___=_U_(p_N_,pad_char_a0_);
                             if(neg_aZ_)
                              _ag_(x_z_,i_O_,res___,0,len_B_);
                             else
                              _ag_(x_z_,i_O_,res___,p_N_-len_B_|0,len_B_);
                             var _ac_=res___}
                          var s_aK_=_ac_}
                        return cont_s_r_(next_index_p_(spec_g_,n_f_),s_aK_,i_a_+1|0);
                       case 67:
                       case 99:
                        var c_s_=get_arg_l_(spec_g_,n_f_);
                        if(99===conv_q_)
                         var s_aG_=_U_(1,c_s_);
                        else
                         {if(39===c_s_)
                           var _u_=_cZ_;
                          else
                           if(92===c_s_)
                            var _u_=_c0_;
                           else
                            {if(14<=c_s_)
                              var _F_=0;
                             else
                              switch(c_s_)
                               {case 8:var _u_=_c1_,_F_=1;break;
                                case 9:var _u_=_c2_,_F_=1;break;
                                case 10:var _u_=_c3_,_F_=1;break;
                                case 13:var _u_=_c4_,_F_=1;break;
                                default:var _F_=0}
                             if(!_F_)
                              if(caml_is_printable_a2_(c_s_))
                               {var s_au_=caml_create_string_w_(1);
                                s_au_.safeSet(0,c_s_);
                                var _u_=s_au_}
                              else
                               {var s_G_=caml_create_string_w_(4);
                                s_G_.safeSet(0,92);
                                s_G_.safeSet(1,48+(c_s_/num_100_aD_|0)|0);
                                s_G_.safeSet(2,48+((c_s_/10|0)%10|0)|0);
                                s_G_.safeSet(3,48+(c_s_%10|0)|0);
                                var _u_=s_G_}}
                          var s_aG_=_i_(_dr_,_i_(_u_,_dq_))}
                        return cont_s_r_(next_index_p_(spec_g_,n_f_),s_aG_,i_a_+1|0);
                       case 66:
                       case 98:
                        var _a5_=i_a_+1|0,_a7_=get_arg_l_(spec_g_,n_f_)?_cT_:_cU_;
                        return cont_s_r_(next_index_p_(spec_g_,n_f_),_a7_,_a5_);
                       case 40:
                       case num_123_ba_:
                        var
                         xf_S_=get_arg_l_(spec_g_,n_f_),
                         i_aE_=
                          caml_call_gen2_o_
                           (sub_format_for_printf_bx_(conv_q_),fmt_j_,i_a_+1|0);
                        if(num_123_ba_===conv_q_)
                         {var
                           b_P_=_ai_(xf_S_.getLen()),
                           add_char_ay_=
                            function(i_a_,c_b_){_W_(b_P_,c_b_);return i_a_+1|0};
                          iter_on_format_args_by_
                           (xf_S_,
                            function(skip_a_,i_b_,c_c_)
                             {if(skip_a_)_m_(b_P_,_di_);else _W_(b_P_,37);
                              return add_char_ay_(i_b_,c_c_)},
                            add_char_ay_);
                          var _a3_=_aj_(b_P_);
                          return cont_s_r_(next_index_p_(spec_g_,n_f_),_a3_,i_aE_)}
                        var
                         n_aF_=next_index_p_(spec_g_,n_f_),
                         m_bp_=
                          add_int_index_bs_
                           (count_printing_arguments_of_format_bz_(xf_S_),n_aF_);
                        return pr_aS_
                                (function(param_a_){return doprn_E_(m_bp_,i_aE_)},
                                 n_aF_,
                                 xf_S_,
                                 v_aT_);
                       case 33:
                        caml_call_gen1_h_(flush_e_,out_D_);
                        return doprn_E_(n_f_,i_a_+1|0);
                       case 41:return cont_s_r_(n_f_,_do_,i_a_+1|0);
                       case 44:return cont_s_r_(n_f_,_dp_,i_a_+1|0);
                       case 70:
                        var x_ah_=get_arg_l_(spec_g_,n_f_);
                        if(0===widths_c_)
                         var _aH_=_ds_;
                        else
                         {var sfmt_ad_=extract_format_C_(fmt_j_,i_n_,i_a_,widths_c_);
                          if(70===conv_q_)
                           sfmt_ad_.safeSet(sfmt_ad_.getLen()-1|0,num_103_be_);
                          var _aH_=sfmt_ad_}
                        var match_aB_=caml_classify_float_fq_(x_ah_);
                        if(3===match_aB_)
                         var s_ak_=x_ah_<0?_dl_:_dm_;
                        else
                         if(4<=match_aB_)
                          var s_ak_=_dn_;
                         else
                          {var
                            s_R_=caml_format_float_a1_(_aH_,x_ah_),
                            i_Q_=0,
                            l_a4_=s_R_.getLen();
                           for(;;)
                            {if(l_a4_<=i_Q_)
                              var _az_=_i_(s_R_,_dk_);
                             else
                              {var
                                _H_=s_R_.safeGet(i_Q_)-46|0,
                                _br_=
                                 _H_<0||23<_H_?55===_H_?1:0:(_H_-1|0)<0||21<(_H_-1|0)?1:0;
                               if(!_br_){var i_Q_=i_Q_+1|0;continue}
                               var _az_=s_R_}
                             var s_ak_=_az_;
                             break}}
                        return cont_s_r_(next_index_p_(spec_g_,n_f_),s_ak_,i_a_+1|0);
                       case 91:
                        return bad_conversion_format_X_(fmt_j_,i_a_,conv_q_);
                       case 97:
                        var
                         printer_aN_=get_arg_l_(spec_g_,n_f_),
                         n_aO_=_bt_(get_index_bB_(spec_g_,n_f_)),
                         arg_aP_=get_arg_l_(0,n_aO_),
                         i_bl_=i_a_+1|0,
                         n_bm_=next_index_p_(spec_g_,n_aO_);
                        if(to_s_aR_)
                         outs_an_(caml_call_gen2_o_(printer_aN_,0,arg_aP_));
                        else
                         caml_call_gen2_o_(printer_aN_,out_D_,arg_aP_);
                        return doprn_E_(n_bm_,i_bl_);
                       case num_114_aA_:
                        return bad_conversion_format_X_(fmt_j_,i_a_,conv_q_);
                       case num_116_a6_:
                        var
                         printer_aQ_=get_arg_l_(spec_g_,n_f_),
                         i_bn_=i_a_+1|0,
                         n_bo_=next_index_p_(spec_g_,n_f_);
                        if(to_s_aR_)
                         outs_an_(caml_call_gen1_h_(printer_aQ_,0));
                        else
                         caml_call_gen1_h_(printer_aQ_,out_D_);
                        return doprn_E_(n_bo_,i_bn_)
                       }
                    return bad_conversion_format_X_(fmt_j_,i_a_,conv_q_)}},
               i_f_=i_n_+1|0,
               widths_g_=0;
              return scan_positional_spec_bA_
                      (fmt_j_,
                       function(spec_a_,i_b_)
                        {return scan_flags_aC_(spec_a_,n_q_,widths_g_,i_b_)},
                       i_f_)}
            caml_call_gen2_o_(outc_c_,out_D_,c_d_);
            var i_n_=i_n_+1|0;
            continue}}
        function cont_s_r_(n_a_,s_b_,i_c_)
         {outs_an_(s_b_);return doprn_E_(n_a_,i_c_)}
        return doprn_E_(n_b_,0)}
      var _q_=index_of_int_aQ_(0);
      function kpr_k_(_a_,_b_){return pr_aS_(k_f_,_q_,_a_,_b_)}
      var nargs_l_=count_printing_arguments_of_format_bz_(fmt_g_);
      if(nargs_l_<0||6<nargs_l_)
       {var
         loop_n_=
          function(i_f_,args_b_)
           {if(nargs_l_<=i_f_)
             {var
               a_h_=caml_make_vect_H_(nargs_l_,0),
               f_i_=
                function(i_a_,arg_b_)
                 {return caml_array_set_j_(a_h_,(nargs_l_-i_a_|0)-1|0,arg_b_)},
               i_c_=0,
               param_a_=args_b_;
              for(;;)
               {if(param_a_)
                 {var _d_=param_a_[2],_e_=param_a_[1];
                  if(_d_)
                   {f_i_(i_c_,_e_);var i_c_=i_c_+1|0,param_a_=_d_;continue}
                  f_i_(i_c_,_e_)}
                return kpr_k_(fmt_g_,a_h_)}}
            return function(x_a_){return loop_n_(i_f_+1|0,[0,x_a_,args_b_])}};
        return loop_n_(0,0)}
      switch(nargs_l_)
       {case 1:
         return function(x_a_)
          {var a_b_=caml_make_vect_H_(1,0);
           caml_array_set_j_(a_b_,0,x_a_);
           return kpr_k_(fmt_g_,a_b_)};
        case 2:
         return function(x_a_,y_b_)
          {var a_c_=caml_make_vect_H_(2,0);
           caml_array_set_j_(a_c_,0,x_a_);
           caml_array_set_j_(a_c_,1,y_b_);
           return kpr_k_(fmt_g_,a_c_)};
        case 3:
         return function(x_a_,y_b_,z_c_)
          {var a_d_=caml_make_vect_H_(3,0);
           caml_array_set_j_(a_d_,0,x_a_);
           caml_array_set_j_(a_d_,1,y_b_);
           caml_array_set_j_(a_d_,2,z_c_);
           return kpr_k_(fmt_g_,a_d_)};
        case 4:
         return function(x_a_,y_b_,z_c_,t_d_)
          {var a_e_=caml_make_vect_H_(4,0);
           caml_array_set_j_(a_e_,0,x_a_);
           caml_array_set_j_(a_e_,1,y_b_);
           caml_array_set_j_(a_e_,2,z_c_);
           caml_array_set_j_(a_e_,3,t_d_);
           return kpr_k_(fmt_g_,a_e_)};
        case 5:
         return function(x_a_,y_b_,z_c_,t_d_,u_e_)
          {var a_f_=caml_make_vect_H_(5,0);
           caml_array_set_j_(a_f_,0,x_a_);
           caml_array_set_j_(a_f_,1,y_b_);
           caml_array_set_j_(a_f_,2,z_c_);
           caml_array_set_j_(a_f_,3,t_d_);
           caml_array_set_j_(a_f_,4,u_e_);
           return kpr_k_(fmt_g_,a_f_)};
        case 6:
         return function(x_a_,y_b_,z_c_,t_d_,u_e_,v_f_)
          {var a_h_=caml_make_vect_H_(6,0);
           caml_array_set_j_(a_h_,0,x_a_);
           caml_array_set_j_(a_h_,1,y_b_);
           caml_array_set_j_(a_h_,2,z_c_);
           caml_array_set_j_(a_h_,3,t_d_);
           caml_array_set_j_(a_h_,4,u_e_);
           caml_array_set_j_(a_h_,5,v_f_);
           return kpr_k_(fmt_g_,a_h_)};
        default:return kpr_k_(fmt_g_,[0])}}
    function _bD_(oc_d_)
     {function k_e_(_a_){return 0}
      function _b_(param_a_){return oc_d_}
      var _c_=0;
      return function(_a_)
       {return _bC_(_c_,_b_,_cY_,output_string_bn_,_bp_,k_e_,_a_)}}
    function _dv_(fmt_a_){return _ai_(2*fmt_a_.getLen()|0)}
    function _q_(fmt_a_)
     {function _b_(b_a_){var s_b_=_aj_(b_a_);b_a_[2]=0;return s_b_}
      return _bC_(1,_dv_,_W_,_m_,function(_a_){return 0},_b_,fmt_a_)}
    var _aR_=[0,0];
    function _aT_(x_a_,i_b_)
     {var f_c_=x_a_[i_b_+1];
      if(caml_obj_is_block_fX_(f_c_))
       {if(caml_obj_tag_b5_(f_c_)===252)
         return caml_call_gen1_h_(_q_(_dw_),f_c_);
        if(caml_obj_tag_b5_(f_c_)===253)
         {var s_e_=caml_format_float_a1_(_cW_,f_c_),i_d_=0,l_g_=s_e_.getLen();
          for(;;)
           {if(l_g_<=i_d_)return _i_(s_e_,_cV_);
            var
             match_f_=s_e_.safeGet(i_d_),
             _j_=48<=match_f_?58<=match_f_?0:1:45===match_f_?1:0;
            if(_j_){var i_d_=i_d_+1|0;continue}
            return s_e_}}
        return _dx_}
      return caml_call_gen1_h_(_q_(_dy_),f_c_)}
    function _bE_(x_a_,i_b_)
     {if(x_a_.length-1<=i_b_)return _dz_;
      var _c_=_bE_(x_a_,i_b_+1|0),_d_=_aT_(x_a_,i_b_);
      return caml_call_gen2_o_(_q_(_dA_),_d_,_c_)}
    function _bF_(x_a_)
     {var param_b_=_aR_[1];
      for(;;)
       {if(param_b_)
         {var tl_s_=param_b_[2],hd_t_=param_b_[1];
          try
           {var _v_=caml_call_gen1_h_(hd_t_,x_a_),_e_=_v_}
          catch(_f_){var _e_=0}
          if(_e_)return _e_[1];
          var param_b_=tl_s_;
          continue}
        if(x_a_[1]===_dF_)return _dG_;
        if(x_a_[1]===_bG_)return _dH_;
        if(x_a_[1]===_bH_)
         {var
           match_f_=x_a_[2],
           char_k_=match_f_[3],
           line_w_=match_f_[2],
           file_x_=match_f_[1];
          return caml_call_gen5_au_
                  (_q_(_aS_),file_x_,line_w_,char_k_,char_k_+5|0,_dI_)}
        if(x_a_[1]===_u_)
         {var
           match_g_=x_a_[2],
           char_l_=match_g_[3],
           line_y_=match_g_[2],
           file_z_=match_g_[1];
          return caml_call_gen5_au_
                  (_q_(_aS_),file_z_,line_y_,char_l_,char_l_+6|0,_dJ_)}
        if(x_a_[1]===_bI_)
         {var
           match_j_=x_a_[2],
           char_m_=match_j_[3],
           line_A_=match_j_[2],
           file_B_=match_j_[1];
          return caml_call_gen5_au_
                  (_q_(_aS_),file_B_,line_A_,char_m_,char_m_+6|0,_dK_)}
        var n_d_=x_a_.length-1,constructor_C_=x_a_[0+1][0+1];
        if(n_d_<0||2<n_d_)
         var
          _n_=_bE_(x_a_,2),
          _p_=_aT_(x_a_,1),
          _c_=caml_call_gen2_o_(_q_(_dB_),_p_,_n_);
        else
         switch(n_d_)
          {case 1:var _c_=_dD_;break;
           case 2:
            var _r_=_aT_(x_a_,1),_c_=caml_call_gen1_h_(_q_(_dE_),_r_);break;
           default:var _c_=_dC_}
        return _i_(constructor_C_,_c_)}}
    function _bJ_(outchan_a_)
     {var
       backtrace_i_=
        caml_convert_raw_backtrace_fr_
         (caml_get_exception_raw_backtrace_fx_(0));
      if(backtrace_i_)
       {var a_d_=backtrace_i_[1],_f_=a_d_.length-1-1|0,_p_=0;
        if(!(_f_<0))
         {var i_c_=_p_;
          for(;;)
           {if(caml_notequal_fW_(caml_array_get_a0_(a_d_,i_c_),_dR_))
             {var
               li_b_=caml_array_get_a0_(a_d_,i_c_),
               is_raise_j_=0===li_b_[0]?li_b_[1]:li_b_[1],
               info_e_=is_raise_j_?0===i_c_?_dL_:_dO_:0===i_c_?_dP_:_dQ_;
              if(0===li_b_[0])
               var
                endchar_k_=li_b_[5],
                startchar_l_=li_b_[4],
                lineno_m_=li_b_[3],
                filename_n_=li_b_[2],
                _g_=
                 caml_call_gen5_au_
                  (_q_(_dM_),
                   info_e_,
                   filename_n_,
                   lineno_m_,
                   startchar_l_,
                   endchar_k_);
              else
               var _g_=caml_call_gen1_h_(_q_(_dN_),info_e_);
              caml_call_gen2_o_(_bD_(outchan_a_),_dS_,_g_)}
            var _r_=i_c_+1|0;
            if(_f_!==i_c_){var i_c_=_r_;continue}
            break}}
        return 0}
      return caml_call_gen1_h_(_bD_(outchan_a_),_dT_)}
    function _bK_(fn_a_){_aR_[1]=[0,fn_a_,_aR_[1]];return 0}
    32===_aP_;
    function _bL_(param_a_)
     {var seq_b_=[];
      caml_update_dummy_f$_(seq_b_,[0,seq_b_,seq_b_]);
      return seq_b_}
    var Canceled_aU_=[0,_dU_],current_data_D_=[0,0];
    function repr_rec_aV_(t_a_)
     {var _c_=t_a_[1];
      if(3===_c_[0])
       {var t_d_=_c_[1],t_b_=repr_rec_aV_(t_d_);
        if(t_b_!==t_d_)t_a_[1]=[3,t_b_];
        return t_b_}
      return t_a_}
    function repr_Y_(t_a_){return repr_rec_aV_(t_a_)}
    var
     async_exception_hook_bM_=
      [0,
       function(exn_a_)
        {prerr_string_bo_(_dV_);
         prerr_string_bo_(_bF_(exn_a_));
         caml_ml_output_char_b3_(stderr_T_,10);
         _bJ_(stderr_T_);
         _bp_(stderr_T_);
         do_at_exit_aO_(0);
         return caml_sys_exit_f6_(2)}];
    function call_unsafe_bN_(f_a_,x_b_)
     {try
       {var _c_=caml_call_gen1_h_(f_a_,x_b_)}
      catch(exn_f_)
       {exn_f_=caml_wrap_exception_at_(exn_f_);
        return caml_call_gen1_h_(async_exception_hook_bM_[1],exn_f_)}
      return _c_}
    function run_waiters_rec_a3_(counter_a_,state_b_,ws_c_,rem_d_)
     {var ws_f_=ws_c_,rem_e_=rem_d_;
      for(;;)
       if(typeof ws_f_===str_number_A_)
        return counter_a_<50
                ?run_waiters_rec_next_x_(1+counter_a_,state_b_,rem_e_)
                :caml_trampoline_return_y_
                  (run_waiters_rec_next_x_,[0,state_b_,rem_e_]);
       else
        switch(ws_f_[0])
         {case 1:
           caml_call_gen1_h_(ws_f_[1],state_b_);
           return counter_a_<50
                   ?run_waiters_rec_next_x_(1+counter_a_,state_b_,rem_e_)
                   :caml_trampoline_return_y_
                     (run_waiters_rec_next_x_,[0,state_b_,rem_e_]);
          case 2:
           var _i_=[0,ws_f_[2],rem_e_],ws_f_=ws_f_[1],rem_e_=_i_;continue;
          default:
           var _g_=ws_f_[1][1];
           if(_g_)
            {caml_call_gen1_h_(_g_[1],state_b_);
             return counter_a_<50
                     ?run_waiters_rec_next_x_(1+counter_a_,state_b_,rem_e_)
                     :caml_trampoline_return_y_
                       (run_waiters_rec_next_x_,[0,state_b_,rem_e_])}
           else
            return counter_a_<50
                    ?run_waiters_rec_next_x_(1+counter_a_,state_b_,rem_e_)
                    :caml_trampoline_return_y_
                      (run_waiters_rec_next_x_,[0,state_b_,rem_e_])}}
    function run_waiters_rec_next_x_(counter_a_,state_b_,rem_c_)
     {return rem_c_
              ?counter_a_<50
                ?run_waiters_rec_a3_
                  (1+counter_a_,state_b_,rem_c_[1],rem_c_[2])
                :caml_trampoline_return_y_
                  (run_waiters_rec_a3_,[0,state_b_,rem_c_[1],rem_c_[2]])
              :0}
    function run_waiters_rec_dW_(state_b_,ws_c_,rem_d_)
     {return caml_trampoline_av_(run_waiters_rec_a3_(0,state_b_,ws_c_,rem_d_))}
    function run_waiters_rec_next_gc_(state_b_,rem_c_)
     {return caml_trampoline_av_(run_waiters_rec_next_x_(0,state_b_,rem_c_))}
    function run_cancel_handlers_rec_a4_(counter_a_,chs_b_,rem_c_)
     {var chs_e_=chs_b_,rem_d_=rem_c_;
      for(;;)
       if(typeof chs_e_===str_number_A_)
        return counter_a_<50
                ?run_cancel_handlers_rec_next_I_(1+counter_a_,rem_d_)
                :caml_trampoline_return_y_
                  (run_cancel_handlers_rec_next_I_,[0,rem_d_]);
       else
        switch(chs_e_[0])
         {case 1:
           var n_f_=chs_e_[1];
           if(n_f_[4]){n_f_[4]=0;n_f_[1][2]=n_f_[2];n_f_[2][1]=n_f_[1]}
           return counter_a_<50
                   ?run_cancel_handlers_rec_next_I_(1+counter_a_,rem_d_)
                   :caml_trampoline_return_y_
                     (run_cancel_handlers_rec_next_I_,[0,rem_d_]);
          case 2:
           var _h_=[0,chs_e_[2],rem_d_],chs_e_=chs_e_[1],rem_d_=_h_;continue;
          default:
           var f_g_=chs_e_[2];
           current_data_D_[1]=chs_e_[1];
           call_unsafe_bN_(f_g_,0);
           return counter_a_<50
                   ?run_cancel_handlers_rec_next_I_(1+counter_a_,rem_d_)
                   :caml_trampoline_return_y_
                     (run_cancel_handlers_rec_next_I_,[0,rem_d_])}}
    function run_cancel_handlers_rec_next_I_(counter_a_,rem_b_)
     {return rem_b_
              ?counter_a_<50
                ?run_cancel_handlers_rec_a4_(1+counter_a_,rem_b_[1],rem_b_[2])
                :caml_trampoline_return_y_
                  (run_cancel_handlers_rec_a4_,[0,rem_b_[1],rem_b_[2]])
              :0}
    function run_cancel_handlers_rec_dX_(chs_b_,rem_c_)
     {return caml_trampoline_av_(run_cancel_handlers_rec_a4_(0,chs_b_,rem_c_))}
    function run_cancel_handlers_rec_next_gd_(rem_b_)
     {return caml_trampoline_av_(run_cancel_handlers_rec_next_I_(0,rem_b_))}
    function unsafe_run_waiters_al_(sleeper_a_,state_b_)
     {var
       _c_=
        1===state_b_[0]
         ?state_b_[1][1]===Canceled_aU_
           ?(run_cancel_handlers_rec_dX_(sleeper_a_[4],0),1)
           :0
         :0;
      return run_waiters_rec_dW_(state_b_,sleeper_a_[2],0)}
    var wakening_aW_=[0,0],to_wakeup_N_=[0,0,0];
    function wakeup_bO_(t_a_,v_b_)
     {var result_h_=[0,v_b_],t_i_=repr_rec_aV_(t_a_),_e_=t_i_[1];
      switch(_e_[0])
       {case 1:if(_e_[1][1]===Canceled_aU_)return 0;break;
        case 2:
         var sleeper_k_=_e_[1];
         t_i_[1]=result_h_;
         var
          snapshot_g_=current_data_D_[1],
          already_wakening_j_=wakening_aW_[1]?1:(wakening_aW_[1]=1,0);
         unsafe_run_waiters_al_(sleeper_k_,result_h_);
         if(already_wakening_j_){current_data_D_[1]=snapshot_g_;return 0}
         for(;;)
          {if(0===to_wakeup_N_[1])
            {wakening_aW_[1]=0;current_data_D_[1]=snapshot_g_;return 0}
           if(0===to_wakeup_N_[1])throw [0,_c9_];
           to_wakeup_N_[1]=to_wakeup_N_[1]-1|0;
           var tail_c_=to_wakeup_N_[2],head_d_=tail_c_[2];
           if(head_d_===tail_c_)to_wakeup_N_[2]=0;else tail_c_[2]=head_d_[2];
           var _f_=head_d_[1];
           unsafe_run_waiters_al_(_f_[1],_f_[2]);
           continue}
        }
      return invalid_arg_S_(_dY_)}
    function append_bP_(l1_a_,l2_b_)
     {return typeof l1_a_===str_number_A_
              ?l2_b_
              :typeof l2_b_===str_number_A_?l1_a_:[2,l1_a_,l2_b_]}
    function cleanup_aX_(ws_a_)
     {if(typeof ws_a_!==str_number_A_)
       switch(ws_a_[0])
        {case 2:
          var l1_b_=ws_a_[1],_c_=cleanup_aX_(ws_a_[2]);
          return append_bP_(cleanup_aX_(l1_b_),_c_);
         case 1:break;
         default:if(!ws_a_[1][1])return 0}
      return ws_a_}
    var
     pause_hook_d3_=[0,function(_a_){return 0}],
     s1_r_=_bL_(0),
     _d4_=[0,0],
     window_O_=joo_global_object_t_,
     no_handler_am_=null;
    function _d5_(param_a_)
     {var _e_=1-(s1_r_[2]===s1_r_?1:0);
      if(_e_)
       {var seq_b_=_bL_(0);
        seq_b_[1][2]=s1_r_[2];
        s1_r_[2][1]=seq_b_[1];
        seq_b_[1]=s1_r_[1];
        s1_r_[1][2]=seq_b_;
        s1_r_[1]=s1_r_;
        s1_r_[2]=s1_r_;
        _d4_[1]=0;
        var curr_c_=seq_b_[2];
        for(;;)
         {var _d_=curr_c_!==seq_b_?1:0;
          if(_d_)
           {if(curr_c_[4])wakeup_bO_(curr_c_[3],0);
            var curr_c_=curr_c_[2];
            continue}
          return _d_}}
      return _e_}
    var undefined_d6_=undefined;
    function _bQ_(x_a_,f_b_)
     {return x_a_==no_handler_am_?0:caml_call_gen1_h_(f_b_,x_a_)}
    function _bR_(x_a_,f_b_,g_c_)
     {return x_a_==no_handler_am_
              ?caml_call_gen1_h_(f_b_,0)
              :caml_call_gen1_h_(g_c_,x_a_)}
    function _an_(x_a_,f_b_)
     {return x_a_==no_handler_am_?caml_call_gen1_h_(f_b_,0):x_a_}
    var true_Z_=true,false_ao_=false,a67e0736b_bS_=Array,Error_bT_=[0,_d7_];
    caml_register_named_value_b2_(name_d8_,[0,Error_bT_,{}][0+1]);
    _bK_
     (function(param_a_)
       {return param_a_[1]===Error_bT_
                ?[0,new MlWrappedString_g_(param_a_[2].toString())]
                :0});
    _bK_
     (function(e_a_)
       {return e_a_ instanceof a67e0736b_bS_
                ?0
                :[0,new MlWrappedString_g_(e_a_.toString())]});
    function ___(_a_){return _a_}
    function _E_(p_a_,n_b_){p_a_.appendChild(n_b_);return 0}
    function handler_ap_(f_d_)
     {return ___
              (caml_js_wrap_callback_a5_
                (function(e_a_)
                  {if(e_a_)
                    {var res_e_=caml_call_gen1_h_(f_d_,e_a_);
                     if(!(res_e_|0))e_a_.preventDefault();
                     return res_e_}
                   var e_c_=event,res_b_=caml_call_gen1_h_(f_d_,e_c_);
                   if(!(res_b_|0))e_c_.returnValue=res_b_;
                   return res_b_}))}
    var doc_v_=window_O_.document;
    function opt_iter_aq_(x_a_,f_b_)
     {return x_a_?caml_call_gen1_h_(f_b_,x_a_[1]):0}
    function createElement_aY_(doc_a_,name_b_)
     {return doc_a_.createElement(name_b_.toString())}
    function unsafeCreateElement_bU_(doc_a_,name_b_)
     {return createElement_aY_(doc_a_,name_b_)}
    var createElementSyntax_bV_=[0,num_785140586_cp_];
    function unsafeCreateElementEx_bW_(type_a_,name_b_,doc_c_,elt_d_)
     {for(;;)
       {if(0===type_a_)if(0===name_b_)return createElement_aY_(doc_c_,elt_d_);
        var _h_=createElementSyntax_bV_[1];
        if(num_785140586_cp_===_h_)
         {try
           {var
             el_j_=doc_v_.createElement('<input name="x">'),
             _k_=el_j_.tagName.toLowerCase()===str_input_b__?1:0,
             _m_=_k_?el_j_.name===str_x_ci_?1:0:_k_,
             _i_=_m_}
          catch(_f_){var _i_=0}
          var _l_=_i_?num_982028505_cf_:-1003883683;
          createElementSyntax_bV_[1]=_l_;
          continue}
        if(num_982028505_cf_<=_h_)
         {var a_f_=new a67e0736b_bS_();
          a_f_.push("<",elt_d_.toString());
          opt_iter_aq_
           (type_a_,
            function(t_a_)
             {a_f_.push(' type="',caml_js_html_escape_b6_(t_a_),str_az_);
              return 0});
          opt_iter_aq_
           (name_b_,
            function(n_a_)
             {a_f_.push(' name="',caml_js_html_escape_b6_(n_a_),str_az_);
              return 0});
          a_f_.push(">");
          return doc_c_.createElement(a_f_.join(str_e_))}
        var res_g_=createElement_aY_(doc_c_,elt_d_);
        opt_iter_aq_(type_a_,function(t_a_){return res_g_.type=t_a_});
        opt_iter_aq_(name_b_,function(n_a_){return res_g_.name=n_a_});
        return res_g_}}
    function createTextarea_bX_(type_a_,name_b_,doc_c_)
     {return unsafeCreateElementEx_bW_(type_a_,name_b_,doc_c_,_d__)}
    function createBr_aZ_(doc_a_){return unsafeCreateElement_bU_(doc_a_,_d$_)}
    window_O_.HTMLElement===undefined_d6_;
    var
     a80410c35_eb_=caml_js_get_console_fM_(0),
     overflow_limit_ec_=num_2147483_a8_;
    pause_hook_d3_[1]=
    function(param_a_)
     {return 1===param_a_
              ?(window_O_.setTimeout(caml_js_wrap_callback_a5_(_d5_),0),0)
              :0};
    function _bY_(s_a_){return a80410c35_eb_.log(s_a_.toString())}
    async_exception_hook_bM_[1]=
    function(exn_a_){_bY_(_ed_);_bY_(_bF_(exn_a_));return _bJ_(stderr_T_)};
    function _ar_(x_a_,f_b_){return caml_call_gen1_h_(f_b_,x_a_)}
    var Break_bZ_=[0,_ee_];
    function html2wiki_F_(opt_a_,body_b_)
     {var ans_d_=_ai_(10);
      function add_str_f_(opt_a_,s_b_)
       {var surr_f_=opt_a_?opt_a_[1]:_ef_,len_e_=s_b_.getLen();
        try
         {var i_c_=0;
          for(;;)
           {if(!(len_e_<=i_c_))
             {if(92===s_b_.safeGet(i_c_))
               if(i_c_<(len_e_-1|0))
                if(92===s_b_.safeGet(i_c_+1|0)){var i_c_=i_c_+2|0;continue}
              if(10===s_b_.safeGet(i_c_)){var i_c_=i_c_+1|0;continue}
              throw [0,Break_bZ_,1]}
            var _h_=0,_g_=_h_;
            break}}
        catch(exn_f_)
         {exn_f_=caml_wrap_exception_at_(exn_f_);
          if(exn_f_[1]!==Break_bZ_)throw exn_f_;
          var _g_=exn_f_[2]}
        return _g_?_m_(ans_d_,_i_(surr_f_,_i_(s_b_,surr_f_))):0}
      var childNodes_h_=body_b_.childNodes,_j_=childNodes_h_.length-1|0,_k_=0;
      if(!(_j_<0))
       {var i_e_=_k_;
        for(;;)
         {var
           _l_=
            function(node_b_)
             {var hh_a_=new MlWrappedString_g_(node_b_.nodeName);
              if(caml_string_notequal_c_(hh_a_,_eg_))
               {if(caml_string_notequal_c_(hh_a_,_eh_))
                 {if(caml_string_notequal_c_(hh_a_,_ei_))
                   {if(caml_string_notequal_c_(hh_a_,_ej_))
                     {if(caml_string_notequal_c_(hh_a_,_ek_))
                       {if(caml_string_notequal_c_(hh_a_,_el_))
                         if(caml_string_notequal_c_(hh_a_,_em_))
                          if(caml_string_notequal_c_(hh_a_,_en_))
                           return caml_string_notequal_c_(hh_a_,_eo_)
                                   ?caml_string_notequal_c_(hh_a_,_ep_)
                                     ?caml_string_notequal_c_(hh_a_,_eq_)
                                       ?_m_(ans_d_,_i_(_es_,_i_(hh_a_,_er_)))
                                       :add_str_f_(0,_i_(html2wiki_F_(0,node_b_),_et_))
                                     :add_str_f_(_eu_,html2wiki_F_(0,node_b_))
                                   :_m_(ans_d_,_ev_);
                        var prefix_k_=_U_((hh_a_.safeGet(1)-48|0)+1|0,61);
                        return _m_
                                (ans_d_,_i_(prefix_k_,_i_(html2wiki_F_(0,node_b_),_ew_)))}
                      return _m_(ans_d_,html2wiki_F_(0,node_b_))}
                    return _m_(ans_d_,_ex_)}
                  return add_str_f_(_ey_,html2wiki_F_(0,node_b_))}
                var
                 x_l_=___(node_b_),
                 el_h_=_an_(x_l_,function(param_a_){throw [0,_u_,_ez_]}),
                 _n_=
                  function(s_a_)
                   {function _i_(_a_){return new MlWrappedString_g_(_a_)}
                    function _j_(param_a_){throw [0,_u_,_eA_]}
                    var
                     url_e_=_ar_(_an_(el_h_.getAttribute("href"),_j_),_i_),
                     match_f_=new MlWrappedString_g_(s_a_);
                    if(caml_string_notequal_c_(match_f_,_eB_))
                     {if(caml_string_notequal_c_(match_f_,_eC_))
                       return _m_(ans_d_,_eD_);
                      var _k_=function(_a_){return _m_(ans_d_,_a_)};
                      return _ar_(_ah_(_eG_,[0,_eF_,[0,url_e_,_eE_]]),_k_)}
                    return _m_
                            (ans_d_,
                             _ah_
                              (_eK_,
                               [0,
                                _eJ_,
                                [0,url_e_,[0,_eI_,[0,html2wiki_F_(0,node_b_),_eH_]]]]))},
                 _o_=function(param_a_){return _m_(ans_d_,_eL_)};
                return _bR_(el_h_.getAttribute("wysitype"),_o_,_n_)}
              var x_p_=node_b_.nodeValue;
              function _j_(x_a_){return [0,x_a_]}
              var match_e_=_bR_(x_p_,function(param_a_){return 0},_j_);
              return match_e_
                      ?_m_(ans_d_,new MlWrappedString_g_(match_e_[1]))
                      :0};
          _bQ_(childNodes_h_.item(i_e_),_l_);
          var _n_=i_e_+1|0;
          if(_j_!==i_e_){var i_e_=_n_;continue}
          break}}
      return _aj_(ans_d_)}
    window_O_.onload=
    handler_ap_
     (function(param_a_)
       {function _d_(param_a_){throw [0,_u_,_eM_]}
        var
         body_f_=_an_(doc_v_.getElementById("wiki_demo"),_d_),
         iframe_h_=unsafeCreateElement_bU_(doc_v_,_ea_);
        iframe_h_.style.border="2px green solid";
        iframe_h_.src=str_ck_;
        iframe_h_.id="wysiFrame";
        _E_(body_f_,iframe_h_);
        function _e_(iDoc_e_)
         {iDoc_e_.open();
          iDoc_e_.write
           ("<html><body><p><b>Camelus</b><i>bactrianus</i></p></body></html>");
          iDoc_e_.close();
          iDoc_e_.designMode="On";
          var iWin_j_=iframe_h_.contentWindow;
          _E_(body_f_,createBr_aZ_(doc_v_));
          function createButton_a_(opt_a_,_b_,title_c_,action_d_)
           {var
             show_i_=opt_a_?opt_a_[1]:false_ao_,
             value_h_=_b_?_b_[1]:0,
             but_g_=unsafeCreateElementEx_bW_([0,"submit"],0,doc_v_,_d9_);
            but_g_.value=title_c_.toString();
            but_g_.onclick=
            handler_ap_
             (function(param_a_)
               {iWin_j_.focus();
                var _b_=value_h_?___(value_h_[1].toString()):no_handler_am_;
                iDoc_e_.execCommand(action_d_.toString(),show_i_,_b_);
                return true_Z_});
            _E_(body_f_,but_g_);
            return but_g_}
          createButton_a_(0,0,_eO_,_eN_);
          createButton_a_(0,0,_eQ_,_eP_);
          createButton_a_(0,0,_eS_,_eR_);
          createButton_a_(0,0,_eU_,_eT_);
          _E_(body_f_,createBr_aZ_(doc_v_));
          createButton_a_(0,_eX_,_eW_,_eV_);
          createButton_a_(0,_e0_,_eZ_,_eY_);
          createButton_a_(0,_e3_,_e2_,_e1_);
          createButton_a_(0,_e6_,_e5_,_e4_);
          function prompt_k_(query_a_,default_b_)
           {function _c_(_a_){return new MlWrappedString_g_(_a_)}
            function _d_(param_a_){return default_b_.toString()}
            return _ar_
                    (_an_
                      (iWin_j_.prompt(query_a_.toString(),default_b_.toString()),
                       _d_),
                     _c_)}
          var a8d589752_l_=createButton_a_(0,0,_e8_,_e7_);
          a8d589752_l_.onclick=
          handler_ap_
           (function(param_a_)
             {var
               link_c_=prompt_k_(_e__,_e9_),
               link_b_=
                _ah_
                 (_fe_,
                  [0,_fd_,[0,link_c_,[0,_fc_,[0,prompt_k_(_fa_,_e$_),_fb_]]]]);
              iWin_j_.alert(link_b_.toString());
              iDoc_e_.execCommand
               (str_inserthtml_aF_,false_ao_,___(link_b_.toString()));
              return true_Z_});
          var ac28ba819_m_=createButton_a_(0,0,_fg_,_ff_);
          ac28ba819_m_.onclick=
          handler_ap_
           (function(param_a_)
             {var
               link_b_=prompt_k_(_fi_,_fh_),
               link_c_=
                _ar_
                 ([0,_fm_,[0,link_b_,[0,_fl_,[0,link_b_,_fk_]]]],
                  function(_a_){return _ah_(_fj_,_a_)});
              iWin_j_.alert(link_c_.toString());
              iDoc_e_.execCommand
               (str_inserthtml_aF_,false_ao_,___(link_c_.toString()));
              return true_Z_});
          _E_(body_f_,createBr_aZ_(doc_v_));
          var preview_d_=createTextarea_bX_(0,0,doc_v_);
          preview_d_.readOnly=true_Z_;
          preview_d_[caml_new_string_b_(str_cols_b8_)]=34;
          preview_d_[caml_new_string_b_(str_rows_cc_)]=10;
          preview_d_.style.border="1px black solid";
          preview_d_.style.padding=str_5px_b7_;
          _E_(body_f_,preview_d_);
          var wikiFrame_i_=createTextarea_bX_(0,0,doc_v_);
          wikiFrame_i_.id="wikiFrame";
          wikiFrame_i_.readOnly=true_Z_;
          wikiFrame_i_[caml_new_string_b_(str_cols_b8_)]=34;
          wikiFrame_i_[caml_new_string_b_(str_rows_cc_)]=10;
          preview_d_.style.border="2px blue solid";
          preview_d_.style.padding=str_5px_b7_;
          _E_(body_f_,wikiFrame_i_);
          function dyn_preview_y_(old_text_a_,n_b_)
           {var text_l_=new MlWrappedString_g_(iDoc_e_.body.innerHTML);
            if(caml_string_notequal_c_(text_l_,old_text_a_))
             {try
               {preview_d_.value=text_l_.toString();
                wikiFrame_i_.value=html2wiki_F_(0,iDoc_e_.body).toString()}
              catch(_f_){}
              var n_m_=20}
            else
             var
              y_B_=n_b_-1|0,
              x_J_=0,
              _K_=caml_greaterequal_fy_(0,y_B_)?x_J_:y_B_,
              n_m_=_K_;
            function f_z_(param_a_){return dyn_preview_y_(text_l_,n_m_)}
            var _I_=0===n_m_?0.5:0.1,t_h_=[0,[2,[0,1,0,0,0]]],id_v_=[0,0];
            function wait_w_(d_a_,param_b_)
             {var
               match_c_=
                num_2147483_a8_<d_a_
                 ?[0,overflow_limit_ec_,d_a_-num_2147483_a8_]
                 :[0,d_a_,0],
               remain_d_=match_c_[2],
               step_e_=match_c_[1],
               cb_f_=
                remain_d_==0
                 ?function(_a_){return wakeup_bO_(t_h_,_a_)}
                 :function(_a_){return wait_w_(remain_d_,_a_)};
              id_v_[1]=
              [0,
               window_O_.setTimeout
                (caml_js_wrap_callback_a5_(cb_f_),step_e_*num_1e3_cq_)];
              return 0}
            wait_w_(_I_,0);
            function f_x_(param_a_)
             {var _b_=id_v_[1];return _b_?window_O_.clearTimeout(_b_[1]):0}
            var _k_=repr_Y_(t_h_)[1];
            switch(_k_[0])
             {case 1:
               var _C_=_k_[1][1]===Canceled_aU_?(call_unsafe_bN_(f_x_,0),1):0;
               break;
              case 2:
               var
                sleeper_p_=_k_[1],
                handler_q_=[0,current_data_D_[1],f_x_],
                _r_=sleeper_p_[4],
                handler_G_=
                 typeof _r_===str_number_A_?handler_q_:[2,handler_q_,_r_];
               sleeper_p_[4]=handler_G_;
               var _C_=1;
               break;
              default:var _C_=0}
            var t_s_=repr_Y_(t_h_),_f_=t_s_[1];
            switch(_f_[0])
             {case 1:return [0,_f_];
              case 2:
               var
                sleeper_t_=_f_[1],
                res_j_=[0,[2,[0,[0,[0,t_s_]],0,0,0]]],
                data_H_=current_data_D_[1],
                waiter_o_=
                 [1,
                  function(state_a_)
                   {switch(state_a_[0])
                     {case 0:
                       var v_r_=state_a_[1];
                       current_data_D_[1]=data_H_;
                       try
                        {var _s_=f_z_(v_r_),_o_=_s_}
                       catch(exn_f_)
                        {exn_f_=caml_wrap_exception_at_(exn_f_);
                         var _o_=[0,[1,exn_f_]]}
                       var t1_c_=repr_Y_(res_j_),t2_f_=repr_Y_(_o_),_i_=t1_c_[1];
                       if(2===_i_[0])
                        {var sleeper1_b_=_i_[1];
                         if(t1_c_===t2_f_)return 0;
                         var _d_=t2_f_[1];
                         if(2===_d_[0])
                          {var sleeper2_e_=_d_[1];
                           t2_f_[1]=[3,t1_c_];
                           sleeper1_b_[1]=sleeper2_e_[1];
                           var
                            waiters_k_=append_bP_(sleeper1_b_[2],sleeper2_e_[2]),
                            removed_l_=sleeper1_b_[3]+sleeper2_e_[3]|0;
                           if(42<removed_l_)
                            {sleeper1_b_[3]=0;sleeper1_b_[2]=cleanup_aX_(waiters_k_)}
                           else
                            {sleeper1_b_[3]=removed_l_;sleeper1_b_[2]=waiters_k_}
                           var
                            _g_=sleeper2_e_[4],
                            _h_=sleeper1_b_[4],
                            _p_=
                             typeof _h_===str_number_A_
                              ?_g_
                              :typeof _g_===str_number_A_?_h_:[2,_h_,_g_];
                           sleeper1_b_[4]=_p_;
                           return 0}
                         t1_c_[1]=_d_;
                         return unsafe_run_waiters_al_(sleeper1_b_,_d_)}
                       throw [0,_u_,_dZ_];
                      case 1:
                       var t_m_=repr_Y_(res_j_),_n_=t_m_[1];
                       if(2===_n_[0])
                        {var sleeper_q_=_n_[1];
                         t_m_[1]=state_a_;
                         return unsafe_run_waiters_al_(sleeper_q_,state_a_)}
                       throw [0,_u_,_d0_];
                      default:throw [0,_u_,_d1_]}}],
                _n_=sleeper_t_[2],
                waiter_E_=
                 typeof _n_===str_number_A_?waiter_o_:[2,waiter_o_,_n_];
               sleeper_t_[2]=waiter_E_;
               return res_j_;
              case 3:throw [0,_u_,_d2_];
              default:return f_z_(_f_[1])}}
          dyn_preview_y_(_fn_,0);
          return 0}
        _bQ_(iframe_h_.contentDocument,_e_);
        return false_ao_});
    do_at_exit_aO_(0);
    return}
  (this));
