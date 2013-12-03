// This program was compiled from OCaml by js_of_ocaml 1.99dev
(function(joo_global_object_c_)
   {"use strict";
    var
     _bo_=125,
     _br_=123,
     _I_=254,
     _H_=255,
     _cA_="x",
     _cB_=0.99,
     _aM_=".",
     _bv_=108,
     _aK_="+",
     _aL_=65535,
     _w_=16777215,
     _cz_="g",
     _bn_="f",
     _cD_=250,
     _P_=105,
     _cy_="%d",
     _aj_=110,
     _cC_=785140586,
     _ak_=115,
     _aI_="int_of_string",
     _bu_=102,
     _cx_=982028505,
     _bs_=111,
     _bq_=120,
     _A_=" ",
     _ai_="e",
     _bp_=117,
     _O_="-",
     _r_="",
     _bm_=116,
     _aN_=100,
     _s_="0",
     _aJ_=114,
     _bt_=103,
     _cE_=101,
     _B_="number",
     _ah_=1e3;
    function caml_update_dummy_fv_(x_a_,y_b_)
     {if(typeof y_b_==="function"){x_a_.fun=y_b_;return 0}
      if(y_b_.fun){x_a_.fun=y_b_.fun;return 0}
      var i_c_=y_b_.length;
      while(i_c_--)x_a_[i_c_]=y_b_[i_c_];
      return 0}
    function caml_sys_getenv_fu_(){caml_raise_not_found_fn_()}
    function caml_raise_not_found_fn_()
     {caml_raise_constant_cP_(caml_global_data_al_[7])}
    function caml_sys_get_config_ft_()
     {return [0,new MlWrappedString_X_("Unix"),32,0]}
    function caml_sys_exit_fs_()
     {caml_invalid_argument_aQ_("Function 'exit' not implemented")}
    function caml_register_named_value_fr_(nm_a_,v_b_)
     {caml_named_values_fi_[nm_a_]=v_b_;return 0}
    var caml_named_values_fi_={};
    function caml_register_global_fq_(n_a_,v_b_)
     {caml_global_data_al_[n_a_+1]=v_b_}
    function caml_obj_tag_fl_(x_a_){return x_a_ instanceof Array?x_a_[0]:_ah_}
    function caml_obj_is_block_fk_(x_a_){return +(x_a_ instanceof Array)}
    function caml_notequal_fj_(x_a_,y_b_)
     {return +(caml_compare_val_cH_(x_a_,y_b_,false)!=0)}
    function caml_compare_fw_(a_a_,b_b_)
     {return caml_compare_val_cH_(a_a_,b_b_,true)}
    function caml_compare_val_cH_(a_a_,b_b_,total_c_)
     {var stack_e_=[];
      for(;;)
       {if(!(total_c_&&a_a_===b_b_))
         {if(a_a_ instanceof MlString_J_)
           {if(b_b_ instanceof MlString_J_)
             {if(a_a_!=b_b_)
               {var x_d_=a_a_.compare(b_b_);if(x_d_!=0)return x_d_}}
            else
             return 1}
          else
           if(a_a_ instanceof Array&&a_a_[0]===(a_a_[0]|0))
            {var ta_g_=a_a_[0];
             if(ta_g_===_cD_)
              {a_a_=a_a_[1];continue}
             else
              if(b_b_ instanceof Array&&b_b_[0]===(b_b_[0]|0))
               {var tb_h_=b_b_[0];
                if(tb_h_===_cD_)
                 {b_b_=b_b_[1];continue}
                else
                 if(ta_g_!=tb_h_)
                  {return ta_g_<tb_h_?-1:1}
                 else
                  {switch(ta_g_)
                    {case 248:
                      {var x_d_=caml_int_compare_e7_(a_a_[2],b_b_[2]);
                       if(x_d_!=0)return x_d_;
                       break}
                     case _H_:
                      {var x_d_=caml_int64_compare_eX_(a_a_,b_b_);
                       if(x_d_!=0)return x_d_;
                       break}
                     default:
                      if(a_a_.length!=b_b_.length)
                       return a_a_.length<b_b_.length?-1:1;
                      if(a_a_.length>1)stack_e_.push(a_a_,b_b_,1)}}}
              else
               return 1}
           else
            if
             (b_b_ instanceof MlString_J_||
              b_b_ instanceof Array&&
              b_b_[0]===
              (b_b_[0]|0))
             {return -1}
            else
             {if(a_a_<b_b_)return -1;
              if(a_a_>b_b_)return 1;
              if(total_c_&&a_a_!=b_b_)
               {if(a_a_==a_a_)return 1;if(b_b_==b_b_)return -1}}}
        if(stack_e_.length==0)return 0;
        var i_f_=stack_e_.pop();
        b_b_=stack_e_.pop();
        a_a_=stack_e_.pop();
        if(i_f_+1<a_a_.length)stack_e_.push(a_a_,b_b_,i_f_+1);
        a_a_=a_a_[i_f_];
        b_b_=b_b_[i_f_]}}
    function caml_int_compare_e7_(a_a_,b_b_)
     {if(a_a_<b_b_)return -1;if(a_a_==b_b_)return 0;return 1}
    function caml_int64_compare_eX_(x_a_,y_b_)
     {var x3_c_=x_a_[3]<<16,y3_d_=y_b_[3]<<16;
      if(x3_c_>y3_d_)return 1;
      if(x3_c_<y3_d_)return -1;
      if(x_a_[2]>y_b_[2])return 1;
      if(x_a_[2]<y_b_[2])return -1;
      if(x_a_[1]>y_b_[1])return 1;
      if(x_a_[1]<y_b_[1])return -1;
      return 0}
    function caml_mul_fh_(x_a_,y_b_)
     {return ((x_a_>>16)*y_b_<<16)+(x_a_&_aL_)*y_b_|0}
    function caml_mod_fg_(x_a_,y_b_)
     {if(y_b_==0)caml_raise_zero_divide_fp_();return x_a_%y_b_}
    function caml_raise_zero_divide_fp_()
     {caml_raise_constant_cP_(caml_global_data_al_[6])}
    function caml_raise_constant_cP_(tag_a_){throw [0,tag_a_]}
    function caml_ml_output_char_ff_(oc_a_,c_b_)
     {var s_c_=caml_new_string_cN_(String.fromCharCode(c_b_));
      caml_ml_output_cM_(oc_a_,s_c_,0,1)}
    function caml_new_string_cN_(x_a_){return new MlString_J_(x_a_)}
    function caml_ml_output_cM_(oc_a_,buffer_b_,offset_c_,len_d_)
     {var string_f_;
      if(offset_c_==0&&buffer_b_.getLen()==len_d_)
       string_f_=buffer_b_;
      else
       {string_f_=caml_create_string_cI_(len_d_);
        caml_blit_string_cG_(buffer_b_,offset_c_,string_f_,0,len_d_)}
      var
       jsstring_e_=string_f_.toString(),
       id_g_=jsstring_e_.lastIndexOf("\n");
      if(id_g_<0)
       caml_ml_output_buffer_Y_+=jsstring_e_;
      else
       {caml_ml_output_buffer_Y_+=jsstring_e_.substr(0,id_g_);
        caml_ml_flush_cL_(oc_a_);
        caml_ml_output_buffer_Y_+=jsstring_e_.substr(id_g_+1)}}
    function caml_ml_out_channels_list_fe_(){return 0}
    function caml_ml_open_descriptor_out_fd_(x_a_){return x_a_}
    function caml_ml_flush_cL_(oc_a_)
     {joo_global_object_c_.console&&
      joo_global_object_c_.console.log&&
      caml_ml_output_buffer_Y_!=
      _r_&&
      joo_global_object_c_.console.log(caml_ml_output_buffer_Y_);
      caml_ml_output_buffer_Y_=_r_}
    var caml_ml_output_buffer_Y_=_r_;
    function caml_make_vect_fc_(len_a_,init_b_)
     {var b_d_=[0];
      for(var i_c_=1;i_c_<=len_a_;i_c_++)b_d_[i_c_]=init_b_;
      return b_d_}
    function caml_js_wrap_callback_fb_(f_a_)
     {var toArray_c_=Array.prototype.slice;
      return function()
       {var args_b_=arguments.length>0?toArray_c_.call(arguments):[undefined];
        return caml_call_gen_K_(f_a_,args_b_)}}
    var caml_js_regexps_aR_={amp:/&/g,lt:/</g,quot:/\"/g,all:/[&<\"]/};
    function caml_js_html_escape_fa_(s_a_)
     {if(!caml_js_regexps_aR_.all.test(s_a_))return s_a_;
      return s_a_.replace(caml_js_regexps_aR_.amp,"&amp;").replace
               (caml_js_regexps_aR_.lt,"&lt;").replace
              (caml_js_regexps_aR_.quot,"&quot;")}
    function caml_js_get_console_e$_()
     {var
       c_b_=joo_global_object_c_.console?joo_global_object_c_.console:{},
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
       if(!c_b_[m_d_[i_a_]])c_b_[m_d_[i_a_]]=f_e_;
      return c_b_}
    function caml_js_call_e__(f_a_,o_b_,args_c_)
     {return f_a_.apply(o_b_,args_c_.slice(1))}
    function caml_is_printable_e9_(c_a_){return +(c_a_>31&&c_a_<127)}
    function caml_int_of_string_e8_(s_a_)
     {var
       r_g_=caml_parse_sign_and_base_fm_(s_a_),
       i_e_=r_g_[0],
       sign_h_=r_g_[1],
       base_f_=r_g_[2],
       threshold_i_=-1>>>0,
       c_d_=s_a_.get(i_e_),
       d_c_=caml_parse_digit_cO_(c_d_);
      if(d_c_<0||d_c_>=base_f_)caml_failwith_aP_(_aI_);
      var res_b_=d_c_;
      for(;;)
       {i_e_++;
        c_d_=s_a_.get(i_e_);
        if(c_d_==95)continue;
        d_c_=caml_parse_digit_cO_(c_d_);
        if(d_c_<0||d_c_>=base_f_)break;
        res_b_=base_f_*res_b_+d_c_;
        if(res_b_>threshold_i_)caml_failwith_aP_(_aI_)}
      if(i_e_!=s_a_.getLen())caml_failwith_aP_(_aI_);
      res_b_=sign_h_*res_b_;
      if((res_b_|0)!=res_b_)caml_failwith_aP_(_aI_);
      return res_b_}
    function caml_failwith_aP_(msg_a_)
     {caml_raise_with_string_cQ_(caml_global_data_al_[3],msg_a_)}
    var caml_global_data_al_=[0];
    function caml_parse_digit_cO_(c_a_)
     {if(c_a_>=48&&c_a_<=57)return c_a_-48;
      if(c_a_>=65&&c_a_<=90)return c_a_-55;
      if(c_a_>=97&&c_a_<=122)return c_a_-87;
      return -1}
    function caml_parse_sign_and_base_fm_(s_a_)
     {var i_b_=0,base_c_=10,sign_d_=s_a_.get(0)==45?(i_b_++,-1):1;
      if(s_a_.get(i_b_)==48)
       switch(s_a_.get(i_b_+1))
        {case _bq_:
         case 88:base_c_=16;i_b_+=2;break;
         case _bs_:
         case 79:base_c_=8;i_b_+=2;break;
         case 98:
         case 66:base_c_=2;i_b_+=2;break
         }
      return [i_b_,sign_d_,base_c_]}
    function caml_int64_format_eY_(fmt_a_,x_b_)
     {var f_c_=caml_parse_format_bx_(fmt_a_);
      if(f_c_.signedconv&&caml_int64_is_negative_eZ_(x_b_))
       {f_c_.sign=-1;x_b_=caml_int64_neg_e2_(x_b_)}
      var
       buffer_d_=_r_,
       wbase_h_=caml_int64_of_int32_e3_(f_c_.base),
       cvtbl_g_="0123456789abcdef";
      do
       {var p_f_=caml_int64_udivmod_e6_(x_b_,wbase_h_);
        x_b_=p_f_[1];
        buffer_d_=cvtbl_g_.charAt(caml_int64_to_int32_e5_(p_f_[2]))+buffer_d_}
      while
       (!caml_int64_is_zero_e0_(x_b_));
      if(f_c_.prec>=0)
       {f_c_.filler=_A_;
        var n_e_=f_c_.prec-buffer_d_.length;
        if(n_e_>0)buffer_d_=caml_str_repeat_am_(n_e_,_s_)+buffer_d_}
      return caml_finish_formatting_bw_(f_c_,buffer_d_)}
    function caml_int64_neg_e2_(x_a_)
     {var
       y1_b_=-x_a_[1],
       y2_c_=-x_a_[2]+(y1_b_>>24),
       y3_d_=-x_a_[3]+(y2_c_>>24);
      return [_H_,y1_b_&_w_,y2_c_&_w_,y3_d_&_aL_]}
    function caml_int64_is_negative_eZ_(x_a_){return x_a_[3]<<16<0}
    function caml_int64_to_int32_e5_(x_a_){return x_a_[1]|x_a_[2]<<24}
    function caml_int64_udivmod_e6_(x_a_,y_b_)
     {var
       offset_e_=0,
       modulus_d_=x_a_.slice(),
       divisor_c_=y_b_.slice(),
       quotient_f_=[_H_,0,0,0];
      while(caml_int64_ucompare_cK_(modulus_d_,divisor_c_)>0)
       {offset_e_++;caml_int64_lsl1_cJ_(divisor_c_)}
      while(offset_e_>=0)
       {offset_e_--;
        caml_int64_lsl1_cJ_(quotient_f_);
        if(caml_int64_ucompare_cK_(modulus_d_,divisor_c_)>=0)
         {quotient_f_[1]++;
          modulus_d_=caml_int64_sub_e4_(modulus_d_,divisor_c_)}
        caml_int64_lsr1_e1_(divisor_c_)}
      return [0,quotient_f_,modulus_d_]}
    function caml_int64_lsr1_e1_(x_a_)
     {x_a_[1]=(x_a_[1]>>>1|x_a_[2]<<23)&_w_;
      x_a_[2]=(x_a_[2]>>>1|x_a_[3]<<23)&_w_;
      x_a_[3]=x_a_[3]>>>1}
    function caml_int64_lsl1_cJ_(x_a_)
     {x_a_[3]=x_a_[3]<<1|x_a_[2]>>23;
      x_a_[2]=(x_a_[2]<<1|x_a_[1]>>23)&_w_;
      x_a_[1]=x_a_[1]<<1&_w_}
    function caml_int64_ucompare_cK_(x_a_,y_b_)
     {if(x_a_[3]>y_b_[3])return 1;
      if(x_a_[3]<y_b_[3])return -1;
      if(x_a_[2]>y_b_[2])return 1;
      if(x_a_[2]<y_b_[2])return -1;
      if(x_a_[1]>y_b_[1])return 1;
      if(x_a_[1]<y_b_[1])return -1;
      return 0}
    function caml_int64_sub_e4_(x_a_,y_b_)
     {var
       z1_c_=x_a_[1]-y_b_[1],
       z2_d_=x_a_[2]-y_b_[2]+(z1_c_>>24),
       z3_e_=x_a_[3]-y_b_[3]+(z2_d_>>24);
      return [_H_,z1_c_&_w_,z2_d_&_w_,z3_e_&_aL_]}
    function caml_int64_of_int32_e3_(x_a_)
     {return [_H_,x_a_&_w_,x_a_>>24&_w_,x_a_>>31&_aL_]}
    function caml_int64_is_zero_e0_(x_a_){return (x_a_[3]|x_a_[2]|x_a_[1])==0}
    function caml_get_exception_backtrace_eW_(){return 0}
    function caml_format_int_eV_(fmt_a_,i_b_)
     {if(fmt_a_.toString()==_cy_)return new MlWrappedString_X_(_r_+i_b_);
      var f_c_=caml_parse_format_bx_(fmt_a_);
      if(i_b_<0){if(f_c_.signedconv){f_c_.sign=-1;i_b_=-i_b_}else i_b_>>>=0}
      var s_d_=i_b_.toString(f_c_.base);
      if(f_c_.prec>=0)
       {f_c_.filler=_A_;
        var n_e_=f_c_.prec-s_d_.length;
        if(n_e_>0)s_d_=caml_str_repeat_am_(n_e_,_s_)+s_d_}
      return caml_finish_formatting_bw_(f_c_,s_d_)}
    function caml_format_float_eU_(fmt_a_,x_b_)
     {var
       s_c_,
       f_f_=caml_parse_format_bx_(fmt_a_),
       prec_e_=f_f_.prec<0?6:f_f_.prec;
      if(x_b_<0){f_f_.sign=-1;x_b_=-x_b_}
      if(isNaN(x_b_))
       {s_c_="nan";f_f_.filler=_A_}
      else
       if(!isFinite(x_b_))
        {s_c_="inf";f_f_.filler=_A_}
       else
        switch(f_f_.conv)
         {case _ai_:
           var s_c_=x_b_.toExponential(prec_e_),i_d_=s_c_.length;
           if(s_c_.charAt(i_d_-3)==_ai_)
            s_c_=s_c_.slice(0,i_d_-1)+_s_+s_c_.slice(i_d_-1);
           break;
          case _bn_:s_c_=x_b_.toFixed(prec_e_);break;
          case _cz_:
           prec_e_=prec_e_?prec_e_:1;
           s_c_=x_b_.toExponential(prec_e_-1);
           var j_i_=s_c_.indexOf(_ai_),exp_h_=+s_c_.slice(j_i_+1);
           if(exp_h_<-4||x_b_.toFixed(0).length>prec_e_)
            {var i_d_=j_i_-1;
             while(s_c_.charAt(i_d_)==_s_)i_d_--;
             if(s_c_.charAt(i_d_)==_aM_)i_d_--;
             s_c_=s_c_.slice(0,i_d_+1)+s_c_.slice(j_i_);
             i_d_=s_c_.length;
             if(s_c_.charAt(i_d_-3)==_ai_)
              s_c_=s_c_.slice(0,i_d_-1)+_s_+s_c_.slice(i_d_-1);
             break}
           else
            {var p_g_=prec_e_;
             if(exp_h_<0)
              {p_g_-=exp_h_+1;s_c_=x_b_.toFixed(p_g_)}
             else
              while(s_c_=x_b_.toFixed(p_g_),s_c_.length>prec_e_+1)p_g_--;
             if(p_g_)
              {var i_d_=s_c_.length-1;
               while(s_c_.charAt(i_d_)==_s_)i_d_--;
               if(s_c_.charAt(i_d_)==_aM_)i_d_--;
               s_c_=s_c_.slice(0,i_d_+1)}}
           break
          }
      return caml_finish_formatting_bw_(f_f_,s_c_)}
    function caml_finish_formatting_bw_(f_a_,rawbuffer_b_)
     {if(f_a_.uppercase)rawbuffer_b_=rawbuffer_b_.toUpperCase();
      var len_e_=rawbuffer_b_.length;
      if(f_a_.signedconv&&(f_a_.sign<0||f_a_.signstyle!=_O_))len_e_++;
      if(f_a_.alternate){if(f_a_.base==8)len_e_+=1;if(f_a_.base==16)len_e_+=2}
      var buffer_c_=_r_;
      if(f_a_.justify==_aK_&&f_a_.filler==_A_)
       for(var i_d_=len_e_;i_d_<f_a_.width;i_d_++)buffer_c_+=_A_;
      if(f_a_.signedconv)
       {if(f_a_.sign<0)
         buffer_c_+=_O_;
        else
         if(f_a_.signstyle!=_O_)buffer_c_+=f_a_.signstyle}
      if(f_a_.alternate&&f_a_.base==8)buffer_c_+=_s_;
      if(f_a_.alternate&&f_a_.base==16)buffer_c_+="0x";
      if(f_a_.justify==_aK_&&f_a_.filler==_s_)
       for(var i_d_=len_e_;i_d_<f_a_.width;i_d_++)buffer_c_+=_s_;
      buffer_c_+=rawbuffer_b_;
      if(f_a_.justify==_O_)
       for(var i_d_=len_e_;i_d_<f_a_.width;i_d_++)buffer_c_+=_A_;
      return new MlWrappedString_X_(buffer_c_)}
    function caml_parse_format_bx_(fmt_a_)
     {fmt_a_=fmt_a_.toString();
      var len_e_=fmt_a_.length;
      if(len_e_>31)caml_invalid_argument_aQ_("format_int: format too long");
      var
       f_b_=
        {justify:_aK_,
         signstyle:_O_,
         filler:_A_,
         alternate:false,
         base:0,
         signedconv:false,
         width:0,
         uppercase:false,
         sign:1,
         prec:-1,
         conv:_bn_};
      for(var i_d_=0;i_d_<len_e_;i_d_++)
       {var c_c_=fmt_a_.charAt(i_d_);
        switch(c_c_)
         {case _O_:f_b_.justify=_O_;break;
          case _aK_:
          case _A_:f_b_.signstyle=c_c_;break;
          case _s_:f_b_.filler=_s_;break;
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
          case _aM_:
           f_b_.prec=0;
           i_d_++;
           while(c_c_=fmt_a_.charCodeAt(i_d_)-48,c_c_>=0&&c_c_<=9)
            {f_b_.prec=f_b_.prec*10+c_c_;i_d_++}
           i_d_--;
          case "d":
          case "i":f_b_.signedconv=true;
          case "u":f_b_.base=10;break;
          case _cA_:f_b_.base=16;break;
          case "X":f_b_.base=16;f_b_.uppercase=true;break;
          case "o":f_b_.base=8;break;
          case _ai_:
          case _bn_:
          case _cz_:f_b_.signedconv=true;f_b_.conv=c_c_;break;
          case "E":
          case "F":
          case "G":
           f_b_.signedconv=true;
           f_b_.uppercase=true;
           f_b_.conv=c_c_.toLowerCase();
           break
          }}
      return f_b_}
    function caml_fill_string_eT_(s_a_,i_b_,l_c_,c_d_)
     {s_a_.fill(i_b_,l_c_,c_d_)}
    function caml_create_string_cI_(len_a_)
     {if(len_a_<0)caml_invalid_argument_aQ_("String.create");
      return new MlMakeString_cF_(len_a_)}
    function caml_classify_float_eS_(x_a_)
     {if(isFinite(x_a_))
       {if(Math.abs(x_a_)>=2.22507385850720138e-308)return 0;
        if(x_a_!=0)return 1;
        return 2}
      return isNaN(x_a_)?4:3}
    function caml_call_gen_K_(f_c_,args_b_)
     {if(f_c_.fun)return caml_call_gen_K_(f_c_.fun,args_b_);
      var n_a_=f_c_.length,d_d_=n_a_-args_b_.length;
      if(d_d_==0)
       return f_c_.apply(null,args_b_);
      else
       if(d_d_<0)
        return caml_call_gen_K_
                (f_c_.apply(null,args_b_.slice(0,n_a_)),args_b_.slice(n_a_));
       else
        return function(x_a_)
         {return caml_call_gen_K_(f_c_,args_b_.concat([x_a_]))}}
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
      if(!a_g_)a_g_=s2_c_.toArray();else{s2_c_.bytes=s2_c_.string=null}
      s1_a_.blitToArray(i1_b_,a_g_,i2_d_,len_e_)}
    function caml_array_set_eR_(array_a_,index_b_,newval_c_)
     {if(index_b_<0||index_b_>=array_a_.length-1)caml_array_bound_error_aO_();
      array_a_[index_b_+1]=newval_c_;
      return 0}
    function caml_array_get_eQ_(array_a_,index_b_)
     {if(index_b_<0||index_b_>=array_a_.length-1)caml_array_bound_error_aO_();
      return array_a_[index_b_+1]}
    function caml_str_repeat_am_(n_a_,s_b_)
     {if(!n_a_){return _r_}
      if(n_a_&1){return caml_str_repeat_am_(n_a_-1,s_b_)+s_b_}
      var r_c_=caml_str_repeat_am_(n_a_>>1,s_b_);
      return r_c_+r_c_}
    function MlString_J_(param_a_)
     {if(param_a_!=null)
       {this.bytes=this.fullBytes=param_a_;this.last=this.len=param_a_.length}}
    MlString_J_.prototype=
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
        {joo_global_object_c_.console&&
         joo_global_object_c_.console.error&&
         joo_global_object_c_.console.error
          ('MlString.toJsString: wrong encoding for \"%s\" ',a_a_);
         return a_a_}},
     toBytes:
     function()
      {if(this.string!=null)
        {try
          {var b_a_=unescape(encodeURIComponent(this.string))}
         catch(e_f_)
          {joo_global_object_c_.console&&
           joo_global_object_c_.console.error&&
           joo_global_object_c_.console.error
            ('MlString.toBytes: wrong encoding for \"%s\" ',this.string);
           var b_a_=this.string}}
       else
        {var b_a_=_r_,a_d_=this.array,l_e_=a_d_.length;
         for(var i_b_=0;i_b_<l_e_;i_b_++)b_a_+=String.fromCharCode(a_d_[i_b_])}
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
        {this.bytes=b_a_+=caml_str_repeat_am_(this.len-this.last,"\0");
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
        {if(i2_c_<=i1_a_)
          {for(var i_e_=0;i_e_<l_d_;i_e_++)a2_b_[i2_c_+i_e_]=a1_g_[i1_a_+i_e_]}
         else
          {for(var i_e_=l_d_-1;i_e_>=0;i_e_--)
            a2_b_[i2_c_+i_e_]=a1_g_[i1_a_+i_e_]}}
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
       if(i_a_<0||i_a_>=this.len)caml_array_bound_error_aO_();
       return this.get(i_a_)},
     set:
     function(i_a_,c_b_)
      {var a_c_=this.array;
       if(!a_c_)
        {if(this.last==i_a_)
          {this.bytes+=String.fromCharCode(c_b_&_H_);this.last++;return 0}
         a_c_=this.toArray()}
       else
        if(this.bytes!=null){this.bytes=this.fullBytes=this.string=null}
       a_c_[i_a_]=c_b_&_H_;
       return 0},
     safeSet:
     function(i_a_,c_b_)
      {if(this.len==null)this.toBytes();
       if(i_a_<0||i_a_>=this.len)caml_array_bound_error_aO_();
       this.set(i_a_,c_b_)},
     fill:
     function(ofs_a_,len_b_,c_c_)
      {if(ofs_a_>=this.last&&this.last&&c_c_==0)return;
       var a_d_=this.array;
       if(!a_d_)
        a_d_=this.toArray();
       else
        if(this.bytes!=null){this.bytes=this.fullBytes=this.string=null}
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
    function MlWrappedString_X_(s_a_){this.string=s_a_}
    MlWrappedString_X_.prototype=new MlString_J_();
    function MlMakeString_cF_(l_a_){this.bytes=_r_;this.len=l_a_}
    MlMakeString_cF_.prototype=new MlString_J_();
    function MlStringFromArray_eP_(a_a_)
     {var len_b_=a_a_.length;this.array=a_a_;this.len=this.last=len_b_}
    MlStringFromArray_eP_.prototype=new MlString_J_();
    function caml_array_bound_error_aO_()
     {caml_invalid_argument_aQ_("index out of bounds")}
    function caml_invalid_argument_aQ_(msg_a_)
     {caml_raise_with_string_cQ_(caml_global_data_al_[4],msg_a_)}
    function caml_raise_with_string_cQ_(tag_a_,msg_b_)
     {caml_raise_with_arg_fo_(tag_a_,new MlWrappedString_X_(msg_b_))}
    function caml_raise_with_arg_fo_(tag_a_,arg_b_){throw [0,tag_a_,arg_b_]}
    var
     _F_=_r_,
     _aC_='"',
     _co_="'",
     _cp_="''",
     _cn_=_aM_,
     _cw_="copy",
     _eO_="false",
     _cq_="input",
     _aD_="planet.ml",
     _W_="src/core/lwt.ml",
     _eN_="true",
     caml_array_get_j_=caml_array_get_eQ_,
     caml_array_set_h_=caml_array_set_eR_,
     caml_blit_string_aF_=caml_blit_string_cG_,
     caml_create_string_G_=caml_create_string_cI_,
     caml_format_float_bi_=caml_format_float_eU_,
     caml_format_int_aG_=caml_format_int_eV_,
     caml_is_printable_bj_=caml_is_printable_e9_,
     caml_js_html_escape_cv_=caml_js_html_escape_fa_,
     caml_js_wrap_callback_bl_=caml_js_wrap_callback_fb_,
     caml_make_vect_v_=caml_make_vect_fc_,
     caml_ml_flush_cr_=caml_ml_flush_cL_,
     caml_ml_output_char_cs_=caml_ml_output_char_ff_,
     caml_mod_ag_=caml_mod_fg_,
     caml_new_string_b_=caml_new_string_cN_,
     caml_obj_tag_ct_=caml_obj_tag_fl_,
     caml_register_global_aE_=caml_register_global_fq_,
     caml_sys_getenv_cu_=caml_sys_getenv_fu_;
    function caml_call_gen1_i_(_a_,_b_)
     {return _a_.length==1?_a_(_b_):caml_call_gen_K_(_a_,[_b_])}
    function caml_call_gen2_n_(_a_,_b_,_c_)
     {return _a_.length==2?_a_(_b_,_c_):caml_call_gen_K_(_a_,[_b_,_c_])}
    function caml_call_gen3_k_(_a_,_b_,_c_,_d_)
     {return _a_.length==3
              ?_a_(_b_,_c_,_d_)
              :caml_call_gen_K_(_a_,[_b_,_c_,_d_])}
    function caml_call_gen5_aH_(_a_,_b_,_c_,_d_,_e_,_f_)
     {return _a_.length==5
              ?_a_(_b_,_c_,_d_,_e_,_f_)
              :caml_call_gen_K_(_a_,[_b_,_c_,_d_,_e_,_f_])}
    var
     _aT_=[0,caml_new_string_b_("Failure")],
     _by_=[0,caml_new_string_b_("Invalid_argument")],
     ___=[0,caml_new_string_b_("Not_found")],
     _m_=[0,caml_new_string_b_("Assert_failure")],
     _a2_=caml_new_string_b_('File "%s", line %d, characters %d-%d: %s');
    caml_register_global_aE_(6,___);
    caml_register_global_aE_(5,[0,caml_new_string_b_("Division_by_zero")]);
    caml_register_global_aE_(3,_by_);
    caml_register_global_aE_(2,_aT_);
    var
     _dB_=[0,caml_new_string_b_("Out_of_memory")],
     _dF_=[0,caml_new_string_b_("Match_failure")],
     _dD_=[0,caml_new_string_b_("Stack_overflow")],
     _dI_=[0,caml_new_string_b_("Undefined_recursive_module")],
     _cU_=caml_new_string_b_("%.12g"),
     _cT_=caml_new_string_b_(_cn_),
     _cR_=caml_new_string_b_(_eN_),
     _cS_=caml_new_string_b_(_eO_),
     _cV_=caml_new_string_b_("Pervasives.do_at_exit"),
     _cZ_=caml_new_string_b_("\\b"),
     _c0_=caml_new_string_b_("\\t"),
     _c1_=caml_new_string_b_("\\n"),
     _c2_=caml_new_string_b_("\\r"),
     _cY_=caml_new_string_b_("\\\\"),
     _cX_=caml_new_string_b_("\\'"),
     _c5_=caml_new_string_b_("String.contains_from"),
     _c4_=caml_new_string_b_("String.blit"),
     _c3_=caml_new_string_b_("String.sub"),
     _c8_=caml_new_string_b_("Queue.Empty"),
     _c__=caml_new_string_b_("Buffer.add: cannot grow buffer"),
     _dl_=caml_new_string_b_(_F_),
     _dm_=caml_new_string_b_(_F_),
     _dp_=caml_new_string_b_(_aC_),
     _dq_=caml_new_string_b_(_aC_),
     _dn_=caml_new_string_b_(_co_),
     _do_=caml_new_string_b_(_co_),
     _dk_=caml_new_string_b_(_cn_),
     _dj_=caml_new_string_b_("printf: bad positional specification (0)."),
     _di_=caml_new_string_b_("%_"),
     _dh_=[0,caml_new_string_b_("printf.ml"),144,8],
     _df_=caml_new_string_b_(_cp_),
     _dg_=caml_new_string_b_("Printf: premature end of format string ``"),
     _db_=caml_new_string_b_(_cp_),
     _dc_=caml_new_string_b_(" in format string ``"),
     _dd_=caml_new_string_b_(", at char number "),
     _de_=caml_new_string_b_("Printf: bad conversion %"),
     _c$_=caml_new_string_b_("Sformat.index_of_int: negative argument "),
     _dv_=caml_new_string_b_(_F_),
     _dw_=caml_new_string_b_(", %s%s"),
     _dQ_=[1,1],
     _dR_=caml_new_string_b_("%s\n"),
     _dS_=
      caml_new_string_b_
       ("(Program not linked with -g, cannot print stack backtrace)\n"),
     _dK_=caml_new_string_b_("Raised at"),
     _dN_=caml_new_string_b_("Re-raised at"),
     _dO_=caml_new_string_b_("Raised by primitive operation at"),
     _dP_=caml_new_string_b_("Called from"),
     _dL_=caml_new_string_b_('%s file "%s", line %d, characters %d-%d'),
     _dM_=caml_new_string_b_("%s unknown location"),
     _dC_=caml_new_string_b_("Out of memory"),
     _dE_=caml_new_string_b_("Stack overflow"),
     _dG_=caml_new_string_b_("Pattern matching failed"),
     _dH_=caml_new_string_b_("Assertion failed"),
     _dJ_=caml_new_string_b_("Undefined recursive module"),
     _dx_=caml_new_string_b_("(%s%s)"),
     _dy_=caml_new_string_b_(_F_),
     _dz_=caml_new_string_b_(_F_),
     _dA_=caml_new_string_b_("(%s)"),
     _du_=caml_new_string_b_(_cy_),
     _ds_=caml_new_string_b_("%S"),
     _dt_=caml_new_string_b_("_"),
     _eL_=caml_new_string_b_("OCAMLRUNPARAM"),
     _eJ_=caml_new_string_b_("CAMLRUNPARAM"),
     _dT_=caml_new_string_b_(_F_),
     _d3_=[0,caml_new_string_b_(_W_),814,20],
     _d4_=[0,caml_new_string_b_(_W_),816,8],
     _d1_=[0,caml_new_string_b_(_W_),648,20],
     _d2_=[0,caml_new_string_b_(_W_),651,8],
     _d0_=[0,caml_new_string_b_(_W_),498,8],
     _dZ_=[0,caml_new_string_b_(_W_),487,9],
     _dY_=caml_new_string_b_("Lwt.wakeup_result"),
     _dX_=caml_new_string_b_("Fatal error: exception "),
     _dV_=caml_new_string_b_("Lwt.Canceled"),
     _el_=caml_new_string_b_("canvas"),
     _ei_=caml_new_string_b_("img"),
     _eh_=caml_new_string_b_("br"),
     _eg_=caml_new_string_b_("p"),
     _ef_=caml_new_string_b_("div"),
     _ee_=caml_new_string_b_("label"),
     _ed_=caml_new_string_b_(_cq_),
     _ec_=caml_new_string_b_("select"),
     _eb_=caml_new_string_b_("option"),
     _d9_=caml_new_string_b_("mouseup"),
     _d$_=caml_new_string_b_("mousemove"),
     _ej_=caml_new_string_b_("Dom_html.Canvas_not_available"),
     _en_=caml_new_string_b_("Exception during Lwt.async: "),
     _eI_=caml_new_string_b_("% 2.f"),
     _eB_=caml_new_string_b_("Resume"),
     _eC_=caml_new_string_b_("Pause"),
     _eD_=caml_new_string_b_("Fixed position"),
     _eE_=caml_new_string_b_("Follow rotation"),
     _eF_=
      [0,
       caml_new_string_b_("December solstice"),
       [0,
        caml_new_string_b_("Equinox"),
        [0,caml_new_string_b_("June solstice"),0]]],
     _eG_=caml_new_string_b_("Lighting"),
     _eH_=caml_new_string_b_("Clip"),
     _ey_=[0,caml_new_string_b_(_aD_),415,0],
     _ex_=[0,caml_new_string_b_(_aD_),416,0],
     _ew_=[0,caml_new_string_b_(_aD_),417,0],
     _ev_=[0,caml_new_string_b_(_aD_),418,0],
     v_ez_=[_I_,0,0,1];
    function _aS_(s_a_){throw [0,_aT_,s_a_]}
    function _Q_(s_a_){throw [0,_by_,s_a_]}
    function _l_(s1_a_,s2_b_)
     {var
       l1_c_=s1_a_.getLen(),
       l2_e_=s2_b_.getLen(),
       s_d_=caml_create_string_G_(l1_c_+l2_e_|0);
      caml_blit_string_aF_(s1_a_,0,s_d_,0,l1_c_);
      caml_blit_string_aF_(s2_b_,0,s_d_,l1_c_,l2_e_);
      return s_d_}
    function string_of_int_aU_(n_a_){return caml_new_string_b_(_r_+n_a_)}
    function string_of_float_bz_(f_a_)
     {var _c_=caml_format_float_bi_(_cU_,f_a_),i_b_=0,l_f_=_c_.getLen();
      for(;;)
       {if(l_f_<=i_b_)
         var _e_=_l_(_c_,_cT_);
        else
         {var _d_=_c_.safeGet(i_b_),_g_=48<=_d_?58<=_d_?0:1:45===_d_?1:0;
          if(_g_){var i_b_=i_b_+1|0;continue}
          var _e_=_c_}
        return _e_}}
    var stderr_Z_=caml_ml_open_descriptor_out_fd_(2);
    function output_string_bA_(oc_a_,s_b_)
     {return caml_ml_output_cM_(oc_a_,s_b_,0,s_b_.getLen())}
    function prerr_string_bB_(s_a_){return output_string_bA_(stderr_Z_,s_a_)}
    function do_at_exit_aV_(param_a_)
     {var param_b_=caml_ml_out_channels_list_fe_(0);
      for(;;)
       {if(param_b_)
         {var l_c_=param_b_[2],a_d_=param_b_[1];
          try {caml_ml_flush_cr_(a_d_)}catch(_f_){}
          var param_b_=l_c_;
          continue}
        return 0}}
    caml_register_named_value_fr_(_cV_,do_at_exit_aV_);
    function _cW_(_a_,_b_){return caml_ml_output_char_cs_(_a_,_b_)}
    function _bC_(_a_){return caml_ml_flush_cr_(_a_)}
    function _an_(f_a_,a_b_)
     {var l_d_=a_b_.length-1;
      if(0===l_d_)return [0];
      var
       r_e_=caml_make_vect_v_(l_d_,caml_call_gen1_i_(f_a_,a_b_[0+1])),
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
    function _ao_(n_a_,c_b_)
     {var s_c_=caml_create_string_G_(n_a_);
      caml_fill_string_eT_(s_c_,0,n_a_,c_b_);
      return s_c_}
    function _ap_(s_a_,ofs_b_,len_c_)
     {if(0<=ofs_b_&&0<=len_c_&&!((s_a_.getLen()-len_c_|0)<ofs_b_))
       {var r_d_=caml_create_string_G_(len_c_);
        caml_blit_string_aF_(s_a_,ofs_b_,r_d_,0,len_c_);
        return r_d_}
      return _Q_(_c3_)}
    function _aq_(s1_a_,ofs1_b_,s2_c_,ofs2_d_,len_e_)
     {if
       (0<=
        len_e_&&
        0<=
        ofs1_b_&&
        !((s1_a_.getLen()-len_e_|0)<ofs1_b_)&&
        0<=
        ofs2_d_&&
        !((s2_c_.getLen()-len_e_|0)<ofs2_d_))
       return caml_blit_string_aF_(s1_a_,ofs1_b_,s2_c_,ofs2_d_,len_e_);
      return _Q_(_c4_)}
    var
     _aW_=caml_sys_get_config_ft_(0)[2],
     _$_=caml_mul_fh_(_aW_/8|0,(1<<(_aW_-10|0))-1|0)-1|0,
     _c6_=252,
     _c7_=253,
     _c9_=[0,_c8_];
    function _aX_(n_a_)
     {var
       n_b_=1<=n_a_?n_a_:1,
       n_c_=_$_<n_b_?_$_:n_b_,
       s_d_=caml_create_string_G_(n_c_);
      return [0,s_d_,0,n_c_,s_d_]}
    function _aY_(b_a_){return _ap_(b_a_[1],0,b_a_[2])}
    function _bF_(b_a_,more_b_)
     {var new_len_c_=[0,b_a_[3]];
      for(;;)
       {if(new_len_c_[1]<(b_a_[2]+more_b_|0))
         {new_len_c_[1]=2*new_len_c_[1]|0;continue}
        if(_$_<new_len_c_[1])
         if((b_a_[2]+more_b_|0)<=_$_)new_len_c_[1]=_$_;else _aS_(_c__);
        var new_buffer_d_=caml_create_string_G_(new_len_c_[1]);
        _aq_(b_a_[1],0,new_buffer_d_,0,b_a_[2]);
        b_a_[1]=new_buffer_d_;
        b_a_[3]=new_len_c_[1];
        return 0}}
    function _aa_(b_a_,c_b_)
     {var pos_c_=b_a_[2];
      if(b_a_[3]<=pos_c_)_bF_(b_a_,1);
      b_a_[1].safeSet(pos_c_,c_b_);
      b_a_[2]=pos_c_+1|0;
      return 0}
    function _aZ_(b_a_,s_b_)
     {var len_c_=s_b_.getLen(),new_position_d_=b_a_[2]+len_c_|0;
      if(b_a_[3]<new_position_d_)_bF_(b_a_,len_c_);
      _aq_(s_b_,0,b_a_[1],b_a_[2],len_c_);
      b_a_[2]=new_position_d_;
      return 0}
    function index_of_int_a0_(i_a_)
     {return 0<=i_a_?i_a_:_aS_(_l_(_c$_,string_of_int_aU_(i_a_)))}
    function add_int_index_bG_(i_a_,idx_b_)
     {return index_of_int_a0_(i_a_+idx_b_|0)}
    var _da_=1;
    function _bH_(_a_){return add_int_index_bG_(_da_,_a_)}
    function _bI_(fmt_a_){return _ap_(fmt_a_,0,fmt_a_.getLen())}
    function bad_conversion_bJ_(sfmt_a_,i_b_,c_c_)
     {var
       _d_=_l_(_dc_,_l_(sfmt_a_,_db_)),
       _e_=_l_(_dd_,_l_(string_of_int_aU_(i_b_),_d_));
      return _Q_(_l_(_de_,_l_(_ao_(1,c_c_),_e_)))}
    function bad_conversion_format_ab_(fmt_a_,i_b_,c_c_)
     {return bad_conversion_bJ_(_bI_(fmt_a_),i_b_,c_c_)}
    function incomplete_format_as_(fmt_a_)
     {return _Q_(_l_(_dg_,_l_(_bI_(fmt_a_),_df_)))}
    function extract_format_L_(fmt_f_,start_b_,stop_c_,widths_d_)
     {function skip_positional_spec_j_(start_a_)
       {if
         ((fmt_f_.safeGet(start_a_)-48|0)<
          0||
          9<
          (fmt_f_.safeGet(start_a_)-48|0))
         return start_a_;
        var i_b_=start_a_+1|0;
        for(;;)
         {var _c_=fmt_f_.safeGet(i_b_);
          if(48<=_c_)
           {if(!(58<=_c_)){var i_b_=i_b_+1|0;continue}var _d_=0}
          else
           if(36===_c_){var _e_=i_b_+1|0,_d_=1}else var _d_=0;
          if(!_d_)var _e_=start_a_;
          return _e_}}
      var
       start_k_=skip_positional_spec_j_(start_b_+1|0),
       b_g_=_aX_((stop_c_-start_k_|0)+10|0);
      _aa_(b_g_,37);
      var l1_e_=widths_d_,l2_i_=0;
      for(;;)
       {if(l1_e_)
         {var _n_=[0,l1_e_[1],l2_i_],l1_e_=l1_e_[2],l2_i_=_n_;continue}
        var i_a_=start_k_,widths_h_=l2_i_;
        for(;;)
         {if(i_a_<=stop_c_)
           {var _l_=fmt_f_.safeGet(i_a_);
            if(42===_l_)
             {if(widths_h_)
               {var t_o_=widths_h_[2];
                _aZ_(b_g_,string_of_int_aU_(widths_h_[1]));
                var i_a_=skip_positional_spec_j_(i_a_+1|0),widths_h_=t_o_;
                continue}
              throw [0,_m_,_dh_]}
            _aa_(b_g_,_l_);
            var i_a_=i_a_+1|0;
            continue}
          return _aY_(b_g_)}}}
    function extract_format_int_bK_(conv_a_,fmt_b_,start_c_,stop_d_,widths_e_)
     {var sfmt_f_=extract_format_L_(fmt_b_,start_c_,stop_d_,widths_e_);
      if(78!==conv_a_&&_aj_!==conv_a_)return sfmt_f_;
      sfmt_f_.safeSet(sfmt_f_.getLen()-1|0,_bp_);
      return sfmt_f_}
    function sub_format_for_printf_bL_(conv_a_)
     {return function(_c_,_b_)
       {var len_m_=_c_.getLen();
        function sub_fmt_n_(c_a_,i_b_)
         {var close_o_=40===c_a_?41:_bo_;
          function sub_k_(j_a_)
           {var j_d_=j_a_;
            for(;;)
             {if(len_m_<=j_d_)return incomplete_format_as_(_c_);
              if(37===_c_.safeGet(j_d_))
               {var _e_=j_d_+1|0;
                if(len_m_<=_e_)
                 var _f_=incomplete_format_as_(_c_);
                else
                 {var _g_=_c_.safeGet(_e_),_h_=_g_-40|0;
                  if(_h_<0||1<_h_)
                   {var _l_=_h_-83|0;
                    if(_l_<0||2<_l_)
                     var _j_=1;
                    else
                     switch(_l_)
                      {case 1:var _j_=1;break;
                       case 2:var _i_=1,_j_=0;break;
                       default:var _i_=0,_j_=0}
                    if(_j_){var _f_=sub_k_(_e_+1|0),_i_=2}}
                  else
                   var _i_=0===_h_?0:1;
                  switch(_i_)
                   {case 1:
                     var
                      _f_=
                       _g_===close_o_
                        ?_e_+1|0
                        :bad_conversion_format_ab_(_c_,i_b_,_g_);
                     break;
                    case 2:break;
                    default:var _f_=sub_k_(sub_fmt_n_(_g_,_e_+1|0)+1|0)}}
                return _f_}
              var j_d_=j_d_+1|0;
              continue}}
          return sub_k_(i_b_)}
        return sub_fmt_n_(conv_a_,_b_)}}
    function iter_on_format_args_bM_(fmt_i_,add_conv_b_,add_char_c_)
     {var lim_m_=fmt_i_.getLen()-1|0;
      function scan_fmt_s_(i_a_)
       {var i_l_=i_a_;
        a:
        for(;;)
         {if(i_l_<lim_m_)
           {if(37===fmt_i_.safeGet(i_l_))
             {var skip_e_=0,i_h_=i_l_+1|0;
              for(;;)
               {if(lim_m_<i_h_)
                 var _w_=incomplete_format_as_(fmt_i_);
                else
                 {var _o_=fmt_i_.safeGet(i_h_);
                  if(58<=_o_)
                   {if(95===_o_){var skip_e_=1,i_h_=i_h_+1|0;continue}}
                  else
                   if(32<=_o_)
                    switch(_o_-32|0)
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
                       var i_h_=caml_call_gen3_k_(add_conv_b_,skip_e_,i_h_,_P_);
                       continue;
                      default:var i_h_=i_h_+1|0;continue}
                  var i_d_=i_h_;
                  b:
                  for(;;)
                   {if(lim_m_<i_d_)
                     var _f_=incomplete_format_as_(fmt_i_);
                    else
                     {var _j_=fmt_i_.safeGet(i_d_);
                      if(126<=_j_)
                       var _g_=0;
                      else
                       switch(_j_)
                        {case 78:
                         case 88:
                         case _aN_:
                         case _P_:
                         case _bs_:
                         case _bp_:
                         case _bq_:
                          var
                           _f_=caml_call_gen3_k_(add_conv_b_,skip_e_,i_d_,_P_),
                           _g_=1;
                          break;
                         case 69:
                         case 70:
                         case 71:
                         case _cE_:
                         case _bu_:
                         case _bt_:
                          var
                           _f_=caml_call_gen3_k_(add_conv_b_,skip_e_,i_d_,_bu_),
                           _g_=1;
                          break;
                         case 33:
                         case 37:
                         case 44:
                         case 64:var _f_=i_d_+1|0,_g_=1;break;
                         case 83:
                         case 91:
                         case _ak_:
                          var
                           _f_=caml_call_gen3_k_(add_conv_b_,skip_e_,i_d_,_ak_),
                           _g_=1;
                          break;
                         case 97:
                         case _aJ_:
                         case _bm_:
                          var
                           _f_=caml_call_gen3_k_(add_conv_b_,skip_e_,i_d_,_j_),
                           _g_=1;
                          break;
                         case 76:
                         case _bv_:
                         case _aj_:
                          var j_t_=i_d_+1|0;
                          if(lim_m_<j_t_)
                           {var
                             _f_=caml_call_gen3_k_(add_conv_b_,skip_e_,i_d_,_P_),
                             _g_=1}
                          else
                           {var _q_=fmt_i_.safeGet(j_t_)-88|0;
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
                                 _f_=
                                  caml_call_gen2_n_
                                   (add_char_c_,
                                    caml_call_gen3_k_(add_conv_b_,skip_e_,i_d_,_j_),
                                    _P_),
                                 _g_=1,
                                 _r_=0;
                                break;
                               default:var _r_=1}
                            if(_r_)
                             {var
                               _f_=caml_call_gen3_k_(add_conv_b_,skip_e_,i_d_,_P_),
                               _g_=1}}
                          break;
                         case 67:
                         case 99:
                          var
                           _f_=caml_call_gen3_k_(add_conv_b_,skip_e_,i_d_,99),
                           _g_=1;
                          break;
                         case 66:
                         case 98:
                          var
                           _f_=caml_call_gen3_k_(add_conv_b_,skip_e_,i_d_,66),
                           _g_=1;
                          break;
                         case 41:
                         case _bo_:
                          var
                           _f_=caml_call_gen3_k_(add_conv_b_,skip_e_,i_d_,_j_),
                           _g_=1;
                          break;
                         case 40:
                          var
                           _f_=
                            scan_fmt_s_(caml_call_gen3_k_(add_conv_b_,skip_e_,i_d_,_j_)),
                           _g_=1;
                          break;
                         case _br_:
                          var
                           i_u_=caml_call_gen3_k_(add_conv_b_,skip_e_,i_d_,_j_),
                           j_v_=
                            caml_call_gen2_n_
                             (sub_format_for_printf_bL_(_j_),fmt_i_,i_u_),
                           i_p_=i_u_;
                          for(;;)
                           {if(i_p_<(j_v_-2|0))
                             {var
                               i_p_=
                                caml_call_gen2_n_(add_char_c_,i_p_,fmt_i_.safeGet(i_p_));
                              continue}
                            var i_d_=j_v_-1|0;
                            continue b}
                         default:var _g_=0}
                      if(!_g_)var _f_=bad_conversion_format_ab_(fmt_i_,i_d_,_j_)}
                    var _w_=_f_;
                    break}}
                var i_l_=_w_;
                continue a}}
            var i_l_=i_l_+1|0;
            continue}
          return i_l_}}
      scan_fmt_s_(0);
      return 0}
    function count_arguments_of_format_bN_(fmt_a_)
     {var ac_d_=[0,0,0,0];
      function add_conv_b_(skip_a_,i_b_,c_c_)
       {var _f_=41!==c_c_?1:0,_g_=_f_?_bo_!==c_c_?1:0:_f_;
        if(_g_)
         {var inc_e_=97===c_c_?2:1;
          if(_aJ_===c_c_)ac_d_[3]=ac_d_[3]+1|0;
          if(skip_a_)
           ac_d_[2]=ac_d_[2]+inc_e_|0;
          else
           ac_d_[1]=ac_d_[1]+inc_e_|0}
        return i_b_+1|0}
      iter_on_format_args_bM_
       (fmt_a_,add_conv_b_,function(i_a_,param_b_){return i_a_+1|0});
      return ac_d_[1]}
    function scan_positional_spec_bO_(fmt_a_,got_spec_b_,i_c_)
     {var _h_=fmt_a_.safeGet(i_c_);
      if((_h_-48|0)<0||9<(_h_-48|0))
       return caml_call_gen2_n_(got_spec_b_,0,i_c_);
      var accu_e_=_h_-48|0,j_d_=i_c_+1|0;
      for(;;)
       {var _f_=fmt_a_.safeGet(j_d_);
        if(48<=_f_)
         {if(!(58<=_f_))
           {var accu_e_=(10*accu_e_|0)+(_f_-48|0)|0,j_d_=j_d_+1|0;continue}
          var _g_=0}
        else
         if(36===_f_)
          if(0===accu_e_)
           {var _i_=_aS_(_dj_),_g_=1}
          else
           {var
             _i_=
              caml_call_gen2_n_
               (got_spec_b_,[0,index_of_int_a0_(accu_e_-1|0)],j_d_+1|0),
             _g_=1}
         else
          var _g_=0;
        if(!_g_)var _i_=caml_call_gen2_n_(got_spec_b_,0,i_c_);
        return _i_}}
    function next_index_q_(spec_a_,n_b_){return spec_a_?n_b_:_bH_(n_b_)}
    function get_index_bP_(spec_a_,n_b_){return spec_a_?spec_a_[1]:n_b_}
    function _bQ_(to_s_aM_,get_out_b_,outc_c_,outs_ag_,flush_e_,k_f_,fmt_g_)
     {var out_w_=caml_call_gen1_i_(get_out_b_,fmt_g_);
      function pr_aO_(k_a_,n_b_,fmt_k_,v_aP_)
       {var len_h_=fmt_k_.getLen();
        function doprn_D_(n_m_,i_b_)
         {var i_p_=i_b_;
          for(;;)
           {if(len_h_<=i_p_)return caml_call_gen1_i_(k_a_,out_w_);
            var _d_=fmt_k_.safeGet(i_p_);
            if(37===_d_)
             {var
               get_arg_o_=
                function(spec_a_,n_b_)
                 {return caml_array_get_j_(v_aP_,get_index_bP_(spec_a_,n_b_))},
               scan_flags_aw_=
                function(spec_g_,n_f_,widths_c_,i_d_)
                 {var i_a_=i_d_;
                  for(;;)
                   {var _ac_=fmt_k_.safeGet(i_a_)-32|0;
                    if(!(_ac_<0||25<_ac_))
                     switch(_ac_)
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
                        return scan_positional_spec_bO_
                                (fmt_k_,
                                 function(wspec_a_,i_b_)
                                  {var _d_=[0,get_arg_o_(wspec_a_,n_f_),widths_c_];
                                   return scan_flags_aw_
                                           (spec_g_,next_index_q_(wspec_a_,n_f_),_d_,i_b_)},
                                 i_a_+1|0);
                       default:var i_a_=i_a_+1|0;continue}
                    var _r_=fmt_k_.safeGet(i_a_);
                    if(124<=_r_)
                     var _h_=0;
                    else
                     switch(_r_)
                      {case 78:
                       case 88:
                       case _aN_:
                       case _P_:
                       case _bs_:
                       case _bp_:
                       case _bq_:
                        var
                         x_bb_=get_arg_o_(spec_g_,n_f_),
                         s_bc_=
                          caml_format_int_aG_
                           (extract_format_int_bK_(_r_,fmt_k_,i_p_,i_a_,widths_c_),
                            x_bb_),
                         _j_=cont_s_s_(next_index_q_(spec_g_,n_f_),s_bc_,i_a_+1|0),
                         _h_=1;
                        break;
                       case 69:
                       case 71:
                       case _cE_:
                       case _bu_:
                       case _bt_:
                        var
                         x_a6_=get_arg_o_(spec_g_,n_f_),
                         s_a7_=
                          caml_format_float_bi_
                           (extract_format_L_(fmt_k_,i_p_,i_a_,widths_c_),x_a6_),
                         _j_=cont_s_s_(next_index_q_(spec_g_,n_f_),s_a7_,i_a_+1|0),
                         _h_=1;
                        break;
                       case 76:
                       case _bv_:
                       case _aj_:
                        var _ae_=fmt_k_.safeGet(i_a_+1|0)-88|0;
                        if(_ae_<0||32<_ae_)
                         var _ah_=1;
                        else
                         switch(_ae_)
                          {case 0:
                           case 12:
                           case 17:
                           case 23:
                           case 29:
                           case 32:
                            var i_U_=i_a_+1|0,_af_=_r_-_bv_|0;
                            if(_af_<0||2<_af_)
                             var _ai_=0;
                            else
                             {switch(_af_)
                               {case 1:var _ai_=0,_al_=0;break;
                                case 2:
                                 var
                                  x_ba_=get_arg_o_(spec_g_,n_f_),
                                  _aD_=
                                   caml_format_int_aG_
                                    (extract_format_L_(fmt_k_,i_p_,i_U_,widths_c_),x_ba_),
                                  _al_=1;
                                 break;
                                default:
                                 var
                                  x_a$_=get_arg_o_(spec_g_,n_f_),
                                  _aD_=
                                   caml_format_int_aG_
                                    (extract_format_L_(fmt_k_,i_p_,i_U_,widths_c_),x_a$_),
                                  _al_=1}
                              if(_al_){var s_aC_=_aD_,_ai_=1}}
                            if(!_ai_)
                             {var
                               x_a__=get_arg_o_(spec_g_,n_f_),
                               s_aC_=
                                caml_int64_format_eY_
                                 (extract_format_L_(fmt_k_,i_p_,i_U_,widths_c_),x_a__)}
                            var
                             _j_=cont_s_s_(next_index_q_(spec_g_,n_f_),s_aC_,i_U_+1|0),
                             _h_=1,
                             _ah_=0;
                            break;
                           default:var _ah_=1}
                        if(_ah_)
                         {var
                           x_a8_=get_arg_o_(spec_g_,n_f_),
                           s_a9_=
                            caml_format_int_aG_
                             (extract_format_int_bK_(_aj_,fmt_k_,i_p_,i_a_,widths_c_),
                              x_a8_),
                           _j_=cont_s_s_(next_index_q_(spec_g_,n_f_),s_a9_,i_a_+1|0),
                           _h_=1}
                        break;
                       case 37:
                       case 64:
                        var _j_=cont_s_s_(n_f_,_ao_(1,_r_),i_a_+1|0),_h_=1;break;
                       case 83:
                       case _ak_:
                        var x_z_=get_arg_o_(spec_g_,n_f_);
                        if(_ak_===_r_)
                         var x_A_=x_z_;
                        else
                         {var n_b_=[0,0],_ar_=x_z_.getLen()-1|0,_aR_=0;
                          if(!(_ar_<0))
                           {var i_N_=_aR_;
                            for(;;)
                             {var
                               _y_=x_z_.safeGet(i_N_),
                               _bk_=
                                14<=_y_
                                 ?34===_y_?1:92===_y_?1:0
                                 :11<=_y_?13<=_y_?1:0:8<=_y_?1:0,
                               _aV_=_bk_?2:caml_is_printable_bj_(_y_)?1:4;
                              n_b_[1]=n_b_[1]+_aV_|0;
                              var _aW_=i_N_+1|0;
                              if(_ar_!==i_N_){var i_N_=_aW_;continue}
                              break}}
                          if(n_b_[1]===x_z_.getLen())
                           var _aF_=x_z_;
                          else
                           {var s__m_=caml_create_string_G_(n_b_[1]);
                            n_b_[1]=0;
                            var _as_=x_z_.getLen()-1|0,_aS_=0;
                            if(!(_as_<0))
                             {var i_M_=_aS_;
                              for(;;)
                               {var _x_=x_z_.safeGet(i_M_),_B_=_x_-34|0;
                                if(_B_<0||58<_B_)
                                 if(-20<=_B_)
                                  var _V_=1;
                                 else
                                  {switch(_B_+34|0)
                                    {case 8:
                                      s__m_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s__m_.safeSet(n_b_[1],98);
                                      var _K_=1;
                                      break;
                                     case 9:
                                      s__m_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s__m_.safeSet(n_b_[1],_bm_);
                                      var _K_=1;
                                      break;
                                     case 10:
                                      s__m_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s__m_.safeSet(n_b_[1],_aj_);
                                      var _K_=1;
                                      break;
                                     case 13:
                                      s__m_.safeSet(n_b_[1],92);
                                      n_b_[1]++;
                                      s__m_.safeSet(n_b_[1],_aJ_);
                                      var _K_=1;
                                      break;
                                     default:var _V_=1,_K_=0}
                                   if(_K_)var _V_=0}
                                else
                                 var
                                  _V_=
                                   (_B_-1|0)<0||56<(_B_-1|0)
                                    ?(s__m_.safeSet(n_b_[1],92),
                                      n_b_[1]++,
                                      s__m_.safeSet(n_b_[1],_x_),
                                      0)
                                    :1;
                                if(_V_)
                                 if(caml_is_printable_bj_(_x_))
                                  s__m_.safeSet(n_b_[1],_x_);
                                 else
                                  {s__m_.safeSet(n_b_[1],92);
                                   n_b_[1]++;
                                   s__m_.safeSet(n_b_[1],48+(_x_/_aN_|0)|0);
                                   n_b_[1]++;
                                   s__m_.safeSet(n_b_[1],48+((_x_/10|0)%10|0)|0);
                                   n_b_[1]++;
                                   s__m_.safeSet(n_b_[1],48+(_x_%10|0)|0)}
                                n_b_[1]++;
                                var _aU_=i_M_+1|0;
                                if(_as_!==i_M_){var i_M_=_aU_;continue}
                                break}}
                            var _aF_=s__m_}
                          var x_A_=_l_(_dq_,_l_(_aF_,_dp_))}
                        if(i_a_===(i_p_+1|0))
                         var s_aE_=x_A_;
                        else
                         {var _J_=extract_format_L_(fmt_k_,i_p_,i_a_,widths_c_);
                          try
                           {var neg_W_=0,i_u_=1;
                            for(;;)
                             {if(_J_.getLen()<=i_u_)
                               var _at_=[0,0,neg_W_];
                              else
                               {var _X_=_J_.safeGet(i_u_);
                                if(49<=_X_)
                                 if(58<=_X_)
                                  var _am_=0;
                                 else
                                  {var
                                    _at_=
                                     [0,
                                      caml_int_of_string_e8_
                                       (_ap_(_J_,i_u_,(_J_.getLen()-i_u_|0)-1|0)),
                                      neg_W_],
                                    _am_=1}
                                else
                                 {if(45===_X_){var neg_W_=1,i_u_=i_u_+1|0;continue}
                                  var _am_=0}
                                if(!_am_){var i_u_=i_u_+1|0;continue}}
                              var match_Z_=_at_;
                              break}}
                          catch(_f_)
                           {if(_f_[1]!==_aT_)throw _f_;
                            var match_Z_=bad_conversion_bJ_(_J_,0,_ak_)}
                          var
                           p_O_=match_Z_[1],
                           _C_=x_A_.getLen(),
                           neg_a0_=match_Z_[2],
                           _Q_=0,
                           _a1_=32;
                          if(p_O_===_C_&&0===_Q_){var ___=x_A_,_aQ_=1}else var _aQ_=0;
                          if(!_aQ_)
                           if(p_O_<=_C_)
                            var ___=_ap_(x_A_,_Q_,_C_);
                           else
                            {var res_Y_=_ao_(p_O_,_a1_);
                             if(neg_a0_)
                              _aq_(x_A_,_Q_,res_Y_,0,_C_);
                             else
                              _aq_(x_A_,_Q_,res_Y_,p_O_-_C_|0,_C_);
                             var ___=res_Y_}
                          var s_aE_=___}
                        var
                         _j_=cont_s_s_(next_index_q_(spec_g_,n_f_),s_aE_,i_a_+1|0),
                         _h_=1;
                        break;
                       case 67:
                       case 99:
                        var x_t_=get_arg_o_(spec_g_,n_f_);
                        if(99===_r_)
                         var s_az_=_ao_(1,x_t_);
                        else
                         {if(39===x_t_)
                           var _v_=_cX_;
                          else
                           if(92===x_t_)
                            var _v_=_cY_;
                           else
                            {if(14<=x_t_)
                              var _E_=0;
                             else
                              switch(x_t_)
                               {case 8:var _v_=_cZ_,_E_=1;break;
                                case 9:var _v_=_c0_,_E_=1;break;
                                case 10:var _v_=_c1_,_E_=1;break;
                                case 13:var _v_=_c2_,_E_=1;break;
                                default:var _E_=0}
                             if(!_E_)
                              if(caml_is_printable_bj_(x_t_))
                               {var s_an_=caml_create_string_G_(1);
                                s_an_.safeSet(0,x_t_);
                                var _v_=s_an_}
                              else
                               {var s_F_=caml_create_string_G_(4);
                                s_F_.safeSet(0,92);
                                s_F_.safeSet(1,48+(x_t_/_aN_|0)|0);
                                s_F_.safeSet(2,48+((x_t_/10|0)%10|0)|0);
                                s_F_.safeSet(3,48+(x_t_%10|0)|0);
                                var _v_=s_F_}}
                          var s_az_=_l_(_do_,_l_(_v_,_dn_))}
                        var
                         _j_=cont_s_s_(next_index_q_(spec_g_,n_f_),s_az_,i_a_+1|0),
                         _h_=1;
                        break;
                       case 66:
                       case 98:
                        var
                         _a4_=i_a_+1|0,
                         _a5_=get_arg_o_(spec_g_,n_f_)?_cR_:_cS_,
                         _j_=cont_s_s_(next_index_q_(spec_g_,n_f_),_a5_,_a4_),
                         _h_=1;
                        break;
                       case 40:
                       case _br_:
                        var
                         xf_T_=get_arg_o_(spec_g_,n_f_),
                         j_ax_=
                          caml_call_gen2_n_
                           (sub_format_for_printf_bL_(_r_),fmt_k_,i_a_+1|0);
                        if(_br_===_r_)
                         {var
                           b_R_=_aX_(xf_T_.getLen()),
                           add_char_au_=
                            function(i_a_,c_b_){_aa_(b_R_,c_b_);return i_a_+1|0};
                          iter_on_format_args_bM_
                           (xf_T_,
                            function(skip_a_,i_b_,c_c_)
                             {if(skip_a_)_aZ_(b_R_,_di_);else _aa_(b_R_,37);
                              return add_char_au_(i_b_,c_c_)},
                            add_char_au_);
                          var
                           _a2_=_aY_(b_R_),
                           _j_=cont_s_s_(next_index_q_(spec_g_,n_f_),_a2_,j_ax_),
                           _h_=1}
                        else
                         {var
                           _ay_=next_index_q_(spec_g_,n_f_),
                           m_bh_=
                            add_int_index_bG_(count_arguments_of_format_bN_(xf_T_),_ay_),
                           _j_=
                            pr_aO_
                             (function(param_a_){return doprn_D_(m_bh_,j_ax_)},
                              _ay_,
                              xf_T_,
                              v_aP_),
                           _h_=1}
                        break;
                       case 33:
                        caml_call_gen1_i_(flush_e_,out_w_);
                        var _j_=doprn_D_(n_f_,i_a_+1|0),_h_=1;
                        break;
                       case 41:var _j_=cont_s_s_(n_f_,_dl_,i_a_+1|0),_h_=1;break;
                       case 44:var _j_=cont_s_s_(n_f_,_dm_,i_a_+1|0),_h_=1;break;
                       case 70:
                        var x_ad_=get_arg_o_(spec_g_,n_f_);
                        if(0===widths_c_)
                         var s_aA_=string_of_float_bz_(x_ad_);
                        else
                         {var sfmt_$_=extract_format_L_(fmt_k_,i_p_,i_a_,widths_c_);
                          if(70===_r_)sfmt_$_.safeSet(sfmt_$_.getLen()-1|0,_bt_);
                          var s_I_=caml_format_float_bi_(sfmt_$_,x_ad_);
                          if(3<=caml_classify_float_eS_(x_ad_))
                           var _aB_=s_I_;
                          else
                           {var i_S_=0,l_a3_=s_I_.getLen();
                            for(;;)
                             {if(l_a3_<=i_S_)
                               var _av_=_l_(s_I_,_dk_);
                              else
                               {var
                                 _H_=s_I_.safeGet(i_S_)-46|0,
                                 _bl_=
                                  _H_<0||23<_H_?55===_H_?1:0:(_H_-1|0)<0||21<(_H_-1|0)?1:0;
                                if(!_bl_){var i_S_=i_S_+1|0;continue}
                                var _av_=s_I_}
                              var _aB_=_av_;
                              break}}
                          var s_aA_=_aB_}
                        var
                         _j_=cont_s_s_(next_index_q_(spec_g_,n_f_),s_aA_,i_a_+1|0),
                         _h_=1;
                        break;
                       case 91:
                        var _j_=bad_conversion_format_ab_(fmt_k_,i_a_,_r_),_h_=1;
                        break;
                       case 97:
                        var
                         printer_aH_=get_arg_o_(spec_g_,n_f_),
                         n_aI_=_bH_(get_index_bP_(spec_g_,n_f_)),
                         arg_aK_=get_arg_o_(0,n_aI_),
                         _bd_=i_a_+1|0,
                         _be_=next_index_q_(spec_g_,n_aI_);
                        if(to_s_aM_)
                         caml_call_gen2_n_
                          (outs_ag_,out_w_,caml_call_gen2_n_(printer_aH_,0,arg_aK_));
                        else
                         caml_call_gen2_n_(printer_aH_,out_w_,arg_aK_);
                        var _j_=doprn_D_(_be_,_bd_),_h_=1;
                        break;
                       case _aJ_:
                        var _j_=bad_conversion_format_ab_(fmt_k_,i_a_,_r_),_h_=1;
                        break;
                       case _bm_:
                        var
                         printer_aL_=get_arg_o_(spec_g_,n_f_),
                         _bf_=i_a_+1|0,
                         _bg_=next_index_q_(spec_g_,n_f_);
                        if(to_s_aM_)
                         caml_call_gen2_n_
                          (outs_ag_,out_w_,caml_call_gen1_i_(printer_aL_,0));
                        else
                         caml_call_gen1_i_(printer_aL_,out_w_);
                        var _j_=doprn_D_(_bg_,_bf_),_h_=1;
                        break;
                       default:var _h_=0}
                    if(!_h_)var _j_=bad_conversion_format_ab_(fmt_k_,i_a_,_r_);
                    return _j_}},
               _f_=i_p_+1|0,
               _g_=0;
              return scan_positional_spec_bO_
                      (fmt_k_,
                       function(spec_a_,i_b_)
                        {return scan_flags_aw_(spec_a_,n_m_,_g_,i_b_)},
                       _f_)}
            caml_call_gen2_n_(outc_c_,out_w_,_d_);
            var i_p_=i_p_+1|0;
            continue}}
        function cont_s_s_(n_a_,s_b_,i_c_)
         {caml_call_gen2_n_(outs_ag_,out_w_,s_b_);return doprn_D_(n_a_,i_c_)}
        return doprn_D_(n_b_,0)}
      var _o_=index_of_int_a0_(0);
      function kpr_k_(_a_,_b_){return pr_aO_(k_f_,_o_,_a_,_b_)}
      var _d_=count_arguments_of_format_bN_(fmt_g_);
      if(_d_<0||6<_d_)
       {var
         loop_m_=
          function(i_i_,args_b_)
           {if(_d_<=i_i_)
             {var
               a_j_=caml_make_vect_v_(_d_,0),
               _l_=
                function(i_a_,arg_b_)
                 {return caml_array_set_h_(a_j_,(_d_-i_a_|0)-1|0,arg_b_)},
               i_c_=0,
               param_a_=args_b_;
              for(;;)
               {if(param_a_)
                 {var _e_=param_a_[2],_f_=param_a_[1];
                  if(_e_)
                   {_l_(i_c_,_f_);var i_c_=i_c_+1|0,param_a_=_e_;continue}
                  _l_(i_c_,_f_)}
                return kpr_k_(fmt_g_,a_j_)}}
            return function(x_a_){return loop_m_(i_i_+1|0,[0,x_a_,args_b_])}},
         _a_=loop_m_(0,0)}
      else
       switch(_d_)
        {case 1:
          var
           _a_=
            function(x_a_)
             {var a_b_=caml_make_vect_v_(1,0);
              caml_array_set_h_(a_b_,0,x_a_);
              return kpr_k_(fmt_g_,a_b_)};
          break;
         case 2:
          var
           _a_=
            function(x_a_,y_b_)
             {var a_c_=caml_make_vect_v_(2,0);
              caml_array_set_h_(a_c_,0,x_a_);
              caml_array_set_h_(a_c_,1,y_b_);
              return kpr_k_(fmt_g_,a_c_)};
          break;
         case 3:
          var
           _a_=
            function(x_a_,y_b_,z_c_)
             {var a_d_=caml_make_vect_v_(3,0);
              caml_array_set_h_(a_d_,0,x_a_);
              caml_array_set_h_(a_d_,1,y_b_);
              caml_array_set_h_(a_d_,2,z_c_);
              return kpr_k_(fmt_g_,a_d_)};
          break;
         case 4:
          var
           _a_=
            function(x_a_,y_b_,z_c_,t_d_)
             {var a_e_=caml_make_vect_v_(4,0);
              caml_array_set_h_(a_e_,0,x_a_);
              caml_array_set_h_(a_e_,1,y_b_);
              caml_array_set_h_(a_e_,2,z_c_);
              caml_array_set_h_(a_e_,3,t_d_);
              return kpr_k_(fmt_g_,a_e_)};
          break;
         case 5:
          var
           _a_=
            function(x_a_,y_b_,z_c_,t_d_,u_e_)
             {var a_f_=caml_make_vect_v_(5,0);
              caml_array_set_h_(a_f_,0,x_a_);
              caml_array_set_h_(a_f_,1,y_b_);
              caml_array_set_h_(a_f_,2,z_c_);
              caml_array_set_h_(a_f_,3,t_d_);
              caml_array_set_h_(a_f_,4,u_e_);
              return kpr_k_(fmt_g_,a_f_)};
          break;
         case 6:
          var
           _a_=
            function(x_a_,y_b_,z_c_,t_d_,u_e_,v_f_)
             {var a_i_=caml_make_vect_v_(6,0);
              caml_array_set_h_(a_i_,0,x_a_);
              caml_array_set_h_(a_i_,1,y_b_);
              caml_array_set_h_(a_i_,2,z_c_);
              caml_array_set_h_(a_i_,3,t_d_);
              caml_array_set_h_(a_i_,4,u_e_);
              caml_array_set_h_(a_i_,5,v_f_);
              return kpr_k_(fmt_g_,a_i_)};
          break;
         default:var _a_=kpr_k_(fmt_g_,[0])}
      return _a_}
    function _bR_(oc_d_)
     {function _e_(_a_){return 0}
      function _b_(param_a_){return oc_d_}
      var _c_=0;
      return function(_a_)
       {return _bQ_(_c_,_b_,_cW_,output_string_bA_,_bC_,_e_,_a_)}}
    function _dr_(fmt_a_){return _aX_(2*fmt_a_.getLen()|0)}
    function _p_(fmt_a_)
     {function _b_(_a_){var s_b_=_aY_(_a_);_a_[2]=0;return s_b_}
      return _bQ_(1,_dr_,_aa_,_aZ_,function(_a_){return 0},_b_,fmt_a_)}
    var _a1_=[0,0];
    function _a3_(x_a_,i_b_)
     {var f_c_=x_a_[i_b_+1];
      return caml_obj_is_block_fk_(f_c_)
              ?caml_obj_tag_ct_(f_c_)===_c6_
                ?caml_call_gen1_i_(_p_(_ds_),f_c_)
                :caml_obj_tag_ct_(f_c_)===_c7_?string_of_float_bz_(f_c_):_dt_
              :caml_call_gen1_i_(_p_(_du_),f_c_)}
    function _bS_(x_a_,i_b_)
     {if(x_a_.length-1<=i_b_)return _dv_;
      var _c_=_bS_(x_a_,i_b_+1|0),_d_=_a3_(x_a_,i_b_);
      return caml_call_gen2_n_(_p_(_dw_),_d_,_c_)}
    function _bT_(x_a_)
     {var param_c_=_a1_[1];
      for(;;)
       {if(param_c_)
         {var tl_u_=param_c_[2],hd_v_=param_c_[1];
          try
           {var _w_=caml_call_gen1_i_(hd_v_,x_a_),_g_=_w_}
          catch(_f_){var _g_=0}
          if(!_g_){var param_c_=tl_u_;continue}
          var _b_=_g_[1]}
        else
         if(x_a_[1]===_dB_)
          var _b_=_dC_;
         else
          if(x_a_[1]===_dD_)
           var _b_=_dE_;
          else
           if(x_a_[1]===_dF_)
            {var
              match_f_=x_a_[2],
              char_k_=match_f_[3],
              line_x_=match_f_[2],
              file_y_=match_f_[1],
              _b_=
               caml_call_gen5_aH_
                (_p_(_a2_),file_y_,line_x_,char_k_,char_k_+5|0,_dG_)}
           else
            if(x_a_[1]===_m_)
             {var
               match_h_=x_a_[2],
               char_o_=match_h_[3],
               line_z_=match_h_[2],
               file_A_=match_h_[1],
               _b_=
                caml_call_gen5_aH_
                 (_p_(_a2_),file_A_,line_z_,char_o_,char_o_+6|0,_dH_)}
            else
             if(x_a_[1]===_dI_)
              {var
                match_j_=x_a_[2],
                char_q_=match_j_[3],
                line_B_=match_j_[2],
                file_C_=match_j_[1],
                _b_=
                 caml_call_gen5_aH_
                  (_p_(_a2_),file_C_,line_B_,char_q_,char_q_+6|0,_dJ_)}
             else
              {var _e_=x_a_.length-1,constructor_D_=x_a_[0+1][0+1];
               if(_e_<0||2<_e_)
                {var
                  _r_=_bS_(x_a_,2),
                  _s_=_a3_(x_a_,1),
                  _d_=caml_call_gen2_n_(_p_(_dx_),_s_,_r_)}
               else
                switch(_e_)
                 {case 1:var _d_=_dz_;break;
                  case 2:
                   var _t_=_a3_(x_a_,1),_d_=caml_call_gen1_i_(_p_(_dA_),_t_);
                   break;
                  default:var _d_=_dy_}
               var _b_=_l_(constructor_D_,_d_)}
        return _b_}}
    function _bU_(outchan_a_)
     {var _f_=caml_get_exception_backtrace_eW_(0);
      if(_f_)
       {var a_d_=_f_[1],_g_=a_d_.length-1-1|0,_r_=0;
        if(!(_g_<0))
         {var i_c_=_r_;
          for(;;)
           {if(caml_notequal_fj_(caml_array_get_j_(a_d_,i_c_),_dQ_))
             {var
               _b_=caml_array_get_j_(a_d_,i_c_),
               is_raise_k_=0===_b_[0]?_b_[1]:_b_[1],
               info_e_=is_raise_k_?0===i_c_?_dK_:_dN_:0===i_c_?_dO_:_dP_;
              if(0===_b_[0])
               {var
                 endchar_l_=_b_[5],
                 startchar_m_=_b_[4],
                 lineno_o_=_b_[3],
                 filename_q_=_b_[2],
                 _h_=
                  caml_call_gen5_aH_
                   (_p_(_dL_),
                    info_e_,
                    filename_q_,
                    lineno_o_,
                    startchar_m_,
                    endchar_l_)}
              else
               var _h_=caml_call_gen1_i_(_p_(_dM_),info_e_);
              caml_call_gen2_n_(_bR_(outchan_a_),_dR_,_h_)}
            var _s_=i_c_+1|0;
            if(_g_!==i_c_){var i_c_=_s_;continue}
            break}}
        return 0}
      return caml_call_gen1_i_(_bR_(outchan_a_),_dS_)}
    32===_aW_;
    try
     {var _eM_=caml_sys_getenv_cu_(_eL_),params_a4_=_eM_}
    catch(_f_)
     {if(_f_[1]!==___)throw _f_;
      try
       {var _eK_=caml_sys_getenv_cu_(_eJ_),_bV_=_eK_}
      catch(_f_){if(_f_[1]!==___)throw _f_;var _bV_=_dT_}
      var params_a4_=_bV_}
    var l_bD_=params_a4_.getLen(),_dU_=82,_bE_=0;
    if(0<=0&&!(l_bD_<_bE_))
     try
      {var i_ar_=_bE_;
       for(;;)
        {if(l_bD_<=i_ar_)throw [0,___];
         if(params_a4_.safeGet(i_ar_)!==_dU_){var i_ar_=i_ar_+1|0;continue}
         var _bk_=1;
         break}}
     catch(_f_){if(_f_[1]!==___)throw _f_;var _bk_=1}
    else
     var _bk_=0;
    if(!_bk_)_Q_(_c5_);
    function _bW_(param_a_)
     {var seq_b_=[];
      caml_update_dummy_fv_(seq_b_,[0,seq_b_,seq_b_]);
      return seq_b_}
    var Canceled_a5_=[0,_dV_],current_data_M_=[0,0],max_removed_dW_=42;
    function repr_rec_a6_(t_a_)
     {var _c_=t_a_[1];
      {if(3===_c_[0])
        {var t__d_=_c_[1],t___b_=repr_rec_a6_(t__d_);
         if(t___b_!==t__d_)t_a_[1]=[3,t___b_];
         return t___b_}
       return t_a_}}
    function repr_S_(t_a_){return repr_rec_a6_(t_a_)}
    var
     async_exception_hook_a7_=
      [0,
       function(exn_a_)
        {prerr_string_bB_(_dX_);
         prerr_string_bB_(_bT_(exn_a_));
         caml_ml_output_char_cs_(stderr_Z_,10);
         _bU_(stderr_Z_);
         _bC_(stderr_Z_);
         do_at_exit_aV_(0);
         return caml_sys_exit_fs_(2)}];
    function call_unsafe_bX_(f_a_,x_b_)
     {try
       {var _c_=caml_call_gen1_i_(f_a_,x_b_)}
      catch(_f_){return caml_call_gen1_i_(async_exception_hook_a7_[1],_f_)}
      return _c_}
    function run_waiters_rec_bY_(state_a_,ws_b_,rem_c_)
     {var ws_d_=ws_b_,rem_e_=rem_c_;
      for(;;)
       if(typeof ws_d_===_B_)
        return run_waiters_rec_next_at_(state_a_,rem_e_);
       else
        switch(ws_d_[0])
         {case 1:
           caml_call_gen1_i_(ws_d_[1],state_a_);
           return run_waiters_rec_next_at_(state_a_,rem_e_);
          case 2:
           var _g_=[0,ws_d_[2],rem_e_],ws_d_=ws_d_[1],rem_e_=_g_;continue;
          default:
           var _f_=ws_d_[1][1];
           return _f_
                   ?(caml_call_gen1_i_(_f_[1],state_a_),
                     run_waiters_rec_next_at_(state_a_,rem_e_))
                   :run_waiters_rec_next_at_(state_a_,rem_e_)}}
    function run_waiters_rec_next_at_(state_a_,rem_b_)
     {return rem_b_?run_waiters_rec_bY_(state_a_,rem_b_[1],rem_b_[2]):0}
    function run_cancel_handlers_rec_bZ_(chs_a_,rem_b_)
     {var chs_c_=chs_a_,rem_e_=rem_b_;
      for(;;)
       if(typeof chs_c_===_B_)
        return run_cancel_handlers_rec_next_a8_(rem_e_);
       else
        switch(chs_c_[0])
         {case 1:
           var n_d_=chs_c_[1];
           if(n_d_[4]){n_d_[4]=0;n_d_[1][2]=n_d_[2];n_d_[2][1]=n_d_[1]}
           return run_cancel_handlers_rec_next_a8_(rem_e_);
          case 2:
           var _g_=[0,chs_c_[2],rem_e_],chs_c_=chs_c_[1],rem_e_=_g_;continue;
          default:
           var f_f_=chs_c_[2];
           current_data_M_[1]=chs_c_[1];
           call_unsafe_bX_(f_f_,0);
           return run_cancel_handlers_rec_next_a8_(rem_e_)}}
    function run_cancel_handlers_rec_next_a8_(rem_a_)
     {return rem_a_?run_cancel_handlers_rec_bZ_(rem_a_[1],rem_a_[2]):0}
    function unsafe_run_waiters_au_(sleeper_a_,state_b_)
     {var
       _c_=
        1===state_b_[0]
         ?state_b_[1][1]===Canceled_a5_
           ?(run_cancel_handlers_rec_bZ_(sleeper_a_[4],0),1)
           :0
         :0;
      return run_waiters_rec_bY_(state_b_,sleeper_a_[2],0)}
    var wakening_a9_=[0,0],_R_=[0,0,0];
    function wakeup_a__(t_a_,v_b_)
     {var _k_=[0,v_b_],t_l_=repr_rec_a6_(t_a_),_f_=t_l_[1];
      switch(_f_[0])
       {case 1:
         if(_f_[1][1]===Canceled_a5_){var _g_=0,_c_=1}else var _c_=0;break;
        case 2:
         var sleeper_n_=_f_[1];
         t_l_[1]=_k_;
         var
          snapshot_i_=current_data_M_[1],
          already_wakening_m_=wakening_a9_[1]?1:(wakening_a9_[1]=1,0);
         unsafe_run_waiters_au_(sleeper_n_,_k_);
         if(already_wakening_m_)
          {current_data_M_[1]=snapshot_i_;var _j_=0}
         else
          for(;;)
           {if(0!==_R_[1])
             {if(0===_R_[1])throw [0,_c9_];
              _R_[1]=_R_[1]-1|0;
              var tail_d_=_R_[2],head_e_=tail_d_[2];
              if(head_e_===tail_d_)_R_[2]=0;else tail_d_[2]=head_e_[2];
              var _h_=head_e_[1];
              unsafe_run_waiters_au_(_h_[1],_h_[2]);
              continue}
            wakening_a9_[1]=0;
            current_data_M_[1]=snapshot_i_;
            var _j_=0;
            break}
         var _g_=_j_,_c_=1;
         break;
        default:var _c_=0}
      if(!_c_)var _g_=_Q_(_dY_);
      return _g_}
    function append_b0_(l1_a_,l2_b_)
     {return typeof l1_a_===_B_?l2_b_:typeof l2_b_===_B_?l1_a_:[2,l1_a_,l2_b_]}
    function cleanup_a$_(ws_a_)
     {if(typeof ws_a_!==_B_)
       switch(ws_a_[0])
        {case 2:
          var l1_b_=ws_a_[1],_c_=cleanup_a$_(ws_a_[2]);
          return append_b0_(cleanup_a$_(l1_b_),_c_);
         case 1:break;
         default:if(!ws_a_[1][1])return 0}
      return ws_a_}
    function task_b1_(param_a_)
     {var _b_=[0,[2,[0,1,0,0,0]]];return [0,_b_,_b_]}
    function add_immutable_waiter_b2_(sleeper_a_,waiter_b_)
     {var
       _d_=[1,waiter_b_],
       _c_=sleeper_a_[2],
       _e_=typeof _c_===_B_?_d_:[2,_d_,_c_];
      sleeper_a_[2]=_e_;
      return 0}
    function bind_ba_(t_a_,f_b_)
     {var t_d_=repr_S_(t_a_),_c_=t_d_[1];
      switch(_c_[0])
       {case 1:return [0,_c_];
        case 2:
         var
          _k_=[0,[2,[0,[0,[0,t_d_]],0,0,0]]],
          sleeper_e_=_c_[1],
          data_v_=current_data_M_[1];
         add_immutable_waiter_b2_
          (sleeper_e_,
           function(state_a_)
            {switch(state_a_[0])
              {case 0:
                var v_w_=state_a_[1];
                current_data_M_[1]=data_v_;
                try
                 {var _x_=caml_call_gen1_i_(f_b_,v_w_),_s_=_x_}
                catch(_f_){var _s_=[0,[1,_f_]]}
                var t1_d_=repr_S_(_k_),t2_g_=repr_S_(_s_),_n_=t1_d_[1];
                {if(2===_n_[0])
                  {var sleeper1_c_=_n_[1];
                   if(t1_d_===t2_g_)
                    var _l_=0;
                   else
                    {var _e_=t2_g_[1];
                     if(2===_e_[0])
                      {var sleeper2_f_=_e_[1];
                       t2_g_[1]=[3,t1_d_];
                       sleeper1_c_[1]=sleeper2_f_[1];
                       var
                        waiters_o_=append_b0_(sleeper1_c_[2],sleeper2_f_[2]),
                        removed_p_=sleeper1_c_[3]+sleeper2_f_[3]|0;
                       if(max_removed_dW_<removed_p_)
                        {sleeper1_c_[3]=0;sleeper1_c_[2]=cleanup_a$_(waiters_o_)}
                       else
                        {sleeper1_c_[3]=removed_p_;sleeper1_c_[2]=waiters_o_}
                       var
                        _h_=sleeper2_f_[4],
                        _j_=sleeper1_c_[4],
                        _t_=typeof _j_===_B_?_h_:typeof _h_===_B_?_j_:[2,_j_,_h_];
                       sleeper1_c_[4]=_t_;
                       var _l_=0}
                     else
                      {t1_d_[1]=_e_;
                       var _l_=unsafe_run_waiters_au_(sleeper1_c_,_e_)}}
                   return _l_}
                 throw [0,_m_,_dZ_]}
               case 1:
                var t_q_=repr_S_(_k_),_r_=t_q_[1];
                {if(2===_r_[0])
                  {var sleeper_u_=_r_[1];
                   t_q_[1]=state_a_;
                   return unsafe_run_waiters_au_(sleeper_u_,state_a_)}
                 throw [0,_m_,_d0_]}
               default:throw [0,_m_,_d1_]}});
         return _k_;
        case 3:throw [0,_m_,_d2_];
        default:return caml_call_gen1_i_(f_b_,_c_[1])}}
    var
     pause_hook_d5_=[0,function(_a_){return 0}],
     _t_=_bW_(0),
     _d6_=[0,0],
     _T_=joo_global_object_c_,
     null_av_=null,
     undefined_b3_=undefined,
     _true_x_=true,
     _false_bb_=false,
     array_constructor_b4_=Array,
     date_constr_aw_=Date;
    function _d7_(param_a_)
     {var _e_=1-(_t_[2]===_t_?1:0);
      if(_e_)
       {var tmp_b_=_bW_(0);
        tmp_b_[1][2]=_t_[2];
        _t_[2][1]=tmp_b_[1];
        tmp_b_[1]=_t_[1];
        _t_[1][2]=tmp_b_;
        _t_[1]=_t_;
        _t_[2]=_t_;
        _d6_[1]=0;
        var curr_c_=tmp_b_[2];
        for(;;)
         {var _d_=curr_c_!==tmp_b_?1:0;
          if(_d_)
           {if(curr_c_[4])wakeup_a__(curr_c_[3],0);
            var curr_c_=curr_c_[2];
            continue}
          return _d_}}
      return _e_}
    function _d8_(e_a_)
     {return e_a_ instanceof array_constructor_b4_
              ?0
              :[0,new MlWrappedString_X_(e_a_.toString())]}
    _a1_[1]=[0,_d8_,_a1_[1]];
    function _b5_(_a_){return _a_}
    function _d_(p_a_,n_b_){p_a_.appendChild(n_b_);return 0}
    function _y_(f_d_)
     {return _b5_
              (caml_js_wrap_callback_bl_
                (function(e_a_)
                  {if(e_a_)
                    {var res_e_=caml_call_gen1_i_(f_d_,e_a_);
                     if(!(res_e_|0))e_a_.preventDefault();
                     return res_e_}
                   var _c_=event,res_b_=caml_call_gen1_i_(f_d_,_c_);
                   if(!(res_b_|0))_c_.returnValue=res_b_;
                   return res_b_}))}
    function _b6_(s_a_){return s_a_.toString()}
    function _b7_(e_e_,typ_b_,h_c_,capt_d_)
     {if(e_e_.addEventListener===undefined_b3_)
       {var
         ev_f_="on".concat(typ_b_),
         callback_g_=
          function(e_a_)
           {var _d_=[0,h_c_,e_a_,[0]];
            return function(_a_,_b_){return caml_js_call_e__(_d_,_a_,_b_)}};
        e_e_.attachEvent(ev_f_,callback_g_);
        return function(param_a_){return e_e_.detachEvent(ev_f_,callback_g_)}}
      e_e_.addEventListener(typ_b_,h_c_,capt_d_);
      return function(param_a_)
       {return e_e_.removeEventListener(typ_b_,h_c_,capt_d_)}}
    function _b8_(id_a_){return caml_call_gen1_i_(id_a_,0)}
    var
     mouseup_d__=_b6_(_d9_),
     _2d__ax_="2d",
     mousemove_ea_=_b6_(_d$_),
     document_e_=_T_.document;
    function opt_iter_ay_(x_a_,f_b_)
     {return x_a_?caml_call_gen1_i_(f_b_,x_a_[1]):0}
    function createElement_bc_(doc_a_,name_b_)
     {return doc_a_.createElement(name_b_.toString())}
    function unsafeCreateElement_N_(doc_a_,name_b_)
     {return createElement_bc_(doc_a_,name_b_)}
    var createElementSyntax_b9_=[0,_cC_];
    function unsafeCreateElementEx_b__(_type_a_,name_b_,doc_c_,elt_d_)
     {for(;;)
       {if(0===_type_a_&&0===name_b_)return createElement_bc_(doc_c_,elt_d_);
        var _h_=createElementSyntax_b9_[1];
        if(_cC_===_h_)
         {try
           {var
             el_j_=document_e_.createElement('<input name="x">'),
             _k_=el_j_.tagName.toLowerCase()===_cq_?1:0,
             _m_=_k_?el_j_.name===_cA_?1:0:_k_,
             _i_=_m_}
          catch(_f_){var _i_=0}
          var _l_=_i_?_cx_:-1003883683;
          createElementSyntax_b9_[1]=_l_;
          continue}
        if(_cx_<=_h_)
         {var a_f_=new array_constructor_b4_();
          a_f_.push("<",elt_d_.toString());
          opt_iter_ay_
           (_type_a_,
            function(t_a_)
             {a_f_.push(' type="',caml_js_html_escape_cv_(t_a_),_aC_);
              return 0});
          opt_iter_ay_
           (name_b_,
            function(n_a_)
             {a_f_.push(' name="',caml_js_html_escape_cv_(n_a_),_aC_);
              return 0});
          a_f_.push(">");
          return doc_c_.createElement(a_f_.join(_F_))}
        var res_g_=createElement_bc_(doc_c_,elt_d_);
        opt_iter_ay_(_type_a_,function(t_a_){return res_g_.type=t_a_});
        opt_iter_ay_(name_b_,function(n_a_){return res_g_.name=n_a_});
        return res_g_}}
    function createInput_bd_(_type_a_,name_b_,doc_c_)
     {return unsafeCreateElementEx_b__(_type_a_,name_b_,doc_c_,_ed_)}
    function createLabel_b$_(doc_a_)
     {return unsafeCreateElement_N_(doc_a_,_ee_)}
    function createDiv_az_(doc_a_){return unsafeCreateElement_N_(doc_a_,_ef_)}
    var Canvas_not_available_ek_=[0,_ej_];
    _T_.HTMLElement===undefined_b3_;
    var _em_=caml_js_get_console_e$_(0),overflow_limit_be_=2147483;
    pause_hook_d5_[1]=
    function(param_a_)
     {return 1===param_a_
              ?(_T_.setTimeout(caml_js_wrap_callback_bl_(_d7_),0),0)
              :0};
    function _ca_(s_a_){return _em_.log(s_a_.toString())}
    async_exception_hook_a7_[1]=
    function(exn_a_){_ca_(_en_);_ca_(_bT_(exn_a_));return _bU_(stderr_Z_)};
    var
     pi_f_=4*Math.atan(1),
     obliquity_ac_=23.5*pi_f_/180,
     dark_bf_=Math.pow(0.2,2),
     button_type_cb_="button",
     width_z_=600,
     gamma_eo_=2;
    function toggle_button_cc_(txt1_a_,txt2_b_,action_c_)
     {var
       state_d_=[0,0],
       txt1_g_=txt1_a_.toString(),
       txt2_h_=txt2_b_.toString(),
       b_f_=createInput_bd_([0,button_type_cb_],0,document_e_);
      b_f_.value=txt1_g_;
      b_f_.onclick=
      _y_
       (function(param_a_)
         {state_d_[1]=1-state_d_[1];
          var _b_=state_d_[1]?txt2_h_:txt1_g_;
          b_f_.value=_b_;
          caml_call_gen1_i_(action_c_,state_d_[1]);
          return _true_x_});
      return b_f_}
    function checkbox_cd_(txt_a_,checked_b_,action_c_)
     {var b_f_=createInput_bd_([0,"checkbox"],0,document_e_);
      b_f_.checked=!!checked_b_;
      b_f_.onclick=
      _y_
       (function(param_a_)
         {caml_call_gen1_i_(action_c_,b_f_.checked|0);return _true_x_});
      var lab_g_=createLabel_b$_(document_e_);
      _d_(lab_g_,b_f_);
      _d_(lab_g_,document_e_.createTextNode(txt_a_.toString()));
      return lab_g_}
    function vertex_g_(x_a_,y_b_,z_c_){return [_I_,x_a_,y_b_,z_c_]}
    function vect_ce_(param_a_,_b_)
     {return [_I_,_b_[1]-param_a_[1],_b_[2]-param_a_[2],_b_[3]-param_a_[3]]}
    function matrix_vect_mul_ad_(m_a_,param_b_)
     {var
       z_c_=param_b_[3],
       y_d_=param_b_[2],
       x_e_=param_b_[1],
       r3_f_=m_a_[3],
       r2_g_=m_a_[2],
       r1_h_=m_a_[1];
      return [_I_,
              x_e_*r1_h_[1]+y_d_*r1_h_[2]+z_c_*r1_h_[3],
              x_e_*r2_g_[1]+y_d_*r2_g_[2]+z_c_*r2_g_[3],
              x_e_*r3_f_[1]+y_d_*r3_f_[2]+z_c_*r3_f_[3]]}
    function matrix_transp_cf_(m_a_)
     {var r3_b_=m_a_[3],r2_c_=m_a_[2],r1_d_=m_a_[1];
      return [0,
              [_I_,r1_d_[1],r2_c_[1],r3_b_[1]],
              [_I_,r1_d_[2],r2_c_[2],r3_b_[2]],
              [_I_,r1_d_[3],r2_c_[3],r3_b_[3]]]}
    function matrix_mul_aA_(m_a_,m__b_)
     {var
       m__c_=matrix_transp_cf_(m__b_),
       _d_=matrix_vect_mul_ad_(m__c_,m_a_[3]),
       _e_=matrix_vect_mul_ad_(m__c_,m_a_[2]);
      return [0,matrix_vect_mul_ad_(m__c_,m_a_[1]),_e_,_d_]}
    function xz_rotation_bg_(phi_a_)
     {var
       cos_phi_b_=Math.cos(phi_a_),
       sin_phi_c_=Math.sin(phi_a_),
       _d_=vertex_g_(-sin_phi_c_,0,cos_phi_b_),
       _e_=vertex_g_(0,1,0);
      return [0,vertex_g_(cos_phi_b_,0,sin_phi_c_),_e_,_d_]}
    function xy_rotation_cg_(phi_a_)
     {var
       cos_phi_b_=Math.cos(phi_a_),
       sin_phi_c_=Math.sin(phi_a_),
       _d_=vertex_g_(0,0,1),
       _e_=vertex_g_(-sin_phi_c_,cos_phi_b_,0);
      return [0,vertex_g_(cos_phi_b_,sin_phi_c_,0),_e_,_d_]}
    var matrix_identity_ch_=xz_rotation_bg_(0);
    function face_ae_(v1_a_,v2_b_,v3_c_){return [0,v1_a_,v2_b_,v3_c_]}
    function create_canvas_aB_(w_a_,h_b_)
     {var c_c_=unsafeCreateElement_N_(document_e_,_el_);
      if(1-(c_c_.getContext==null_av_?1:0))
       {c_c_.width=w_a_;c_c_.height=h_b_;return c_c_}
      throw [0,Canvas_not_available_ek_]}
    function min_V_(u_a_,v_b_){return u_a_<v_b_?u_a_:v_b_}
    function max_o_(u_a_,v_b_){return u_a_<v_b_?v_b_:u_a_}
    var
     t_delta_ci_=pi_f_/8,
     n_u_=8*12|0,
     vertices_C_=caml_make_vect_v_(n_u_+2|0,vertex_g_(0,0,0)),
     faces_D_=caml_make_vect_v_(n_u_*2|0,face_ae_(0,0,0)),
     south_cj_=n_u_+1|0;
    caml_array_set_h_(vertices_C_,n_u_,vertex_g_(0,-1,0));
    caml_array_set_h_(vertices_C_,south_cj_,vertex_g_(0,1,0));
    var
     _ck_=12-1|0,
     _E_=8,
     p_delta_ep_=2*pi_f_/12,
     t_offset_eq_=(pi_f_-t_delta_ci_)/2,
     _er_=0;
    if(!(_ck_<0))
     {var i_af_=_er_;
      for(;;)
       {var _cl_=_E_-1|0,_es_=0;
        if(!(_cl_<0))
         {var j_U_=_es_;
          for(;;)
           {var
             phi_cm_=i_af_*p_delta_ep_,
             theta_bh_=j_U_*t_delta_ci_-t_offset_eq_,
             k_a_=(i_af_*_E_|0)+j_U_|0;
            caml_array_set_h_
             (vertices_C_,
              k_a_,
              vertex_g_
               (Math.cos(phi_cm_)*Math.cos(theta_bh_),
                Math.sin(theta_bh_),
                Math.sin(phi_cm_)*Math.cos(theta_bh_)));
            if(0===j_U_)
             {caml_array_set_h_
               (faces_D_,
                2*k_a_|0,
                face_ae_(n_u_,k_a_,caml_mod_ag_(k_a_+_E_|0,n_u_)));
              caml_array_set_h_
               (faces_D_,
                (2*k_a_|0)+1|0,
                face_ae_
                 (south_cj_,
                  caml_mod_ag_((k_a_+(2*_E_|0)|0)-1|0,n_u_),
                  (k_a_+_E_|0)-1|0))}
            else
             {caml_array_set_h_
               (faces_D_,
                2*k_a_|0,
                face_ae_(k_a_,caml_mod_ag_(k_a_+_E_|0,n_u_),k_a_-1|0));
              caml_array_set_h_
               (faces_D_,
                (2*k_a_|0)+1|0,
                face_ae_
                 (k_a_-1|0,
                  caml_mod_ag_(k_a_+_E_|0,n_u_),
                  caml_mod_ag_((k_a_+_E_|0)-1|0,n_u_)))}
            var _eu_=j_U_+1|0;
            if(_cl_!==j_U_){var j_U_=_eu_;continue}
            break}}
        var _et_=i_af_+1|0;
        if(_ck_!==i_af_){var i_af_=_et_;continue}
        break}}
    var texture_eA_="../planet/texture.jpg";
    _T_.onload=
    _y_
     (function(param_a_)
       {function _s_(texture_a_)
         {var
           w_aa_=texture_a_.width,
           h_ab_=texture_a_.height,
           canvas_E_=create_canvas_aB_(w_aa_,h_ab_),
           ctx_h_=canvas_E_.getContext(_2d__ax_),
           _s_=h_ab_/8|0,
           _k_=w_aa_/8|0,
           img_ae_=ctx_h_.getImageData(0,0,_k_,_s_),
           data_af_=img_ae_.data,
           inv_gamma_ao_=1/gamma_eo_;
          function update_shadow_ag_(obliquity_a_)
           {var
             _d_=_s_-1|0,
             cos_obl_m_=Math.cos(obliquity_a_),
             sin_obl_n_=-Math.sin(obliquity_a_),
             _o_=0;
            if(!(_d_<0))
             {var j_b_=_o_;
              for(;;)
               {var _e_=(_k_/2|0)-1|0,_p_=0;
                if(!(_e_<0))
                 {var i_c_=_p_;
                  for(;;)
                   {var
                     theta_g_=(j_b_/_s_-0.5)*pi_f_,
                     _i_=
                      Math.cos(i_c_/_k_*2*pi_f_)*
                      Math.cos(theta_g_)*
                      cos_obl_m_+
                      Math.sin(theta_g_)*
                      sin_obl_n_,
                     k_r_=4*(i_c_+j_b_*_k_)|0,
                     k__t_=4*(_k_-i_c_+j_b_*_k_-1)|0,
                     c_j_=0<_i_?dark_bf_:dark_bf_-_i_*(1-dark_bf_)*1.2,
                     c_u_=c_j_<=1?c_j_:1,
                     c_l_=_H_-(255.99*Math.pow(c_u_,inv_gamma_ao_)|0)|0;
                    data_af_[k_r_+3|0]=c_l_;
                    data_af_[k__t_+3|0]=c_l_;
                    var _v_=i_c_+1|0;
                    if(_e_!==i_c_){var i_c_=_v_;continue}
                    break}}
                var _q_=j_b_+1|0;
                if(_d_!==j_b_){var j_b_=_q_;continue}
                break}}
            ctx_h_.putImageData(img_ae_,0,0);
            ctx_h_.globalCompositeOperation=_cw_;
            ctx_h_.save();
            ctx_h_.scale(8*(_k_+2|0)/_k_,8*(_s_+2|0)/_s_);
            ctx_h_.translate(-1,-1);
            ctx_h_.drawImage(canvas_E_,0,0);
            return ctx_h_.restore()}
          update_shadow_ag_(obliquity_ac_);
          var
           w_G_=texture_a_.width,
           canvas__ai_=create_canvas_aB_(w_G_,texture_a_.height),
           ctx__J_=canvas__ai_.getContext(_2d__ax_),
           no_lighting_P_=[0,0],
           canvas_Q_=create_canvas_aB_(width_z_,width_z_),
           canvas__at_=create_canvas_aB_(width_z_,width_z_);
          _d_(document_e_.body,canvas_Q_);
          var
           ctx_R_=canvas_Q_.getContext(_2d__ax_),
           ctx__c_=canvas__at_.getContext(_2d__ax_),
           r_n_=width_z_/2,
           tw_b_=texture_a_.width,
           th_t_=texture_a_.height,
           uv_U_=
            _an_
             (function(v_a_)
               {var
                 u_c_=
                  (tw_b_-Math.atan2(v_a_[3],v_a_[1])*((tw_b_/2-_cB_)/pi_f_)|0)%
                  tw_b_,
                 v_d_=th_t_/2+Math.asin(v_a_[2])*((th_t_-_cB_)/pi_f_)|0;
                if(0<=u_c_)
                 {if(u_c_<tw_b_)
                   {if(0<=v_d_)
                     {if(v_d_<th_t_)return [0,u_c_,v_d_];throw [0,_m_,_ev_]}
                    throw [0,_m_,_ew_]}
                  throw [0,_m_,_ex_]}
                throw [0,_m_,_ey_]},
              vertices_C_),
           normals_aL_=
            _an_
             (function(param_a_)
               {var
                 v1_k_=caml_array_get_j_(vertices_C_,param_a_[1]),
                 v2_l_=caml_array_get_j_(vertices_C_,param_a_[2]),
                 _b_=
                  vect_ce_(v1_k_,caml_array_get_j_(vertices_C_,param_a_[3])),
                 _c_=vect_ce_(v1_k_,v2_l_),
                 z2_d_=_b_[3],
                 y2_e_=_b_[2],
                 x2_f_=_b_[1],
                 z1_g_=_c_[3],
                 y1_h_=_c_[2],
                 x1_i_=_c_[1];
                return [_I_,
                        y1_h_*z2_d_-y2_e_*z1_g_,
                        z1_g_*x2_f_-z2_d_*x1_i_,
                        x1_i_*y2_e_-x2_f_*y1_h_]},
              faces_D_),
           paused_W_=[0,0],
           follow_au_=[0,0],
           lighting_ay_=[0,1],
           clipped_aC_=[0,1],
           obl_aj_=[0,obliquity_ac_],
           face_info_aM_=
            _an_
             (function(f_a_)
               {var
                 match_E_=caml_array_get_j_(uv_U_,f_a_[1]),
                 v1_l_=match_E_[2],
                 u1_F_=match_E_[1],
                 match_G_=caml_array_get_j_(uv_U_,f_a_[2]),
                 v2_m_=match_G_[2],
                 u2_n_=match_G_[1],
                 match_H_=caml_array_get_j_(uv_U_,f_a_[3]),
                 v3_p_=match_H_[2],
                 u3_f_=match_H_[1],
                 mid_c_=tw_b_/2;
                if(u1_F_==0)
                 {if(mid_c_<u2_n_||mid_c_<u3_f_)
                   var _N_=1;
                  else
                   {var _B_=0,_N_=0}
                  if(_N_){var u1_g_=tw_b_-2,_B_=1}}
                else
                 var _B_=0;
                if(!_B_)var u1_g_=u1_F_;
                if(u2_n_==0)
                 {if(mid_c_<u1_g_||mid_c_<u3_f_)
                   var _O_=1;
                  else
                   {var _C_=0,_O_=0}
                  if(_O_){var u2_h_=tw_b_-2,_C_=1}}
                else
                 var _C_=0;
                if(!_C_)var u2_h_=u2_n_;
                if(u3_f_==0)
                 {if(mid_c_<u2_h_||mid_c_<u1_g_)
                   var _P_=1;
                  else
                   {var _D_=0,_P_=0}
                  if(_P_){var u3_i_=tw_b_-2,_D_=1}}
                else
                 var _D_=0;
                if(!_D_)var u3_i_=u3_f_;
                var mth_q_=th_t_-2;
                if(v1_l_==0||mth_q_<=v1_l_)
                 var _Q_=0;
                else
                 {var u1_k_=u1_g_,_Q_=1}
                if(!_Q_)var u1_k_=(u2_h_+u3_i_)/2;
                if(v2_m_==0||mth_q_<=v2_m_)
                 var _R_=0;
                else
                 {var u2_r_=u2_h_,_R_=1}
                if(!_R_)var u2_r_=(u1_k_+u3_i_)/2;
                if(v3_p_==0||mth_q_<=v3_p_)
                 var _S_=0;
                else
                 {var u3_I_=u3_i_,_S_=1}
                if(!_S_)var u3_I_=(u2_r_+u1_k_)/2;
                var
                 u1_d_=max_o_(1,u1_k_),
                 u2_s_=max_o_(1,u2_r_),
                 u3_u_=max_o_(1,u3_I_),
                 v1_e_=max_o_(1,v1_l_),
                 v2_v_=max_o_(1,v2_m_),
                 v3_w_=max_o_(1,v3_p_),
                 du2_x_=u2_s_-u1_d_,
                 du3_y_=u3_u_-u1_d_,
                 dv2_z_=v2_v_-v1_e_,
                 dv3_A_=v3_w_-v1_e_,
                 su_J_=dv2_z_*du3_y_-dv3_A_*du2_x_,
                 sv_K_=du2_x_*dv3_A_-du3_y_*dv2_z_,
                 u_L_=max_o_(0,min_V_(u1_d_,min_V_(u2_s_,u3_u_))-4),
                 v_M_=max_o_(0,min_V_(v1_e_,min_V_(v2_v_,v3_w_))-4),
                 u__T_=min_V_(tw_b_,max_o_(u1_d_,max_o_(u2_s_,u3_u_))+4);
                return [0,
                        u1_d_,
                        v1_e_,
                        du2_x_/su_J_,
                        dv2_z_/sv_K_,
                        du3_y_/su_J_,
                        dv3_A_/sv_K_,
                        u_L_,
                        v_M_,
                        u__T_-u_L_,
                        min_V_(th_t_,max_o_(v1_e_,max_o_(v2_v_,v3_w_))+4)-v_M_]},
              faces_D_),
           m_obliq_aD_=[0,xy_rotation_cg_(-obliquity_ac_)],
           m_r_=[0,matrix_identity_ch_],
           phi_rot_K_=[0,0],
           rateText_aE_=document_e_.createTextNode(_F_),
           ctrl_u_=createDiv_az_(document_e_);
          ctrl_u_.className="controls";
          var d_ak_=createDiv_az_(document_e_);
          _d_
           (d_ak_,
            document_e_.createTextNode("Click and drag mouse to rotate."));
          _d_(ctrl_u_,d_ak_);
          var form_l_=createDiv_az_(document_e_);
          function br_v_(param_a_)
           {return unsafeCreateElement_N_(document_e_,_eh_)}
          _d_
           (form_l_,
            toggle_button_cc_
             (_eC_,_eB_,function(p_a_){paused_W_[1]=p_a_;return 0}));
          _d_(form_l_,br_v_(0));
          _d_
           (form_l_,
            toggle_button_cc_
             (_eE_,_eD_,function(f_a_){follow_au_[1]=f_a_;return 0}));
          _d_(form_l_,br_v_(0));
          var b_O_=createInput_bd_([0,button_type_cb_],0,document_e_);
          b_O_.value="Reset orientation";
          b_O_.onclick=
          _y_
           (function(param_a_)
             {m_r_[1]=matrix_identity_ch_;
              phi_rot_K_[1]=0;
              m_obliq_aD_[1]=xy_rotation_cg_(-obl_aj_[1]);
              return _true_x_});
          _d_(form_l_,b_O_);
          _d_(form_l_,br_v_(0));
          var lab_X_=createLabel_b$_(document_e_);
          _d_(lab_X_,document_e_.createTextNode("Date:"));
          var
           _A_=unsafeCreateElementEx_b__(0,0,document_e_,_ec_),
           param_w_=_eF_;
          for(;;)
           {if(param_w_)
             {var
               l_al_=param_w_[2],
               a_am_=param_w_[1],
               ___=unsafeCreateElement_N_(document_e_,_eb_);
              _d_(___,document_e_.createTextNode(a_am_.toString()));
              _A_.add(___,null_av_);
              var param_w_=l_al_;
              continue}
            _A_.onchange=
            _y_
             (function(param_a_)
               {var
                 _b_=_A_.selectedIndex,
                 o_c_=0===_b_?obliquity_ac_:1===_b_?0:-obliquity_ac_;
                update_shadow_ag_(o_c_);
                obl_aj_[1]=o_c_;
                return _true_x_});
            _d_(lab_X_,_A_);
            _d_(form_l_,lab_X_);
            _d_(ctrl_u_,form_l_);
            var form_q_=createDiv_az_(document_e_);
            _d_
             (form_q_,
              checkbox_cd_
               (_eG_,1,function(l_a_){lighting_ay_[1]=l_a_;return 0}));
            _d_(form_q_,br_v_(0));
            _d_
             (form_q_,
              checkbox_cd_
               (_eH_,1,function(l_a_){clipped_aC_[1]=l_a_;return 0}));
            _d_(form_q_,br_v_(0));
            _d_(form_q_,document_e_.createTextNode("Frames per second: "));
            _d_(form_q_,rateText_aE_);
            _d_(ctrl_u_,form_q_);
            _d_(document_e_.body,ctrl_u_);
            var _$_=unsafeCreateElement_N_(document_e_,_eg_);
            _$_.innerHTML=
            "Credit: <a href='http://visibleearth.nasa.gov/'>Visual Earth</a>, Nasa";
            _d_(document_e_.body,_$_);
            var mx_Y_=[0,0],my_Z_=[0,0];
            canvas_Q_.onmousedown=
            _y_
             (function(ev_a_)
               {mx_Y_[1]=ev_a_.clientX;
                my_Z_[1]=ev_a_.clientY;
                var
                 c2_b_=[0,null_av_],
                 c1_d_=
                  _b7_
                   (document_e_,
                    mousemove_ea_,
                    _y_
                     (function(ev_a_)
                       {var
                         x_d_=ev_a_.clientX,
                         y_e_=ev_a_.clientY,
                         dx_f_=x_d_-mx_Y_[1]|0,
                         dy_h_=y_e_-my_Z_[1]|0;
                        if(0!==dy_h_)
                         {var
                           _i_=2*dy_h_/width_z_,
                           cos_phi_b_=Math.cos(_i_),
                           sin_phi_c_=Math.sin(_i_),
                           _m_=m_r_[1],
                           _j_=vertex_g_(0,-sin_phi_c_,cos_phi_b_),
                           _k_=vertex_g_(0,cos_phi_b_,sin_phi_c_);
                          m_r_[1]=matrix_mul_aA_([0,vertex_g_(1,0,0),_k_,_j_],_m_)}
                        if(0!==dx_f_)
                         {var _l_=m_r_[1];
                          m_r_[1]=
                          matrix_mul_aA_(xz_rotation_bg_(2*dx_f_/width_z_),_l_)}
                        mx_Y_[1]=x_d_;
                        my_Z_[1]=y_e_;
                        return _true_x_}),
                    _true_x_);
                c2_b_[1]=
                _b5_
                 (_b7_
                   (document_e_,
                    mouseup_d__,
                    _y_
                     (function(param_a_)
                       {_b8_(c1_d_);
                        var _c_=c2_b_[1];
                        if(_c_!=null_av_)_b8_(_c_);
                        return _true_x_}),
                    _true_x_));
                return _false_bb_});
            var
             ti_aF_=[0,new date_constr_aw_().getTime()],
             fps_L_=[0,0],
             loop_aG_=
              function(t_aN_,phi_b_)
               {var rotation_aO_=xz_rotation_bg_(phi_b_-phi_rot_K_[1]);
                if(lighting_ay_[1])
                 {no_lighting_P_[1]=0;
                  ctx__J_.drawImage(texture_a_,0,0);
                  var i_I_=(2*pi_f_-phi_b_%(2*pi_f_))*w_G_/2/pi_f_%w_G_|0;
                  ctx__J_.drawImage(canvas_E_,i_I_,0);
                  ctx__J_.drawImage(canvas_E_,i_I_-w_G_,0)}
                else
                 if(!no_lighting_P_[1])
                  {ctx__J_.drawImage(texture_a_,0,0);no_lighting_P_[1]=1}
                var
                 _aP_=matrix_mul_aA_(m_obliq_aD_[1],rotation_aO_),
                 m_av_=matrix_mul_aA_(m_r_[1],_aP_),
                 _o_=
                  _an_
                   (function(v_a_){return matrix_vect_mul_ad_(m_av_,v_a_)},
                    vertices_C_),
                 _m_=matrix_vect_mul_ad_(matrix_transp_cf_(m_av_),v_ez_);
                ctx__c_.clearRect(0,0,width_z_,width_z_);
                ctx__c_.save();
                if(clipped_aC_[1])
                 {ctx__c_.beginPath();
                  ctx__c_.arc(r_n_,r_n_,r_n_*0.95,0,-2*pi_f_,_true_x_);
                  ctx__c_.clip()}
                ctx__c_.setTransform(r_n_-2,0,0,r_n_-2,r_n_,r_n_);
                ctx__c_.globalCompositeOperation="lighter";
                var _s_=faces_D_.length-1-1|0,_aH_=0;
                if(!(_s_<0))
                 {var i_e_=_aH_;
                  for(;;)
                   {var
                     _k_=faces_D_[i_e_+1],
                     match_N_=caml_array_get_j_(_o_,_k_[1]),
                     y1_g_=match_N_[2],
                     x1_h_=match_N_[1],
                     match_O_=caml_array_get_j_(_o_,_k_[2]),
                     y2_Q_=match_O_[2],
                     x2_U_=match_O_[1],
                     match_V_=caml_array_get_j_(_o_,_k_[3]),
                     y3_X_=match_V_[2],
                     x3_Y_=match_V_[1],
                     _q_=caml_array_get_j_(normals_aL_,i_e_);
                    if(0<=_q_[1]*_m_[1]+_q_[2]*_m_[2]+_q_[3]*_m_[3])
                     {ctx__c_.beginPath();
                      ctx__c_.moveTo(x1_h_,y1_g_);
                      ctx__c_.lineTo(x2_U_,y2_Q_);
                      ctx__c_.lineTo(x3_Y_,y3_X_);
                      ctx__c_.closePath();
                      ctx__c_.save();
                      ctx__c_.clip();
                      var
                       match_d_=caml_array_get_j_(face_info_aM_,i_e_),
                       dv_Z_=match_d_[10],
                       du___=match_d_[9],
                       v_$_=match_d_[8],
                       u_aa_=match_d_[7],
                       dv3_ab_=match_d_[6],
                       du3_ac_=match_d_[5],
                       dv2_ae_=match_d_[4],
                       du2_af_=match_d_[3],
                       v1_ag_=match_d_[2],
                       u1_aj_=match_d_[1],
                       dx2_ak_=x2_U_-x1_h_,
                       dx3_al_=x3_Y_-x1_h_,
                       dy2_am_=y2_Q_-y1_g_,
                       dy3_ao_=y3_X_-y1_g_,
                       a_ap_=dx2_ak_*dv3_ab_-dx3_al_*dv2_ae_,
                       b_aq_=dx2_ak_*du3_ac_-dx3_al_*du2_af_,
                       d_ar_=dy2_am_*dv3_ab_-dy3_ao_*dv2_ae_,
                       e_as_=dy2_am_*du3_ac_-dy3_ao_*du2_af_;
                      ctx__c_.transform
                       (a_ap_,
                        d_ar_,
                        b_aq_,
                        e_as_,
                        x1_h_-a_ap_*u1_aj_-b_aq_*v1_ag_,
                        y1_g_-d_ar_*u1_aj_-e_as_*v1_ag_);
                      ctx__c_.drawImage
                       (canvas__ai_,u_aa_,v_$_,du___,dv_Z_,u_aa_,v_$_,du___,dv_Z_);
                      ctx__c_.restore()}
                    var _aI_=i_e_+1|0;
                    if(_s_!==i_e_){var i_e_=_aI_;continue}
                    break}}
                ctx__c_.restore();
                ctx_R_.globalCompositeOperation=_cw_;
                ctx_R_.drawImage(canvas__at_,0,0);
                try {ctx_R_.getImageData(0,0,1,1)}catch(_f_){}
                var
                 t__ax_=new date_constr_aw_().getTime(),
                 hz_az_=_ah_/(t__ax_-ti_aF_[1]),
                 _aQ_=fps_L_[1]==0?hz_az_:0.9*fps_L_[1]+0.1*hz_az_;
                fps_L_[1]=_aQ_;
                var _aR_=fps_L_[1];
                rateText_aE_.data=
                caml_call_gen1_i_(_p_(_eI_),_aR_).toString();
                ti_aF_[1]=t__ax_;
                function _aS_(param_a_)
                 {var
                   t__d_=new date_constr_aw_().getTime(),
                   dt_c_=t__d_-t_aN_,
                   dt_g_=dt_c_<0?0:_ah_<dt_c_?0:dt_c_,
                   angle_e_=2*pi_f_*dt_g_/_ah_/10,
                   _i_=
                    paused_W_[1]
                     ?0
                     :follow_au_[1]?(phi_rot_K_[1]=phi_rot_K_[1]+angle_e_,1):0,
                   _h_=paused_W_[1]?phi_b_:phi_b_+angle_e_;
                  return loop_aG_(t__d_,_h_)}
                var
                 match_w_=task_b1_(0),
                 t_y_=match_w_[1],
                 id_A_=[0,0],
                 _aT_=0.01,
                 w_aK_=match_w_[2];
                function wait_F_(d_a_,param_b_)
                 {var
                   match_c_=
                    overflow_limit_be_<d_a_
                     ?[0,overflow_limit_be_,d_a_-overflow_limit_be_]
                     :[0,d_a_,0],
                   remain_d_=match_c_[2],
                   step_e_=match_c_[1],
                   cb_f_=
                    remain_d_==0
                     ?function(_a_){return wakeup_a__(w_aK_,_a_)}
                     :function(_a_){return wait_F_(remain_d_,_a_)};
                  id_A_[1]=
                  [0,
                   _T_.setTimeout
                    (caml_js_wrap_callback_bl_(cb_f_),step_e_*_ah_)];
                  return 0}
                wait_F_(_aT_,0);
                function _H_(param_a_)
                 {var _b_=id_A_[1];return _b_?_T_.clearTimeout(_b_[1]):0}
                var _l_=repr_S_(t_y_)[1];
                switch(_l_[0])
                 {case 1:
                   var
                    _aB_=
                     _l_[1][1]===Canceled_a5_?(call_unsafe_bX_(_H_,0),1):0;
                   break;
                  case 2:
                   var
                    sleeper_t_=_l_[1],
                    handler_u_=[0,current_data_M_[1],_H_],
                    _v_=sleeper_t_[4],
                    _aJ_=typeof _v_===_B_?handler_u_:[2,handler_u_,_v_];
                   sleeper_t_[4]=_aJ_;
                   var _aB_=1;
                   break;
                  default:var _aB_=0}
                return bind_ba_(t_y_,_aS_)};
            return loop_aG_(new date_constr_aw_().getTime(),0)}}
        var _c_=unsafeCreateElement_N_(document_e_,_ei_);
        function _r_(param_a_){return [0,[0,_c_]]}
        var match_h_=task_b1_(0),w_l_=match_h_[2],t_n_=match_h_[1];
        function cont_q_(x_a_){return wakeup_a__(w_l_,x_a_)}
        _c_.onload=_y_(function(param_a_){cont_q_(0);return _false_bb_});
        _c_.src=texture_eA_;
        var _b_=repr_S_(bind_ba_(bind_ba_(t_n_,_r_),_s_))[1];
        switch(_b_[0])
         {case 1:throw _b_[1];
          case 2:
           var sleeper_k_=_b_[1];
           add_immutable_waiter_b2_
            (sleeper_k_,
             function(param_a_)
              {switch(param_a_[0])
                {case 0:return 0;
                 case 1:
                  return caml_call_gen1_i_
                          (async_exception_hook_a7_[1],param_a_[1]);
                 default:throw [0,_m_,_d3_]}});
           break;
          case 3:throw [0,_m_,_d4_];
          default:}
        return _false_bb_});
    do_at_exit_aV_(0);
    return}
  (this));
