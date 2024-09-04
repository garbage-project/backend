package com.project.trash.auth.controller;

import com.project.trash.auth.request.LoginRequest;
import com.project.trash.auth.request.ReissueRequest;
import com.project.trash.common.utils.ValidatorUtils;

import lombok.experimental.UtilityClass;

/**
 * 인증 요청 파라미터 검증
 */
@UtilityClass
public class AuthValidator {

  /**
   * 로그인 유효성 검증
   */
  public void validate(LoginRequest param) {
    ValidatorUtils.validateEmpty(param.getId(), "auth.param_id_empty");
    ValidatorUtils.validateEmpty(param.getPassword(), "auth.param_password_empty");
  }

  /**
   * 엑세스 토큰 재발급 유효성 검증
   */
  public void validate(ReissueRequest param) {
    ValidatorUtils.validateEmpty(param.getId(), "auth.param_id_empty");
    ValidatorUtils.validateEmpty(param.getRefreshToken(), "auth.param_refresh_token_empty");
  }
}