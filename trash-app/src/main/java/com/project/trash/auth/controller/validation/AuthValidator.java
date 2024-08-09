package com.project.trash.auth.controller.validation;

import com.project.trash.auth.request.LoginRequest;
import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.utils.ValidatorUtils;
import com.project.trash.member.domain.enums.SocialType;

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
    ValidatorUtils.validateEmpty(param.getSocialType(), "auth.param_social_type_empty");
    if (!SocialType.containCode(param.getSocialType())) {
      throw new ValidationException("auth.param_social_type_invalid");
    }
    ValidatorUtils.validateEmpty(param.getAccessToken(), "auth.param_access_token_empty");
    ValidatorUtils.validateEmpty(param.getSocialId(), "auth.param_social_id_empty");
  }
}
