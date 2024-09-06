package com.project.trash.member.controller.validation;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.utils.ValidatorUtils;
import com.project.trash.member.domain.enums.SocialType;
import com.project.trash.member.request.LoginRequest;
import com.project.trash.member.request.ReissueRequest;

import lombok.experimental.UtilityClass;

/**
 * 회원 요청 파라미터 검증
 */
@UtilityClass
public class MemberValidator {

  /**
   * 로그인 유효성 검증
   */
  public void validate(LoginRequest param) {
    ValidatorUtils.validateEmpty(param.getSocialType(), "member.param_social_type_empty");
    if (!SocialType.containCode(param.getSocialType())) {
      throw new ValidationException("member.param_social_type_invalid");
    }
    ValidatorUtils.validateEmpty(param.getAccessToken(), "member.param_access_token_empty");
    ValidatorUtils.validateEmpty(param.getSocialId(), "member.param_social_id_empty");
  }

  /**
   * 엑세스 토큰 재발급 유효성 검증
   */
  public void validate(ReissueRequest param) {
    ValidatorUtils.validateEmpty(param.getSocialId(), "member.param_social_id_empty");
    ValidatorUtils.validateEmpty(param.getRefreshToken(), "member.param_refresh_token_empty");
  }
}
