package com.project.trash.member.controller.validation;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.utils.ValidatorUtils;
import com.project.trash.member.domain.enums.SocialType;
import com.project.trash.member.request.LoginRequest;
import com.project.trash.member.request.ReissueRequest;

import lombok.experimental.UtilityClass;

import static com.project.trash.common.domain.resultcode.RequestResultCode.PARAM_INVALID;

/**
 * 회원 요청 파라미터 검증
 */
@UtilityClass
public class MemberValidator {

  /**
   * 로그인 유효성 검증
   */
  public void validate(LoginRequest param) {
    ValidatorUtils.validateEmpty(param.getSocialType());
    if (!SocialType.containCode(param.getSocialType())) {
      throw new ValidationException(PARAM_INVALID);
    }
    ValidatorUtils.validateEmpty(param.getAccessToken());
    ValidatorUtils.validateEmpty(param.getSocialId());
  }

  /**
   * 엑세스 토큰 재발급 유효성 검증
   */
  public void validate(ReissueRequest param) {
    ValidatorUtils.validateEmpty(param.getSocialId());
    ValidatorUtils.validateEmpty(param.getRefreshToken());
  }
}
