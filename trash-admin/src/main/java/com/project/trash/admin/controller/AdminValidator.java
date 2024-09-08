package com.project.trash.admin.controller;

import com.project.trash.admin.request.AdminModifyRequest;
import com.project.trash.admin.request.LoginRequest;
import com.project.trash.admin.request.ReissueRequest;
import com.project.trash.common.utils.ValidatorUtils;

import lombok.experimental.UtilityClass;

/**
 * 관리자 요청 파라미터 검증
 */
@UtilityClass
public class AdminValidator {

  /**
   * 관리자 수정 요청 검증
   */
  public void validate(AdminModifyRequest param) {
    ValidatorUtils.validateEmpty(param.getPassword());
  }

  /**
   * 로그인 유효성 검증
   */
  public void validate(LoginRequest param) {
    ValidatorUtils.validateEmpty(param.getId());
    ValidatorUtils.validateEmpty(param.getPassword());
  }

  /**
   * 엑세스 토큰 재발급 유효성 검증
   */
  public void validate(ReissueRequest param) {
    ValidatorUtils.validateEmpty(param.getId());
  }
}