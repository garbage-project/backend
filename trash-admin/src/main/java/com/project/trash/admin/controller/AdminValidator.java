package com.project.trash.admin.controller;

import com.project.trash.admin.request.AdminModifyRequest;
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
    ValidatorUtils.validateEmpty(param.getPassword(), "auth.param_password_empty");
  }
}