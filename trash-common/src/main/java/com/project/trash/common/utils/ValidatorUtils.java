package com.project.trash.common.utils;

import com.project.trash.common.exception.ValidationException;

import org.apache.commons.lang3.StringUtils;

import java.util.Objects;

import lombok.experimental.UtilityClass;

import static com.project.trash.common.domain.resultcode.RequestResultCode.PARAM_INVALID;

/**
 * 파라미터 유효성 검증 유틸
 */
@UtilityClass
public class ValidatorUtils {

  /**
   * 문자열 공백 검증
   */
  public void validateEmpty(String param) {
    if (StringUtils.isBlank(param)) {
      throw new ValidationException(PARAM_INVALID);
    }
  }

  /**
   * null 검증
   */
  public void validateNull(Object param) {
    if (Objects.isNull(param)) {
      throw new ValidationException(PARAM_INVALID);
    }
  }
}
