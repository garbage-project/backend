package com.project.trash.common.utils;

import com.project.trash.common.exception.ValidationException;

import org.apache.commons.lang3.StringUtils;

import java.util.Objects;

import lombok.experimental.UtilityClass;

/**
 * 파라미터 유효성 검증 유틸
 */
@UtilityClass
public class ValidatorUtils {

  /**
   * 문자열 공백 검증
   */
  public void validateEmpty(String param, String validation) {
    if (StringUtils.isBlank(param)) {
      throw new ValidationException(validation);
    }
  }

  /**
   * null 검증
   */
  public void validateNull(Object param, String validation) {
    if (Objects.isNull(param)) {
      throw new ValidationException(validation);
    }
  }
}
