package com.project.trash.common.domain.converter;

import com.project.trash.common.domain.enums.Valid;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;

/**
 * Boolean -> DB 문자로 변경
 */
@Converter(autoApply = true)
public class BooleanToStringConverter implements AttributeConverter<Boolean, String> {

  @Override
  public String convertToDatabaseColumn(Boolean attribute) {
    if (attribute == null) {
      return null;
    }
    if (attribute) {
      // TRUE -> 'Y'
      return Valid.TRUE.getCode();
    } else {
      // FALSE -> 'N'
      return Valid.FALSE.getCode();
    }
  }

  @Override
  public Boolean convertToEntityAttribute(String data) {
    if (data == null) {
      return null;
    }
    if (data.equalsIgnoreCase(Valid.TRUE.getCode())) {
      // 'Y' -> TRUE
      return Boolean.TRUE;
    } else {
      // 'N' -> FALSE
      return Boolean.FALSE;
    }
  }
}
