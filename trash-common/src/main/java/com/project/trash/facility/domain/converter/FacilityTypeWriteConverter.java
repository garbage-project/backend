package com.project.trash.facility.domain.converter;

import com.project.trash.facility.domain.enums.FacilityType;

import org.springframework.core.convert.converter.Converter;
import org.springframework.data.convert.WritingConverter;

/**
 * 시설물 종류 쓰기 Converter
 */
@WritingConverter
public class FacilityTypeWriteConverter implements Converter<FacilityType, String> {

  @Override
  public String convert(FacilityType type) {
    return type.getCode();
  }
}
