package com.project.trash.facility.domain.converter;

import com.project.trash.facility.domain.enums.FacilityType;

import org.springframework.core.convert.converter.Converter;
import org.springframework.data.convert.ReadingConverter;

/**
 * 시설물 종류 읽기 Converter
 */
@ReadingConverter
public class FacilityTypeReadConverter implements Converter<String, FacilityType> {

  @Override
  public FacilityType convert(String type) {
    return FacilityType.fromCode(type);
  }
}
