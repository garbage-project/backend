package com.project.trash.facility.domain.converter;

import com.project.trash.facility.domain.enums.FacilityType;

import org.springframework.data.mongodb.core.convert.MongoConversionContext;
import org.springframework.data.mongodb.core.convert.MongoValueConverter;

/**
 * 시설물 종류 Converter
 */
public class FacilityTypeConverter implements MongoValueConverter<FacilityType, String> {

  @Override
  public FacilityType read(String value, MongoConversionContext context) {
    return FacilityType.fromCode(value);
  }

  @Override
  public String write(FacilityType value, MongoConversionContext context) {
    return value.getCode();
  }
}
