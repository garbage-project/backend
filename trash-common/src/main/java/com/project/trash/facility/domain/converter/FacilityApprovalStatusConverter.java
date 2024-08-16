package com.project.trash.facility.domain.converter;

import com.project.trash.facility.domain.enums.FacilityApprovalStatus;

import org.springframework.data.mongodb.core.convert.MongoConversionContext;
import org.springframework.data.mongodb.core.convert.MongoValueConverter;

/**
 * 시설물 승인 상태 Converter
 */
public class FacilityApprovalStatusConverter implements MongoValueConverter<FacilityApprovalStatus, String> {

  @Override
  public FacilityApprovalStatus read(String value, MongoConversionContext context) {
    return FacilityApprovalStatus.fromCode(value);
  }

  @Override
  public String write(FacilityApprovalStatus value, MongoConversionContext context) {
    return value.getCode();
  }
}
