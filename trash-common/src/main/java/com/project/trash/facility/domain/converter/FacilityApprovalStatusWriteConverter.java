package com.project.trash.facility.domain.converter;

import com.project.trash.facility.domain.enums.FacilityApprovalStatus;

import org.springframework.core.convert.converter.Converter;
import org.springframework.data.convert.WritingConverter;

/**
 * 시설물 승인 상태 쓰기 Converter
 */
@WritingConverter
public class FacilityApprovalStatusWriteConverter implements Converter<FacilityApprovalStatus, String> {

  @Override
  public String convert(FacilityApprovalStatus status) {
    return status.getCode();
  }
}
