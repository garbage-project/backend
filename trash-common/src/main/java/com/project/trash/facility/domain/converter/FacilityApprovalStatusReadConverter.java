package com.project.trash.facility.domain.converter;

import com.project.trash.facility.domain.enums.FacilityApprovalStatus;

import org.springframework.core.convert.converter.Converter;
import org.springframework.data.convert.ReadingConverter;

/**
 * 시설물 승인 상태 읽기 Converter
 */
@ReadingConverter
public class FacilityApprovalStatusReadConverter implements Converter<String, FacilityApprovalStatus> {

  @Override
  public FacilityApprovalStatus convert(String status) {
    return FacilityApprovalStatus.fromCode(status);
  }
}
