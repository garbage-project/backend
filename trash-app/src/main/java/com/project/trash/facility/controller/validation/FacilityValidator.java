package com.project.trash.facility.controller.validation;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.utils.ValidatorUtils;
import com.project.trash.facility.domain.enums.FacilityType;
import com.project.trash.facility.request.FacilityEntryRequest;
import com.project.trash.facility.request.FacilityListRequest;

import java.util.Set;

import lombok.experimental.UtilityClass;

/**
 * 시설물 요청 파라미터 검증
 */
@UtilityClass
public class FacilityValidator {

  /**
   * 시설물 등록 요청 검증
   */
  public void validate(FacilityEntryRequest param) {
    ValidatorUtils.validateEmpty(param.getType(), "facility.param_type_empty");
    if (!FacilityType.containCode(param.getType())) {
      throw new ValidationException("facility.param_type_invalid");
    }
    ValidatorUtils.validateEmpty(param.getLocation(), "facility.param_location_empty");
    ValidatorUtils.validateEmpty(param.getDetailLocation(), "facility.param_detail_location_empty");
    ValidatorUtils.validateNull(param.getLatitude(), "facility.param_latitude_null");
    ValidatorUtils.validateNull(param.getLongitude(), "facility.param_longitude_null");
  }

  /**
   * 시설물 목록 조회 요청 검증
   */
  public void validate(FacilityListRequest param) {
    Set<String> typeSet = param.getType();
    if (typeSet != null) {
      for (String type : typeSet) {
        if (!FacilityType.containCode(type)) {
          throw new ValidationException("facility.param_type_invalid");
        }
      }
    }
  }
}
