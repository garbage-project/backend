package com.project.trash.facility.controller.validation;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.common.utils.ValidatorUtils;
import com.project.trash.facility.domain.enums.FacilityApprovalStatus;
import com.project.trash.facility.domain.enums.FacilityType;
import com.project.trash.facility.request.FacilityEntryRequest;
import com.project.trash.facility.request.FacilityListRequest;
import com.project.trash.facility.request.FacilityModifyRequest;
import com.project.trash.facility.request.FacilityReviewListRequest;
import com.project.trash.member.request.MemberReviewListRequest;

import org.apache.commons.lang3.StringUtils;

import java.math.BigDecimal;

import lombok.experimental.UtilityClass;

import static com.project.trash.common.domain.resultcode.RequestResultCode.PARAM_INVALID;

/**
 * 시설물 요청 파라미터 검증
 */
@UtilityClass
public class FacilityValidator {

  /**
   * 시설물 등록 요청 검증
   */
  public void validate(FacilityEntryRequest param) {
    validate(param.getType(), param.getName(), param.getLocation(), param.getDetailLocation(), param.getLatitude(),
        param.getLongitude(), param.getDepartment(), param.getDepartmentPhoneNumber(), param.getApprovalStatus());
  }

  /**
   * 시설물 수정 요청 검증
   */
  public void validate(FacilityModifyRequest param) {
    validate(param.getType(), param.getName(), param.getLocation(), param.getDetailLocation(), param.getLatitude(),
        param.getLongitude(), param.getDepartment(), param.getDepartmentPhoneNumber(), param.getApprovalStatus());

    ValidatorUtils.validateNull(param.getFacilityId());
  }

  /**
   * 시설물 목록 조회 요청 검증
   */
  public void validate(FacilityListRequest param) {
    if (StringUtils.isNotBlank(param.getType()) && !FacilityType.containCode(param.getType())) {
      throw new ValidationException(PARAM_INVALID);
    }
    if (StringUtils.isNotBlank(param.getApprovalStatus()) && !FacilityApprovalStatus.containCode(param.getApprovalStatus())) {
      throw new ValidationException(PARAM_INVALID);
    }
    String startDate = param.getStartDate();
    if (StringUtils.isNotBlank(startDate) && !DateTimeUtils.validFormat(startDate)) {
      throw new ValidationException(PARAM_INVALID);
    }
    String endDate = param.getEndDate();
    if (StringUtils.isNotBlank(endDate) && !DateTimeUtils.validFormat(endDate)) {
      throw new ValidationException(PARAM_INVALID);
    }
    if (!DateTimeUtils.isBeforeDate(startDate, endDate)) {
      throw new ValidationException(PARAM_INVALID);
    }
  }

  /**
   * 시설물 리뷰 목록 조회 요청 검증
   */
  public void validate(FacilityReviewListRequest param) {
    ValidatorUtils.validateNull(param.getFacilityId());
  }

  private void validate(String type, String name, String location, String detailLocation, BigDecimal latitude,
      BigDecimal longitude, String department, String departmentPhoneNumber, String approvalStatus) {
    ValidatorUtils.validateEmpty(type);
    if (!FacilityType.containCode(type)) {
      throw new ValidationException(PARAM_INVALID);
    }
    ValidatorUtils.validateEmpty(name);
    ValidatorUtils.validateEmpty(location);
    ValidatorUtils.validateEmpty(detailLocation);
    ValidatorUtils.validateNull(latitude);
    ValidatorUtils.validateNull(longitude);
    ValidatorUtils.validateEmpty(department);
    ValidatorUtils.validateEmpty(departmentPhoneNumber);
    ValidatorUtils.validateEmpty(approvalStatus);
    if (!FacilityApprovalStatus.containCode(approvalStatus)) {
      throw new ValidationException(PARAM_INVALID);
    }
  }
}
