package com.project.trash.member.controller.validation;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.common.utils.ValidatorUtils;
import com.project.trash.member.request.MemberFacilityListRequest;
import com.project.trash.member.request.MemberListRequest;
import com.project.trash.member.request.MemberReviewListRequest;

import org.apache.commons.lang3.StringUtils;

import lombok.experimental.UtilityClass;

import static com.project.trash.common.domain.resultcode.RequestResultCode.PARAM_INVALID;

/**
 * 회원 요청 파라미터 검증
 */
@UtilityClass
public class MemberValidator {

  /**
   * 회원 목록 조회 요청 검증
   */
  public void validate(MemberListRequest param) {
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
   * 등록한 시설물 목록 조회 요청 검증
   */
  public void validate(MemberFacilityListRequest param) {
    ValidatorUtils.validateEmpty(param.getMemberId());
  }

  /**
   * 등록한 리뷰 목록 조회 요청 검증
   */
  public void validate(MemberReviewListRequest param) {
    ValidatorUtils.validateNull(param.getMemberId());
  }
}
