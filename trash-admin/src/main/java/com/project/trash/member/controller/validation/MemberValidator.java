package com.project.trash.member.controller.validation;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.common.utils.ValidatorUtils;
import com.project.trash.member.request.MemberFacilityListRequest;
import com.project.trash.member.request.MemberListRequest;
import com.project.trash.member.request.MemberReviewListRequest;

import org.apache.commons.lang3.StringUtils;

import lombok.experimental.UtilityClass;

/**
 * 회원 요청 파라미터 검증
 */
@UtilityClass
public class MemberValidator {

  /**
   * 회원 목록 조회 요청 검증
   */
  public void validate(MemberListRequest param) {
    if (StringUtils.isNotBlank(param.getBirthday()) && !DateTimeUtils.validFormat(param.getBirthday())) {
      throw new ValidationException("member.param_birthday_format");
    }
    String startDate = param.getStartDate();
    if (StringUtils.isNotBlank(startDate) && !DateTimeUtils.validFormat(startDate)) {
      throw new ValidationException("member.param_start_date_format");
    }
    String endDate = param.getEndDate();
    if (StringUtils.isNotBlank(endDate) && !DateTimeUtils.validFormat(endDate)) {
      throw new ValidationException("member.param_end_date_format");
    }
    if (!DateTimeUtils.isBeforeDate(startDate, endDate)) {
      throw new ValidationException("member.param_end_date_before_start_date");
    }
  }

  /**
   * 등록한 시설물 목록 조회 요청 검증
   */
  public void validate(MemberFacilityListRequest param) {
    ValidatorUtils.validateNull(param.getMemberSeq(), "member.param_seq_null");
  }

  /**
   * 등록한 리뷰 목록 조회 요청 검증
   */
  public void validate(MemberReviewListRequest param) {
    ValidatorUtils.validateNull(param.getMemberSeq(), "member.param_seq_null");
  }
}
