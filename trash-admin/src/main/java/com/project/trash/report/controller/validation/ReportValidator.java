package com.project.trash.report.controller.validation;

import com.project.trash.common.domain.enums.Valid;
import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.common.utils.ValidatorUtils;
import com.project.trash.report.request.ReportListRequest;
import com.project.trash.report.request.ReportModifyRequest;

import org.apache.commons.lang3.StringUtils;

import lombok.experimental.UtilityClass;

/**
 * 신고 요청 파라미터 검증
 */
@UtilityClass
public class ReportValidator {

  /**
   * 신고 목록 조회 요청 검증
   */
  public void validate(ReportListRequest param) {
    String startDate = param.getStartDate();
    if (StringUtils.isNotBlank(startDate) && !DateTimeUtils.validFormat(startDate)) {
      throw new ValidationException("report.param_start_date_format");
    }
    String endDate = param.getEndDate();
    if (StringUtils.isNotBlank(endDate) && !DateTimeUtils.validFormat(endDate)) {
      throw new ValidationException("report.param_end_date_format");
    }
    if (!DateTimeUtils.isBeforeDate(startDate, endDate)) {
      throw new ValidationException("report.param_end_date_before_start_date");
    }
  }

  /**
   * 신고 수정 요청 검증
   */
  public void validate(ReportModifyRequest param) {
    // 신고 일련번호
    ValidatorUtils.validateNull(param.getReportSeq(), "report.param_seq_null");
    // 신고 처리상태
    ValidatorUtils.validateEmpty(param.getStatus(), "report.param_status_empty");
    if (!Valid.containCode(param.getStatus())) {
      throw new ValidationException("report.param_status_invalid");
    }
  }
}
