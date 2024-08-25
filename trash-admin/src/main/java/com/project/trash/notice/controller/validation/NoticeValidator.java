package com.project.trash.notice.controller.validation;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.common.utils.ValidatorUtils;
import com.project.trash.notice.request.NoticeEntryRequest;
import com.project.trash.notice.request.NoticeListRequest;
import com.project.trash.notice.request.NoticeModifyRequest;

import org.apache.commons.lang3.StringUtils;

import lombok.experimental.UtilityClass;

/**
 * 공지 요청 파라미터 검증
 */
@UtilityClass
public class NoticeValidator {

  /**
   * 공지 목록 조회 요청 검증
   */
  public void validate(NoticeListRequest param) {
    String startDate = param.getStartDate();
    if (StringUtils.isNotBlank(startDate) && !DateTimeUtils.validFormat(startDate)) {
      throw new ValidationException("notice.param_start_date_format");
    }
    String endDate = param.getEndDate();
    if (StringUtils.isNotBlank(endDate) && !DateTimeUtils.validFormat(endDate)) {
      throw new ValidationException("notice.param_end_date_format");
    }
    if (!DateTimeUtils.isBeforeDate(startDate, endDate)) {
      throw new ValidationException("notice.param_end_date_before_start_date");
    }
  }


  /**
   * 공지 등록 요청 검증
   */
  public void validate(NoticeEntryRequest param) {
    ValidatorUtils.validateEmpty(param.getTitle(), "notice.param_title_empty");
    ValidatorUtils.validateEmpty(param.getContent(), "notice.param_content_empty");
    ValidatorUtils.validateEmpty(param.getValid(), "notice.param_valid_empty");
  }

  /**
   * 공지 수정 요청 검증
   */
  public void validate(NoticeModifyRequest param) {
    ValidatorUtils.validateNull(param.getNoticeSeq(), "notice.param_seq_null");
    ValidatorUtils.validateEmpty(param.getTitle(), "notice.param_title_empty");
    ValidatorUtils.validateEmpty(param.getContent(), "notice.param_content_empty");
    ValidatorUtils.validateEmpty(param.getValid(), "notice.param_valid_empty");
  }
}
