package com.project.trash.notice.controller.validation;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.common.utils.ValidatorUtils;
import com.project.trash.notice.request.NoticeEntryRequest;
import com.project.trash.notice.request.NoticeListRequest;
import com.project.trash.notice.request.NoticeModifyRequest;

import org.apache.commons.lang3.StringUtils;

import lombok.experimental.UtilityClass;

import static com.project.trash.common.domain.resultcode.RequestResultCode.PARAM_INVALID;

@UtilityClass
public class NoticeValidator {

  public void validate(NoticeListRequest param) {
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

  public void validate(NoticeEntryRequest param) {
    ValidatorUtils.validateEmpty(param.getTitle());
    ValidatorUtils.validateEmpty(param.getContent());
    ValidatorUtils.validateEmpty(param.getValid());
  }

  public void validate(NoticeModifyRequest param) {
    ValidatorUtils.validateNull(param.getNoticeId());
    ValidatorUtils.validateEmpty(param.getTitle());
    ValidatorUtils.validateEmpty(param.getContent());
    ValidatorUtils.validateEmpty(param.getValid());
  }
}
