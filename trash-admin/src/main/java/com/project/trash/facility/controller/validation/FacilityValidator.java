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

import org.apache.commons.lang3.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import java.math.BigDecimal;
import java.util.List;
import java.util.Set;

import lombok.experimental.UtilityClass;

import static com.project.trash.common.domain.resultcode.RequestResultCode.PARAM_INVALID;

@UtilityClass
public class FacilityValidator {

  public void validate(FacilityEntryRequest param) {
    validate(param.getType(), param.getName(), param.getLocation(), param.getLatitude(),
        param.getLongitude(), param.getApprovalStatus(), param.getImageIds());
  }

  public void validate(List<MultipartFile> images) {
    if (images == null || images.isEmpty() || images.size() > 3) {
      throw new ValidationException(PARAM_INVALID);
    }

    for (MultipartFile image : images) {
      if (image.isEmpty()) {
        throw new ValidationException(PARAM_INVALID);
      }
    }
  }

  public void validate(FacilityModifyRequest param) {
    validate(param.getType(), param.getName(), param.getLocation(), param.getLatitude(),
        param.getLongitude(), param.getApprovalStatus(), param.getImageIds());

    ValidatorUtils.validateNull(param.getFacilityId());
  }

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


  public void validate(FacilityReviewListRequest param) {
    ValidatorUtils.validateNull(param.getFacilityId());
  }

  private void validate(String type, String name, String location, BigDecimal latitude,
      BigDecimal longitude, String approvalStatus, Set<Long> imageIds) {
    ValidatorUtils.validateEmpty(type);
    if (!FacilityType.containCode(type)) {
      throw new ValidationException(PARAM_INVALID);
    }
    ValidatorUtils.validateEmpty(name);
    ValidatorUtils.validateEmpty(location);
    ValidatorUtils.validateNull(latitude);
    ValidatorUtils.validateNull(longitude);
    ValidatorUtils.validateEmpty(approvalStatus);
    if (!FacilityApprovalStatus.containCode(approvalStatus)) {
      throw new ValidationException(PARAM_INVALID);
    }
    if (imageIds != null && imageIds.size() > 3) {
      throw new ValidationException(PARAM_INVALID);
    }
  }

  public void validate(MultipartFile file, String type) {
    if (file == null || file.isEmpty()) {
      throw new ValidationException(PARAM_INVALID);
    }
    if (StringUtils.isBlank(type) || !FacilityType.containCode(type)) {
      throw new ValidationException(PARAM_INVALID);
    }
  }
}
