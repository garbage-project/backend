package com.project.trash.facility.controller.validation;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.utils.ValidatorUtils;
import com.project.trash.facility.domain.enums.FacilityType;
import com.project.trash.facility.request.FacilityEntryRequest;
import com.project.trash.facility.request.FacilityListRequest;
import com.project.trash.facility.request.FacilityModifyRequest;
import com.project.trash.facility.request.FacilityReviewListRequest;
import com.project.trash.facility.request.ReportEntryRequest;
import com.project.trash.facility.request.ReviewEntryRequest;
import com.project.trash.facility.request.ReviewModifyRequest;

import org.springframework.web.multipart.MultipartFile;

import java.math.BigDecimal;
import java.util.List;
import java.util.Set;

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
        param.getLongitude(), param.getImageIds());
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

  /**
   * 시설물 수정 요청 검증
   */
  public void validate(FacilityModifyRequest param) {
    validate(param.getType(), param.getName(), param.getLocation(), param.getDetailLocation(), param.getLatitude(),
        param.getLongitude(), param.getImageIds());

    ValidatorUtils.validateNull(param.getFacilityId());
  }

  /**
   * 시설물 목록 조회 요청 검증
   */
  public void validate(FacilityListRequest param) {
    Set<String> typeSet = param.getType();
    if (typeSet != null) {
      for (String type : typeSet) {
        if (!FacilityType.containCode(type)) {
          throw new ValidationException(PARAM_INVALID);
        }
      }
    }

    if (param.getSouthLat() == null || param.getNorthLat() == null
        || param.getWestLng() == null || param.getEastLng() == null) {
      throw new ValidationException(PARAM_INVALID);
    }

    if (param.getSouthLat().compareTo(param.getNorthLat()) == 1
        || param.getWestLng().compareTo(param.getEastLng()) == 1) {
      throw new ValidationException(PARAM_INVALID);
    }
  }

  /**
   * 시설물 리뷰 목록 조회 요청 검증
   */
  public void validate(FacilityReviewListRequest param) {
    ValidatorUtils.validateNull(param.getFacilityId());
  }

  /**
   * 리뷰 등록 요청 검증
   */
  public void validate(ReviewEntryRequest param) {
    ValidatorUtils.validateNull(param.getFacilityId());
    ValidatorUtils.validateEmpty(param.getContent());
  }

  /**
   * 리뷰 수정 요청 검증
   */
  public void validate(ReviewModifyRequest param) {
    ValidatorUtils.validateNull(param.getReviewId());
    ValidatorUtils.validateEmpty(param.getContent());
  }

  /**
   * 신고 등록 요청 검증
   */
  public void validate(ReportEntryRequest param) {
    ValidatorUtils.validateNull(param.getFacilityId());
    ValidatorUtils.validateEmpty(param.getContent());
  }

  private void validate(String type, String name, String location, String detailLocation, BigDecimal latitude,
      BigDecimal longitude, Set<Long> imageIds) {
    ValidatorUtils.validateEmpty(type);
    if (!FacilityType.containCode(type)) {
      throw new ValidationException(PARAM_INVALID);
    }
    ValidatorUtils.validateEmpty(name);
    ValidatorUtils.validateEmpty(location);
    ValidatorUtils.validateEmpty(detailLocation);
    ValidatorUtils.validateNull(latitude);
    ValidatorUtils.validateNull(longitude);
    if (imageIds != null && imageIds.size() > 3) {
      throw new ValidationException(PARAM_INVALID);
    }
  }
}
