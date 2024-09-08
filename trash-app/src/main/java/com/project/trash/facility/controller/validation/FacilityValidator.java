package com.project.trash.facility.controller.validation;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.utils.ValidatorUtils;
import com.project.trash.facility.domain.enums.FacilityType;
import com.project.trash.facility.request.FacilityEntryRequest;
import com.project.trash.facility.request.FacilityListRequest;
import com.project.trash.facility.request.FacilityModifyRequest;
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
  public void validate(FacilityEntryRequest param, List<MultipartFile> images) {
    validate(param.getType(), param.getName(), param.getLocation(), param.getDetailLocation(), param.getLatitude(),
        param.getLongitude());

    // 이미지
    if (images != null) {
      // 최대 3개
      if (images.size() > 3) {
        throw new ValidationException(PARAM_INVALID);
      }
      for (MultipartFile file : images) {
        if (file.isEmpty()) {
          throw new ValidationException(PARAM_INVALID);
        }
      }
    }
  }

  /**
   * 시설물 수정 요청 검증
   */
  public void validate(FacilityModifyRequest param, List<MultipartFile> images) {
    validate(param.getType(), param.getName(), param.getLocation(), param.getDetailLocation(), param.getLatitude(),
        param.getLongitude());

    ValidatorUtils.validateNull(param.getFacilitySeq());

    // 이미지
    if (images != null) {
      // 최대 3개
      int imageSize = param.getImageIndexes() != null ? param.getImageIndexes().size() : 0;
      for (MultipartFile file : images) {
        if (file.isEmpty()) {
          throw new ValidationException(PARAM_INVALID);
        }
        imageSize++;
      }
      if (imageSize > 3) {
        throw new ValidationException(PARAM_INVALID);
      }
    }
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
  }

  /**
   * 리뷰 등록 요청 검증
   */
  public void validate(ReviewEntryRequest param) {
    ValidatorUtils.validateNull(param.getFacilitySeq());
    ValidatorUtils.validateEmpty(param.getContent());
  }

  /**
   * 리뷰 수정 요청 검증
   */
  public void validate(ReviewModifyRequest param) {
    ValidatorUtils.validateNull(param.getReviewSeq());
    ValidatorUtils.validateEmpty(param.getContent());
  }

  /**
   * 신고 등록 요청 검증
   */
  public void validate(ReportEntryRequest param) {
    ValidatorUtils.validateNull(param.getFacilitySeq());
    ValidatorUtils.validateEmpty(param.getContent());
  }

  private void validate(String type, String name, String location, String detailLocation, BigDecimal latitude,
      BigDecimal longitude) {
    ValidatorUtils.validateEmpty(type);
    if (!FacilityType.containCode(type)) {
      throw new ValidationException(PARAM_INVALID);
    }
    ValidatorUtils.validateEmpty(name);
    ValidatorUtils.validateEmpty(location);
    ValidatorUtils.validateEmpty(detailLocation);
    ValidatorUtils.validateNull(latitude);
    ValidatorUtils.validateNull(longitude);
  }
}
