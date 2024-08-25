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
        throw new ValidationException("facility.param_image_max_count");
      }
      for (MultipartFile file : images) {
        if (file.isEmpty()) {
          throw new ValidationException("file.not_empty");
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

    ValidatorUtils.validateEmpty(param.getFacilityId(), "facility.param_id_null");

    // 이미지
    if (images != null) {
      // 최대 3개
      int imageSize = param.getImageIndexes() != null ? param.getImageIndexes().size() : 0;
      for (MultipartFile file : images) {
        if (file.isEmpty()) {
          throw new ValidationException("file.not_empty");
        }
        imageSize++;
      }
      if (imageSize > 3) {
        throw new ValidationException("facility.param_image_max_count");
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
          throw new ValidationException("facility.param_type_invalid");
        }
      }
    }
  }

  /**
   * 리뷰 등록 요청 검증
   */
  public void validate(ReviewEntryRequest param) {
    ValidatorUtils.validateEmpty(param.getFacilityId(), "facility.param_id_null");
    ValidatorUtils.validateEmpty(param.getContent(), "review.param_content_empty");
  }

  /**
   * 리뷰 수정 요청 검증
   */
  public void validate(ReviewModifyRequest param) {
    ValidatorUtils.validateNull(param.getReviewSeq(), "review.param_seq_null");
    ValidatorUtils.validateEmpty(param.getContent(), "review.param_content_empty");
  }

  /**
   * 신고 등록 요청 검증
   */
  public void validate(ReportEntryRequest param) {
    ValidatorUtils.validateEmpty(param.getFacilityId(), "facility.param_id_null");
    ValidatorUtils.validateEmpty(param.getContent(), "report.param_content_empty");
  }

  private void validate(String type, String name, String location, String detailLocation, BigDecimal latitude,
      BigDecimal longitude) {
    ValidatorUtils.validateEmpty(type, "facility.param_type_empty");
    if (!FacilityType.containCode(type)) {
      throw new ValidationException("facility.param_type_invalid");
    }
    ValidatorUtils.validateEmpty(name, "facility.param_name_empty");
    ValidatorUtils.validateEmpty(location, "facility.param_location_empty");
    ValidatorUtils.validateEmpty(detailLocation, "facility.param_detail_location_empty");
    ValidatorUtils.validateNull(latitude, "facility.param_latitude_null");
    ValidatorUtils.validateNull(longitude, "facility.param_longitude_null");
  }
}
