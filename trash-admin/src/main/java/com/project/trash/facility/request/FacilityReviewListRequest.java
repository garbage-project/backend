package com.project.trash.facility.request;

import com.project.trash.common.request.PageRequest;

import io.swagger.v3.oas.annotations.Parameter;
import lombok.Getter;
import lombok.Setter;

/**
 * 시설물 리뷰 목록 조회
 */
@Getter
@Setter
public class FacilityReviewListRequest extends PageRequest {

  /**
   * 시설물 ID
   */
  @Parameter(description = "시설물 ID", example = "1")
  private Long facilityId;
}
