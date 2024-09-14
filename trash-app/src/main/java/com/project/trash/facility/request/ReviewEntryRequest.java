package com.project.trash.facility.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 라뷰 등록 요청
 */
@Getter
public class ReviewEntryRequest {

  /**
   * 시설물 ID
   */
  @Schema(description = "시설물 ID", example = "1")
  private Long facilityId;
  /**
   * 리뷰 내용
   */
  @Schema(description = "리뷰 내용", example = "시설물이 청결합니다~")
  private String content;
}
