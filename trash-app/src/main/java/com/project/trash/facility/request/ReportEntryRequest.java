package com.project.trash.facility.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 신고 등록 요청
 */
@Getter
public class ReportEntryRequest {

  /**
   * 시설물 ID
   */
  @Schema(description = "시설물 ID", example = "1")
  private Long facilityId;
  /**
   * 신고 내용
   */
  @Schema(description = "신고 내용", example = "현재는 해당 시설물이 존재하지 않습니다.")
  private String content;
}
