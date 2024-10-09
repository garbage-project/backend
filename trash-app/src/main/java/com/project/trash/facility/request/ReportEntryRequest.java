package com.project.trash.facility.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

@Getter
public class ReportEntryRequest {

  @Schema(description = "시설물 ID", example = "1")
  private Long facilityId;

  @Schema(description = "신고 내용", example = "현재는 해당 시설물이 존재하지 않습니다.")
  private String content;
}
