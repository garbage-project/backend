package com.project.trash.report.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * 신고 답변 요청
 */
@Getter
public class ReportModifyRequest {

  /**
   * 신고 ID
   */
  @Schema(description = "신고 ID", example = "1")
  private Long reportId;
  /**
   * 상태
   */
  @Schema(description = "신고 처리상태(Y - 처리완료, N - 미처리)", example = "Y")
  private String status;
  /**
   * 답변
   */
  @Schema(description = "답변", example = "처리 완료했습니다!")
  private String answer;
}
