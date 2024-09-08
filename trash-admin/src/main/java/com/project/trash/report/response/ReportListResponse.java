package com.project.trash.report.response;

import com.project.trash.common.utils.DateTimeUtils;

import java.time.LocalDateTime;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

/**
 * 신고 목록 조회 응답
 */
@Getter
@Setter
@Schema(title = "신고 목록 조회 응답")
public class ReportListResponse {

  /**
   * 신고 일련번호
   */
  @Schema(description = "신고 일련번호", example = "1")
  private Long reportSeq;
  /**
   * 신고 내용
   */
  @Schema(description = "신고 내용", example = "현재는 해당 시설물이 존재하지 않습니다.")
  private String content;
  /**
   * 상태
   */
  @Schema(description = "신고 처리상태(Y - 처리완료, N - 미처리)", example = "Y")
  private String status;
  /**
   * 회원 일련번호(신고자 ID)
   */
  @Schema(description = "회원 일련번호", example = "1")
  private Long memberSeq;
  /**
   * 시설물 일련번호
   */
  @Schema(description = "시설물 일련번호", example = "1")
  private Long facilitySeq;
  /**
   * 등록일자
   */
  @Schema(description = "신고 등록일자", example = "2024-09-01")
  private String createdDate;

  public ReportListResponse(Long reportSeq, String content, String status, Long memberSeq, Long facilitySeq,
      LocalDateTime createdAt) {
    this.reportSeq = reportSeq;
    this.content = content;
    this.status = status;
    this.memberSeq = memberSeq;
    this.facilitySeq = facilitySeq;
    this.createdDate = DateTimeUtils.convertToString(createdAt, DateTimeUtils.DEFAULT_DATE);
  }
}
