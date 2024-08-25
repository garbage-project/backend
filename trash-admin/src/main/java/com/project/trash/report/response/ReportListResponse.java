package com.project.trash.report.response;

import com.project.trash.common.utils.DateTimeUtils;

import java.time.LocalDateTime;

import lombok.Getter;
import lombok.Setter;

/**
 * 신고 목록 조회 응답
 */
@Getter
@Setter
public class ReportListResponse {

  /**
   * 신고 일련번호
   */
  private Long reportSeq;
  /**
   * 내용
   */
  private String content;
  /**
   * 상태
   */
  private String status;
  /**
   * 회원 일련번호(신고자 ID)
   */
  private Long memberSeq;
  /**
   * 시설물 ID
   */
  private String facilityId;
  /**
   * 등록일자
   */
  private String createdDate;

  public ReportListResponse(Long reportSeq, String content, String status, Long memberSeq, String facilityId,
      LocalDateTime createdAt) {
    this.reportSeq = reportSeq;
    this.content = content;
    this.status = status;
    this.memberSeq = memberSeq;
    this.facilityId = facilityId;
    this.createdDate = DateTimeUtils.convertToString(createdAt, DateTimeUtils.DEFAULT_DATE);
  }
}
