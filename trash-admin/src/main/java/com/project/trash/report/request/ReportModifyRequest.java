package com.project.trash.report.request;

import lombok.Getter;

/**
 * 신고 답변 요청
 */
@Getter
public class ReportModifyRequest {

  /**
   * 신고 일련번호
   */
  private Long reportSeq;
  /**
   * 상태
   */
  private String status;
  /**
   * 답변
   */
  private String answer;
}
