package com.project.trash.report.request;

import lombok.Getter;

/**
 * 신고 답변 요청
 */
@Getter
public class ReportAnswerRequest {

  /**
   * 신고 일련번호
   */
  private Long reportSeq;
  /**
   * 답변
   */
  private String answer;
}
