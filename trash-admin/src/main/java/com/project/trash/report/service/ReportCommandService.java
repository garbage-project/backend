package com.project.trash.report.service;

import com.project.trash.facility.domain.Report;
import com.project.trash.report.request.ReportAnswerRequest;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;

/**
 * 신고 수정 서비스
 */
@Service
@RequiredArgsConstructor
public class ReportCommandService {

  private final ReportQueryService reportQueryService;

  /**
   * 신고 답변 수정
   */
  @Transactional
  public void modify(ReportAnswerRequest param) {
    Report report = reportQueryService.getOne(param.getReportSeq());

    report.setAnswer(param.getAnswer());
  }
}
