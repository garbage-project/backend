package com.project.trash.report.service;

import com.project.trash.common.domain.enums.Valid;
import com.project.trash.facility.domain.Report;
import com.project.trash.report.request.ReportModifyRequest;

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
   * 신고 수정
   */
  @Transactional
  public void modify(ReportModifyRequest param) {
    Report report = reportQueryService.getOne(param.getReportId());

    report.update(param.getAnswer(), Valid.convertToBoolean(param.getStatus()));
  }
}
