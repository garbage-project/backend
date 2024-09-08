package com.project.trash.report.service;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.facility.domain.Report;
import com.project.trash.facility.repository.ReportRepository;
import com.project.trash.facility.service.FacilityQueryService;
import com.project.trash.report.dao.ReportDao;
import com.project.trash.report.request.ReportListRequest;
import com.project.trash.report.response.ReportDetailResponse;
import com.project.trash.report.response.ReportListResponse;

import org.apache.commons.lang3.tuple.Pair;
import org.jooq.Record2;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

import lombok.RequiredArgsConstructor;
import trash.tables.records.ReportRecord;

import static com.project.trash.common.domain.resultcode.ReportResultCode.REPORT_NOT_FOUND;

/**
 * 신고 조회 서비스
 */
@Service
@RequiredArgsConstructor
public class ReportQueryService {

  @Value("${cloud.aws.s3.url}")
  private String s3ImageUrl;

  private final ReportRepository reportRepository;
  private final ReportDao reportDao;

  private final FacilityQueryService facilityQueryService;

  /**
   * 신고 상세 조회
   */
  @Transactional(readOnly = true)
  public ReportDetailResponse getDetail(Long reportSeq) {
    Record2<ReportRecord, String> reportDetail = reportDao.select(reportSeq);
    return new ReportDetailResponse(reportDetail, facilityQueryService.getOne(reportDetail.value1().getFcltyId()),
        s3ImageUrl);
  }

  /**
   * 신고 목록 조회
   */
  @Transactional(readOnly = true)
  public Pair<List<ReportListResponse>, Long> getList(ReportListRequest param) {
    return Pair.of(reportDao.select(param), reportDao.count(param));
  }

  /**
   * 신고 조회
   */
  @Transactional(readOnly = true)
  public Report getOne(Long noticeSeq) {
    return reportRepository.findById(noticeSeq).orElseThrow(() -> new ValidationException(REPORT_NOT_FOUND));
  }
}
