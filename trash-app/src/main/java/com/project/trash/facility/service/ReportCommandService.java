package com.project.trash.facility.service;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.facility.domain.Report;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.facility.repository.ReportRepository;
import com.project.trash.facility.request.ReportEntryRequest;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;

import static com.project.trash.common.domain.resultcode.FacilityResultCode.FACILITY_NOT_FOUND;

/**
 * 신고 등록 서비스
 */
@Service
@RequiredArgsConstructor
public class ReportCommandService {

  private final ReportRepository reportRepository;
  private final FacilityRepository facilityRepository;

  /**
   * 신고 등록
   */
  @Transactional
  public void entry(ReportEntryRequest param) {
    // 시설물 존재여부 검증
    verifyFacilityExist(param.getFacilityId());

    reportRepository.save(new Report(param.getContent(), param.getFacilityId()));
  }

  /**
   * 시설물 존재여부 검증
   */
  private void verifyFacilityExist(String facilityId) {
    if (!facilityRepository.existsById(facilityId)) {
      throw new ValidationException(FACILITY_NOT_FOUND);
    }
  }
}
