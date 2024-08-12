package com.project.trash.facility.service;

import com.project.trash.facility.domain.Facility;
import com.project.trash.facility.domain.enums.FacilityType;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.facility.request.FacilityEntryRequest;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;

/**
 * 시설물 등록/수정/삭제 서비스
 */
@Service
@RequiredArgsConstructor
public class FacilityCommandService {

  private final FacilityRepository facilityRepository;

  /**
   * 시설물 등록
   */
  @Transactional
  public void entry(FacilityEntryRequest param) {
    facilityRepository.save(
        new Facility(FacilityType.fromCode(param.getType()), param.getLocation(), param.getDetailLocation(),
            param.getLatitude(), param.getLongitude(), param.getInformation()));
  }
}
