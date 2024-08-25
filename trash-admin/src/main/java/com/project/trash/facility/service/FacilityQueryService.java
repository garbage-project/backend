package com.project.trash.facility.service;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.facility.domain.Facility;
import com.project.trash.facility.repository.FacilityRepository;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;

/**
 * 시설물 조회 서비스
 */
@Service
@RequiredArgsConstructor
public class FacilityQueryService {

  private final FacilityRepository facilityRepository;

  /**
   * 시설물 단일 조회
   */
  @Transactional(readOnly = true)
  public Facility getOne(String facilityId) {
    return facilityRepository.findById(facilityId).orElseThrow(() -> new ValidationException("facility.not_found"));
  }
}