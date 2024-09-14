package com.project.trash.facility.service;

import com.project.trash.facility.domain.Facility;
import com.project.trash.facility.domain.enums.FacilityApprovalStatus;
import com.project.trash.facility.domain.enums.FacilityType;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.facility.request.FacilityEntryRequest;
import com.project.trash.facility.request.FacilityModifyRequest;
import com.project.trash.utils.AdminUtils;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Set;

import lombok.RequiredArgsConstructor;

/**
 * 시설물 등록/수정/삭제 서비스
 */
@Service
@RequiredArgsConstructor
public class FacilityCommandService {

  private final FacilityRepository facilityRepository;
  private final FacilityQueryService facilityQueryService;

  /**
   * 선택한 시설물 목록 삭제
   */
  @Transactional
  public void delete(Set<Long> facilityIds) {
    facilityRepository.deleteAllByIdInBatch(facilityIds);
  }

  /**
   * 시설물 등록
   */
  @Transactional
  public void entry(FacilityEntryRequest param) {
    facilityRepository.save(new Facility(FacilityType.fromCode(param.getType()), param.getName(), param.getLocation(),
        param.getDetailLocation(), param.getLatitude(), param.getLongitude(), param.getInformation(), param.getDepartment(),
        param.getDepartmentPhoneNumber(), FacilityApprovalStatus.fromCode(param.getApprovalStatus()), AdminUtils.getId()));
  }

  /**
   * 시설물 수정
   */
  @Transactional
  public void modify(FacilityModifyRequest param) {
    Facility facility = facilityQueryService.getOne(param.getFacilityId());

    facility.update(FacilityType.fromCode(param.getType()), param.getName(), param.getLocation(),
        param.getDetailLocation(), param.getLatitude(), param.getLongitude(), param.getInformation(),
        param.getDepartment(), param.getDepartmentPhoneNumber(), FacilityApprovalStatus.fromCode(param.getApprovalStatus()));
  }
}